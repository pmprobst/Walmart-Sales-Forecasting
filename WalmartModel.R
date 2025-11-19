library(vroom)
library(dplyr)
library(tidymodels)

#Load in data
train_data <- vroom("data/train.csv")
test_data <- vroom("data/test.csv")
features_df <- vroom("data/features.csv")
store_df <- vroom("data/stores.csv")

###DATA CLEANING###

#clean up markdown folumns in features data
clean_features_df <- features_df %>% 
  # Replace NA values in markdown columns with 0
  mutate(across(starts_with("MarkDown"), ~tidyr::replace_na(., 0))) %>%
  
  # Replace negative markdown values with 0
  mutate(across(starts_with("MarkDown"), ~if_else(. < 0, 0, .))) %>%
  
  # Create TotalMarkdown column
  mutate(TotalMarkdown = rowSums(across(starts_with("MarkDown")))) %>%
  
  # Remove original markdown columns
  select(-starts_with("MarkDown")) %>%

  # Create MarkdownFlag (1 if any markdown > 0, else 0)
  mutate(MarkdownFlag = if_else(TotalMarkdown > 0, 1, 0))

#Join data
joined_train_data <- left_join(train_data, store_df, by = "Store")
joined_train_data <- left_join(joined_train_data, clean_features_df, by = c("Store" ,"Date"))
head(joined_train_data)

###FEATURE ENGINEERING###

# Holiday date vectors keyed by year
christmas_dates    <- as.Date(c("2010-12-31","2011-12-30","2012-12-28","2013-12-27")); names(christmas_dates)    <- 2010:2013
thanksgiving_dates <- as.Date(c("2010-11-26","2011-11-25","2012-11-23","2013-11-29")); names(thanksgiving_dates) <- 2010:2013
labor_day_dates    <- as.Date(c("2010-09-10","2011-09-09","2012-09-07","2013-09-06")); names(labor_day_dates)    <- 2010:2013
super_bowl_dates   <- as.Date(c("2010-02-12","2011-02-11","2012-02-10","2013-02-08")); names(super_bowl_dates)   <- 2010:2013

# Helpers: get next (or same-year) holiday date, then weeks until
next_or_same <- function(x, dates_by_year) {
  yr   <- year(x)
  this <- dates_by_year[as.character(yr)]
  next <- dates_by_year[as.character(yr + 1)]
  ifelse(!is.na(this) & x <= this, this,
         ifelse(!is.na(next), next, as.Date(NA)))
}

weeks_until <- function(x, dates_by_year) {
  target <- next_or_same(x, dates_by_year)
  ifelse(is.na(target), NA_real_,
         ceiling(as.numeric(difftime(target, x, units = "days")) / 7))
}

# Build recipe (adjust outcome/predictor names as needed)
my_recipe <- recipe(Weekly_Sales ~ ., data = joined_train_data) %>%
  step_mutate(
    date = as.Date(Date),
    is_christmas    = date %in% christmas_dates,
    is_thanksgiving = date %in% thanksgiving_dates,
    is_labor_day    = date %in% labor_day_dates,
    is_super_bowl   = date %in% super_bowl_dates,
    #weeks_till_christmas    = weeks_until(date, christmas_dates),
    #weeks_till_thanksgiving = weeks_until(date, thanksgiving_dates),
    #weeks_till_labor_day    = weeks_until(date, labor_day_dates),
    #weeks_till_super_bowl   = weeks_until(date, super_bowl_dates)
  ) %>%
  step_rm(Date)




# 1. Time & Calendar Features
# 
# (Weekly data depends heavily on seasonal timing)
# Basic calendar:
#   
#   Year
# 
# Month
# 
# Week
# 
# Quarter
# 
# WeekOfYear
# 
# IsHoliday (from data)
# IsHoliday_nextWeek
# IsHoliday_prevWeek
# Holiday proximity (very important):
#   
#   Weeks_to_Christmas
# Weeks_to_Thanksgiving
# Weeks_to_SuperBowl
# Weeks_to_LaborDay
# IsWeekBeforeHoliday
# IsWeekAfterHoliday
# IsTwoWeeksBeforeHoliday
# Seasonality indicators:
#   
#   IsWinter
# IsSpring
# IsSummer
# IsFall
# 
# Cyclical encodings (optional):
#   sin_week = sin(2π × week/52)
# 
# cos_week = cos(2π × week/52)
# (captures annual periodicity)
# 
# 2. Store-Level Features
# Engineered:
#   
#   Store_Size_Bucket (Small, Medium, Large)
# Store_Age (years since first appearance in dataset)
# Store_Historical_AvgSales
# Store_Historical_Trend (slope of sales over time)
# Interaction terms:
# Store_Type × Month
# Store_Type × Holiday
# 
# 3. Department-Level Features
# (dept behavior is the single strongest predictor)
# Department historical stats:
#   Dept_Mean_Sales
# Dept_Median_Sales
# 
# Dept_Trend_Linear (slope per dept)
# 
# Dept_Seasonality_Index (avg sales by week)
# 
# Interaction terms:
#   
#   Dept × Holiday
# 
# Dept × Temperature
# 
# Dept × Fuel_Price
# 
# Dept × CPI
# 
# Dept × Unemployment
# 
# Department category encoding:
#   
#   If you manually categorize:
#   
#   Seasonal (lawn & garden, toys)
# 
# Nonseasonal
# 
# Perishable
# 
# Clothing
# 
# Dept_Category → use one-hot.
# 
# 4. Store–Dept History Features
# 
# These are extremely powerful.
# 
# Lags:
#   
#   Lag_1_week_sales
# 
# Lag_2_week_sales
# 
# Lag_4_week_sales
# 
# Lag_52_week_sales (captures annual patterns)
# 
# Rolling windows:
#   
#   Rolling_4w_mean
# 
# Rolling_8w_mean
# 
# Rolling_13w_mean
# 
# Rolling_26w_mean
# 
# Rolling_52w_mean
# 
# (optionally also std dev, min, max)
# 
# Expanding windows:
#   
#   Cumulative_Mean_Sales
# 
# Cumulative_Trend_Slope
# 
# Holiday-conditional lags:
#   
#   (sales spikes at holidays are highly correlated year-to-year)
# 
# Lag_Christmas_Sales
# 
# Lag_Thanksgiving_Sales
# 
# 5. Price, Promotion, and Markdown Features
# 
# (MarkDown variables are sparse but extremely predictive)
# 
# Raw fields:
#   
#   MarkDown1
# 
# MarkDown2
# 
# MarkDown3
# 
# MarkDown4
# 
# MarkDown5
# 
# Missingness indicators:
#   
#   (add for each)
# 
# MD1_missing
# 
# MD2_missing
# 
# …
# 
# Promotion presence:
#   
#   Any_Markdown = 1 if any MarkDown > 0
# 
# Total_MarkDown = sum(MD1…MD5)
# 
# Holiday_Markdown (markdown > 0 during holiday weeks)
# 
# Imputed values (if needed):
#   
#   Zero-filled imputation if outside promo window
# 
# Mean/median per store-month
# 
# Model-based imputation
# 
# Interactions:
#   
#   MarkDown × Dept
# 
# MarkDown × Store_Type
# 
# MarkDown × Holiday
# 
# MarkDown × Dept is one of the most powerful combinations.
# 
# 6. Regional Economic Features
# 
# (from features.csv)
# 
# Raw fields:
#   
#   Temperature
# Fuel_Price
# 
# CPI
# 
# Unemployment
# 
# Temporal versions:
#   
#   Temp_Deviation (temp – historical mean for that store)
# 
# Fuel_Price_Trend
# 
# CPI_Change (week-over-week)
# 
# Unemployment_Change
# 
# Bucketed versions:
#   
#   Temp_Bin (cold/normal/hot)
# 
# Fuel_Price_Bin
# 
# CPI_Bin
# 
# Interaction terms:
#   
#   Temperature × Dept
# 
# FuelPrice × Store_Size
# 
# CPI × Dept
# 
# Unemployment × StoreType
# 
# Temperature × Holiday
# 
# 7. Weather & Seasonal Patterns
# 
# Weather affects some departments (gardening, apparel, grills, etc.)
# 
# Construct:
#   
#   IsColdSpell = Temperature < 20th percentile
# 
# IsHeatWave = Temperature > 80th percentile
# 
# Temp_Anomaly = Temperature - month_avg_temp
# 
# Combined weather-season features:
#   
#   Winter × Temperature
# 
# Summer × Temperature
# 
# Dept × Temp_Anomaly
# 
# 8. Holiday-Specific Features
# 
# You can create a structured set for each major holiday.
# 
# Example for Christmas:
#   
#   Is_Christmas_Week
# 
# Is_Week_Before_Christmas
# 
# Is_TwoWeeks_Before_Christmas
# 
# Is_After_Christmas
# 
# Repeat for:
#   
#   Thanksgiving
# 
# Super Bowl
# 
# Labor Day
# 
# Holiday strength feature:
#   
#   Holiday_Weight (proximity scaled: e.g., 1 if week-of, 0.5 if week-before)
# 
# 9. Target Transformations
# 
# Useful for tree models & linear models.
# 
# log_sales = log(Weekly_Sales + 1)
# 
# sales_diff = Weekly_Sales - Lag_1_week_sales
# 
# pct_change_sales = sales_diff / Lag_1_week_sales
# 
# 10. Encoding & Miscellaneous Features
# Categorical encoding:
#   
#   One-hot encode: Store_Type, Dept, Month
# 
# Label encoding (if using tree models): Dept, Store_Type
# 
# Interaction expansions:
#   
#   StoreType × Month
# 
# Dept × Month
# 
# StoreSize × Dept × Month (optional high-cardinality)
# 
# Frequency encodings:
#   
#   Dept_Frequency = count(dept occurrences)
# 
# Store_Frequency
# 
# Outlier indicators:
#   
#   Is_Sales_Spike = Weekly_Sales > 95th percentile
# 
# Is_Anomalous_Week = unusual spike/drop
# 
# 11. Extreme-Value / Market-Shift Features
# 
# Tracks long-term macro trends.
# 
# Macro_Trend = rolling_mean(CPI – Unemployment)
# 
# Store_Macro_Interaction = Store_Size × CPI
# 
# Fuel_Price_Elasticity_Proxy = Fuel_Price / Fuel_Price_meanStore
# 
# 12. Cross-Feature Groups (high gain for models like CatBoost/LightGBM)
# Dept + Season:
#   
#   Dept × Season
# 
# Dept × Quarter
# 
# StoreType + Holiday:
#   
#   StoreType × Thanksgiving
# 
# StoreType × Christmas
# 
# Dept + Markdowns:
#   
#   Dept × Total_MarkDown
# 
# Dept × Any_Promo
# 
# Dept × MD1, Dept × MD2, etc.
# 
# Store + Economic:
#   
#   Store_Size × Unemployment
# 
# Store_Type × CPI

#Prep & Bake Recipe
prep <- prep(my_recipe)

###Model###
knn_model <- nearest_neighbor(
  mode = "regression",
  neighbors = tune(),
  weight_func = "rectangular",  # standard unweighted KNN
  dist_power = 2
) %>% set_engine("kknn")

#Set Workflow
wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(knn_model)

#set up grid of tuning values
tuning_grid <- grid_regular(
  neighbors(range = c(1L, 51L)),
  levels = 5)

folds <- vfold_cv(joined_train_data ,v = 3 ,repeats = 1)

#CV
CV_results <- wf %>%
  tune_grid(resamples = folds
            ,grid = tuning_grid
            ,metrics = metric_set(roc_auc))

bestTune <- CV_results %>%
  select_best(metric = "roc_auc")

#finalize and fit workflow
final_wf <-
  wf %>%
  finalize_workflow(bestTune) %>%
  fit(data = train_data)

#Identify the best levels of penalty and mixture (highest mean)
CV_results %>%
  collect_metrics() %>%
  arrange(desc(mean))

#plot levels of penalty and mixture
autoplot(CV_results)

#Get Predictions
predictions <-predict(final_wf,
                      new_data = test_data
                      ,type = "prob")

#Remove p(0) column from df
predictions <- predictions %>% 
  select(-.pred_0) %>%
  #rename .pred_1 as "action" for kaggle submission
  rename (Action = .pred_1)


# Combine with test_data ID
kaggle_submission <- bind_cols(
  test_data %>% select(id),
  predictions
)

#write submission df to CSV for submission
vroom_write(kaggle_submission, "KNNSubmission.csv" ,delim = ",")


