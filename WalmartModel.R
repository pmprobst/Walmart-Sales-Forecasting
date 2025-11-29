library(vroom)
library(dplyr)
library(tidymodels)
library(lubridate)

setwd("/Users/pprobst/Library/CloudStorage/OneDrive-BrighamYoungUniversity/STAT 348/WalmartSalesForecasting")


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

# Join test data with engineered features
joined_test_data <- left_join(test_data, store_df, by = "Store")
joined_test_data <- left_join(joined_test_data, clean_features_df, by = c("Store" ,"Date"))

###FEATURE ENGINEERING###

# ---------- Holiday date helpers (US) ----------
first_weekday_on_or_after <- function(d, target_wday, week_start = 1) {
  # target_wday: Monday=1 ... Sunday=7 (when week_start=1)
  d + days((target_wday - wday(d, week_start = week_start) + 7) %% 7)
}
thanksgiving_date <- function(y) { # 4th Thursday in Nov
  first_thu <- first_weekday_on_or_after(ymd(sprintf("%d-11-01", y)), target_wday = 4)
  first_thu + weeks(3)
}
labor_day_date <- function(y) {     # 1st Monday in Sep
  first_weekday_on_or_after(ymd(sprintf("%d-09-01", y)), target_wday = 1)
}
super_bowl_date <- function(y) {    # (approx) 1st Sunday in Feb
  first_weekday_on_or_after(ymd(sprintf("%d-02-01", y)), target_wday = 7)
}
weeks_to <- function(date, holiday_date) as.numeric(difftime(holiday_date, date, units = "days")) / 7

# ---------- Feature builders ----------
add_calendar_features <- function(df) {
  df %>%
    mutate(
      Year      = year(Date),
      Month     = month(Date),
      Quarter   = quarter(Date),
      WeekOfYear= isoweek(Date),
      Week      = week(Date)
    )
}

add_holiday_proximity <- function(df) {
  df %>%
    mutate(
      Weeks_to_Christmas    = weeks_to(Date, ymd(sprintf("%d-12-25", Year))),
      Weeks_to_Thanksgiving = weeks_to(Date, map_dbl(Year, ~ as.numeric(thanksgiving_date(.x)) ) |> as_date()),
      Weeks_to_SuperBowl    = weeks_to(Date, map_dbl(Year, ~ as.numeric(super_bowl_date(.x)) ) |> as_date()),
      Weeks_to_LaborDay     = weeks_to(Date, map_dbl(Year, ~ as.numeric(labor_day_date(.x)) ) |> as_date())
    )
}

add_markdown_features <- function(df) {
  md_cols <- paste0("MarkDown", 1:5)
  df %>%
    mutate(across(all_of(md_cols), ~ replace_na(.x, 0))) %>%
    mutate(
      Holiday_Markdown = IsHoliday & (MarkDown1 > 0 | MarkDown2 > 0 | MarkDown3 > 0 | MarkDown4 > 0 | MarkDown5 > 0)
    )
}

add_store_dept_stats <- function(train_joined, df) {
  # calendar fields needed for seasonality
  tj <- add_calendar_features(train_joined)
  
  store_stats <- tj %>%
    group_by(Store) %>%
    summarise(
      Store_Historical_AvgSales = mean(Weekly_Sales, na.rm = TRUE),
      Store_First_Date = min(Date),
      Store_Last_Date  = max(Date),
      Store_Historical_Trend = coef(lm(Weekly_Sales ~ as.numeric(Date)))[2],
      .groups = "drop"
    ) %>%
    mutate(Store_Age = as.numeric(difftime(Store_Last_Date, Store_First_Date, units = "days")) / 365.25) %>%
    select(-Store_First_Date, -Store_Last_Date)
  
  dept_stats <- tj %>%
    group_by(Dept) %>%
    summarise(
      Dept_Mean_Sales   = mean(Weekly_Sales, na.rm = TRUE),
      Dept_Median_Sales = median(Weekly_Sales, na.rm = TRUE),
      Dept_Trend_Linear = coef(lm(Weekly_Sales ~ as.numeric(Date)))[2],
      .groups = "drop"
    )
  
  dept_season <- tj %>%
    group_by(Dept, WeekOfYear) %>%
    summarise(Dept_Seasonality_Index = mean(Weekly_Sales, na.rm = TRUE), .groups = "drop")
  
  df %>%
    left_join(store_stats, by = "Store") %>%
    left_join(dept_stats,  by = "Dept") %>%
    left_join(dept_season, by = c("Dept", "WeekOfYear"))
}

add_store_dept_history <- function(df) {
  # prev/next holiday flags at Store timeline (same across depts)
  hol_by_store <- df %>%
    distinct(Store, Date, IsHoliday) %>%
    arrange(Store, Date) %>%
    group_by(Store) %>%
    mutate(
      # IsHoliday_prevWeek = lag(IsHoliday, 1),
      # IsHoliday_nextWeek = lead(IsHoliday, 1)
    ) %>%
    ungroup()
  
  df %>%
    left_join(hol_by_store, by = c("Store", "Date", "IsHoliday")) %>%
    arrange(Store, Dept, Date) %>%
    group_by(Store, Dept) %>%
    mutate(
      # Lag_1_week_sales  = lag(Weekly_Sales, 1),
      # Lag_2_week_sales  = lag(Weekly_Sales, 2),
      # Lag_4_week_sales  = lag(Weekly_Sales, 4),
      # Lag_52_week_sales = lag(Weekly_Sales, 52),
      #
      # sales_lag1 = lag(Weekly_Sales, 1),
      # Rolling_4w_mean   = slide_dbl(sales_lag1, mean, .before = 3,  .complete = TRUE, na.rm = TRUE),
      # Rolling_8w_mean   = slide_dbl(sales_lag1, mean, .before = 7,  .complete = TRUE, na.rm = TRUE),
      # Rolling_13w_mean  = slide_dbl(sales_lag1, mean, .before = 12, .complete = TRUE, na.rm = TRUE),
      # Rolling_26w_mean  = slide_dbl(sales_lag1, mean, .before = 25, .complete = TRUE, na.rm = TRUE),
      # Rolling_52w_mean  = slide_dbl(sales_lag1, mean, .before = 51, .complete = TRUE, na.rm = TRUE),
      #
      Dept_x_Holiday       = as.numeric(IsHoliday),
      Dept_x_Temperature   = Temperature,
      Dept_x_Fuel_Price    = Fuel_Price,
      Dept_x_CPI           = CPI,
      Dept_x_Unemployment  = Unemployment
    ) %>%
    ungroup() %>%
    # select(-sales_lag1)
}

add_macro_features <- function(df) {
  df %>%
    arrange(Store, Date) %>%
    group_by(Store) %>%
    mutate(
      Macro_raw    = CPI - Unemployment,
      # Macro_Trend  = slide_dbl(lag(Macro_raw, 1), mean, .before = 12, .complete = FALSE, na.rm = TRUE),
      Fuel_Price_meanStore = mean(Fuel_Price, na.rm = TRUE),
      Fuel_Price_Elasticity_Proxy = Fuel_Price / Fuel_Price_meanStore
    ) %>%
    ungroup() %>%
    select(-Macro_raw)
}

# Holiday-week sales (prev year) lookups
add_holiday_sales_lookbacks <- function(df) {
  df <- add_calendar_features(df)
  
  years <- sort(unique(df$Year))
  xmas_week <- tibble(Year = years, XMasWeek = isoweek(ymd(sprintf("%d-12-25", years))))
  thx_week  <- tibble(Year = years, ThxWeek  = isoweek(map(years, thanksgiving_date) |> as_date()))
  
  # xmas_sales <- df %>%
  #   left_join(xmas_week, by = "Year") %>%
  #   filter(WeekOfYear == XMasWeek) %>%
  #   group_by(Store, Dept, Year) %>%
  #   summarise(Christmas_Sales = mean(Weekly_Sales, na.rm = TRUE), .groups = "drop") %>%
  #   mutate(Year = Year + 1) %>%
  #   rename(Lag_Christmas_Sales = Christmas_Sales)
  # 
  # thx_sales <- df %>%
  #   left_join(thx_week, by = "Year") %>%
  #   filter(WeekOfYear == ThxWeek) %>%
  #   group_by(Store, Dept, Year) %>%
  #   summarise(Thanksgiving_Sales = mean(Weekly_Sales, na.rm = TRUE), .groups = "drop") %>%
  #   mutate(Year = Year + 1) %>%
  #   rename(Lag_Thanksgiving_Sales = Thanksgiving_Sales)
  # 
  # df %>%
  #   left_join(xmas_sales, by = c("Store", "Dept", "Year")) %>%
  #   left_join(thx_sales,  by = c("Store", "Dept", "Year"))
  df
}

# ---------- Build final modeling frame ----------
fullTrain <- joined_train_data %>%
  add_calendar_features() %>%
  add_holiday_proximity() %>%
  add_markdown_features() %>%
  add_holiday_sales_lookbacks() %>%
  add_store_dept_stats(train_joined = joined_train_data, df = .) %>%
  add_store_dept_history() %>%
  add_macro_features() %>%
  mutate(
    Store = factor(Store),
    Dept  = factor(Dept),
    IsHoliday = factor(IsHoliday)
    # IsHoliday_prevWeek = factor(coalesce(IsHoliday_prevWeek, FALSE)),
    # IsHoliday_nextWeek = factor(coalesce(IsHoliday_nextWeek, FALSE))
  )

fullTest <- joined_test_data %>%
  add_calendar_features() %>%
  add_holiday_proximity() %>%
  add_markdown_features() %>%
  add_holiday_sales_lookbacks() %>%
  add_store_dept_stats(train_joined = joined_train_data, df = .) %>%
  add_store_dept_history() %>%
  add_macro_features() %>%
  mutate(
    Store = factor(Store),
    Dept  = factor(Dept),
    IsHoliday = factor(IsHoliday)
    # IsHoliday_prevWeek = factor(coalesce(IsHoliday_prevWeek, FALSE)),
    # IsHoliday_nextWeek = factor(coalesce(IsHoliday_nextWeek, FALSE))
  )

# keep df_feat for recipe/tuning convenience
df_feat <- fullTrain

# ---------- Tidymodels recipe ----------
sales_rec <- recipe(Weekly_Sales ~ ., data = df_feat) %>%
  step_rm(Date) %>%                          # keep engineered calendar fields instead
  step_zv(all_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_normalize(all_numeric_predictors())   # drop this if using tree models

#Prep & Bake Recipe
prep <- prep(sales_rec)

all_preds <- tibble(Id = character(), Weekly_Sales = numeric())
n_storeDepts <- df_feat %>% distinct(Store, Dept) %>% nrow()
cntr <- 0
for(store in unique(fullTest$Store)){
  
  store_train <- fullTrain %>%
    filter(Store==store)
  store_test <- fullTest %>%
    filter(Store==store)
  
  for(dept in unique(store_test$Dept)){
    
    ## Filter Test and Training Data
    dept_train <- store_train %>%
      filter(Dept==dept)
    dept_test <- store_test %>%
      filter(Dept==dept)
    
    ## If Statements for data scenarios
    if(nrow(dept_train)==0){
      
      ## Predict 0
      preds <- dept_test %>%
        transmute(Id=paste(Store, Dept, Date, sep="_"),
                  Weekly_Sales=0)
      
    } else if(nrow(dept_train) < 10 && nrow(dept_train) > 0){
      
      ## Predict the mean
      preds <- dept_test %>%
        transmute(Id=paste(Store, Dept, Date, sep="_"),
                  Weekly_Sales=mean(dept_train$Weekly_Sales))
      
    } else {
      
      ## Fit a penalized regression model
      my_recipe <- recipe(Weekly_Sales ~ ., data = dept_train) %>%
        step_mutate(Holiday = as.integer(IsHoliday)) %>%
        step_date(Date, features=c("month","year")) %>%
        step_rm(Date, Store, Dept, IsHoliday)
      prepped_recipe <- prep(my_recipe)
      tst <- bake(prepped_recipe, new_data=dept_test)
      
      my_model <- rand_forest(mtry=3,
                              trees=100,
                              min_n=5) %>%
        set_engine("ranger") %>%
        set_mode("regression")
      
      my_wf <- workflow() %>%
        add_recipe(my_recipe) %>%
        add_model(my_model) %>%
        fit(dept_train)
      
      preds <- dept_test %>%
        transmute(Id=paste(Store, Dept, Date, sep="_"),
                  Weekly_Sales=predict(my_wf, new_data = .) %>%
                    pull(.pred))
      
    }
    
    ## Bind predictions together
    all_preds <- bind_rows(all_preds,
                           preds)
    
    ## Print out Progress
    cntr <- cntr+1
    cat("Store", store, "Department", dept, "Completed.",
        round(100 * cntr / n_storeDepts, 1), "% overall complete.\n")
    
  } ## End Dept Loop
  
} ## End Store Loop

## Write out after each store so I don't have to start over
vroom_write(x=all_preds,
            file=paste0("./HeatonPredictions.csv"), delim=",")
