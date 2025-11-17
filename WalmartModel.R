library(vroom)
library(dplyr)

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



###FEATURE ENGINEERING###