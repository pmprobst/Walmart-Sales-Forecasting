

#join store.csv data in or bring it in as a lookup table

#Feature engineering
##number of days till holiday
##type of hoiday

library(vroom)
library(dplyr)

train_data <- vroom("data/train.csv")
test_data <- vroom("data/test.csv")

features_df <- vroom("data/features.csv")
store_df <- vroom("data/stores.csv")

train_data <- left_join(train_data, store_df, by = "Store")
train_data <- left_join(train_data, features_df, by = c("Store" ,"Date"))
Vview()