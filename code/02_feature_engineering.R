library(tidyverse)
library(parsnip)
library(tidymodels)

df_training <- readRDS("data/df_training.rds")
df_testing <- readRDS("data/df_testing.rds")

# model specification
logistic_model <- logistic_reg() %>%
  #set_engine('stan') %>%
  set_engine('glm') %>%
  set_mode('classification')

# fit model 
lm_fit <- logistic_model %>%
  fit(discuss_GW ~ ., data = df_training)

# get parameters
tidy(lm_fit) %>% print (n=30)

# make predictions
df_predictions <- lm_fit %>%
  predict(new_data = df_testing)

# many NAs, probably because model returns NA when predictors are NA
questionr::freq(df_training$discuss_GW)
questionr::freq(df_predictions)

# add predicted values to dataset 
df_test_results <- df_testing %>%
  select(everything()) %>%
  bind_cols(df_predictions)

# evaluate model performance


