library(tidyverse)
library(parsnip)
library(tidymodels)

#df_training <- readRDS("data/df_training.rds")
#df_testing <- readRDS("data/df_testing.rds")
df <- read_rds('data/df.rds')

df_split <- rsample::initial_split(df, prop = 0.75, strata = discuss_GW)

# model specification
logistic_model <- logistic_reg() %>%
  #set_engine('stan') %>%
  set_engine('glm') %>%
  set_mode('classification')

lm_last_fit <- 
  logistic_model %>%
  last_fit(discuss_GW ~ ., split = df_split)

results <- 
lm_last_fit %>%
  collect_predictions()

custom_metrics <-
  metric_set(accuracy, sens) #, spec, roc_auc)

custom_metrics(results, 
               truth = discuss_GW, 
               estimate = .pred_class)

conf_mat(results, 
               truth = discuss_GW, 
               estimate = .pred_class) %>%
  autoplot(type = 'heatmap')

results %>%
roc_curve(truth = discuss_GW,
          .pred_0) %>%
  autoplot()

results %>%
  roc_auc(truth = discuss_GW,
            .pred_0)

custom_metrics(results,
               truth = discuss_GW,
               estimate = .pred_class,
               .pred_0)

discuss <- recipe(discuss_GW ~ ., 
                  df) %>%
  step_log()

sum_variables <-
discuss %>%
  summary()

########
# stepwise procedure


# fit model 
lm_fit <- logistic_model %>%
  fit(discuss_GW ~ ., data = df_training)

# get parameters
tidy(lm_fit) %>% print(n=30)

# make classification predictions
df_pred_class <- lm_fit %>%
  predict(new_data = df_testing,
          type = 'class')

# make probability predictions
df_pred_prob <- lm_fit %>%
  predict(new_data = df_testing,
          type = 'prob')

# many NAs, probably because model returns NA when predictors are NA
questionr::freq(df_training$discuss_GW)

# add predicted values to dataset 
df_test_results <- df_testing %>%
  select(discuss_GW) %>%
  bind_cols(df_pred_class, df_pred_prob)

# evaluate model performance
sum(is.na(df_test_results$.pred_class))

