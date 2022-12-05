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











# so far we stopped here, no recipes used

# old version without creating dummies
discuss_recipe <- recipe(discuss_GW ~ ., 
                  df) %>%
  # do we need next line 
  step_log()

sum_variables <-
  discuss_recipe %>%
  summary()

discuss_recipe %>% 
  prep() %>% 
  bake(new_data = NULL) %>% 
  #select(starts_with("city")) %>%
  names() # level "anything" is the reference level

# create workflow
discuss_wkfl <- workflow() %>% 
  add_model(logistic_model) %>% 
  add_recipe(discuss_recipe)

# run workflow
discuss_wkfl_fit <- discuss_wkfl %>% 
  last_fit(split = df_split)

discuss_wkfl_fit %>% 
  collect_metrics()




# new version with  dummies
discuss_recipe_dummies <- recipe(discuss_GW ~ ., 
                  df) %>%
  # TODO next 3 lines are new
  step_corr(all_numeric(), threshold = 0.8) %>%
  step_normalize(all_numeric()) %>%
  step_dummy(all_nominal(),all_factor(), -all_outcomes()) %>% 
# do we need next line
step_log()

sum_variables <-
  discuss_recipe_dummies %>%
  summary()

discuss_recipe_dummies %>% 
  prep() %>% 
  bake(new_data = NULL) %>% 
  #select(starts_with("city")) %>%
  names() # level "anything" is the reference level

# create workflow
discuss_wkfl_dummies <- workflow() %>% 
  add_model(logistic_model) %>% 
  add_recipe(discuss_recipe_dummies)

# run workflow
discuss_wkfl_fit_dummies <- discuss_wkfl_dummies %>% 
  last_fit(split = df_split)

discuss_wkfl_fit_dummies %>% 
  collect_metrics()


# hyperparameter tuning
dt_tune_model <- decision_tree(cost_complexity = tune(),
                               tree_depth = tune (),
                               min_n = tune ()) %>% 
  set_engine('rpart') %>%
  set_mode('classification')
  
dt_tune_model

discussion_tune_wkfl <- discuss_wkfl_dummies %>% 
  update_model(dt_tune_model)

discussion_tune_wkfl

set.seed(214)
dt_grid <- grid_random(parameters(discussion_tune_wkfl),
            size = 5)


discussion_folds <- vfold_cv(df, # should this only be training data? "df_training"
                            v = 10,
                            strata = discuss_GW)

dt_tuning <- discussion_tune_wkfl %>% 
  tune_grid(resamples = discussion_folds, 
            grid = dt_grid,
            metrics = custom_metrics) # maybe need to use different metrics



dt_tuning %>% 
  collect_metrics()


dt_tuning %>% 
  show_best()


# TODO 
# check if only training data should be used in tuning (probably yes)
# add more metrics
# find out what exactly the tuning process did and how we can interpret the findings
# would this also work with other model than decision tree?
# what is the difference between glm and rpart? 
# do predictions with test data




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

