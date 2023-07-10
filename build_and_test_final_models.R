# Build final models on validation set

library(tidyverse)
library(broom)
library(caret)
library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(mlr3proba)
library(mlr3extralearners)
library(mlr3fselect)
library(mlr3misc)
build_and_test <- function(validation_set,
                           holdout_set,
                           gbm_tune,
                           xgbt_tune,
                           time_var = "DeathPDays",
                           event_var = "death_1yr") {
  
  df <- readRDS(validation_set)
  holdout <- readRDS(holdout_set)
  dmy <- dummyVars(" ~ .", data = df)
  df <- data.frame(predict(dmy, newdata = df))
  holdout <- data.frame(predict(dmy, newdata = holdout))
  gbm_tune_res <- readRDS(gbm_tune)
  xgbt_tune_res <- readRDS(xgbt_tune)
  
  task = TaskSurv$new(
    "survival",
    df,
    time = time_var,
    event = event_var
  )
  
  task_holdout <- TaskSurv$new(
    "survival_holdout",
    holdout,
    time = time_var,
    event = event_var
  )
  
  features <- task$feature_names
  
  measures = msrs(c("surv.cindex"))
  
  learner_cox = lrn("surv.coxph")
  
  learner_lasso = lrn("surv.glmnet",
                      alpha = 1)
  
  learner_ridge = lrn("surv.glmnet",
                      alpha = 0)
  
  learner_eln = lrn("surv.glmnet",
                    alpha = 0.5)
  
  learner_gbm = lrn("surv.gbm",
                    interaction.depth = gbm_tune_res %>% 
                      arrange(desc(cindex.test.mean)) %>%
                      slice(1) %>% 
                      .$interaction.depth,
                    shrinkage = gbm_tune_res %>% 
                      arrange(desc(cindex.test.mean)) %>%
                      slice(1) %>% 
                      .$shrinkage)
  
  learner_xgbl = lrn("surv.xgboost",
                     booster = "gblinear")
  
  learner_xgbt = lrn("surv.xgboost",
                     booster = "gbtree",
                     alpha = 0,
                     nrounds = 10,
                     eta = 0.1,
                     gamma = 0,
                     max_depth = xgbt_tune_res %>% 
                       arrange(desc(cindex.test.mean)) %>%
                       slice(1) %>% 
                       .$max_depth,
                     min_child_weight = xgbt_tune_res %>% 
                       arrange(desc(cindex.test.mean)) %>%
                       slice(1) %>% 
                       .$min_child_weight,
                     subsample = 0.8,
                     colsample_bytree = 0.8)
  
  learner_cox$train(task)
  # learner_cox$model %>%
  #   tidy(exponentiate = TRUE,
  #        conf.int = TRUE) %>%
  #   filter(p.value < 0.05) %>% 
  #   arrange(desc(estimate)) %>%
  #   slice_head(n = 10) %>% 
  #   ggplot(aes(x = term, y = estimate, col = term)) +
  #   geom_point() +
  #   geom_linerange(aes(ymin = conf.low, ymax = conf.high)) + 
  #   labs(x = "Variable",
  #        y = "Hazard Ratio") +
  #   coord_flip() +
  #   theme_bw() + 
  #   theme(legend.position = "none")
    
  prediction_cox = learner_cox$predict(task_holdout)

  learner_lasso$train(task)
  prediction_lasso = learner_lasso$predict(task_holdout)
  
  learner_ridge$train(task)
  prediction_ridge = learner_ridge$predict(task_holdout)
  
  learner_eln$train(task)
  prediction_eln = learner_eln$predict(task_holdout)
  
  learner_gbm$train(task)
  prediction_gbm = learner_gbm$predict(task_holdout)
  
  learner_xgbl$train(task)
  prediction_xgbl = learner_xgbl$predict(task_holdout)
  
  learner_xgbt$train(task)
  prediction_xgbt = learner_xgbt$predict(task_holdout)
  
  tibble(model = c("cox", "ridge", "lasso", "eln", "gbm", "xgbl", "xgbt"),
         test_cindex = c(prediction_cox$score(measures),
                         prediction_ridge$score(measures),
                         prediction_lasso$score(measures),
                         prediction_eln$score(measures),
                         prediction_gbm$score(measures),
                         prediction_xgbl$score(measures),
                         prediction_xgbt$score(measures)))
}

build_and_test("data/mice_post2018_validation.Rds",
               "data/mice_post2018_holdout.Rds",
               "data/gbm_post_tune_results.Rds",
               "data/xgbt_post_tune_results.Rds")

build_and_test("data/mice_pre2018_validation.Rds",
               "data/mice_pre2018_holdout.Rds",
               "data/gbm_pre_tune_results.Rds",
               "data/xgbt_pre_tune_results.Rds")

build_and_test("data/mice_pre2018_validation.Rds",
               "data/mice_post2018_holdout.Rds",
               "data/gbm_pre_tune_results.Rds",
               "data/xgbt_pre_tune_results.Rds")
