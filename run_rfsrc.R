library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(mlr3proba)
library(mlr3extralearners)
library(mlr3fselect)
library(mlr3misc)
library(tidyverse)
library(broom)
library(caret)
validation_df <- readRDS("data/mice_pre2018_validation.Rds")
holdout_df <- readRDS("data/mice_post2018_holdout.Rds")

## dummy encode
dmy_validation <- dummyVars(" ~ .", data = validation_df)
validation_df <- data.frame(predict(dmy_validation, newdata = validation_df))

dmy_holdout <- dummyVars(" ~ .", data = holdout_df)
holdout_df <- data.frame(predict(dmy_validation, newdata = holdout_df))

task_val <- TaskSurv$new(
  "rfsrc_val",
  validation_df,
  time = "DeathPDays",
  event = "death_1yr"
)

task_holdout <- TaskSurv$new(
  "rfsrc_holdout",
  holdout_df,
  time = "DeathPDays",
  event = "death_1yr"
)

features <- task_val$feature_names

#surv.brier is possible
measures = msrs(c("surv.cindex"))
resampling = rsmp("repeated_cv", folds = 5, repeats = 5)
learner_tune = lrn("surv.rfsrc", predict_type = "distr",
                   mtry = to_tune(seq(10, 100, by = 10)),
                   nodesize = to_tune(seq(10, 100, by = 10)))

instance = tune(
  task = task,
  learner = learner_tune,
  resampling = resampling,
  measures = measures,
  tuner = tnr("random_search"),
  term_evals = 1
)

# best performing hyperparameter configuration
instance$result

# learner$param_set$values = instance$result_learner_param_vals
learner = lrn("surv.rfsrc", predict_type = "distr",
              importance = "TRUE",
              # mtry = instance$result_learner_param_vals[[1]]$mtry,
              # nodesize = instance$result_learner_param_vals[[1]]$nodesize,
              mtry = 10,
              nodesize = 30)
learner$train(task_val)

prediction = learner$predict(task_holdout)
prediction

prediction$score(measures)

# all evaluated hyperparameter configuration
as.data.table(instance$archive)
# 
# learner$param_set$values = list(ntree = 500, 
#                                 importance = "TRUE",
#                                 nsplit = 5,
#                                 mtry = 40,
#                                 nodesize = 40)

# d = benchmark_grid(tasks = task, learners = learner, resamplings = resampling)
# 
# bmr = mlr3::benchmark(design = d,
#                       store_models = TRUE)

# need features and their importance, each column is an interaction, each row is a var
#each model meta info
rr <- mlr3::resample(task, learner, resampling, store_models = TRUE)

# cindex, one per iteration
write_csv(tibble(RFSRC.scale = rr$score(measure = msr("surv.cindex"))$surv.cindex) %>%
            mutate(x1 = row_number()),
          "rfsrc_1_perf_.csv")

rfsrc_importance <- map(rr$learners, function(x) x$model$importance)

rfsrc_importance_df <- as.data.frame(rfsrc_importance) %>%
  setNames(paste0("RFSRC.scale-", seq(1:25))) %>%
  rownames_to_column(var = "x1")
write_csv(rfsrc_importance_df, "rfsrc_1_featsel_.csv") 

rfsrc_importance_df_agg <- rfsrc_importance_df %>% 
  mutate(RFSRC.scale.mean = rowMeans(dplyr::select(., `RFSRC.scale-1`:`RFSRC.scale-25`)),
         RFSRC.scale.sd = apply(rfsrc_importance_df %>% dplyr::select(., `RFSRC.scale-1`:`RFSRC.scale-25`), 1, sd)) %>%
  select(-c(`RFSRC.scale-1`:`RFSRC.scale-25`))
write_csv(rfsrc_importance_df_agg, "rfsrc_1_featsel_aggr_.csv")
