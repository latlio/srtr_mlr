# Survival Machine Learning Methods for Mortality Prediction Under New 2018 Donor Heart Allocation System: An Exploratory Benchmarking


This repository is the official implementation. 

## Requirements

To install packages:

```setup
install.packages(c("tidyverse", "survival", "mlr", "mlrCPOR", "pryr", "checkmate", "mice", "Hmisc", "BBmisc", "testthat", "survAUC", "xgboost", "gbm", "randomForestSRC", "glmnet"))
```

## Data Preprocessing
The data are available from SRTR upon request by emailing srtr@srtr.org (More information can be found at https://www.srtr.org/requesting-srtr-data/data-requests/). SRTR is a third-party organization that owns the data, which is why we are not allowed to share data per the Data Use Agreement that we signed with SRTR. The authors confirm they did not have any special privileges that other authors would not have.

Thus, we unfortunately cannot share our data preprocessing script. Please feel free to reach out if you require assistance.

## Running the pipeline

Step 1
Set up your config file. Important columns are:

| Variable | Definition |
| :---:   | :---: | 
| ResultFile | path where output will be saved |
| DataFile | path of your data (.Rds format) |
| TimeVar | name of your time variable |
| StatusVar | name of your censor variable |
| ActiveLearner | see survival_const.R for list of available learners |
| Categoricals | comma-separated list of categorical variables in your data. Code will convert them to dummy variables |

Note: If running xgboost, change line 75 in RLearner_surv_xgboost.R to your time and status variables

Step 2
Run the benchmark pipeline.

1st argument is name of config file
2nd argument is result index, you can leave as 1
3rd argument is impute TRUE/FALSE
4th argumnet is exclude variables listed in Exclusions in config file TRUE/FALSE
5th argument is type of resampling, 1 = nested CV, 0 = single CV

```eval
Rscript benchmark.R config_post2018.csv 1 FALSE TRUE 1
```
