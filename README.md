# Survival Machine Learning Methods for Mortality Prediction After Heart Transplantation in the Contemporary Era 


This repository is the official implementation. 

## Requirements

To install packages:

```setup
install.packages(c("tidyverse", "survival", "mlr", "mlrCPOR", "pryr", "checkmate", "mice", "Hmisc", "BBmisc", "testthat", "survAUC", "xgboost", "gbm", "randomForestSRC", "glmnet", "mlr3", "mlr3learners", "mlr3tuning", "mlr3proba", "mlr3extralearners", "mlr3fselect", "mlr3misc", "caret", "broom"))
```

## Data Preprocessing
The data are available from SRTR upon request by emailing srtr@srtr.org (More information can be found at https://www.srtr.org/requesting-srtr-data/data-requests/). SRTR is a third-party organization that owns the data, which is why we are not allowed to share data per the Data Use Agreement that we signed with SRTR. The authors confirm they did not have any special privileges that other authors would not have.

Thus, we unfortunately cannot share our data preprocessing script. Please feel free to reach out (lathan.liou@icahn.mssm.edu) if you require assistance.

## Running the pipeline

**Step 1. Set up your config file. Important columns are:**

| Variable | Definition |
| :---:   | :---: | 
| ResultFile | path where output will be saved |
| DataFile | path of your data (.Rds format) |
| TimeVar | name of your time variable |
| StatusVar | name of your censor variable |
| ActiveLearner | see survival_const.R for list of available learners |
| Categoricals | comma-separated list of categorical variables in your data. Code will convert them to dummy variables |

Note: If running xgboost, change line 75 in RLearner_surv_xgboost.R to your time and status variables

**Step 2. Run the benchmark pipeline.**

1st argument is name of config file

2nd argument is result index, you can leave as 1

3rd argument is impute TRUE/FALSE

4th argumnet is exclude variables listed in Exclusions in config file TRUE/FALSE

5th argument is type of resampling, 1 = nested CV, 0 = single CV

```eval
Rscript benchmark.R config_post2018.csv 1 FALSE TRUE 1
```

**Step 2b. Running Random Survival Forests**

There have been some bugs where the randomForestSRC package isn't fully supported by the previous mlr framework. Thus, the analysis needs to be run separately. The run_rfsrc.R script contains all relevant code.

**Step 3. Test model performance on hold-out set**

Run build_and_test_final_models.R

**Step 4. Create figures and tables of analysis results**

Run make_figures.Rmd. Note that you may have to change your file paths to point to where you saved the model outputs.