library(survival)
#library(survivalsvm)
library(Hmisc)
library(mlr)
library(mlrCPO)
library(pryr)
library(checkmate)
library(BBmisc)
library(mice)
library(purrr)
library(stringr)

code_dir = "mlr_code/"
source(paste0(code_dir, "stability.R"))
source(paste0(code_dir, "performance.R"))
source(paste0(code_dir, "features.R"))

# dataset <- readRDS("mice_clean_elizabeth_df_post2018_full.Rds")
# task_id <- "TEST"
# time_var <- "Pers_DeathPDays"
# status_var <- "Pers_DeathCensor"
# NUM_FOLDS <- 5

survival_tests = function(task_id, dataset, result_file, time_var, status_var, active_learners = LRN_ALL, run_stability = FALSE, impute = TRUE, rpt = 0) 
{  
  #--------------------------------------------------------------------------
  # INITIALISATION
  #--------------------------------------------------------------------------
  print(Sys.time())
  configureMlr(show.learner.output = TRUE, on.learner.error = 'warn')	
  
  #-----------------------------------------------------------------------------------------------------------------------------
  # THE TASK - I.E. DATSET
  #-----------------------------------------------------------------------------------------------------------------------------
  dataset = dataset[ , !(names(dataset) %in% c("ID"))]
  surv.task = makeSurvTask(id = task_id, data = dataset, target = c(time_var, status_var))
  # ohe.task = surv.task %>>% cpoDummyEncode() %>>% cpoModelMatrix(~ 0 + .^2)
  #Features: 105 -> 175 -> 255
  ohe.task = surv.task %>>% cpoDummyEncode()
  # ohe.task = surv.task %>>% cpoDummyEncode() %>>% cpoModelMatrix(~ 0 + .^2)
  # ohe.task = surv.task %>>% cpoDummyEncode() %>>% cpoModelMatrix(~ 0 + . + Dialysis_Baseline:DM_Baseline + Dialysis_Baseline:REC_TOT_BILI + DM_Baseline:REC_TOT_BILI + REC_ECMO:Dialysis_Baseline + REC_ECMO:DM_Baseline + REC_ECMO:REC_LIFE_SUPPORT + REC_ECMO:REC_TOT_BILI + REC_ECMO:REC_VENTILATOR_SUPPORT + REC_ECMO:VAD + REC_LIFE_SUPPORT:Dialysis_Baseline + REC_LIFE_SUPPORT:DM_Baseline + REC_LIFE_SUPPORT:REC_TOT_BILI + REC_LIFE_SUPPORT:REC_VENTILATOR_SUPPORT + REC_VENTILATOR_SUPPORT:Dialysis_Baseline + REC_VENTILATOR_SUPPORT:DM_Baseline + REC_VENTILATOR_SUPPORT:REC_TOT_BILI + VAD:Dialysis_Baseline + VAD:DM_Baseline + VAD:REC_LIFE_SUPPORT + VAD:REC_TOT_BILI + VAD:REC_VENTILATOR_SUPPORT)
  
  # ohe.task = surv.task %>>% cpoDummyEncode() %>>% cpoModelMatrix(~ 0 + . + Dialysis_Baseline:CARDIAC_ETIOLOGY3 + Dialysis_Baseline:DM_Baseline + Dialysis_Baseline:REC_FUNCTIONAL_STATUS4 + Dialysis_Baseline:REC_GRAFT_STAT + Dialysis_Baseline:REC_IMMUNO_MAINT_MEDS + Dialysis_Baseline:REC_TOT_BILI + DM_Baseline:CARDIAC_ETIOLOGY3 + DON_AGE:CARDIAC_ETIOLOGY3 + DON_AGE:Dialysis_Baseline + DON_AGE:DM_Baseline + DON_AGE:REC_AGE_AT_TX + DON_AGE:REC_ECMO + DON_AGE:REC_FUNCTIONAL_STATUS4 + DON_AGE:REC_GRAFT_STAT + DON_AGE:REC_IMMUNO_MAINT_MEDS + DON_AGE:REC_LIFE_SUPPORT + DON_AGE:REC_TOT_BILI + DON_AGE:REC_VENTILATOR_SUPPORT + DON_AGE:VAD + REC_AGE_AT_TX:CARDIAC_ETIOLOGY3 + REC_AGE_AT_TX:Dialysis_Baseline + REC_AGE_AT_TX:DM_Baseline + REC_AGE_AT_TX:REC_ECMO + REC_AGE_AT_TX:REC_FUNCTIONAL_STATUS4 + REC_AGE_AT_TX:REC_GRAFT_STAT + REC_AGE_AT_TX:REC_IMMUNO_MAINT_MEDS + REC_AGE_AT_TX:REC_LIFE_SUPPORT + REC_AGE_AT_TX:REC_TOT_BILI + REC_AGE_AT_TX:REC_VENTILATOR_SUPPORT + REC_AGE_AT_TX:VAD + REC_ECMO:CARDIAC_ETIOLOGY3 + REC_ECMO:Dialysis_Baseline + REC_ECMO:DM_Baseline + REC_ECMO:REC_FUNCTIONAL_STATUS4 + REC_ECMO:REC_GRAFT_STAT + REC_ECMO:REC_IMMUNO_MAINT_MEDS + REC_ECMO:REC_LIFE_SUPPORT + REC_ECMO:REC_TOT_BILI + REC_ECMO:REC_VENTILATOR_SUPPORT + REC_ECMO:VAD + REC_FUNCTIONAL_STATUS4:CARDIAC_ETIOLOGY3 + REC_FUNCTIONAL_STATUS4:DM_Baseline + REC_GRAFT_STAT:CARDIAC_ETIOLOGY3 + REC_GRAFT_STAT:DM_Baseline + REC_GRAFT_STAT:REC_FUNCTIONAL_STATUS4 + REC_IMMUNO_MAINT_MEDS:CARDIAC_ETIOLOGY3 + REC_IMMUNO_MAINT_MEDS:DM_Baseline + REC_IMMUNO_MAINT_MEDS:REC_FUNCTIONAL_STATUS4 + REC_IMMUNO_MAINT_MEDS:REC_GRAFT_STAT + REC_LIFE_SUPPORT:CARDIAC_ETIOLOGY3 + REC_LIFE_SUPPORT:Dialysis_Baseline + REC_LIFE_SUPPORT:DM_Baseline + REC_LIFE_SUPPORT:REC_FUNCTIONAL_STATUS4 + REC_LIFE_SUPPORT:REC_GRAFT_STAT + REC_LIFE_SUPPORT:REC_IMMUNO_MAINT_MEDS + REC_LIFE_SUPPORT:REC_TOT_BILI + REC_LIFE_SUPPORT:REC_VENTILATOR_SUPPORT + REC_TOT_BILI:CARDIAC_ETIOLOGY3 + REC_TOT_BILI:DM_Baseline + REC_TOT_BILI:REC_FUNCTIONAL_STATUS4 + REC_TOT_BILI:REC_GRAFT_STAT + REC_TOT_BILI:REC_IMMUNO_MAINT_MEDS + REC_VENTILATOR_SUPPORT:CARDIAC_ETIOLOGY3 + REC_VENTILATOR_SUPPORT:Dialysis_Baseline + REC_VENTILATOR_SUPPORT:DM_Baseline + REC_VENTILATOR_SUPPORT:REC_FUNCTIONAL_STATUS4 + REC_VENTILATOR_SUPPORT:REC_GRAFT_STAT + REC_VENTILATOR_SUPPORT:REC_IMMUNO_MAINT_MEDS + REC_VENTILATOR_SUPPORT:REC_TOT_BILI + VAD:CARDIAC_ETIOLOGY3 + VAD:Dialysis_Baseline + VAD:DM_Baseline + VAD:REC_FUNCTIONAL_STATUS4 + VAD:REC_GRAFT_STAT + VAD:REC_IMMUNO_MAINT_MEDS + VAD:REC_LIFE_SUPPORT + VAD:REC_TOT_BILI + VAD:REC_VENTILATOR_SUPPORT)
  
  # So that we know the  number of features after one-hot encoding and their names 
  task.names = getTaskFeatureNames(ohe.task)
  num_features = getTaskNFeats(ohe.task)
  print(paste0("Num Features: ", num_features))
  # print(paste0(task.names))
  
  if (num_features == 0) {
    print(Sys.time())
    print("Returning ...")
    return(NULL)
  }
  
  # for ADNI data, remove class "labelled"
  # ind = which(sapply(dataset, function(y) inherits(y, "labelled")))
  # if (length(ind) > 0)
  # 	dataset[ind] = lapply(dataset[ind], as.numeric)
  # feature_types <- sapply(dataset, function(feature) paste(class(feature), collapse = "_"))
  # if (any(!is.element(feature_types, c("integer", "numeric", "ordered_factor", "factor", "Surv")))) {
  # 	print("data columns must be either of numeric, ordered factor or Surv type")
  # 	print(feature_types[!is.element(feature_types, c("integer", "numeric", "ordered_factor", "factor", "Surv"))])
  # }
  
  #-----------------------------------------------------------------------------------------------------------------------------
  # DATA STRUCTURES TO COLLECT RESULTS
  #-----------------------------------------------------------------------------------------------------------------------------	
  if (rpt == 0)
    perf  = new_performance(NUM_FOLDS)
  else
    perf  = new_performance(NUM_FOLDS * NUM_ITERS)
  
  feats = new_features(ohe.task)
  stab  = new_stability()
  
  #--------------------------------------------------------
  # TUNING LIMITS
  #--------------------------------------------------------
  psqrt = round(sqrt(num_features))
  tune_lowerlim = 5
  tune_upperlim = round(num_features/3)
  if (tune_upperlim < tune_lowerlim) {
    tune_upperlim = num_features
    tune_lowerlim = 0
  }
  #	tune_upperlim = min(round(num_features/2), MAX_FEATURES)
  #	tune_seq = seq(from = tune_lowerlim, to = tune_upperlim, by = 5)
  #	ctrl = makeTuneControlRandom(maxit = 5)
  ctrl = makeTuneControlRandom(maxit = NUM_ITERS_TUNE)
  #	ctrl = makeTuneControlGrid(resolution = 5L)
  
  #-----------------------------------------------------------------------------------------------------------------------------
  # PERFORMANCE MEASURES
  #-----------------------------------------------------------------------------------------------------------------------------
  test.mean_narm = makeAggregation(
    id = "test.mean_narm",
    name = "Test mean with NA removed",
    properties = "req.test",
    fun = function(task, perf.test, perf.train, measure, group, pred) mean(perf.test, na.rm = TRUE)
  )
  
  test.sd_narm = makeAggregation(
    id = "test.sd_narm",
    name = "Test sd with NA removed",
    properties = "req.test",
    fun = function(task, perf.test, perf.train, measure, group, pred) sd(perf.test, na.rm = TRUE)
  )
  
  cindex.na = setAggregation(cindex, test.mean_narm)
  cindex.sdna = setAggregation(cindex, test.sd_narm)
  cindex.uno.na = setAggregation(cindex.uno, test.mean_narm)
  cindex.uno.sdna = setAggregation(cindex.uno, test.sd_narm)
  # brier.na = setAggregation(ibrier, test.mean_narm)
  # brier.sdna = setAggregation(ibrier, test.sd_narm)
  surv.measures = list(cindex.na, cindex.sdna, cindex.uno.na, cindex.uno.sdna)
  
  #-----------------------------------------------------------------------------------------------------------------------------
  # RESAMPLING STRATEGIES
  #-----------------------------------------------------------------------------------------------------------------------------
  if (run_stability) NUM_ITERS = NUM_ITERS_STAB
  inner = makeResampleDesc("RepCV", reps = NUM_ITERS, folds = NUM_FOLDS, stratify = TRUE)	# Benchmarking: 5-fold CV repeated 5 times
  outer = makeResampleDesc("RepCV", reps = NUM_ITERS, folds = NUM_FOLDS, stratify = TRUE)	# Benchmarking: 5-fold CV repeated 5 times
  single = makeResampleDesc("CV", iters = NUM_ITERS, stratify = TRUE)											# Single run of 5-fold CV
  tuning = makeResampleDesc("CV", iters = NUM_ITERS_TUNE, stratify = TRUE)								# Tuning: 5-fold CV, no repeats
  stabsel = makeResampleDesc("Subsample", iters = NUM_ITERS, split = 1/2, stratify = TRUE)	# Stability selection: 100 iterations of subsampling
  
  #-----------------------------------------------------------------------------------------------------------------------------
  # PRE-PROCESSING
  #-----------------------------------------------------------------------------------------------------------------------------
  
  # CPO to run imputation using mice within the CV loop
  cpoMice = makeCPO("mice", # nolint
                    dataformat = "df.features",
                    properties.data = c("numerics", "factors", "ordered", "missings"),
                    properties.adding = "missings",
                    cpo.train = function(data, target) { 
                      # Remove constant and colinear features
                      init <- mice(data, maxit=0)
                      # flux_r1 <- flux(data) %>%
                      #   tibble::rownames_to_column(var = "variable")
                      # 
                      # out_r1 <- flux_r1 %>%
                      #   filter(outflux < 0.5) %>%
                      #   pull(variable)
                      # 
                      # flux_r2 <- flux(data %>%
                      #                   select(-c(all_of(out_r1)))) %>%
                      #   tibble::rownames_to_column(var = "variable")
                      # 
                      # out_r2 <- flux_r2 %>%
                      #   filter(outflux < 0.5) %>%
                      #   pull(variable)
                      # 
                      # flux_r3 <- flux(data %>%
                      #                   select(-c(all_of(out_r1)),
                      #                          -c(all_of(out_r2)))) %>%
                      #   tibble::rownames_to_column(var = "variable")
                      # 
                      # out_r3 <- flux_r3 %>%
                      #   filter(outflux < 0.5) %>%
                      #   pull(variable)
                      # 
                      # all_out <- unique(c(out_r1, out_r2, out_r3))
                      # 
                      # data2 <- data %>%
                      #   select(-all_of(all_out))
                      
                      predmat = mice::quickpred(data, minpuc = 0, mincor = 0.1,
                                                method = "spearman")
                      
                      return(predmat)
                    }, 
                    cpo.retrafo = function(data, control) {
                      predmat = control
                      
                      imp_data = tryCatch({
                        mice::mice(data, m=5,  method="cart", pred=predmat, seed = 23109, print = TRUE)
                      }, 
                      error = function(cond) {
                        print(paste("mice::mice returned error: ", cond))
                        return(data)	
                      })
                      
                      imputed = mice::complete(imp_data)
                      return(imputed)
                    }
  )
  
  #-----------------------------------------------------------------------------------------------------------------------------
  # BASE LEARNERS - METHODS WITH EMBEDDED FEATURE SELECTION - INHERENTLY SUITED TO HIGH_DIM DATA
  #-----------------------------------------------------------------------------------------------------------------------------	
  rfsrc_params1 = makeParamSet(
    makeIntegerParam("mtry", lower = round(psqrt/2), upper = psqrt*2),
    makeIntegerParam("nodesize", lower = 1, upper = 20)
    #			makeIntegerParam("nodedepth", lower = 1, upper = 20)	
  )
  rfsrc_params2 = makeParamSet(
    makeDiscreteParam("mtry", values = seq(from = 10, to = 120, by = 10)),
    makeDiscreteParam("nodesize", values = c(5,seq(from = 10, to = 100, by = 10)))
  )
  ranger_params = makeParamSet(
    makeIntegerParam("mtry", lower = round(psqrt/2), upper = psqrt*2),
    makeIntegerParam("min.node.size", lower = 5, upper = 50)
  )
  xgbtree_params = makeParamSet(
    makeIntegerParam("nrounds", lower = 10, upper = 25),
    makeIntegerParam("max_depth", lower = 1, upper = 10),
    makeNumericParam("eta", lower = .01, upper = .4)
  )	
  xgblinear_params = makeParamSet(
    makeNumericParam("lambda", lower = 0, upper = 50)
  )
  ssvm1_params = makeParamSet(
    makeDiscreteParam("type", values = c("vanbelle1")),
    makeDiscreteParam("kernel", values = c("lin_kernel")),
    makeDiscreteParam("diff.meth", values = c("makediff3")),
    makeDiscreteParam("gamma.mu", values = 2^(-5L:-1L))
  )
  ssvm2_params = makeParamSet(
    makeDiscreteParam("type", values = c("vanbelle2")),
    makeDiscreteParam("kernel", values = c("lin_kernel")),
    makeDiscreteParam("diff.meth", values = c("makediff3")),
    makeDiscreteParam("gamma.mu", values = 2^(-5L:-1L))
  )
  
  base_learners = list(
    "coxph" = list("class" = "surv.coxph",
                   "code" = LRN_COX,
                   "name" = "CoxPH",
                   "tune_params" = NULL,
                   "args" = list(x = TRUE)),
    "lasso" = list("class" = "surv.cvglmnet",
                   "code" = LRN_LASSO,
                   "name" = "Lasso",
                   "tune_params" = NULL,
                   "args" = list(alpha = 1, nfolds = 5)),
    "ridge" = list("class" = "surv.cvglmnet",
                   "code" = LRN_RIDGE,
                   "name" = "Ridge",
                   "tune_params" = NULL,
                   "args" = list(alpha = 0, nfolds = 5)),
    "elasticnet" = list("class" = "surv.cvglmnet",
                        "code" = LRN_ELASTICNET,
                        "name" = "ElasticNet",
                        "tune_params" = NULL,
                        "args" = list(alpha = 0.5, nfolds = 5)),
    "coxboost" = list("class" = "surv.gbm",
                      "code" = LRN_COXBOOST,
                      "name" = "CoxBoost",
                      "tune_params" = NULL,
                      "args" = NULL),
    "glmboost" = list("class" = "surv.glmboost",
                      "code" = LRN_GLMBOOST,
                      "name" = "GLMBoost",
                      "tune_params" = NULL,
                      "args" = NULL),
    "xgbtree" = list("class" = "surv.xgboost",
                     "code" = LRN_XGB_TREE,
                     "name" = "XGBTree",
                     # "tune_params" = xgbtree_params,
                     tune_params = NULL,
                     "args" = list(booster = BOOSTER_TREE, alpha = 0,
                                   nrounds = 19,
                                   max_depth = 5,
                                   eta = 0.144)),
    "xgblinear" = list("class" = "surv.xgboost",
                       "code" = LRN_XGB_LINEAR,
                       "name" = "XGBLinear",
                       "tune_params" = xgblinear_params,
                       "args" = list(booster = BOOSTER_LINEAR, alpha = 0)),
    "rpart" = list("class" = "surv.rpart",
                   "code" = LRN_RPART,
                   "name" = "Rpart",
                   "tune_params" = NULL,
                   "args" = NULL),
    "rfsrc" = list("class" = "surv.randomForestSRC",
                   "code" = LRN_RFSRC,
                   "name" = "RFSRC",
                   # "tune_params" = rfsrc_params2,
                   tune_params = NULL,
                   "args" = list(ntree = 500, 
                                 importance = TRUE,
                                 nsplit = 5,
                                 mtry = 40,
                                 nodesize = 40)),
    # mtry = 10,
    # nodesize = 100)),
    "ranger" = list("class" = "surv.ranger",
                    "code" = LRN_RANGER,
                    "name" = "Ranger",
                    "tune_params" = ranger_params,
                    "args" = list(splitrule = "maxstat", importance = "permutation", num.trees = 1000))
    #		"ssvm1" = list("class" = "surv.survivalsvm",
    #									"code" = LRN_SSVM_VB1,
    #									"name" = "SSVM VB1",
    #									"tune_params" = ssvm1_params,
    #									"args" = NULL),
    #		"ssvm2" = list("class" = "surv.survivalsvm",
    #									"code" = LRN_SSVM_VB2,
    #									"name" = "SSVM VB2",
    #									"tune_params" = ssvm2_params,
    #									"args" = NULL)
  )
  
  model_results = function(task_id, bmr, model_id, result_file, unwrap = TRUE)
  {
    res = getBMRTuneResults(bmr, as.df = TRUE)
    
    saveRDS(res, "results/tune_results.Rds")
    
    mods = getBMRModels(bmr, learner.ids = c(model_id))
    
    names = list()
    scores = list()
    num_models = length(mods[[task_id]][[model_id]])
    
    for (i in 1:num_models) {
      mod = getLearnerModel(mods[[task_id]][[model_id]][[i]], more.unwrap = unwrap)
      if (is.null(mod)) {
        cat(paste("=======> model i =", i, "is null!!\n"))
      }
      
      if (inherits(mod, "coxph")) {
        pvalues = summary(mod)$coefficients[,5]
        scores[[i]] = pvalues[!is.na(pvalues) & pvalues < 0.05]
        names[[i]] = names(pvalues[!is.na(pvalues) & pvalues < 0.05])
      } else if (inherits(mod, "cv.glmnet")) {
        coef.min = coef(mod, s = mod$lambda.min)
        active.min = which(as.matrix(coef.min) != 0)
        scores[[i]] = as.vector(coef.min[active.min])
        names[[i]] = as.vector(rownames(coef.min)[active.min])
      } else if (inherits(mod, "gbm")) {
        # ind = which(mod$coefficients[mod$stepno+1, ]!=0, arr.ind = T)
        # names[[i]]  = mod$xnames[ind]
        # scores[[i]] = mod$coefficients[mod$stepno+1, ind]				
        scores[[i]] <- summary(mod) %>% arrange(var) %>% pull(rel.inf)
        names[[i]] <- mod$var.names
      } else if (inherits(mod, "glmboost")) {
        coef.nonzero = coef(mod)
        scores[[i]]  = coef.nonzero[2:length(coef.nonzero)]						# 1st column is Intercept - ignore this
        names[[i]]   = names(coef.nonzero[2:length(coef.nonzero)])		# 1st column is Intercept - ignore this
      } else if (inherits(mod, "rfsrc")) {
        #				vimp_data   = randomForestSRC::vimp(mod)	#If I call vimp, results are different
        scores[[i]] = mod$importance
        names[[i]]  = mod$xvar.names
      } else if (inherits(mod, "ranger")) {
        ind = which(mod$variable.importance != 0)
        scores[[i]] = mod$variable.importance[ind]
        names[[i]]  = names(mod$variable.importance)[ind]
      } else if (inherits(mod, "survivalsvm")) {
        names[[i]]  = mod$var.names
        scores[[i]] = rep(1, length(mod$var.names))
      } else if (inherits(mod, "xgb.Booster")) {
        imp_data = xgboost::xgb.importance(model = mod)
        names[[i]]  = imp_data$Feature
        if (mod$params$booster == BOOSTER_TREE) {
          scores[[i]] = imp_data$Gain
        } else {
          scores[[i]] = imp_data$Weight
        }
      } else if (inherits(mod, "WrappedModel")) {
        if (isFailureModel(mod)) {
          cat(paste("=======> model i =", i, "is a FailureModel!!!\n"))
          cat("----> msg: \n")
          cat(getFailureModelMsg(mod))
          cat("\n----> dump: \n")
          cat(getFailureModelDump(mod))
          cat("\n--------------------------\n")
        } else {
          names[[i]] = getFilteredFeatures(mod)
          scores[[i]] = 1
        }
      }
      else if (class(mod) == "character" ) {
        cat("----> Error: \n")
        cat(mod)
        cat("\n--------------------------\n")
      }
    }
    
    perf  = save_performance(perf, model_id, bmr, task_id)
    # print(length(unlist(names)))
    # print(length(unlist(scores)))
    names2 <- map(names, ~str_replace_all(.x, "`", ""))
    scores2 <- imap(scores, ~.x %>% setNames(names2[[.y]]))
    feats = save_features(feats, model_id, names2, scores2, num_models)
    stab  = save_stability(stab, model_id, names2, getTaskNFeats(surv.task))
    
    write_performance(perf, result_file, "")
    write_features(feats, result_file, "")
    write_stability(stab, result_file, "")
    
    return(list("perf" = perf, "feats" = feats, "stab" = stab))
  }
  
  # Run one base method - no feature selection	
  run_learner = function(baselrn, resamp, result_file, impute) 
  {
    lrn = do.call(makeLearner, args = append(list("cl" = baselrn$class, "id" = baselrn$name, "predict.type" = "response"), baselrn$args))
    lrn = cpoScale() %>>% lrn
    # lrn = cpoModelMatrix(~ 0 + . + Dialysis_Baseline:CARDIAC_ETIOLOGY3 + Dialysis_Baseline:DM_Baseline + Dialysis_Baseline:REC_FUNCTIONAL_STATUS4 + Dialysis_Baseline:REC_GRAFT_STAT + Dialysis_Baseline:REC_IMMUNO_MAINT_MEDS + Dialysis_Baseline:REC_TOT_BILI + DM_Baseline:CARDIAC_ETIOLOGY3 + DON_AGE:CARDIAC_ETIOLOGY3 + DON_AGE:Dialysis_Baseline + DON_AGE:DM_Baseline + DON_AGE:REC_AGE_AT_TX + DON_AGE:REC_ECMO + DON_AGE:REC_FUNCTIONAL_STATUS4 + DON_AGE:REC_GRAFT_STAT + DON_AGE:REC_IMMUNO_MAINT_MEDS + DON_AGE:REC_LIFE_SUPPORT + DON_AGE:REC_TOT_BILI + DON_AGE:REC_VENTILATOR_SUPPORT + DON_AGE:VAD + REC_AGE_AT_TX:CARDIAC_ETIOLOGY3 + REC_AGE_AT_TX:Dialysis_Baseline + REC_AGE_AT_TX:DM_Baseline + REC_AGE_AT_TX:REC_ECMO + REC_AGE_AT_TX:REC_FUNCTIONAL_STATUS4 + REC_AGE_AT_TX:REC_GRAFT_STAT + REC_AGE_AT_TX:REC_IMMUNO_MAINT_MEDS + REC_AGE_AT_TX:REC_LIFE_SUPPORT + REC_AGE_AT_TX:REC_TOT_BILI + REC_AGE_AT_TX:REC_VENTILATOR_SUPPORT + REC_AGE_AT_TX:VAD + REC_ECMO:CARDIAC_ETIOLOGY3 + REC_ECMO:Dialysis_Baseline + REC_ECMO:DM_Baseline + REC_ECMO:REC_FUNCTIONAL_STATUS4 + REC_ECMO:REC_GRAFT_STAT + REC_ECMO:REC_IMMUNO_MAINT_MEDS + REC_ECMO:REC_LIFE_SUPPORT + REC_ECMO:REC_TOT_BILI + REC_ECMO:REC_VENTILATOR_SUPPORT + REC_ECMO:VAD + REC_FUNCTIONAL_STATUS4:CARDIAC_ETIOLOGY3 + REC_FUNCTIONAL_STATUS4:DM_Baseline + REC_GRAFT_STAT:CARDIAC_ETIOLOGY3 + REC_GRAFT_STAT:DM_Baseline + REC_GRAFT_STAT:REC_FUNCTIONAL_STATUS4 + REC_IMMUNO_MAINT_MEDS:CARDIAC_ETIOLOGY3 + REC_IMMUNO_MAINT_MEDS:DM_Baseline + REC_IMMUNO_MAINT_MEDS:REC_FUNCTIONAL_STATUS4 + REC_IMMUNO_MAINT_MEDS:REC_GRAFT_STAT + REC_LIFE_SUPPORT:CARDIAC_ETIOLOGY3 + REC_LIFE_SUPPORT:Dialysis_Baseline + REC_LIFE_SUPPORT:DM_Baseline + REC_LIFE_SUPPORT:REC_FUNCTIONAL_STATUS4 + REC_LIFE_SUPPORT:REC_GRAFT_STAT + REC_LIFE_SUPPORT:REC_IMMUNO_MAINT_MEDS + REC_LIFE_SUPPORT:REC_TOT_BILI + REC_LIFE_SUPPORT:REC_VENTILATOR_SUPPORT + REC_TOT_BILI:CARDIAC_ETIOLOGY3 + REC_TOT_BILI:DM_Baseline + REC_TOT_BILI:REC_FUNCTIONAL_STATUS4 + REC_TOT_BILI:REC_GRAFT_STAT + REC_TOT_BILI:REC_IMMUNO_MAINT_MEDS + REC_VENTILATOR_SUPPORT:CARDIAC_ETIOLOGY3 + REC_VENTILATOR_SUPPORT:Dialysis_Baseline + REC_VENTILATOR_SUPPORT:DM_Baseline + REC_VENTILATOR_SUPPORT:REC_FUNCTIONAL_STATUS4 + REC_VENTILATOR_SUPPORT:REC_GRAFT_STAT + REC_VENTILATOR_SUPPORT:REC_IMMUNO_MAINT_MEDS + REC_VENTILATOR_SUPPORT:REC_TOT_BILI + VAD:CARDIAC_ETIOLOGY3 + VAD:Dialysis_Baseline + VAD:DM_Baseline + VAD:REC_FUNCTIONAL_STATUS4 + VAD:REC_GRAFT_STAT + VAD:REC_IMMUNO_MAINT_MEDS + VAD:REC_LIFE_SUPPORT + VAD:REC_TOT_BILI + VAD:REC_VENTILATOR_SUPPORT) %>>% lrn
    model_id = paste0(baselrn$name, '.scale')
    if (!is.null(baselrn$tune_params)) {
      lrn = makeTuneWrapper(lrn, resampling = tuning, par.set = baselrn$tune_params, control = ctrl, show.info = TRUE)
      model_id = paste0(model_id, '.tuned')
    }
    if (impute) {
      lrn = cpoMice() %>>% lrn
      lrn$id <- sub(".scale", "", lrn$id)
      model_id = paste0(model_id, '.mice') 
    }	
    
    ### N.B. There is a bug in package mlrCPO that causes the name of the first cpo attached to a learner to be added twice.
    ###		See https://github.com/mlr-org/mlrCPO/issues/77
    ###		When this is fixed, the extra .scale (below) can be removed)
    ###		If tuning, then not adding two cpos together - they are separated by tuning.
    #			if (!is.null(baselrn$tune_params))
    #				model_id = paste0(model_id, '.mice') 
    #			else
    #				model_id = paste0(model_id, '.scale.mice') 
    ###	
    writeLines(paste0("\nBASE METHOD: ", baselrn$name, " ", baselrn$class))
    print(paste0("Model name: ", model_id))
    bmr = benchmark(lrn, ohe.task, resampling = resamp, surv.measures, show.info = TRUE, models = TRUE, keep.extract = TRUE,
                    keep.pred = TRUE)
    print("Benchmarking done")
    getBMRPredictions(bmr)
    model_results(task_id, bmr, model_id, result_file, TRUE)
  }
  
  
  #------------------------------------------------------------------------------------------------------------------------------
  # FEATURE SELECTION - FILTER METHODS
  #------------------------------------------------------------------------------------------------------------------------------
  #	params = makeParamSet(makeNumericParam("fw.perc", lower = 0.01, upper = 0.5))
  params_tuning = makeParamSet(makeIntegerParam("fw.abs", lower = tune_lowerlim, upper = tune_upperlim))
  params_untuned = makeParamSet(makeDiscreteParam("fw.perc", values = c(1.0)))
  
  #Filters
  cox.lrn = makeLearner(cl = "surv.coxph", id = "perf.cox", predict.type = "response")
  base_filters = list(
    "univariate.model.score" = list("method" = "univariate.model.score",
                                    "code" = LRN_FS_UNIVARIATE,
                                    "prefix" = "Univariate",
                                    "args" = list(perf.learner = cox.lrn)),
    "randomForestSRC_importance" = list("method" = "randomForestSRC_importance",
                                        "code" = LRN_FS_RF_VARIMP,
                                        "prefix" = "RF_VarImp",
                                        "args" = list(ntree = 1000, nsplit = 10, mtry = psqrt, nodesize = 3)),
    "randomForestSRC_mindepth" = list("method" = "randomForestSRC_var.select",
                                      "code" = LRN_FS_RF_MINDEPTH,
                                      "prefix" = "RF_MinDepth",
                                      "args" = list(metho = "md", ntree = 1000, nsplit = 10, nodesize = 3, splitrule = "logrank")),
    "randomForestSRC_varhunt" = list("method" = "randomForestSRC_var.select",
                                     "code" = LRN_FS_RF_VARHUNT,
                                     "prefix" = "RF_VarHunt",
                                     "args" = list(metho = "vh", ntree = 1000, nsplit = 10, nodesize = 3, splitrule = "logrank")),
    "ranger_permutation" = list("method" = "ranger_permutation",
                                "code" = LRN_FS_RF_PERMUTE,
                                "prefix" = "RF_PERMUTE",
                                "args" = list(num.trees = 1000, splitrule = "maxstat"))
  )
  
  #Run a filter on a base learner
  #Base methods should be tuned as well. Can I use above values?
  run_filter = function(baselrn, basefilt, resamp, result_file) 
  {
    #		set.seed(24601, "L'Ecuyer")	
    model_name = paste(basefilt$prefix, baselrn$name, sep = "_")
    lrn = do.call(makeLearner, args = append(list("cl" = baselrn$class, "id" = model_name, "predict.type" = "response"), baselrn$args))
    lrn = cpoScale() %>>% lrn
    
    if (!(basefilt$method %in% c("randomForestSRC_mindepth"))) {
      filt = makeFilterWrapper(lrn, fw.method = basefilt$method, more.args = basefilt$args, cache = TRUE)
      model_name = paste0(model_name, '.scale.filtered')
      filt = makeTuneWrapper(filt, resampling = tuning, par.set = params_tuning, control = ctrl, show.info = TRUE)
      model_name = paste0(model_name, '.tuned')
    } else {
      filt = makeFilterWrapper(lrn, fw.method = basefilt$method, fw.perc = 1.0, more.args = basefilt$args, cache = TRUE)
      model_name = paste0(model_name, '.scale.filtered')
    }
    
    filt = cpoMice() %>>% filt
    model_name = paste0(model_name, '.mice')
    
    writeLines(paste("\n\nFILTER: ", basefilt$method))
    cat(paste0("Model name: ", model_name))
    bmr = benchmark(filt, surv.task, resamp, surv.measures, show.info = TRUE, models = TRUE, keep.extract = TRUE)
    model_results(task_id, bmr, model_name, result_file, FALSE)
  }
  
  #------------------------------------------------------------------------------------------------------------------------------
  # FEATURE SELECTION - WRAPPER METHODS
  #------------------------------------------------------------------------------------------------------------------------------
  base_wrappers = list(
    "sfs" = list("method" = "sfs",
                 "code" = LRN_FS_SFS,
                 "prefix" = "sfs",
                 "args" = list()),
    "sffs" = list("method" = "sffs",
                  "code" = LRN_FS_SFFS,
                  "prefix" = "sffs",
                  "args" = list())
  )
  
  wrapper_results = function(task_id, bmr, model_id, result_file, unwrap = TRUE)
  {
    tid = getBMRTaskIds(bmr)
    lid = getBMRLearnerIds(bmr)
    mods = getBMRModels(bmr)
    res = mods[[tid]][[lid]]
    
    #	Note - Need to use the next model to get the features, as the mice model is the last one in the chain
    names = list()
    scores = list()
    for (i in 1:length(res)) {
      if (!inherits(res[[i]]$learner.model$next.model, 'FailureModel')) {
        feats = getFeatSelResult(res[[i]]$learner.model$next.model)		
        names[[i]] = feats$x
        scores[[i]] = i
      }
    }
    
    feats = save_features(feats, model_id, names, scores, length(res))
    stab  = save_stability(stab, model_id, names, getTaskNFeats(surv.task))
    perf  = save_performance(perf, model_id, bmr, task_id)
    
    write_performance(perf, result_file, "")
    write_features(feats, result_file, "")
    write_stability(stab, result_file, "")
    return(list("perf" = perf, "feats" = feats, "stab" = stab))
  }
  
  #Run each wrapper on each of the base methods, except glmnet as you need a min model size of 2
  run_wrapper = function(baselrn, filt, resamp, result_file)
  {
    #		set.seed(24601, "L'Ecuyer")	
    model_name = paste(filt$prefix, baselrn$name, sep = "_")
    lrn = do.call(makeLearner, args = append(list("cl" = baselrn$class, "id" = model_name, "predict.type" = "response"), baselrn$args))
    lrn = cpoScale() %>>% lrn
    ctrl = makeFeatSelControlSequential(method = filt$method, maxit = NUM_ITERS_TUNE, max.features = 20)	 
    wrap = makeFeatSelWrapper(lrn, resampling = tuning, measures = surv.measures, control = ctrl, show.info = FALSE)
    wrap = cpoMice() %>>% wrap
    model_name = paste0(model_name, '.scale.featsel.mice')
    writeLines(paste("\n\nWRAPPER: ", filt$method))
    bmr = benchmark(wrap, surv.task, resamp, surv.measures, show.info = TRUE, models = TRUE, keep.extract = TRUE)
    wrapper_results(task_id, bmr, model_name, result_file, FALSE)
  }
  
  
  rs_strategy = if (rpt > 0) outer else single
  if (substring(Sys.info()['nodename'], 1, 1) == 'k' || is.null(active_learners)) {
    pbs_index = as.integer(Sys.getenv("PBS_ARRAY_INDEX"))
    model_index = (pbs_index-1) %/% NUM_ALGORITHMS
    print(paste0("model_index = ", model_index))
    if (model_index == 0) {
      model_index = ((pbs_index-1) %% NUM_ALGORITHMS) + 1
      run_learner(base_learners[[model_index]], rs_strategy, result_file, impute)
    } else {
      fs_index = ((pbs_index-1) %% NUM_ALGORITHMS) + 1
      if (fs_index < NUM_FEATSEL)
        run_filter(base_learners[[model_index]], base_filters[[fs_index]], rs_strategy, result_file)
      else
        run_wrapper(base_learners[[model_index]], base_wrappers[[fs_index - NUM_FEATSEL]], rs_strategy, result_file)
    }
  } else {
    if (active_learners <= LRN_LAST || active_learners == LRN_ALL_MODELS) {
      for (baselrn in base_learners) {
        if (bitwAnd(active_learners, baselrn$code) || bitwAnd(active_learners, LRN_ALL_MODELS)) {
          run_learner(baselrn, rs_strategy, result_file, impute)
        }
      }
    } else if (active_learners <= LRN_LAST_WRAPPER) {
      for (wrap in base_wrappers) {
        if (bitwAnd(active_learners, wrap$code) || bitwAnd(active_learners, LRN_ALL_FS)) {
          for (baselrn in base_learners) {
            if (baselrn$code %nin% c(LRN_RIDGE, LRN_ELASTICNET, LRN_LASSO)) {
              if (bitwAnd(active_learners, baselrn$code) || bitwAnd(active_learners, LRN_ALL_MODELS)) {
                run_wrapper(baselrn, wrap, rs_strategy, result_file)
              }
            }
          }
        }
      }
    } else {
      for (filt in base_filters) {
        if (bitwAnd(active_learners, filt$code) || bitwAnd(active_learners, LRN_ALL_FS)) {
          for (baselrn in base_learners) {
            if (bitwAnd(active_learners, baselrn$code) || bitwAnd(active_learners, LRN_ALL_MODELS)) {
              run_filter(baselrn, filt, rs_strategy, result_file)
            }
          }
        }
      }
    }
  }	
  
  writeLines("\n")
  print(warnings())
  print(Sys.time())
}
