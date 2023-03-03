library(survival)
#library(survivalsvm)
library(mlr)
library(stringi)
library(mice)
library(tools)
library(dplyr)

code_dir = "mlr_code/"
data_dir = "data/"

# SMTP_creds_file <- ".MerckEmailSMTP"
# 
# if(!file.exists(SMTP_creds_file)){
#   cat("creating SMTP creds file\n") 
#   
#   blastula::create_smtp_creds_file(
#     file = SMTP_creds_file,
#     #id = "Merck Exchange",          # Name the Credential
#     user = NULL,
#     provider = NULL,
#     host = "mailhost.merck.com",
#     port = 25,
#     use_ssl = FALSE 
#   )
# } else { cat("SMTP credential file exists\n") }


source(paste0(code_dir, "survival_const.R"))
source(paste0(code_dir, "survival_tests.R"))
#source(paste(code_dir, "survival-svm-wrappers.R", sep=""))
source(paste0(code_dir, "RLearner_surv_xgboost.R"))

#-------------------------------------------------------------------
# Get Environment Variables
#-------------------------------------------------------------------
if(interactive()) {
  config <- read.csv(paste0(code_dir, "config_post2018.csv"))
  fs_index = 0
  res_index = 1
  imp = FALSE
  excl = TRUE
  rep_num = 1
  ens = "hom"
} else {
  args = commandArgs(TRUE)
  cat("Config file: ", args[1], "\n")
  config_file <- args[1]
  config <- read.csv(paste(code_dir, config_file, sep=""), header=TRUE)
  fs_index = 0
  res_index = args[2]
  imp = as.logical(args[3])
  excl = as.logical(args[4])
  rep_num = as.numeric(args[5])
  ens = "hom"
}

env_vars = Sys.getenv()
for (name in names(env_vars)) {
	switch(name, 
				 fs_index = {fs_index = as.numeric(env_vars[[name]])}, 
				 res_index = {res_index = as.numeric(env_vars[[name]])}, 
				 rep_num = {rep_num = as.numeric(env_vars[[name]])}, 
				 excl = {excl = as.logical(env_vars[[name]])},
				 imp = {imp = as.logical(env_vars[[name]])},
				 ens = {ens = env_vars[[name]]}
				)
}

print(paste0("fs_index = ", fs_index))
print(paste0("res_index = ", res_index))
print(paste0("rep_num = ", rep_num))
print(paste0("excl = ", excl))
print(paste0("imp = ", imp))
print(paste0("ens = ", ens))

run_experiments = function(config.df) {
	print("Running experiments ...")
	data_file = paste(data_dir, config.df['DataFile'], sep="")
  result_file = paste0(config.df['ResultFile'], "_", res_index)
	if (substring(Sys.info()['nodename'], 1, 1) == 'k') {  
		index <- Sys.getenv("PBS_ARRAY_INDEX")
		if (index > 0) {
			result_file = paste0(result_file, ".", index)
			if (rep_num > 0) {
				result_file = paste0(result_file, ".", rep_num)
			}	
		}
	}
  print(data_file)
  print(result_file)
  
	if (file_ext(config.df['DataFile']) == 'Rds') {
		dataset = readRDS(data_file)	
		
		cat_str = config.df['Categoricals']
		if (!is.na(cat_str)) {
		  categoricals = unlist(strsplit(cat_str, split = ", ", fixed=TRUE))
		  dataset[categoricals] <- lapply(dataset[categoricals], as.factor)
		}
		
		excl_str = config.df['Exclusions']
		if (excl && !is.na(excl_str)) {
		  exclusions = unlist(strsplit(excl_str, split = ", ", fixed=TRUE))
		  dataset = dataset[, !(names(dataset) %in% exclusions)]			
		}
		
		# For ADNI data, remove "labelled" class as it upsets mRMR
		# if (any(sapply(dataset, function(y) inherits(y, "labelled")))) {
		# 	ind = which(sapply(dataset, function(y) inherits(y, "factor")))
		# 	for (i in ind)
		# 	 attr(dataset[[i]],"class") = "factor"
		# 
		# 	ind = which(sapply(dataset, function(y) inherits(y, "integer")))
		# 	for (i in ind)
		# 		attr(dataset[[i]],"class") = "integer"
		# 
		# 	ind = which(sapply(dataset, function(y) inherits(y, "numeric")))
		# 	for (i in ind)
		# 		attr(dataset[[i]],"class") = "numeric"
		# }
	} else {
		dataset <- read.csv(data_file, header = TRUE)
		feature_types <- sapply(dataset, function(feature) paste(class(feature), collapse = "_"))

		cat_str = config.df['Categoricals']
		if (!is.na(cat_str)) {
			categoricals = unlist(strsplit(cat_str, split = ",", fixed=TRUE))
			dataset[categoricals] <- lapply(dataset[categoricals], as.factor)
		}
		
		excl_str = config.df['Exclusions']
		if (excl && !is.na(excl_str)) {
			exclusions = unlist(strsplit(excl_str, split = ",", fixed=TRUE))
			dataset = dataset[, !(names(dataset) %in% exclusions)]			
		}
	}
  
  learners = eval(parse(text=config.df['ActiveLearners']))
	print(paste0("Experiment type: ", config.df['ExpType']))
	if (config.df['ExpType'] == EXP_SURVIVAL)
	  survival_tests(config.df['TaskID'], dataset, result_file, config.df['TimeVar'], config.df['StatusVar'], learners, run_stability=FALSE, impute = imp, rpt = rep_num)
}

tryCatch({
  apply(config, 1, run_experiments)
  },
  error = function(err) {
    print(err)
    # compose_email(
    #   body = md(glue::glue(
    #     "Hello Lathan, your ML job ran into this error: {err}"))
    # ) %>%
    #   smtp_send(
    #     from = "lathan.liou@merck.com",
    #     to = "lathan.liou@merck.com",
    #     subject = "Email from MLR",
    #     credentials = creds_file(".MerckEmailSMTP")
    #   )
  })

# compose_email(
#   body = md(glue::glue(
#     "Hello Lathan, your ML job finished"))
# ) %>%
#   smtp_send(
#     from = "lathan.liou@merck.com",
#     to = "lathan.liou@merck.com",
#     subject = "Email from MLR",
#     credentials = creds_file(".MerckEmailSMTP")
#   )

