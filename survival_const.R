#
# Experiment types
#
EXP_SURVIVAL = "SURV"
EXP_ENSEMBLE = "ENS"

#
#	Constant values for survival benchmarking tests
#
NUM_INPUTS	 = 3 #only documentation
NUM_MEASURES = 4 #only documentation
NUM_ITERS    = 5 #for code
NUM_FOLDS    = 5 #for code, 5
NUM_ITERS_TUNE 	 = 5 #for code
NUM_FOLDS_TUNE 	 = 2	#only documentation
NUM_ITERS_FILTER = 10  #for code
NUM_ITERS_STAB 	 = 100 #for code
NUM_BOOTSTRAP_SAMPLES = 50 #only documentation

NUM_AGGREGATORS = 5 #only documentation
NUM_THRESHOLDS  = 7 #only documentation
NUM_ALGORITHMS  = 10 #for code
NUM_FEATSEL		  = 9 #for code
NUM_WRAPPERS	  = 2 #only documentation

MAX_FEATURES	 = 21 #for code
MAX_RESOLUTION	 = 20 #only documentation

NUM_WEIGHT_AGGREGATES = 2 #only documentation
IND_MEAN_WEIGHT = 1 #only documentation
IND_SUM_WEIGHT  = 2 #only documentation

NUM_RANK_AGGREGATES = 4 #only documentation
IND_MEAN_RANK       = 1 #only documentation
IND_SUM_RANK        = 2 #only documentation
IND_MIN_RANK    	= 3 #only documentation
IND_MAX_RANK    	= 4 #only documentation

NUM_STAB_METRICS	= 4 #only documentation
IND_STAB_JACCARD	= 1 #only documentation
IND_STAB_DICE			= 2 #only documentation
IND_STAB_KUNCHEVA	= 3 #only documentation
IND_STAB_LUSTGARTEN	= 4 #only documentation

BOOSTER_TREE		= "gbtree"
BOOSTER_LINEAR		= "gblinear"

NO_OHE = list(c("CoxPH","RFSRC", "Ranger", "XGBTree"))
FS_RANKERS = list("univariate.model.score", "ranger_permutation", "randomForestSRC_importance", "randomForestSRC_var.select", "mrmr", "XGBoost")
FS_SPARSE = list("cv.glmnet", "glmboost", "cv.CoxBoost")
ALGO_NAMES = list(c("CoxPH", "Lasso", "Ridge", "ElasticNet", "CoxBoost", "GLMBoost", "RFSRC", "Ranger", "Univariate", "XGBTree", "XBGLinear"))
DATASET_NAMES = list(datsets=c("Activities", "Blood", "Cog Objective", "Cog Subjective", "Demographics", "Family History", "Nutrition", "Functional", "Genetic Risk", 
					 "Quality of Life", "Medical Exam", "Medical History", "Medications", "MRI", "Psychological", "Combination")) 

	
#
# Codes to enable/disable different learners
#
# Base learners - embedded feature selection
LRN_COX 	 			= 0x001
LRN_RIDGE	 			= 0x002
LRN_ELASTICNET	= 0x004
LRN_LASSO	 			= 0x008
LRN_COXBOOST 		= 0x010
LRN_GLMBOOST 		= 0x020
LRN_RFSRC	 			= 0x040
LRN_RANGER	 		= 0x080
LRN_XGB_TREE 		= 0x100
LRN_XGB_LINEAR	= 0x200 
LRN_SSVM_VB1		= 0x400
LRN_SSVM_VB2		= 0x800
LRN_RPART       = 0x1000
LRN_LAST				= 0x1000

# Wrappers
LRN_FS_SFS					= 0x01000
LRN_FS_SFFS					= 0x02000
LRN_LAST_WRAPPER 		= 0x02000

# Filters
LRN_FS_UNIVARIATE	 	= 0x004000
LRN_FS_RF_VARIMP	 	= 0x008000
LRN_FS_RF_MINDEPTH 	= 0x010000
LRN_FS_RF_VARHUNT	 	= 0x020000
LRN_FS_RF_PERMUTE	 	= 0x040000
LRN_FS_MRMR			   	= 0x080000
LRN_LAST_FS			   	= 0x400000

#Combinations
LRN_ALL_FS			  	= 0x1000000	#All feature selection methods on one algorithm
LRN_ALL_MODELS			= 0x2000000	#One feature selection method on all algorithms
LRN_ALL_ENS_FS			= 0x4000000	#All ensemble feature selection methods on one algorithm

# Ensembles
LRN_FS_ENS_START		= 0x010000000
LRN_FS_ENS_MEAN			= 0x010000000
LRN_FS_ENS_SUM			= 0x020000000
LRN_FS_ENS_MIN			= 0x040000000
LRN_FS_ENS_FREQ			= 0x080000000
LRN_FS_ENS_WMA			= 0x100000000
LRN_FS_ENS_END			= 0x1FFFFFFFF

#Combinations
LRN_LEARNERS		= 0x0003FF	#All base algorithms except SSVM, no filters
LRN_FILTERS			= 0x0FF000	#All feature selection methods
LRN_MOST	 		= 0x0FF07F	#All but SSVM & XGBoost until that is working& Ranger - too slow
LRN_ALL		 		= 0x0FF3FF	#All but SSVM
LRN_XGBLIN_FILTERS 	= 0x100200	#All filters on XGB Linear only
#LRN_XGBTREE_FILTERS = 0x100100	#All filters on XGB Tree only. 
LRN_XGBTREE_FILTERS = 0x0DF100	#All filters except RFVH on XGB Tree only. 
LRN_XGBTREE_SFS		= 0x01100 # XGB Tree with SFS
LRN_XGBTREE_SFFS	= 0x02100 # XGB Tree with SFFS
LRN_XGBTREE_RANGER	= 0x040100 # XGB Tree with Ranger
LRN_XGBTREE_RFVH	= 0x020100 # XGB Tree with RF VAR HUNT
LRN_RFUNIV_FILTER		= 0x204000	#Univariate filter on all models
LRN_RFMD_FILTER		= 0x210000	#RF Min-depth filter on all models
LRN_RFVI_FILTER		= 0x208000	#RF Variable importance filter on all models
LRN_RANGER_FILTER   = 0x240000	#Ranger filter on all models
LRN_MRMR_FILTER   = 0x280000	#MRMR filter on all models
LRN_MIND_FILTERS    = 0x230000	#RFSRC filters on all models
LRN_REST_FILTERS	= 0x2F3000	#All filters except Univariate & RF_VARIMP, all models
LRN_FILTERS_ONLY	= 0x300000	#All filter selection methods only
LRN_SFS_COXBOOST	= 0x001010	#SFS on Coxboost only
LRN_ALL_RFVH		= 0x220000	#RF Variable hunting filter on all algorithms
LRN_COX_RFVH		= 0x020001	#RF Variable hunting filter on Cox only
LRN_RIDGE_RFVH		= 0x020002	#RF Variable hunting filter on Ridge only
LRN_ELASTICNET_RFVH		= 0x020004	#RF Variable hunting filter on ElasticNet only
LRN_LASSO_RFVH		= 0x020008	#RF Variable hunting filter on Lasso only
LRN_COXBOOST_RFVH		= 0x020010	#RF Variable hunting filter on CoxBoost only
LRN_GLMBOOST_RFVH		= 0x020020	#RF Variable hunting filter on GLMBoost only
LRN_RFSRC_RFVH		= 0x020040	#RF Variable hunting filter on RFSRC only
LRN_RANGER_RFVH		= 0x020080	#RF Variable hunting filter on Ranger only
LRN_XGBLINEAR_RFVH		= 0x020200	#RF Variable hunting filter on XGB Linear only
LRN_COX_RFMD		= 0x010001	#RF Minimal depth filter on Cox only
LRN_COX_MEAN		= 0x400001	#Ensemble mean filter with Cox
LRN_ALL_WRAPPERS	= 0x202000	#SFFS on all models
LRN_SSVM_FILTERS 	= 0x100C00	#All filters on SSVM only
LRN_ENS_FILTERS		= 0x600000	#All ensemble filters on all models
LRN_ENS_FREQ		= 0x08000001	#Ensemble frequency filter with Cox
LRN_ENS_MEAN		= 0x01000001	#Ensemble mean filter with Cox
LRN_ENS_MEAN_CB	= 0x01000010	#Ensemble mean filter with CoxBoost
LRN_SFS_COX			= 0x001001	#SFS on Cox only
LRN_COX_ALL			= 0x100001	# Cox with all filters
LRN_COX_RANGER	= 0x040001	# Cox with Ranger
LRN_COX_WRAP		= 0x003001	# Cox with wrappers
LRN_XGBTREE_RFVARHUNT = 0x520100 # RF VarHunt and XGBTree on all featsel methods
LRN_RFVH_ALLBUTXGBTREE	= 0x202FF # RF Varhunt of all learners except XGBTree
LRN_RFSRC_XGBTREE	= 0x008100	# RF Var imp on XGBtree
LRN_COXBOOST_SFS		= 0x01010 # CoxBOost with SFS
LRN_COXBOOST_SFFS	= 0x02010 # CoxBoost with SFFS
LRN_XGBLINEAR_SFS		= 0x01200 # XGB Linear with SFS
LRN_XGBLINEAR_SFFS	= 0x02200 # XGB Linear with SFFS
LRN_MRMR_LASSO	= 0x080008	# MRMR on Lasso
LRN_MRMR_ELASTICNET	= 0x080004	# MRMR on ElasticNet
LRN_MRMR_RIDGE	= 0x080004	# MRMR on Ridge
LRN_MRMR_COXPH	= 0x080001	# MRMR on CoxPH

