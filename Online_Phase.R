#script for performing PC-specific baselines w/out feature selection

#clear workspace:
rm(list = ls())

#############LIBRARIES#################
#import required libraries
library(dplyr)
library(ranger)
library(ModelMetrics)
library(caret)
library(pryr)
library(stringr)
library(data.table)

#############USER INPUT#################
#select the target variable of interest: norm_f (solution quality) or norm_t (run time)
target_variable <- "norm_f"
#select clustering result to implement
  #1: PC
  #2-15: clustering
k_size <- 15

#get current dir based on PC implementation (assumption of current dir.)
base_dir <- getwd()
no_cores <- 6 #a minimum of 32 cores recommended for full portfolio implementation
print(paste("No. of cores specified:", no_cores))

#define order of best-performing cluster algs
  #PC, 2:15
cluster_order <- c("PC", "Xmeans", "Xmeans", "Xmeans", "kmeans", 
                    "Xmeans", "kmeans", "kmeans", "kmeans", "Xmeans", 
                    "Xmeans", "kmeans", "Xmeans", "kmeans", "Xmeans")
#training regime: 5-fold CV
k <- 1:5
PCs <- c("TSP", "KP", "GCP", "1DBPP")

#select clustering algorithm and cluster size
cluster_alg <- cluster_order[k_size]
print(sprintf("Cluster algorithm: %s...Size: %d", cluster_alg, k_size))

#IMPORT VERIFICATION INSTANCES
veri_insts <- readRDS(paste(base_dir, "ML_Functions/Verification_Insts.rds", sep="/"))
veri_insts_unlisted <- as.vector(unlist(veri_insts))

#Select which verification instance to consider for testing - the first is selected for example.
verification_instance <- veri_insts_unlisted[1]

#metaheuristics
MHs <- c("GA", "SA")
#save prediction results in "Prediction_Results" dir.
ML_results_dir <- paste(base_dir, "Prediction_Results", sep="/")
#save prediction models in "Trained_Models" dir.
Models_dir <- paste(base_dir, "Trained_Models", sep="/")

#import and save meta-feature labels
FLA_clust_cols <- colnames(readRDS(paste(base_dir, "/TSP/TSP_FLA_Results.rds", sep="")))[c(3:21, 23:30, 32:44 , 46:53)]

############IMPORT PC INSTANCES############
#i. setup all instance names
INSTANCES_TSP <- character()
for (i in c("CLKeasy", "CLKhard", "LKCCeasy", "LKCChard","random")){
  for (j in 1:190){
    INSTANCES_TSP <- append(INSTANCES_TSP, paste(i, j, sep="_"))
  }
}
INSTANCES_KP  <- readRDS(paste(base_dir, "/KP/Sampled_KP_Insts.rds", sep=""))
INSTANCES_GCP <- readRDS(paste(base_dir, "/GCP/Sampled_GCP_Insts.rds", sep=""))
INSTANCES_BPP <- readRDS(paste(base_dir, "/1DBPP/Sampled_1DBPP_Insts_final.rds", sep=""))
INSTANCES_ALL <- c(INSTANCES_TSP, INSTANCES_KP, INSTANCES_GCP, INSTANCES_BPP)
#remove verification instances
INSTANCES_ALL <- INSTANCES_ALL[-which(INSTANCES_ALL %in% veri_insts_unlisted)]

############IMPORT VERIF DATA##############
source(paste(base_dir, "ML_Functions/Import_Verification_Data.R", sep="/"))
source(paste(base_dir, "ML_Functions/Nearest_Cluster_ID.R", sep="/"))
source(paste(base_dir, "ML_Functions/Cluster_Based_Input_Prep.R", sep="/"))
GA_veri_data <- import_veri_data(base_dir, INSTANCES_ALL, "GA", veri_insts_unlisted, 
                                 INSTANCES_TSP, INSTANCES_KP, INSTANCES_GCP, INSTANCES_BPP)
SA_veri_data <- import_veri_data(base_dir, INSTANCES_ALL, "SA", veri_insts_unlisted, 
                                 INSTANCES_TSP, INSTANCES_KP, INSTANCES_GCP, INSTANCES_BPP)

#######IMPORT CLUSTERING INFORMATION#######
#import selected cluster labels
cluster_info <- extract_cluster_labels(cluster_alg, k_size, FLA_clust_cols)
centroids <- cluster_info[[1]]
labels <- cluster_info[[2]]
#check if labels match 
if (min(labels) == 0){
  #then increment all by 1
  labels <- labels + 1
}

#print instance split between clusters
clust_sum <- table(labels)
print(clust_sum)
  
#identify the needed cluster
cluster_no <- verification_cluster_ID(base_dir, verification_instance, centroids)

############5-FOLD CV TRAINING PER REQUIRED CLUSTER################
source(paste(base_dir, "ML_Functions/Train_and_Test_ML_Model.R", sep="/"))
source(paste(base_dir, "ML_Functions/Remove_Useless.R", sep="/"))
print(sprintf("Evaluation initiated...Cluster %d", cluster_no))
print(sprintf("Cluster size: %d", clust_sum[cluster_no]))

#######IMPORT CLUSTER-BASED INPUT DATA#######
cluster_training_data_GA <- prep_cl_input_data(base_dir, INSTANCES_ALL, labels, cluster_no, "GA")
cluster_training_data_SA <- prep_cl_input_data(base_dir, INSTANCES_ALL, labels, cluster_no, "SA")
print("Cluster-based Databases Prepped...")

#remove useless features
cluster_training_data_GA <- remove_useless_f(cluster_training_data_GA)
cluster_training_data_SA <- remove_useless_f(cluster_training_data_SA)
print("USeless features removed...")

############5-FOLD TRAINING################
#i) import model if already trained, else conduct training
if (file.exists(paste(Models_dir, "/GA_model_", target_variable, "_", k_size, "_", cluster_no, ".rds", sep=""))){
  #assuming that if the GA model has been trained, then the SA model has also been trained 
  GA_veri_model <- readRDS(file=paste(Models_dir, "/GA_model_", target_variable, "_", k_size, "_", cluster_no, ".rds", sep=""))
  SA_veri_model <- readRDS(file=paste(Models_dir, "/GA_model_", target_variable, "_", k_size, "_", cluster_no, ".rds", sep=""))
  print(sprintf("Models imported for Cluster %d", cluster_no))
}else{
  #save results for training fold
  mae_fold_ga <- list()
  mae_fold_sa <- list()
  
  #run through 5-fold training with 80/20 training-test split
  print("k-folds initiated...")
  for (fold in k){ 
    print(sprintf("Cluster: %d...Fold no: %d", cluster_no, fold))
    #PREP INPUT TRAINING DATA
    source(paste(base_dir, "ML_Functions/Prep_Input_Data.R", sep="/"))
    #UPDATE RANGER CONFIGS
    source(paste(base_dir, "ML_Functions/Ranger_Configs.R", sep="/"))
    
    #EMPTY LISTS FOR RAW PREDICTIONS
    list_f_GA <- list()
    list_f_SA <- list()
    
    list_MAE_GA <- numeric()
    list_MAE_SA <- numeric()
    
    #RECORD TOTAL FOLD EXECUTION TIMES
    st <- Sys.time()
    
    #CONDUCT EACH RANGER CONFIG SEQUENTIALLY
    for (HP_row in nrow(hyper_grid)){ 
      print(sprintf("Configuration: %d", HP_row))
      #TRAIN GA f(x) model
      unseen_model_f_GA <- train_ml_model(GA_x_input, GA_y_input, hyper_grid_GA, HP_row, no_cores, target_variable)
      #TEST GA f(x) model
      mae_store_GA <- test_ml_model(cluster_training_data_GA, test_set, unseen_model_f_GA, target_variable)
      #save MAE results
      list_MAE_GA <- append(list_MAE_GA, mean(mae_store_GA))
      #remove model for memory
      remove(unseen_model_f_GA)
      print(mem_used())
      
      #TRAIN SA f(x) model
      unseen_model_f_SA <- train_ml_model(SA_x_input, SA_y_input, hyper_grid_SA, HP_row, no_cores, target_variable)
      #TEST SA f(x) model
      mae_store_SA <- test_ml_model(cluster_training_data_SA, test_set, unseen_model_f_SA, target_variable)
      #save MAE results
      list_MAE_SA <- append(list_MAE_SA, mean(mae_store_SA))
      #remove model for memory
      remove(unseen_model_f_SA)
      print(mem_used())
      
      #free memory
      gc()
    }
    #5. process and save HPT results for fold
    print(sprintf("Fold completed: %.2f mins", difftime(Sys.time(), st, units = "mins")))
    
    #STORE RESULTS FOR FOLD
    mae_fold_ga <- append(mae_fold_ga, list(list_MAE_GA))
    mae_fold_sa <- append(mae_fold_sa, list(list_MAE_SA))
    
    print("ML Results Stored....")
  }
  #EXTRACT ML PERFORMANCE AND AGGREGATE ACROSS FOLDS (folds x configs)
  unfolded_MAE_GA <- do.call(rbind, mae_fold_ga)
  unfolded_MAE_SA <- do.call(rbind, mae_fold_sa)
  
  #OBTAIN BEST PERFORMANCE FOR 5-FOLD TRAINING
  best_ML_GA <- which.min(colMeans(unfolded_MAE_GA))
  best_ML_SA <- which.min(colMeans(unfolded_MAE_SA))

  #TRAIN AND TEST MODELS ON VERIFICATION INSTANCES
  #1. PREP VERIFICATION TRAINING DATA
  source(paste(base_dir, "ML_Functions/Prep_Verification_Data.R", sep="/"))
  
  #1.1 train verification models
  GA_veri_model <- train_ml_model(GA_x_input_veri, GA_y_input_veri, hyper_grid_GA, best_ML_GA, no_cores, target_variable)
  SA_veri_model <- train_ml_model(SA_x_input_veri, SA_y_input_veri, hyper_grid_SA, best_ML_SA, no_cores, target_variable)
  
  #1.1.1 save trained models
  saveRDS(GA_veri_model, file=paste(Models_dir, "/GA_model_", target_variable, "_", k_size, "_", cluster_no, ".rds", sep=""))
  saveRDS(SA_veri_model, file=paste(Models_dir, "/SA_model_", target_variable, "_", k_size, "_", cluster_no, ".rds", sep=""))
}

#1.2 EVALUATE TRAINED MODELS ON VERIFICATION INSTANCES 
performance_GA <- verify_ml_model(GA_veri_data, verification_instance, GA_veri_model, target_variable)
performance_SA <- verify_ml_model(SA_veri_data, verification_instance, SA_veri_model, target_variable)

#2. EXTRACT TRUE PERFORMANCE PERSPECTIVE FROM GA AND SA
results_list <- assess_true_verification(performance_GA, performance_SA, GA_veri_data, SA_veri_data, 
                                         target_variable, verification_instance)

#print cluster done...
print(sprintf("Evaluation completed...Cluster %d", cluster_no))

#3. SAVE RESULTS according to the no. of clusters, which cluster, and the verification instance
saveRDS(results_list, paste(ML_results_dir, "/Results_", target_variable, "_", k_size, "_", 
                            cluster_no, "_", verification_instance, ".rds", sep=""))
