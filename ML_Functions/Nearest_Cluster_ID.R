#0. import and prep clustering input data (FLA)
import_FLA_clustering_data <- function(base_dir, veri_insts_unlisted, BPP_PC){
  ########################0. IMPORT AND PREP##################
  #get distinct instances of FLA results
  TSP_FLA <- readRDS(paste(base_dir, "TSP", "TSP_FLA_Results.rds", sep="/"))
  KP_FLA <- readRDS(paste(base_dir, "KP", "KP_FLA_Results.rds", sep="/"))
  GCP_FLA <- readRDS(paste(base_dir, "GCP", "GCP_FLA_Results.rds", sep="/"))
  BPP_FLA <- readRDS(paste(base_dir, "1DBPP", "BPP_FLA_Results.rds", sep="/"))

  #convert to dataframes and extract only the local move data
  TSP_FLA <- as.data.frame(TSP_FLA)
  TSP_FLA <- TSP_FLA %>% mutate_at(3:ncol(TSP_FLA), as.numeric)
  TSP_FLA <- TSP_FLA[which(TSP_FLA$moves == "2_opt"),]
  
  KP_FLA <- as.data.frame(KP_FLA)
  KP_FLA <- KP_FLA %>% mutate_at(3:ncol(KP_FLA), as.numeric)
  KP_FLA <- KP_FLA[which(KP_FLA$moves == "bit_flip"),]
  
  GCP_FLA <- as.data.frame(GCP_FLA)
  GCP_FLA <- GCP_FLA %>% mutate_at(3:ncol(GCP_FLA), as.numeric)
  GCP_FLA <- GCP_FLA[which(GCP_FLA$moves == "one_move"),]
  
  BPP_FLA <- as.data.frame(BPP_FLA)
  BPP_FLA <- BPP_FLA %>% mutate_at(3:ncol(BPP_FLA), as.numeric)
  BPP_FLA <- BPP_FLA[which(BPP_FLA$moves == "one_move"),]
  
  #change all NaNs and NAs to zeros
  TSP_FLA[is.na(TSP_FLA)] <- 0
  KP_FLA[is.na(KP_FLA)] <- 0
  GCP_FLA[is.na(GCP_FLA)] <- 0
  BPP_FLA[is.na(BPP_FLA)] <- 0

  # TSP_FLA$avg_viol_lower <- NULL
  # KP_FLA$avg_viol_lower <- NULL
  # GCP_FLA$avg_viol_lower <- NULL
  # BPP_FLA$avg_viol_lower <- NULL
  # PBP_FLA$avg_viol_lower <- NULL
  
  TSP_FLA$moves <- NULL
  KP_FLA$moves <- NULL
  GCP_FLA$moves <- NULL
  BPP_FLA$moves <- NULL

  TSP_FLA$H_star <- NULL
  KP_FLA$H_star <- NULL
  GCP_FLA$H_star <- NULL
  BPP_FLA$H_star <- NULL

  TSP_FLA$h_star <- NULL
  KP_FLA$h_star <- NULL
  GCP_FLA$h_star <- NULL
  BPP_FLA$h_star <- NULL

  ########################1. FEATURE SETUP##################
  FLA_features <- rbind(TSP_FLA, KP_FLA, GCP_FLA, BPP_FLA) 
  FLA_features <- FLA_features %>% mutate_at(2:50, as.numeric)
  FLA_features$PC <- c(rep("TSP", times=950), rep("KP", times=1060), rep("GCP", times=1000), 
                       rep("1DBPP", times=1000))
  FLA_features$PC2 <- c(rep(1, times=950), rep(2, times=1060), rep(3, times=1000), 
                        rep(4, times=1000))
  
  #remove verification instances
  FLA_features_veri <- FLA_features[which(FLA_features$instance %in% veri_insts_unlisted), ]
  
  #record min's and max's for each feature
  mins_maxes <- numeric()
  for (f in 2:50){
    mins_maxes <- cbind(mins_maxes, c(min(FLA_features[,f]), max(FLA_features[,f])))
  }
  colnames(mins_maxes) <- colnames(FLA_features)[2:50]
  
  for (f in 2:50){
    #scale both sets based on mins and maxes of training data
    #1. training
    FLA_features[,f] <- (FLA_features[,f] - mins_maxes[1,f-1]) / (mins_maxes[2,f-1] - mins_maxes[1,f-1])
    #2. verification
    FLA_features_veri[,f] <- (FLA_features_veri[,f] - mins_maxes[1,f-1]) / (mins_maxes[2,f-1] - mins_maxes[1,f-1])
  }
  
  #return fla data - for all training and all verifications
  return(list(FLA_features, FLA_features_veri))
}


#1. identify the cluster to which each verification instance belongs
verification_cluster_ID <- function(base_dir, test_insts, centroids){
  #if centroids are based on PC
  if (sum(centroids == "No centroids") != 0){
    #then return the PC corresponding cluster for each verification instance
    if (length(test_insts) == 10){
      return(rep(5, times=10))
    }else{
      return(c(rep(1, times=30), rep(2, times=30), rep(3, times=30), rep(4, times=30)))
    }
  }
  
  #import clustering features
  feat_list <- import_FLA_clustering_data(base_dir, test_insts)
  #extract from list
  FLA_features <- feat_list[[1]]
  FLA_features_veri <- feat_list[[2]]
  #remove avg_viol_lower
  FLA_features_veri$avg_viol_lower <- NULL
  
  #cycle through each verification instance
  c_insts <- numeric()
  for (veri_inst in test_insts){
    #calculate distance between each cluster centroid
    c_dists <- numeric()
    for (c in 1:nrow(centroids)){
      c_dists <- append(c_dists, dist(rbind(FLA_features_veri[which(FLA_features_veri$instance==veri_inst), 2:49], centroids[c,])))
    }
    #return which cluster is nearest for that instance
    c_insts <- append(c_insts, which.min(c_dists))
  }
  #return which cluster is nearest to each inst
  return(c_insts)
}












