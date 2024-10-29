#1. extract cluster labels based on input algorithm, MH and cluster size
extract_cluster_labels <- function(cluster_alg, k_size, FLA_clust_cols){
  #i. if cluster size 1 is selected: then return PC labels
  if (cluster_alg == "PC"){
    PC_labels <- c(rep(1, times=920), rep(2, times=1030), rep(3, times=970), rep(4, times=970))
    return(list("No centroids", PC_labels))
  }
  #ii. if cluster size 0 is selected: then select 250 labels/class
  if (cluster_alg == "Random"){
    PC_labels <- c(rep(1, times=250), rep(2, times=250), rep(3, times=250), rep(4, times=250))
    return(list("No centroids", PC_labels))
  }
  
  #0. initialise directories
  if (cluster_alg == "kmeans"){
    cl_folder <- "k-means labels"
    cl_mh <- "kMeans"
  }else if (cluster_alg == "gmeans"){
    cl_folder <- "g-means labels"
    cl_mh <- "GMeans"
  }else{
    cl_folder <- "X-means labels"
    cl_mh <- "XMeans"
  }
  
  #0.1 write dir
  cluster_lab_dir <- paste(base_dir, "Clustering_Results", cl_folder, sep="/")
  
  #1. Extract all relevant instances based on cluster labels
  clustering <- readRDS(paste(cluster_lab_dir, "/", cl_mh, "_Labels_", k_size, ".rds", sep=""))
  
  #2. extract relevant info based on algorithm
  if (cluster_alg == "kmeans"){
    cluster_centroids <- clustering$centers
    cluster_labels <- clustering$cluster
  }else if (cluster_alg == "gmeans"){
    cluster_centroids <- as.matrix(t(clustering[[2]]))
    cluster_labels <- clustering[[1]]
  }else{
    cluster_centroids <- clustering[[1]]
    cluster_centroids <- do.call(rbind, cluster_centroids)
    cluster_labels <- as.vector(clustering[[2]])
  }
  #convert to dataframe and numeric
  colnames(cluster_centroids) <- FLA_clust_cols
  rownames(cluster_centroids) <- 1:k_size
  cluster_centroids <- as.data.frame(cluster_centroids)
  cluster_centroids <- cluster_centroids %>% mutate_at(1:ncol(cluster_centroids), as.numeric)
  cluster_labels <- as.numeric(cluster_labels)
  #return information
  return(list(cluster_centroids, cluster_labels))
}

#2. bring in the database for GA and SA and evaluate together
import_MH_database <- function(base_dir, PC, PC_d, MH){
  results_dir <- paste(base_dir, PC, "Databases", sep = "/")
  #import single FLA database
  return(readRDS(paste(results_dir,
                       paste(paste(PC_d, MH, "FLA", "Database", sep="_"), ".rds", sep=""),
                       sep="/")))
}

#3. prep all ML data based on cluster labels and selected cluster
prep_cl_input_data <- function(base_dir, INSTANCES_ALL, labels, cluster_no, MH){
  #0. find which instances are in selected cluster
  ids <- which(labels == cluster_no)
  #1. pull out instance names
  clust_insts <- INSTANCES_ALL[ids]
  
  #2.0 create empty database
  clust_database <- data.table()
  
  #2. go through each PC to see IDS and pull out relevant info
  for (PC in PCs){
    #reassign applicable instances based on PC
    if (PC == "TSP"){
      INSTANCES_PC <- INSTANCES_TSP
      PC_d <- PC
    }else if (PC == "KP"){
      INSTANCES_PC <- INSTANCES_KP
      PC_d <- PC
    }else if (PC == "GCP"){
      INSTANCES_PC <- INSTANCES_GCP
      PC_d <- PC
    }else{
      INSTANCES_PC <- INSTANCES_BPP
      PC_d <- "BPP"
    }
  
    #check if any of the selected clustered instances are in the currently 
      #considered PC
    if (any(clust_insts %in% INSTANCES_PC)){
      #2.1 import database based on PC and MH
      database <- import_MH_database(base_dir, PC, PC_d, MH)
      
      #2.2 for each PC instance in cluster, pull out relevant info
      rows <- which(database$id %in% clust_insts)
      st_in <- which(colnames(database)=="CHT")
      vec1 <- c(1:st_in, st_in+5, st_in+6)
      relevant_inst_info <- database[rows, ..vec1]
      #2.3 add to cluster-based database
      clust_database <- rbindlist(list(clust_database, relevant_inst_info))
    }
  }
  #3. return cluster-based database
  return(clust_database)
}

#4. prep all ML data based on randomly selected instances
prep_rand_input_data <- function(base_dir, INSTANCES_ALL, RANDOM_INSTS, MH){
  #0. find which instances are in selected cluster
  ids <- which(INSTANCES_ALL %in% RANDOM_INSTS)
  #1. pull out instance names
  clust_insts <- INSTANCES_ALL[ids] 
  
  #2.0 create empty database
  clust_database <- data.table()
  
  #2. go through each PC to see IDS and pull out relevant info
  for (PC in PCs){
    print(PC)
    #reassign applicable instances based on PC
    if (PC == "TSP"){
      INSTANCES_PC <- INSTANCES_TSP
      PC_d <- PC
    }else if (PC == "KP"){
      INSTANCES_PC <- INSTANCES_KP
      PC_d <- PC
    }else if (PC == "GCP"){
      INSTANCES_PC <- INSTANCES_GCP
      PC_d <- PC
    }else{
      INSTANCES_PC <- INSTANCES_BPP
      PC_d <- "BPP"
    }
    
    #check if any of the selected clustered instances are in the currently 
    #considered PC
    if (any(clust_insts %in% INSTANCES_PC)){
      #2.1 import database based on PC and MH
      database <- import_MH_database(base_dir, PC, PC_d, MH)
      
      #2.2 for each PC instance in cluster, pull out relevant info
      rows <- which(database$id %in% clust_insts)
      st_in <- which(colnames(database)=="CHT")
      vec1 <- c(1:st_in, st_in+5, st_in+6)
      relevant_inst_info <- database[rows, ..vec1]
      #2.3 add to cluster-based database
      clust_database <- rbindlist(list(clust_database, relevant_inst_info))
    }
  }
  #3. return cluster-based database
  return(clust_database)
}

