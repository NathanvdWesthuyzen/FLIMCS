#1. import all data for 120 verification data
import_veri_data <- function(base_dir, INSTANCES_ALL, MH, veri_insts_unlisted, INSTANCES_TSP, INSTANCES_KP, INSTANCES_GCP, INSTANCES_BPP, INSTANCES_PBP){
  #2.0 create empty database
  veri_database <- data.table()
  
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
    }else if (PC == "1DBPP" | PC == "BPP"){
      INSTANCES_PC <- INSTANCES_BPP
      PC_d <- "BPP"
    }else{
      INSTANCES_PC <- INSTANCES_PBP
      PC_d <- "PBP"
    }
    
    #check if any of the selected clustered instances are in the currently 
    #considered PC
    if (any(veri_insts_unlisted %in% INSTANCES_PC)){
      #2.1 import database based on PC and MH
      database <- import_MH_database(base_dir, PC, PC_d, MH)
      
      #2.2 for each PC instance in cluster, pull out relevant info
      rows <- which(database$id %in% veri_insts_unlisted)
      st_in <- which(colnames(database)=="CHT")
      vec1 <- c(1:st_in, st_in+5, st_in+6)
      relevant_inst_info <- database[rows, ..vec1]
      #2.3 add to cluster-based database
      veri_database <- rbindlist(list(veri_database, relevant_inst_info))
    }
  }
  #return database
  print(sprintf("%s verification data imported...", MH))
  return(veri_database)
}
