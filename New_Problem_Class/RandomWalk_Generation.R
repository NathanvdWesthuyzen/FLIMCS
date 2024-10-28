#script for generating random walks
#clear workspace:
rm(list = ls())

#############LIBRARIES#################
#import required libraries
library(dplyr)
library(doParallel)
library(stringr)

#############FUNCTIONS#################
#get current dir based on PC implementation (assumption of current dir.)
base_dir <- getwd()
no_cores <- 6 #a minimum of 32 cores recommended for full portfolio implementation
print(paste("No. of cores specified:", no_cores))

#save RW data results in "FLA_Samples" dir.
save_gen_dir <- paste(base_dir, "FLA_Samples", sep="/")

#specify which instance to consider (via ID)
INSTANCES <- 1

#define functions dir
funcs_dir <- paste(base_dir, "Functions", sep="/")
#import script functions
  #import instance importing function
  source(paste(funcs_dir, "Import_Instance.R", sep="/"))
  #import move operators
  source(paste(funcs_dir, "Move_Operators.R", sep="/"))
  #import feasible status functions
  source(paste(funcs_dir, "Feasibility_State.R", sep="/"))
  #import solution generation functions
  source(paste(funcs_dir, "Solution_Generation.R", sep="/"))
  #import fitness function functions
  # source(paste(funcs_dir, "Fitness_Function.R", sep="/"))
  source(paste(funcs_dir, "Fitness_Function_CHT.R", sep="/"))
  #import random walk functions
  source(paste(funcs_dir, "FLA_RW.R", sep="/"))

############FLA DEFINTIONS###############
#A) RANDOM FEASIBLE SOLUTIONS
no_sols <- 1000

#B) RANDOM WALKS
walk_length <- 1000
no_walks <- no_cores #matching the threading

#C) SELECTED MOVE OPERATORS
#summarise all moves
moves <- c("one_move", "ejection_chain", 1, 0.5, 0.1)
move_local <- moves[1]
move_global <- moves[2]
gammas <- c(1, 0.5, 0.1)

#D) FIX CHT TO REJECTION
CHT <- "Rejection"

#############PREP // COMPUTATION##########
cl <- makeCluster(no_cores)
registerDoParallel(cl)

#############IMPORT INSTANCES############
#cycle through for loop for each problem class then instance
for (INSTANCE in INSTANCES){
  print(sprintf("%s instance", INSTANCE))
  st <- Sys.time()
  #import instance
  inst <- import_instance(INSTANCE, base_dir)
  #extract problem variables
  var_1 <- inst[[1]]
  var_2 <- inst[[2]]
  ...
  var_n <- inst[[n]]
  
  #############PROBLEM PARAMETER LIST########
  #defined problem parameters in a list
  problem_parameters <- list(var_1, var_2, ..., var_n)
  
  #export all instance info to // nodes
  clusterExport(cl, ls(), envir = .GlobalEnv)

  #############FLA GENERATION##########
  #cycle through each MO combination
  for (move in moves){
    # print(move)
    #Obtain multiple RWs for MO
    if (move %in% moves[1:2]){
      RWs <- parLapply(cl=cl, X=1:no_walks, fun=random_walk, problem_parameters, walk_length, move)
      #save random walks
      saveRDS(RWs, file = paste(save_gen_dir, "/", INSTANCE, "_RWs_", move, ".rds", sep=""))
    }else{
      RWs <- parLapply(cl=cl, X=1:no_walks, fun=random_walk_gamma, problem_parameters, walk_length, moves[1], moves[2], move)
      #save random walks
      saveRDS(RWs, file = paste(save_gen_dir, "/", INSTANCE, "_RWs_g_",
                                str_replace(as.character(move),"[.]", "_"), ".rds", sep=""))
    }
  }
  print("Random walks conducted and saved...")
  print(difftime(Sys.time(), st, "min"))
}

print("RW Generation Complete...")
#terminate parallel execution
stopCluster(cl)
