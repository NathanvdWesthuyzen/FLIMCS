#script for SA optimisation given an input SA configuration
#clear workspace:
rm(list = ls())

#############LIBRARIES#################
#import required libraries
library(dplyr)
library(doParallel)
library(stringr)
library(BBmisc)

#############FUNCTIONS#################
#specify if all configurations of SA can be run
HPC <- FALSE #TRUE / FALSE
#get current dir based on PC implementation (assumption of current dir.)
base_dir <- getwd()
no_cores <- 6 #a minimum of 32 cores recommended for full portfolio implementation
print(paste("No. of cores specified:", no_cores))

#save SA results in "SA_Results" dir.
save_SA_dir <- paste(base_dir, "SA_Results", sep="/")

#specify which instance to consider (via ID)
INSTANCES <- 1

#set time limit for configuration execution
timeLim <- 130 #seconds for each configuration

#############IMPORT FUNCTIONS#################
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
  source(paste(funcs_dir, "Fitness_Function_CHT.R", sep="/"))

#define SA functions dir
SA_funcs_dir <- paste(base_dir, "SA_Functions", sep="/")
  #import T_0 function
  source(paste(SA_funcs_dir, "Initial_Temperature.R", sep="/"))
  #import x_p function
  source(paste(SA_funcs_dir, "Generate_Neighbour.R", sep="/"))
  #import epoch control function
  source(paste(SA_funcs_dir, "Epoch_Control.R", sep="/"))
  #import termination recording functions
  source(paste(SA_funcs_dir, "Termination_Recording.R", sep="/"))
  #import SA execution function
  source(paste(SA_funcs_dir, "SA_Solve.R", sep="/"))

############SA DEFINTIONS###############
#A) REDUCING PROBABILITY FACTOR: GLOBAL
Gammas <- c(1,0.5,0.1, 1e-10, 1e10) 
                      #1e-10 --> LOCAL ONLY
                      #1e10 --> GLOBAL ONLY

#B) INITIAL SOLUTION GENERATIONS PROCEDURE
ISPs <- c("Random", "Hybrid")

#C) COOLING PARAMETER 
Alphas <- c(0.85,0.92,0.99)

#D) REHEATING PARAMETER
Betas <- c(1.5,1.2,1.05)

#E) EPOCH CONTROL: NO. OF ACCEPTANCES
c_maxs <- c(5,10,30)

#F) EPOCH CONTROL: NO. OF REJECTIONS --> d_max=c_max+d
d_s <- c(1,3,5)

#G) TERMINATION CRITERIA --> "Static" or "Dynamic" with a 2MIN CUTOFF
  #define the static termination criteria based on the computational resources
t_max <- c(1e3, seq(from=10e3, to=100e3, by=10e3)) 
  #define the dynamic termination criteria based on the computational resources
p_max <- c(3,4,5)

#H) CONSTRAINT HANDLING TECHNIQUE
CHTs <- c("Rejection", "Penalty", "Repair")

#update MOs based on Functions/Move_Operators.R script
#I) LOCAL MOVE OPERATOR (example:)
move <- "one_move"                       
#J) GLOBAL MOVE OPERATOR
global <- "ejection_chain"

#############SA ALL CONFIGS############
#no global only with rejection
all_configs <- expand.grid(Gammas, ISPs, Alphas, Betas, c_maxs, d_s, CHTs)

#label columns
na_mes <- c("gamma", "ISP", "cooling", "heating", "c_max", "d", "CHT")
colnames(all_configs) <- na_mes

#total no of configurations
if (HPC){
  config_nos <- 1:nrow(all_configs)
}else{
  config_nos <- round(seq(1, 2430, length.out=no_cores))
}

#############IMPORT INSTANCE#############
st <- Sys.time()
inst <- import_instance(INSTANCE, base_dir)
#extract problem variables
var_1 <- inst[[1]]
var_2 <- inst[[2]]
...
var_n <- inst[[n]]

#############PROBLEM PARAMETER LIST########
#defined problem parameters in a list for simplified data transfer between functions
problem_parameters <- list(var_1, var_2, ..., var_n)

#############// EXECUTION##################
cl <- makeCluster(no_cores)
registerDoParallel(cl)
#export required to // nodes
clusterExport(cl, "problem_parameters", envir = .GlobalEnv) 

############EXECUTE CONFIGURATIONS#########
e <- simpleError("timeout reached")
#implement foreach // method
SA_results <- foreach(config_num = config_nos, .errorhandling = "pass") %dopar% {
  withCallingHandlers({ 
    setTimeLimit(elapsed = timeLim, transient = TRUE)
    SA_solve(config_num, all_configs, problem_parameters, t_max, p_max)
  }, error = function(e) e)#stop("timeout reached"))
}
stopCluster(cl)

#check if error occurred
if (sum(sapply(SA_results, is.error)) > 0){
  print(sprintf("Not all results captured...%d failed", sum(sapply(SA_results, is.error))))
}else{
  print("All results captured...")
}
############SAVE OBTAINED RESULTS##########
saveRDS(SA_results, file = paste(save_SA_dir, "/SA_", INSTANCE, ".rds", sep=""))
#print computation time
print(difftime(Sys.time(), st))
    
print("SA optimisation complete.")


