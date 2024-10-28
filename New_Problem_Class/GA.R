#script for GA optimisation given an input GA configuration for MINIMISATION

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
save_GA_dir <- paste(base_dir, "GA_Results", sep="/")

#specify which instance to consider (via ID)
INSTANCES <- 1

#set time limit for configuration execution
timeLim <- 130 #seconds for each configuration

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

#define GA functions dir
GA_funcs_dir <- paste(base_dir, "GA_Functions", sep="/")
  #import ISP functions
  source(paste(GA_funcs_dir, "GA_ISP.R", sep="/"))
  #import selection strategies functions
  source(paste(GA_funcs_dir, "Selection_Strategies.R", sep="/"))
  #import offspring evaluation functions
  source(paste(GA_funcs_dir, "Evaluate_Offspring.R", sep="/"))
  #import Termination recording functions
  source(paste(GA_funcs_dir, "Termination_Recording.R", sep="/"))
  #import offspring generation function
  source(paste(GA_funcs_dir, "Generate_Offspring.R", sep="/"))
  #import GA execution function
  source(paste(GA_funcs_dir, "GA_Solve.R", sep="/"))

############GA DEFINTIONS###############
#A) POPULATION SIZE
pop_size <- c(20,40,60)

#B) INITIAL POPULATION PROCEDURE
ISPs <- c("Random", "Hybrid")

#C) SELECTION STRATEGY --> "Tournament" or "Rank-based"
k <- c(3,10)
# or 
s <- c(1.5,2)

#D) SELECTED CROSSOVER MOVE OPERATOR w/ PROBABILITY
crossover <- "1_point"
prob_cross <- c(0.4,0.7,0.95)

#E) SELECTED MUTATION MOVE OPERATOR w/ PROBABILITY
mutation <- "one_move"       ### <---- set mutation operator correctly
prob_mut <- c(0.01,0.05,0.1)

#F) REPLACEMENT STRATEGY --> "Steady-state" or "Elitism"
lambda <- c(1,2,8)

#G) TERMINATION CRITERIA --> "Static" or "Dynamic"
  #define the static termination criteria based on the computational resources
t_max <- c(100, seq(from=200, to=1e3, by=100), 1.5e3, 2e3) 
  #define the dynamic termination criteria based on the computational resources
t_non <- c(50,100,200,250,500,750,1e3,2e3)

#H) CONSTRAINT HANDLING TECHNIQUE
CHTs <- c("Rejection", "Penalty", "Repair") 

#############GA ALL CONFIGS############
all_tournament_configs <- expand.grid(ISPs,k,prob_cross,
                                      prob_mut,lambda,CHTs,pop_size)
all_ranked_configs <- expand.grid(ISPs,s,prob_cross,
                                  prob_mut,lambda,CHTs,pop_size)
#label columns
na_mes <- c("ISP", "selection", "cross_prob", "mutate_prob", "replacement", "CHT", "pop_size")
colnames(all_tournament_configs) <- na_mes
colnames(all_ranked_configs) <- na_mes

#combine all configs into a list of 2 config_dfs
all_configs <- list(all_tournament_configs, all_ranked_configs)
#total no of configurations
if (HPC){
  config_nos <- 1:(2*nrow(all_configs[[1]]))
}else{
  config_nos <- round(seq(1, 1944, length.out=no_cores))
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
#defined problem parameters in a list
problem_parameters <- list(var_1, var_2, ..., var_n)

#############// EXECUTION##################
cl <- makeCluster(no_cores)
registerDoParallel(cl)
#export all instance info to // nodes
clusterExport(cl, "problem_parameters", envir = .GlobalEnv)

############EXECUTE GA CONFIGURATIONS######
e <- simpleError("timeout reached")
# try foreach method
GA_results <- foreach(config_num = config_nos, .errorhandling = "pass") %dopar% {
  withCallingHandlers({
    setTimeLimit(elapsed = timeLim, transient = TRUE)
    GA_solve(config_num, all_configs, problem_parameters, t_max, t_non)
  }, error = function(e) e)#stop("timeout reached"))
}
stopCluster(cl)

#check if error occurred
if (sum(sapply(GA_results, is.error)) > 0){
  print(sprintf("Not all results captured...%d failed", sum(sapply(GA_results, is.error))))
}else{
  print("All results captured...")
}

############SAVE OBTAINED RESULTS##########
saveRDS(GA_results, file = paste(save_GA_dir, "/GA_", INSTANCE, ".rds", sep=""))
#print computation time
print(difftime(Sys.time(), st))

print("GA optimisation complete.")