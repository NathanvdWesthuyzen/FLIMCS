#script for executed FLA and saving results - for a MINIMISATION problem
#clear workspace:
rm(list = ls())

#############LIBRARIES#################
#import required libraries
library(dplyr)
library(doParallel)
library(stringr)
library(pryr)

#############FUNCTIONS#################
#get current dir based on PC implementation (assumption of current dir.)
base_dir <- getwd()
no_cores <- 6 #a minimum of 32 cores recommended for full portfolio implementation
print(paste("No. of cores specified:", no_cores))
#import RW data results in "FLA_Samples" dir.
save_gen_dir <- paste(base_dir, "FLA_Samples", sep="/")
#save FLA data results in "FLA_Results" dir.
save_fla_dir <- paste(base_dir, "FLA_Results", sep="/")

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
  hybrid_solutions <- function(num, problem_parameters, move){
    return(hybrid_feas_solution(problem_parameters, move, "Rejection"))
  }
  #import fitness function functions
  source(paste(funcs_dir, "Fitness_Function_CHT.R", sep="/"))
  #import random walk functions
  source(paste(funcs_dir, "FLA_RW.R", sep="/"))

############FLA DEFINTIONS###############
#A) RANDOM FEASIBLE SOLUTIONS
if (HPC){
  no_sols <- 1000
}else{
  no_sols <- 1000
}

#B) RANDOM WALKS
walk_length <- 1000
no_walks <- no_cores #(HPC and PC friendly)
no_globals <- 50               #<----- limit no. of global moves if needed
pop_walk_length <- 500

#C) SELECTED MOVE OPERATORS
#summarise all moves
moves <- c("one_move", "ejection_chain", 1, 0.5, 0.1)
plain_moves <- c("one_move", "ejection_chain")
move_local <- moves[1]
move_global <- moves[2]
gammas <- c(1, 0.5, 0.1)
mutation <- move_local

#D) FIX CHT TO REJECTION
CHT <- "Rejection"

#############PREP // COMPUTATION##########
cl <- makeCluster(no_cores)
registerDoParallel(cl)

#############FLA TECHNIQUES#############
#define FLA dir
FLA_dir <- paste(base_dir, "FLA_Techniques", sep="/")
#IMPORT FLA TECHNIQUES
source(paste(FLA_dir, "Autocorrelation.R", sep="/"))
source(paste(FLA_dir, "Entropic_Measures.R", sep="/"))
source(paste(FLA_dir, "AEP.R", sep="/"))
source(paste(FLA_dir, "HDIL.R", sep="/"))
source(paste(FLA_dir, "HDFM.R", sep="/"))
source(paste(FLA_dir, "PSM.R", sep="/"))
source(paste(FLA_dir, "CVSM.R", sep="/"))
GA_dir <- paste(base_dir, "GA_Functions", sep="/")
source(paste(GA_dir, "Generate_Offspring.R", sep="/"))
source(paste(GA_dir, "Evaluate_Offspring.R", sep="/"))
source(paste(GA_dir, "Selection_Strategies.R", sep="/"))
source(paste(GA_dir, "GA_ISP.R", sep="/"))
source(paste(FLA_dir, "PEM.R", sep="/"))
source(paste(FLA_dir, "NSC.R", sep="/"))
source(paste(FLA_dir, "PIC.R", sep="/"))

#export all functions to // nodes
clusterExport(cl, ls(), envir = .GlobalEnv)

#############IMPORT INSTANCES############
#cycle through for loop for each problem class then instance
for (INSTANCE in INSTANCES){
  print(sprintf("%s instance", INSTANCE))
  st <- Sys.time()
  #############IMPORT INSTANCE#############
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
  clusterExport(cl, "problem_parameters", envir = .GlobalEnv)
  print("Exported...")
  
  #############FLA GENERATION##########
  #import RWs for each move operator
  RW_local <- readRDS(file = paste(save_gen_dir, "/", INSTANCE, "_RWs_", moves[1], ".rds", sep=""))
  RW_global <- readRDS(file = paste(save_gen_dir, "/", INSTANCE, "_RWs_", moves[2], ".rds", sep=""))
  RW_g1 <- readRDS(file = paste(save_gen_dir, "/", INSTANCE, "_RWs_g_",
                                str_replace(as.character(moves[3]),"[.]", "_"), ".rds", sep=""))
  RW_g2 <- readRDS(file = paste(save_gen_dir, "/", INSTANCE, "_RWs_g_",
                                str_replace(as.character(moves[4]),"[.]", "_"), ".rds", sep=""))
  RW_g3 <- readRDS(file = paste(save_gen_dir, "/", INSTANCE, "_RWs_g_",
                                str_replace(as.character(moves[5]),"[.]", "_"), ".rds", sep=""))
  print("RWs imported...")
  
  ############EXECUTE FLA TECHNIQUES######
  #i. generate random initial solutions
  initial_sols <- parLapply(cl=cl, X=1:no_sols, fun=random_feas_solution, problem_parameters)
  print("Initial solutions generated...")
  
  print(mem_used())
  
  #ii. cycle through each move operator and rbind the results
  AF_moves <- numeric()
  cor_moves <- numeric()
  FEMSEM_moves <- numeric()
  AEP_moves <- numeric()
  for (mo in moves){
    print(mo)
    if (mo == "one_move"){
      RWs <- RW_local
    }else if (mo == "ejection_chain"){
      RWs <- RW_global
    }else if (mo == "1"){
      RWs <- RW_g1
    }else if (mo == "0.5"){
      RWs <- RW_g2
    }else{
      RWs <- RW_g3
    }

    #1. AUTOCORRELATION FUNCTION
    # define lag values
    d <- 1:10
    #extract AF
    AF_d <- colMeans(t(as.matrix(parSapply(cl=cl, X=1:length(RWs), FUN=auto_corr, RWs, d, walk_length))))
    AF_moves <- rbind(AF_moves, AF_d)
    # print("Autocorrelation done.")

    #2. CORRELATION LENGTH
    cor_l <- -1/log(abs(AF_d[1]))
    cor_moves <- rbind(cor_moves, cor_l)
    # print("Correlation length done.")

    #3. ENTROPIC MEASURES
    #determine FEM and SEM // function
    FEMSEM <- colMeans(t(as.matrix(parSapply(cl=cl, X=1:length(RWs), FUN=FEM_SEM, RWs))))
    FEMSEM_moves <- rbind(FEMSEM_moves, FEMSEM)
    # print("Entropic measures done.")

    #4. ACCUMULATED ESCAPE METRIC (FOR TRAJECTORY)
    #calculate AEP of neighbours based on MO
    if (mo == "one_move"){
      AEP_loc <- mean(parSapply(cl=cl, X=1:(no_sols), FUN=AEP_local, initial_sols, problem_parameters, CHT))
    }else if (mo == "ejection_chain"){
      AEP_glob <- mean(t(as.matrix(parSapply(cl=cl, X=1:(no_sols),
                                             FUN=AEP_global, initial_sols, problem_parameters, CHT, no_globals))))
    }
  }
  AEP_moves <- rbind(AEP_loc, AEP_glob, AEP_loc, AEP_loc, AEP_loc)
  # print("AEP done.")
  print("Trajectory-based measures done.")

  #5. HAMMING DISTANCE IN A LEVEL
  #obtain the FFVs for all initial solutions
  f_lvls <- sapply(initial_sols, fitness_function, problem_parameters, CHT)
  #define iso-cost levels based on mean and SD
  range_1 <- range(f_lvls)
  lim_1 <- c(range_1[1], mean(f_lvls)-0.5*sd(f_lvls),
             mean(f_lvls)+0.5*sd(f_lvls), range_1[2]*1.01)

  #identify lvls for HDIL
  iso_lvls <- sapply(1:no_sols, organise_levels, initial_sols, lim_1)

  #if HDIL ranges are the same, then
  if (length(unique(iso_lvls)) < 3){
    lvls <- unique(iso_lvls)
  }else{
    lvls <- 1:3
  }

  #analyse HDIL at each lvl
  HDIL <- numeric()
  for (lvl in lvls){
    # print(lvl)
    #determine list of sols in level
    HDIL_combos <- combn(which(iso_lvls==lvl), 2)

    #// computations
    D_IL <- mean(t(as.matrix(parSapply(cl=cl, X=1:ncol(HDIL_combos), FUN=HDIL_for_combo, HDIL_combos, initial_sols))))
    HDIL <- append(HDIL, D_IL)
  }
  if (length(lvls) == 2){
    HDIL_moves <- matrix(c(HDIL, HDIL[2]), nrow=length(moves), ncol=3, byrow=TRUE)
  }else{
    HDIL_moves <- matrix(HDIL, nrow=length(moves), ncol=3, byrow=TRUE)
  }
  HDIL_moves <- HDIL_moves/(problem_parameters[[3]]^2)  #<---- scaled based on solution size
  print("HDIL done.")
  
  #6. HAMMING DISTANCE FEASIBILITY MEASURE
  #i generate solutions regardless of feasibility
  rand_sols <- parLapply(cl=cl, X=1:(no_sols), fun=random_solution, problem_parameters)
  #check feasibility states and split
  feasibility_states <- sapply(1:no_sols, function(x){feasible(rand_sols[[x]], problem_parameters)})
  feas_split <- sum(feasibility_states)/no_sols

  HDFM_row <- t(as.matrix(parSapply(cl=cl, X=1:length(rand_sols), FUN=HDFM, rand_sols, feasibility_states, feas_split, problem_parameters)))
  HDFM_moves <- matrix(colMeans(HDFM_row, na.rm=TRUE), nrow=length(moves), ncol=ncol(HDFM_row), byrow=TRUE)
  print("HDFM done.")

  #7. POCKET SIZE MEASURE
  #set computational limit, p_max
  pmax <- 30
  #i. for a set of feasible solutions
  PSM_feas_info <- parSapply(cl=cl, X=1:no_cores, FUN=PSM_feasible, initial_sols, pmax)
  #i.i avg feasible pocket size
  PSM_feas <- mean(PSM_feas_info, na.rm=TRUE)
  #i.ii proportion of zero length walks
  zero_p_feas <- length(which(PSM_feas_info == 0))/length(PSM_feas_info)
  print("PSM feasible done.")

  #generate a set of infeasible solutions
  infeas_sols <- parLapply(cl=cl, X=1:no_sols, fun=random_infeas_solution, problem_parameters)
  #ii. for a set of infeasible solutions
  PSM_infeas_info <- parSapply(cl=cl, X=1:no_cores, FUN=PSM_infeasible, infeas_sols, pmax)
  #i.i avg feasible pocket size
  PSM_infeas <- mean(PSM_infeas_info, na.rm=TRUE)
  #i.ii proportion of zero length walks
  zero_p_infeas <- length(which(PSM_infeas_info == 0))/length(PSM_infeas_info)

  PSM_row <- c(PSM_feas, PSM_infeas, zero_p_feas, zero_p_infeas)
  PSM_moves <- matrix(PSM_row, nrow=length(moves), ncol=length(PSM_row), byrow=TRUE)
  print("PSM infeasible done.")

  #8. CONSTRAINT VIOLATION SEVERITY MEASURE
  #use set of randomly generated solutions regardless of feasibility
    # - demonstrates how "easy" it actually is to violate...
  violations <- t(parSapply(cl=cl, X=1:no_sols, FUN=CVSM, rand_sols, problem_parameters))
  CVSM_moves <- matrix(colMeans(violations, na.rm=TRUE), nrow=length(moves), ncol=2, byrow=TRUE)
  print("CVSM done.")

  #i. population-based walks
  pop_walks <- parLapply(cl=cl, X=1:no_cores, fun=population_walk_w_PEM, problem_parameters, pop_walk_length)
  # print(length(pop_walks))
  print("Population-based walks conducted...")

  #9. POPULATION EVOLVABILITY METRIC (for POPULATION-BASED)
  PEM_values <- lapply(pop_walks, function(x) x[3])
  PEM_moves <- matrix(colMeans(do.call(rbind, lapply(PEM_values, `[[`, 1)), na.rm = TRUE),
                      nrow=length(moves), ncol=3, byrow=TRUE)
  print("PEM done.")
  
  #i. generate some hybrid solutions for analysis
  hybrid_sols <- parLapply(cl=cl, X=1:(no_sols), fun=hybrid_solutions, problem_parameters, move_local) ########################
  print("Hybrid solutions generated...")
  
  #10. NEGATIVE SLOPE COEFFICIENT
  #10.1 collect parent and offspring FFVs
  NSC_ffv_values <- parLapply(cl=cl, X=1:no_sols, fun=NSC_FFVs, initial_sols, hybrid_sols, problem_parameters)
  print("NSC ffvs values attained...")
  
  #10.2 apply size driven bisection
  #10.2.1 extract all parent ffvs
  parent_values <- unlist(lapply(NSC_ffv_values, function(x) x[1:2]))
  
  #if all parents are the same...
  if (length(unique(parent_values)) <= 2){
    print("Minimal parent variation...")
    NSC_moves <- matrix(c(0,0), nrow=length(moves), ncol=2, byrow=TRUE)
  }else{
    #10.2.2 get the initial limit points for parents (size-wise)
    mid_point <- median(parent_values)
    one_split_limits <- c(min(parent_values), mid_point, max(parent_values))
    #if one split limits too close, then ignore SDB
    if (0 %in% diff(one_split_limits)){
      final_split_limits <- one_split_limits
    }else{
      #10.2.3 Apply size-driven bisection to the parent_values
      max_points <- 50        # max size value
      min_bin_size <- 0.01    # minimum bin size
      final_split_limits <- size_driven_bisection(parent_values, one_split_limits, max_points, min_bin_size)
    }
    
    #10.3 calculate NSC
    NSC_moves <- matrix(NSC(final_split_limits, parent_values, NSC_ffv_values), 
                        nrow=length(moves), ncol=2, byrow=TRUE)
  }
  print("NSC done.")
  
  #11. POPULATION INFORMATION CONTENT
  PIC_values <- parLapply(cl=cl, X=1:length(pop_walks), fun=PIC, pop_walks)
  PIC_inc <- mean(unlist(lapply(PIC_values, function(x) x[1])))
  PIC_mean <- mean(unlist(lapply(PIC_values, function(x) x[2])))
  PIC_moves <- matrix(c(PIC_inc, PIC_mean), nrow=length(moves), ncol=2, byrow=TRUE)
  print("PIC done.")


  #combine all features into single row vector
  fla_inst <- cbind(rep(INSTANCE, 5), moves, AF_moves, cor_moves, FEMSEM_moves, AEP_moves,
                    HDIL_moves, HDFM_moves, PSM_moves, CVSM_moves,
                    PEM_moves, NSC_moves, PIC_moves)

  #assign column names for easy labelling in the future
  col_labels <- c("instance", "moves", "AF_1","AF_2","AF_3","AF_4","AF_5","AF_6","AF_7","AF_8","AF_9","AF_10",
                  "CL","H_0","H_128","H_64","H_32","H_16","H_8","H_4","H_2","H_star",
                  "h_0","h_128","h_64","h_32","h_16","h_8","h_4","h_2","h_star",
                  "AEP", "HDIL_1", "HDIL_2", "HDIL_3",
                  'feas_N', 'infeas_N', 'feas_split', 'feas_pocket', 'infeas_pocket',
                  'PSM_feas', 'PSM_infeas', 'zero_p_feas', 'zero_p_infeas',
                  'avg_viol_lower', 'avg_viol_upper',
                  'PEM_avg', 'PEM_sd', 'change_rate',
                  'NSC', 'NSC_mean',
                  'PIC_x*', 'PIC_avg')

  colnames(fla_inst) <- col_labels
  #save FLA vector based on problem instance and move operator
  saveRDS(fla_inst, file = paste(save_fla_dir, "/FLA_", INSTANCE, ".rds", sep=""))
  print(difftime(Sys.time(), st, "min"))
  
  remove("initial_sols", "infeas_sols", "rand_sols", "hybrid_sols") #if instances done in a loop
  gc()
  print(mem_used())
}

print("Extraction complete.")
#terminate parallel execution
stopCluster(cl)
