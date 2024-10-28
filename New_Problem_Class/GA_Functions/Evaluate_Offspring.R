#script for evaluating an offspring population of a GA for MINIMISATION
  #1. Evaluate offspring population and update accordingly

    #extra functions:


#1. Evaluate Population
evaluate_offspring <- function(offspring_list, off_ffvs, search_list, replacement, P){
  #no_of parents to remove
  no_par <- replacement - (P-length(search_list[[3]]))
  
  #get ranking of current population and delete worst lambda individuals
  worst_rankings <- rank(-search_list[[3]], ties.method = "first") # <-- minimisation
  worst <- which(worst_rankings <= no_par)
  
  #delete worst individuals and their FFVs if should be deleted
  if (no_par != 0){
    search_list[[2]] <- search_list[[2]][-worst]
    search_list[[3]] <- search_list[[3]][-worst]
  }
  
  #evaluate feasibility states of offspring
  feas <- sapply(offspring_list, feasible, problem_parameters)
  
  #best offspring from feasible options for x_incumbent
  if (sum(feas)==0){
    best_off_FFV_feas <- 100e5                          # <--- minimisation set to large number
  }else{
    best_off_FFV_feas <- min(off_ffvs[which(feas)])      # <--- minimisation
  }
  
  #get best offspring for replacement into
  best_off <- which.min(off_ffvs)                       # <--- minimisation
  
  #update current population and if better, the incumbent solution
  if (best_off_FFV_feas < search_list[[5]]){ # if an offspring is better than x* and feasible <-- minimisation
    #update incumbent solution
    search_list[[4]] <- offspring_list[[best_off]]
    search_list[[5]] <- min(off_ffvs)                          # <--- minimisation
  }
  #replace new population with offspring and FFVs accordingly
  if (replacement == 1){
    search_list[[2]] <- append(search_list[[2]], list(offspring_list[[best_off]]))
    search_list[[3]] <- append(search_list[[3]], off_ffvs[best_off])
  }else{
    search_list[[2]] <- append(search_list[[2]], offspring_list)
    search_list[[3]] <- append(search_list[[3]], off_ffvs)
  }
  #return re-evaluated search list
  return(search_list)
}
