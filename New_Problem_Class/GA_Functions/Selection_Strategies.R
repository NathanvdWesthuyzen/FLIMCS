#script for selecting parents for offspring generation for MINIMISATION
  #1. Tournament selection
  #2. Ranked-based selection

    #extra functions:

#1. Tournament selection of parents
selection_tournament <- function(search_list, select_param, replacement, CHT){
  #tournament size:
  k <- select_param
  #define no. of parents
  if (replacement == 1){
    no_of_parents <- 2
  }else{
    no_of_parents <- replacement
  }
  
  #eligible population for parents
  elig_pop <- 1:length(search_list[[3]])
  #initialise selected parents
  selected_parents <- list()
  #run through no. of parents to select --> based on the no. of offspring to generate
  for (i in 1:no_of_parents){
    #sample k number of solutions for the tournament
    tourney_indxs <- sample(elig_pop, k)
    
    #extract best individual from contestants
    tourney_ffvs <- search_list[[3]][tourney_indxs]
    winner_indx <- tourney_indxs[which.min(tourney_ffvs)] ### <--- MINIMISATION setting
    winner <- search_list[[2]][[winner_indx]]
    
    #update eligible population --> delete winner
    elig_pop <- elig_pop[!elig_pop == winner_indx]
    
    #append winner to the number of parents to select
    selected_parents <- append(selected_parents, list(winner))
  }
  #return list of selected parents
  return(selected_parents)
}

#2. Ranked-based selection of parents
selection_ranked <- function(search_list, select_param, replacement, P){
  #selection pressure
  s <- select_param
  
  #define no. of parents
  if (replacement == 1){
    no_of_parents <- 2
  }else{
    no_of_parents <- replacement
  }

  #obtain ranking for each eligible parent
  pure_rank <- rank(search_list[[3]], ties.method = "first")    ###<---- MINIMISATION setting
  #obtain selection probabilities
  select_probs <- (2-s)/P + (2*(pure_rank-1)*(s-1))/(P*(P-1))

  #generate random numbers (0-->1) and select parents based on cumulative probabilities
    #however, ensure that the same parent is not selected more than once
  repeat{
    #select parent indices
    par_indxs <- sapply(runif(no_of_parents, 0, 1), function(x){min(which(x < cumsum(select_probs)))})
    #break if distinct parents are selected
    if (length(unique(par_indxs)) == length(par_indxs)){
      break
    }
  }
  #extract and return selected parents
  selected_parents <- lapply(par_indxs, function(x) {search_list[[2]][[x]]})
  return(selected_parents)
}


