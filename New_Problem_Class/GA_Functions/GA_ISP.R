#script for generating an initial population for GA initialisation
  #1. Random ISP
  #2. Hybrid ISP

#1. Random ISP given an input population size
ISP_random <- function(pop_size, problem_parameters){
  #generate initial population
  init_pop <- lapply(1:pop_size, random_feas_solution, problem_parameters)
  return(init_pop)
}

#2. Hybrid ISP given an input population size
ISP_hybrid <- function(pop_size, problem_parameters, mutation, CHT){
  #define random pop subset ratio
  if (pop_size > 20){
    subset <- 0.7
  }else{
    subset <- 0.5
  }
  P_hyb <- pop_size*(1-subset)
  P_rand <- pop_size*subset
  
  #generate random population based on subpop size
  init_pop <- ISP_random(P_rand, problem_parameters)
  
  #greedy search for remaining population
  for (i in 1:P_hyb){
    #add to initial population
    init_pop <- append(init_pop, list(hybrid_feas_solution(problem_parameters, mutation, CHT)))
  }
  #return initial population
  return(init_pop)
}
