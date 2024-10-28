#script for generating offspring population of a GA
  #1. Generate offspring population
    
    #extra functions:
      #i. Cross parents

    #i. Cross parents
    cross_parents <- function(parents, replacement, cross_prob, mutate_prob, CHT){
      parent_list <- 1:length(parents)
      #initialise list of offspring
      offspring <- list()
      #run through pairs to cross
      for (i in 1:ceiling(replacement/2)){
        #select crossing pair
        pair <- sample(parent_list, 2)
        #generate offspring if within crossing_probability
        if (runif(1) < cross_prob){
          offspring <- append(offspring, cross_1point(parents[[pair[1]]], parents[[pair[2]]], CHT, problem_parameters))
        }else{
          #clone parents
          offspring <- append(offspring, list(parents[[pair[1]]], parents[[pair[2]]]))
        }
        #update eligible parents for future crossing
        parent_list <- parent_list[-which(parent_list %in% pair)]
      }
      return(offspring)
    }

#1. Generate offspring population
generate_offspring <- function(parents, replacement, cross_prob, mutate_prob, CHT, problem_parameters){
  #Generate offspring solutions until unique offspring based on CHT
  repeat{
    #4. Cross random pairs of parents 
    #list of possible parents to cross
    offspring <- cross_parents(parents, replacement, cross_prob, mutate_prob, CHT)
    
    #5. Mutate individual offspring if within mutation_probability
    for (i in 1:length(offspring)){
      if (feasible(offspring[[i]], problem_parameters) & runif(1) < mutate_prob){
        offspring[[i]] <- move_operator(offspring[[i]], mutation, CHT, problem_parameters)
      }
    }
    #obtain FFVs for offspring
    off_ffvs <- sapply(1:length(offspring), function(x){fitness_function(offspring[[x]], problem_parameters, CHT)})
    
    #if rejection CHT then break if all offspring feasible
    if (CHT == "Rejection"){
      #evaluate feasibility state of solutions
      feas_states <- sapply(offspring, feasible, problem_parameters)
      if (mean(feas_states)==1){
        break
      }
    }else{
      #else if Penalty or Repair
        #then break if all offspring are unique
      break
    }
  }
  #return offspring
  return(append(offspring, list(off_ffvs)))
}