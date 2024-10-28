#script for a determining the initial temperature of a SA search for an instance 
  #1. INITIAL TEMPERATURE FUNCTION for MINIMISATION

    #extra functions

#1. INITIAL TEMPERATURE FUNCTION
find_T0 <- function(iterations, problem_parameters, move, CHT){
  #record increase in energy between iterations
  record <- numeric()
  
  #start pilot search - for ... iterations
  x <- random_feas_solution(1, problem_parameters) #initial solution
  
  #random walk for k iterations
  for (k in 1:iterations){
    #generate feasible neighbour
    repeat{
      x_p <- move_operator(x, move, "Rejection", problem_parameters)
      if (feasible(x_p, problem_parameters)){ #if feasible, then break
        break
      }
    }
    
    #calculate energy
    f_diff <- fitness_function(x,problem_parameters,CHT) -  
      fitness_function(x_p,problem_parameters,CHT)                ########<--- MINIMISATION setting
    
    #if energy worse then store degredation value
    if (f_diff < 0){                                     
      record <- append(record, f_diff)
    }
    #accept solution regardless
    x <- x_p
  }
  
  #calculate initial temperature and return with an acceptance ratio of 80%
  T_0 <- mean(record)/(log(0.8))
  print(sprintf("T_0 calculated: %.3f", T_0))
  return(T_0)
}
