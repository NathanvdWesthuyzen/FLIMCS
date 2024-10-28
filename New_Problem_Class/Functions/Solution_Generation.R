#the script for solution generation
  #1. random feasible solution
  #2. greedy heuristic LO solution
  #3. random solution (regardless of feasibility)
  #4. random infeasible solution 

#1. generate random feasible solution  
random_feas_solution <- function(num, problem_parameters){
  repeat{  
    x_i <...>
    if (feasible(x_i){
      #return solution
      return(x_i)
      #perhaps a counter of sorts?
    }
  }
}

#2. generate a greedy heuristic solution generation
hybrid_feas_solution <- function(problem_parameters, move, CHT){
  #define greedy search termination criteria based on implementation
  t_max <- 75
  p_max <- 50
  
  #initialise search based on local move and accept if superior, else reject
  t <- 0
  p <- 0
  x_i <- random_feas_solution(1, problem_parameters)
  repeat{
    x_p <- move_operator(x_i, move, "Rejection", problem_parameters)
    if (fitness_function(x_p, problem_parameters, CHT) < ########minimisation
        fitness_function(x_i, problem_parameters, CHT) & feasible(x_p, problem_parameters)){ 
      x_i <- x_p
      t <- t+1
      p <- 0
    }else{
      p <- p+1
    }
    # c <- c + 1
    #terminate after t_max iterations or p_max rejections (likely in a near-local minima)
    if (t > t_max | p > p_max){
      break
    }
  }
  #return solution
  return(x_i)
}

#3. random solution regardless of feasibility
random_solution <- function(num, problem_parameters){
  x_i <...>
  #return solution
  return(x_i)
}

#4. random infeasible solution 
random_infeas_solution <- function(num, problem_parameters){
  repeat{  
    x_i <...>
    if (!feasible(x_i){
      #return solution
      return(x_i)
      #perhaps a counter of sorts?
    }
  }
}