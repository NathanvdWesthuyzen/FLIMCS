#script for the accumulated escape probability given a list of an initial solution
  #extra functions

#1. AEP function: check all neighbours for superior fitness for a move
#1.A) one_move move
AEP_local <- function(num, initial_sols, problem_parameters, CHT){
  #extract solution
  x <- initial_sols[[num]]
  #determine FFv of current solution
  f_lvl <- fitness_function(x, problem_parameters, CHT)
  #superior neighbours count
  neighbours <- 0

  #find best n based on solution size
    #limit the number of neighbours if needed (computationally) - n
  n <- ...
  
  #run through number of possible neighbours by altering one bit at a time 
    #i.e. flip bits and compare FFVs of neighbour
  for (<2D or 1D binary encoding?>){
    #generate neighbour systematically
    x_p <- ...

    #if feasible and superior
    if (feasible(x_p, problem_parameters) & fitness_function(x_p, problem_parameters, CHT) <= f_lvl){ #### <- MINIMISATION setting
      neighbours <- neighbours + 1
    }
  }
  #return proportion of superior/equal neighbours
  return(neighbours/(nrow(x)*n))
}

#1.B) INSERTION move
AEP_global <- function(num, initial_sols, problem_parameters, CHT, no_globals){
  #extract solution
  x <- initial_sols[[num]]
  #determine FFv of current solution
  f_lvl <- fitness_function(x, problem_parameters, CHT)
  # superior neighbours count
  neighbours <- 0
  
  #run through X no. of global operators...
    #change X relative to n to evaluate representative-ness of subsample
  for (i in 1:no_globals){
    sol_eject <- ejection_chain(x, "Repair", problem_parameters)
    #evaluate if superior or equal
    f_eject <- fitness_function(sol_eject, problem_parameters, CHT) 
    if (feasible(sol_eject, problem_parameters) & f_eject <= f_lvl){   #### <- MINIMISATION setting
      # print(f_eject)
      neighbours <- neighbours + 1
    }
  }
  #return proportion of superior/equal neighbours
  return(neighbours/no_ejections)
}

