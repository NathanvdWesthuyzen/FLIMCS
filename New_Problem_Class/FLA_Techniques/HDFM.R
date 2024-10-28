#script for the Hamming Distance Feasibility Measure given a list of 1000 initial solutions
  #extra functions:

#1. HDFM
HDFM <- function(num, rand_sols, feasibility_states, feas_split, problem_parameters){
  #extract solution
  x <- rand_sols[[num]]
  neighbours <- 0
  
  #find best n based on solution size
    #limit the number of neighbours if needed (computationally) - n
  n <- ...
  
  #run through number of possible neighbours by moving each item to a different bin
  #i.e. move item and check feasibility
  for (<2D or 1D binary encoding?>){
    #generate neighbour systematically
    x_p <- ...
      
    #if feasible and superior
    if (feasible(x_p, problem_parameters)){ 
      neighbours <- neighbours + 1
    }
  }
  
  neighbours <- sum(neighbours)
  #compile results
  if (feasibility_states[num]){  #feas
    feas_N <- neighbours/(nrow(x)*n)
    infeas_N <- NA
    if (neighbours == 0){ #pocket
      feas_pocket <- 1
      infeas_pocket <- NA
    }else{
      feas_pocket <- 0
      infeas_pocket <- NA
    }
  }else{                            #infeas
    feas_N <- NA
    infeas_N <- neighbours/(nrow(x)*n)
    if (neighbours == 0){ #pocket
      feas_pocket <- NA
      infeas_pocket <- 1
    }else{
      feas_pocket <- NA
      infeas_pocket <- 0
    }
  }
  #compile
  return(c(feas_N, infeas_N, feas_split, feas_pocket, infeas_pocket))
}
