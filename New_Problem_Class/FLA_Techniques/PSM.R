#script for the Pocket Size Measure given a list of 1000 initial solutions
  #1. PSM FEASIBLE
  #2. PSM INFEASIBLE

  #extra functions:
  #i. check all FEAS neighbours for moving item to new bin 
  Check_Neighbours_Feas <- function(x){
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
      if (feasible(x_p, problem_parameters)){ 
        neighbours <- neighbours + 1
      }else{
        return(FALSE)
      }
    }
    
    #return proportion of superior/equal neighbours
    return(TRUE)
  }
  
  #ii. check all INF neighbours for moving item to new bin 
  Check_Neighbours_Inf <- function(x){
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
      if (!feasible(x_p, problem_parameters)){ 
        neighbours <- neighbours + 1
      }else{
        return(FALSE)
      }
    }
    
    #return proportion of superior/equal neighbours
    return(TRUE)
  }

#1. PSM for a feasible sol
PSM_feasible <- function(num, initial_sols, pmax){
  #extract initial solution
  x <- initial_sols[[num]]
  if (is.na(x)[1]){
    return(NA)
  }else{
    #set LOW counter and flag to 0
    LOW <- 0
    flag <- 0
    
    #while neighbours all feasible
    while (flag == 0){
      #if all available neighbours feasible
      if (Check_Neighbours_Feas(x)){
        #add to LOW
        LOW <- LOW + 1
        #select random feasible LOCAL neighbour
        x <- move_operator(x, "one_move", "Rejection", problem_parameters)
        if (LOW == pmax){
          flag <- 1
        }
      }else{
        #trigger flag
        flag <- 1
      }
    }
    #return LOW for solution
    return(LOW)
  }
}

#2. PSM for an infeasible sol
PSM_infeasible <- function(num, infeas_sols, pmax){
  #extract initial solution
  x <- infeas_sols[[num]]
  if (is.na(x)[1]){
    return(NA)
  }else{
    #set LOW counter and flag to 0
    LOW <- 0
    flag <- 0
    
    #while neighbours all feasible
    while (flag == 0){
      print(LOW)
      #if all available neighbours feasible
      if (Check_Neighbours_Inf(x)){
        #add to LOW
        LOW <- LOW + 1
        #select random feasible LOCAL neighbour
        x <- move_operator(x, "one_move", "Rejection", problem_parameters)
        if (LOW == pmax){
          flag <- 1
        }
      }else{
        #trigger flag
        flag <- 1
      }
    }
    #return LOW for solution
    return(LOW)
  }
}
