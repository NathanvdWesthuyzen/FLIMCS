#script for a generating a neighbouring solution based on CHT 
  #1. Generate x' functions based on CHT

    #extra functions

#1. Generate x' with CHT as input, based on gamma
generate_x_p <- function(x, move, global, CHT, par_vector){
  #do while loop and break depending on CHT
  repeat{
    #conduct move operator until x_p is perturbed
    repeat{
      #2. Global only
      if (par_vector[7] > 1){
        x_p <- move_operator(x, global, CHT, problem_parameters)
      }else{
        #1. and 3. Local only and probabilistic 
        #apply move operator based on reducing probability
        prob <- 0.5*exp(-10*par_vector[5]/(par_vector[7]*par_vector[8]))
        if (runif(1) > prob){ 
          x_p <- move_operator(x, move, CHT, problem_parameters)
        }else{
          x_p <- move_operator(x, global, CHT, problem_parameters)
        }
      }
      #check if successfully perturbed
      if (length(which(x!=x_p)) != 0){
        break
      }
    }
    #2. return Penalty and Repair regardless
    if (CHT != "Rejection"){
      return(x_p)
    }else if (feasible(x_p, problem_parameters)){ #if Rejection and feasible, then return
      #CHT 1. Rejection:
      return(x_p)
    }
  }
}
