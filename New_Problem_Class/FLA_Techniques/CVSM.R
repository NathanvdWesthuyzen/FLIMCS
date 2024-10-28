#script for the Constraint Violation Severity Measure given a list of 1000 infeasible solutions
  #1. CVSM
  
  #extra functions:

#1. CVSM 
CVSM <- function(num, rand_sols, problem_parameters){
  #extract solution
  x <- rand_sols[[num]]
  #severity metric of upper and lower avg violation
    #only an upper capacity constraint
  degree <- c(NA,NA)
  #evaluate the average degree of constraint violation
  
  #Lower Constraints
  degree[1] <- <...>
  #Upper Constraints
  degree[2] <- <...>
  #return violations
  return(degree)
}
