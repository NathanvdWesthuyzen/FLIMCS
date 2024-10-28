#script for fitness function evaluation with constraint handling techniques (CHT)
fitness_function <- function(x, problem_parameters, CHT){
  #calculate fitness function value (FFV) based on the implemented CHT
  #1) REJECTION & REPAIR FUNCTION
    #return unaltered FFV
  if (CHT != "Penalty"){
    #return FFV as is
    return(<FFV calculation here>)
  }else{
  #2) DYNAMIC PENALTY
    #return FFV with an additional penalty term if infeasible
    if (feasible(x, problem_parameters)){
      p1 <- <set to 0 or 1 depending on penalisation method>
    }else{
      #calculate appropriate penalty factor
      p1 <- <determine p1>
    }
    #return (possibly) penalised FFV
      #<- increase for minimisation (or) + & *
      #<- decrease for maximisation - & /
    return(<FFV calculation here>) (+) (-) (*) (/) p1) 
  }
}
