#script for evaluating the PEM of a population relative to the incumbent solution x*
  #1. Evaluate the PEM of Pi relative to f(x*)

    #extra functions:

#1. Population evolvability metric of a population
PEM_Pi <- function(off_ffvs, f_x_inc_before, replacement, P, search_list){
  #evaluate differently if replacement == 1
  if (replacement == 1){#evaluate offspring of  Pij, M=1
    #select best offspring to evaluate Pij
    off_ffv_1 <- off_ffvs[which.max(off_ffvs)]
    
    #extract offspring superior to x*
    off_ffv_1_superior <- ifelse(off_ffv_1 < f_x_inc_before, off_ffv_1, numeric()) ### <---- minimisation
    
    #if none are superior then return eap of 0
    if (is.na(off_ffv_1_superior)){
      return(0)
    }else{
      epp <- 1
      eap <- (sum(abs(f_x_inc_before - off_ffv_1_superior)/(P*sd(search_list[[3]]))))/(replacement)
      return(epp*eap)
    }
  }else{#evaluate all offspring for Pij, M>1
    #extract offspring superior to x*
    off_ffv_superior <- off_ffvs[which(off_ffvs < f_x_inc_before)]  ### <---- minimisation
    
    #if none are superior then return eap of 0
    if (length(off_ffv_superior) == 0){
      return(0)
    }else{
      epp <- (length(off_ffv_superior))/(replacement)
      eap <- (sum(abs(f_x_inc_before - off_ffv_superior)/(P*sd(search_list[[3]]))))/(replacement)
      return(epp*eap)
    }
  }
}

#PEM for a population-based walk
PEM_Pop_walk <- function(off_ffvs, f_x_inc_before, replacement, P, search_list){
  #evaluate all offspring for Pij, M>1
  #extract offspring superior to x*
  off_ffv_superior <- off_ffvs[which(off_ffvs < f_x_inc_before)]  ### <---- minimisation
  
  #if none are superior then return eap of 0
  if (length(off_ffv_superior) == 0){
    return(0)
  }else{
    epp <- (length(off_ffv_superior))/(replacement)
    eap <- (sum(abs(f_x_inc_before - off_ffv_superior)/(P*sd(search_list[[3]]))))/(replacement)
    return(epp*eap)
  }
}
