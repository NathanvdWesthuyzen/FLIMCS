#script for entropic measures: FEM and SEM given a list of n 1000-length vectors of fitness function values 
  #1. FEM and SEM 
    #extra functions:
    #FEM and SEM Technique symbol function
    Symbol_function <- function(f_i, f_j, epsilon){
      #3 class object: (-1,0,1)
      #triple if statement
      if (f_i-f_j < -epsilon){
        return(-1)
      }else if (abs(f_i-f_j) <= epsilon){
        return(0)
      }else if (f_i-f_j > epsilon){
        return(1)
      }
    }
    
    #FEM and SEM Technique probabilities function for p!=q
    Probabilities_H <- function(string){
      #intialise symbol probablilities vector
      symbol_probs <- matrix(0, dim(string)[1], 6)
      #step through eps rows
      for (eps in 1:dim(string)[1]){
        #step through cols
        for (step in 2:dim(string)[2]){
          #6 step if statement
          if (identical(string[eps, c(step-1, step)], c(-1,0))){
            symbol_probs[eps, 1] <- symbol_probs[eps, 1] + 1
            
          }else if (identical(string[eps, c(step-1, step)], c(-1,1))){
            symbol_probs[eps, 2] <- symbol_probs[eps, 1] + 1
            
          }else if (identical(string[eps, c(step-1, step)], c(0,-1))){
            symbol_probs[eps, 3] <- symbol_probs[eps, 1] + 1
            
          }else if (identical(string[eps, c(step-1, step)], c(0,1))){
            symbol_probs[eps, 4] <- symbol_probs[eps, 1] + 1
            
          }else if (identical(string[eps, c(step-1, step)], c(1,-1))){
            symbol_probs[eps, 5] <- symbol_probs[eps, 1] + 1
            
          }else if (identical(string[eps, c(step-1, step)], c(1,0))){
            symbol_probs[eps, 6] <- symbol_probs[eps, 1] + 1
            
          } 
        }
      }
      return(symbol_probs/(dim(string)[2]-1))
    }
    
    #FEM and SEM Technique probabilities function for p=q
    Probabilities_h <- function(string){
      #intialise symbol probablilities vector
      symbol_probs <- matrix(0, dim(string)[1], 3)
      #step through eps rows
      for (eps in 1:dim(string)[1]){
        #step through cols
        for (step in 2:dim(string)[2]){
          #6 step if statement
          if (identical(string[eps, c(step-1, step)], c(-1,-1))){
            symbol_probs[eps, 1] <- symbol_probs[eps, 1] + 1
            
          }else if (identical(string[eps, c(step-1, step)], c(0,0))){
            symbol_probs[eps, 2] <- symbol_probs[eps, 1] + 1
            
          }else if (identical(string[eps, c(step-1, step)], c(1,1))){
            symbol_probs[eps, 3] <- symbol_probs[eps, 1] + 1
            
          }
        }
      }
      return(symbol_probs/(dim(string)[2]-1))
    }
    
    #calculate eps_star a single time given the RWs as input
    calc_eps_star <- function(RWs){
      #store all eps_stars for each RW
      RW_eps <- numeric()
      for (i in 1:length(lengths(RWs))){
        #extract FFvs
        store_1 <- as.numeric(unlist(RWs[[i]]))
        walk <- length(store_1)
        
        f_diffs <- numeric()
        #obtain list of diff(f_i, f_i+1)
        for (j in 2:walk){
          #append difference in FFVs
          f_diffs <- append(f_diffs, store_1[j] - store_1[j-1])
        }
        
        RW_eps <- append(RW_eps, max(f_diffs))
      }
      print(RW_eps)
      #return largest obtained value
      return(max(RW_eps)*1.05)
    }
    
#1. FEM and SEM 
FEM_SEM <- function(run, RWs, eps_star){
  #extract fitness stroage values
  store_1 <- as.numeric(unlist(RWs[[run]]))
  walk <- length(store_1)
  
  #obtain eps_star value 
  eps_star <- calc_eps_star(RWs)
  
  #generate \eps sequence 0->/2^7
  eps_1 <- c(0,eps_star/128,eps_star/64,eps_star/32,eps_star/16,eps_star/8,
             eps_star/4,eps_star/2,eps_star)
  
  #Initialise string vector of symbols (S(eps)) for each fitness function
  S_1 <- matrix(numeric(), length(eps_1), length(store_1)-1)
  
  #step through range of \eps values to generate S(eps)
  for (eps in 1:length(eps_1)){
    #Run through fitness values
    for(step in 2:(walk)){
      #call symbol function for each s_i
      S_1[eps, step-1] <- Symbol_function(store_1[step], store_1[step-1], eps_1[eps])
    }
  }
  
  #evaluate each combination of P_pq for H and h
  P_pq_H_1 <- Probabilities_H(S_1)
  P_pq_h_1 <- Probabilities_h(S_1)
  
  #Calculate H and h for each eps value eps_length fitness functions
  HHH <- matrix(0, 1, length(eps_1))
  hhh <- matrix(0, 1, length(eps_1))
  
  #step through each eps value
  for (eps in 1:length(eps_1)){
    #calculate H_STGG
    HHH[eps]<- -sum(P_pq_H_1[eps,which(P_pq_H_1[eps,]!=0)]*log(P_pq_H_1[eps,which(P_pq_H_1[eps,]!=0)],6))
    hhh[eps]<- -sum(P_pq_h_1[eps,which(P_pq_h_1[eps,]!=0)]*log(P_pq_h_1[eps,which(P_pq_h_1[eps,]!=0)],3))
  }
  
  #return row vector of values
  return(c(HHH,hhh))
}









