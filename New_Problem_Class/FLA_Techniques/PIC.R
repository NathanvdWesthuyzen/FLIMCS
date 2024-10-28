#script for the POPULATION INFORMATION CONTENT given a list of 64 initial solutions
  #1. PIC
  
  #extra functions:
  block_probabilities <- function(bit_string){
    #0. initialise two probabilities
    p_01 <- 0
    p_10 <- 0
    #1. examine a single block at a time and count occurances
    for (step in 2:length(bit_string)){
      if (identical(bit_string[c(step-1, step)], c(1,0))){
        p_01 <- p_01 + 1
      }else if (identical(bit_string[c(step-1, step)], c(0,1))){
        p_10 <- p_10 + 1
      }
    }
    #return occurances
    return(c(p_01,p_10)/length(bit_string))
  }
  
#1. PIC - with input a population walk containing f(x*) and mean(f(pop))
PIC <- function(num, pop_walks){                      #### <- MINIMISATION setting
  #1. extract strings
  f_inc <- pop_walks[[num]][[1]]
  f_mean <- pop_walks[[num]][[2]]
  
  #2. convert to bit strings based on improving iterations
  bit_inc <- as.numeric(diff(f_inc) < 0)
  bit_mean <- as.numeric(diff(f_mean) < 0)
  
  #3. calculate probabilities
  p_inc <- block_probabilities(bit_inc)
  p_mean <- block_probabilities(bit_mean)
  
  #4. remove 0 probabilities
  p_inc <- p_inc[which(p_inc != 0)]
  p_mean <- p_mean[which(p_mean != 0)]
  
  #5. calculate H
  H_inc <- - sum(p_inc * log(p_inc, 2))
  H_mean <- - sum(p_mean * log(p_mean, 2))
  
  #6. return H values
  return(c(H_inc, H_mean))
}
  
  