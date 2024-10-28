#script for the autocorrelation function given a list of n 1000-length vectors of fitness function values 
  
#1. Autocorrelation
auto_corr <- function(run, RWs, d, walk_length){
  #correlation storage vectors
  r_1_store <- numeric()
  #convert stored vectors to numeric()
  store_1 <- as.numeric(unlist(RWs[[run]]))
  #mean objective values
  f_mean_1 <- mean(store_1)
  #variance of FFVs
  var_store_1 <- var(store_1)
  #Run through all step sizes
  for (step in d){
    
    #empty numerators
    num_1 <- 0
    
    for(i in 1:(walk_length-step)){
      num_1 <- num_1 + (store_1[i]-f_mean_1)*(store_1[i+step]-f_mean_1)
    }
    r_1 <- (num_1)/((walk_length-step)*var_store_1)
    
    #store correlation lengths (of length d)
    r_1_store <- append(r_1_store, r_1)
  }
  
  return(r_1_store)
}