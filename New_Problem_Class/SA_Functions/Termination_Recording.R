#script for recording the state of a GA execution 
  #1. termination recording

    #extra functions:

#1. Termination recording for a static termination criterion
termination_record_static <- function(par_vector, configuration, start_time, search_list){
  #calc elapsed computational time
  elapsed_time <- proc.time()["elapsed"] - start_time["elapsed"]
  
  #compile termination information
  termination_info <- c(configuration[2], configuration[1], configuration[3], configuration[4], configuration[5], 
                        configuration[6], configuration[7], par_vector[5], par_vector[6], search_list[[2]], 
                        as.numeric(elapsed_time))
  #return termination info
  return(termination_info)
}

#2. Termination recording for a dynamic termination criterion
termination_record_dynamic <- function(par_vector, configuration, start_time, search_list){
  #calc elapsed computational time
  elapsed_time <- proc.time()["elapsed"] - start_time["elapsed"]
  
  #compile termination information
  termination_info <- c(configuration[2], configuration[1], configuration[3], configuration[4], configuration[5], 
                        configuration[6], configuration[7], par_vector[5], par_vector[6], search_list[[2]], 
                        as.numeric(elapsed_time))
  #return termination info
  return(termination_info)
}
