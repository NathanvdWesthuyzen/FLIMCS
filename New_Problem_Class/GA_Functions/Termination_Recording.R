#script for recording the state of a GA execution 
  #1. termination recording

    #extra functions:

#1. Termination recording for a static termination criterion
termination_record_static <- function(search_list, configuration, start_time, selection_procedure){
  #calc elapsed computational time
  elapsed_time <- proc.time()["elapsed"] - start_time["elapsed"]
  
  #compile termination information
  termination_info <- c(configuration[1], configuration[7], selection_procedure, configuration[2], 
                        configuration[3], configuration[4], configuration[5], configuration[6],
                        search_list[[1]], search_list[[6]], search_list[[5]], as.numeric(elapsed_time))
  #return termination info
  return(termination_info)
}

#2. Termination recording for a dynamic termination criterion
termination_record_dynamic <- function(search_list, configuration, start_time, selection_procedure){
  #calc elapsed computational time
  elapsed_time <- proc.time()["elapsed"] - start_time["elapsed"]
  
  #compile termination information
  termination_info <- c(configuration[1], configuration[7], selection_procedure, configuration[2], 
                        configuration[3], configuration[4], configuration[5], configuration[6],
                        search_list[[1]], search_list[[6]], search_list[[5]], as.numeric(elapsed_time))
  #return termination info
  return(termination_info)
}
