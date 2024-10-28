#script for controlling epochs during SA execution
  #1. c_max function
  #2. d_max function
  
    #extra functions:


#1.c_max function
c_max_function <- function(c_max, cool, par_vec){
  if (par_vec[3] == c_max){
    # print(sprintf("Cooling......%.5f -> %.5f", par_vec[1], par_vec[1]*cool))

    par_vec <- c(par_vec[1]*cool, par_vec[2]+1, 0, 0, 
                 par_vec[5], 0, par_vec[7], par_vec[8])
    return(par_vec)
  }else{
    return(par_vec)
  }
}

#2. d_max function
d_max_function <- function(d_max, heat, par_vec){
  if (par_vec[4] == d_max){
    # print(sprintf("Heating......%.5f -> %.5f", par_vec[1], par_vec[1]*heat))

        par_vec <- c(par_vec[1]*heat, par_vec[2]+1, 0, 0, 
                 par_vec[5], par_vec[6]+1, par_vec[7], par_vec[8])
    return(par_vec)
  }else{
    return(par_vec)
  }
}