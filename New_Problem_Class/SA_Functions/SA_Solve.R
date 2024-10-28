#script for a single SA configuration given an input configuration for MINIMISATION
SA_solve <- function(config_num, all_configs, problem_parameters, t_max, p_max){
  start_time <- proc.time()
  #empty results
  recorded_results_static <- numeric()
  recorded_results_dynamic <- numeric()
  temps <- numeric()
  FFVs <- numeric()
  #################CONFIGURATION######################
  configs <- all_configs[config_num,]
  print(configs)
  
  #extract remaining config parameters??????
  gamma       <- as.numeric(configs[1])        #reducing probabaility
  ISP         <- configs[2]                    #initial solution procedure
  cool        <- as.numeric(configs[3])        #cooling parameter
  heat        <- as.numeric(configs[4])        #heating parameter
  c_max       <- as.numeric(configs[5])        #no. accepts
  d           <- as.numeric(configs[6])        #no. of rejects more than accepts
  d_max       <- c_max + d                     #no. of rejects
  CHT         <- configs[7]                    #constraint handling technique
  
  #################PRE-SOLVE######################
  #1) generate initial solution
  if (ISP == "Random"){
    x_i <- random_feas_solution(1, problem_parameters)
    print(sprintf("Random solution...f(x*): %.2f", fitness_function(x_i,problem_parameters,CHT)))
  }else{
    x_i <- hybrid_feas_solution(problem_parameters, move, "Rejection")
    print(sprintf("Hybrid solution...f(x*): %.2f", fitness_function(x_i,problem_parameters,CHT)))
  }
  
  #1.1 update current solution
  x <- x_i
  
  #2) set counters, incumbent solution, initial temperature
  T_0 <- find_T0(5000, problem_parameters, move, CHT)
  #(1.Temp, 2.epoch, 3.accepts, 4.rejects, 5.iterations, 
  #6.poor_consec_epochs, 7.gammma, 8. t_max)
  
  #SA parameter vector: (T, i, c, d, t, p, gamma, t_max)
  par_vector <- c(T_0, 1, 0, 0, 1, 0, gamma, max(t_max))
  
  #2.i) store all important information of incumbent solutions
        #1. x*, f(x*)
  search_list <- list(x, fitness_function(x, problem_parameters, CHT))
  
  #initialise temp non-improving iterations
  p_max_temp <- p_max
  
  #initial tracking
  temps <- append(temps, par_vector[1])
  FFVs <- append(FFVs, search_list[[2]])
  
  #################SOLVE WHILE LOOP#################
  #INITIATE WHILE LOOP UNTIL MAXIMUM TERMINATION IS REACHED
  while(par_vector[5] < max(t_max) | 
        par_vector[6] < ifelse(length(p_max_temp)==0, -1, max(p_max))){
    
    #1. evaluate d_max epoch
    par_vector <- d_max_function(d_max, heat, par_vector)
    #1.1 set current no. of reheats:
    reheat_count <- par_vector[6]
    #2. evaluate c_max epoch
    par_vector <- c_max_function(c_max, cool, par_vector)
    #intialise acceptance flag for do while loop
    accept <- 0
    
    #calculate current FFV
    x_FFV <- fitness_function(x,problem_parameters,CHT)
    
    #3. do whiile loop for neighbour generation
    repeat{
      #3.1 generate neighbour (based on CHT)
      x_p <- generate_x_p(x, move, global, CHT, par_vector)
      
      #3.2 calculate energy
      x_p_FFV <- fitness_function(x_p,problem_parameters,CHT)
      Energy <- x_FFV - x_p_FFV                         ############<----MINIMISATION setting
      
      #3.3 evaluate neighbour energy and re-evalute c_max and d_max if required
      if (runif(1) > min(c(1,exp(Energy/par_vector[1])))){
        #3.3.1 reject x_p and increment rejections
        par_vector[4] <- par_vector[4]+1
        #3.3.2 evaluate d_max
        par_vector <- d_max_function(d_max, heat, par_vector)
        #3.3.3 evaluate c_max
        par_vector <- c_max_function(c_max, cool, par_vector)
      }else{
        #3.3.4 update acceptance flag
        accept <- 1
      }
      #break loop if solution is accepted
      if (accept == 1){
        break
      }
    }
    
    #4. accept solution
    x <- x_p
    feas_x <- feasible(x, problem_parameters)
    #5. increment no. of  acceptances
    par_vector[3] <- par_vector[3] + 1
    
    #6. update current FFV
    f_x <- x_p_FFV
    
    #7. update incumbent solution if superior and feasible
    if (f_x < search_list[[2]] & feas_x==TRUE){                 ###########<--- MINIMISATION stetting
      #7.1 update incumbent solution
      search_list[[2]] <- f_x    
    }
    
    #8. increment iterations
    par_vector[5] <- par_vector[5] + 1

    ###########TERMINATION###########
    #9.1 Termination criteria --> STATIC
    #at each reached iteration value record results
    if (par_vector[5] %in% t_max){
      #compile results row 
      SA_recording_static <- termination_record_static(par_vector, configs, start_time, search_list)
      #bind results 
      recorded_results_static <- rbind(recorded_results_static, SA_recording_static)
      print("Static recording point reached.")
    }
    
    #9.2 Termination criteria --> DYNAMIC
    # at each reached non-improving iteration value record results
    if (par_vector[6] %in% p_max_temp){
      #compile results row
      SA_recording_dynamic <- termination_record_dynamic(par_vector, configs, start_time, search_list)
      #bind results
      recorded_results_dynamic <- rbind(recorded_results_dynamic, SA_recording_dynamic)
      #update remaining eligible p_max values
      p_max_temp <- p_max_temp[!p_max_temp == par_vector[6]]
      print("Dynamic recording point reached.")
    }
    if (par_vector[5] %% 1e3 == 0){
      #print(iteration counter with incumbent solution)
      print(sprintf("T: %f...i: %d...t: %d...p: %d...time: %.1fs...f(x*): %d", 
                    par_vector[1], par_vector[2], par_vector[5], par_vector[6], 
                    proc.time()["elapsed"]-start_time["elapsed"],
                    search_list[[2]]))
    }
    
    #break while loop if elapsed time is too long.
    if ((proc.time()["elapsed"]-start_time["elapsed"]) > 120){ #2min limit
      if (length(recorded_results_static) == 0){
        SA_recording_static <- termination_record_static(par_vector, configs, start_time, search_list)
        recorded_results_static <- rbind(recorded_results_static, SA_recording_static)  
      }
      if (length(recorded_results_dynamic) == 0){
        SA_recording_dynamic <- termination_record_dynamic(par_vector, configs, start_time, search_list)
        recorded_results_dynamic <- rbind(recorded_results_dynamic, SA_recording_dynamic)
      }
      break
    }
  }########end of while loop
  #return the compiled results
  return(list(recorded_results_static, recorded_results_dynamic))
}
