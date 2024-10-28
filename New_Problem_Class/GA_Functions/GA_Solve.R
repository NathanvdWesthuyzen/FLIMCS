#script for a single GA configuration given an input configuration for MINIMISATION
GA_solve <- function(config_num, all_configs, problem_parameters, t_max, t_non){
  start_time <- proc.time()
  #empty results
  recorded_results_static <- numeric()
  recorded_results_dynamic <- numeric()
  #################CONFIGURATION######################
  if (config_num <= nrow(all_configs[[1]])){
    selection_procedure <- "Tournament"
    num <- config_num
    configs <- all_configs[[1]][num,]
  }else{
    selection_procedure <- "Rank-based"
    num <- config_num - nrow(all_configs[[1]])
    configs <- all_configs[[2]][num,]
  }
  print(configs)
  
  #extract remaining config parameters??????
  ISP         <- configs[1]                    #initial solution procedure
  select_param<- as.numeric(configs[2])        #selection parameter (k or s)
  cross_prob  <- as.numeric(configs[3])        #crossover probability
  mutate_prob <- as.numeric(configs[4])        #mutation probability
  replacement <- as.numeric(configs[5])        #no. of poor individuals to replace
  CHT         <- configs[6]                    #constraint handling technique
  P           <- as.numeric(configs[7])        #population size
  
  #################PRE-SOLVE######################
  #1) generate initial population
  st <- proc.time()
  if (ISP == "Random"){
    current_pop <- ISP_random(P, problem_parameters)
  }else{
    current_pop <- ISP_hybrid(P, problem_parameters, mutation, CHT)
    print("Hybrid population generated...")
  }

  #2) set counters, population f(x)'s, incumbent solution
  t <- 1
  p <- 0
  pop_ffvs <- sapply(1:P, function(x){fitness_function(current_pop[[x]], problem_parameters, CHT)})
  x_inc <- current_pop[[which.min(pop_ffvs)]]
  f_inc <- pop_ffvs[which.min(pop_ffvs)]
  feasibility <- sapply(current_pop, feasible, problem_parameters)
  
  #2.i) store all important search elements in a list for function passing
  search_list <- list(t, current_pop, pop_ffvs, x_inc, f_inc, p, feasibility)
  
  #initialise temp non-improving iterations
  t_non_temp <- t_non 
  
  #################SOLVE WHILE LOOP#################
  #INITIATE WHILE LOOP UNTIL MAXIMUM TERMINATION IS REACHED
  while(search_list[[1]] < t_max[length(t_max)] | 
        search_list[[6]] < ifelse(length(t_non_temp)==0, -1, t_non[length(t_non)])){
    #3) selection procedure
    if (selection_procedure == "Tournament"){
      parents <- selection_tournament(search_list, select_param, replacement, CHT)
    }else{
      parents <- selection_ranked(search_list, select_param, replacement, P)
    }
    #3,4,5. Generate offspring population based on implemented CHT
    offspring_info <- generate_offspring(parents, replacement, cross_prob, 
                                         mutate_prob, CHT, problem_parameters)
    
    #extract offspring info
    if (replacement == 1){
      offspring <- offspring_info[1:2]
      off_ffvs <- offspring_info[[3]]
    }else{
      offspring <- offspring_info[1:(2*ceiling(replacement/2))]
      off_ffvs <- offspring_info[[replacement+1]]
    }
    
    # record f(x*) before
    f_x_inc_before <- search_list[[5]]
    
    #if offspring identical to parents then don't duplicate parents in population
      #remove parent duplication in current population
    clones <- which(search_list[[3]] %in% off_ffvs)
    
    #remove parents if clones < replacement (could be none)
    if (length(clones) < replacement){
      #remove clones if present 
      if (length(clones) != 0){
        search_list[[2]] <- search_list[[2]][-clones]
        search_list[[3]] <- search_list[[3]][-clones]
      }
      #6. Evaluate offspring for replacement into current population
      search_list <- evaluate_offspring(offspring, off_ffvs, search_list, replacement, P)
    }else if (replacement == 1 & length(clones)==1){
      # print("Option 2")
      #cut down offspring list to non-clones
      off_clones <- which(off_ffvs %in% search_list[[3]])
      
      if (length(off_clones)==2){
        del_off <- sample(off_clones, 1)
      }else{
        del_off <- off_clones
      }
      offspring_1 <- offspring[-del_off]
      off_ffv_1 <- off_ffvs[-del_off]
      
      #6. Evaluate single offspring for replacement into current population
      search_list <- evaluate_offspring(offspring_1, off_ffv_1, search_list, replacement, P)
    }else if(length(clones) >= replacement){
      #then delete random "clones" based on replacement size and continue...
      if (length(clones)==1){
        del_clones <- clones
      }else{
        del_clones <- sample(clones, replacement)
      }
      search_list[[2]] <- search_list[[2]][-del_clones]
      search_list[[3]] <- search_list[[3]][-del_clones]
      #6. Evaluate offspring for replacement into current population
      search_list <- evaluate_offspring(offspring, off_ffvs, search_list, replacement, P)
    }
    #else, leave population as is, since all offspring are clones

    #7. update iteration counters
    search_list[[1]] <- search_list[[1]] + 1
    if (search_list[[5]] < f_x_inc_before){ # if improved f(x*), then reset p ################<-- minimisation
      search_list[[6]] <- 0
    }else{ #else add to non-improving counter
      search_list[[6]] <- search_list[[6]] + 1
    }
    
    #8 Termination criteria --> STATIC
    #at each reached iteration value record results
    if (search_list[[1]] %in% t_max){
      #compile results row 
      GA_recording_static <- termination_record_static(search_list, configs, start_time, selection_procedure)
      #bind results 
      recorded_results_static <- rbind(recorded_results_static, GA_recording_static)
      print("Static recording point reached.")
    }
    
    #9 Termination criteria --> DYNAMIC
    # at each reached non-improving iteration value record results
    if (search_list[[6]] %in% t_non_temp){
      #compile results row
      GA_recording_dynamic <- termination_record_dynamic(search_list, configs, start_time, selection_procedure)
      #bind results
      recorded_results_dynamic <- rbind(recorded_results_dynamic, GA_recording_dynamic)
      #update remaining eligible t_non values
      t_non_temp <- t_non_temp[!t_non_temp == search_list[[6]]]
      print("Dynamic recording point reached.")
    }
    if (search_list[[1]] %% 100 == 0){
      #print(iteration counter with incumbent solution)
      print(sprintf("t: %d...p: %d...time: %.1fs...f(x*): %d...mean(pop): %.2f", 
                    search_list[[1]], search_list[[6]], proc.time()["elapsed"]-start_time["elapsed"],
                    search_list[[5]], mean(search_list[[3]])))
    }
    
    #break while loop if elapsed time is too long.
    if ((proc.time()["elapsed"]-start_time["elapsed"]) > 120){ #2min limit
      if (length(recorded_results_static) == 0){
        GA_recording_static <- termination_record_static(search_list, configs, start_time, selection_procedure)
        recorded_results_static <- rbind(recorded_results_static, GA_recording_static)
      }
      if (length(recorded_results_dynamic) == 0){
        GA_recording_dynamic <- termination_record_dynamic(search_list, configs, start_time, selection_procedure)
        recorded_results_dynamic <- rbind(recorded_results_dynamic, GA_recording_dynamic)
      }
      break
    }
  }########end of while loop
  
  #return the compiled results
  return(list(recorded_results_static, recorded_results_dynamic))
}
