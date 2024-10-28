#script for FLA landscape walks
  #1. random walk function (local only)
  #2. random walk function (LOCAL+GLOBAL)
  #3. population-based walk with PEM

#1. random walk function
random_walk <- function(sol_num, problem_parameters, walk_length, move){
  #generate initial solution
  init_sol <- random_feas_solution(1, problem_parameters)
  #storage variable
  fitness_FFVs <- fitness_function(init_sol, problem_parameters, "Rejection")
  #update current solution
  x <- init_sol
  if (move == "one_move"){
    CHT_i <- "Rejection"
  }else{
    CHT_i <- "Repair"
  }
  #run through walk length
  for (it in 1:(walk_length-1)){
    # print(it)
    #perturb current solution until feasible solution returned
    repeat{
      x_p <- move_operator(x, move, CHT_i, problem_parameters)
      if (feasible(x_p, problem_parameters)){
        break
      }
    }
    #store FFV
    fitness_FFVs <- append(fitness_FFVs, fitness_function(x_p, problem_parameters, "Rejection"))
    #update new solution
    x <- x_p
  }
  return(fitness_FFVs)
}

#2. random walk function (LOCAL+GLOBAL)
random_walk_gamma <- function(sol_num, problem_parameters, walk_length, move_local, move_global, gam){
  #extract initial solution
  init_sol <- random_feas_solution(1, problem_parameters)
  #storage variable
  fitness_store <- fitness_function(init_sol, problem_parameters, "Rejection")
  #update current solution
  x <- init_sol
  #run through walk length
  for (it in 1:(walk_length-1)){
    print(it)
    #perturb current solution based on reducing probability
    if (runif(1) > 0.5*exp(-10*it/(as.numeric(gam)*walk_length))){
      repeat{
        x_p <- move_operator(x, move_local, "Rejection", problem_parameters)
        if (feasible(x_p, problem_parameters)){
          break
        }
      }
    }else{
      repeat{
        x_p <- move_operator(x, move_global, "Repair", problem_parameters)
        if (feasible(x_p, problem_parameters)){
          break
        }
      }
    }
    #store fitness value
    fitness_store <- append(fitness_store, fitness_function(x_p, problem_parameters, "Rejection"))
    #update current solution
    x <- x_p
  }
  #return FFVs
  return(fitness_store)
}

#3. population-based walk with PEM
population_walk_w_PEM <- function(num, problem_parameters, pop_walk_length){
  #0. settings
  f_record <- numeric()
  f_mean <- numeric()
  evp_store <- numeric()
  # CHT <- "Repair"
  replacement <- 2
  #1. initialise a random population
  P <- 80
  current_pop <- ISP_random(P, problem_parameters)
  
  #2. initialise settings
  t <- 1; p <- 0
  pop_ffvs <- sapply(1:P, function(x){fitness_function(current_pop[[x]], problem_parameters, "Repair")})
  x_inc <- current_pop[[which.min(pop_ffvs)]]                                       ###<------- MINIMISATION
  f_inc <- pop_ffvs[which.min(pop_ffvs)]                                            ###<------- MINIMISATION
  feasibility <- sapply(current_pop, feasible, problem_parameters)
  search_list <- list(t, current_pop, pop_ffvs, x_inc, f_inc, p, feasibility)
  f_record <- append(f_record, search_list[[5]])
  f_mean <- append(f_mean, mean(search_list[[3]]))
  
  #3. conduct population search 
  while(search_list[[1]] < pop_walk_length){
    print(search_list[[1]])
    #3.1 tournament selection k=10, replace=2
    parents <- selection_tournament(search_list, 10, 2, "Repair")
    #3.2 Generate offspring population based on implemented CHT
    offspring_info <- generate_offspring(parents, 2, 1, 0.05, "Repair", problem_parameters)
    #3.3 offspring info
    offspring <- offspring_info[1:2]
    off_ffvs <- offspring_info[[replacement+1]]
    #3.4 record f(x*) before
    f_x_inc_before <- search_list[[5]]
    #3.5 remove parent duplication in current population
    clones <- which(search_list[[3]] %in% off_ffvs)
    #3.6 remove parents if clones < replacement (could be none)
    if (length(clones) < replacement){
      # print("Option 1")
      #remove clones if present 
      if (length(clones) != 0){
        search_list[[2]] <- search_list[[2]][-clones]
        search_list[[3]] <- search_list[[3]][-clones]
      }
      #6. Evaluate offspring for replacement into current population
      search_list <- evaluate_offspring(offspring, off_ffvs, search_list, 2, P)
    }else if(length(clones) >= replacement){
      # print("Option 3")
      # print(length(clones))
      #then delete random "clones" based on replacement size and continue...
      if (length(clones)==1){
        del_clones <- clones
      }else{
        del_clones <- sample(clones, replacement)
      }
      search_list[[2]] <- search_list[[2]][-del_clones]
      search_list[[3]] <- search_list[[3]][-del_clones]
      #6. Evaluate offspring for replacement into current population
      search_list <- evaluate_offspring(offspring, off_ffvs, search_list, 2, P)
    }
    #3.7 record f(x*) of search
    f_record <- append(f_record, search_list[[5]])
    f_mean <- append(f_mean, mean(search_list[[3]]))
    #3.8 increment iterations
    search_list[[1]] <- search_list[[1]] + 1
    
    #4. calculate evp
    evp_store <- append(evp_store, PEM_Pop_walk(off_ffvs, f_x_inc_before, 2, P, search_list))
  }
  #5. POPULATION EVOLVABILITY METRIC 
  #store PEM values
  PEM_values <- numeric()
  #evaluate PEM values until the first time PEM_inc_FFV is "hit"
  PEM_length <- 1:min(which(f_record[pop_walk_length] == f_record))
  #1. average evp value
  evp_avg <- mean(evp_store[PEM_length])
  #2. sd of evp values
  evp_sd <- sd(evp_store[PEM_length])
  #3. ratio of iterations where x* was improved upon
  change_rate <- length(which(evp_store[PEM_length] != 0))/length(PEM_length)
  #combine and return PEM values
  PEM_values <- c(evp_avg, evp_sd, change_rate)
  
  #6. return incumbent recordings
  return(list(f_record, f_mean, PEM_values))
}