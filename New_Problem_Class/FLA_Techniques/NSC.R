#script for the nEGATIVE sLOPE cOEFFICIENT given a list of 1000 initial solutions
  #1. NSC_FFVS
  #2. NSC


  #extra functions:
  #i tournament selection given a set of solutions
  select_tourney <- function(all_parents, select_param, replacement, CHT){
    #tournament size:
    k <- select_param
    #define no. of parents
    if (replacement == 1){
      no_of_parents <- 2
    }else{
      no_of_parents <- replacement
    }
    
    #eligible population for parents
    elig_pop <- 1:length(all_parents)
    #initialise selected parents
    selected_parents <- list()
    #run through no. of parents to select --> based on the no. of offspring to generate
    for (i in 1:no_of_parents){
      #sample k number of solutions for the tournament
      tourney_indxs <- sample(elig_pop, k)
      
      #extract best individual from contestants
      tourney_ffvs <- sapply(tourney_indxs, function(x){fitness_function(all_parents[[x]], problem_parameters, "Rejection")})
      winner_indx <- tourney_indxs[which.min(tourney_ffvs)] #<--- MINIMISATION
      winner <- all_parents[[winner_indx]]
      
      #update eligible population --> delete winner
      elig_pop <- elig_pop[!elig_pop == winner_indx]
      
      #append winner to the number of parents to select
      selected_parents <- append(selected_parents, list(winner))
    }
    #return list of selected parents
    return(selected_parents)
  }
  
  #ii size-driven bisection, given a one-split limit 
  size_driven_bisection <- function(parent_values, limits, max_points, min_bin_size) {
    repeat{
      #calculate required information
        #size per segment
      segment_sizes <- diff(limits)
        #capacity per segment
      segment_capacity <- as.vector(table(cut(parent_values, breaks = limits, include.lowest = TRUE, labels = FALSE)))
      
      #test termination
      if (any(segment_sizes < min_bin_size*diff(range(limits))) | any(segment_capacity < max_points)){
        break
      }else{
        #else, split the biggest segment
        #i. identify biggest segment
        next_seg <- which.max(segment_sizes)
        #ii. extract values within that segment
        seg_vals <- parent_values[parent_values >= limits[next_seg] & parent_values <= limits[next_seg+1]]
        #iii. partition segment based on size and add to limit
        if (median(seg_vals) %in% limits){
          break
        }else{
          limits <- sort(append(limits, median(seg_vals)))
        }
      }
    }
    #then return the segmented limits
    return(limits)
  }
  

#1. NSC
NSC_FFVs <- function(num, initial_sols, hybrid_sols, problem_parameters){
  #0. set random and different seed
  p <- 0.8
  set.seed(runif(1,num,10e3)*123456/465)
  # Create a combined list with the appropriate proportions
  combined_list <- c(rep("random", length(initial_sols) * p),
                     rep("hybrid", length(initial_sols) * (1 - p)))
  # Sample from the combined list
  sampled_items <- sample(combined_list, size = 50)
  
  set.seed(runif(1,num,10e3)*123456/465)
  #1. select a sample population (size of 50) from initial and hybrid sols
  rand_parents <- initial_sols[sample(1:length(initial_sols), size=length(which(sampled_items == "random")))]
  set.seed(runif(1,num,10e3)*123456/465)
  hyb_parents <- hybrid_sols[sample(1:length(hybrid_sols), size=length(which(sampled_items == "hybrid")))]
  
  #2. tournament selection with k=10
  parents <- select_tourney(c(rand_parents, hyb_parents), 10, 2, "Repair")

  #3. generate offspring
  offspring_info <- generate_offspring(parents, 2, 1, 0, "Repair", problem_parameters)
  
  #4. extract all FFVs
  off_FFVS <- offspring_info[[3]]
  par_FFVS <- sapply(1:length(parents), function(x){fitness_function(parents[[x]], problem_parameters, "Rejection")})

  #5. return FFVs
  return(c(par_FFVS, off_FFVS))
}


#2. NSC
NSC <- function(final_split_limits, parent_values, NSC_ffv_values){
  #10.3.1 calculate the average parent values per segment (abscissa)
  abscissa_avgs <- sapply(1:(length(final_split_limits)-1), 
                          function(x){mean(parent_values[parent_values >= final_split_limits[x] & 
                                                           parent_values <= final_split_limits[x+1]])})
  
  #10.3.2 calculate the average offspring per segment (ordinate)
  #i. get list indices to pull offspring out of per segment
  par_1s <- unlist(lapply(NSC_ffv_values, function(x) x[1]))
  par_2s <- unlist(lapply(NSC_ffv_values, function(x) x[2]))
  #ii. use indices to extract corresponding offspring for segment
  ordinate_avgs <- sapply(1:(length(final_split_limits)-1),
                          function(x){
                            #parent indices for segment
                            ind_1 <- which(par_1s >= final_split_limits[x] & par_1s <= final_split_limits[x+1])
                            ind_2 <- which(par_2s >= final_split_limits[x] & par_2s <= final_split_limits[x+1])
                            #pull out corresponding offspring values
                            off_1 <- unlist(lapply(ind_1, function(x) NSC_ffv_values[[x]][3:4]))
                            off_2 <- unlist(lapply(ind_2, function(x) NSC_ffv_values[[x]][3:4]))
                            #obtain mean for segment ordinate
                            return(mean(c(off_1, off_2)))
                          })
  
  #10.3.3 calculate slopes and nsc
  P_vals <- sapply(1:(length(abscissa_avgs)-1),
                   function(x){
                     p <- (ordinate_avgs[x+1]-ordinate_avgs[x])/(abscissa_avgs[x+1]-abscissa_avgs[x])   
                   })
  nsc <- sum(P_vals[which(P_vals <= 0)])
  nsc_mean <- mean(P_vals)
  
  #return values
  return(c(nsc, nsc_mean))
}




