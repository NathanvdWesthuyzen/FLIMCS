#script for the Hamming Distance in a Level given a list of 1000 initial solutions
  #extra functions:
    #HDIL iso-cost level identifier function
    organise_levels <- function(num, sol_list, lim_1){
      #return 2 column vector stating which iso-cost level
      sol <- sol_list[[num]]
      f <- f_lvls[num]
      return(max(which(f>=lim_1)))
    }

#1. HDIL
HDIL_for_combo <- function(combo, HDIL_combos, initial_sols){
  #return HD for combination
  return(length(which(initial_sols[[HDIL_combos[1, combo]]]!=initial_sols[[HDIL_combos[2, combo]]])))
}