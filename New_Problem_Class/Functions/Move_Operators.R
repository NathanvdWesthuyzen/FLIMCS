#the script for move operators

#<This is an example of the move operators employed for the 1-dimensional bin packing problem class>

  #i. REPAIR FUNCTION
  #1. LOCAL: one move

  #2. GLOBAL: ejection chain

  #3. CROSSOVER: 1-point crossover

#extra functions
#i. Repair function
BPP_repair <- function(x_p, problem_parameters){
  #check solution feasibility
  if(feasible(x_p, problem_parameters)){ #if feasible return current solution
    return(x_p)
  }else{ #if infeasible then repair
    #1. fix that each item is only placed in a single bin
    if (max(rowSums(x_p)) > 1){
      #find rows with too many assignments
      more_than_1 <- which(rowSums(x_p) > 1)
      #delete rows with more than 1
      while (length(more_than_1) > 0){
        #reset one of them
        x_p[more_than_1[1], sample(which(x_p[more_than_1[1],] == 1), 1)] <- 0
        #update row info
        more_than_1 <- which(rowSums(x_p) > 1)
      }

      #fix under assigning constraint (1x per row)
      zeros <- which(rowSums(x_p) == 0)
      #delete rows with more than 1
      while (length(zeros) > 0){
        #assign a one in the row
        x_p[zeros[1], round(runif(1, min=1, max=ncol(x_p)))] <- 1
        #update row info
        zeros <- which(rowSums(x_p) == 0)
      }
    }
    #break if feasible
    if (feasible(x_p, problem_parameters)){
      return(x_p)
    }
    
    #2. fix over-capacity bins
    c <- 1
    repeat{
      fix <- which(apply(x_p, 2, function(z){sum(z*problem_parameters[[2]])}) > problem_parameters[[1]])
      for (j in fix){
        mover <- which(apply(x_p, 2, function(z){sum(z*problem_parameters[[2]])}) == 0)
        i <- sample(which(x_p[,j] == 1), 1)
        x_p[i, j] <- 0
        x_p[i, sample(mover, 1)] <- 1
      }
      
      #if feasible, then return solution
      if (feasible(x_p, problem_parameters)){
        #return repaired solution
        return(x_p)
      }else{
        c <- c + 1
      }
      #if unfixable, then return new solution
      if (c > 5){
        return(random_feas_solution(1, problem_parameters))
      }
    }
  }
}

##############LOCAL MOVE OPERATORS################
#1. 1-move MO
one_move <- function(x, CHT, problem_parameters){
  x_p <- x
  #select random item
  item <- sample(1:nrow(x_p), 1, replace=FALSE)
  #move item to different bin
  bin_1 <- which(x_p[item, ] == 1)
  #remove item
  x_p[item, ] <- 0
  
  if (length(bin_1) == 0){
    x_p[item, sample(1:ncol(x_p), 1)] <- 1
  }else{
    x_p[item, sample((1:ncol(x_p))[-bin_1], 1)] <- 1
  }
  
  #evaluate CHT
  if (CHT == "Repair"){
    #repair and return
    return(BPP_repair(x_p, problem_parameters))
  }else{
    #return current solution if rejection and penalty
    return(x_p)
  }
}

##############GLOBAL MOVE OPERATORS################
#2. ejection_chain operator
ejection_chain <- function(x, CHT, problem_parameters){
  #select a random length for ejection chain
  eject_len <- round(runif(1, 10, 50))
  #initialise solution size 
  n <- nrow(x)
  #initialise perturbed solution
  x_p <- x
  #1. randomly select a column to start with
  current_col <- sample(which(colSums(x) != 0), 1)
  #run through ejection length steps
  for (i in 1:eject_len){
    # print(current_col)
    #a) select row index with a 1 in current column
    current_row <- ifelse(length(which(x_p[,current_col]==1)) == 1, 
                          which(x_p[,current_col]==1), 
                          sample(which(x_p[,current_col]==1), 1))
    #b) randomly select row index in current column to move 1 to
    row_indx_swap <- sample((1:n)[-which(x_p[,current_col]==1)], 1)
    #c) swap 1 <--> 0 within current column and selected indx
    x_p[current_row, current_col] <- 0
    x_p[row_indx_swap, current_col] <- 1
    # if feasible then break early, otherwise continue till end
    if (i > 0.5*eject_len & feasible(x_p, problem_parameters)){
      break; print("EARLY...")
    }else{
      #c) select new column in the swapped row indx (that is not current column) and update
      avail_cols <- which(x_p[row_indx_swap,]==1)[which(which(x_p[row_indx_swap,]==1)!=current_col)]
      # print(avail_cols)
      if (length(avail_cols) == 1){
        current_col <- avail_cols
      }else if (length(avail_cols) == 0){
        break
      }else{
        current_col <- sample(avail_cols, 1)
      }
    }
  }
  
  #evaluate CHT
  if (CHT == "Repair"){
    #repair and return
    x_p <- (BPP_repair(x_p, problem_parameters))
  }else{
    #return current solution if rejection and penalty
    return(x_p)
  }
}

##############CROSSOVER OPERATOR#################
#3. 1-point crossover operator
cross_1point <- function(x_1, x_2, CHT, problem_parameters){
  #get dims
  row <- nrow(x_1)
  col <- ncol(x_1)
  #select horizontal swap or vertical crossover
  hor <- as.logical(round(runif(1)))
  #exchange portions based on orientation and crossing point
  if (hor){
    #select row cross point
    cross <- round(runif(1, 1, row-1))
    off1 <- rbind(x_1[(1:cross),], x_2[((cross+1):row),])
    off2 <- rbind(x_2[(1:cross),], x_1[((cross+1):row),])
    #apply CHT
    if (CHT == "Repair"){
      #repair offspring
      offspring_1 <- BPP_repair(off1, problem_parameters)
      offspring_2 <- BPP_repair(off2, problem_parameters)
      #return both offs
      return(list(offspring_1, offspring_2))
    }else{
      return(list(off1,off2))
    }
  }else{
    #select column cross point
    cross <- round(runif(1, 1, col-1))
    off1 <- cbind(x_1[,(1:cross)], x_2[,((cross+1):col)])
    off2 <- cbind(x_2[,(1:cross)], x_1[,((cross+1):col)])
    #apply CHT
    if (CHT == "Repair"){
      #repair offspring
      offspring_1 <- BPP_repair(off1, problem_parameters)
      offspring_2 <- BPP_repair(off2, problem_parameters)
      #return both offs
      return(list(offspring_1, offspring_2))
    }else{
      return(list(off1,off2))
    }
  }
}

##############COMBINED MOVE OPERATOR#############
move_operator <- function(x, move, CHT, problem_parameters){
  #conduct move operator based on selected move and CHT
  if (move == "one_move"){
    return(one_move(x, CHT, problem_parameters))
  }else{
    return(ejection_chain(x, CHT, problem_parameters))
  }
}
