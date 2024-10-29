#1. remove useless data entries from databases
remove_useless_f <- function(database){
  lens <- numeric()
  na_mes <- colnames(database)
  for (i in 1:length(na_mes)){
    len <- length(unique(database %>% pull(i)))
    #record the lengths
    lens <- append(lens, len)
  }
  
  #convert to data frame
  database <- as.data.frame(database)
  
  # remove any useless columns (and t & p)
  database[, which(lens == 1)] <- NULL
  database$H_star <- NULL
  database$h_star <- NULL
  database$t <- NULL
  database$p <- NULL
  
  database <- as.data.table(database)
  return(database)
}
