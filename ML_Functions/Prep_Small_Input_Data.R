#0. set seed accordingly
set.seed(1234*input_size)
#1. extract training instances from databases
#1.1 sample 30 problem instances from set
train_set <- sample(unlist(unique(GA_database$id)), input_size, replace=FALSE)
g_rows <- which(GA_database$id %in% train_set)
s_rows <- which(SA_database$id %in% train_set)

#1.2 separate training data with f(x) target variable
#1.2.1 GA
g_st_in <- which(colnames(GA_database)=="CHT")
vec1 <- c(2:g_st_in)
vec2 <- c(g_st_in+1, g_st_in+2)
GA_x_input <- GA_database[g_rows, ..vec1]
GA_y_input <- GA_database[g_rows, ..vec2]

#1.2.2 SA
s_st_in <- which(colnames(SA_database)=="CHT")
vec1 <- c(2:s_st_in)
vec2 <- c(s_st_in+1, s_st_in+2)
SA_x_input <- SA_database[s_rows, ..vec1]
SA_y_input <- SA_database[s_rows, ..vec2]
print("Smaller Input data prepped.")
print(dim(GA_x_input))
print(dim(SA_x_input))