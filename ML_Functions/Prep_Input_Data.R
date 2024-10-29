#0. set seed accordingly
set.seed(1234*fold)
#0.1 extract training instances from databases
training_split <- round(0.8*nrow(cluster_training_data_GA)/1944)
test_split <- nrow(cluster_training_data_GA)/1944 - training_split

#1.1 sample 80/20% training/test split
test_set <- sample(unlist(unique(cluster_training_data_GA$id)), test_split, replace=FALSE)
g_rows <- which(cluster_training_data_GA$id %in% test_set)
s_rows <- which(cluster_training_data_SA$id %in% test_set)

#1.2 separate training data with f(x) target variable
#1.2.1 GA
g_st_in <- which(colnames(cluster_training_data_GA)=="CHT")
vec1 <- c(2:g_st_in)
vec2 <- c(g_st_in+1, g_st_in+2)
GA_x_input <- cluster_training_data_GA[-g_rows, ..vec1]
GA_y_input <- cluster_training_data_GA[-g_rows, ..vec2]
#1.2.2 SA
s_st_in <- which(colnames(cluster_training_data_SA)=="CHT")
vec1 <- c(2:s_st_in)
vec2 <- c(s_st_in+1, s_st_in+2)
SA_x_input <- cluster_training_data_SA[-s_rows, ..vec1]
SA_y_input <- cluster_training_data_SA[-s_rows, ..vec2]
print("Input data prepped.")