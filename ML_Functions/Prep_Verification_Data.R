#1.2 separate cluster data with target variables
#1.2.1 GA
g_st_in <- which(colnames(cluster_training_data_GA)=="CHT")
vec1 <- c(2:g_st_in)
vec2 <- c(g_st_in+1, g_st_in+2)
GA_x_input_veri <- cluster_training_data_GA[, ..vec1]
GA_y_input_veri <- cluster_training_data_GA[, ..vec2]
#1.2.2 SA
s_st_in <- which(colnames(cluster_training_data_SA)=="CHT")
vec1 <- c(2:s_st_in)
vec2 <- c(s_st_in+1, s_st_in+2)
SA_x_input_veri <- cluster_training_data_SA[, ..vec1]
SA_y_input_veri <- cluster_training_data_SA[, ..vec2]
print("Verification data prepped.")