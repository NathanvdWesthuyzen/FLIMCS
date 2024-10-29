#RANGER configuations for HPT
#1. GA-features
hyper_grid_GA <- expand.grid(
  mtry = floor(sqrt(ncol(GA_x_input))*c(0.5,1,1.5)),
  min.node.size = c(3,5,7),
  splitrule = c("variance", "extratrees"), 
  tree_size = c(100,200,300)
)
#2. SA-features
hyper_grid_SA <- expand.grid(
  mtry = floor(sqrt(ncol(SA_x_input))*c(0.5,1,1.5)),
  min.node.size = c(3,5,7),
  splitrule = c("variance", "extratrees"), 
  tree_size = c(100,200,300)
)