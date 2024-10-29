#1. train ml model based on MH and ranger config (HP row)
train_ml_model <- function(x_input, y_input, hyper_grid, HP_row, no_cores, target_variable){
  
  #1.1 train f(x) ranger model
  mtry <- hyper_grid$mtry[HP_row]
  node <- hyper_grid$min.node.size[HP_row]
  rule <- as.character(hyper_grid$splitrule[HP_row])
  tree <- hyper_grid$tree_size[HP_row]
  
  #1.2 train the correct target variable
  if (target_variable == "norm_f"){
    #start timing
    config_st <- proc.time()
    trained_ml_model <- ranger(y = y_input$norm_f, x = x_input, mtry=mtry, 
                               num.trees=tree, min.node.size=node, 
                               splitrule=rule, importance="impurity",
                               num.threads=no_cores, verbose=FALSE)
    #RECORD TRAINING TIME
    train_time <- as.numeric((proc.time() - config_st)["elapsed"])
    print(sprintf("%s ML model built...%.2f secs", target_variable, train_time))
  }else{
    #start timing
    config_st <- proc.time()
    trained_ml_model <- ranger(y = y_input$norm_t, x = x_input, mtry=mtry, 
                               num.trees=tree, min.node.size=node, 
                               splitrule=rule, importance="impurity",
                               num.threads=no_cores, verbose=FALSE)
    #RECORD TRAINING TIME
    train_time <- as.numeric((proc.time() - config_st)["elapsed"])
    print(sprintf("%s ML model built...%.2f secs", target_variable, train_time))
  }
  #1.3 return model
  return(trained_ml_model)
}

#2. test ml model
test_ml_model <- function(cluster_training_data, test_set, trained_ml_model, target_variable){
  #prediction storage lists
  mae_store <- numeric()
  for (test_inst in test_set){
  # print(which(test_inst == test_set))
    #4.1.2.1 extract test data
    st_in <- which(colnames(cluster_training_data)=="CHT")
    vec1 <- c(1:st_in)
    vec2 <- c(st_in+1, st_in+2)
    rows_test <- which(cluster_training_data$id == test_inst)
    x_output <- cluster_training_data[rows_test, ..vec1]
    y_output <- cluster_training_data[rows_test, ..vec2]
    #4.1.2.2 predict 
    pred <- predict(trained_ml_model, x_output)
    preds <- pred$predictions
    #4.1.2.3 store mae
    if (target_variable == "norm_f"){
      mae_store <- append(mae_store, mae(preds, y_output$norm_f))
    }else{
      mae_store <- append(mae_store, mae(preds, y_output$norm_t))
    }
  }
  print(sprintf("Test set evaluated...MAE: %.5f", mean(mae_store)))
  #return MAE results
  return(mae_store)
}

#3. verify ml model on verification instances
verify_ml_model <- function(MH_veri_data, veri_insts_for_cluster, trained_ml_model, target_variable){
  #prediction storage lists
  mae_store <- numeric()
  veri_preds <- list()
  for (veri_inst in veri_insts_for_cluster){
    # print(which(test_inst == test_set))
    #4.1.2.1 extract test data
    st_in <- which(colnames(MH_veri_data)=="CHT")
    vec1 <- c(1:st_in)
    vec2 <- c(st_in+1, st_in+2)
    rows_test <- which(MH_veri_data$id == veri_inst)
    x_output <- MH_veri_data[rows_test, ..vec1]
    y_output <- MH_veri_data[rows_test, ..vec2]
    #4.1.2.2 predict 
    pred <- predict(trained_ml_model, x_output)
    preds <- pred$predictions
    #4.1.2.3 store mae
    if (target_variable == "norm_f"){
      mae_store <- append(mae_store, mae(preds, y_output$norm_f))
    }else{
      mae_store <- append(mae_store, mae(preds, y_output$norm_t))
    }
    #4.1.2.3 store predictions
    veri_preds <- append(veri_preds, list(preds))
    
  }
  print(sprintf("Verification set evaluated...MAE: %.5f", mean(mae_store)))
  #return MAE results
  return(list(mae_store, veri_preds))
}

#4. assess verification true performance
assess_true_verification <- function(performance_GA, performance_SA, GA_veri_data, SA_veri_data, target_variable, veri_inst){
  #0. initialise storage etc
  if (target_variable == "norm_f"){
    MAE_combo_veri <- numeric()
    best_rank_combo_veri <- numeric()
    Top1p_rank_combo_veri <- numeric()
    best_actual_perf_veri <- numeric()
    top1p_actual_perf_veri <- numeric()
  }else{
    MAE_combo_veri <- numeric()
    R2_combo_veri <- numeric()
    RMSE_combo_veri <- numeric()
  }
  
  #1. cycle through each relevant verification instances
  #1.1. extract true values for each test instance
  if (target_variable == "norm_f"){
    true_vals_GA <- GA_veri_data$norm_f[which(GA_veri_data$id == veri_inst)]
    true_vals_SA <- SA_veri_data$norm_f[which(SA_veri_data$id == veri_inst)]
  }else{
    true_vals_GA <- GA_veri_data$norm_t[which(GA_veri_data$id == veri_inst)]
    true_vals_SA <- SA_veri_data$norm_t[which(SA_veri_data$id == veri_inst)]
  }
  
  #1.2 combine pred values and true values
  true_vals_combo <- c(true_vals_GA, true_vals_SA)
  pred_vals_combo <- c(unlist(performance_GA[[2]]),
                       unlist(performance_SA[[2]]))
  
  #1.2.1 generate useful plots of predictions vs true
  df <- data.frame(
    True = true_vals_combo,
    Prediction = pred_vals_combo,
    Algorithm = c(rep("GA", times=1944), rep("SA", times=2430))
  )
  gg <- ggplot(df)+geom_point(aes(x=True, y=Prediction, colour=Algorithm))+theme_minimal()+
    ylim(c(0,1)) + xlim(c(0,1)) + geom_abline(slope=1, intercept=0, linewidth=1)
  
  #1.3 check target variable
  if (target_variable == "norm_t"){
    #1.3.1 evaluate and append the 3 accuracy measures
    MAE_combo_veri <- append(MAE_combo_veri, mae(pred_vals_combo, true_vals_combo))
    R2_combo_veri <- append(R2_combo_veri, R2(pred_vals_combo, true_vals_combo))
    RMSE_combo_veri <- append(RMSE_combo_veri, RMSE(pred_vals_combo, true_vals_combo))
  }else{
    #1.3.2 find ranking information
    pred_ranks <- min_rank(pred_vals_combo)
    true_ranks <- min_rank(true_vals_combo)
    
    #1.3.3 calculate the and append performance metrics 
    MAE_combo_veri <- append(MAE_combo_veri, mae(pred_vals_combo, true_vals_combo))
    best_rank_combo_veri <- append(best_rank_combo_veri, true_ranks[which.min(pred_ranks)]/4374)
    Top1p_rank_combo_veri <- append(Top1p_rank_combo_veri, mean(true_ranks[which(pred_ranks <= ceiling(length(true_vals_combo)*0.01))])/4374)
    best_actual_perf_veri <- append(best_actual_perf_veri, true_vals_combo[which.min(pred_ranks)])
    top1p_actual_perf_veri <- append(top1p_actual_perf_veri, mean(true_vals_combo[which(pred_ranks <= ceiling(length(true_vals_combo)*0.01))]))
  }
  
  #2. return the appropriate set of results
  if (target_variable == "norm_f"){
    return(list(MAE_combo_veri, best_rank_combo_veri, Top1p_rank_combo_veri,
                best_actual_perf_veri, top1p_actual_perf_veri, gg))
  }else{
    return(list(MAE_combo_veri, R2_combo_veri, RMSE_combo_veri, gg))
  }
}
