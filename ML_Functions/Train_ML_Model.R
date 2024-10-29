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

#2. verify ml model on verification instances
verify_ml_model <- function(MH_veri_data, veri_insts_for_cluster, trained_ml_model, target_variable){
  #prediction storage lists
  mae_store <- numeric()
  for (veri_inst in veri_insts_for_cluster){
    # print(which(test_inst == test_set))
    #4.1.2.1 extract test data
    st_in <- which(colnames(cluster_training_data)=="CHT")
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
    
  }
  print(sprintf("Verification set evaluated...MAE: %.5f", mean(mae_store)))
  #return MAE results
  return(mae_store)
}

#3. employ ML model on unseen instance
employ_ml_model_unseen <- function(unseen_input_data, trained_ml_model){
  pred <- predict(trained_ml_model, unseen_input_data)
  preds <- pred$predictions
  print("Unseen instance evaluated...")
  #return MAE results
  return(preds)
}

