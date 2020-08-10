# 切割資料,將資料依照年份區分，產生使用者輸入範圍之資料後丟入模型
cut_year_nmf <- function(data_dtm,from_year,to_year,k){
  get_row <- which((year>=from_year & year<=to_year)) #get row id in this range
  get_dtm <- data_dtm[get_row,] #get dtm in this range
  cut_dtm <- get_dtm[rowSums(get_dtm) > 0,colSums(get_dtm) >0 ] # delete column which all term value is 0
  dtm_16 <- data_dtm[which((year>=from_year & year<=2016)),]
  dtm_16 <- data_dtm[rowSums(dtm_16) > 0,colSums(dtm_16) >0 ]
  #year_nmf <- nmf(cut_dtm, k, algorithm = "mu", initMethod = "EIn-NMF", seed = 5, threshold = 0.00001, maxiter = 2000, runs = 10)
  year_nmf <- nmf(cut_dtm, k,  algorithm="nsNMF", maxiter = 1000, seed = 5, verbose = T, threshold = 0.001,
                  parameters=list(theta = 0.9), runs = 10, initMethod = "EIn-NMF")
  # normalize sparse term-topic matrix H. 
  year_nmf$h_hat = solve(diag(rowSums(year_nmf$H))) %*% year_nmf$H #normalized
  rownames(year_nmf$h_hat) = paste("topic_", 1:nrow(year_nmf$h_hat), sep = "")
  termsNotIn = colnames(dtm_16)[! colnames(dtm_16) %in% colnames(year_nmf$h_hat)]
  m = matrix(0, nrow = dim(year_nmf$h_hat)[1], ncol = length(termsNotIn) ); colnames(m) = termsNotIn
  year_nmf$h_hat = cbind(year_nmf$h_hat, m)
  print("dim of h_hat: "); print(dim(year_nmf$h_hat))
  year_nmf$t_h_hat = t(year_nmf$h_hat)
  year_nmf$t_h_hat = year_nmf$t_h_hat[order(rownames(year_nmf$t_h_hat)), ]
  year_nmf$t_h_hat = ifelse(year_nmf$t_h_hat <= .Machine$double.eps, 0, year_nmf$t_h_hat)
  # Adding topic titles to W matrix
  colnames(year_nmf$W) = paste("topic_", 1:ncol(year_nmf$W), sep = "")
  year_nmf$W = ifelse(year_nmf$W  <= .Machine$double.eps, 0, year_nmf$W )
  year_nmf$p_topic = colSums(year_nmf$W > 0) / sum(year_nmf$W  > 0)
  return(year_nmf)
}


# .Machine$double.eps 是 R 中不同數值的最小差異值，如果兩個不同數值的差異小於這個值，那麼 R 會將兩個數值視為相同的
# kernel_regularizer = regularizer_l1(l = 0.000001) -> for overfitting.
DeepAE <- function(dtm){
  model <- keras_model_sequential()
  model %>%
    # layer_dense(name = "L1", units = 100, activation = "relu",
    #             kernel_constraint = constraint_nonneg(), use_bias = F, input_shape = ncol(dtm)) %>%
    layer_dense(name = "L2", units = 10, activation = "relu", 
                kernel_constraint = constraint_nonneg(), use_bias = F) %>%
    # layer_dense(name = "L3", units = 100, activation = "relu",
    #             kernel_constraint = constraint_nonneg(), use_bias = F) %>%
    layer_dense(name = "L_out", units = ncol(dtm), activation = "relu",  
                kernel_constraint = constraint_nonneg(), use_bias = F)
    ### [Notice!] If finetunen layer need chane the under code that find topic_term.
  
  model %>% compile(
    loss = "mean_squared_error", #categorical_crossentropy
    optimizer = "adam"
    #optimizer= optimizer_adam(lr = 0.5)
    #optimizer_rmsprop(lr = 0.01)
  )
  
  model %>% fit(
    x = dtm, 
    y = dtm, 
    epochs = 3000, 
    batch_size = 1024, 
    view_metrics = TRUE 
    #callbacks = list(checkpoint, early_stopping)
  )
  return(model)
}

cut_year_AE <- function(data_dtm,from_year,to_year){
  get_row <- which((year>=from_year & year<=to_year)) #get row id in this range
  get_dtm <- data_dtm[get_row,] #get dtm in this range
  cut_dtm <- get_dtm[rowSums(get_dtm) > 0,colSums(get_dtm) >0 ] # delete column which all term value is 0
  dtm_16 <- data_dtm[which((year>=from_year & year<=2016)),]
  dtm_16 <- data_dtm[rowSums(dtm_16) > 0,colSums(dtm_16) >0 ]
  termsNotIn = colnames(dtm_16)[! colnames(dtm_16) %in% colnames(cut_dtm)]
  m = matrix(0, nrow = dim(cut_dtm), ncol = length(termsNotIn) ); colnames(m) = termsNotIn
  year_dtm = cbind(cut_dtm, m)
  # Run Autoencoder
  year_model <- DeepAE(year_dtm)
  DAE_norm = norm(year_dtm - predict(year_model, year_dtm), type = "F")
  
  w = get_weights(year_model)
  topic_term = w[[1]] 
  topic_term = t(solve(diag(colSums(topic_term))) %*% t(topic_term))
  rownames(topic_term) = colnames(year_dtm)
  colnames(topic_term) = paste("topic_", 1:ncol(topic_term), sep = "")

  AE_list = list()
  AE_list$model = year_model
  AE_list$data = year_dtm
  AE_list$topic_term = topic_term
  AE_list$w = w
  AE_list$Norm = DAE_norm
  
  return(AE_list)
}

