source("R/GPU_envir_vars.R")
source("R/find_topic.R")
source("R/hungarianAssignNMF.R")
source("R/topic_dist.R")
source("R/load_dtm.R")
library(tm)

# Data preparation --------------------------------------------------------
# Load metadata for years
load("/data_lake/arXiv_ML/ML_meta_2019.RData")

all_df <- all_df[grep(pattern = "(astro|cond-|gr|hep|math-ph|nlin|nucl|physics|quant|q-bio)",
                      x = all_df$categories,
                      invert = T),]

# Load dtm
dtm <- load_dtm(file = "/data_lake/arXiv_ML/dtm_2019.RData", tfIdf = T)
dtm <- dtm[rownames(dtm) %in% all_df$id,]
dtm <- dtm[rowSums(dtm) > 1, colSums(dtm) > 1]

# Cut text data into slices as per year.
paperIDYear <- all_df[, 1:2]
paperIDYear$submitYear <- as.integer(substr(paperIDYear$submitted, 1, 4))
paperIDYear$submitted <- NULL


# Parameters --------------------------------------------------------------
batch_size <- 128L
original_dim <- ncol(dtm)
latent_dim <- 10L
intermediate_dim <- 30L
first_dim <- 500L
epochs <- 5000L
epsilon_std <- 1.0

# Model definition  --------------------------------------------------------

model =  keras_model_sequential() %>%
  layer_dense(name = "L1", units = intermediate_dim, activation = "linear", input_shape = original_dim, 
              kernel_constraint = constraint_nonneg(),use_bias = FALSE) %>% 
  layer_dense(name = "code_layer", units = latent_dim, activation = "linear", 
              kernel_constraint = constraint_nonneg(),use_bias = FALSE) %>%   
  layer_dense(name = "L5", units = intermediate_dim, activation = "linear", 
              kernel_constraint = constraint_nonneg(),use_bias = FALSE) %>% 
  layer_dense(name = "L_out", units = original_dim, activation = "linear", 
              kernel_constraint = constraint_nonneg(),use_bias = FALSE)


# RMSE
rmse <- function(y_true, y_pred) {
  return(k_sqrt(k_mean(k_square(y_true - y_pred))))
}

#model %>% compile( optimizer = "rmsprop", loss = "mse")
model %>% compile( optimizer = optimizer_adam(lr = 0.001), loss = rmse)


# Single Model training ----------------------------------------------------------
model %>% fit( dtm, dtm, epochs = 1000, batch_size = 64, view_metrics = F,
               callbacks = list(callback_early_stopping(monitor = "loss", patience = 10)) )
# Get weight from Keras model
w <- get_weights(model)
View(w)

# Build topic-term matrix
topic_term <- w[[1]] %*% w[[3]] %*% w[[5]]
# Normalize for each colSum = 1
#topic_term <- t(solve(diag(colSums(topic_term))) %*% t(topic_term))
rownames(topic_term) <- colnames(dtm)
colnames(topic_term) <- paste0(2020, "_topic_", 1:ncol(topic_term))
topic_term <- ifelse(topic_term <= .Machine$double.eps, 0, topic_term)

top10_terms <- find_topic(topic_term, 10)$term
#top10_terms_1000 <- top10_terms 
View(top10_terms)

# Online Topic Evolution ----------------------------------------------------------
topic_evo <- list()
for(y in c(1:5)) {
  print(paste0(2014 + y))
  # Slice dtm by year    
  # 2015 ~ 2019 for setting weight
  train_dtm <- dtm[rownames(dtm) %in% paperIDYear[paperIDYear$submitYear %in% 2007:(2014 + y), "id"], ]
  # For online learning, we don't remove terms with frequency = 0
  train_dtm <- train_dtm[rowSums(train_dtm) > 0,]
  # Do tf-idf by slices
  train_dtm <- tf_idf(dtm = train_dtm)
  train_dtm <- train_dtm / rowSums(train_dtm)
  
  # Run autoencoder with early stopping and patience epoch = 100
  model %>% fit(
    x = train_dtm, 
    y = train_dtm, 
    shuffle = TRUE, 
    epochs = epochs, 
    batch_size = batch_size,
    view_metrics = F,
    callbacks = list(callback_early_stopping(monitor = "loss", patience = 30))
  )
  
  # Save model for each year
  #save_model_hdf5(object = model, filepath = paste0("shaomin/online_models/model_model", y))
  
  # Compute the Frobenius norm for input dtm
  original_norm <- norm(train_dtm, type = "F")
  
  # Recovery Frobenius norm
  pred_norm <- norm(train_dtm - predict(model, train_dtm), type = "F")
  
  # RMSE
  RMSE <- sqrt(mean((train_dtm - predict(model, train_dtm)) ^ 2))
  
  # Get weight from Keras model
  w <- get_weights(model)
  
  # Build topic-term matrix
  topic_term <- w[[1]] %*% w[[2]]
  # Normalize for each colSum = 1
  # topic_term <- t(solve(diag(colSums(topic_term))) %*% t(topic_term))
  rownames(topic_term) <- colnames(train_dtm)
  colnames(topic_term) <- paste0(2014 + y, "_topic_", 1:ncol(topic_term))
  topic_term <- ifelse(topic_term <= .Machine$double.eps, 0, topic_term)
  #topic_term <- round(topic_term,digits = 5)
  
  # Subtopic weight of each topic
  subtopic_weight <- round(w[[2]],digits = 5) 
  colnames(subtopic_weight) <- paste0(2014 + y, "_topic_", 1:ncol(subtopic_weight))
  rownames(subtopic_weight) <- paste0("subtopic_", 1:nrow(subtopic_weight))
  
  
  # Build subtopic-topic matrix
  subtopic_term <- w[[1]]
  #subtopic_term <- t(solve(diag(colSums(subtopic_term))) %*% t(subtopic_term))
  # Normalize for each colSum = 1
  rownames(subtopic_term) <- colnames(train_dtm)
  colnames(subtopic_term) <- paste0(2014 + y, "_sub_topic_", 1:ncol(subtopic_term))
  subtopic_term <- ifelse(subtopic_term <= .Machine$double.eps, 0, subtopic_term)
  #subtopic_term <- round(subtopic_term,digits = 8)
  
  topic_evo[[y]] <- list("topic_term" = topic_term,
                         "subtopic_term" = subtopic_term,
                         "subtopic_weight" = subtopic_weight,
                         "original_norm" = original_norm,
                         "pred_norm" = pred_norm,
                         "w"=w,
                         "RMSE" = RMSE)
}

top10_terms_year <- lapply(seq_along(topic_evo), function(i) {
  top10_terms <- find_topic(topic_evo[[i]]$topic_term, 10)$term
})

top10_terms_year <- lapply(seq(10), function(x) {
  each_topic <- sapply(top10_terms_year, function(y) {
    paste0(y[, x], collapse = ",\n")
  })
  return(each_topic)
})

top10_terms <- lapply(topic_evo, function(x) {
  top10_terms <- find_topic(x$topic_term, 10)$term
  top10_terms <- apply(top10_terms, 2, function(x) paste0(x, collapse = ",\n"))
})


top10_terms <- do.call(what = cbind, args = top10_terms)
colnames(top10_terms) <- 2015:2019
View(top10_terms)

# save(topic_evo,file = "experiment/topic_evo6.RData"
write.csv(top10_terms, file = "experiment/NAE/NAE_part1.csv")


#save(topic_evo, file = "experiment/NAE/NAE_topic_evo1.RData")
