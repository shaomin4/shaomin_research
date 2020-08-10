source("R/GPU_envir_vars.R")
source("R/find_topic.R")
source("R/tf_idf.R")
library(tm)

# Data preparation --------------------------------------------------------
load("/data_lake/arXiv_ML/ML_meta_2019.RData")
load("/data_lake/arXiv_ML/dtm_2019.RData")

all_df <- all_df[grep(pattern = "(astro|cond-|gr|hep|math-ph|nlin|nucl|physics|quant|q-bio)",
                      x = all_df$categories,
                      invert = T),]

# Load dtm [34212,18970]
dtm <- as.matrix(dtm, sparse = T)
dtm <- dtm[rowSums(dtm) > 0, colSums(dtm) > 0] # => [34210,18814]
nm = c("twitter","coding","news","individual fairness","fairness","fair","group","counterfactual","attribute","privacy","utility","sentiment","fid","images","language","aic","a2c")
dtm <- dtm[,!colnames(dtm) %in% nm] # [34210,18797]
dtm <- dtm[rownames(dtm) %in% all_df$id,] # [31904,18797]
dtm <- tf_idf(dtm) # [31904,18797]
dtm <- dtm[rowSums(dtm) > 1, colSums(dtm) > 1] #[31663,5607]




# Cut text data into slices as per year.
paperIDYear <- all_df[, 1:2]
paperIDYear$submitYear <- as.integer(substr(paperIDYear$submitted, 1, 4))
paperIDYear$submitted <- NULL
# Get the frequency table of paper years
table(paperIDYear$submitYear) 
# Get the frequency table of paper years
table(paperIDYear$submitYear[paperIDYear$id %in% rownames(dtm)])

# Parameters --------------------------------------------------------------
#batch_size <-256L
original_dim <- ncol(dtm)
latent_dim <- 10L
intermediate_dim <- 30L
epochs <- 20000L


# Model definition  --------------------------------------------------------
# RMSE
rmse <- function(y_true, y_pred) {
  return(k_sqrt(k_mean(k_square(y_true - y_pred))))
}

# Initial autoencoder
model <- keras_model_sequential() %>%
  layer_dense(name = "L1",
              units = 40,
              kernel_constraint = constraint_nonneg(),
              input_shape = ncol(dtm),
              use_bias = F) %>%
  layer_dense(name = "L2",
              units = 10,
              kernel_constraint = constraint_nonneg(),
              use_bias = F) %>%
  layer_dense(name = "L3",
              units = 40,
              kernel_constraint = constraint_nonneg(),
              use_bias = F) %>%
  layer_dense(name = "L_out",
              units = ncol(dtm),
              kernel_constraint = constraint_nonneg(),
              use_bias = F)

# Use rmse as loss function
model %>% compile(
  loss = rmse,
  optimizer = optimizer_adam(lr = 0.0001)
)


# Online Topic Evolution ----------------------------------------------------------
topic_evo <- list()
for(y in c(2,4,6,8,10)) {
  print(paste0(2009 + y))
  # Slice dtm by year
  if (y == 2) {
    train_dtm <- dtm[rownames(dtm) %in% paperIDYear[paperIDYear$submitYear %in% 2007:(2009 + y), "id"], ]
    batch_size <-256L
  }else if(y == 10){
    train_dtm <- dtm[rownames(dtm) %in% paperIDYear[paperIDYear$submitYear %in% (2009 + y - 1) :(2009 + y), "id"], ]
    batch_size <- 4096L
  }else if(y == 8){
    train_dtm <- dtm[rownames(dtm) %in% paperIDYear[paperIDYear$submitYear %in% (2009 + y - 1) :(2009 + y), "id"], ]
    batch_size <- 1024L
  }else {
    train_dtm <- dtm[rownames(dtm) %in% paperIDYear[paperIDYear$submitYear %in% (2009 + y - 1) :(2009 + y), "id"], ]
    batch_size <-512L
  }
  
  
  
  # Run autoencoder with early stopping and patience epoch
  model %>% fit(
    x = train_dtm, 
    y = train_dtm, 
    epochs = epochs, 
    batch_size = batch_size,
    view_metrics = F,
    callbacks = list(callback_early_stopping(monitor = "loss", patience = 300))
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
  colnames(topic_term) <- paste0(2009 + y, "_topic_", 1:ncol(topic_term))
  topic_term <- ifelse(topic_term <= .Machine$double.eps, 0, topic_term)
  #topic_term <- round(topic_term,digits = 5)
  
  # Subtopic weight of each topic
  subtopic_weight <- round(w[[2]],digits = 5) 
  colnames(subtopic_weight) <- paste0(2009 + y, "_topic_", 1:ncol(subtopic_weight))
  rownames(subtopic_weight) <- paste0("subtopic_", 1:nrow(subtopic_weight))
  
  
  # Build subtopic-topic matrix
  subtopic_term <- w[[1]]
  #subtopic_term <- t(solve(diag(colSums(subtopic_term))) %*% t(subtopic_term))
  # Normalize for each colSum = 1
  rownames(subtopic_term) <- colnames(train_dtm)
  colnames(subtopic_term) <- paste0(2009 + y, "_sub_topic_", 1:ncol(subtopic_term))
  subtopic_term <- ifelse(subtopic_term <= .Machine$double.eps, 0, subtopic_term)
  #subtopic_term <- round(subtopic_term,digits = 8)
  
  topic_evo[[y/2]] <- list("topic_term" = topic_term,
                           "subtopic_term" = subtopic_term,
                           "subtopic_weight" = subtopic_weight,
                           "original_norm" = original_norm,
                           "pred_norm" = pred_norm,
                           "w"=w,
                           "RMSE" = RMSE)
}

# Normalize for each rowSum = 1
for(i in c(1:5)){
  topic_evo[[i]]$h_head2 <- topic_evo[[i]]$topic_term / rowSums(topic_evo[[i]]$topic_term)
  topic_evo[[i]]$h_head2 <- ifelse(is.nan(topic_evo[[i]]$h_head2), 0, topic_evo[[i]]$h_head2)
  rownames(topic_evo[[i]]$h_head2) <- rownames(topic_evo[[i]]$topic_term)
  colnames(topic_evo[[i]]$h_head2) <- paste0(2009 + (i*2), "_topic_", 1:ncol(topic_evo[[i]]$h_head2))
}

#save(topic_evo, file = "thesis/NAE11~19/topic_evo.RData")

# Generate top-10 term table
top10_terms <- lapply(topic_evo, function(x) {
  top10_terms <- find_topic(x$topic_term, 10)$term
  top10_terms <- apply(top10_terms, 2, function(x) paste0(x, collapse = ",\n"))
})
top10_terms <- do.call(what = cbind, args = top10_terms)
colnames(top10_terms) <- c("2011","2013","2015","2017","2019")
View(top10_terms)
#write.csv(top10_terms, file = "thesis/NAE11~19/top_10_terms.csv")

# Ranking of subtopics in each topic
topics_subtopics <- lapply(seq_along(topic_evo), function(i){
  topics <- topic_evo[[i]]$subtopic_weight
  topics <- apply(topics, 2, function(x) {
    topic_order <- order(x, decreasing = T)[1:3]
    x <- x[order(x, decreasing = T)][1:3]
    topic_order <- topic_order[x > 0]
    return(paste(round(x[x > 0], digits = 2), "Subtopic", topic_order, collapse = ",\n"))
  })
})
topics_subtopics <- do.call(what = cbind, args = topics_subtopics)
rownames(topics_subtopics) <- paste("Topic", seq(10))
colnames(topics_subtopics) <- c("2011","2013","2015","2017","2019")
#write.csv(topics_subtopics, file = "thesis/top3_subtopics.csv")
View(topics_subtopics)



# Normalize for each colSum = 1
for(i in c(1:5)){
  topic_evo[[i]]$h_head <- round(topic_evo[[i]]$topic_term,digits = 5)
  topic_evo[[i]]$h_head <- ifelse(topic_evo[[i]]$h_head <= .Machine$double.eps, 0, topic_evo[[i]]$h_head)
  topic_evo[[i]]$h_head <- t(solve(diag(colSums(topic_evo[[i]]$h_head))) %*% t(topic_evo[[i]]$h_head))
  rownames(topic_evo[[i]]$h_head) <- colnames(dtm)
  colnames(topic_evo[[i]]$h_head) <- paste0(2009 + (i*2), "_topic_", 1:ncol(topic_evo[[i]]$h_head))

}




