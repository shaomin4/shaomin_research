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
dtm <- load_dtm(file = "/data_lake/arXiv_ML/dtm_2019.RData", tfIdf = F)
dtm <- dtm[rownames(dtm) %in% all_df$id,]
dtm <- dtm[rowSums(dtm) > 1, colSums(dtm) > 1]

# Cut text data into slices as per year.
paperIDYear <- all_df[, 1:2]
paperIDYear$submitYear <- as.integer(substr(paperIDYear$submitted, 1, 4))
paperIDYear$submitted <- NULL




# Parameters --------------------------------------------------------------
batch_size <- 64L
original_dim <- ncol(dtm)
latent_dim <- 15L
intermediate_dim <- 30L
epochs <- 5000L
epsilon_std <- 1.0

# Model definition --------------------------------------------------------
sampling <- function(arg){
  z_mean <- arg[, 1:(latent_dim)]
  z_log_var <- arg[, (latent_dim + 1):(2 * latent_dim)]
  
  epsilon <- k_random_normal(
    shape = c(k_shape(z_mean)[[1]]), 
    mean=0.,
    stddev=epsilon_std
  )
  
  return(z_mean + k_exp(z_log_var/2)*epsilon)
}

x <- layer_input(name="encoder_input" ,shape = c(original_dim))
h <- layer_dense(name = "en_hidd", x, intermediate_dim, activation = "relu",
                 kernel_constraint = constraint_nonneg())
z_mean <- layer_dense(name='z_mean', h, latent_dim,
                      kernel_constraint = constraint_nonneg())
z_log_var <- layer_dense(name='z_log_var', h, latent_dim)

# note that "output_shape" isn't necessary with the TensorFlow backend
z <- layer_concatenate(list(z_mean, z_log_var)) %>% 
  layer_lambda(name='z', sampling)

# we instantiate these layers separately so as to reuse them later
decoder_h <- layer_dense(name = "de_hidd",units = intermediate_dim, activation = "relu",
                         kernel_constraint = constraint_nonneg())
decoder_mean <- layer_dense(name= "output",units = original_dim, activation = "relu",
                            kernel_constraint = constraint_nonneg())
h_decoded <- decoder_h(z)
x_decoded_mean <- decoder_mean(h_decoded)

# end-to-end autoencoder
vae <- keras_model(x, x_decoded_mean)

# encoder, from inputs to latent space
encoder <- keras_model(x, z_mean)

# generator, from latent space to reconstructed inputs
decoder_input <- layer_input(shape = latent_dim)
h_decoded_2 <- decoder_h(decoder_input)
x_decoded_mean_2 <- decoder_mean(h_decoded_2)
generator <- keras_model(decoder_input, x_decoded_mean_2)

vae_loss <- function(x, x_decoded_mean){
  #xent_loss <- (original_dim/1.0)*loss_binary_crossentropy(x, x_decoded_mean)
  rmse_loss <- k_sqrt(k_mean(k_square(x - x_decoded_mean)))
  kl_loss <- - k_sum(1 + z_log_var - k_square(z_mean) - k_exp(z_log_var), axis = -1L)
  return(rmse_loss + kl_loss)
}

vae %>% compile(optimizer = optimizer_rmsprop(lr = 0.0001), loss = vae_loss)

# Single Model training ----------------------------------------------------------
vae %>% fit(
  dtm, dtm, 
  shuffle = TRUE, 
  epochs = epochs, 
  batch_size = batch_size,
  view_metrics = F,
  callbacks = list(callback_early_stopping(monitor = "loss", patience = 50))
)

# Get weight from Keras model
w <- get_weights(vae)
View(w)

# Build topic-term matrix
topic_term <- w[[1]] %*% w[[3]]
# Normalize for each colSum = 1
topic_term <- t(solve(diag(colSums(topic_term))) %*% t(topic_term))
rownames(topic_term) <- colnames(dtm)
colnames(topic_term) <- paste0(2019, "_topic_", 1:ncol(topic_term))
topic_term <- ifelse(topic_term <= .Machine$double.eps, 0, topic_term)

top10_terms <- find_topic(topic_term, 10)$term
top10_terms_15 <- top10_terms

save(top10_terms_15,file="output/2015_top10.RData")
top10_2020 <- get(load("shaomin/output/2019_top10.RData"))

# Online Topic Evolution ----------------------------------------------------------
topic_evo <- list()
for(y in c(1:5)) {
  print(paste0(2014 + y))
  # Slice dtm by year
  # 200˙ ~ 2011 for setting weight
  train_dtm <- dtm[rownames(dtm) %in% paperIDYear[paperIDYear$submitYear %in% 2007:(2014 + y), "id"], ]

  
  # For online learning, we don't remove terms with frequency = 0
  train_dtm <- train_dtm[rowSums(train_dtm) > 0,]
  # Do tf-idf by slices
  train_dtm <- tf_idf(dtm = train_dtm)
  train_dtm <- train_dtm / rowSums(train_dtm)
  
  
  # Run autoencoder with early stopping and patience epoch = 100
  vae %>% fit(
    x = train_dtm, 
    y = train_dtm, 
    shuffle = TRUE, 
    epochs = epochs, 
    batch_size = batch_size,
    view_metrics = F,
    callbacks = list(callback_early_stopping(monitor = "loss", patience = 30))
  )
  
  #if (!dir.exists(path = "shaomin/online_models")) {
  #  dir.create(path = "shaomin/online_models")
  #}
  
  # Save model for each year
  #save_model_hdf5(object = vae, filepath = paste0("shaomin/online_models/vae_model", y))
  
  # Compute the Frobenius norm for input dtm
  original_norm <- norm(train_dtm, type = "F")
  
  # Recovery Frobenius norm
  pred_norm <- norm(train_dtm - predict(vae, train_dtm), type = "F")
  
  # RMSE
  RMSE <- sqrt(mean((train_dtm - predict(vae, train_dtm)) ^ 2))
  
  # Get weight from Keras model
  w <- get_weights(vae)
  
  # Build topic-term matrix
  topic_term <- w[[1]] %*% w[[3]]
  # Normalize for each colSum = 1
  #topic_term <- t(solve(diag(colSums(topic_term))) %*% t(topic_term))
  rownames(topic_term) <- colnames(train_dtm)
  colnames(topic_term) <- paste0(2014 + y, "_topic_", 1:ncol(topic_term))
  topic_term <- ifelse(topic_term <= .Machine$double.eps, 0, topic_term)
  topic_term <- round(topic_term,digits = 8)
  
  # Subtopic weight of each topic
  subtopic_weight <- round(w[[3]],digits = 5) 
  colnames(subtopic_weight) <- paste0(2014 + y, "_topic_", 1:ncol(subtopic_weight))
  rownames(subtopic_weight) <- paste0("subtopic_", 1:nrow(subtopic_weight))
  
  # Build subtopic-topic matrix
  subtopic_term <- w[[1]]
  #subtopic_term <- t(solve(diag(colSums(subtopic_term))) %*% t(subtopic_term))
  # Normalize for each colSum = 1
  rownames(subtopic_term) <- colnames(train_dtm)
  colnames(subtopic_term) <- paste0(2014 + y, "_sub_topic_", 1:ncol(subtopic_term))
  subtopic_term <- ifelse(subtopic_term <= .Machine$double.eps, 0, subtopic_term)
  subtopic_term <- round(subtopic_term,digits = 8)
  
  
  topic_evo[[y]] <- list("topic_term" = topic_term,
                         "subtopic_term" = subtopic_term,
                         "subtopic_weight" = subtopic_weight,
                         "original_norm" = original_norm,
                         "pred_norm" = pred_norm,
                         "w" = w,
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
#write.csv(top10_terms, file = "output/VAE_list_top15.csv")
View(top10_terms)


top10_terms_value <- lapply(topic_evo, function(x) {
  top10_terms <- find_topic(x$topic_term, 10)$value
  top10_terms <- apply(top10_terms, 2, function(x) paste0(x, collapse = ",\n"))
})
top10_terms_value <- do.call(what = cbind, args = top10_terms_value)
colnames(top10_terms) <- 2015:2019






