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


# Model definition --------------------------------------------------------
vae_model <- function(train_dtm){
  # Parameters --------------------------------------------------------------
  batch_size <- 64L
  original_dim <- ncol(dtm)
  latent_dim <- 10L
  intermediate_dim <- 30L
  epochs <- 5000L
  epsilon_std <- 1.0
  
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
  
  # 0 ~ 1 weight constraint function
  zero2one_constraint = function(w){
    w * k_cast(k_greater_equal(w, 0) & k_less_equal(w, 1), k_floatx())
  }
  
  x <- layer_input(name="encoder_input" ,shape = c(original_dim))
  h <- layer_dense(name = "en_hidd", x, intermediate_dim, activation = "relu",
                   kernel_constraint = constraint_nonneg())
  z_mean <- layer_dense(name='z_mean', h, latent_dim, activation = "relu",
                        kernel_constraint = constraint_nonneg())
  z_log_var <- layer_dense(name='z_log_var', h, latent_dim,
                           kernel_constraint = constraint_nonneg())
  
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
    rmse_loss <- k_sqrt(k_mean(k_square(x - x_decoded_mean)))
    #kl_loss <- - k_sum(1 + z_log_var - k_square(z_mean) - k_exp(z_log_var), axis = -1L)
    kl_loss <- -0.5*k_mean(1 + z_log_var - k_square(z_mean) - k_exp(z_log_var), axis = -1L)
    return(rmse_loss + kl_loss)
  }
  
  vae %>% compile(optimizer = optimizer_rmsprop(lr = 0.0001), loss = vae_loss)
  
  vae %>% fit(
    x = train_dtm, 
    y = train_dtm, 
    shuffle = TRUE, 
    epochs = epochs, 
    batch_size = batch_size,
    view_metrics = F,
    callbacks = list(callback_early_stopping(monitor = "loss", patience = 50))
  )
  return(vae)
}


# Topic Evolution --------------------------------------------------------
topic_evo <- list()
for(y in c(1:5)) {
  print(paste0(2014 + y))
  # Slice dtm by year
  if (y == 1) {
    # 2008 ~ 2011 for setting weight
    train_dtm <- dtm[rownames(dtm) %in% paperIDYear[paperIDYear$submitYear %in% 2007:(2014 + y), "id"], ]
  } else {
    # 2012 ~ 2016 input separately
    train_dtm <- dtm[rownames(dtm) %in% paperIDYear[paperIDYear$submitYear %in% (2014 + y), "id"], ]
  }
  
  # For online learning, we don't remove terms with frequency = 0
  train_dtm <- train_dtm[rowSums(train_dtm) > 0,]
  # Do tf-idf by slices
  train_dtm <- tf_idf(dtm = train_dtm)
  train_dtm <- train_dtm / rowSums(train_dtm)
  
  
  # Run autoencoder with early stopping and patience epoch = 100
  vae <- vae_model(train_dtm)
  
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
  colnames(topic_term) <- paste0(2015 + y, "_topic_", 1:ncol(topic_term))
  #topic_term <- ifelse(topic_term <= .Machine$double.eps, 0, topic_term)
  
  # Subtopic weight of each topic
  subtopic_weight <- w[[3]]
  
  # Build subtopic-topic matrix
  subtopic_term <- w[[1]]
  #subtopic_term <- t(solve(diag(colSums(subtopic_term))) %*% t(subtopic_term))
  # Normalize for each colSum = 1
  rownames(subtopic_term) <- colnames(train_dtm)
  colnames(subtopic_term) <- paste0(2015 + y, "_sub_topic_", 1:ncol(subtopic_term))
  #subtopic_term <- ifelse(subtopic_term <= .Machine$double.eps, 0, subtopic_term)
  
  topic_evo[[y]] <- list("topic_term" = topic_term,
                         "subtopic_term" = subtopic_term,
                         "subtopic_weight" = subtopic_weight,
                         "original_norm" = original_norm,
                         "pred_norm" = pred_norm,
                         "RMSE" = RMSE)
}

# kl--------------------------------------------------------
rowSums(topic_evo[[1]]$topic_term)

