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

#load("/data_lake/arXiv_ML/dym_2019.RData")
load("/data_lake/arXiv_ML/tfym_2019.RData")

# Parameters --------------------------------------------------------------
batch_size <- 64L
original_dtm <- ncol(dtm)
original_dym <- ncol(dym)
latent_dim <- 10L
intermediate_dim <- 30L
epochs <- 3000L
epsilon_std <- 1.0

# Model definition --------------------------------------------------------
left_input <- layer_input(name="left_input" ,shape = c(original_dtm))
left_branch <- layer_dense(name="left_branch", left_input, intermediate_dim, activation = "relu",
                           kernel_constraint = constraint_nonneg())
left_code <- layer_dense(name = "left_code",left_branch,latent_dim, activation = "relu",
                         kernel_constraint = constraint_nonneg())

right_input <- layer_input(name="right_input",shape = c(original_dym))
right_branch <- layer_dense(name="right_branch", right_input, intermediate_dim, activation = "relu",
                            kernel_constraint = constraint_nonneg())
right_code <- layer_dense(name = "right_code",right_branch,latent_dim, activation = "relu",
                         kernel_constraint = constraint_nonneg())


z <- layer_concatenate(list(left_code, right_code)) %>% 
  layer_dense(name = "code_layer",latent_dim, activation = "relu",
              kernel_constraint = constraint_nonneg())

encoder <- keras_model(c(left_input,right_input),z)

# we instantiate these layers separately so as to reuse them later
decoder_left <- layer_dense(name = "left_decoder",z,units = intermediate_dim, activation = "relu",
                         kernel_constraint = constraint_nonneg())

decoder_right <- layer_dense(name= "right_decoder",z,units = intermediate_dim, activation = "relu",
                            kernel_constraint = constraint_nonneg())

left_output <- layer_dense(name= "left_output",decoder_left,units = original_dtm, activation = "relu",
                           kernel_constraint = constraint_nonneg())

right_output <- layer_dense(name= "right_output",decoder_right ,units = original_dym, activation = "relu",
                           kernel_constraint = constraint_nonneg())


autoencoder <- keras_model(c(left_input,right_input),c(left_output,right_output))


# RMSE
rmse <- function(y_true, y_pred) {
  return(k_sqrt(k_mean(k_square(y_true - y_pred))))
}

# Use rmse as loss function
autoencoder %>% compile(
  loss = rmse,
  optimizer = optimizer_adam(lr = 0.001)
)

# Run autoencoder with early stopping and patience epoch = 100
autoencoder %>% fit(list(dtm,dym),
                    list(dtm,dym),
                    epochs = 5, 
                    batch_size = 128,
                    view_metrics = F,
                    callbacks = list(callback_early_stopping(monitor = "loss", patience = 100))
                    )


# Get weight from Keras model
w <- get_weights(autoencoder)
View(w)
# Build topic-term matrix
topic_term <- w[[1]] %*% w[[5]]
# Normalize for each colSum = 1
topic_term <- t(solve(diag(colSums(topic_term))) %*% t(topic_term))
rownames(topic_term) <- colnames(dtm)
colnames(topic_term) <- paste0(2019, "_topic_", 1:ncol(topic_term))
topic_term <- ifelse(topic_term <= .Machine$double.eps, 0, topic_term)

top10_terms <- find_topic(topic_term, 10)$term
top10_terms <- top10_terms

# Build topic-term matrix
topic_year <- w[[3]] %*% w[[7]]
# Normalize for each colSum = 1
topic_year <- t(solve(diag(colSums(topic_year))) %*% t(topic_year))
rownames(topic_year) <- colnames(dym)
colnames(topic_year) <- paste0(2019, "_topic_", 1:ncol(topic_year))
topic_year <- ifelse(topic_year <= .Machine$double.eps, 0, topic_year)

top10_terms <- find_topic(topic_term, 10)$term
top10_terms <- top10_terms






