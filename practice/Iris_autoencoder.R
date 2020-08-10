### This code comes from https://statslab.eighty20.co.za/posts/autoencoders_keras_r/ .
### It is a autoencoder sample with iris.

library(ggplot2)
library(keras)
library(tidyverse)
library(tensorflow)
Sys.setenv(CUDA_HOME="/usr/local/cuda")
Sys.setenv(PATH=paste(Sys.getenv("PATH"), "/usr/local/cuda/bin", sep = ":"))
# use card #1 only
Sys.setenv(CUDA_VISIBLE_DEVICES="1")
# Use 10% of GPU memory by default, but allow using more memory (allow_grwoth = True) 
gpu_options=tf$GPUOptions(per_process_gpu_memory_fraction=0.1, allow_growth=T )
sess = tf$Session(config=tf$ConfigProto(gpu_options=gpu_options))
# Set Keras backend = Tensorflow
k_set_session(sess)

## load and prepare the data
# Split test train
split_ind <- iris$Species %>% caret::createDataPartition(p = 0.8,list = FALSE)
train <- iris[split_ind,]
test <- iris[-split_ind,]
# Pre-process
train_X <- train[,1:4] %>% as.matrix()
train_y <- train[,5] %>% 
  keras::to_categorical()
test_X <- test[,1:4] %>% as.matrix()

## define the encoder and decoder
input_layer <- 
  layer_input(shape = c(4)) 

encoder <- 
  input_layer %>% 
  layer_dense(units = 150, activation = "relu") %>% 
  layer_batch_normalization() %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 50, activation = "relu") %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 25, activation = "relu") %>%
  layer_dense(units = 2) # 2 dimensions for the output layer

decoder <- 
  encoder %>% 
  layer_dense(units = 150, activation = "relu") %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 50, activation = "relu") %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 25, activation = "relu") %>%
  layer_dense(units = 4) # 4 dimensions for the original 4 variables

## compile and train the autoencoder

autoencoder_model <- keras_model(inputs = input_layer, outputs = decoder)

autoencoder_model %>% compile(
  loss='mean_squared_error',
  optimizer='adam',
  metrics = c('accuracy')
)

summary(autoencoder_model)

history <-
  autoencoder_model %>%
  keras::fit(train_X,
             train_X,
             epochs=100,
             shuffle=TRUE,
             validation_data= list(test_X, test_X)
  )
plot(history)
