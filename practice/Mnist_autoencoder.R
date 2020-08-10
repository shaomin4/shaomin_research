######### Mnist autoencoder ##########
# Prepare GPU and setting
library(keras)
library(tensorflow)
Sys.setenv(CUDA_HOME="/usr/local/cuda")
Sys.setenv(PATH=paste(Sys.getenv("PATH"), "/usr/local/cuda/bin", sep = ":"))
# use card #1 only
Sys.setenv(CUDA_VISIBLE_DEVICES="0")
# Use 10% of GPU memory by default, but allow using more memory (allow_grwoth = True) 
gpu_options=tf$GPUOptions(per_process_gpu_memory_fraction=0.1, allow_growth=T )
sess = tf$Session(config=tf$ConfigProto(gpu_options=gpu_options))
# Set Keras backend = Tensorflow
k_set_session(sess)

# load MNIST
mnist <- dataset_mnist()
c(c(train_images, train_labels), c(test_images, test_labels)) %<-% mnist


# Autoencoder model
input_layer <- layer_input(shape = c(784),name="image_input")

# encoder
encoder <- input_layer %>%
  layer_dense(128,activation = "relu") %>%
  layer_dense(64,activation =  "relu") %>%
  layer_dense(10,activation = "relu") %>%
  layer_dense(2)
# decoder
decoder <- encoder %>%
  layer_dense(10,activation = "relu") %>%
  layer_dense(64,activation =  "relu") %>%
  layer_dense(128,activation = "relu") %>%
  layer_dense(784,activation = "tanh")

autoencoder_model <- keras_model(inputs = input_layer, outputs = decoder)

encoder_model <- keras_model(input_layer,encoder)
summary(autoencoder_model)

## compile and train the autoencoder

autoencoder_model %>% compile(
  loss='mean_squared_error',
  optimizer='adam',
  metrics = c('accuracy')
)

history <-
  autoencoder_model %>%
  keras::fit(x_train,
             x_train,
             epochs=20,
             shuffle=TRUE,
             batch_size=256
             
  )
plot(history)



