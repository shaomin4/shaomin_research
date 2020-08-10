library(keras); library(tensorflow)
Sys.setenv(CUDA_HOME="/usr/local/cuda")
Sys.setenv(PATH=paste(Sys.getenv("PATH"), "/usr/local/cuda/bin", sep = ":"))

# Use both card #0 and #1
# Sys.setenv(CUDA_VISIBLE_DEVICES="0,1")

# use card #1 only
Sys.setenv(CUDA_VISIBLE_DEVICES="1")

# Use 10% of GPU memory by default, but allow using more memory (allow_grwoth = True) 
gpu_options=tf$GPUOptions(per_process_gpu_memory_fraction=0.1, allow_growth=T )
sess = tf$Session(config=tf$ConfigProto(gpu_options=gpu_options))

# Set Keras backend = Tensorflow
k_set_session(sess)

# Example from "Deep Learnig with Keras"
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu", input_shape = c(28, 28, 1)) %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu")

model <- model %>%
  layer_flatten() %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dense(units = 10, activation = "softmax")

summary(model)

# load MNIST
mnist <- dataset_mnist()
c(c(train_images, train_labels), c(test_images, test_labels)) %<-% mnist
train_images <- array_reshape(train_images, c(60000, 28, 28, 1))
train_images <- train_images / 255
train_labels <- to_categorical(train_labels)
test_labels <- to_categorical(test_labels)
model %>% compile( optimizer = "rmsprop", loss = "categorical_crossentropy", metrics = c("accuracy") )
model %>% fit( train_images, train_labels, epochs = 5, batch_size=64 )
