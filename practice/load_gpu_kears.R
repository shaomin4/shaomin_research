library(keras); library(tensorflow)
Sys.setenv(CUDA_HOME = "/usr/local/cuda")
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "/usr/local/cuda/bin", sep = ":"))
# Use both card #0 and #1
# Sys.setenv(CUDA_VISIBLE_DEVICES="0,1")

# use card #1 only
Sys.setenv(CUDA_VISIBLE_DEVICES="1")

# Use 10% of a GPU card memory by default, but allow using more memory (allow_grwoth = True) 
gpu_options = tf$GPUOptions(per_process_gpu_memory_fraction = 0.9, allow_growth = T )
sess = tf$Session(config = tf$ConfigProto(gpu_options = gpu_options))

# Set Keras backend = Tensorflow
k_set_session(sess)
