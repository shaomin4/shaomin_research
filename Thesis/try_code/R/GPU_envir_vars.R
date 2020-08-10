# Prepare GPU and setting
library(keras)
library(tensorflow)
Sys.setenv(CUDA_HOME="/usr/local/cuda")
Sys.setenv(PATH=paste(Sys.getenv("PATH"), "/usr/local/cuda/bin", sep = ":"))
# use card #1 only
Sys.setenv(CUDA_VISIBLE_DEVICES="1")
# Use 10% of GPU memory by default, but allow using more memory (allow_grwoth = True) 
#gpu_options=tf$GPUOptions(per_process_gpu_memory_fraction=0.1, allow_growth=T )
#sess = tf$Session(config=tf$ConfigProto(gpu_options=gpu_options))
# Set Keras backend = Tensorflow
#k_set_session(sess)
# Set GPU seed
#use_session_with_seed(seed = 5487, disable_gpu = F, disable_parallel_cpu = T, quiet = T)