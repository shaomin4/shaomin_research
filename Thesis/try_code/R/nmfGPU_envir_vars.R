library(nmfgpu4R)
# 設定路徑
Sys.setenv(CUDA_PATH = "/usr/local/cuda")
Sys.setenv(CUDA_ROOT = "/usr/local/cuda")
Sys.setenv(CUDA_HOME = "/usr/local/cuda")
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "/usr/local/cuda/bin", sep = ":"))
Sys.setenv(NMFGPU_ROOT = "/usr/local/nmfgpu")
Sys.setenv(LD_LIBRARY_PATH = paste(Sys.getenv("LD_LIBRARY_PATH"), "/usr/local/cuda/lib", sep = ":"))
# Initialize nmfgpu4R
nmfgpu4R.init()
chooseDevice(0)

