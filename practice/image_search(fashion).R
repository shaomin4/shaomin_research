# Library to manipulate image files
library(magick)
source("load_gpu_kears.R")
# Food-101 folder path
fashion_dir = "/data_lake/fashionStyleNew/"

# Set current working directory to food101_dir
#setwd(food101_dir)
# Show image files in a folder, take "dumplings" as the example
# Map(function(x) image_read(x),
#     list.files(paste(food101_dir, "/images/dumplings/", sep = "") , full.names = T))

# All original image folders with classe names
image_folders = list.files(paste(fashion_dir, sep = ""), full.names = F)
# All test image file path
img_test = list()
for(f in image_folders){
  img_test = append(img_test,list.files((paste(fashion_dir,f, sep = "")), recursive = TRUE, full.names = TRUE))
}

# Here we consider using pre-trained VGG16 model for fine-tuning
model = application_vgg16(weights = "imagenet", include_top = FALSE,input_shape = c(224,224,3))
model = keras_model_sequential() %>%
  model %>%
  layer_flatten() %>%
  layer_dense(units = 1024, activation = "relu") %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_dense(units = 100,activation = "softmax")


### Get image feature.
predictions = list()
for( i in img_test){
  img_path <- i
  img <- image_load(img_path, target_size = c(224,224))
  x <- image_to_array(img)
  x <- array_reshape(x, c(1, dim(x)))
  x <- imagenet_preprocess_input(x)
  predictions[[i]] = predict(model,x)
}
#save(predictions,file = "~/RProject/food101/predictions_feature_flatten.RData")
#load("~/RProject/food101/predictions_feature_fintune.RData")

# Get test image feature
img_path <- "/data_lake/fashionStyleNew/Artsy Fashion Style/000016.jpg"
ori_img <- image_read(img_path) 
ori_img%>% plot
img <- image_load(img_path, target_size = c(224,224))
x <- image_to_array(img)
x <- array_reshape(x, c(1, dim(x)))
x <- imagenet_preprocess_input(x)
test_pre = predict(model,x) 

# Calculate distance with dist function with euclidean
#similar_img <- lapply(predictions, function(x){
#  as.numeric(dist(rbind(test_pre,x)))
#})

# try JSD (if use JSD the each predict output of sum must 1,so suggest last layer use softmax.) 
similar_img <- lapply(predictions,function(x){
  m <- 0.5 * (test_pre + x)
  JS <- 0.5 * (sum(test_pre * log(test_pre / m)) + sum(x * log(x / m)))
  #philentropy::JSD(rbind(test_pre,x),unit = "log")
})


similar_five <- similar_img[order(unlist(similar_img), decreasing = F)][1:5]
names(similar_five)
pre_img <- image_read(names(similar_five))

# display testing image and predict image
img <- c(ori_img,pre_img)
img <- image_scale(img, "300x300")
image_append(image_scale(img, "x200"), stack = TRUE)

