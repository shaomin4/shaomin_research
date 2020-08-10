library(keras);library(tensorflow)
Sys.setenv(CUDA_HOME = "/usr/local/cuda")
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "/usr/local/cuda/bin", sep = ":"))

# Use both card #0 and #1
# Sys.setenv(CUDA_VISIBLE_DEVICES="0,1")

# use card #1 only
Sys.setenv(CUDA_VISIBLE_DEVICES="2")

# Use 10% of a GPU card memory by default, but allow using more memory (allow_grwoth = True) 
gpu_options = tf$GPUOptions(per_process_gpu_memory_fraction = 0.5, allow_growth = T )
sess = tf$Session(config = tf$ConfigProto(gpu_options = gpu_options))

# Set Keras backend = Tensorflow
k_set_session(sess)
K = backend()
model <- application_vgg16(weights = "imagenet") 

img_path = "elephant.jpg"
ori_img = "https://goo.gl/zCTWXW"


image <- get_file(img_path, ori_img) %>%
  image_load(target_size = c(224, 224)) %>%
  image_to_array() %>%
  array_reshape(dim = c(1, 224, 224, 3)) %>%
  imagenet_preprocess_input()


preds <- model %>% predict(image)
imagenet_decode_predictions(preds, top = 3)[[1]]

# class_name class_description      score
# 1  n02504458  African_elephant 0.78988522
# 2  n01871265            tusker 0.19872670
# 3  n02504013   Indian_elephant 0.01114247

max_pred = which.max(preds[1,])

# [1] 387

pred_output <- model$output[, max_pred]
last_conv_layer <- model %>% get_layer("block5_conv3")
grads <- K$gradients(pred_output, last_conv_layer$output)[[1]]
pooled_grads <- K$mean(grads, axis = c(0L, 1L, 2L))
iterate <- K$`function`(list(model$input), 
                        list(pooled_grads, last_conv_layer$output[1,,,])) 

#c(pooled_grads_value, conv_layer_output_value) %<-% iterate(list(x))
c(pooled_grads_value, conv_layer_output_value) %<-% iterate(list(image))

for (i in 1:512) {
  conv_layer_output_value[,,i] <- 
    conv_layer_output_value[,,i] * pooled_grads_value[[i]] 
}

# average pooling
heatmap <- apply(conv_layer_output_value, c(1,2), mean)
# remove negative value
heatmap <- pmax(heatmap, 0) 
# standardization
heatmap <- heatmap / max(heatmap)

write_heatmap <- function(heatmap, filename, width = 224, height = 224,
                          bg = "white", col = terrain.colors(12)) {
  png(filename, width = width, height = height, bg = bg)
  op = par(mar = c(0,0,0,0))
  on.exit({par(op); dev.off()}, add = TRUE)
  rotate <- function(x) t(apply(x, 2, rev))
  image(rotate(heatmap), axes = FALSE, asp = 1, col = col)
}

# write heatmap
write_heatmap(heatmap, "elephant_heatmap.png") 



library(magick) 
library(viridis) 

image <- image_read("photo/bowen_face.jpg") 
info <- image_info(image) 
geometry <- sprintf("%dx%d!", info$width, info$height) 

pal <- col2rgb(viridis(20), alpha = TRUE)
alpha <- floor(seq(0, 255, length = ncol(pal))) 
pal_col <- rgb(t(pal), alpha = alpha, maxColorValue = 255)
write_heatmap(heatmap, "bowenF_overlay.png", 
              width = 256, height = 256, bg = NA, col = pal_col) 

image_read("elephant_overlay.png") %>%
  image_resize(geometry, filter = "quadratic") %>%
  image_composite(image, operator = "blend", compose_args = "20") %>%
  plot()
