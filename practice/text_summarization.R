library(keras)

# Listing 8.1 Reweighting a probability distribution to a different temperature
reweight_distribution <- function(original_distribution, temperature = 0.5){
  distribution <- log(original_distribution) / temperature
  distribution <- exp(distribution)
  distribution / sum(distribution)
}

# Listing 8.2 Downloading and parsing the initial text file
library(stringr)

path <- get_file(
  "nietzsche.txt",
  origin = "https://s3.amazonaws.com/text-datasets/nietzsche.txt"
)
text <- tolower(readChar(path, file.info(path)$size))
cat("Corpus length:", nchar(text), "\n")

# Listing 8.3 Vectorizing sequences of characters
maxlen <- 60
step <- 3

text_indexes <- seq(1, nchar(text) - maxlen, by = step)
sentences <- str_sub(text, text_indexes, text_indexes + maxlen - 1)
next_chars <- str_sub(text, text_indexes + maxlen, text_indexes + maxlen)
cat("Number of sequences: ", length(sentences), "\n")

chars <- unique(sort(strsplit(text, "")[[1]]))
cat("Unique characters:", length(chars), "\n")

char_indices <- 1:length(chars)
names(char_indices) <- chars

cat("Vectorization...\n")
x <- array(0L, dim = c(length(sentences), maxlen, length(chars)))
y <- array(0L, dim = c(length(sentences), length(chars)))
for (i in 1:length(sentences)) {
  sentence <- strsplit(sentences[[i]], "")[[1]]
  for (t in 1:length(sentence)){
    char <- sentence[[t]] 
    x[i, t, char_indices[[char]]] <- 1
  }
  next_char <- next_chars[[i]]
  y[i, char_indices[[next_char]]] <- 1
}


library(keras); library(tensorflow)
Sys.setenv(CUDA_HOME="/usr/local/cuda")
Sys.setenv(PATH=paste(Sys.getenv("PATH"), "/usr/local/cuda/bin", sep = ":"))

# Use both card #0 and #1
# Sys.setenv(CUDA_VISIBLE_DEVICES="0,1")

# use card #1 only
Sys.setenv(CUDA_VISIBLE_DEVICES="1")

# Use 10% of GPU memory by default, but allow using more memory (allow_grwoth = True) 
gpu_options=tf$GPUOptions(per_process_gpu_memory_fraction=0.5, allow_growth=T )
sess = tf$Session(config=tf$ConfigProto(gpu_options=gpu_options))

# Set Keras backend = Tensorflow
k_set_session(sess)



# Listing 8.4 Single-layer LSTM model for next-character prediction
model <- keras_model_sequential() %>%
  layer_lstm(units = 128, input_shape = c(maxlen, length(chars))) %>%
  layer_dense(units = length(chars), activation = "softmax")

# Listing 8.5 Model compilation configuration
optimizer <- optimizer_rmsprop(lr = 0.01)

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer
)

# Listing 8.6 Function to sample the next character given the model’s
# predictions
sample_next_char <- function(preds, temperature = 1.0){
  preds <- as.numeric(preds)
  preds <- log(preds) / temperature
  exp_preds <- exp(preds)
  preds <- exp_preds / sum(exp_preds)
  which.max(t(rmultinom(1, 1, preds)))
}

# Listing 8.7 Text-generation loop


model %>% fit(x, y, batch_size = 128, epochs = 5)

start_index <- sample(1:(nchar(text) - maxlen - 1), 1)
seed_text <- str_sub(text, start_index, start_index + maxlen - 1)

cat("—- Generating with seed:", seed_text, "\n\n")

for(temperature in c(0.2, 0.5, 1.0, 1.2)){
  cat("—---- temperature:", temperature, "\n")
  cat(seed_text, "\n")
  
  generated_text <- seed_text
  
  for(i in 1:400){
    sampled <- array(0, dim = c(1, maxlen, length(chars)))
    generated_chars <- strsplit(generated_text, "")[[1]]
    for(t in 1:length(generated_chars)){
      char <- generated_chars[[t]]
      sampled[1, t, char_indices[[char]]] <- 1
    }
    
    preds <- model %>% predict(sampled, verbose = 0)
    next_index <- sample_next_char(preds[1,], temperature)
    next_char <- chars[[next_index]]
    
    generated_text <- paste0(generated_text, next_char)
    generated_text <- substring(generated_text, 2)
    
    cat(next_char)
    
  }
  cat("\n\n")
}

