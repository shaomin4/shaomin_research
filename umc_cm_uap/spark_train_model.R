library(sparklyr)

# Load data to spark from hdfs
hdfs_to_spark <- function(sc, path) {
  # tempFileName <- strsplit(path, split="/", fixed=T)[[1]]
  # fileName <- tempFileName[length(tempFileName)]
  # fileName <- strsplit(fileName, split=".",fixed=T)[[1]][1]
  path <- paste0("hdfs:///", path)
  sdf <- spark_read_csv(sc, name = "filename", path = path, header = T)
  return(sdf)
}

# Load data to spark from local
local_to_spark <- function(sc, path) {
  # tempFileName <- strsplit(path, split="/", fixed=T)[[1]]
  # fileName <- tempFileName[length(tempFileName)]
  # fileName <- strsplit(fileName, split=".",fixed=T)[[1]][1]
  sdf <- spark_read_csv(sc, name = "filename", path = path, header = T)
  return(sdf)
}

# build a model
build_model <- function(sdf, modeltype, outcome, variable="."){
  if(identical(modeltype, "spark.glm")){
    model <- sdf %>% ml_generalized_linear_regression(response = outcome, features = variable)
  }
  else if (identical(modeltype, "spark.randomForest") ){
    model <- sdf %>% ml_random_forest_classifier(response = outcome, features = variable , num_trees = 100L)
  }
  return(model)
}

# fit on test data and output a confusion matrix
predict_result <- function(testdata, model){
  result <- testdata %>% sdf_predict(model) %>% select(label ,prediction)
  result <- as.data.frame(result)
  return(table(result$label,result$prediction))
}

connect_to_spark <- function(){
  conf = spark_config(); conf$spark.ui.port = 4040 # default port
  conf$spark.executor.memory = "4G" # memory per working node
  conf$spark.yarn.executor.memoryOverhead = "1024"
  spark_home = "/usr/local/spark/spark-2.3.0-bin-hadoop2.7"
  # Create a connection to local Spark
  # You may check out http://192.168.1.100:8080/ for Spark UI
  sc = spark_connect(master = "spark://192.168.1.100:7077", version = "2.3.0",
                     app_name = "local_app", config = conf, spark_home = spark_home)
  return(sc)
}


train_button <- function(input, output, session, path, USER) {
  observeEvent(input$train_btn, {
    modeltype <- input$model_selector
    outcome <- input$target_col
    
    if(input$modeling_data_source == "Greenplum") {
      print("Greenplum is not avaiable now.")
    } else {
      # Connect spark
      sc <- connect_to_spark()
      # Deploy data
      if(input$modeling_data_source == "Local") {
        sdf <- local_to_spark(sc, path)
      } 
      else if(input$modeling_data_source == "HDFS") {
        sdf <- hdfs_to_spark(sc, path)
      }
      # Train model
      showNotification("Start to train the model.", type = "default")
      model <- build_model(sdf, modeltype, outcome)
      showNotification("Finish training.", type = "message")
      
      # Save the model
      model_saved_dest = paste(USER$root, 
                               paste0(format(Sys.time(), "%g%m%d%H%M%S"), "_model.RData"), 
                               sep = "/")
      print(model_saved_dest)
      ml_save(model, model_saved_dest, overwrite=T)
      # Report model information
      output$model_summary <- renderPrint({  summary(model) })
      
      # Disconnect spark
      # spark_disconnect(sc = sc)
    }
    
    # Update list of model selector in prediction part
    trained_model_list <- get_trained_model_list(USER$root)
    updateSelectInput(session = session, 
                      inputId = "trained_model_selector",
                      label = "Database Tables:",
                      choices = c(trained_model_list))
  })
}

get_trained_model_list <- function(path) {
  model_path <- list.files(path)
  model_path <- model_path[grep(pattern = "[0-9]*_model.RData", x = model_path)]
  return(model_path)
}

predict_button <- function(input, output, session, path, USER) {
  observeEvent(input$pred_btn, {
    if(identical(input$trained_model_selector, "")) {
      print("No selected trained model")
    } else {
      # Connect spark
      sc <- connect_to_spark()
      testdata <- hdfs_to_spark(sc, path)
      model = ml_load(sc, paste(USER$root, input$trained_model_selector, sep = "/"))
      cf <- predict_result(testdata, model)
      # spark_disconnect(sc = sc)
      output$prediction_summary <- renderPrint({ cf })
    }
    
  })
}

# get_modeltype <- function(input, output, session){
#   reactive({
#     modeltype <- input$model_selector
#   })
# }
# 
# get_outcome <- function(input, output, session){
#   reactive({
#     outcome <- input$target_col
#   })
# }

