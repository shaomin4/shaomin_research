# Setting Hadoop environment variables
Sys.setenv("HADOOP_CMD" = "/home/hadoop/hadoop/bin/hadoop");
Sys.setenv("HADOOP_STREAMING"="/home/hadoop/hadoop/share/hadoop/tools/lib/hadoop-streaming-3.0.0.jar")
# library ----
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyFiles)
library(devtools)
library(rhdfs)
library(radiant.data)
# source ----
source("process_monitor.R")
source("main_ui_content.R")
source("login.R")
source("task_monitor.R")
source("spark_monitor.R")
source("import_cluster.R")
source("View_hdfs.R")
source("greenplum_viewer.R")
# source("spark_train_model.R")


# UI part ----
ui <- dashboardPage(skin="black",
                    header = dashboardHeader(title = "Unified Analytics Platform"),
                    sidebar = 
                      dashboardSidebar(
                        sidebarMenu(sidebar_content())
                      ),
                    
                    body = 
                      dashboardBody(
                        tabitems_content()
                      )
                    
) # End of dashboardPage


# Server part ----
server = function(input, output, session) {
  
  # Login ====
  # Init USER reactive values
  USER <- reactiveValues(authenticated = FALSE, root = c(wd = '.'), gp_pw = "")
  # Set up login modal observer
  login_observer <- observe_login()
  # Set up login action observer
  login_action_observer <- observe_login_act(input, USER, login_observer)
  
  
  # Uplaod files ====
  # Oberse type of uploading
  upload_type_checking_observer <- observe_upload_type_checking(input, USER)
  url_user_defined_delimiter_observer <- observe_url_user_defined_delimiter(input)
  # Preview uploaded file
  output$ufl_df_content <- ufl_df_preview(input)
  # Upload file to server
  upload_action_observer <- observe_upload_act(input, USER)
  # Notice message
  output$text1 <- renderText({ paste("Notice: Maximum file size is 60MB",input$n)})
  
  # json
  json_observe(input, output, session)
  
  observe_upload_json_act(input, USER)
  
  # Import to cluster ====
  # Preview the file which will import to cluster 
  output$i2c_df_content <- i2c_df_preview(input, output, session, USER)
  # Observe import to cluster action button
  import_cluster_action_observer <- observe_import_cluster_act(input, output, USER)
  i2c_user_defined_delimiter_observer <- observe_i2c_user_defined_delimiter(input)
  import_to_greenplum_action_observer <- observeImportToGreenplumAction(input, output, session, USER)
  
  # Data Visualize
  
  #updatevars(input, output, session, USER)
  output$vis_filename <- updatevars(input, output, session, USER)
  output$vis_result <- visualize_result(input, output, session, USER)
  
  # View HDFS ====
  #Observe datatable & selection change
  import_view_hdfs_obseve <- view_hdfs_preview(input, output, session, USER)
  
  
  
  # Task monitor ====
  # Showing time
  output$currentTime <- tsk_monitor_time_render(session)
  # Refresh process data frame
  process_df <- reactiveValues(table = NULL)
  # Init process data table
  refresh_tsk_monitor_df_render(input, output, session, process_df, refresh = T, user_name = USER$Username)
  # Kill process
  PID <- reactiveValues()
  ## Observe kill button in process list
  process_kill_observer <- observe_kill(input, output, PID)
  ## Observe kill cancel action button in modal
  kill_cancel_action_observer <- observe_kill_cancel_action(input, output, session, process_df, USER)
  ## Observe kill confirm button in modal
  process_kill_action_observer <- observe_kill_action(input, output, session, process_df, PID, process_kill_observer, USER)
  ## Observe kill and send email confirm button in modal
  process_kill_action_observer <- observe_kil_n_send_email_l_action(input, output, session, process_df, PID, process_kill_observer, USER)
  
  
  # GPU monitor ====
  # Showing time
  output$gpu_currentTime <- gpu_monitor_time_render(session)
  # Get the status from GPU
  get_gpu_stat <- gpu_process_monitor()
  # Get the information of NVIDIA-smi and driver
  output$gpu_nvidia <- gpu_info_render(get_gpu_stat$gpu_info[1], session)
  output$gpu_driver <- gpu_info_render(get_gpu_stat$gpu_info[2], session)
  # Refresh GPU status data frame
  output$gpu_stat_contents <- gpu_monitor_stat_df_render(input, session)
  # Refresh GPU process data frame
  gpu_process_df <- reactiveValues(table = NULL)
  gpu_monitor_process_df_render(input, output, session, gpu_process_df, refresh = T)
  # GPU Kill process
  gpu_PID <- reactiveValues()
  ## Observe kill button in process list
  gpu_process_kill_observer <- observe_kill(input, output, gpu_PID)
  ## Observe kill confirm button in modal
  gpu_process_kill_action_observer <- observe_kill_action(input, output, session, gpu_process_df, gpu_PID, process_kill_observer, USER)
  ## Observe kill and send email confirm button in modal
  gpu_process_kill_action_observer <- observe_kil_n_send_email_l_action(input, output, session, gpu_process_df, gpu_PID, process_kill_observer, USER)
  
  
  # Spark monitor ====
  output$spark_currentTime <- spark_monitor_time_render(session)
  refresh_spark_monitor_df_render(input, output, session, process_df, refresh = T, user_name = USER$Username)
  observe_spark_application_kill(input, output, session, USER$Username)
  
  
  # View greenplum
  refreshing_greenplum_table_selector_observer <- observe_refreshing_greenplum_table_selector(input, output, USER, session)
  output$gp_table_viewer <- gp_df_preview(input, output, USER)
  
  
  # Modeling
  # SELECTED_PATH <- reactiveValues(path = "")
  # modeling_data_chooser_observer <- observe_modeling_data_chooser(input, output, USER, session, SELECTED_PATH)
  # enter_gp_passwd_in_data_chooser_observer <- observe_enter_gp_passwd_in_data_chooser(input, output, session, USER)
  # chosen_local_file_observer <- observe_chosen_local_file(input, output, USER, session, SELECTED_PATH)
  # chosen_greenplum_table_observer <- observe_chosen_greenplum_table(input, output, USER, session, SELECTED_PATH)
  
  # Prediction
  # PREDICTION_SELECTED_PATH <- reactiveValues(path = "")
  # prediction_data_chooser_observer <- observe_prediction_data_chooser(input, output, USER, session, PREDICTION_SELECTED_PATH)
  # prediction_enter_gp_passwd_in_data_chooser_observer <- observe_prediction_enter_gp_passwd_in_data_chooser(input, output, session, USER)
  # chosen_prediction_local_file_observer <- observe_prediction_chosen_local_file(input, output, USER, session, PREDICTION_SELECTED_PATH)
  # chosen_prediction_greenplum_table_observer <- observe_prediction_chosen_greenplum_table(input, output, USER, session, PREDICTION_SELECTED_PATH)
  # observe({
  #   req(USER$root)
  #   trained_model_list <- get_trained_model_list(USER$root)
  #   print(trained_model_list)
  #   updateSelectInput(session = session, 
  #                     inputId = "trained_model_selector",
  #                     label = "Database Tables:",
  #                     choices = c("", trained_model_list))
  # })
  
  # train model by spark 
  
  # path = "/home/bowen/diamonds.csv"
  # modeltype = "rf"
  # outcome = "color"
  # testdata = diamond_sdf
  
  # modeltype <- get_modeltype
  # outcome <- get_outcome
  
  # model <- train_button(input, output, session, SELECTED_PATH$path, USER)
  # prediction <- predict_button(input, output, session, PREDICTION_SELECTED_PATH$path, USER)

}

# Limit size of uploaded file
upload_size_limit <- 60  # MB
MB_unit <- 1024^2
options(shiny.maxRequestSize = upload_size_limit * MB_unit)

# Start shiny app ----
shinyApp(ui,server)