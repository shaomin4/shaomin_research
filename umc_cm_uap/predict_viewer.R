library(shiny)
pre_main_content <- function() {
  tagList(
    fluidPage(
      
      h2("Prediction"),
      
      # Input: selector for data source
      selectInput(inputId = "prediction_data_source",
                  label = "Choose data source:",
                  choices = c("Local", "Greenplum", "HDFS")),
      
      # Selector: Local
      shinyFilesButton( "prediction_local_data_selector", "File select", "Please select a file", multiple = FALSE ),
      
      # Selector: Greenplum
      fluidRow(
        useShinyjs(),
        column(width = 6,
               passwordInput("prediction_gp_pw", "database password:"),
               actionButton("prediction_gp_login", "login greenplum")
        ),
        column(width = 12,
               # uiOutput("gp_table_select_list"),
               selectInput(inputId = "prediction_gp_table_selector",
                           label = "Database Tables:", 
                           choices = ""),
               DT::dataTableOutput("prediction_gp_table_viewer"))
      ),
      
      # Selector: HDFS
      fluidRow(
        actionButton("prediction_refresh_btn", label = "Refresh"),
        column(12, DT::dataTableOutput('prediction_hdfs_table')),
        column(5, verbatimTextOutput('prediction_noDataMsg'))
      ),
      column(5, verbatimTextOutput('prediction_mySelect')),
      actionButton( 'prediction_next_btn', label ="Next"),
      actionButton( 'prediction_pre_btn', label = "Previous"),
      
      # Selector: models
      selectInput(inputId = "trained_model_selector",
                  label = "Choose a trained model:",
                  choices = c("")),
      
      actionButton( 'pred_btn',label ="Start"),
      verbatimTextOutput("prediction_summary")
      )
  )
  
}


observe_prediction_enter_gp_passwd_in_data_chooser <- function(input, output, session, USER) {
  
  observeEvent(input$prediction_gp_login, {
    # There is no record of greenplum password
    if(input$prediction_gp_pw != ""){
      
      req(input$prediction_gp_pw)
      
      # Check connection ok
      showNotification("Start to connect to Greenplum.", type = "default")
      connection_reulst <- connectGreenplum(database_name = USER$Username, user = USER$Username, 
                                            password = input$prediction_gp_pw)
      
      if (is.null(connection_reulst$conn_id)) {
        
        # Connection failed
        showNotification(connection_reulst$msg, type = "error")
        return()
        
      } else {
        useShinyjs()
        shinyjs::hide(id = "gp_pw")
        shinyjs::hide(id = "gp_login")
        shinyjs::hide(id = "modeling_gp_pw")
        shinyjs::hide(id = "modeling_gp_login")
        shinyjs::hide(id = "prediction_gp_pw")
        shinyjs::hide(id = "prediction_gp_login")
        
        shinyjs::show(id = "gp_table_selector")
        shinyjs::show(id = "gp_table_viewer")
        shinyjs::show(id = "modeling_gp_table_selector")
        shinyjs::show(id = "modeling_gp_table_viewer")
        shinyjs::show(id = "prediction_gp_table_selector")
        shinyjs::show(id = "prediction_gp_table_viewer")
        
        # If login successfully, remember user's greenplum password
        USER$gp_pw <- input$prediction_gp_pw
        
        # Change content of table list
        # gp_table_list <- gp_get_object_name(connection_reulst$conn_id)[-1]
        gp_table_list <- gp_get_object_name(connection_reulst$conn_id)
        
        # Disconnect greenplum
        db.disconnect(conn.id = connection_reulst$conn_id)
        
        # Return new select input UI
        updateSelectInput(session = session, 
                          inputId = "gp_table_selector",
                          label = "Database Tables:",
                          choices = c(gp_table_list))
        updateSelectInput(session = session, 
                          inputId = "modeling_gp_table_selector",
                          label = "Database Tables:",
                          choices = c(gp_table_list))
        updateSelectInput(session = session, 
                          inputId = "prediction_gp_table_selector",
                          label = "Database Tables:",
                          choices = c(gp_table_list))
        
      }
      
    } else {
      
      # Show message of input password empty
      showNotification("You must enter greenplum password", type = "error")
      
      return(NULL)
      
    }
  })
}


observe_prediction_data_chooser <- function(input, output, USER, session, PREDICTION_SELECTED_PATH) {
  # -------------------- #
  # Local preparing part #
  reactive({
    shinyFileChoose(input = input, id = "prediction_local_data_selector", updateFreq = 0, 
                    session = session, roots = USER$root, filetypes = c('', 'csv'))
  })
  # ------------------- #
  # HDFS preparing part #
  hdfs_table <- reactiveValues(value = NULL) # for reload datatable
  selectedPath <- reactiveValues(value = NULL) # for choose path
  Path <- reactiveValues(oldpath = NULL, goback = NULL, originalPath = NULL, path = NULL) # for choose path
  
  # ---- Initialize HDFS ----
  hdfs.init()
  
  # ---- For reload dataframe ----   
  observe({
    req(USER$Username)
    username = USER$Username
    path = paste("/home", username, sep = "/")
    hdfs_df = hdfs.ls(path)
    Path$path = path
    if(!is.null(hdfs_df)){
      # if hdfs_table$value changed, dataframe will reload. hdfs_table$value is a path to show the list of folder.
      output$prediction_hdfs_table = DT::renderDataTable({
        if(is.null(hdfs_table$value)){
          hdfs_table$value <- hdfs_df
          hdfs_df
        }else{
          hdfs_table$value
        }
      }, selection = 'single')
    }else{
      output$prediction_noDataMsg = renderPrint({
        cat("No data available in table")
      })
    }
  })
  
  # ---- For refresh button ----
  observeEvent(input$prediction_refresh_btn, {
    Path$oldpath = NULL
    Path$goback = NULL
    Path$originalPath = NULL
    hdfs_table$value <- hdfs.ls(Path$path)
  })
  
  # ---- For output selected path ----
  # Print the selected index
  selection = reactive ({
    input$prediction_hdfs_table_rows_selected
  })
  # Show the selected path
  output$prediction_mySelect = renderPrint({
    req(selection())
    if (length(selection())) {
      cat("path:")
      selectedPath$value <- hdfs_table$value[input$prediction_hdfs_table_rows_selected, 6]
      selectedPath <- selectedPath$value
      cat(selectedPath, sep = "&")
    }
  })
  observe({
    req(selection())
    if (length(selection())) {
      cat("path:")
      selectedPath$value <- hdfs_table$value[input$prediction_hdfs_table_rows_selected, 6]
      selectedPath <- selectedPath$value
      cat(selectedPath, sep = "&")
      PREDICTION_SELECTED_PATH$path <- selectedPath
    }
  })
  
  # ---- For next button ----
  observeEvent(input$prediction_next_btn, {
    if (length(selection())) {
      Path$goback <- Path$oldpath 
      newHdfs = hdfs.ls(selectedPath$value)
      hdfs_table$value <- newHdfs
      Path$oldpath <- selectedPath$value
    }
    else{
      Path$oldpath <- Path$path
      hdfs_table$value <- hdfs.ls(Path$oldpath)
    }
  })
  
  # ---- For previous button ----
  observeEvent(input$prediction_pre_btn, {
    Path$oldpath <- Path$goback
    newHdfs = hdfs.ls(Path$goback)
    hdfs_table$value <- newHdfs
    Path$goback <- Path$path #if you want to go back twice, will go back to home directory now
  })
  
  observeEvent(input$prediction_data_source, {
    useShinyjs()
    if(input$prediction_data_source == "Local") {
      # ---- show ----
      shinyjs::show(id = "prediction_local_data_selector")
      # --------------
      shinyjs::hide(id = "modeling_gp_pw")
      shinyjs::hide(id = "modeling_gp_login")
      shinyjs::hide(id = "modeling_gp_table_selector")
      shinyjs::hide(id = "modeling_gp_table_viewer")
      shinyjs::hide(id = "prediction_gp_pw")
      shinyjs::hide(id = "prediction_gp_login")
      shinyjs::hide(id = "prediction_gp_table_selector")
      shinyjs::hide(id = "prediction_gp_table_viewer")
      shinyjs::hide(id = "prediction_refresh_btn")
      shinyjs::hide(id = "prediction_hdfs_table")
      shinyjs::hide(id = "prediction_noDataMsg")
      shinyjs::hide(id = "prediction_mySelect")
      shinyjs::hide(id = "prediction_next_btn")
      shinyjs::hide(id = "prediction_pre_btn")
      
      
    } else if(input$prediction_data_source == "Greenplum") {
      if(USER$gp_pw != '') {
        shinyjs::hide(id = "prediction_local_data_selector")
        shinyjs::hide(id = "modeling_gp_pw")
        shinyjs::hide(id = "modeling_gp_login")
        shinyjs::hide(id = "prediction_gp_pw")
        shinyjs::hide(id = "prediction_gp_login")
        # ---- show ----
        shinyjs::show(id = "modeling_gp_table_selector")
        shinyjs::show(id = "modeling_gp_table_viewer")
        shinyjs::show(id = "prediction_gp_table_selector")
        shinyjs::show(id = "prediction_gp_table_viewer")
        # --------------
        shinyjs::hide(id = "prediction_refresh_btn")
        shinyjs::hide(id = "prediction_hdfs_table")
        shinyjs::hide(id = "prediction_noDataMsg")
        shinyjs::hide(id = "prediction_mySelect")
        shinyjs::hide(id = "prediction_next_btn")
        shinyjs::hide(id = "prediction_pre_btn")
        # Update table list
        connection_reulst <- connectGreenplum(database_name = USER$Username, user = USER$Username, 
                                              password = USER$gp_pw)
        if (is.null(connection_reulst$conn_id)) {
          
          # Connection failed
          showNotification(connection_reulst$msg, type = "error")
          return()
          
        } else {
          # Change content of table list
          # gp_table_list <- gp_get_object_name(connection_reulst$conn_id)[-1]
          gp_table_list <- gp_get_object_name(connection_reulst$conn_id)
          
          # Disconnect greenplum
          db.disconnect(conn.id = connection_reulst$conn_id)
          
          # Return new select input UI
          updateSelectInput(session = session, 
                            inputId = "gp_table_selector",
                            label = "Database Tables:",
                            choices = c(gp_table_list))
          updateSelectInput(session = session, 
                            inputId = "modeling_gp_table_selector",
                            label = "Database Tables:",
                            choices = c(gp_table_list))
          updateSelectInput(session = session, 
                            inputId = "prediction_gp_table_selector",
                            label = "Database Tables:",
                            choices = c(gp_table_list))
        }
      } else {
        shinyjs::hide(id = "prediction_local_data_selector")
        # ---- show ----
        shinyjs::show(id = "modeling_gp_pw")
        shinyjs::show(id = "modeling_gp_login")
        shinyjs::show(id = "prediction_gp_pw")
        shinyjs::show(id = "prediction_gp_login")
        # --------------
        shinyjs::hide(id = "modeling_gp_table_selector")
        shinyjs::hide(id = "modeling_gp_table_viewer")
        shinyjs::hide(id = "prediction_gp_table_selector")
        shinyjs::hide(id = "prediction_gp_table_viewer")
        shinyjs::hide(id = "prediction_refresh_btn")
        shinyjs::hide(id = "prediction_hdfs_table")
        shinyjs::hide(id = "prediction_noDataMsg")
        shinyjs::hide(id = "prediction_mySelect")
        shinyjs::hide(id = "prediction_next_btn")
        shinyjs::hide(id = "prediction_pre_btn")
      }
    } else if(input$prediction_data_source == "HDFS") {
      shinyjs::hide(id = "prediction_local_data_selector")
      shinyjs::hide(id = "modeling_gp_pw")
      shinyjs::hide(id = "modeling_gp_login")
      shinyjs::hide(id = "modeling_gp_table_selector")
      shinyjs::hide(id = "modeling_gp_table_viewer")
      shinyjs::hide(id = "prediction_gp_pw")
      shinyjs::hide(id = "prediction_gp_login")
      shinyjs::hide(id = "prediction_gp_table_selector")
      shinyjs::hide(id = "prediction_gp_table_viewer")
      # ---- show ----
      shinyjs::show(id = "prediction_refresh_btn")
      shinyjs::show(id = "prediction_hdfs_table")
      shinyjs::show(id = "prediction_noDataMsg")
      shinyjs::show(id = "prediction_mySelect")
      shinyjs::show(id = "prediction_next_btn")
      shinyjs::show(id = "prediction_pre_btn")
      # --------------
    }
  })
  
}

observe_prediction_chosen_local_file <- function(input, output, USER, session, PREDICTION_SELECTED_PATH) {
  
  observe({
    
    if(!is.null(input$prediction_local_data_selector)) {
      
      file_info <- parseFilePaths(USER$root, input$prediction_local_data_selector)
      
      file_path <- as.character(file_info$datapath)
      
      if(!identical(file_path, character(0))) {
        df <- read.csv(file = file_path)
        PREDICTION_SELECTED_PATH$path <- file_path
        
      }
      
    }
  })
  
}

observe_prediction_chosen_greenplum_table <- function(input, output, USER, session, PREDICTION_SELECTED_PATH) {
  
  observe({
    
    req(input$prediction_gp_table_selector)
    connection_reulst <- connectGreenplum(database_name = USER$Username, user = USER$Username, 
                                          password = USER$gp_pw)
    
    if (is.null(connection_reulst$conn_id)) {
      
      # Connection failed
      showNotification(connection_reulst$msg, type = "error")
      target_table <- data.frame()
      
    } else {
      PREDICTION_SELECTED_PATH$path <- paste("public", input$prediction_gp_table_selector, sep = ".")
      print(PREDICTION_SELECTED_PATH$path)
      # Get table content
      target_table <- db.data.frame(paste("public", input$prediction_gp_table_selector, sep = "."), 
                                    conn.id = connection_reulst$conn_id)
      # Disconnect greenplum
      db.disconnect(conn.id = connection_reulst$conn_id)
      
    }
  })
  
}