# library ----
library(shinyFiles)
library(PivotalR)
source("do_greenplum_operator.R")
source("copy_file.R")


# Modal ----
## Greenplum password input modal
getGreenplumLoginModal <- function(USER) {
  
  modalDialog(
    fluidRow(
      column(width = 12, 
             textInput("greenplum_tblname", "Table Name:"),
             if (USER$gp_pw == "") {
               passwordInput("greenplum_password", "Greenplum Password:")
             }
             
      )),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("enter_greenplum_passwd_btn", "Upload")
    )
  )
}


# Observer ----
## Observe user click 'user defined delimiter'
observe_i2c_user_defined_delimiter <- function(input) {
  
  observe({
    
    req(input$i2c_sep)
    
    useShinyjs()
    
    if(input$i2c_sep == "userDefined") {
      enable("i2c_user_defined_delimiter")
    } else {
      disable("i2c_user_defined_delimiter")
    }
    
  })
}

observe_import_cluster_act = function(input, output, USER) {
  observeEvent(input$import_cluster_btn, {
    
    req(input$import_file)
    
    file_info = parseFilePaths(USER$root, input$import_file)
    file_path = as.character(file_info$datapath)
    file_name = as.character(file_info$name)
    
    dest_service = input$i2c_dest
    
    if(dest_service == "greenplum") {
      showModal(getGreenplumLoginModal(USER))
    } else if(dest_service == "hdfs") {
      showNotification("Start to import into HDFS", type = "default")
      dest_hdfs_dir <- paste0("/home/", USER$Username)
      #hdfs_move_cmd = system(paste("/home/hadoop/hadoop/bin/hdfs dfs -put",file_path,dest_hdfs_dir,"2>&1"),intern = T)
      hdfs_move_cmd = put_hdfs_file(from = file_path,to = dest_hdfs_dir,user = USER$Username)
      cat(hdfs_move_cmd)
      file_name <- strsplit(x = dest_hdfs_dir, split = "/")[[1]]
      file_name <- file_name[length(file_name)]
      filePath <- paste(dest_hdfs_dir,file_name,sep = "/")
      if(length(hdfs_move_cmd)==0){
        showNotification("Successfully upload!", type = "message")
        hdfs_owner_reuslt <- chown_hdfs_file(file = filePath,  user = USER$Username)
      }else{
        showNotification(hdfs_move_cmd, type = "warning")
      }
    }
    
  })
}

observeImportToGreenplumAction <- function(input, output, session, USER) {
  observeEvent(input$enter_greenplum_passwd_btn, {
    
    req(input$greenplum_tblname)
    
    # Setup connection parameters
    gp_user   <- USER$Username
    gb_dbname <- USER$Username
    gp_passwd <- input$greenplum_password
    
    # There is no record of greenplum password
    if(USER$gp_pw == "") {
      
      if (input$greenplum_password == "") {
        showNotification("You must enter greenplum password", type = "error")
        return()
      }
      
      # Check connection ok
      showNotification("Start to connect to Greenplum.", type = "default")
      connection_reulst <- connectGreenplum(database_name = gb_dbname, user = gp_user, 
                                            password = gp_passwd)
      
      if(is.null(connection_reulst$conn_id)) {
        
        # Connection failed
        showNotification(connection_reulst$msg, type = "error")
        return()
        
      } else {
        
        # Remember this greenplum password
        USER$gp_pw <- gp_passwd
        showNotification(connection_reulst$msg, type = "message")
        
      }
      
    } else {
      
      gp_passwd <- USER$gp_pw
      
      # Check connection ok
      showNotification("Start to connect Greenplum.", type = "default")
      connection_reulst <- connectGreenplum(database_name = gb_dbname, user = gp_user, 
                                            password = gp_passwd)
      
      if (is.null(connection_reulst$conn_id)) {
        
        # Connect failed
        output$import_result <- renderText(connection_reulst$msg)
        showNotification(connection_reulst$msg, type = "error")
        
        USER$gp_pw <- ""
        useShinyjs()
        shinyjs::show(id = "gp_pw")
        shinyjs::show(id = "gp_login")
        shinyjs::show(id = "modeling_gp_pw")
        shinyjs::show(id = "modeling_gp_login")
        shinyjs::show(id = "prediction_gp_pw")
        shinyjs::show(id = "prediction_gp_login")
        shinyjs::hide(id = "gp_table_selector")
        shinyjs::hide(id = "gp_table_viewer")
        shinyjs::hide(id = "modeling_gp_table_selector")
        shinyjs::hide(id = "modeling_gp_table_viewer")
        shinyjs::hide(id = "prediction_gp_table_selector")
        shinyjs::hide(id = "prediction_gp_table_viewer")
        
        return()
        
      } else {
        
        showNotification(connection_reulst$msg, type = "message")
        
      }  
      
    }
    
    # Read imported file
    file_info = parseFilePaths(USER$root, input$import_file)
    file_path = as.character(file_info$datapath)
    file_name = as.character(file_info$name)
    
    # Check user defined delimiter
    if(input$i2c_sep == "userDefined") {
      delimiter <- input$i2c_user_defined_delimiter
    } else {
      delimiter <- input$i2c_sep
    }
    
    # Read CSV
    df <- read.csv(file_path,
                   header = input$i2c_header,
                   sep = delimiter,
                   quote = input$i2c_quote, 
                   nrows = 10)
    
    # Setup greenplum import parameters
    gp_tblname <- input$greenplum_tblname
    gp_tblname <- paste("public", gp_tblname, sep=".")
    
    # Import to greenplum
    showNotification("Creating data schema in Greenplum.", type = "default")
    summary_result <- addTable(conn_id = connection_reulst$conn_id, db_object = df, table_name = gp_tblname)
    
    if (summary_result$succ == FALSE) {
      # Import failed
      output$import_result <- renderText(summary_result$msg)
      showNotification(summary_result$msg, type = "error")
    } else {
      output$import_result <- renderText(summary_result$msg)
      showNotification(summary_result$msg, type = "message")
      
      # Yeild yml file content
      yml_content <- paste0(
        "---
        VERSION: 1.0.0.1
        DATABASE: ", gb_dbname, "
        USER: ", gp_user, "
        PASSWORD: ", gp_passwd, "
        HOST: 192.168.1.100
        PORT: 5432
        GPLOAD:
        INPUT:
        - SOURCE:
        LOCAL_HOSTNAME:
        - ", "192.168.1.5", "
        PORT: 5432
        FILE:
        - ", file_path, "
        - FORMAT: text
        - DELIMITER: '", delimiter, "'
        - QUOTE: '", input$i2c_quote, "'
        - ERROR_LIMIT: 25
        - error_table: public.med_ord_pgm_d_err
        OUTPUT:
        - TABLE: ", gp_tblname, "
        - MODE: INSERT")
      yml_path <- paste0("/tmp/", gp_tblname, ".yml")
      write(x = yml_content, file = yml_path, append = FALSE)
      
      showNotification("Importing data in Greenplum.", type = "default")
      print(paste0("source /opt/greenplum/greenplum-db/greenplum_path.sh && $GPHOME/bin/gpload -f ", yml_path))
      system(paste0("source /opt/greenplum/greenplum-db/greenplum_path.sh && $GPHOME/bin/gpload -f ", yml_path))
      
      useShinyjs()
      shinyjs::hide(id = "gp_pw")
      shinyjs::hide(id = "gp_login")
      shinyjs::hide(id = "modeling_gp_pw")
      shinyjs::hide(id = "modeling_gp_login")
      shinyjs::hide(id = "prediction_gp_pw")
      shinyjs::hide(id = "prediction_gp_login")
      shinyjs::show(id = "gp_table_selector")
      shinyjs::show(id = "gp_table_viewer")
      # if(input$modeling_data_source == "Greenplum") {
      #   shinyjs::show(id = "modeling_gp_table_selector")
      #   shinyjs::show(id = "modeling_gp_table_viewer")
      # }
      # if(input$prediction_data_source == "Greenplum") {
      #   shinyjs::show(id = "prediction_gp_table_selector")
      #   shinyjs::show(id = "prediction_gp_table_viewer")
      # }
      
      gp_table_list <- gp_get_object_name(connection_reulst$conn_id)
      
      # Change content of table list
      updateSelectInput(session = session, 
                        inputId = "gp_table_selector",
                        label = "Database Tables:",
                        choices = c(gp_table_list))
      # updateSelectInput(session = session, 
      #                   inputId = "modeling_gp_table_selector",
      #                   label = "Database Tables:",
      #                   choices = c(gp_table_list))
      # updateSelectInput(session = session, 
      #                   inputId = "prediction_gp_table_selector",
      #                   label = "Database Tables:",
      #                   choices = c(gp_table_list))
      
    }
    
    # Disconnect greenplum
    db.disconnect(conn.id = connection_reulst$conn_id)
    
    # Remove import modal
    removeModal()
    
  })
}



# Render ----
# Content of import to cluster ====
i2c_main_content = function() {
  tagList(
    sidebarLayout(
      sidebarPanel(
        
        h4("Server file selection"),
        shinyFilesButton('import_file', 'File select', 'Please select a file', FALSE),
        
        hr(),
        
        # Table setting
        # Input: Checkbox if file has header ----
        checkboxInput("i2c_header", "Header", TRUE),  
        # Input: Select separator ----
        radioButtons("i2c_sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t",
                                 UserDefined = "userDefined"),
                     selected = ","),
        
        # call useShinyjs() before using disable and enable
        useShinyjs(),
        disabled(
          textInput(inputId = "i2c_user_defined_delimiter", label = "User defined delimiter")
        ),
        # Input: Select quotes ----
        radioButtons("i2c_quote", "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"'),
        # Input: Select number of rows to display ----
        radioButtons("i2c_disp", "Display",
                     choices = c(Head = "head",
                                 All = "all"),
                     selected = "head"),
        
        hr(),
        
        radioButtons("i2c_dest", "Import to",
                     choices = c(HDFS = "hdfs",
                                 Greenplum = "greenplum"),
                     selected = "greenplum"),
        
        hr(),
        
        actionButton("import_cluster_btn", "Import")
      ),
      mainPanel(
        verbatimTextOutput("import_result"),
        titlePanel("Data Preview"),
        div(style = 'overflow-x: scroll', dataTableOutput("i2c_df_content"))
      )
    )
  )
}

i2c_df_preview = function(input, output, session, user_info) {
  renderDataTable({
    
    # input$urf_upload_file will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$import_file)
    
    file_info <- parseFilePaths(user_info$root, input$import_file)
    
    file_path <- as.character(file_info$datapath)
    
    if(input$i2c_sep == "userDefined") {
      delimiter <- input$i2c_user_defined_delimiter
    } else {
      delimiter <- input$i2c_sep
    }
    
    if(input$i2c_disp == "head") {
      df <- read.csv(file_path,
                     header = input$i2c_header,
                     sep = delimiter,
                     quote = input$i2c_quote, nrows = 100)
      return(head(df))
    } else {
      df <- read.csv(file_path,
                     header = input$i2c_header,
                     sep = delimiter,
                     quote = input$i2c_quote)
      return(df)
    }
    
  })
}


