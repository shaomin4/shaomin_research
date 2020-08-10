# library ----
library(shinyFiles)
library(PivotalR)


# sub-functions ----
gp_get_object_name = function(conn_id){
  
  # Get all table name
  db_objects = db.objects(conn.id = conn_id)
  # Filter table name which start with public.*
  public_object = grep("^public\\.", db_objects, value = FALSE)
  # Delete table name which contain madlib_tmp_
  delete_object = grep("^public\\.madlib_temp_", db_objects, value = FALSE)
  table_object = setdiff(public_object, delete_object)
  
  table_names = as.character(db_objects[table_object])
  table_names = gsub("^public\\.","",table_names)
  
  return(table_names)
}


# sub-functions ----
get_gp_table_content = function(conn_id, table_name) {
  
  # List all object
  db_objects = db.objects(conn.id = conn_id)
  # Get the position of target table
  target_table_name = paste("public.", table_name, sep="")
  table_object = which(db_objects == target_table_name)
  
  # Output table
  db_table <- db.data.frame(db_objects[table_object])
  target_table <- lk(db_table, 100)
  
  return(target_table)
}


vgp_main_content <- function() {
  
  tagList(
    fluidPage(
      titlePanel("Database Viewer"),
      fluidRow(
        useShinyjs(),
        # If there is no greenplum password record
        # Show password input and login button
        column(width = 6,
               passwordInput("gp_pw", "database password:"),
               actionButton("gp_login", "login greenplum")
        ),
        # If there is it, show table selector and data viwer
        column(width = 12,
               # uiOutput("gp_table_select_list"),
               selectInput(inputId = "gp_table_selector",
                           label = "Database Tables:", 
                           choices = ""),
               DT::dataTableOutput("gp_table_viewer"))
        )
    )
  )
  
}


gp_df_preview <- function(input, output, USER) {
  
  DT::renderDataTable(
    DT::datatable({
      
      table_name <- input$gp_table_selector
      
      if (table_name != "") {
        
        # Check connection ok
        connection_reulst <- connectGreenplum(database_name = USER$Username, user = USER$Username, 
                                              password = USER$gp_pw)
        
        if (is.null(connection_reulst$conn_id)) {
          
          # Connection failed
          showNotification(connection_reulst$msg, type = "error")
          target_table <- data.frame()
          
        } else {
          
          # Get table content
          target_table <- get_gp_table_content(conn_id = connection_reulst$conn_id, table_name = table_name)
          target_table <- as.data.frame(target_table)
          # Disconnect greenplum
          db.disconnect(conn.id = connection_reulst$conn_id)
        }
        
      } else {
        target_table <- data.frame()
      }
      
      # Return target_table to data table object
      return(target_table) })
  )
}

observe_refreshing_greenplum_table_selector <- function(input, output, USER, session) {
  
  # hide selector and viewer first
  shinyjs::hide(id = "gp_table_selector")
  shinyjs::hide(id = "gp_table_viewer")
  shinyjs::hide(id = "modeling_gp_table_selector")
  shinyjs::hide(id = "modeling_gp_table_viewer")
  
  observeEvent(input$gp_login, { 
    
    # There is no record of greenplum password
    if(input$gp_pw != ""){
      
      req(input$gp_pw)
      
      # Check connection ok
      showNotification("Start to connect to Greenplum.", type = "default")
      connection_reulst <- connectGreenplum(database_name = USER$Username, user = USER$Username, 
                                            password = input$gp_pw)
      
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
        shinyjs::show(id = "gp_table_selector")
        shinyjs::show(id = "gp_table_viewer")
        #if(input$modeling_data_source == "Greenplum") {
        #  shinyjs::show(id = "modeling_gp_table_selector")
        #  shinyjs::show(id = "modeling_gp_table_viewer")
        #}
        #if(input$prediction_data_source == "Greenplum") {
        #  shinyjs::show(id = "prediction_gp_table_selector")
        #  shinyjs::show(id = "prediction_gp_table_viewer")
        #}
        
        
        # If login successfully, remember user's greenplum password
        USER$gp_pw <- input$gp_pw
        
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