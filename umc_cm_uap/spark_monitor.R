library(httr)
library(mailR)
library(rvest)
source("process_monitor.R")
# source("kill_process.R")

# --------------------
# parseRunningAppTable function is used to parse running applications table
parseRunningAppTable <- function(table_tags) {
  
  # -------------------
  # removeSpaceAndEnter function is used to remove spaces, tabs and enter characters
  removeSpaceAndEnter <- function(string) {
    gsub(pattern = "[\n\t\ ]", replacement = "", string)
  }
  
  running_app_rows <- html_nodes(table_tags, "tbody tr")
  running_application_df <- data.frame("application_id" = character(0), "name" = character(0),
                                       "cores" = character(0), "memory_per_executor" = character(0),
                                       "submitted_time" = character(0), "user" = character(0), 
                                       "state" = character(0), "duration" = character(0))
  
  for(row in running_app_rows) {
    columns <- html_nodes(row, "td")
    
    application_id <- gsub(x = html_text(columns[1]),
                           pattern = ".*(app-[0-9]*-[0-9]*).*", replacement = "\\1")
    name <- removeSpaceAndEnter(html_text(columns[2]))
    cores <- removeSpaceAndEnter(html_text(columns[3]))
    memory_per_executor <- removeSpaceAndEnter(html_text(columns[4]))
    submitted_time <- html_text(columns[5])
    user <- html_text(columns[6])
    state <- html_text(columns[7])
    duration <- html_text(columns[8])
    
    prepared_row <- data.frame("application_id" = application_id, "name" = name,
                               "cores" = cores, "memory_per_executor" = memory_per_executor,
                               "submitted_time" = submitted_time, "user" = user, 
                               "state" = state, "duration" = duration)
    running_application_df <- rbind(running_application_df, prepared_row)
  }
  
  return(running_application_df)
}

# -----------------
# parseWorkersTable is used to parse workers table
parseWorkersTable <- function(table_tags) {
  
  # -------------------
  # removeSpaceAndEnter function is used to remove spaces, tabs and enter characters
  removeSpaceAndEnter <- function(string) {
    gsub(pattern = "[\n\t\ ]", replacement = "", string)
  }
  
  workers_rows <- html_nodes(table_tags, "tbody tr")
  workers_df <- data.frame("worker_id" = character(0), "address" = character(0), 
                           "state" = character(0), "cores" = character(0), "memory" = character(0))
  
  for(row in workers_rows) {
    columns <- html_nodes(row, "td")
    
    worker_id <- removeSpaceAndEnter(html_text(columns[1]))
    address <- html_text(columns[2])
    state <- html_text(columns[3])
    cores <- removeSpaceAndEnter(html_text(columns[4]))
    memory <- removeSpaceAndEnter(html_text(columns[5]))
    
    prepared_row <- data.frame("worker_id" = worker_id, "address" = address, 
                               "state" = state, "cores" = cores, "memory" = memory)
    workers_df <- rbind(workers_df, prepared_row)
  }
  
  return(workers_df)
}

# SubFunction ----
generateApplicationsTable <- function(applications_tbls) {

  applications_df <- parseRunningAppTable(applications_tbls)
  
  # Get process status data frame
  process_table = process_monitor()
  # Generate kill buttons
  kill_action_btns <- Map(x = applications_df$application_id, 
                          f = function(x) {
                            shinyInput(actionButton, paste0('buttonSpark_', x), 
                                       label = "Kill", 
                                       onclick = 'Shiny.onInputChange(\"select_spark_button\",  this.id)')
                          })
  # Unlist object which return from Map function
  kill_action_btns <- unlist(kill_action_btns)
  applications_df <- cbind(applications_df, Kill = kill_action_btns)
  # rownames(applications_df) <- applications_df$application_id
  return(applications_df)
}


## Observe kill button in process list
observe_spark_application_kill <- function(input, output, session, user_name) {
  observeEvent(input$select_spark_button, {
    # Get pid
    application_id <- strsplit(input$select_spark_button, "_")[[1]][2]
    result <- POST(url = "http://192.168.1.100:8080/app/kill/", 
                   body = list(id = application_id, 
                               terminate = "true"), 
                   encode="form")
    refresh_spark_monitor_df_render(input, output, session, refresh = T, user_name = user_name)
    showNotification(paste("Kill", application_id),
                     type = "message")
  })
}


# Render ----
# Content of task monitor ====
spark_monitor_main_content <- function() {
  tagList(
    sidebarLayout(
      sidebarPanel(width = 2,
                   h4(
                     textOutput("spark_currentTime", container = span)
                   ),
                   selectInput("spark_mon_interval", "Update every:", c(
                     "5 seconds" = "5000",
                     "30 seconds" = "30000",
                     "1 minute" = "60000"
                   ), selected = 30000, selectize = FALSE)
      ),
      
      mainPanel(width = 10,
                div(style = 'overflow-x: scroll', 
                    titlePanel("Workers"),
                    dataTableOutput("spark_workers_df_contents"),
                    titlePanel("Running Applications"),
                    dataTableOutput("spark_applications_df_contents"))
      )
    )
  )
}

spark_monitor_time_render <- function(session) {
  # Refresh every 0.1 secs
  renderText({
    invalidateLater(as.integer(100), session)
    return(format(Sys.time()))
  })
}

refresh_spark_monitor_df_render <- function(input, output, session, process_df, refresh = F, user_name = "") {
  workers_dt = renderDataTable(
    expr = {
      # Timer to refresh this data table
      if(refresh) {
        invalidateLater(as.integer(input$spark_mon_interval), session)
      }
      
      spark_UI_url <- "http://192.168.1.100:8080/applications?status=running"
      html <- read_html(spark_UI_url)
      tables <- html_nodes(html, ".table")
      workers_tbls <- tables[1]
      workers_table <- parseWorkersTable(workers_tbls)
      return(workers_table)
      # process_df$workers_table <- workers_table
      # return(process_df$workers_table)
      }, 
    escape = FALSE)
  output$spark_workers_df_contents <- workers_dt
  
  applications_dt = renderDataTable(
    expr = {
      # Timer to refresh this data table
      if(refresh) {
        invalidateLater(as.integer(input$spark_mon_interval), session)
      }
      
      spark_UI_url <- "http://192.168.1.100:8080/applications?status=running"
      html <- read_html(spark_UI_url)
      tables <- html_nodes(html, ".table")
      running_app_tbl <- tables[2]
      applications_table <- generateApplicationsTable(running_app_tbl)
      return(applications_table)
      # process_df$applications_table <- applications_table
      # return(process_df$applications_table)
      }, 
    escape = FALSE,
    options = list(search = list(regex = TRUE, caseInsensitive = FALSE, search = user_name)))
  output$spark_applications_df_contents <- applications_dt
}

shinyInput <- function(FUN, id, ...) {
  input <- as.character(FUN(id, ...))
  return(input)
}