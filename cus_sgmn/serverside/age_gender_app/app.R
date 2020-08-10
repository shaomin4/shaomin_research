# Import packages ----
library(shiny)
library(shinyjs)
library(dygraphs)
library(RPostgreSQL)
library(htmlwidgets)
library(jsonlite)

options(digits.secs=6)
options(digits=6)

config <- read_json("../../config.json")[[1]]
processed_photos_path <- config$server$age_gender$path$processed_photos

ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)

ui <- fluidPage( 
  
  titlePanel("Face Age and Gender Monitor"),
  
  fluidRow(
    column(width = 12,  
           selectInput(inputId = "machine_id", label = "Machine ID", choices = character(0)),
           dygraphOutput("age_gender_graph"),
           imageOutput("chosen_processed_photo"))
  )
  
)


server <- function(input, output, session) {
  
  # PF_VAR <- reactiveValues(last_fetch_time = Sys.time(), current_time = Sys.time(), age_gender_df = NA)
  PF_VAR <- reactiveValues(age_gender_df = NA)
  
  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  con <- dbConnect(drv, dbname = config$server$psql_database,
                   host = config$server$psql_host, port = config$server$psql_port,
                   user = config$server$psql_user, password = config$server$psql_password)
  
  machine_ids <- dbGetQuery(con, "SELECT machine_id FROM age_gender GROUP BY machine_id;")$machine_id
  
  updateSelectInput(inputId = "machine_id", session = session, choices = machine_ids)
  
  observeEvent(input$machine_id, {
    req(input$machine_id)
    # PF_VAR$last_fetch_time <- Sys.time()
    age_gender_df <- dbGetQuery(con, paste0("select time, COUNT(index) AS detected_faces 
                                             FROM age_gender 
                                             WHERE machine_id='", input$machine_id, "' 
                                             GROUP BY time
                                             ORDER BY time"))
    rownames(age_gender_df) <- format(age_gender_df$time, "%Y-%m-%d %H:%M:%OS6")
    age_gender_df <- age_gender_df[, c("detected_faces"), drop=F]
    PF_VAR$age_gender_df <- age_gender_df
  })
  
  
  # ----
  # Auto Refresh: not finished, so I commented them.
  # observe({
  #   # Re-execute this reactive expression after 5000 milliseconds
  #   invalidateLater(5000, session)
  #   print("QQ")
  #   PF_VAR$last_fetch_time <- PF_VAR$current_time
  #   PF_VAR$current_time <- Sys.time()
  # })
  
  # observe({
  #   last_fetch_time <- PF_VAR$last_fetch_time
  #   new_age_gender_df <- dbGetQuery(con, paste0("SELECT * FROM age_gender WHERE time > ', ", last_fetch_time, "' ORDER BY time"))
  #   if(nrow(new_age_gender_df) > 0) {
  #     rownames(new_age_gender_df) <- format(new_age_gender_df$time, "%Y-%m-%d %H:%M:%OS6")
  #     new_age_gender_df$time <- NULL
  #     PF_VAR$age_gender_df <- rbind(PF_VAR$age_gender_df, new_age_gender_df)
  #   }
  # })
  
  observe({
    req(PF_VAR$age_gender_df)
    output$age_gender_graph <- renderDygraph({
      dygraph_obj <- dygraph(PF_VAR$age_gender_df) %>%
        dySeries("detected_faces", label = "People Flow") %>%
        dyBarChart() %>%
        dyCallbacks(clickCallback =
                      sprintf("function(e, x, pt) {
                                 Shiny.onInputChange('clicked_time_idx', parseInt(pt[0]['idx'])+1);
                               }" ))
    })
  })
  
  observe({
    req(input$clicked_time_idx)
    
    output$chosen_processed_photo <- renderImage({
      # Get timestamp
      chosen_timestamp <- rownames(PF_VAR$age_gender_df)[input$clicked_time_idx]
      # Turn timestamp into POSIX format
      chosen_timestamp <- as.POSIXlt(chosen_timestamp)
      # Turn timestamp from POSIX to numeric
      chosen_timestamp <- as.double(chosen_timestamp)
      # Get formal file name
      chosen_photo_name <- list.files(processed_photos_path, 
                                      pattern = paste(input$machine_id, substr(format(chosen_timestamp, digits = 16), 1, 14), sep = "_"))
      print(chosen_photo_name)
      # Get whole path
      chosen_photo_path <- paste0(processed_photos_path, chosen_photo_name)
      # Return a list containing the filename
      list(src = chosen_photo_path)
    }, deleteFile = FALSE)
  })
  
}


onStart = function() {
  cat("Doing application setup\n")
  
  onStop(function() {
    cat("Doing application cleanup\n")
    conns <- dbListConnections(drv)
    disconnect_result <- lapply(conns, dbDisconnect)
    print(disconnect_result)
  })
}

shinyApp(ui = ui, server = server, onStart = onStart)
