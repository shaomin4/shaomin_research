library(mailR)
source("gpu_process_monitor.R")
source("kill_process.R")

# SubFunction ----
generateGPUProcessTable <- function() {
  # Get process status data frame
  gpu_process_table = gpu_process_monitor()$process_df
  if(gpu_process_table[1, 1] != "No running processes found"){
    # Generate kill buttons
    kill_action_btns <- Map(x = gpu_process_table$PID, 
                            f = function(x) {
                              shinyInput(actionButton, paste0('button_', x), 
                                         label = "Kill", 
                                         onclick = 'Shiny.onInputChange(\"select_button\",  this.id)')
                            })
    # Unlist object which return from Map function
    kill_action_btns <- unlist(kill_action_btns)
    gpu_process_table <- cbind(gpu_process_table, Kill = kill_action_btns)
  }
  return(gpu_process_table)
}

# Modal ----
killModal <- function(email = "") {
  
  modalDialog(
    fluidRow(
      column(width = 12, 
             h4("You want to kill the following process."),
             div(style = 'overflow-x: scroll', dataTableOutput("selected_process_info")),
             textInput(inputId = "process_owner_email", label = "Process owner email:", 
                       value = email, placeholder = "process_owner@email.here"),
             textAreaInput(inputId = "kill_message", label = "Leave message:", 
                           placeholder = "Leave some message here...")
      )),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("kill_btn", "Just Kill"),
      actionButton("kill_n_send_email_btn", "Send Email and Kill")
    )
  )
}

# Observer ----
gpu_monitor_main_content = function() {
  tagList(
    sidebarLayout(
      sidebarPanel(width = 2,
        h4(
          textOutput("gpu_currentTime", container = span)
        ),
        selectInput("gpu_mon_interval", "Update every:", c(
          "5 seconds" = "5000",
          "30 seconds" = "30000",
          "1 minute" = "60000"
        ), selected = 30000, selectize = FALSE)
      ),
      
      mainPanel(width = 10,
        titlePanel(
          title = "GPU Status"
        ),
        h4(
          textOutput("gpu_nvidia", container = span)
        ),
        h4(
          textOutput("gpu_driver", container = span)
        ),
        div(style = 'overflow-x: scroll', tableOutput("gpu_stat_contents")),
        hr(),
        titlePanel(
          title = "GPU Processes"
        ),
        div(style = 'overflow-x: scroll', dataTableOutput("gpu_process_contents"))
      )
    )
  )
}

# Render ----
# Content of GPU monitor ====
gpu_monitor_time_render = function(session) {
  # Refresh every 0.1 secs
  renderText({
    invalidateLater(as.integer(100), session)
    return(format(Sys.time()))
  })
}

gpu_info_render = function(gpu_info, session) {
  renderText({
    return(gpu_info)
  })
}

gpu_monitor_stat_df_render = function(input, session) {
  renderTable({
    invalidateLater(as.integer(input$gpu_mon_interval), session)
    get_gpu_stat = gpu_process_monitor()
    return(get_gpu_stat$gpu_stat_df)
  })
}

gpu_monitor_process_df_render = function(input, output, session, gpu_process_df, refresh = F) {
  output$gpu_process_contents <- renderDataTable({
    # Timer to refresh this data table
    if(refresh) {
      invalidateLater(as.integer(input$gpu_mon_interval), session)
    }
    
    gpu_process_table <- generateGPUProcessTable()
    gpu_process_df$table <- gpu_process_table
    
    return(gpu_process_df$table)
  }, escape = F)
}
