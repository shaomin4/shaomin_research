library(mailR)
source("process_monitor.R")
source("kill_process.R")

# SubFunction ----
generateProcessTable <- function() {
  # Get process status data frame
  process_table = process_monitor()
  # Generate kill buttons
  kill_action_btns <- Map(x = process_table$PID, 
                          f = function(x) {
                            shinyInput(actionButton, paste0('button_', x), 
                                       label = "Kill", 
                                       onclick = 'Shiny.onInputChange(\"select_button\",  this.id)')
                          })
  # Unlist object which return from Map function
  kill_action_btns <- unlist(kill_action_btns)
  process_table <- cbind(process_table, Kill = kill_action_btns)
  rownames(process_table) <- process_table$PID
  return(process_table)
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
      actionButton("cancel_btn", "Cancel"),
      actionButton("kill_btn", "Just Kill"),
      actionButton("kill_n_send_email_btn", "Send Email and Kill")
    )
  )
}

# Observer ----
## Observe kill button in process list
observe_kill <- function(input, output, PID) {
  observeEvent(input$select_button, {
    # Get pid
    pid <- strsplit(input$select_button, "_")[[1]][2]
    PID$pid <- pid
    # Get process table
    process_table <- process_monitor()
    # Find the process infomation in process table
    idx <- which(process_table$PID == pid)
    process_info <- process_table[idx,]
    # Get owner of the process
    process_owner <- process_info$USER
    PID$process_owner <- process_owner
    # Get owner's email
    user_sys_info <- system(paste("cat /etc/passwd | grep", process_owner), 
                            intern = TRUE)
    user_email <- strsplit(x = strsplit(x = user_sys_info, split = ":")[[1]][5],
                           split = "_")[[1]][1]
    # Temp email
    output$selected_process_info <- renderDataTable(process_info)
    showModal(killModal(email = user_email))
  })
}

observe_kill_cancel_action <- function(input, output, session, process_df, USER) {
  observeEvent(input$cancel_btn, {
    removeModal()
    refresh_tsk_monitor_df_render(input, output, session, process_df, refresh = T, user_name = USER$Username)
  })
}

## Observe kill confirm button in modal
observe_kill_action <- function(input, output, session, process_df, PID, kill_observer, USER) {
  observeEvent(input$kill_btn, {
    # Get pid
    pid <- PID$pid
    
    kill_process_result = kill_process(pid, USER$Username, PID$process_owner)
    
    if (kill_process_result) { 
      ## Successfully killed
      removeModal()
      ## Show alert message
      showNotification(paste("Successfully killed process which pid is", pid),
                       type = "message")
      # Empty PID
      PID$pid <- NULL
    } else {
      ## Unsuccessfully killed
      ## Show alert message
      showNotification(paste("UNSUCCESSFULLY killed process which pid is", pid),
                       type = "error")
    }
    
    refresh_tsk_monitor_df_render(input, output, session, process_df, refresh = T, user_name = USER$Username)
  })
}

## Observe kill and send email confirm button in modal
observe_kil_n_send_email_l_action <- function(input, output, session, process_df, PID, kill_observer, USER) {
  observeEvent(input$kill_n_send_email_btn, {
    # Get pid
    pid <- PID$pid
    # If killing process successfully, it would return "1" and "0"
    # If not, it would return only "0"
    kill_process_result = kill_process(pid, USER$Username, PID$process_owner)
    # kill_process_execution <- system(paste("sudo runuser -l ", PID$process_owner, " -c 'kill ", pid, "'"), 
    #                                  intern = T)
    # kill_process_result <- system("echo $?", intern = T)
    if (kill_process_result) { 
      ## Successfully killed
      removeModal()
      ## Show alert message
      showNotification(paste("Successfully killed process which pid is", pid, sep=" "),
                       type = "message")
      # Send Eamil to process owner
      send.mail(from = "datasci.lab.nsysu@gmail.com",  # 寄件者
                #收件者
                to = c(input$process_owner_email),
                #標題
                subject = "NSYSU Analytics : Sorry for killing your process",
                #內文
                body = input$kill_message, 
                encoding = "utf-8",  # 編碼
                #夾帶檔案
                # attach.files = c("attach.xlsx"),
                smtp = list(host.name = "smtp.gmail.com", 
                            port = 465, 
                            user.name = "datasci.lab.nsysu@gmail.com", 
                            passwd = "thisisbowenbowen", 
                            ssl = TRUE),
                authenticate = TRUE,
                send = TRUE)
      # Empty PID
      PID$pid <- NULL
    } else {
      ## Unsuccessfully killed
      ## Show alert message
      showNotification(paste("UNSUCCESSFULLY killed process which pid is", pid, sep=" "),
                       type = "error")
    }
    
    refresh_tsk_monitor_df_render(input, output, session, process_df, refresh = T, user_name = USER$Username)
  })
}

# Render ----
# Content of task monitor ====
task_monitor_main_content <- function() {
  tagList(
    sidebarLayout(
      sidebarPanel(width = 2,
        h4(
          textOutput("currentTime", container = span)
        ),
        selectInput("tsk_mon_interval", "Update every:", c(
          "5 seconds" = "5000",
          "30 seconds" = "30000",
          "1 minute" = "60000"
        ), selected = 30000, selectize = FALSE)
      ),
      
      mainPanel(width = 10,
        div(style = 'overflow-x: scroll', dataTableOutput("tsk_df_contents"))
      )
    )
  )
}

tsk_monitor_time_render <- function(session) {
  # Refresh every 0.1 secs
  renderText({
    invalidateLater(as.integer(100), session)
    return(format(Sys.time()))
  })
}

refresh_tsk_monitor_df_render <- function(input, output, session, process_df, refresh = F, user_name = "") {
  dt = renderDataTable(
          expr = {
            # Timer to refresh this data table
            if(refresh) {
              invalidateLater(as.integer(input$tsk_mon_interval), session)
            }
            
            process_table <- generateProcessTable()
            process_df$table <- process_table
            
            return(process_df$table)}, 
          escape = FALSE,
          options = list(search = list(regex = TRUE, caseInsensitive = FALSE, search = user_name)))
  output$tsk_df_contents <- dt
}

shinyInput <- function(FUN, id, ...) {
  input <- as.character(FUN(id, ...))
  return(input)
}