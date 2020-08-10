source("authenticate.R")

dataModal <- function() {
  modalDialog(
    fluidRow(
      column(6, offset=3, 
             textInput("username", "Username:"),
             passwordInput("password", "Password:")
      )),
    footer = tagList(
      actionButton("login_btn", "Login")
    )
  )
}


# Show modal when button is clicked.
# This `observe` is suspended only whith right user credential 
# Observe login modal ----
observe_login =  function() {
  observe({
    showModal(dataModal())
  })
}

observe_login_act = function(input, USER, login_observer) {
  # When login button is pressed, attempt to authenticate. If successful,
  # remove the modal.
  # Observe login action ----
  observeEvent(input$login_btn, {
    
    isolate({
      Username <- input$username
      Password <- input$password
    })
    
    auth_result = authenticate(Username, Password)
    
    if(auth_result) {
      # Set up USER information
      USER$authenticated = TRUE
      USER$Username = Username
      # Stop observe login
      login_observer$suspend()
      # Remove login modal
      removeModal()
      # Set up file chooser
      USER$root = c(wd = paste('/home', USER$Username, sep = "/"))
      shinyFileChoose(input, 'import_file',
                      roots = USER$root,
                      filetypes=c('', 'csv'))
      shinyFileChoose(input, 'vis_data',
                      roots = USER$root,
                      filetypes=c('', 'csv'))
    } else {
      # Maybe need to show some error message
      showNotification("Authenticate failed", type = "error")
      USER$authenticated = FALSE
    }
  })
}