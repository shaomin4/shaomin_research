library(shiny)


my_username <- "test"
my_password <- "test"

#login page
login = renderUI({
  tagList(
    #h2("Hello", align = "center"),
    div(
      id = "login",
      wellPanel(
        textInput("userName", "Username"),
        passwordInput("passwd", "Password"),
        br(), actionButton("Login", "Log in")
      )
    ),
    tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )
})

#main page
main = renderUI({
  tagList(
    tabPanel(""),
    pageWithSidebar(
      headerPanel(
        ""
      ),
      sidebarPanel(
        actionButton("logout", "Logout")
      ),
      mainPanel(tableOutput("aaa"))
    )
  )
})

# declare a place for main page
ui <- fluidPage(
  uiOutput("ui")
)

server <- function(input, output, session) {
  
  USER <- reactiveValues(Logged = FALSE)
  # if logged=T then login to main page
  observe({
    if (USER$Logged == FALSE) {
      output$ui <- login
    } else if (USER$Logged == TRUE) {
      output$ui <- main
    }
  })
  
  #login logic
  observeEvent(input$Login, {
    if (!is.null(input$Login)) {
      if (input$Login > 0) {
        Username <- isolate(input$userName)
        Password <- isolate(input$passwd)
        Id.username <- which(my_username == Username)
        Id.password <- which(my_password == Password)
        if (length(Id.username) > 0 & length(Id.password) > 0) {
          if (Id.username == Id.password) {
            USER$Logged <- TRUE
          } 
        }
      } 
    }
  })
  
  #log out button(onclick)
  observeEvent(input$logout, {
    USER$Logged <- FALSE
  })
}

shinyApp(ui, server)