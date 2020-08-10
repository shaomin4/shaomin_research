library(RCurl)
source("copy_file.R")
source("save_file.R")

# Observer ----
observe_upload_type_checking <- function(input, USER) {
  
  observe({
    
    req(input$ufl_upload_type)
    
    useShinyjs()
    
    if(input$ufl_upload_type == "none") {
      disable("urf_upload_file")
      disable("urf_url")
    } else if(input$ufl_upload_type == "from_file") {
      enable("urf_upload_file")
      disable("urf_url")
    } else if(input$ufl_upload_type == "from_url") {
      disable("urf_upload_file")
      enable("urf_url")
    }
    
  })
  
}

observe_url_user_defined_delimiter <- function(input) {
  
  observe({
    
    req(input$ufl_sep)
    
    useShinyjs()
    
    if(input$ufl_sep == "userDefined") {
      enable("ufl_user_defined_delimiter")
    } else {
      disable("ufl_user_defined_delimiter")
    }
    
  })
}

observe_upload_act <- function(input, USER) {
  observeEvent(input$upload_local_btn, {
    
    req(input$ufl_upload_type)
    
    if (input$ufl_upload_type == "from_file") {
      # Upload from local file
      req(input$urf_upload_file)
      file_name <- input$urf_upload_file$name
      file_tmp_path <- input$urf_upload_file$datapath
      file_new_path <- paste(USER$root, file_name, sep="/")
      copy_file(from <- file_tmp_path, to = file_new_path, USER$Username)
    } else if(input$ufl_upload_type == "from_url") {
      # Upload from URL
      req(input$urf_url)
      # Get URL from text input
      isolate({
        url <- input$urf_url
        })
      # Generate file name and path
      url_token <- strsplit(url, split = "/")[[1]]
      file_name <- url_token[length(url_token)]
      file_new_path <- paste(USER$root, file_name, sep="/")
      # Download CSV from URl
      save_file(url = url, user = USER$Username, path = file_new_path)
    }
    
    showNotification(paste("Successfully Upload", file_name, "\n", 
                           "The path is", file_new_path, "now", sep=" "),
                     type = "message")
    
  })
} 


observe_upload_json_act <- function(input, USER) {
  observeEvent(input$upload_json_btn, {
      # Upload from local file
      req(input$load_json_file)
      file_name <- input$load_json_file$name
      file_tmp_path <- input$load_json_file$datapath
      file_new_path <- paste(USER$root, file_name, sep="/")
      copy_file(from <- file_tmp_path, to = file_new_path, USER$Username)
    
    
    showNotification(paste("Successfully Upload", file_name, "\n", 
                           "The path is", file_new_path, "now", sep=" "),
                     type = "message")
    
  })
} 

# Render ----
# Content of upload from local ====
ufl_main_content <- function() {
  tagList(
    sidebarLayout(
        sidebarPanel(width = 3,
          textOutput('text1'),
          tags$head(tags$style("#text1{color: red;
                                 font-size: 16px;
                                 font-style: italic;
                                 }"
                              )
                    ),
          selectInput("ufl_file_type", "Upload file type", 
                      choices = c("CSV" = "csv",
                                  "JSON" = "json"),
                      selected = "csv"),
          
          
          conditionalPanel(condition = "input.ufl_file_type == 'csv'", 
                           # Control uploaded data source
                           radioButtons("ufl_upload_type", "Upload from",
                                        choices = c("From local" = "from_file",
                                                    "From URL" = "from_url"),
                                        selected = "from_file"),
                           # call useShinyjs() before using disable and enable
                           useShinyjs(),
                           # Upload from local file
                           disabled(
                             fileInput("urf_upload_file", "Choose CSV file",
                                       multiple = FALSE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv"))
                           ),
                           # Upload from URL link
                           disabled(
                             textInput("urf_url", "Enter URL" , value = "", width = NULL, placeholder = "Enter URL")
                           ),
                           
                           
                           # Horizontal line ----
                           hr(),
                           
                           # Table setting ----
                           # Input: Checkbox if file has header ----
                           checkboxInput("ufl_header", "Header", TRUE),  
                           # Input: Select separator ----
                           radioButtons("ufl_sep", "Delimiter",
                                        choices = c(Comma = ",",
                                                    Semicolon = ";",
                                                    Tab = "\t",
                                                    UserDefined = "userDefined"),
                                        selected = ","),
                           disabled(
                             textInput(inputId = "ufl_user_defined_delimiter", label = "User defined delimiter")
                           ),
                           # Input: Select quotes ----
                           radioButtons("ufl_quote", "Quote",
                                        choices = c(None = "",
                                                    "Double Quote" = '"',
                                                    "Single Quote" = "'"),
                                        selected = '"'),
                           # Input: Select number of rows to display ----
                           radioButtons("ufl_disp", "Display",
                                        choices = c(Head = "head",
                                                    All = "all"),
                                        selected = "head"),
                           hr(),
                           actionButton("upload_local_btn", "Upload")
                           
                           ),
                     
          conditionalPanel(condition = "input.ufl_file_type == 'json'",
                           fileInput("load_json_file", "Choose JSON file", 
                                     accept = c(
                                       "application/json",
                                       "text/comma-separated-values,text/plain",
                                       ".json")
                           ),
                           hr(),
                           actionButton("upload_json_btn", "Upload"),
                           renderPrint({
                             print("JSON");})
                           )
          
          
        ),
        mainPanel(width = 8,
          titlePanel("Data Preview"),
          conditionalPanel(condition = "input.ufl_file_type == 'csv'",
              div(style = 'overflow-x: scroll', dataTableOutput("ufl_df_content"))
          ),
          conditionalPanel(condition = "input.ufl_file_type == 'json'",
              jsoneditOutput("json_output")
          )
        )
      )
    )
}

# json input 
json_observe <- function(input, output, session)
  observe({
    req(input$load_json_file)
    inFile <- input$load_json_file$datapath
    js_dat <- fromJSON(paste(readLines(inFile), collapse=""))
    output$json_output <- renderJsonedit({
      jsonedit(
        as.list( js_dat )
        ,"change" = htmlwidgets::JS('function(){
                                  console.log( event.currentTarget.parentNode.editor.get() )
                                  }')
      )
    })
  })


# File uploaded from local previewer ====
ufl_df_preview = function(input) {
  renderDataTable({
    # input$urf_upload_file will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$ufl_upload_type)
    
    if (input$ufl_upload_type == "from_file") {
      # Upload from local file
      req(input$urf_upload_file)
      
      file_tmp_path <- input$urf_upload_file$datapath
    } else if(input$ufl_upload_type == "from_url") {
      # Upload from URL
      req(input$urf_url)
      
      showNotification("Reading CSV from URL...",
                       type = "default")
      
      isolate({
        file_tmp_path <- input$urf_url
      })
    }
    
    if (input$ufl_sep == "userDefined") {
      delimiter <- input$ufl_user_defined_delimiter
    } else {
      delimiter <- input$ufl_sep
    }
    
    df <- read.csv(file_tmp_path,
                   header = input$ufl_header,
                   sep = delimiter,
                   quote = input$ufl_quote)
    
    if (input$ufl_disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
} 