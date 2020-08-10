visualize_main_ui = function() {
  tagList(
    sidebarLayout(
      sidebarPanel(width = 3,
        
        h4("Data selection"),
        shinyFilesButton('vis_data', 'File select', 'Please select a file', FALSE),
        textOutput("vis_filename"),
        
        selectInput('viz_type', 'Plot Type', c("Distribution" = "dist",
                                                "Density" = "density",
                                                "Scatter" = "scatter",
                                                "Surface" = "surface",
                                                "Line" = "line",
                                                "Bar" = "bar",
                                                "Box-plot" = "box")),
        conditionalPanel("input.viz_type == 'dist' | input.viz_type == 'density'",
          selectInput(
            inputId = "vis_xvar", label = "X-variable:",
            choices = c(),
            #selected = state_multiple("vis_xvar", vars, isolate(input$viz_yvar)),
            multiple = TRUE , size = 3, selectize = FALSE #, size = min(3, length(vars)), selectize = FALSE
          )
        ),
        
        conditionalPanel("input.viz_type == 'scatter' | input.viz_type == 'surface' | 
                         input.viz_type == 'line' | input.viz_type == 'bar' | input.viz_type == 'box'",
           selectInput(
             inputId = "vis_xvar2", label = "X-variable:",
             choices = c(),
             #selected = state_multiple("vis_xvar", vars, isolate(input$viz_yvar)),
             multiple = TRUE , size = 3, selectize = FALSE #, size = min(3, length(vars)), selectize = FALSE
           ),
           selectInput(
             inputId = "vis_yvar", label = "Y-variable:",
             choices = c(),
             #selected = state_multiple("vis_xvar", vars, isolate(input$viz_yvar)),
             multiple = TRUE , size = 3, selectize = FALSE #, size = min(3, length(vars)), selectize = FALSE
           )
        )
        
        
      ),
      mainPanel(width = 9,
        titlePanel("Data Visualize"),
        plotOutput("vis_result")
      )
    )
  )
}

visualize_result <- function(input, output, session, USER){
    renderPlot({
      #input$viz_run
      req(input$vis_data)
      
      file_info = parseFilePaths(USER$root, input$vis_data)
      file_path = as.character(file_info$datapath)
      print(file_path)
      if(!(identical(file_path,character(0)))) {
        dataset = read.csv(file_path)
        print(dataset)
        if(input$viz_type == 'dist'| input$viz_type =='density'){
          req(input$vis_xvar)
          print(input$vis_xvar)
          return(visualize(dataset = dataset, xvar = input$vis_xvar, type = input$viz_type))
        }
        else{
          req(input$vis_xvar2)
          req(input$vis_yvar)
          print(input$vis_xvar2)
          print(input$vis_yvar)
          return(visualize(dataset = dataset, xvar = input$vis_xvar2, yvar = input$vis_yvar, type = input$viz_type))
        }
      }
      
    })
}



updatevars <- function(input, output, session, user_info){
  renderText({
    req(input$vis_data)

    file_info <- parseFilePaths(user_info$root, input$vis_data)

    file_path <- as.character(file_info$datapath)
    file_name = as.character(file_info$name)
    
    if(!(identical(file_path,character(0)))) {
    
      df <- read.csv(file_path, header = T)
  
      updateSelectInput(session, "vis_xvar",
                        #label = colnames(df),
                        choices = colnames(df))
      updateSelectInput(session, "vis_xvar2",
                        #label = colnames(df),
                        choices = colnames(df))
      updateSelectInput(session, "vis_yvar",
                        #label = colnames(df),
                        choices = colnames(df))
      return(file_name)
    }
 })
}