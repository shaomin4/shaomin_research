library(listviewer)
library(rjson)

json_main_content <- function() {
  tagList(
    fluidPage(
      jsoneditOutput( "jsed" )
    ))
}

json_demo_observe <- function(input, output, session)
observe({
  js_dat <- fromJSON(paste(readLines("http://api.worldbank.org/country?per_page=10&region=OED&lendingtype=LNX&format=json"), collapse=""))
  output$jsed <- renderJsonedit({
    jsonedit(
      as.list( js_dat )
      ,"change" = htmlwidgets::JS('function(){
                                  console.log( event.currentTarget.parentNode.editor.get() )
                                  }')
                                    )
    })
})