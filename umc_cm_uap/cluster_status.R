# Render ----
# Content of upload from local ====
cs_main_content = function() {
  tagList(
    mainPanel(
      tags$iframe(src = "http://192.168.1.5:3838/uap/perfbar.html",height = 400,width=650
                  ,frameborder=0, seamless="seamless" )
    )
  )
}