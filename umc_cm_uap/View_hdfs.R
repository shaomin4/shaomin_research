# Setting Hadoop environment variables
Sys.setenv("HADOOP_CMD" = "/home/hadoop/hadoop/bin/hadoop");
Sys.setenv("HADOOP_STREAMING"="/home/hadoop/hadoop/share/hadoop/tools/lib/hadoop-streaming-3.0.0.jar")
# Load RHadoop packages
library(rhdfs)
library(rmr2)
library(plyrmr)
library(DT)

source("copy_file.R")


# ---- Render ----
hdfs_main_content <- function(){
  tagList(
    fluidPage(
      h2("The HDFS data"),
      fluidRow(
        actionButton("refresh_btn",label = "Refresh"),
        column(12, DT::dataTableOutput('hdfs_table')),
        column(5, verbatimTextOutput('noDataMsg'))
      ),
      column(5, verbatimTextOutput('mySelect')),
      actionButton( 'next_btn',     label ="Next"),
      actionButton( 'pre_btn',    label = "Previous"),
      hr(),
      actionButton( 'delete_btn', label = "Delete"),
      actionButton( 'rename_btn', label = "Rename"),
      actionButton( 'move_btn',   label = "Move"),
      actionButton( 'copy_btn',   label = "Copy"),
      actionButton( 'mkdir_btn',  label = "Make a directory")
    )
  )
}


view_hdfs_preview <- function(input, output,session,USER) {
  
  # ---- Set varialbes ----
  hdfs_table <- reactiveValues(value = NULL) # for reload datatable
  selectedPath <- reactiveValues(value = NULL) # for choose path
  Path <- reactiveValues(oldpath = NULL, goback = NULL, originalPath = NULL, movepath = NULL, 
                         copypath = NULL,copy2path = NULL, path = NULL) # for choose path
  rename <- reactiveValues(name = NULL, directory = NULL) # rename$name for rename,rename$directory for mkdir
  
  # ---- Initialize HDFS ----
  hdfs.init()
  
  # ---- For reload dataframe ----   
  observe({
    req(USER$Username)
    username = USER$Username
    path = paste("/home", username, sep = "/")
    hdfs_df = hdfs.ls(path)
    Path$path = path
    if(!is.null(hdfs_df)){
      # if hdfs_table$value changed, dataframe will reload. hdfs_table$value is a path to show the list of folder.
      output$hdfs_table = DT::renderDataTable({
        if(is.null(hdfs_table$value)){
          hdfs_table$value <- hdfs_df
          hdfs_df
        }else{
          hdfs_table$value
        }
      }, selection = 'single')
    }else{
      output$noDataMsg = renderPrint({
        cat("No data available in table")
      })
    }
  })
  
  # ---- For refresh button ----
  observeEvent(input$refresh_btn, {
    Path$oldpath = NULL
    Path$goback = NULL
    Path$originalPath = NULL
    Path$movepath = NULL
    Path$copypath = NULL
    Path$copy2path = NULL
    hdfs_table$value <- hdfs.ls(Path$path)
  })
  
  # ---- For output selected path ----
  # Print the selected index
  selection = reactive ({
    input$hdfs_table_rows_selected
  })
  # Show the selected path
  output$mySelect = renderPrint({
    req(selection())
    if (length(selection())) {
      cat("path:")
      selectedPath$value <- hdfs_table$value[input$hdfs_table_rows_selected, 6]
      selectedPath <- selectedPath$value
      cat(selectedPath, sep = "&")
    }
  })
  
  # ---- For next button ----
  observeEvent(input$next_btn, {
    if (length(selection())) {
      Path$goback <- Path$oldpath 
      newHdfs = hdfs.ls(selectedPath$value)
      hdfs_table$value <- newHdfs
      Path$oldpath <- selectedPath$value
    }
    else{
      Path$oldpath <- Path$path
      hdfs_table$value <- hdfs.ls(Path$oldpath)
    }
  })
  
  # ---- For previous button ----
  observeEvent(input$pre_btn, {
    Path$oldpath <- Path$goback
    newHdfs = hdfs.ls(Path$goback)
    hdfs_table$value <- newHdfs
    Path$goback <- Path$path #if you want to go back twice, will go back to home directory now
  })
  
  # ---- For delete button ----
  observeEvent(input$delete_btn, {
    if (length(selection())) {
      #Change by shaomin on 11/7#
      showNotification("Start to delete file.", type = "default")
      hdfs_del_cmd = del_hdfs_file(file = selectedPath$value,user = USER$Username)
      if(length(hdfs_del_cmd)==0){
        showNotification("Successfully delete!", type = "message")
      }else{
        showNotification(hdfs_del_cmd, type = "warning")
      }
      if(is.null(Path$oldpath)){
        Path$oldpath <- Path$path
        Path$goback <- Path$path
        newHdfs = hdfs.ls(Path$path)
      }else{
        newHdfs = hdfs.ls(Path$oldpath)
      }
      hdfs_table$value <- newHdfs
    }else{
      showModal(modalDialog(
        "Please select a file to delete!",
        easyClose = TRUE
      ))
    }
  })
  
  # ---- For rename button ----
  observeEvent(input$rename_btn, {
    if(length(selection())){
      showModal(modalDialog(
        "Please enter a new file name !",
        textInput("text",label = "Enter name"),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok", "OK")
        )
      ))
      #hdfs.rename(selectedPath$value)
    }else{
      showModal(modalDialog(
        "Please select a file to rename!",
        easyClose = TRUE
      ))
    }
  })
  #If press ok ,than rename a file name from selecvPath$path change to newfileName pathe
  observeEvent(input$ok, {
    rename$name <-  input$text
    removeModal()
    if(is.null(Path$oldpath)){
      Path$oldpath <- Path$path
      newfileName = paste(Path$oldpath,rename$name,sep="/")
    }else{
      newfileName = paste(Path$oldpath,rename$name,sep="/")
    }
    #Change by shaomin on 11/7#
    showNotification("Start to rename.", type = "default")
    hdfs_rename_cmd = rename_hdfs_file(from = selectedPath$value,newfileName,user = USER$Username)
    if(length(hdfs_rename_cmd)==0){
      showNotification("Successfully Rename File!", type = "message")
    }else{
      showNotification(hdfs_rename_cmd, type = "warning")
    }
    if(is.null(Path$oldpath)){
      newHdfs = hdfs.ls(Path$path)
    }else{
      newHdfs = hdfs.ls(Path$oldpath)
    }
    hdfs_table$value <- newHdfs
  })
  
  # ---- For move button ----
  observeEvent(input$move_btn, {
    if(length(selection())){
      if(!is.null(Path$originalPath)){
        Path$movepath = selectedPath$value
        movelist = paste("Are you sure want to move '",Path$originalPath,"' to '",Path$movepath,"' ?",sep=" ")
        showModal(modalDialog(
          movelist,
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton("move_ok", "OK")
          )
        ))
      }else{
        Path$originalPath <- selectedPath$value
        movelist = paste("Your choose is'",Path$originalPath,"'")
        showModal(modalDialog(
          movelist,
          h4("Now please choose a directory where you want to moved."),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Ok")
          )
        ))
      }
    }else{
      if(!is.null(Path$originalPath)&&is.null(Path$movepath)){
        if(is.null(Path$oldpath)){Path$oldpath <- Path$path}
        Path$movepath = Path$oldpath
        movelist = paste("Are you sure want to move '",Path$originalPath,"' to '",Path$movepath,"' ?",sep=" ")
        showModal(modalDialog(
          movelist,
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton("move_ok", "OK")
          )
        ))
      }else{
        showModal(modalDialog(
          "Please select a file!",
          easyClose = TRUE
        ))
      }
    }
  })
  #The button for check move file
  observeEvent(input$move_ok, {
    removeModal()
    if(!is.null(Path$originalPath)&&!is.null(Path$movepath)){
      #Change by shaomin on 11/7#
      showNotification("Start to move file.", type = "default")
      hdfs_move_cmd = move_hdfs_file(from = Path$originalPath,to = Path$movepath,user = USER$Username)
      if(length(hdfs_move_cmd)==0){
        showNotification("Successfully Move File!", type = "message")
      }else{
        showNotification(hdfs_move_cmd, type = "warning")
      }
      if(is.null(Path$oldpath)){
        Path$oldpath <- Path$path
        Path$goback <- Path$path
        newHdfs = hdfs.ls(Path$path)
      }else{
        newHdfs = hdfs.ls(Path$oldpath)
      }
      hdfs_table$value <- newHdfs
      #clear the move path recard 
      Path$originalPath=NULL
      Path$movepath=NULL
    }else{
      showModal(modalDialog(
        "Please select a file!",
        easyClose = TRUE
      ))
    }
  })
  
  
  # ---- For copy button ----
  observeEvent(input$copy_btn, {
    if(length(selection())){
      if(!is.null(Path$copypath)){
        Path$copy2path = selectedPath$value
        copylist = paste("Are you sure want to copy '",Path$copypath,"' to '",Path$copy2path,"' ?",sep=" ")
        showModal(modalDialog(
          copylist,
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton("copy_ok", "OK")
          )
        ))
      }else{
        Path$copypath <- selectedPath$value
        copylist = paste("Your choose is'",Path$copypath,"'")
        showModal(modalDialog(
          copylist,
          h4("Now please choose a directory where you want to paste."),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Ok")
          )
        ))
      }
    }else{
      if(!is.null(Path$copypath)&&is.null(Path$copy2path)){
        if(is.null(Path$oldpath)){Path$oldpath <- Path$path}
        Path$copy2path = Path$oldpath
        copylist = paste("Are you sure want to copy '",Path$copypath,"' to '",Path$copy2path,"' ?",sep=" ")
        showModal(modalDialog(
          copylist,
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton("copy_ok", "OK")
          )
        ))
      }else{
        showModal(modalDialog(
          "Please select a file!",
          easyClose = TRUE
        ))
      }
    }
  })
  #The button for check copy file
  observeEvent(input$copy_ok, {
    removeModal()
    if(!is.null(Path$copypath)&&!is.null(Path$copy2path)){
      #Change by shaomin on 11/7#
      showNotification("Start to cope file.", type = "default")
      hdfs_copy_cmd = copy_hdfs_file(from = Path$copypath,to = Path$copy2path,user = USER$Username)
      if(length(hdfs_copy_cmd) == 0){
        showNotification("Successfully Copy File!", type = "message")
      }else{
        showNotification(hdfs_copy_cmd, type = "warning")
      }
      if(is.null(Path$oldpath)){
        Path$oldpath <- Path$path
        Path$goback <- Path$path
        newHdfs = hdfs.ls(Path$path)
      }else{
        newHdfs = hdfs.ls(Path$oldpath)
      }
      hdfs_table$value <- newHdfs
      Path$copypath = NULL
      Path$copy2path = NULL
    }else{
      showModal(modalDialog(
        "Please select a file!",
        easyClose = TRUE
      ))
    }
    
  })
  
  # ---- For mkdir button ----
  observeEvent(input$mkdir_btn, {
    showModal(modalDialog(
      "Please enter a directory name !",
      textInput("mkdir_text",label = "Enter name"),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("mkdir_ok", "OK")
      )
    ))
  })
  #if press ok, than get direcotry name and make it to hdfs.
  observeEvent(input$mkdir_ok, {
    rename$directory <- input$mkdir_text
    removeModal()
    if(is.null(Path$oldpath)){
      Path$oldpath <- Path$path
      mkdirPath = paste(Path$oldpath,rename$directory,sep="/")
    }else{
      mkdirPath = paste(Path$oldpath,rename$directory,sep="/")
    }
    #hdfs_mkdir_cmd = system(paste("/home/hadoop/hadoop/bin/hdfs dfs -mkdir",mkdirPath,"2>&1"),intern = T)
    #Change by shaomin on 11/7#
    showNotification("Start to make a directory!", type = "default")
    hdfs_mkdir_cmd = mkdir_hdfs_file(file = mkdirPath,user = USER$Username)
    if(length(hdfs_mkdir_cmd)==0){
      showNotification("Successfully make a directory!", type = "message")
    }else{
      showNotification(hdfs_mkdir_cmd, type = "warning")
    }
    if(is.null(Path$oldpath)){
      newHdfs = hdfs.ls(Path$path)
    }else{
      newHdfs = hdfs.ls(Path$oldpath)
    }
    hdfs_table$value <- newHdfs
  })
}