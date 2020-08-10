library(shiny)
library(shinyjs)
library(shinydashboard)
source("task_monitor.R")
source("upload_files.R")
source("import_cluster.R")
source("data_visualize.R")
source("cluster_status.R")
source("gpu_monitor.R")
source("spark_monitor.R")
source("View_hdfs.R")
source("greenplum_viewer.R")
source("json_viewer.R")
# source("model_viewer.R")
# source("predict_viewer.R")
sidebar_content = function() {
  tagList(
    menuItem("Data Management", icon = icon("upload"), startExpanded = FALSE,
             menuSubItem("Upload Data",tabName = "ufl"),
             menuSubItem("Visualize Data",tabName = "vis"),
             menuSubItem("Import to Cluster",tabName = "i2c")),
    menuItem("Cluster Management", icon = icon("database"), startExpanded = FALSE,
             menuSubItem("Greenplum", tabName = "vgp"),
             menuSubItem("HDFS", tabName = "vhdfs")),
    menuItem("Monitor", icon = icon("bar-chart-o"), startExpanded = FALSE,
             menuSubItem("Process Status", tabName = "tsk"),
             menuSubItem("Cluster Status", tabName = "cs"),
             menuSubItem("GPU Status", tabName = "gpu"),
             menuSubItem("Spark Status", tabName = "spark"))
    )
}

tabitems_content = function() {
  tagList(
    tabItems(
      # Upload from local ----
      tabItem(
        tabName = "ufl",
        fluidRow(
          column(width = 12,
                 ufl_main_content()
          )
        )
      ), # End of tabItem
      
      # Import to cluster ----
      tabItem(
        tabName = "i2c",
        fluidRow(
          column(width = 12,
                 i2c_main_content()
          )
        )
      ), # End of tabItem
      
      # visualize ----
      tabItem(
        tabName = "vis",
        fluidRow(
          column(width = 12,
                 visualize_main_ui()
          )
        )
      ), # End of tabItem
      
      # View greenplum
      tabItem(
        tabName = "vgp",
        
        fluidRow(
          column(width = 12,
                 vgp_main_content()
          )
        )
        
      ), # End of tabItem
      
      #Management HDFS
      tabItem(
        tabName = "vhdfs",
        fluidRow(
          column(width = 12,
                 hdfs_main_content()
          )
        )
      ), # End of tabItem
      
      # Task monitor
      tabItem(
        tabName = "tsk",
        fluidRow(
          column(width = 12,
                 task_monitor_main_content()
          )
        )
        
      ), # End of tabItem
      
      # Cluster status
      tabItem(
        tabName = "cs",
        fluidRow(
          column(width = 12,
                 cs_main_content()
          )
        )
      ),
      
      # GPU monitor
      tabItem(
        tabName = "gpu",
        fluidRow(
          column(width = 12,
                 gpu_monitor_main_content()
          )
        )
      ), # End of tabItem
      
      # GPU monitor
      tabItem(
        tabName = "spark",
        fluidRow(
          column(width = 12,
                 spark_monitor_main_content()
          )
        )
      ) # End of tabItem
    )
  )
}
