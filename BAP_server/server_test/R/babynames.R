rm(list = ls(all=TRUE))
library(dplyr)               # load before sparklyr
library(sparklyr)   
library(tidyr)
library(ggplot2)
library(dygraphs)
library(rbokeh)
library(RColorBrewer)

# Path of HDFS
path = "hdfs://192.168.1.100:9000/home/sample/"  

# Spark Context
Sys.setenv(SPARK_HOME="/usr/local/spark/spark-2.1.0-bin-hadoop2.7/")
config <- spark_config()
# config$spark.ui.port = "4043"
config$spark.executor.memory = "4G"
config$spark.driver.memory = "4G" 
config$spark.yarn.executor.memoryOverhead = "4096"
sc <- spark_connect(master = "spark://hnamenode:7077", config = config)

###################################
# Read CSV file from HDFS
###################################
baby = spark_read_csv(sc, "babynames", paste0(path, "babynames.csv"))
tbl_cache(sc, "babynames")
count(baby)

###################################
# Example 1: Aggregate & Spread
###################################
baby %>% 
  group_by(year, sex) %>% 
  summarise(millions = sum(n)/1000000) %>% 
  collect %>% 
  spread(sex, millions) %>% 
  dygraph(main="No. New Borns") %>%
  dySeries("F") %>% dySeries("M") %>%
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20)

######################################
# Example 2: Window Function & Join
######################################
baby %>%
  group_by(year,sex) %>%
  filter(min_rank(desc(n)) == 1 & sex =="M") %>%
  ungroup %>% 
  select(sex, name) %>%
  distinct %>% 
  inner_join(baby) %>% 
  select(year, name, prop)%>% 
  collect %>% 
  mutate(prop = ifelse(prop < 0.005, NA, 100*prop)) %>% 
  spread(name, prop) %>% 
  dygraph("Trend of the Most Popular Names") %>% dyRangeSelector %>%
  dyLegend("auto", labelsSeparateLines=T, width=120) %>%
  dyOptions(strokeWidth=2, colors=brewer.pal(8, "Set2")) %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesBackgroundAlpha = 0.3)

# PLEASE Always Rememeber to Disconnect !!!
spark_disconnect(sc)

