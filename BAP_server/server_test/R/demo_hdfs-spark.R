Sys.setenv("HADOOP_CMD" = "/home/hadoop/hadoop/bin/hadoop")
Sys.setenv("HADOOP_STREAMING"="/home/hadoop/hadoop/share/hadoop/tools/lib/hadoop-streaming-2.8.1.jar")

library(rmr2)
library(plyrmr)
library(rhdfs)
hdfs.init()

# 將mtcars這個dataframe上傳至hdfs檔案處理系統
to.dfs(mtcars, "/home/shaomin/mtcars.csv", format = "csv")
# 從hdfs檔案處理系統上讀取檔案
from.dfs("/home/shaomin/mtcars.csv", format = "csv")
hdfs.ls("/home/shaomin")
hdfs.del("/home/shaomin/mtcars.csv")

#========================#
Sys.setenv("HADOOP_CMD" = "/home/hadoop/hadoop/bin/hadoop")
Sys.setenv("HADOOP_STREAMING"="/home/hadoop/hadoop/share/hadoop/tools/lib/hadoop-streaming-2.8.1.jar")
library("rhdfs")
hdfs.init()
# the following code are fixed setting
library(dplyr) 
library(sparklyr)
Sys.setenv(SPARK_HOME="/usr/local/spark/spark-2.1.0-bin-hadoop2.7/")
config <- spark_config()
config$spark.executor.memory = "32G"
config$spark.cores.max = "50"
config$spark.driver.memory = "16G" 
config$spark.yarn.executor.memoryOverhead = "4096"

# create a connection to Spark
sc <- spark_connect(master = "spark://hnamenode:7077", config = config)
hdfs.ls("/home/shaomin")
mySDF = spark_read_csv(sc, name = "mtcars", path = "hdfs:/home/shaomin/mtcars.csv", header = T)
# you can also move R dataframe to Spark.
movies_sdf = copy_to(sc, df = ggplot2movies::movies, name = "movies",overwrite = T)  

spark_disconnect(sc)
