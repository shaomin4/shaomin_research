# Stress Test (2019-08-10, tonychuo)
Sys.setenv(SPARK_HOME="/usr/local/spark/spark-2.1.0-bin-hadoop2.7")
library(magrittr)
library(SparkR, lib.loc=c(file.path(Sys.getenv("SPARK_HOME"),"R","lib")))

# hadoop file paths
json = "hdfs:/home/tonychuo/yelp13/json/"
pq = "hdfs:/home/tonychuo/yelp13/pq/"

# open spark session
sparkR.session(master = "spark://hnamenode:7077", sparkConfig = list(
  spark.ui.port = "4043",
  spark.driver.memory = "32G",
  spark.executor.memory = "8G",
  spark.yarn.executor.memoryOverhead = "4096"
  ))

# read review texts
zT = read.parquet(paste0(pq, "zR")) %>% select(c("rid", "txt"))

# repartition for parallel processing
zTX = repartition(zT, numPartitions=800, col=zT$rid)

# define the data structure for sentiment analysis
schema = structType(
  structField("rid", "integer"), 
  structField("anger", "integer"), 
  structField("anticipation", "integer"), 
  structField("disgust", "integer"), 
  structField("fear", "integer"), 
  structField("joy", "integer"), 
  structField("sadness", "integer"), 
  structField("surprise", "integer"), 
  structField("trust", "integer"), 
  structField("negative", "integer"), 
  structField("positive", "integer")
  )

# sentiment analysis in parallel
t0 = Sys.time()
zSenti = dapply(zTX, function(df) {
  library(syuzhet)
  dx = data.frame(rid=df$rid,get_nrc_sentiment(df$txt)) 
  dx[2:11] = lapply(dx[2:11], as.integer)
  dx
}, schema)
cache(zSenti)       
count(zSenti)      # 6,685,900
Sys.time() - t0    # 2.410581 mins

head(zSenti,5)
#       rid anger anticipation disgust fear joy sadness surprise trust negative positive
# 1 3820248     0            1       0    2   0       1        0     1        2        2
# 2 3820905     3            3       3    2   2       2        3     3        4        3
# 3 3821956     5            2       4    3   3       4        1     2        5        8
# 4 3822162     3            4       1    2   5       5        2     6        8       16
# 5 3823354     0            1       0    0   3       0        1     3        0        6

# over-wriet the resultant parque if needed
# zSenti = repartition(zSenti, numPartitions=16)
# write.parquet(zSenti, paste0(pq, "zSenti"), mode="overwrite")

# remeber to stop the session when you're done !!!
sparkR.session.stop() 

