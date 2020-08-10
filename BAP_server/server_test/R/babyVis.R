# the following code are fixed setting
Sys.setenv("HADOOP_CMD" = "/home/hadoop/hadoop/bin/hadoop")
Sys.setenv("HADOOP_STREAMING"="/home/hadoop/hadoop/share/hadoop/tools/lib/hadoop-streaming-2.8.1.jar")
library("rhdfs")
hdfs.init()

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

data_file_1 = "hdfs:/home/sample/babynames.csv"
# 透過spark_read_csv 至 hdfs上 讀取babynames 資料集
baby = spark_read_csv(sc, "babynames", data_file_1)
# babynames 資料集總共有一百八十多萬筆  
baby %>% summarise(n = n()) 

# 設定起始時間
start_time <- proc.time()
baby %>% 
  summarise(n_year = n_distinct(year),
            n_sex = n_distinct(sex),
            n_name = n_distinct(name),
            n_n = n_distinct(n),
            n_prop = n_distinct(prop)) %>% 
  data.frame
#   n_year n_sex  n_name  n_n   n_prop
#     136    2    95025  13604  162480

# 用程式執行完的時間減去起始時間
proc.time() - start_time
#   user   system  elapsed  
#   0.059   0.025   0.769  

# 設定起始時間
start_time <- proc.time()
babynames::babynames %>% 
  summarise(n_year = n_distinct(year),
            n_sex = n_distinct(sex),
            n_name = n_distinct(name),
            n_n = n_distinct(n),
            n_prop = n_distinct(prop)) %>%
            data.frame
#   n_year n_sex  n_name  n_n   n_prop
#     136    2    95025  13604  162480

proc.time() - start_time
#   user   system  elapsed  
#   0.578   0.013   0.658 


# 資料視覺化：以 babynames 這個資料集為範例

library(ggplot2)  #The following are the tools for graphing
library(dygraphs)
library(rbokeh)
library(RColorBrewer)
library(plotly)


topNames = baby %>% 
  filter(year > 1986) %>%
  group_by(name, sex) %>%
  summarize(count = sum(n)) %>%
  filter(count > 1000) %>%
  select(name, sex)

yearlyName = baby %>%
  filter(year > 1986) %>%
  inner_join(topNames) %>%
  group_by(year, name, sex) %>%
  summarize(count = sum(n)) %>%
  sdf_copy_to(sc, ., "yearlyname", T, overwrite=T) 

MostPopularNames <- function(year) {
  topNames <- baby %>% 
    filter(year >= 1986) %>%
    group_by(name, sex) %>%
    summarize(count = sum(n)) %>%
    filter(count > 1000) %>%
    select(name, sex)
  
  yearlyName <- baby %>%
    filter(year >= 1986) %>%
    inner_join(topNames) %>%
    group_by(year, name, sex) %>%
    summarize(count = sum(n)) %>%
    sdf_copy_to(sc, ., "yearlyname", T, overwrite=T)
  
  TopNm <- yearlyName %>%
    filter(year == year) %>%
    group_by(name,sex) %>%
    summarize(count=sum(count)) %>%
    group_by(sex) %>%
    mutate(rank = min_rank(desc(count))) %>%
    filter(rank < 5) %>%
    arrange(sex, rank) %>%
    select(name,sex,rank) %>%
    sdf_copy_to(sc,.,"topNames",T,overwrite=TRUE)
  
  topNamesYearly <- yearlyName %>% 
    inner_join(select(TopNm, sex, name)) %>% 
    collect
  
  #兩種畫法可以選一種畫就好
  #第一種圖
  ggplot(topNamesYearly,
         aes(year, count, color = name)) + 
    facet_grid(~sex) + 
    geom_line() + 
    ggtitle(paste0("Most Popular Names of ", year))
  
  #第二種圖
  # 拿出名字欄位之後要做factor化
  names <- TopNm %>% 
    select(name) %>% 
    collect %>% 
    as.data.frame
  
  names <- names[, 1, T]
  
  topNamesYearly$name <- factor(topNamesYearly$name, levels = names, labels = names)
  
  p = ggplot(df, aes(year, count, color=name)) + 
    theme_light() + 
    facet_wrap(~sex, nrow=2) +
    geom_vline(xintercept = year, col='yellow', lwd=1.2) +
    geom_line() +
    ggtitle(sprintf('Most Popular Names of %d',year)) +
    scale_colour_brewer(palette = "Paired")
  
  plotly_build(p)
}

MostPopularNames(2015)
MostPopularNames(2000) 
