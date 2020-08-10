# Refference
## usr: https://spark.apache.org/docs/2.1.0/ml-classification-regression.html

library(sparklyr);library(DBI); library(dplyr); library(ggplot2)
# Spark connection configuration
conf = spark_config(); conf$spark.ui.port = 4040 # default port
conf$spark.executor.memory = "4G" # memory per working node
conf$spark.yarn.executor.memoryOverhead = "1024"
spark_home = "/usr/local/spark/spark-2.1.0-bin-hadoop2.7/"
# Create a connection to local Spark
sc = spark_connect(master = "spark://192.168.1.100:7077", version = "2.1.0",
                   app_name = "local_app", config = conf, spark_home = spark_home)
spark_version(sc) # Check Spark running version
#spark_disconnect(sc)
# Move R dataframe to Spark. Movies dataset as the example.
iris_sdf = copy_to(sc, df = iris, name = "iris", overwrite = T)
head(iris_sdf)
# List all available Spark data frames in Spark memory
dbListTables(sc)


Sys.setenv("HADOOP_CMD" = "/home/hadoop/hadoop/bin/hadoop");
Sys.setenv("HADOOP_STREAMING"="/home/hadoop/hadoop/share/hadoop/tools/lib/hadoop-streaming-2.8.1.jar")
library(rhdfs); hdfs.init(); 
hdfs.ls("/home/sample")
diamond_sdf = spark_read_csv(sc, name = "diamonds", path = "hdfs:////home/sample/diamonds.csv", header = T)


# Create an SDF pointer to existing RDD
#test = tbl(sc, from = "test")

iris_train_test = sdf_partition(iris_sdf, training = 0.7, test = 0.3, seed =1)

iris_sdf = tbl(sc, "iris")
tbl_cache(sc, "iris")
iris_train_sdf = sdf_sample(iris_sdf, 0.7, seed = 1 )
#nrow(iris_train_sdf[[2]])
# 30% as the testing set
iris_test_sdf = setdiff(iris_sdf, iris_train_sdf)
#nrow(iris_test_sdf)

# Actually create RDDs to faciliate model fitting
compute(iris_train_sdf, "iris_train", temporary = F)
tbl_cache(sc, "iris_train", force = T)
compute(iris_test_sdf, "iris_test", temporary = F)
tbl_cache(sc, "iris_test", force = T)

test <- diamond_sdf %>% ml_generalized_linear_regression( color ~ .)

diamonds_sdf = tbl(sc, "diamonds")
tbl_cache(sc, "diamonds")

iris_train_sdf %>% ml_random_forest_classifier( response = "Species", features = ".")

iris_lm <- iris_train_sdf %>% ml_linear_regression( Sepal_Width ~ .)
summary(iris_lm)

iris_tree <- iris_train_sdf %>% ml_random_forest_classifier( Species ~ ., num_trees = 500L)
summary(iris_tree)

iris_test_sdf %>% sdf_predict(iris_tree) %>% filter(label == prediction) %>% tally()
iris_test_sdf %>% tally()

diamond <- as.data.frame(diamonds)
diamond_sdf %>% sdf_predict(test)

result <- iris_test_sdf %>% sdf_predict(iris_tree) %>%  select(label ,prediction)
result <- as.data.frame(result)
table(result$label,result$prediction)

spark_disconnect(sc)