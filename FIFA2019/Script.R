library(tidyverse)
library(rJava)
library(sparklyr)

# Import data

data <- readRDS("data.RDS")

# Connect to Spark Cluster

spark_conn <- spark_connect("local")

# Copy data to Spark Cluster 

data_tbl <- copy_to(spark_conn, data)