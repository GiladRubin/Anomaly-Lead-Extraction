rm(list = ls())

source(file = "Auxiliary-Functions.R")

## Install Packages
ipak(c("data.table", "forecast", "zoo", "lubridate", "stringr", 
       "tsoutliers", "fasttime", "xts", "utils"))


#load dataset correct format
table_name <- "clean_requests"
path <- paste("./Raw Data/", table_name, ".csv", sep = "")
dt <- get_dt_from_table(path)

#find anomalies
win_size <- 5
iqr_factor <- 3
anomaly_values <- find_anomalies(dt, win_size, iqr_factor)
dt <- anomaly_values$dt
anomalies <- anomaly_values$anomalies
clusters <- anomaly_values$clusters

#analyze anomlies and save into folders
anomaly_num <- 20
k <- 3
tuples_dt <- detect_leads_for_anomaly(dt, win_size, iqr_factor,
                             anomalies, clusters, anomaly_num, k)
