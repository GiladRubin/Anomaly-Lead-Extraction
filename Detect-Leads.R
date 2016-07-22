rm(list = ls())

source(file = "Auxiliary-Functions.R")

## Install Packages
ipak(c("data.table", "forecast", "zoo", "lubridate", "stringr", 
                                    "fasttime", "xts", "utils"))


#load dataset
table_name <- "clean_requests"
dt <- get_dt_from_table(table_name)

#choose date
date <- as.Date(as.Date(max(dt$TimeStamp)))

#find anomalies
win_size <- 5
iqr_factor <- 3
iterations <- 1

anomaly_values <- find_anomalies(dt, win_size, iqr_factor, iterations)
dt <- anomaly_values$dt
anomalies <- anomaly_values$anomalies
clusters <- anomaly_values$clusters

ts <- get_ts_from_dt(dt, win_size = win_size, type = "xts")
plot(ts)
lines(ts[anomalies$index] ~ time(ts)[anomalies$index]
      ,type = "p"
      ,col = as.character(clusters))

#analyze anomlies
time_windows <- unique(dt$time_window)
anomaly_times <- time_windows[anomalies$index]
anomalies_in_date <- which(as.Date(anomaly_times) == date)

results <- fread("./synthetic_data/anomalies.csv")
cbind(time_windows[anomalies$index])

k <- 2
tuples_dt <- list()
for (anomaly_num in anomalies_in_date)
{
  time_window <- as.character(time_windows[anomalies$index[anomaly_num]])
  tuples_dt[[time_window]] <- detect_leads_for_anomaly(dt, 
                                                       win_size, 
                                                       iqr_factor,
                                                       anomalies, 
                                                       clusters, 
                                                       anomaly_num, 
                                                       k)
}

## save tuples_dt in folder
save_rds(table_name, dt)
save_rds(table_name, anomalies)
save_rds(table_name, clusters)
save_rds(table_name, tuples_dt, date = date)
tuples_dt <- read_rds(table_name, "tuples_dt", date = date)