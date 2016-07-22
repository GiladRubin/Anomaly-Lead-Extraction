rm(list = ls())

source(file = "Auxiliary-Functions.R")

#load dataset
#table_name <- "clean_requests"
table_name <- "synthetic_data"

dt <- get_dt_from_table(table_name)

#choose date
date <- as.Date(as.Date(min(dt$TimeStamp)))

# find anomalies using previously found optimal parameters
win_size <- 8
iqr_factor <- 3.5
iterations <- 2
anomaly_values <- find_anomalies(dt, win_size, iqr_factor, iterations)

dt <- anomaly_values$dt
anomalies <- anomaly_values$anomalies
clusters <- anomaly_values$clusters

## Plot the time series with the detected anomalies
ts <- get_ts_from_dt(dt, win_size = win_size, type = "xts")
plot(ts)
lines(ts[anomalies$index] ~ time(ts)[anomalies$index]
      ,type = "p"
      ,col = as.character(clusters))

## Find leads for tuples
time_windows <- unique(dt$time_window)
anomaly_times <- time_windows[anomalies$index]
anomalies_in_date <- which(as.Date(anomaly_times) == date)

k <- 3
i <- 1
tuples_dt <- list()
for (anomaly_num in anomalies_in_date)
{
  time_window <- as.character(time_windows[anomalies$index[anomaly_num]])
  temp_tuples_dt <- detect_leads_for_anomaly(dt, win_size, 
                                             iqr_factor,
                                             iterations,
                                             anomalies, 
                                             clusters, 
                                             anomaly_num, 
                                             k)
  filename <- paste("temp_tuples", i)
  save_rds(table_name, temp_tuples_dt, filename, date = date)
  tuples_dt[[time_window]] <- temp_tuples_dt
}

## save tuples_dt in folder
save_rds(table_name, dt)
save_rds(table_name, anomalies)
save_rds(table_name, clusters)
save_rds(table_name, tuples_dt, date = date)
tuples_dt <- read_rds(table_name, "tuples_dt", date = date)