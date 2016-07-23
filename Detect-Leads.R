rm(list = ls())

source(file = "Auxiliary-Functions.R")

#load dataset
table_name <- "clean_requests"
#table_name <- "synthetic_data"

dt <- get_dt_from_table(table_name)
#dt <- read_rds(table_name, "dt")

#choose date
dates <- unique(as.Date(dt$TimeStamp))
# date <- as.Date(min(dt$TimeStamp))
# dates <- as.Date(setdiff(dates, date))

# find anomalies using previously found optimal parameters
win_size <- 5
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

k <- 3

for (date in dates)
{
  i <- 1
  anomalies_in_date <- which(as.Date(anomaly_times) == date)
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
    filename <- paste("temp_tuples", i, sep = "-")
    save_rds(table_name, temp_tuples_dt, filename, date = date)
    tuples_dt[[time_window]] <- temp_tuples_dt
    i <- i+1
  }
  save_rds(table_name, tuples_dt, date = date)
}
## save tuples_dt in folder
save_rds(table_name, dt)
save_rds(table_name, anomalies)
save_rds(table_name, clusters)
save_rds(table_name, tuples_dt, object_name = "all_tuples")
saveRDS(tuples_dt, "./synthetic_data/all_tuples.rds")
tuples_dt <- read_rds(table_name, "tuples_dt", date = date)

readRDS("./synthetic_data/all_tuples.rds")