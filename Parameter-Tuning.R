rm(list = ls())

source(file = "Auxiliary-Functions.R")

## Install Packages
ipak(c("data.table", "forecast", "zoo", "lubridate", "stringr", 
       "fasttime", "xts", "utils"))

#load dataset
#table_name <- "clean_requests"
table_name <- "synthetic_data"
dt <- get_dt_from_table(table_name)
win_sizes <- seq(1, 15, 1)
iqr_factors <- seq(1, 10, 0.5)
iterations <- c(1, 2)

parameters <- as.data.table(expand.grid(iterations, iqr_factors, win_sizes))
setnames(parameters, c("iterations", "iqr_factor", "win_si"))

results <- fread("./synthetic_data/anomlies_timestamps.csv")
setnames(results, "time stamps", "TimeStamp")
op_on_columns(results, "TimeStamp", fastPOSIXct)
fscores <- c()
times <- c()
for (win_size in win_sizes)
{
  for (iqr_factor in iqr_factors)
  {
    for (iteration in iterations)
    {
      ptm <- proc.time()
      anomaly_values <- find_anomalies(dt, win_size, iqr_factor, iteration)
      dt <- anomaly_values$dt
      anomalies <- anomaly_values$anomalies
      clusters <- anomaly_values$clusters
      elapsed_time <- proc.time() - ptm
      
      ts <- get_ts_from_dt(dt, win_size = win_size, type = "xts")
      plot(ts)
      lines(ts[anomalies$index] ~ time(ts)[anomalies$index]
            ,type = "p"
            ,col = as.character(clusters))
      
      #analyze anomlies
      time_windows <- unique(dt$time_window)
      anomaly_times <- unique(time_windows[anomalies$index])
      
      results[, timestamp_window := align.time(TimeStamp, win_size * 60)]
      actual_anomaly_times <- unique(results$timestamp_window)
      ## TP
      TP <- length(intersect(anomaly_times, actual_anomaly_times))
      
      ## FP
      FP <- length(setdiff(anomaly_times,actual_anomaly_times))
      
      ## FN
      FN <- length(setdiff(actual_anomaly_times, anomaly_times))
      
      precision <- TP/(TP + FP)
      recall <- TP / (TP + FN)
      fscore <- 2 * ((precision * recall) / (precision + recall)) 
      
      result <- paste("iqr factor = ", iqr_factor, ", win_size = ", win_size
            , ", iterations = ", iteration, ". Got F-Score: ", 
            fscore, "in time: ", round(as.numeric(elapsed_time[3]), 4), 
            " seconds", sep = "")
      print(result)
      fscores <- c(fscores, fscore)
      times <- c(times, as.numeric(elapsed_time[3]))
    }
  }
}

parameters[, fscore := fscores]
parameters[, time := times]
plot(fscore ~ time, data = parameters)
parameters <- parameters[order(-fscore, time)]
head(parameters, 10)
parameters[1,]