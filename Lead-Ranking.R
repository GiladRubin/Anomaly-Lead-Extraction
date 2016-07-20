rm(list = ls())

source(file = "Auxiliary-Functions.R")

## Install Packages
ipak(c("data.table", "forecast", "zoo", "lubridate", "stringr", 
       "fasttime", "xts", "arules", "rpart", "partykit", "rpart.plot"))

# input : dt, fitted values, anomaly time/index
# output : ranked list of tuples
dt <- readRDS("dt_time_window.rds")
outliers <- readRDS("outliers.rds")
clusters <- readRDS("clusters.rds")
arima_fit <- readRDS("arima_fit.rds")

fit_dt <- data.table(fitted_value = as.numeric(fitted(arima_fit)))
fit_dt[, time_window := unique(dt$time_window)]
fit_dt[, anomaly := FALSE]
fit_dt[outliers$index, anomaly := TRUE]

# attach the fitted values to the appropriate timestamps
dt <- merge(dt, fit_dt, by = "time_window")

# get the residuals for each event
dt[, residuals := ReqDuration - fitted_value]
temp <- dt[order(residuals)]
hist(temp$residuals[1:100000])

anomaly_times <- unique(dt[anomaly == TRUE, time_window])
index <- 8
anomaly_time <- anomaly_times[index]

# train regression tree only on the data at anomaly time
categorical_columns <- setdiff(colnames(dt), 
                               c("TimeStamp", 
                                 "ReqDuration", 
                                 "time_window",
                                 "fitted_value",
                                 "anomaly",
                                 "residuals"))
                                 #"Province",
                                 #"Country"))

categorical_indices <- which(names(dt) %in% categorical_columns)
value <- which(names(dt) == "residuals")

window_dt <- dt[time_window == anomaly_time, 
               c(categorical_indices, value), with = F]

op_on_columns(window_dt, categorical_columns, function(x) {as.factor(x)})

value_count <- sapply(window_dt[, categorical_columns, with = F]
                        , FUN = function(x) {length(unique(x))})
value_count


## Insert k, features and get all the combinations possible
k = 3
k_comb <- list()
for (m in 2:k)
{
  #m <- 1
  comb <- combn(categorical_columns, m)
  combinations <- list()
  for (i in 1:ncol(comb))
  {
    #i <- 1
    tuple <- comb[, i]
    values <- list()
    for (j in 1:m)
    {
      #j <- 1
      values[[j]] <- unique(window_dt[, get(tuple[j])])
    }
    combinations[[i]] <- as.data.table(expand.grid(values))
    setnames(combinations[[i]], tuple)
  }
  k_comb[[m]] <- combinations
}

#get original distance
tuples_distances <- data.table(tuple = "All"
                               ,distance = original_distance
                               ,overall_anomalies = 1.0 
                               ,unique_anomalies = 0.0)

k_comb[[2]]
for (combinations in k_comb)
{
  for (combination in combinations)
  {
    for (i in 1:nrow(combination))
    {
      tuple <- combination[i]
      indices_to_filter <- 1:nrow(dt)
      for (col in colnames(tuple))
      {
        column_indices <- which(dt[, get(col)] == tuple[, get(col)])
        indices_to_filter <- intersect(indices_to_filter, column_indices)
      }
      filtered_dt <- dt[!indices_to_filter, ]
      
      # create time series
      # get current outlier distance from expected value
      # if still outlier - break!
      # get past outlier time windows
      # get both anomaly history values
      
      # get_tuple_string
      values <- as.character(sapply(tuple[1,], as.character))
      tuple_string <- paste(colnames(tuple), values, sep = " = ", 
                            collapse = ", ")
      
      tuple_dt <- data.table(tuple = tuple_string
                             ,distance = tuple_distance
                             ,overall_anomalies = overall_anomalies 
                             ,unique_anomalies = unique_anomalies)
      
      tuples_distances <- rbind(tuples_distances, tuple_dt)
      
      tuples_tables[[j]] <- list(tuple_string, tuple_distance_dt)
      names(tuples_tables)[j] <- tuple_string
      j <- j + 1
    }
  }
}