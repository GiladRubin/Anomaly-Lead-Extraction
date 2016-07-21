rm(list = ls())

source(file = "Auxiliary-Functions.R")

## Install Packages
packages <- c("data.table", "forecast", "zoo", "lubridate", "stringr", 
            "doParallel", "fasttime", "xts", "arules", "rpart", 
              "partykit", "rpart.plot")
ipak(packages)

source(file = "Modified-TSOutliers.R")

# input : dt, fitted values, anomaly time/index
# output : ranked list of tuples
dt <- readRDS("dt_time_window.rds")
outliers <- readRDS("outliers.rds")
clusters <- readRDS("clusters.rds")
#arima_fit <- readRDS("arima_fit.rds")
win_size <- readRDS("win_size.rds")
iqr_factor <- readRDS("iqr_factor.rds")
anomaly_num <- readRDS("anomaly_num.rds")

#fit_dt <- data.table(fitted_value = as.numeric(fitted(arima_fit)))
time_windows <- unique(dt$time_window)
fit_dt <- data.table(time_window = time_windows)
fit_dt[, anomaly := FALSE]
fit_dt[outliers$index, anomaly := TRUE]

# attach the fitted values to the appropriate timestamps
dt <- merge(dt, fit_dt, by = "time_window")

anomaly_index <- outliers$index[anomaly_num]
anomaly_time <- fit_dt[anomaly_index, time_window]

# train regression tree only on the data at anomaly time
categorical_columns <- setdiff(colnames(dt), 
                               c("TimeStamp", 
                                 "ReqDuration", 
                                 "time_window",
                                 "anomaly"))

categorical_indices <- which(names(dt) %in% categorical_columns)
value <- which(names(dt) == "residuals")

window_dt <- dt[time_window == anomaly_time, 
               c(categorical_indices, value), with = F]

op_on_columns(window_dt, categorical_columns, function(x) {as.factor(x)})

value_count <- sapply(window_dt[, categorical_columns, with = F]
                        , FUN = function(x) {length(unique(x))})
value_count


## Insert k, features and get all the combinations possible
k = 2
k_comb <- list()
for (m in 1:k)
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
original_distance <- outliers$residuals[anomaly_num]

tuples_distances <- data.table(tuple = "All"
                               ,distance = original_distance
                               ,overall_anomalies = 1.0 
                               ,unique_anomalies = 0.0)

get_outlier_values_from_dt <- function (time_windows,
                                        filtered_dt,
                                        full_outliers,
                                        iqr_factor = 3,
                                        anomaly_index,
                                        win_size)
{
  agg_dt <- filtered_dt[, .(Value = mean(ReqDuration)), by = time_window]
  
  ## make sure both DTs have same sizes
  if (nrow(agg_dt) < length(time_windows))
  {
    agg_dt <- merge(time_windows
                    ,agg_dt
                    ,by = "time_window"
                    ,all.x = TRUE)
  }
  
  windows_in_day <- (60 / win_size) * 24
  x <- ts(agg_dt$Value, frequency = windows_in_day)
  
  outliers <- tsoutliers(x
                         ,iqr_factor = iqr_factor
                         ,index_for_inspection = anomaly_index)

  if (!anomaly_index %in% outliers$index)
  {
    unique_anomalies <- length(setdiff(outliers$index
                                       ,full_outliers$index))
    overall_anomalies <- length(intersect(full_outliers$index,
                                          outliers$index)) /
                                    length(full_outliers$index)
    distance <- outliers$index_distance
    return(list(tuple_distance = distance
                ,overall_anomalies = overall_anomalies
                ,unique_anomalies = unique_anomalies))
  }
  return(NULL)
}

get_dt_from_tuple <- function(dt, tuple)
{
  indices_to_filter <- 1:nrow(dt)
  for (col in colnames(tuple))
  {
    column_indices <- which(dt[, get(col)] == tuple[, get(col)])
    indices_to_filter <- intersect(indices_to_filter, column_indices)
  }
  filtered_dt <- dt[!indices_to_filter, ]
}

tuples_tables <- list()
for (combinations in k_comb)
{
  for (combination in combinations)
  {
    for (i in 1:nrow(combination))
    {
      tuple <- combination[i]
      print(tuple)
      filtered_dt <- get_dt_from_tuple(dt, tuple)
      values <- get_outlier_values_from_dt(time_windows,
                                           filtered_dt, 
                                           outliers, 
                                           iqr_factor,
                                           anomaly_index,
                                           win_size)
      #get_tuple_string
      tuple_values <- as.character(sapply(tuple[1,], as.character))
      if (!is.null(values))
      {
        tuple_string <- paste(colnames(tuple), tuple_values, sep = " = ",
                              collapse = ", ")
        
        tuple_dt <- data.table(tuple = tuple_string
                               ,distance = values$tuple_distance
                               ,overall_anomalies = values$overall_anomalies
                               ,unique_anomalies = values$unique_anomalies)
        
        tuples_distances <- rbind(tuples_distances, tuple_dt)
        
        tuples_tables[[tuple_string]] <- list(tuple, tuple_dt)
      }
    }
  }
}

plot(distance ~ unique_anomalies, data = tuples_distances)

saveRDS(tuples_tables, "tuples_tables.rds")
saveRDS(tuples_distances, "tuples_distances.rds")

tuples_distances <- (readRDS("tuples_distances.rds"))
plot(distance ~ unique_anomalies, data = tuples_distances)
tuples_tables <- (readRDS("tuples_tables.rds"))
original_ts <- get_ts_from_dt(dt, win_size)
plot(original_ts)
indices <- outliers$index
residuals <- outliers$residuals

clusters <- discretize(x = residuals
                       ,method = "cluster"
                       ,ordered = TRUE
                       ,categories = 3
                       ,labels = c("green", "orange", "red")
)
# collapse identical tuples (province is very specific)
# 

#change reqduration to value
#change outlier to anomaly

# get_values_from_combination <- function(combination)
# {
#   for (i in 1:nrow(combination))
#   {
#     tuple <- combination[i]
#     print(tuple)
#     indices_to_filter <- 1:nrow(dt)
#     for (col in colnames(tuple))
#     {
#       column_indices <- which(dt[, get(col)] == tuple[, get(col)])
#       indices_to_filter <- intersect(indices_to_filter, column_indices)
#     }
#     filtered_dt <- dt[!indices_to_filter, ]
#     values <- get_outlier_values_from_dt(time_windows,
#                                          filtered_dt, 
#                                          outliers, 
#                                          iqr_factor,
#                                          anomaly_index,
#                                          win_size)
#     
#     #get_tuple_string
#     tuple_values <- as.character(sapply(tuple[1,], as.character))
#     if (!is.null(values))
#     {
#       tuple_string <- paste(colnames(tuple), tuple_values, sep = " = ",
#                             collapse = ", ")
#       
#       tuple_dt <- data.table(tuple = tuple_string
#                              ,distance = values$tuple_distance
#                              ,overall_anomalies = values$overall_anomalies
#                              ,unique_anomalies = values$unique_anomalies)
#       
#       tuples_distances <- rbind(tuples_distances, tuple_dt)
#       
#       tuples_tables[[tuple_string]] <- tuple_dt
#     }
#   }
# }
# 
# cl <- makeCluster(detectCores())
# registerDoParallel(cl)
# 
# foreach(combinations = k_comb, .packages=packages) %do%
# {
#   foreach(combination = combinations, .packages=packages) %do%
#   {
#     get_values_from_combination(combination)
#   }
# }
# stopCluster(cl)