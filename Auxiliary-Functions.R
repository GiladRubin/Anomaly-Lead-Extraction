## installs packages that are not installed on the machine 
## and loads packagse that are already installed
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(c("arules", "NbClust", "data.table", "forecast", "zoo", 
       "lubridate", "stringr", "fasttime", "xts", "utils", "stats"))

source(file = "Modified-TSOutliers.R")

#################################
## Basic Functions on Data Tables
#################################

replace_empty_values_with_na <- function (dt, columns)
{
  indices <- which(colnames(dt) %in% columns)
  for (j in indices)
  {
    set(dt, i = which(dt[[j]] == "" | is.na(dt[[j]]) | is.null(dt[[j]])), 
        j = j, value = "NA")
  }
}

## apply generic function on specified columns
op_on_columns <- function (dt, columns, op)
{
  if (is.character(columns)) {
    columns <- which(colnames(dt) %in% columns)
  }
  for (j in columns)
  {
    set(x = dt, j = j, value = op(dt[[j]]))
  }
}

## return Time Series Object from data table
## Aggregated by window size (win_size)
get_ts_from_dt <- function (dt, win_size, type = "ts")
{
  dt[, time_window := align.time(TimeStamp, win_size * 60)]
  agg_dt <- dt[, .(Value = mean(Value)), 
               by = time_window]
  windows_in_day <- (60 / win_size) * 24
  if (type == "ts")
  {
    x <- ts(agg_dt$Value, frequency = windows_in_day)  
  } else {
    x <- xts(agg_dt$Value, order.by = agg_dt$time_window)
  }
  x
}

## Read csv from table name folder and return and data table 
get_dt_from_table <- function(table_name)
{
  path <- paste("./", table_name, "/", table_name, ".csv", sep = "")
  ## Read Dataset
  dt <- fread(input = path)
  
  ## Convert Timestamp String to Timestamp
  dt[, TimeStamp := fastPOSIXct(TimeStamp, tz = "GMT")]
  dt <- dt[order(TimeStamp)] ## Sort datatable according to TimeStamp
  
  dt
}

## Save RDS file for fast loading in following iterations
save_rds <- function(table_name, object, object_name = NULL, date = NULL)
{
  if (!is.null(object_name))
    object_name <- deparse(substitute(object))
  folder <- paste(".", table_name, date, sep = "/")
  if (!dir.exists(folder))
    dir.create(folder, recursive = TRUE)
  filename <- paste(object_name, ".rds", sep ="")
  saveRDS(object, paste(folder, filename, sep = "/"))
}

## Read RDS
read_rds <- function(table_name, object_name, date = NULL)
{
  folder <- paste(".", table_name, date, sep = "/")
  filename <- paste(object_name, ".rds", sep ="")
  readRDS(paste(folder, filename, sep = "/"))
}

###########################
## Lead Detection Functions
###########################

## input: anomalies with their residuals
## output: 3 clusters of the anomalies by their corresponding residuals
cluster_anomalies <- function (anomalies)
{
  indices <- anomalies$index
  residuals <- anomalies$residuals

  clusters <- discretize(x = residuals
                         ,method = "cluster"
                         ,ordered = TRUE
                         ,categories = ifelse(length(indices) < 6, 1, 3)
                         ,labels = c("green", "orange", "red"))
}


## input: data table, window size, iqr_factor and number of iterations
## the algorithm performs an anomaly detection procedure 
## using STL decomposition 
## output: indices of anomalies, their residuals and their cluster

## tsoutliers is a modified version of the function tsoutliers found in
## the "forecast" package in R by Rob Hyndman:
## https://cran.r-project.org/web/packages/forecast/forecast.pdf
find_anomalies <- function(dt, win_size, iqr_factor, iterations = 2)
{
  dt[, time_window := align.time(TimeStamp, win_size * 60)]
  x <- get_ts_from_dt(dt, win_size)
  anomalies <- tsoutliers(x, iqr_factor = iqr_factor, iterate = iterations)
  # anomalies <- tsoutliers(x, iqr_factor = iqr_factor, iterate = iterations)
  # windows_in_day <- (60 / win_size) * 24
  # x_clean <- tsclean(x)
  # seas1 <- fourier(x_clean, K=4)
  # seas2 <- fourier(ts(x_clean, freq=windows_in_day * 7), K=4)
  # arima_fit <- auto.arima(x_clean
  #                        ,xreg=cbind(seas1,seas2)
  #                          ,seasonal = FALSE)
  # anomalies <- tsoutliers(x = x
  #                       ,iqr_factor = 10
  #                       ,fitted_values = fitted(arima_fit))
  
  clusters <- cluster_anomalies(anomalies)
  
  return(list(dt = dt
              ,clusters = clusters
              ,anomalies = anomalies
  ))
}

## Input: an integer k, vector of features (strings) 
## Output: All possible combinations of 3 features
get_k_combinations <- function(dt, features, k = 3)
{
  k_comb <- list()
  for (m in 1:k)
  {
    comb <- combn(features, m)
    combinations <- list()
    for (i in 1:ncol(comb))
    {
      tuple <- comb[, i]
      values <- list()
      for (j in 1:m)
      {
        values[[j]] <- unique(dt[, get(tuple[j])])
      }
      combinations[[i]] <- as.data.table(expand.grid(values))
      setnames(combinations[[i]], tuple)
    }
    k_comb[[m]] <- combinations
  }
  k_comb
}

## auxiliary function to get the tuple string by a data table
## representing the tuple
get_tuple_string <- function(tuple)
{
  tuple_strings <- as.character(sapply(tuple[1,], as.character))
  paste(colnames(tuple), tuple_strings, sep = " = ",
        collapse = ", ")
}

## detecting anomalies for the different tuples
## each filtered_dt is the original data table without the events of the tuple
## the function returns the anomalies detected in this data table
get_outlier_values_from_dt <- function (time_windows,
                                        filtered_dt,
                                        full_anomalies,
                                        iqr_factor,
                                        anomaly_num,
                                        win_size,
                                        iterations)
{
  anomaly_index <- full_anomalies$index[anomaly_num]
  agg_dt <- filtered_dt[, .(Value = mean(Value)), by = time_window]
  
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
  
  anomalies <- tsoutliers(x
                          ,iqr_factor = iqr_factor
                          ,index_for_inspection = anomaly_index
                          ,iterate = iterations)
  
  if (!anomaly_index %in% anomalies$index)
  {
    unique_anomalies <- length(setdiff(anomalies$index
                                       ,full_anomalies$index))
    overall_anomalies <- length(intersect(full_anomalies$index,
                                          anomalies$index)) /
      length(full_anomalies$index)
    distance <- anomalies$index_distance
    return(list(tuple_distance = abs(distance)
              ,overall_anomalies = overall_anomalies
                ,unique_anomalies = unique_anomalies))
  }
  return(NULL)
}

## get a filtered data table by a specified tuple
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

## Main function that generates for a given anomaly the ranking of each
## of the tuples that within the filtered data table do not have 
## an anomaly at the specified anomaly time
get_tuples_dt <- function (dt, k_comb, anomalies, 
                           time_windows, anomaly_num, iterations)
{
  #get original residual
  original_distance <- anomalies$residuals[anomaly_num]
  
  tuples_dt <- data.table(tuple = "All"
                                 ,distance = original_distance
                                 ,overall_anomalies = 1.0 
                                 ,unique_anomalies = 0.0)
  i = 1
  for (combinations in k_comb)
  {
    total <- sum(sapply(combinations, nrow))
    print(paste("Processing Combinations of Size:", i))
    flush.console()
    pb <- txtProgressBar(min = 0, max = total, style = 3)
    j = 0
    for (combination in combinations)
    {
      for (k in 1:nrow(combination))
      {
        tuple <- combination[k]
        filtered_dt <- get_dt_from_tuple(dt, tuple)
        values <- get_outlier_values_from_dt(time_windows,
                                             filtered_dt, 
                                             anomalies, 
                                             iqr_factor,
                                             anomaly_num,
                                             win_size,
                                             iterations)
        
        if (!is.null(values))
        {
          tuple_string <- get_tuple_string(tuple)
          
          tuple_dt <- data.table(tuple = tuple_string
                                 ,distance = values$tuple_distance
                                 ,overall_anomalies = values$overall_anomalies
                                 ,unique_anomalies = values$unique_anomalies)
          
          tuples_dt <- rbind(tuples_dt, tuple_dt)
        }
        j <- j + 1
        setTxtProgressBar(pb, j)
      }
    }
    close(pb)
    i <- i + 1
  }
  tuples_dt
}

## Compress tuples such as:
## (Province = "Tel Aviv") & (Province = "Tel Aviv", Country = "Israel"))
## into (Province = "Tel Aviv"). Country/Continent is less specific 
compress_tuples_dt <- function (tuples_dt)
{
  tuples_dt <- tuples_dt[order(distance, unique_anomalies, 
                               overall_anomalies, str_length(tuple))]
  tuples_dt[, serial_num := sequence(.N), by = .(distance, unique_anomalies, 
                                                          overall_anomalies)]
  tuples_dt <- tuples_dt[serial_num == 1]
  tuples_dt[, serial_num := NULL]
  tuples_dt
}

## Cluster tuples' ranking according to their new distance from the 
## expected value and the number of anomalies they have which do not
## appear in the full dataset
cluster_tuples <- function(tuples_dt)
{
  if (length(unique(tuples_dt$unique_anomalies)) != 1)
    tuples_dt[, unique_anomalies_norm := scale(unique_anomalies, center = FALSE)]
  else 
    tuples_dt[, unique_anomalies_norm := tuples_dt$unique_anomalies]
  
  if (length(unique(tuples_dt$distance)) != 1)
    tuples_dt[, distance_norm := scale(distance, center = FALSE)]
  else 
    tuples_dt[, distance_norm := distance]
  
  clust_dt <- tuples_dt[, .(distance_norm, unique_anomalies_norm)]
  nitems <- nrow(tuples_dt)
  
  ## Cluster
  if (nitems <= 2)
  {
    tuples_dt[, cluster := 1]
  } else if (nitems < 15) {
    clusters <- kmeans(x = clust_dt, centers = min(nitems - 1, 5)
                       ,iter.max = 30)
    tuples_dt[, cluster := clusters$cluster]
  } else {
    nb <- NbClust(data = clust_dt,
                  min.nc=min(nitems - 1, 5), 
                  method = "ward.D2",
                  index = "hartigan")
    tuples_dt[, cluster := nb$Best.partition] 
  }

  tuples_dt[, cluster_value := 
              mean(unique_anomalies_norm) + mean(distance_norm)
            , by = cluster]
  tuples_dt <- tuples_dt[order(cluster_value, overall_anomalies)]
  tuples_dt[, distance_norm := NULL]
  tuples_dt[, unique_anomalies_norm := NULL]
  tuples_dt
}

## This function runs the whole process from start to bottom
## outputting the final compressed and clustered tuples data table
detect_leads_for_anomaly <- function(dt, win_size, iqr_factor, 
                                     iterations, anomalies, 
                                     clusters, anomaly_num, k = 3)
{
  time_windows <- unique(dt$time_window)
  anomaly_index <- anomalies$index[anomaly_num]
  anomaly_time <- time_windows[anomaly_index]
  
  categorical_columns <- setdiff(colnames(dt), 
                                 c("TimeStamp", 
                                   "Value", 
                                   "time_window"))
  
  categorical_indices <- which(names(dt) %in% categorical_columns)
  value_index <- which(names(dt) == "Value")
  window_dt <- dt[time_window == anomaly_time, 
                  c(categorical_indices, value_index), with = F]
  
  op_on_columns(window_dt, categorical_columns, function(x) {as.factor(x)})
  
  k_comb <- get_k_combinations(window_dt, categorical_columns, k)
  
  tuples_dt <- get_tuples_dt(dt, k_comb, anomalies, 
                              time_windows, anomaly_num, 
                                iterations = iterations)
  tuples_dt <- compress_tuples_dt(tuples_dt)
  tuples_dt <- cluster_tuples(tuples_dt)
  tuples_dt
}