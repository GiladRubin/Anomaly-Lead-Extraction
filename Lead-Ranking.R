rm(list = ls())

source(file = "Auxiliary-Functions.R")

## Install Packages
packages <- c("data.table", "forecast", "zoo", "lubridate", "stringr", 
            "doParallel", "fasttime", "xts", "arules", "rpart", 
              "partykit", "rpart.plot", "scatterplot3d")
ipak(packages)


  
#   plot(distance ~ unique_anomalies, data = tuples_distances)
#   
#   saveRDS(tuples_tables, "tuples_tables.rds")
#   saveRDS(tuples_distances, "tuples_distances.rds")
#   
#   tuples_distances <- (readRDS("tuples_distances.rds"))
#   plot(distance ~ unique_anomalies, data = tuples_distances)
#   attach(tuples_distances)
#   View(tuples_distances)
#   
#   scatterplot3d(distance, overall_anomalies, unique_anomalies)
#   
#   tuples_tables <- (readRDS("tuples_tables.rds"))
#   original_ts <- get_ts_from_dt(dt, win_size)
#   original_ts <- get_ts_from_dt(dt, win_size, type = "xts")
#   plot(original_ts)
#   indices <- anomalies$index
#   residuals <- anomalies$residuals
#   
#   clusters <- discretize(x = residuals
#                          ,method = "cluster"
#                          ,ordered = TRUE
#                          ,categories = 3
#                          ,labels = c("green", "orange", "red")
#   )
# }
# collapse identical tuples (e.g. province is very specific) - same values
# sort list by 3 metrics

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
#                                          anomalies, 
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