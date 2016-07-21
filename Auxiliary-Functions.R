ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

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

## Clustering
split_max_cluster <- function (cluster_dt)
{
  category <- names(cluster_dt)[1]
  y_per_cluster <- cluster_dt[, .(mean_y_per_cluster = mean(std_y)), by = cluster_index]
  y_per_cluster <- y_per_cluster[order(mean_y_per_cluster, decreasing = T)]
  max_cluster <- as.numeric(y_per_cluster[1, cluster_index])
  cluster_indices <- which(as.numeric(cluster_dt$cluster_index) == max_cluster)
  cluster_count <- as.numeric(table(cluster_dt$cluster_index)[max_cluster])
  if (cluster_count > 1 & cluster_count < 10)
  {
    cluster_dt[cluster_indices, cluster_name := as.factor(get(category))]
  }
}

rename_clusters <- function (cluster_dt)
{
  category <- names(cluster_dt)[1]
  for (cluster in unique(cluster_dt$cluster_index))
  {
    current_cluster <- cluster_dt[cluster_index == cluster]
    count_per_cluster <- nrow(current_cluster)
    if (count_per_cluster > 1)
    {
      current_cluster <- current_cluster[order(std_y)]
      median_index <- round(count_per_cluster / 2)
      representitive <- current_cluster[median_index, get(category)]
      current_cluster_name <- paste(as.character(representitive), "+", as.character(count_per_cluster))
      cluster_dt[cluster_index == cluster, cluster_name := as.factor(current_cluster_name)]
    } else {
      cluster_dt[cluster_index == cluster, cluster_name := as.factor(get(category))]
    }
  }
}

#############################
###Feature Grouping Functions
#############################

cluster_category <- function (dt, category, value)
{
  ## How many different values for this Category?
  num_levels <- length(unique(dt[, get(category)])) 
  
  ## Get Proportion of each Value
  prop_table <- as.data.table(prop.table(table(dt[, get(category)])))
  setnames(prop_table, c(category, "proportion"))
  
  ## Get Mean Y value Of Each Value Group
  cluster_dt <- dt[, .(mean_y = mean(get(value))), by = get(category)]
  setnames(cluster_dt, 1, category)
  
  ## Merge the two data tables and Standartize the values
  cluster_dt <- merge(cluster_dt, prop_table, by = category, all.x = T)
  cluster_dt[, std_y := scale(mean_y, scale = T, center = F)]
  cluster_dt[, std_proportion := scale(proportion, scale = T, center = F)]
  
  ## Cluster
  nb <- NbClust(data = cluster_dt[, .(std_y, std_proportion)],
                min.nc=2, max.nc=min(num_levels - 1, 15), method = "ward.D2", 
                index = "hartigan")
  
  par(mfrow = c(1, 1)) ## Technical - Ignore this line
  
  ## Get the partition for every value into the suggested Cluster
  partition <- nb$Best.partition
  cluster_dt[, cluster_index := as.factor(partition)]
  setnames(cluster_dt, 1, category)
  
  ## Assign Meaningful Names to Clusters
  cluster_dt[, cluster_name := "default"] ## Default Value
  ## Rename Clusters
  rename_clusters(cluster_dt)
  ## Split the cluster with the highest mean Y value into Independent Groups of size 1
  ## This will help the decision Tree be more percise
  split_max_cluster(cluster_dt)
  
  ## Plot the values and with their relevant Cluster
  plot(data = cluster_dt, std_y ~ std_proportion, 
       xlab = "Proportion In Dataset", ylab = "Mean Y Value",
       col = colorize_vector(cluster_dt$cluster_name), 
       pch = (as.numeric(cluster_dt$cluster_index) %% 15) + 15, 
       main = paste("Clusters For Category:", category))
  
  ## Return
  cluster_dt
}

plot_clusters <- function (cluster_dt)
{
  category <- names(cluster_dt)[1]
  highchart() %>%
    hc_title(text = paste(category, "Mean Y Value Vs. Proportion")) %>% 
    hc_add_series_scatter(x = cluster_dt$std_proportion, y = cluster_dt$std_y
                          ,label = cluster_dt[, get(category)], 
                          color = colorize_vector(cluster_dt$cluster_name)) %>%
    hc_yAxis(title = list(text = "Mean Y Value")) %>%
    hc_xAxis(title = list(text = "Proportion In Dataset"))
}

convert_categories_to_clusters <- function (dt, columns, value)
{
  for (col in columns)
  {
    current_cluster <- cluster_category(dt = dt, category = col, value = value)
    current_cluster <- current_cluster[, .(get(col), cluster_name)]
    new_column_name <- paste(col, "Group", sep = "_")
    setnames(current_cluster, c(col, new_column_name))
    dt <- merge(dt, current_cluster, by = col, all.x = T)
  }
  dt
}

get_ts_from_dt <- function (dt, win_size, type = "ts")
{
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

get_dt_from_table <- function(path)
{
  ## Read Dataset
  dt <- fread(input = path)
  
  ## Convert Timestamp String to Timestamp
  dt[, TimeStamp := fastPOSIXct(TimeStamp, tz = "GMT")]
  dt <- dt[order(TimeStamp)] ## Sort datatable according to TimeStamp
  
  dt
}

cluster_anomalies <- function (anomalies)
{
  indices <- anomalies$index
  residuals <- anomalies$residuals

  clusters <- discretize(x = residuals
                         ,method = "cluster"
                         ,ordered = TRUE
                         ,categories = 3
                         ,labels = c("green", "orange", "red"))
}

find_anomalies <- function(dt, win_size, iqr_factor)
{
  dt[, time_window := align.time(TimeStamp, win_size * 60)]
  x <- get_ts_from_dt(dt, win_size)
  
  anomalies <- tsoutliers(x, iqr_factor = iqr_factor)
  
  clusters <- cluster_anomalies(anomalies)
  
  return(list(dt = dt
              ,clusters = clusters
              ,anomalies = anomalies
  ))
}

## Insert k, features and get all the combinations possible
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

get_tuple_string <- function(tuple)
{
  tuple_strings <- as.character(sapply(tuple[1,], as.character))
  paste(colnames(tuple), tuple_strings, sep = " = ",
        collapse = ", ")
}

get_outlier_values_from_dt <- function (time_windows,
                                        filtered_dt,
                                        full_anomalies,
                                        iqr_factor = 3,
                                        anomaly_index,
                                        win_size)
{
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
                          ,index_for_inspection = anomaly_index)
  
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

get_tuples_dt <- function (dt, k_comb, anomalies, 
                           time_windows, anomaly_index)
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
    #combinations <- k_comb[[1]]
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
                                             anomaly_index,
                                             win_size)
        
        if (!is.null(values))
        {
          tuple_string <- get_tuple_string(tuple)
          
          tuple_dt <- data.table(tuple = tuple_string
                                 ,distance = values$tuple_distance
                                 ,overall_anomalies = values$overall_anomalies
                                 ,unique_anomalies = values$unique_anomalies)
          
          tuples_dt <- rbind(tuples_dt, tuple_dt)
          
          # tuples_tables[[tuple_string]] <- list(tuple, tuple_dt)
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

detect_leads_for_anomaly <- function(dt, win_size, iqr_factor, 
                                     anomalies, clusters, 
                                     anomaly_num, k = 3)
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
                              time_windows, anomaly_index)
}