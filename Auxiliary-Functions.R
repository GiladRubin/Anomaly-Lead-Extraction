ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

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