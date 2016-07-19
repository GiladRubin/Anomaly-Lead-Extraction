rm(list = ls())


date <- as.POSIXct("2015-10-05")

subset_dt <- dt[TimeStamp >= date & TimeStamp < date + days(1)]
## Find Categorical Features With too Many Different Values
value_count <- sapply(subset_dt[, categorical_columns, with = F], 
                      FUN = function(x) {length(unique(x))})
long_categories <- names(value_count[which(as.numeric(value_count) > 15)])
#Cluster_categories
subset_dt <- convert_categories_to_clusters(subset_dt, long_categories, "ReqDuration")
subset_dt <- subset_dt[order(TimeStamp)] ## Re-Order the DataTable

## Train Regression Tree
names(subset_dt)
features <- setdiff(names(subset_dt), c("ReqDuration", "TimeStamp"))
features <- setdiff(features, long_categories)

tree_dt <- subset_dt[, c(features, "ReqDuration"), with = F]
tree <- rpart(formula = ReqDuration ~ ., data = tree_dt, maxdepth = 3)
colors <- c("#88CC88", "#EEEE88", "#EE8888")
y <- tree$frame$yval
y_discretized <- discretize(y, method = "cluster", categories = 3, 
                            ordered = TRUE, labels = 1:3)
tree_plot <- prp(x = tree, type = 1, extra = 1 ,fallen.leaves = T,
                      digits = 4, varlen = 0, faclen = 1, round = 1.4
                      ,shadow.col = "gray", branch.lty = 2,
                      box.col = colors[y_discretized])

# ## Convert Value into Anomaly Score (Deviation from Hourly Trimmed Mean)
# subset_dt[, day_hour := floor_date(TimeStamp, unit = "hour")]
# subset_dt[, hourly_trimmed_mean := mean(ReqDuration, trim = 0.05)]
# subset_dt[, hourly_trimmed_std := sd_trim(ReqDuration, trim = 0.05, const = FALSE)]
# subset_dt[, anomaly_score := abs(ReqDuration - hourly_trimmed_mean) / hourly_trimmed_std]


## Visual:
  ## Try To Direct Tree Better
  ## Why click doesn't work every time?
  ## Hover to emphasize, Click to remove, click againg to restore

## Seasonal Anomalies:
  ## Aggregate to Hourly Data
  ## Find Avg. Trimmed Hourly Proportions
  ## Plot The "Expected" Graph (AVG) Vs. the Actual
  ## Assign Each Event A distance from the expected value
  ## Cluster By Distance From Expected Values
  ## For each Feature, Tuple & Triplet - Measure the Aggregated value per Hour/Time Unit
  ## Give a score to each feature/s by the change in distance upon removal
  ## Scoring: Use Max distance per hour and/or total per day OR Histogram Distance
  ## Show a Scoreboard with priority to single features
  ## On Click - Plot a third graph - With the Removed Feature/s