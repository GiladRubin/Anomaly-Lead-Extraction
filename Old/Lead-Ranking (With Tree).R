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

get_anomaly_limits <- function (x, iqr_factor = 3)
{
  resid.q <- quantile(x, prob=c(0.25,0.75), na.rm=TRUE)
  iqr <- diff(resid.q)
  limits <- resid.q + iqr_factor*iqr*c(-1,1)
  limits
}

limits <- get_anomaly_limits(dt[time_window < anomaly_time, residuals])

tree <- rpart(formula = residuals ~ .
              ,data = train_dt
              ,control = rpart.control(minsplit = 1
                                       ,minbucket = 1
                                       ,cp = 0.01))

tree_plot <- prp(x = tree, type = 1, extra = 1 ,fallen.leaves = T,
                 digits = 4, varlen = 0, faclen = 1, round = 1.4
                 ,shadow.col = "gray", branch.lty = 2)
                 #box.col = colors[y_discretized])

nodes <- tree$frame$yval

if (outliers$residuals > 0) ## anomaly is above expected value 
{
  anomaly_indices <- which(nodes > limits[2])
  anomaly_nodes <- as.numeric(row.names(tree$frame[anomaly_indices, ]))
  #for each node - compress path and return tuples
  path <- path.rpart(tree, anomaly_nodes[2], 0)[[1]]
}

node_tuple <- list()
for (i in 2:length(path))
{
  split <- path[i]
  split_values <- strsplit(split, "=")
  feature <- split_values[[1]][[1]]
  values <- split_values[[1]][[2]]
  node_tuple[[feature]] <- values
}

# create tuples from those vertices

# get differences between the tuples and the non-tuple TS
# get p-values for the anomaly time for each tuple
# prepare a table with each tuple 
# and its corresponding mean, variance and p-value

# train a classifier (logistic?) on all the data

# predict (probabilities) new instances using a pre-trained classifier
# return top k tuples