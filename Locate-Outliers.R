rm(list = ls())

source(file = "Auxiliary-Functions.R")

## Install Packages
ipak(c("data.table", "forecast", "zoo", "lubridate", "stringr", 
                                    "fasttime", "xts", "arules"))

source(file = "Modified-TSOutliers.R")

dt <- readRDS("dt.rds")

## Decide on Window Size (In Minutes)
win_size = 5
dt[, time_window := align.time(TimeStamp, win_size * 60)]

agg_dt <- dt[, .(Value = mean(ReqDuration)), 
                  by = time_window]

trim_size <- 0.01
upper_limit <- quantile(dt$ReqDuration, 1-trim_size)
lower_limit <- quantile(dt$ReqDuration, trim_size)
trim_dt <- dt[ReqDuration < upper_limit & ReqDuration > lower_limit]
agg_trim_dt <- trim_dt[, .(Value = mean(ReqDuration)), 
                                      by = time_window]

windows_in_day <- (60 / win_size) * 24
x <- ts(agg_dt$Value, frequency = windows_in_day)
plot(x)
x_clean <- ts(agg_trim_dt$Value, frequency = windows_in_day)
plot(x_clean)
# x_clean <- tsclean(x, replace.missing = TRUE)

# outliers <- tsoutliers(x, iqr_factor = 1.5)
# indices <- outliers$index
# plot(x)
# lines(x[indices] ~ time(x)[indices], type = "p", col = 3)


# lines(x_clean, col = 3)

# x <- msts(data = agg_dt$Value,
#           seasonal.periods = c(windows_in_day, windows_in_day*7))
# tbats_fit <- tbats(tsclean(x))
# tbats_fc <- forecast(tbats_fit, h=(60/win_size)*24*7)
# plot(tbats_fc)

# seas1 <- fourier(x_clean, K=4)
# seas2 <- fourier(ts(x_clean, freq=windows_in_day * 7), K=4)
# arima_fit <- auto.arima(x_clean
#                         ,xreg=cbind(seas1,seas2)
#                         ,seasonal = FALSE)
# accuracy(arima_fit)

# outliers <- tsoutliers(x = x
#                        ,iqr_factor = 3
#                        ,fitted_values = fitted(arima_fit))

iqr_factor <- 3
outliers <- tsoutliers(x, 
                       iqr_factor = iqr_factor)

indices <- outliers$index
residuals <- outliers$residuals

clusters <- discretize(x = residuals
                      ,method = "cluster"
                      ,ordered = TRUE
                      ,categories = 3
                      ,labels = c("green", "orange", "red")
                      )
plot(x)

lines(x[indices] ~ time(x)[indices]
      ,type = "p"
      ,col = as.character(clusters))


# plot(x)
# lines(x[indices] ~ time(x)[indices]
#       ,type = "p"
#       ,col = as.character(clusters))
# stl_indices <- stl$index
# lines(x[stl_indices] ~ time(x)[stl_indices]
#       ,type = "p"
#       ,pch = 20)
# 
# length(intersect(stl_indices, indices)) / length(stl_indices)
# 
# length(intersect(indices, stl_indices)) / length(indices)

saveRDS(dt, "dt_time_window.rds")
saveRDS(clusters, "clusters.rds")
#saveRDS(arima_fit, "arima_fit.rds")
saveRDS(outliers, "outliers.rds")
saveRDS(win_size, "win_size.rds")
saveRDS(iqr_factor, "iqr_factor.rds")
anomaly_num <- 20
saveRDS(anomaly_num, "anomaly_num.rds")
