rm(list = ls())

source(file = "Auxiliary-Functions.R")

## Install Packages
ipak(c("data.table", "forecast", "zoo", "lubridate", "stringr", 
                                    "fasttime", "xts", "arules"))

source(file = "Modified-TSOutliers.R")

dt <- readRDS("dt.rds")

## Decide on Window Size (In Minutes)
win_size = 30
dt[, time_window := align.time(TimeStamp, win_size * 60)]
trim_size <- 0.01
trim_size <- 0
upper_limit <- quantile(dt$ReqDuration, 1-trim_size)
lower_limit <- quantile(dt$ReqDuration, trim_size)
trim_dt <- dt[ReqDuration < upper_limit & ReqDuration > lower_limit]
#trim_dt <- trim_dt[1:100000]
agg_dt <- trim_dt[, .(Value = mean(ReqDuration)), 
                  by = time_window]

windows_in_day <- (60 / win_size) * 24

x <- ts(agg_dt$Value, frequency = windows_in_day)
outliers <- tsoutliers(x, outlier_power = 1.5)
indices <- outliers$index
plot(x)
lines(x[indices] ~ time(x)[indices], type = "p", col = 3)

x_clean <- tsclean(x, replace.missing = TRUE)
lines(x_clean, col = 3)

# x <- msts(data = agg_dt$Value,
#           seasonal.periods = c(windows_in_day, windows_in_day*7))
# tbats_fit <- tbats(tsclean(x))
# tbats_fc <- forecast(tbats_fit, h=(60/win_size)*24*7)
# plot(tbats_fc)

seas1 <- fourier(x_clean, K=4)
seas2 <- fourier(ts(x_clean, freq=windows_in_day * 7), K=4)
arima_fit <- auto.arima(x_clean, xreg=cbind(seas1,seas2)
                        ,seasonal = FALSE
                        # ,stepwise = FALSE
                        # ,approximation = FALSE
)

outliers <- tsoutliers(x = x, 
                       outlier_power = 1.5, 
                       fitted_values = fitted(arima_fit))

indices <- outliers$index
powers <- outliers$power
clusters <- discretize(x = powers
                      ,method = "cluster"
                      ,ordered = TRUE
                      ,categories = 3
                      ,labels = c("green", "orange", "red")
                      )
plot(x)
lines(x[indices] ~ time(x)[indices]
      ,type = "p"
      ,col = as.character(clusters))