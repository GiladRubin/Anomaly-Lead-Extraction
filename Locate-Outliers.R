rm(list = ls())

source(file = "Auxiliary-Functions.R")

## Install Packages
ipak(c("data.table", "forecast", "zoo", "lubridate", "stringr", 
                                    "fasttime", "xts", "arules"))



# outliers <- tsoutliers(x, iqr_factor = 1.5)
# indices <- outliers$index
# plot(x)
# lines(x[indices] ~ time(x)[indices], type = "p", col = 3)

# seas1 <- fourier(x_clean, K=4)
# seas2 <- fourier(ts(x_clean, freq=windows_in_day * 7), K=4)
# arima_fit <- auto.arima(x_clean
#                         ,xreg=cbind(seas1,seas2)
#                         ,seasonal = FALSE)
# accuracy(arima_fit)

# outliers <- tsoutliers(x = x
#                        ,iqr_factor = 3
#                        ,fitted_values = fitted(arima_fit))

# plot(x)
# 
# lines(x[indices] ~ time(x)[indices]
#       ,type = "p"
#       ,col = as.character(clusters))