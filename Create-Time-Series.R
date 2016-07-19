rm(list = ls())

source(file = "Auxiliary-Functions.R")

## Install Packages
ipak(c("data.table", "forecast", "zoo", "lubridate", "stringr", 
                                              "fasttime", "xts"))

## Read Dataset
raw_dt <- fread(input = "./Raw Data/new_requests.csv")

## Select Only Informative Features
dt <- unique(raw_dt[, .(TimeStamp, 
                        RoleInst, 
                        Continent, 
                        Province, 
                        OpName, 
                        Country, 
                        Host, 
                        Response, 
                        ReqDuration)])

## Convert Timestamp String to Timestamp
dt[, TimeStamp := fastPOSIXct(TimeStamp, tz = "GMT")]
dt <- dt[order(TimeStamp)] ## Sort datatable according to TimeStamp

## Convert non-ascii letters to ASCII & Remove Non-Alphanumeric Letters
op_on_columns(dt, "Province", function (x) {iconv(x, "latin1", "ASCII", sub="")})
op_on_columns(dt, "Province", function (x) {str_replace_all(x, "[^[:alnum:]]", " ")})

#replace_empty_values_with_na(dt, c("Country", "Continent", "Province"))

## Convert String Columns to Categories
categorical_columns <- setdiff(colnames(dt), c("TimeStamp", "ReqDuration"))
categorical_columns
op_on_columns(dt, categorical_columns, function(x) {as.factor(x)})

## Subset dataset to contain only full data

## (Continuous data flow stopped on 2015-10-07 at 18:00)
dt <- dt[TimeStamp >= as.POSIXct('2016-01-01 00:00')
        & TimeStamp < as.POSIXct('2016-01-12 00:00')]

## Decide on Window Size (In Minutes)
win_size = 60
dt[, time_window := align.time(TimeStamp, win_size * 60)]
trim_size <- 0.01
upper_limit <- quantile(dt$ReqDuration, 1-trim_size)
lower_limit <- quantile(dt$ReqDuration, trim_size)
trim_dt <- dt[ReqDuration < upper_limit & ReqDuration > lower_limit]
agg_dt <- trim_dt[, .(Value = mean(ReqDuration)), 
          by = time_window]

windows_in_day <- (60 / win_size) * 24

x <- msts(data = agg_dt$Value,
     seasonal.periods = c(windows_in_day, windows_in_day*7))
tbats_fit <- tbats(tsclean(x))
tbats_fc <- forecast(tbats_fit, h=(60/win_size)*24*7)
plot(tbats_fc)

x <- ts(agg_dt$Value, frequency = windows_in_day)
plot(x)
x_clean <- tsclean(x, replace.missing = TRUE)
lines(x_clean, col = 3)

seas1 <- fourier(x_clean, K=4)
seas2 <- fourier(ts(x_clean, freq=windows_in_day * 7), K=4)
arima_fit <- auto.arima(x_clean, xreg=cbind(seas1,seas2)
                  ,seasonal = FALSE
                  ,stepwise = FALSE
                  ,approximation = FALSE)
seas1.f <- fourier(x_clean, K=4, h=windows_in_day * 7)
seas2.f <- fourier(ts(x_clean, freq=windows_in_day * 7), 
                          K=4, h=windows_in_day * 7)
arima_fc <- forecast(arima_fit, 
                     xreg=cbind(seas1.f,seas2.f), 
                     h = 1)

plot(arima_fc)
plot(x_clean)
lines(fitted.Arima(arima_fit), col = 3)

accuracy(arima_fit)
accuracy(tbats_fit)