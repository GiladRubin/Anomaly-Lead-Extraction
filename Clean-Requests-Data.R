rm(list = ls())

source(file = "Auxiliary-Functions.R")

## Install Packages
ipak(c("data.table", "forecast", "zoo", "lubridate", "stringr", 
       "tsoutliers", "fasttime", "xts"))

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
setnames(dt, "ReqDuration", "Value")
## Convert Timestamp String to Timestamp
dt[, TimeStamp := fastPOSIXct(TimeStamp, tz = "GMT")]
dt <- dt[order(TimeStamp)] ## Sort datatable according to TimeStamp

## Convert non-ascii letters to ASCII & Remove Non-Alphanumeric Letters
op_on_columns(dt, "Province", function (x) {iconv(x, "latin1", "ASCII", sub="")})
op_on_columns(dt, "Province", function (x) {str_replace_all(x, "[^[:alnum:]]", " ")})

## Subset dataset to contain only full data

## (Continuous data flow stopped on 2015-10-07 at 18:00)
dt <- dt[TimeStamp >= as.POSIXct('2016-01-01 00:00')
         & TimeStamp < as.POSIXct('2016-01-12 00:00')]

write.csv(dt, "./Raw Data/clean_requests.csv", row.names = FALSE)
