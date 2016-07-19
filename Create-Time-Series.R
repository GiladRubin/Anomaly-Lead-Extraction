rm(list = ls())

source(file = "Auxiliary-Functions.R")

## Install Packages
ipak(c("data.table", "forecast", "zoo", "lubridate", "stringr", 
                                    "tsoutliers", "fasttime", "xts"))

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
# categorical_columns <- setdiff(colnames(dt), c("TimeStamp", "ReqDuration"))
# categorical_columns
# op_on_columns(dt, categorical_columns, function(x) {as.factor(x)})

## Subset dataset to contain only full data

## (Continuous data flow stopped on 2015-10-07 at 18:00)
dt <- dt[TimeStamp >= as.POSIXct('2016-01-01 00:00')
        & TimeStamp < as.POSIXct('2016-01-12 00:00')]

saveRDS(dt, "dt.rds")