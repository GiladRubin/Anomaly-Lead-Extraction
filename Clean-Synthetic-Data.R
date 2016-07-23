rm(list = ls())
source(file = "Auxiliary-Functions.R")

table_name <- "synthetic_data"
dt <- get_dt_from_table(table_name)
names(dt)
dt[, V1 := NULL]
dt[, TimeStampTrunc := NULL]
setnames(dt, "ReqDuration", "Value")
write.csv(dt, "./synthetic_data/synthetic_data.csv", row.names = F)