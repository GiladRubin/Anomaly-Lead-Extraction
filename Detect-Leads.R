#load dataset correct format
table_name <- "new_requests"
path <- paste("./Raw Data/", table_name, ".csv", sep = "")
dt <- get_dt_from_table(path)
#find anomalies (iqr_factor, win_time)
#analyze anomalies and save into folder