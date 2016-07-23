rm(list = ls())
source(file = "Auxiliary-Functions.R")

tuples_dt <- read_rds("synthetic_data", "tuples_dt")

win_size = 8

#######################
###Data Pre-Proccessing
#######################
real_leads <- fread("./synthetic_data/anomalies.csv")


setnames(real_leads, old = c("uniform feature", "binomial1 feature",
                             "binomial2 feature", "poisson feature")
         , new = c("uniform_feature", "binomial1_feature"
                   ,"binomial2_feature", "poisson_feature"))

op_on_columns(real_leads, c(2:3), fastPOSIXct)

setnames(real_leads, c("start time", "end time"), c("start_time", "end_time"))

real_anomaly_times <- fread("./synthetic_data/anomlies_timestamps.csv")
setnames(real_anomaly_times, "time stamps", "TimeStamp")
op_on_columns(real_anomaly_times, "TimeStamp", fastPOSIXct)
real_anomaly_times[, timestamp_window := align.time(TimeStamp, win_size * 60)]
real_anomaly_times <- unique(real_anomaly_times$timestamp_window)

predicted_anomaly_times <- names(tuples_dt)

####################################
###Test Correctly Detected Anomalies
####################################

mutual_anomaly_times <- intersect(as.character(real_anomaly_times),
                                          predicted_anomaly_times)

tuples_for_test <- tuples_dt[which(names(tuples_dt) %in% mutual_anomaly_times)]

real_leads[, start_window := align.time(start_time, win_size * 60)]
real_leads[, end_window := align.time(end_time, win_size * 60)]

positions <- list()
for (anomaly_time in mutual_anomaly_times)
{
  anomaly_row <- real_leads[start_window <= anomaly_time &
                              end_window >= anomaly_time]
  anomaly_row[, correct := "yes"]
  
  anomaly_tuples <- tuples_dt[[anomaly_time]]
  tuple_strings <- anomaly_tuples[tuple != "All", tuple]
  
  n <- nrow(anomaly_tuples)
  
  predicted_leads <- data.table(uniform_feature = rep(-1, n), 
                                binomial1_feature = rep(-1, n),
                                binomial2_feature = rep(-1, n),
                                poisson_feature = rep(-1, n))
  i <- 1L
  for (tuple_string in tuple_strings)
  {
    splitted <- str_split(tuple_string, ", ")[[1]] 
    for (feature in splitted)
    {
      feature_split <- str_split(feature, " = ")[[1]]
      column <- feature_split[1]
      value <- as.integer(feature_split[2])
      j <- which(colnames(predicted_leads) == column)
      i <- as.integer(i)
      set(predicted_leads, i = i, j = j, value = value)  
    }
    i <- i + 1
  }
  op_on_columns(predicted_leads, columns = 1:4, 
                op = function(x) {ifelse(x == -1, NA, x)})
  merged <- merge(predicted_leads, anomaly_row, 
                  by = names(predicted_leads), all.x = T, sort = F)
  lead_position <- which(merged$correct == "yes")
  positions[[anomaly_time]] <- lead_position
}

#######################################
###Compare Performance Vs. Random Model
#######################################

correct_positions <- as.numeric(unlist(positions))
accuracies <- c()
for (i in 1:10)
{
  correct_tuples <- length(which(correct_positions <= i))
  accuracy <- correct_tuples / length(positions)
  accuracies[i] <- accuracy
}
dt <- read_rds(table_name = "synthetic_data", object_name = "dt")
k_comb <- get_k_combinations(dt, features = names(predicted_leads), k = 3)
total <- 0
for (i in 1:3)
{
  total <- total + sum(sapply(k_comb[[i]], nrow))
}

random <- (1:10) / total

###########################
###Plot & Export Evaluation
###########################

plot(accuracies, ylim = c(0, 1), pch = 1, type = "b", 
     xlab = "Top K Results", ylab = "Accuracy", main = "Model Accuracy")
lines(random, col = 2, type = "b", pch = 2)
legend("right", legend = c("model", "random")
       ,pch = c(1, 2), col = c(1, 2))
model_vs_random <- data.table(Top_K_Leads = 1:10,
                              Model_Accuracy = round(accuracies, 3), 
                              Random_Accuracy = round(random, 3))

write.csv(model_vs_random, "Model Lead Accuracy.csv", row.names = FALSE)