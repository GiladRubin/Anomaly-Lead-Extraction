# input : Time series, anomaly time, forecasted value, sd
# output : ranked list of tuples

# find the time-window where the anomaly time is at
# assign an anomaly score to each event using forecasted value + confidence interval
# train regression tree and get the leaves (and their parents)
# get all leaves and parents that have mean that is an anomaly
# for each tuple prepare a vector of distances from the original time series without the tuple (agg. by original aggregation time)
# get p-values for the anomaly time for each tuple
# prepare a table with each tuple and its corresponding mean, variance and p-value

# train a classifier (logistic?) on all the data
# predict (probabilities) new instances using a pre-trained classifier
# return top k tuples
