rm(list = ls())

source(file = "Auxiliary-Functions.R")

## Install Packages
ipak(c("data.table"))

filenames <- list.files(path = "./Raw Data/new requests")

paths <- paste("./Raw Data/new requests/", filenames, 
                           "/requests.csv", sep = "")

combined_requests <- rbindlist(lapply(paths, fread))

write.csv(combined_requests, 
          "./Raw Data/new_requests.csv", 
          row.names = FALSE)
