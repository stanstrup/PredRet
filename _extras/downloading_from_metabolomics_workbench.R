library(RJSONIO)
library(RCurl)
library(dplyr)

# grab the data
raw_data <- getURL("http://metabolomexchange.org/api/dataset/")


# Then covert from JSON into a list
data <- fromJSON(raw_data)

# Flattening the data
data_flat <- lapply(data,unlist,recursive=FALSE)
data_flat <- lapply(data_flat, function(y) lapply(y, function(x) paste(x,collapse=";")))
data_flat <- lapply(data_flat,as.data.frame,stringsAsFactors=FALSE)

data_table <- bind_rows(data_flat)
data_table <- as.data.frame(data_table)
