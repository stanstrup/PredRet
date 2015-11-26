library(RJSONIO)
library(RCurl)
library(dplyr)

# grab the data
raw_data <- getURL("http://api.metabolomexchange.org/datasets")


# Then covert from JSON into a list
data <- fromJSON(raw_data)

# Flattening the data
data_flat <- lapply(data,unlist,recursive=FALSE)
data_flat <- lapply(data_flat, function(y) lapply(y, function(x) paste(x,collapse=";")))
data_flat <- lapply(data_flat,as.data.frame,stringsAsFactors=FALSE)

data_table <- bind_rows(data_flat)
data_table <- as.data.frame(data_table)



#data_table %>% select(accession,url,title,meta.analysis,meta.platform) %>% View


data_table_filtered <- data_table %>% filter(!grepl("NMR",meta.analysis)) %>% filter(!grepl("GC",meta.platform)) %>% filter(grepl("ST",accession))


MW_mwtab <- list()

for(i in 1:nrow(data_table_filtered)){
  
  MW_mwtab[[i]] <- getURL(paste0("http://www.metabolomicsworkbench.org/rest/study/study_id/",data_table_filtered[i,"accession"],"/mwtab"))
  
}

saveRDS(MW_mwtab,"MW_mwtab.rds")
# \n splits lines \t seperates things

