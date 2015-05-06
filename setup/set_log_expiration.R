## Setting the log entries to automatically expire.

mongo.index.TTLcreate <- function(mongo, ns, field, index_name, expireAfterSeconds) {
  
  indexes = list()
  key=list()
  key[[field]] <- 1L

  indexes[["name"]] <- index_name
  indexes[["expireAfterSeconds"]] <- expireAfterSeconds
  indexes[["key"]] <- key
  
  
  listCreateIndex <- list(    createIndexes = sub(".*\\.", "", ns), indexes = list(indexes)  )                 
  
  bsonCreateIndex <- mongo.bson.from.list(listCreateIndex)
  mongo.command(mongo, db = gsub("\\..*","",ns), bsonCreateIndex)
  
}





mongo <- PredRet_connect()

mongo.index.TTLcreate(mongo=mongo, ns=PredRet.env$namespaces$ns_sysmodels_log, index_name="TTL_time", field="time",expireAfterSeconds=60*60*24*6)  # expire after 6 days

del <- mongo.disconnect(mongo)
del <- mongo.destroy(mongo) 
