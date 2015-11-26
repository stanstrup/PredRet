## Setting the log entries to automatically expire.

library(rmongodb)


mongo <- PredRet_connect()

mongo.index.TTLcreate(mongo=mongo, ns=PredRet.env$namespaces$ns_sysmodels_log, index_name="TTL_time", field="time",expireAfterSeconds=60*60*24*6)  # expire after 6 days

del <- mongo.disconnect(mongo)
del <- mongo.destroy(mongo) 
