## Setting the log entries to automatically expire.
## does not work yet. Set expiration manually in some gui like Robomongo.
mongo <- mongo.create()

buf <- mongo.bson.buffer.create()
mongo.bson.buffer.append(buf, "time", 1L)
#mongo.bson.buffer.append(buf, "expireAfterSeconds", 60*60*24*6) # expire after 6 days
key <- mongo.bson.from.buffer(buf)

mongo.index.create(mongo, ns=ns_sysmodels_log, key)

del <- mongo.disconnect(mongo)
del <- mongo.destroy(mongo) 
