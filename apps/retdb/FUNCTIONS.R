get_user_data <- function() {

  dbsystems <- get_systems()
  
  
  
## Only get users own data




mongo <- mongo.create()
ns <- "test2.rtdata"

data_all = mongo.find.all2(mongo=mongo, ns=ns,query=mongo.bson.empty(),data.frame=T,mongo.oid2character=T)

if(is.null(data_all)) return(NULL)

row.names(data_all) <- seq(nrow(data_all))

del <- mongo.disconnect(mongo)
del <- mongo.destroy(mongo)


# Take some data directly
data = data_all[,c("_id","sys_id","name","rt","pubchem","inchi")]


# Get correctly formatted time
data = cbind.data.frame(data , `date added` = as.POSIXct(data_all[,"time"],origin="1970-01-01")     ,stringsAsFactors = F)


# Get system name from system ID
sys_id_data = as.character(data_all[,"sys_id"])
sys_id_db = unlist(lapply(dbsystems,function(x) as.character.mongo.oid(x$`_id`))  )
sys_name = as.character(unlist(lapply(dbsystems,function(x) x$system_name)))  

data = cbind.data.frame(data , system = sys_name[match(sys_id_data,sys_id_db)]          ,stringsAsFactors = F)


# Format RT data
#data[,"rt"]      =     round(data[,"rt"],digits=2)

return(data)
}



get_systems <- function() {
# Connect to db
mongo <- mongo.create()
ns <- "test2.chrom_systems"


# select fields (think columns)
fields = mongo.bson.buffer.create()
mongo.bson.buffer.append(fields, "_id", 1L)
mongo.bson.buffer.append(fields, "sys_id", 1L)
mongo.bson.buffer.append(fields, "system_name", 1L)
mongo.bson.buffer.append(fields, "system_desc", 1L)
mongo.bson.buffer.append(fields, "userID", 1L)
mongo.bson.buffer.append(fields, "username", 1L)
fields = mongo.bson.from.buffer(fields)

data_back = mongo.find.all2(mongo, ns=ns,fields=fields)

del <- mongo.disconnect(mongo)
del <- mongo.destroy(mongo)

return(data_back)
}








mongo_del_oid = function(ns,oids)  {
  
  mongo <- mongo.create()
  
  # loop through rows to delete
  for(i in 1:length(oids)){
    buf <- mongo.bson.buffer.create()
    mongo.bson.buffer.append(buf, "_id", mongo.oid.from.string(oids[i]))
    criteria <- mongo.bson.from.buffer(buf)
    mongo.remove(mongo, ns, criteria)
  }
  
  # close connection
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
}

