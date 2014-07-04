## Functions for database query #####################


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
















sys_comb_matrix = function(oid1,oid2,ns)  {
  require(rmongodb)
  require(rmongodb.quick)
  
  
  ## get data for the combination of systems ################
  mongo <- mongo.create()
  
  buf <- mongo.bson.buffer.create()
  mongo.bson.buffer.append(buf, "sys_id", oid1)
  query <- mongo.bson.from.buffer(buf)
  rt_sys1 = mongo.find.all2(mongo=mongo, ns=ns,query=query,data.frame=T,mongo.oid2character=T)
  
  buf <- mongo.bson.buffer.create()
  mongo.bson.buffer.append(buf, "sys_id", oid2)
  query <- mongo.bson.from.buffer(buf)
  rt_sys2 = mongo.find.all2(mongo=mongo, ns=ns,query=query,data.frame=T,mongo.oid2character=T)
  
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
  if(is.null(rt_sys1) | is.null(rt_sys2)) return(NULL)
  
  data = rbind(rt_sys1,rt_sys2)
    
  ##
  
  
  
  ## setup comparision matrix ################
  unique_inchi = unique(data[,'inchi'])
  unique_systems = unique(data[,'sys_id'])
  unique_names = data[!duplicated(data[,'inchi']),'name']
  
  inchi_matrix = matrix(nrow=length(unique_inchi),ncol=length(unique_systems))
  colnames(inchi_matrix) = unique_systems
  rownames(inchi_matrix) = unique_names
  
  
  rt_matrix = matrix(nrow=length(unique_inchi),ncol=length(unique_systems))
  colnames(rt_matrix) = unique_systems
  rownames(rt_matrix) = unique_names
  
  
  for (i in 1:length(unique_inchi)){
    for (i2 in 1:length(unique_systems)){
      
      select = unique_inchi[i] == data[,'inchi']        &     unique_systems[i2] == data[,'sys_id']
      
      if (any(select)){  
        inchi_matrix[i,i2]=1   
        rt_matrix[i,i2]=mean(data[select,'rt'])
      }else{   
        inchi_matrix[i,i2]=0    
        rt_matrix[i,i2]=NA
      }
    }
  }
  ##
  
  
  return(list(rt=rt_matrix,newest_entry = max(c(rt_sys1$time,rt_sys2$time))))
  
}





get_ns <- function(ns){

    mongo <- mongo.create()
    data_back = mongo.find.all2(mongo, ns=ns)
    del <- mongo.disconnect(mongo)
    del <- mongo.destroy(mongo)
    
    return(data_back)
  }
  






## Functions for prediction #####################

loess.fun <- function(in_data,inds,newdata,span){
  require(monoProc)
  x.star <- in_data[,1][inds]
  y.star <- in_data[,2][inds]
  
  out.star <- loess(y.star ~ x.star, span=span)
  y_pred= monoproc(out.star, bandwidth = 0.1, mono1 = "increasing", gridsize=100,xx= newdata)@fit@y
  
  return(y_pred)
}





boot2ci <- function(loess.boot){
  # with the boot.ci function
  temp=list()
  for( i2 in 1:length(loess.boot$t0)){
    temp[[i2]]=list()
    temp[[i2]][[1]]=i2
    temp[[i2]][[2]]=loess.boot
  }
  
  
  cl <- makeCluster(detectCores())
  ci=parLapply(cl,temp,function(x) {
    require(boot)
    temp2=boot.ci(x[[2]],index=x[[1]],type="bca")
    
    ci=vector(mode="numeric",length=3)
    ci[1] = temp2$t0
    ci[c(2,3)] = temp2$bca[,c(4,5)]
    return(ci)
  })
  stopCluster(cl)
  ci = do.call(rbind,ci)
  
  return(ci)
}













model_db_write <- function(loess_boot,ci,ns,sysoid1,sysoid2,newest_entry){
  
  mongo <- mongo.create()
  buf <- mongo.bson.buffer.create()
  mongo.bson.buffer.append(buf, "loess_boot", serialize(loess_boot, NULL, FALSE))
  mongo.bson.buffer.append(buf, "ci", ci)
  mongo.bson.buffer.append(buf, "oid_sys1", sysoid1)
  mongo.bson.buffer.append(buf, "oid_sys2", sysoid2)
  mongo.bson.buffer.append(buf, "status", "done")
  mongo.bson.buffer.append(buf, "time", Sys.time())
  mongo.bson.buffer.append(buf, "n_points", nrow(loess_boot$data))
  mongo.bson.buffer.append(buf, "newest_entry", newest_entry)
  
  
  buf <- mongo.bson.from.buffer(buf)
  
  
  db_models_oids = mongo.find.all2(mongo, ns=ns,fields = list(oid_sys1=1L,oid_sys2=1L))
  db_models_oids_hit = unlist(lapply(db_models_oids,function(x) x$oid_sys1==sysoid1 & x$oid_sys2==sysoid2))
  
  if(any(db_models_oids_hit)){
    oids_to_update = lapply(db_models_oids, function(x) x$`_id`)
    oids_to_update = oids_to_update[[which(db_models_oids_hit)]]
    
    criteria <- mongo.bson.buffer.create()
    mongo.bson.buffer.append(criteria, "_id", oids_to_update)
    criteria <- mongo.bson.from.buffer(criteria)
    mongo.update(mongo, ns, criteria, buf)
    
  }else{
    mongo.insert(mongo, ns, buf)
  }
  
  
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
}




