
mongo_del_oid = function(ns,oids)  {
  
  mongo <- PredRet_connect()
  
  # loop through rows to delete
  for(i in 1:length(oids)){
    mongo.remove(mongo, ns, criteria=list(`_id` = mongo.oid.from.string(oids[i]))    )
  }
  
  # close connection
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
}


wrote_model_log <- function(sysoid1,sysoid2,msg){
  
  b <- list(oid_sys1 = sysoid1,
            oid_sys2 = sysoid2,
            msg      = msg,
            time     = Sys.time()
  )
  
  mongo <- PredRet_connect()
  mongo.insert(mongo, ns=PredRet.env$namespaces$ns_sysmodels_log, b)
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo) 
}


purge_predictions <- function(sys_id=NULL){
  mongo <- PredRet_connect()
  
  
  if(is.null(sys_id)){
    mongo.remove(mongo, ns=PredRet.env$namespaces$ns_rtdata, criteria = list(generation = 1))
    mongo.drop(  mongo, ns=PredRet.env$namespaces$ns_pred_stats)
  }else{
    mongo.remove(mongo, ns=PredRet.env$namespaces$ns_rtdata    , criteria = list(generation = 1,sys_id=sys_id)  )
    mongo.remove(mongo, ns=PredRet.env$namespaces$ns_pred_stats, criteria = list(sys_oid=sys_id)                )
  }
  
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo) 
  
}




set_model_status <- function(sysoid1,sysoid2,status){
  
  mongo <- PredRet_connect()
  
  db_models_oids = mongo.find.all(mongo, ns=PredRet.env$namespaces$ns_sysmodels,fields = list(oid_sys1=1L,oid_sys2=1L))
  db_models_oids_hit = unlist(lapply(db_models_oids,function(x) x$oid_sys1==sysoid1 & x$oid_sys2==sysoid2))
  
  
  if(any(db_models_oids_hit)){
    oids_to_update = lapply(db_models_oids, function(x) x$`_id`)
    oids_to_update = oids_to_update[[which(db_models_oids_hit)]]
    
    status <- mongo.update(mongo, ns=PredRet.env$namespaces$ns_sysmodels, criteria = list(`_id` = oids_to_update),   objNew = list("$set"=list("status"=status))            )
    
  }else{
    status = mongo.insert(mongo,  ns=PredRet.env$namespaces$ns_sysmodels,   list(oid_sys1 = sysoid1, oid_sys2 = sysoid2, status = status)     )
  }
  
  
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
}




model_db_write <- function(loess_boot,
                           xy_mat,
                           ci,
                           newdata,
                           sysoid1,
                           sysoid2,
                           newest_entry,
                           mean_error_abs,
                           median_error_abs,
                           q95_error_abs,
                           max_error_abs,
                           mean_ci_width_abs,
                           median_ci_width_abs,
                           q95_ci_width_abs,
                           max_ci_width_abs
){
  
  
  
  
  # Serialize loess_boot
  temp = loess_boot
  temp$statistic <- NULL  # This need to be removed since it references the whole calling environment
  temp$t <- NULL  # we don't need the result from all the simulations.
  temp = serialize(temp, connection = NULL, ascii = FALSE)
  #print(object.size(temp),units = "Kb")
  
  
  
  
  # Make the object to write to the db
  mongo <- PredRet_connect()
  buf <- mongo.bson.buffer.create()
  
  mongo.bson.buffer.append(buf, "loess_boot", temp)
  
  mongo.bson.buffer.append(buf, "ci", mongo.bson.from.df(data.frame(pred=ci[,1],lower=ci[,2],upper=ci[,3])))
  mongo.bson.buffer.append(buf, "xy_mat", mongo.bson.from.df(as.data.frame(xy_mat)))
  mongo.bson.buffer.append(buf, "newdata", newdata)
  mongo.bson.buffer.append(buf, "oid_sys1", sysoid1)
  mongo.bson.buffer.append(buf, "oid_sys2", sysoid2)
  mongo.bson.buffer.append(buf, "status", "done")
  mongo.bson.buffer.append(buf, "time", Sys.time())
  mongo.bson.buffer.append(buf, "n_points", nrow(loess_boot$data))
  mongo.bson.buffer.append(buf, "newest_entry", newest_entry)
  
  mongo.bson.buffer.append(buf, "mean_error_abs", mean_error_abs)
  mongo.bson.buffer.append(buf, "median_error_abs", median_error_abs)
  mongo.bson.buffer.append(buf, "q95_error_abs", as.numeric(q95_error_abs))
  mongo.bson.buffer.append(buf, "max_error_abs", max_error_abs)
  
  mongo.bson.buffer.append(buf, "mean_ci_width_abs", mean_ci_width_abs)
  mongo.bson.buffer.append(buf, "median_ci_width_abs", median_ci_width_abs)
  mongo.bson.buffer.append(buf, "q95_ci_width_abs", as.numeric(q95_ci_width_abs))
  mongo.bson.buffer.append(buf, "max_ci_width_abs", max_ci_width_abs)
  
  buf <- mongo.bson.from.buffer(buf)
  
  
  db_models_oids = mongo.find.all(mongo, ns=PredRet.env$namespaces$ns_sysmodels,fields = list("_id"=1L,oid_sys1=1L,oid_sys2=1L),mongo.oid2character = FALSE)
  db_models_oids_hit = unlist(lapply(db_models_oids,function(x) x$oid_sys1==sysoid1 & x$oid_sys2==sysoid2))
  
  if(any(db_models_oids_hit)){
    oids_to_update = lapply(db_models_oids, function(x) x$`_id`)
    oids_to_update = oids_to_update[[which(db_models_oids_hit)]]
    
    criteria <- mongo.bson.buffer.create()
    mongo.bson.buffer.append(criteria, "_id", oids_to_update)
    criteria <- mongo.bson.from.buffer(criteria)
    
    mongo.remove(mongo, PredRet.env$namespaces$ns_sysmodels, criteria)
  }
  
  
  
  success <- mongo.insert(mongo, PredRet.env$namespaces$ns_sysmodels, buf)
  
  
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
  if(success){
    wrote_model_log(msg="New model data was successfully written to the database.",sysoid1=sysoid1,sysoid2=sysoid2)
  }else{
    wrote_model_log(msg="Attempt to write new model data to the database failed.",sysoid1=sysoid1,sysoid2=sysoid2)
  }
  
  return(success)
}












pred_stat_write <- function(predstats,sys_oid) {
  
  predstats_list <- as.list(t(predstats))
  predstats_list <- c(sys_oid,predstats_list)
  names(predstats_list) <- c("sys_oid",rownames(predstats))
  
  criteria <- list(sys_oid=sys_oid)
  
  
  mongo <- PredRet_connect()
  mongo.remove(mongo, ns=PredRet.env$namespaces$ns_pred_stats, criteria) # Have to remove first. Cannot get updating to work.
  status <- mongo.insert(mongo, ns=PredRet.env$namespaces$ns_pred_stats, predstats_list)
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo) 
}



