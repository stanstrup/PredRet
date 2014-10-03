## Basic settings ##################
source("../settings/mongodb.R",local=TRUE)
source("../settings/predictions.R",local=TRUE)


## functions ##################
source("../functions/FUNCTIONS.R",local=TRUE)



## Make the predictions ##################
models <- get_models() # change to models() if running in shiny
sys_models_oid1 <- sapply(models,function(x) x$oid_sys1)
sys_models_oid1_name <-  sys_oid2name(sys_models_oid1)
sys_models_oid1_name <- sys_models_oid1_name[!duplicated(sys_models_oid1)]
sys_models_oid1 <- sys_models_oid1[!duplicated(sys_models_oid1)]



mongo <- mongo.create()


# We get the time of the last entry in the rt database
criteria_data_time <- mongo.bson.buffer.create()
mongo.bson.buffer.append(criteria_data_time, "generation", 0L)
criteria_data_time <- mongo.bson.from.buffer(criteria_data_time)

data_time <- mongo.find.all(mongo,ns=ns_rtdata,query=criteria_data_time,field=list(time=1L))
data_time_max <- max(sapply(data_time,function(x) x$time))



for(i in 1:length(sys_models_oid1)){
  
  # We get the time of the latest model that predicts to the system we are interested in
  # we check if:
  # 1) there is new data since last model build.
  criteria_model_time <- mongo.bson.buffer.create()
  mongo.bson.buffer.append(criteria_model_time, "oid_sys2", sys_models_oid1[i])
  criteria_model_time <- mongo.bson.from.buffer(criteria_model_time)
  model_time <- mongo.find.all(mongo,ns=ns_sysmodels,query=criteria_model_time,field=list(time=1L))
  
  model_time_max <- max(sapply(model_time,function(x) x$time))
  
  # 2) if there has ever been any data predicted
  prev_predictions_count <- mongo.count(mongo,ns=ns_rtdata,query=list(sys_id=sys_models_oid1[i],generation=1))
  
  
  if(model_time_max > data_time_max & prev_predictions_count>0) next
  
  
  # If there is any new experimental data we predict all data again.
  predicted_data <- predict_RT(sys_models_oid1[i])
  if(is.null(predicted_data)) next
  
  
  for(i2 in 1:nrow(predicted_data)){
    # We check if we already have an identical prediction. If so then we don't rewrite the entry. To avoid updating the date
    criteria_exists <- mongo.bson.buffer.create()
    mongo.bson.buffer.append(criteria_exists, "sys_id", sys_models_oid1[i])
    mongo.bson.buffer.append(criteria_exists, "inchi", predicted_data[i2,"inchi"])
    mongo.bson.buffer.append(criteria_exists, "predicted_rt", predicted_data[i2,"predicted_rt"])
    criteria_exists <- mongo.bson.from.buffer(criteria_exists)
    
    identical_count <- mongo.count(mongo, ns=ns_rtdata, query = criteria_exists)
    
    if(identical_count>0) next
    
    
    # Writing the data.
    bson_data = mongo.bson.from.df(predicted_data[i2,,drop=FALSE])
    
    criteria <- mongo.bson.buffer.create()
    mongo.bson.buffer.append(criteria, "sys_id", sys_models_oid1[i])
    mongo.bson.buffer.append(criteria, "inchi", predicted_data[i2,"inchi"])
    mongo.bson.buffer.append(criteria, "generation", 1L)
    criteria <- mongo.bson.from.buffer(criteria)

    status = mongo.update(mongo, ns=ns_rtdata, criteria, objNew=bson_data[[1]],flags=1L)

  }
  
  
  # write predictions stats
  predstats <- pred_stat_make(predicted_data)
  pred_stat_write(predstats,sys_oid=sys_models_oid1[i],ns=ns_pred_stats)
  
  
}

del <- mongo.disconnect(mongo)
del <- mongo.destroy(mongo)

