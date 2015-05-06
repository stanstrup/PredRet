## packages ##################
library(PredRetR)

PredRet.env$predret_local <- TRUE


## Make the predictions ##################
models <- get_models() # change to models() if running in shiny
sys_models_oid2 <- sapply(models,function(x) x$oid_sys2)
sys_models_oid2_name <-  sys_oid2name(sys_models_oid2)
sys_models_oid2_name <- sys_models_oid2_name[!duplicated(sys_models_oid2)]
sys_models_oid2 <- sys_models_oid2[!duplicated(sys_models_oid2)]



mongo <- PredRet_connect()


# We get the time of the last entry in the rt database
data_time <- mongo.find.all(mongo,ns=PredRet.env$namespaces$ns_rtdata,   query=list(generation = 0L),   field=list(time=1L)    )
data_time_max <- max(sapply(data_time,function(x) x$time))



for(i in 1:length(sys_models_oid2)){
  
  # We get the time of the latest model that predicts to the system we are interested in
  # we check if:
  # 1) there is new data since last model build.
  model_time     <- mongo.find.all(mongo,ns=PredRet.env$namespaces$ns_sysmodels,query=list(oid_sys2 = sys_models_oid2[i]),field=list(time=1L))
  model_time_max <- max(sapply(model_time,function(x) x$time))
  
  # 2) if there has ever been any data predicted
  prev_predictions_count <- mongo.count(mongo,ns=PredRet.env$namespaces$ns_rtdata,query=list(sys_id=sys_models_oid2[i],generation=1))
  
  
  if(model_time_max > data_time_max & prev_predictions_count>0) next
  
  
  # If there is any new experimental data we predict all data again.
  purge_predictions(sys_id=sys_models_oid2[i]) # purge old predictions
  predicted_data <- predict_RT(predict_to_system=sys_models_oid2[i])
  
  
  
  if(is.null(predicted_data)) next
  
  
  for(i2 in 1:nrow(predicted_data)){
    # We check if we already have an identical prediction. If so then we don't rewrite the entry. To avoid updating the date
    criteria_exists <- list(
                             sys_id       = sys_models_oid2[i],
                             inchi        = predicted_data[i2,"inchi"],
                             predicted_rt = predicted_data[i2,"predicted_rt"]
                           )
    
    
    identical_count <- mongo.count(mongo, ns=PredRet.env$namespaces$ns_rtdata, query = criteria_exists)
    
    if(identical_count>0) next
    
    
    # Writing the data.
    bson_data = mongo.bson.from.df(predicted_data[i2,,drop=FALSE])
    
    criteria <- list(
                      sys_id     = sys_models_oid2[i],
                      inchi      = predicted_data[i2,"inchi"],
                      generation = 1L
                    )

    status = mongo.update(mongo, ns=PredRet.env$namespaces$ns_rtdata, criteria, objNew=bson_data[[1]],flags=1L)

  }
  
  
  # write predictions stats
  predstats <- pred_stat_make(predicted_data)
  pred_stat_write(predstats,sys_oid=sys_models_oid2[i])
  
  
}

del <- mongo.disconnect(mongo)
del <- mongo.destroy(mongo)

