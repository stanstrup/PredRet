output$PREDSTATS_table <- renderDataTable({
  
  # Make a table with all relevant data
  mongo <- PredRet_connect()
  prediction_to_oids <- mongo.find.all(mongo=mongo,ns=PredRet.env$namespaces$ns_pred_stats,fields=list(sys_oid=1L),data.frame=T)
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo) 
  
  prediction_to_oids <- prediction_to_oids[,"sys_oid"]
  
  
  
  
  predstats       <- lapply(1:length(prediction_to_oids),function(x) t(pred_stat_get(prediction_to_oids[x])))
  predstats <- do.call(rbind,predstats)
  
  # Change sysid to sysname
  sys_names       <- sys_oid2name(prediction_to_oids)
  predstats <- cbind.data.frame(System = sys_names,predstats)
  
  
  # fix up formatting
  predstats[,2:ncol(predstats)]    <-    round(predstats[,2:ncol(predstats)],2)
  
  
  
  return(predstats)
})
