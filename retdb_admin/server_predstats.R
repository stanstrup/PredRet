output$PREDSTATS_table <- renderDataTable({
  
  # Make a table with all relevant data
  prediction_to_oids <- unique(sapply(models(),function(x) x$oid_sys2))
  
  predstats       <- lapply(1:length(prediction_to_oids),function(x) t(pred_stat_get(prediction_to_oids[x],ns=ns_pred_stats)))
  predstats <- do.call(rbind,predstats)
  
  # Change sysid to sysname
  sys_names       <- sys_oid2name(prediction_to_oids)
  predstats <- cbind.data.frame(System = sys_names,predstats)
  
  
  # fix up formatting
  predstats[,2:ncol(predstats)]    <-    round(predstats[,2:ncol(predstats)],2)
  
  
  
  return(predstats)
})
