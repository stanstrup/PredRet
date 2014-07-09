output$MODELSTATS_table <- renderDataTable({
  
  # Make a table with all relevant data
  models_table <- lapply(models(),function(x) data.frame(x$oid_sys1,x$oid_sys2,x$status,x$mean_error_abs,x$median_error_abs,x$q95_error_abs,x$max_error_abs,x$mean_ci_width_abs,x$median_ci_width_abs,x$q95_ci_width_abs,x$max_ci_width_abs,stringsAsFactors=F))
  
  models_table <- do.call(rbind,models_table)
    
  colnames(models_table)=c("Prediction from",
                           "Prediction to",
                           "Modelling status",
                           "Mean prediction error",
                           "Median prediction error",
                           "95 % percentile prediction error",
                           "Max prediction error",
                           "Mean of 95 % CI width",
                           "Median of 95 % CI width",
                           "95 % percentile of 95 % CI width",
                           "Max of 95 % CI width"
                           )
  
  
  # Change sysid to sysname
  sys_names = sys_oid2name(as.character(as.matrix(models_table[,c("Prediction from","Prediction to")])))
  dim(sys_names)=c(length(sys_names)/2,2)
  models_table[,c("Prediction from","Prediction to")] <- sys_names
  
  
  
  # fix up formatting
  models_table[c("Mean prediction error",
                       "Median prediction error",
                       "95 % percentile prediction error",
                       "Max prediction error",
                       "Mean of 95 % CI width",
                       "Median of 95 % CI width",
                       "95 % percentile of 95 % CI width",
                       "Max of 95 % CI width")] <-

  round(models_table[c("Mean prediction error",
                        "Median prediction error",
                        "95 % percentile prediction error",
                        "Max prediction error",
                        "Mean of 95 % CI width",
                        "Median of 95 % CI width",
                        "95 % percentile of 95 % CI width",
                        "Max of 95 % CI width")]
        ,2)
  
  
  
  return(models_table)
  })