# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

PredRet_get_db <- function(userID=NULL,exp_pred = c("exp","pred"), include_suspect = TRUE,systems = NULL){
 
  # Convert to get_user_data options
  if(all(exp_pred == c("exp","pred"))) generation <- NULL
  if(all(exp_pred == c("exp")))        generation <- 0
  if(all(exp_pred == c("pred")))       generation <- 1
  
  data <- get_user_data(userID     = userID,
                        generation = generation,
                        suspect    =   if(include_suspect){NULL}else{FALSE},
                        sys_id = if(is.null(systems)){NULL}else{name2sys_oid(systems)}
                        ) 
  
  # Select only daat useful to the user
  fields_to_get=c("system","name","pubchem","inchi","date added","username","generation","suspect")
  
  if(!is.null(generation)){
    if(generation==0L){
      fields_to_get = c(fields_to_get,"recorded_rt")
    }else{
      fields_to_get = c(fields_to_get,c("predicted_rt","ci_lower","ci_upper"))
    }
  }else{
    fields_to_get = c(fields_to_get,"recorded_rt",c("predicted_rt","ci_lower","ci_upper"))
  }
  
  data <- data[,fields_to_get]
  
  # Rename generation
  names(data)[which(names(data)=="generation")] <- "predicted"
  data$predicted[data$predicted==0] <- FALSE
  data$predicted[data$predicted==1] <- TRUE
  data$predicted <- as.logical(data$predicted)
  
  # Return data
  return(data)
}
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  

# Get systems
PredRet_get_chrom_systems <- function(){
  sys <- get_systems()
  for(i in 1:length(sys)){
    sys[[i]]$`_id`=NULL
    sys[[i]]$userID=NULL
  }
  
  return(sys)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Get models
PredRet_get_models <- function(from=NULL,to=NULL){
  models <- get_models(include.ci=TRUE,include.newdata=TRUE,include.xy_mat=TRUE,from_oid=if(is.null(from)){NULL}else{name2sys_oid(from)},to_oid=if(is.null(from)){NULL}else{name2sys_oid(to)})
  
  for(i in 1:length(models)){
    
    imodel <- list()
    imodel$predict_from <- sys_oid2name(models[[i]]$oid_sys1)
    imodel$predict_to   <- sys_oid2name(models[[i]]$oid_sys2)
    imodel$model_fit    <- data.frame(x = models[[i]]$newdata, models[[i]]$ci)
    imodel$model_points <- models[[i]]$xy_mat
    names(imodel$model_points)[1:2] <- c(imodel$predict_from,imodel$predict_to)
    imodel$stats        <- unlist(models[[i]][c("n_points","mean_error_abs","median_error_abs","q95_error_abs",
                                                "max_error_abs","mean_ci_width_abs","median_ci_width_abs",
                                                "q95_ci_width_abs","max_ci_width_abs")
                                              ]
                                  )
    
    models[[i]] <- imodel
  }
  
  return(models)
  
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

PredRet_list_models <- function(){
  
  # Select which fields to get
  fields       <- c("oid_sys1",
                    "oid_sys2")
  fields <- as.list(sapply(fields,function(x) x=1L))
  
  # Connect to db
  mongo <- PredRet_connect()
  data_back <- mongo.find.all(mongo, ns=PredRet.env$namespaces$ns_sysmodels,fields=fields)
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
  data_back <- lapply(data_back,unlist)
  data_back <- as.data.frame(do.call(rbind,data_back),stringsAsFactors = FALSE)
  data_back <- data_back[,c("oid_sys1","oid_sys2")]
  colnames(data_back) <- c("to","from")
  
  data_back <- apply(data_back, 2, sys_oid2name)
  
  
  return(data_back)
  
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #