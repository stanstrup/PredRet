PredRet_get_db <- function(userID=NULL,exp_pred = c("exp","pred"), include_suspect = TRUE){
 
  # Convert to get_user_data options
  if(all(exp_pred == c("exp","pred"))) generation <- NULL
  if(all(exp_pred == c("exp")))        generation <- 0
  if(all(exp_pred == c("pred")))       generation <- 1
  
  data <- get_user_data(userID     = userID,
                        generation = generation,
                        suspect    =   if(include_suspect){NULL}else{FALSE}
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
  
  
  

# Get systems
# Get models

