rmongodb.robj2data.frame <- function(x){
  
  attr.row.names <- attr.class <- R_OBJ <- NULL # This is needed to avoid a note from the package check about the "variables being missing"
  
  y <- as.data.frame(x)
  rownames(y) <- y$attr.row.names
  y <- subset(y,select = -attr.row.names)
  y <- subset(y,select = -attr.class)
  y <- subset(y,select = -R_OBJ)
  colnames(y) <- str_replace_all(colnames(y),"value.","")
  
  
  return(y)
}











pred_stat_make <- function(predicted_data) {
  
  
  predstats <- as.data.frame(matrix(ncol=1,nrow=10))
  colnames(predstats)=c(" ")
  rownames(predstats)=c("# Predictions made",
                        "# Predictions made where experimental RT is unknown",
                        "Mean prediction error*",
                        "Median prediction error*",
                        "95 % percentile prediction error*",
                        "Max prediction error*",
                        "Mean width of 95 % CI",
                        "Median width of 95 % CI",
                        "95 % percentile of 95 % CI width",
                        "Max width of 95 % CI")
  
  
  predstats[1,1] <- nrow(  predicted_data  )
  predstats[2,1] <- sum(is.na(predicted_data[,"recorded_rt"]))
  
  predstats[3,1] <- mean(abs(         predicted_data[,"recorded_rt"]   -    predicted_data[,"predicted_rt"]         ),na.rm = TRUE)
  predstats[4,1] <- median(abs(       predicted_data[,"recorded_rt"]   -    predicted_data[,"predicted_rt"]         ),na.rm = TRUE)
  predstats[5,1] <- quantile(abs(     predicted_data[,"recorded_rt"]   -    predicted_data[,"predicted_rt"]         ),probs = 0.95,na.rm = TRUE)
  predstats[6,1] <- max(abs(          predicted_data[,"recorded_rt"]   -    predicted_data[,"predicted_rt"]         ),na.rm = TRUE)
  
  predstats[7,1] <- mean(abs(          predicted_data[,"ci_upper"]   -    predicted_data[,"ci_lower"]         ),na.rm = TRUE)
  predstats[8,1] <- median(abs(        predicted_data[,"ci_upper"]   -    predicted_data[,"ci_lower"]         ),na.rm = TRUE)
  predstats[9,1] <- quantile(abs(      predicted_data[,"ci_upper"]   -    predicted_data[,"ci_lower"]         ),probs = 0.95,na.rm = TRUE)
  predstats[10,1] <- max(abs(          predicted_data[,"ci_upper"]   -    predicted_data[,"ci_lower"]         ),na.rm = TRUE)
  
  return(predstats)
  
}
