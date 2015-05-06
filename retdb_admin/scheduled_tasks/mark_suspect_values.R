## packages ##################
library(PredRetR)

PredRet.env$predret_local <- TRUE



## get data needed ##################
rtdata <- get_user_data(generation=0)

models <- get_models(include.loess=TRUE,include.ci=TRUE,include.newdata=TRUE,include.xy_mat=TRUE)
models_oid_sys1 <- sapply(models,function(x) x$oid_sys1)
models_oid_sys2 <- sapply(models,function(x) x$oid_sys2)


unique_inchi <- as.matrix(table(rtdata$inchi))
unique_inchi <- rownames(unique_inchi)[unique_inchi>1]


## Reset suspect list ##################
# mongo <- PredRet_connect()
# mongo.update(mongo, ns=PredRet.env$namespaces$ns_rtdata, criteria=mongo.bson.empty(),   list('$set'=list(suspect=FALSE))  ,flags=2L    )
# del <- mongo.disconnect(mongo)
# del <- mongo.destroy(mongo)



## Go through all inchis and locate suspect values ##################
suspect <- data.frame(matrix(ncol=2,nrow=0))
colnames(suspect) <- c("sysid","inchi")


for(i in 1:length(unique_inchi)){
  
  # Make comparison table for the inchi
  # The table will say in which models one of the values is suspect
  select <- rtdata$inchi==unique_inchi[i]
  sys_select <- rtdata$sys_id[select]
  
  bad_mat <- matrix(ncol=sum(select),nrow=sum(select))
  colnames(bad_mat) <- rownames(bad_mat) <- sys_select
  
  
  
  # Go through the matrix point by point and fill in the info
  for(row in 1:nrow(bad_mat)){
    for(col in 1:ncol(bad_mat)){
      
      model_select <- models_oid_sys1 == rownames(bad_mat)[row] & models_oid_sys2 == colnames(bad_mat)[col]
      if(all(!model_select)) next
      
      
      # Get the model      
      xy_mat <- models[[which(model_select)]]$xy_mat
      
      entry_select  <-   xy_mat$inchi==unique_inchi[i]
      if(!any(entry_select)){next} # is the inchi of the entry used in the model?
      
      
      # original values. should be same as in rtdata[i,"inchi"]
      x_org <- xy_mat[entry_select,1]
      y_org <- xy_mat[entry_select,2]
      
      
      # predicted value in this model
      pred_select <- which.min(abs(models[[which(model_select)]]$newdata-x_org))
      ci <- models[[which(model_select)]]$ci[pred_select,]
      ci <- as.data.frame(t(sapply(ci,unlist)))
      
      # see if outside CI*suspect_CI_multiplier
      ci_upper_lim <- with(ci,     (upper-pred)*PredRet.env$suspect$suspect_CI_multiplier+pred        )
      ci_lower_lim <- with(ci,     pred-(pred-lower)*PredRet.env$suspect$suspect_CI_multiplier        )
      
      
      if(!(y_org>ci_upper_lim | y_org<ci_lower_lim )){
        bad_mat[row,col]=FALSE# if inside limits
      }else{bad_mat[row,col]=TRUE # if outside limits
      } 
      
    }
  }
  
  
  
  
  # if noone is suspect go on to the next inchi
  if(all(!bad_mat,na.rm = TRUE)){next} 
  
  
  
  # If there are suspect values but never in more than system combination
  # Then we can never know who is to blame so we mark both of them
  # If systems exist both ways we only consider it a suspect value if it is suspect both ways
  if(!(any(rowSums(bad_mat,na.rm = TRUE) > 1) | any(colSums(bad_mat,na.rm = TRUE) > 1))){
    bad_mat_entries <- which(bad_mat,arr.ind = TRUE)
    
    for(i2 in 1:nrow(bad_mat_entries)){
#      if(     bad_mat[bad_mat_entries[i2,1],bad_mat_entries[i2,2]]      &      bad_mat[bad_mat_entries[i2,2],bad_mat_entries[i2,1]]             ){
        suspect[(nrow(suspect)+1):(nrow(suspect)+2),] <- data.frame(sysid=sys_select[bad_mat_entries[i2,]],inchi=unique_inchi[i],stringsAsFactors=F)
#      }
    }
    
  }
  
  
  # If there are suspect values but some values are suspect in more than system combination
  # then we can try to pinpoint who is to blame
  if((any(rowSums(bad_mat,na.rm = TRUE) > 1) | any(colSums(bad_mat,na.rm = TRUE) > 1))){
    bad_mat_entries <- which(rowSums(bad_mat,na.rm = TRUE) > 1 | colSums(bad_mat,na.rm = TRUE) > 1)
    suspect[(nrow(suspect)+1):(nrow(suspect)+length(bad_mat_entries)),] <- data.frame(sysid=sys_select[bad_mat_entries],inchi=unique_inchi[i],stringsAsFactors=F)
  }
  
  
  
} 



suspect <- unique(suspect)






# db connection
mongo <- PredRet_connect()


# Reset suspect list
#mongo.update(mongo, ns=PredRet.env$namespaces$ns_rtdata, criteria=mongo.bson.empty(),   list('$set'=list(suspect=FALSE))  ,flags=2L    )



# write results to db. all records updated
for(i in 1:nrow(suspect)){
  criteria <- list(sys_id=suspect$sysid[i]  ,inchi=suspect$inchi[i] ,generation=0)
  
  old_suspect <- mongo.find.all(mongo, ns=PredRet.env$namespaces$ns_rtdata, query=criteria,fields=list(suspect = 1L))
  old_suspect <- old_suspect[[1]]$suspect
  
  if(!old_suspect){
  mongo.update(mongo, ns=PredRet.env$namespaces$ns_rtdata, criteria=criteria,  objNew = list(  '$set'=list(suspect=TRUE,time = Sys.time())     )     )
  }
  
  
}



del <- mongo.disconnect(mongo)
del <- mongo.destroy(mongo) 
