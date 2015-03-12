## Basic settings ##################
source("../settings/mongodb.R",local=TRUE)
source("../settings/predictions.R",local=TRUE)


## functions ##################
source("../functions/FUNCTIONS.R",local=TRUE)



## get data needed ##################
rtdata <- get_user_data(ns=ns_rtdata,generation=0)

models <- get_models(include.loess=TRUE,include.ci=TRUE,include.newdata=TRUE,include.xy_mat=TRUE)
models_oid_sys1 <- sapply(models,function(x) x$oid_sys1)
models_oid_sys2 <- sapply(models,function(x) x$oid_sys2)





## Run through all entries ##################
suspects_initial <- list()

for(i in 1:nrow(rtdata)){
  
  ## Get the models where the rt could have been used.
  # the system of the entry
  entry_sys_id <- rtdata[i,"sys_id"]
  
  
  
  # run through all models to see if the entry is used in any of them
  model_select <- which(entry_sys_id==models_oid_sys1 | entry_sys_id==models_oid_sys2) # is the system from the entry used in any model?
  if(length(model_select)==0){next}
  
  suspects_initial[[i]] <- list()
  for(i2 in model_select){
    
    
    xy_mat <- models[[i2]]$xy_mat
    
    entry_select  <-   xy_mat$inchi==rtdata[i,"inchi"]
    if(!any(entry_select)){next} # is the inchi of the entry used in the model?
      
      
      # original values. should be same as in rtdata[i,"inchi"]
      x_org <- xy_mat[entry_select,1]
      y_org <- xy_mat[entry_select,2]
      
      
      # predicted value in this model
      pred_select <- which.min(abs(models[[i2]]$newdata-x_org))
      ci <- models[[i2]]$ci[pred_select,]
      ci <- as.data.frame(t(sapply(ci,unlist)))
      
      # see if outside CI*suspect_CI_multiplier
      ci_upper_lim <- with(ci,     (upper-pred)*suspect_CI_multiplier+pred        )
      ci_lower_lim <- with(ci,     pred-(pred-lower)*suspect_CI_multiplier        )
      
      
      if(!(y_org>ci_upper_lim | y_org<ci_lower_lim )){next} # if inside limits just go on
        
        
    
    suspects_initial[[i]][[i2]] <- data.frame(index = i , oid_sys1 = models[[i2]]$oid_sys1,oid_sys2 = models[[i2]]$oid_sys2)
      

  }

}



# function to remove NULL and zero length entries in a list
clean_list <- function(list){
  list_clean <- list[!sapply(list,function(x) is.null(x) | length(x)==0)]
  return(list_clean)
}


# Remove all NULL and zero length values (here the loop was skipped because the entry system was never used in a model or the entry was never used in a model)
suspects_initial_clean <- clean_list(suspects_initial)
suspects_initial_clean <- lapply(suspects_initial_clean,clean_list)
suspects_initial_clean <- suspects_initial_clean[sapply(suspects_initial_clean,length)>0]


#Making each entry data a data.frame
suspects_initial_clean <- lapply(suspects_initial_clean,function(x) do.call(rbind,x))


# Order the ids rowwise. this means that if 2 rows contain the same two sysids the rows are not identical.
suspects_initial_clean <- lapply(suspects_initial_clean,function(x){
  sorted = apply(x,1,function(y){
    order = order(y[c(2,3)])
    out <- y
    out[c(2,3)] <- out[c(2,3)][order]
    return(t(out)) 
  }                                                
)

return(as.data.frame(t(sorted),stringsAsFactors=F))
}

)



# Remove repeated rows
suspects_initial_clean <- lapply(suspects_initial_clean,unique)

# Get count of appearence of each system where entry is suspect
suspects_initial_clean_counts <- lapply(suspects_initial_clean,function(x) as.matrix(table(as.vector(as.matrix(x[,c(2,3)])))))


# Check when the entry is suspect in more than one system combination
n <- 0 # at least in one system. less safe and cannot pinpoint which system is to blame
suspects_initial_clean_hit <- sapply(suspects_initial_clean_counts,function(x) any(x>n))



final_suspects <- as.numeric(unique(do.call(rbind.data.frame,suspects_initial_clean[suspects_initial_clean_hit])[,1]))





# Set all to false. this is just to make the new field available. Done ones
# rtdata <- get_user_data(ns=ns_rtdata)
# ids <- rtdata$`_id`
# 
# mongo <- mongo.create()
# 
# for(i in 1:nrow(rtdata)){
#   
#   buf <- mongo.bson.buffer.create()
#   mongo.bson.buffer.append(buf, "_id", mongo.oid.from.string(ids[i]))
#   criteria <- mongo.bson.from.buffer(buf)
#   
#   suspect = list()
#   suspect[["suspect"]] = FALSE
#   mongo.update(mongo, ns=ns_rtdata, criteria,   list('$set'=suspect)      )
#   
# }
# 
# del <- mongo.disconnect(mongo)
# del <- mongo.destroy(mongo) 





# write results to db. all records updated
mongo <- mongo.create()

for(i in 1:nrow(rtdata)){
  
  buf <- mongo.bson.buffer.create()
  mongo.bson.buffer.append(buf, "_id", mongo.oid.from.string(rtdata[i,"_id"]))
  criteria <- mongo.bson.from.buffer(buf)
  
  
  suspect = list()
  
  if(any(final_suspects==i)){
  suspect[["suspect"]] = TRUE
  }else{
    suspect[["suspect"]] = FALSE
  }
  
  mongo.update(mongo, ns=ns_rtdata, criteria,   list('$set'=suspect)      )
  
}

del <- mongo.disconnect(mongo)
del <- mongo.destroy(mongo) 


