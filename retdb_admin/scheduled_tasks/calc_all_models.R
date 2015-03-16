## Basic settings ##################
source("../settings/mongodb.R",local=TRUE)


## functions ##################
source("../functions/FUNCTIONS.R",local=TRUE)



## Re-calc all models ##################
systems <- get_systems()

systems <- lapply(systems,function(x) {
                                          y <- x
                                          y$`_id` <- as.character.mongo.oid(y$`_id`)
                                          return(y)  
                                      }
                  )

systems <- data.frame(do.call(rbind, systems), stringsAsFactors=FALSE)
systems <- as.data.frame(lapply(systems,unlist), stringsAsFactors=FALSE)


sys_oids  = systems$X_id
system_combs = t(combn(sys_oids,2))
system_combs = as.matrix(system_combs)
system_combs = rbind(system_combs,system_combs[,c(2,1)])




for(i in 1:nrow(system_combs)){
  
  # if different column types don't make models
  if(    systems[system_combs[i,1]==systems$X_id, "system_column_type"]   !=  systems[system_combs[i,2]==systems$X_id, "system_column_type"]       )   next
  
  build_model(oid1=system_combs[i,1],oid2=system_combs[i,2],ns_sysmodels=ns_sysmodels,ns_rtdata=ns_rtdata,ns_sysmodels_log=ns_sysmodels_log,force=FALSE,withProgress=FALSE)
}
