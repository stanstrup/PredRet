## packages ##################
library(PredRetR)

PredRet.env$predret_local <- TRUE



## Re-calc all models ##################
systems <- get_systems()

systems <- lapply(systems,function(x) {
                                          y <- x
                                          y$`_id` <- as.character.mongo.oid(y$`_id`)
                                          return(y)  
                                      }
                  )


systems <- do.call(rbind.data.frame ,systems)
systems <- as.data.frame(apply(systems,2,as.character),stringsAsFactors=FALSE)


sys_oids  = systems$X_id
system_combs = t(combn(sys_oids,2))
system_combs = as.matrix(system_combs)
system_combs = rbind(system_combs,system_combs[,c(2,1)])




for(i in 1:nrow(system_combs)){
  
  # if different column types don't make models
  if(    systems[system_combs[i,1]==systems$X_id, "system_column_type"]   !=  systems[system_combs[i,2]==systems$X_id, "system_column_type"]       )   next
  
  build_model(oid1=system_combs[i,1],oid2=system_combs[i,2],force=FALSE,withProgress=FALSE)
}
