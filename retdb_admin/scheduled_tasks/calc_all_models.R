## Basic settings ##################
source("../settings/mongodb.R",local=TRUE)


## functions ##################
source("../functions/FUNCTIONS.R",local=TRUE)



## Re-calc all models ##################
sys_oids  = as.character(lapply(get_systems(),function(x) x$`_id`))

system_combs = t(combn(sys_oids,2))
system_combs = as.matrix(system_combs)
system_combs = rbind(system_combs,system_combs[,c(2,1)])

for(i in 1:nrow(system_combs)){
  build_model(oid1=system_combs[i,1],oid2=system_combs[i,2],ns_sysmodels=ns_sysmodels,ns_rtdata=ns_rtdata,ns_sysmodels_log=ns_sysmodels_log,force=FALSE,withProgress=FALSE)
}
