# Writing system
# mongo <- mongo.create()
# buf <- mongo.bson.buffer.create()
# mongo.bson.buffer.append(buf, "fit", serialize(fit, NULL, FALSE))
# mongo.insert(mongo, "test2.modeltest", mongo.bson.from.buffer(buf))
# 
# del <- mongo.disconnect(mongo)
# del <- mongo.destroy(mongo)




# Reading back system system
# mongo <- mongo.create()
# ns <- "test2.modeltest"
# data_back = mongo.find.all2(mongo, ns=ns)
# 
# del <- mongo.disconnect(mongo)
# del <- mongo.destroy(mongo)
# 
# fit_db = unserialize(data_back[[1]]$fit)











# Get all systems in the db
systems_all = get_systems() # replace with systems_in_db()

# get oids
systems_all_oids = as.character(unlist(lapply(systems_all,function(x)  as.character.mongo.oid(x$`_id`))))  

# make all combinations of system oids
system_combs = t(combn(systems_all_oids,2))
system_combs = as.matrix(system_combs)
system_combs = rbind(system_combs,system_combs[,c(2,1)])
system_combs_list <- lapply(1:NROW(system_combs), function(i) system_combs[i,,drop=FALSE] )
# select here which systems combinations to pull by indexing system_combs_list


# get Comparision matrix from database
require(parallel)
cl <- makeCluster(detectCores())
clusterExport(cl, c("sys_comb_matrix","ns_rtdata"))
comb_matrixes=parLapply(cl,system_combs_list,function(x) {sys_comb_matrix(x[1],x[2],ns=ns_rtdata) })
stopCluster(cl)




# Remove compounds where we have no data in both systems and order the data
for(i in 1:length(comb_matrixes)){
  if(!is.null(comb_matrixes[[i]]$rt)){
    del = as.vector(apply(comb_matrixes[[i]]$rt,1,function(x) any(is.na(x))))
    comb_matrixes[[i]]$rt = comb_matrixes[[i]]$rt[!del,]
    
    ord = order(comb_matrixes[[i]]$rt[,1])
    comb_matrixes[[i]]$rt = comb_matrixes[[i]]$rt[ord,]
  }
}






# Building the model
# Using boot package
require(boot)
require("bisoreg")

# get info about current models
sys_models = get_ns(ns_sysmodels)
sys_models_newest_entry = lapply(sys_models,function(x) x$newest_entry)
sys_models_n_points = sapply(sys_models,function(x) x$n_points)

sys_models_oid1 = sapply(sys_models,function(x) x$oid_sys1)
sys_models_oid2 = sapply(sys_models,function(x) x$oid_sys2)



for(i in 1:length(comb_matrixes)){
  
  if(   is.null(comb_matrixes[[i]]$rt) )     next        
  if(  nrow(comb_matrixes[[i]]$rt)<10    )   next
  
  
  # check if we already have newest data point in the calculation or if data was deleted.
  if(!(length(sys_models)==0)){ # there are no systems at all
  select    = colnames(comb_matrixes[[i]]$rt)[1]==sys_models_oid1 & colnames(comb_matrixes[[i]]$rt)[2]==sys_models_oid2
  is_newer  = sys_models_newest_entry[[which(select)]]<comb_matrixes[[i]]$newest_entry
  same_nrow = sys_models_n_points[select] == nrow(comb_matrixes[[i]]$rt) 
  
  if(!(is_newer | !same_nrow)) next
  rm(select,is_newer,same_nrow)
  }
  
  fit=loess.wrapper(comb_matrixes[[i]]$rt[,1], comb_matrixes[[i]]$rt[,2], span.vals = seq(0.2, 1, by = 0.05), folds = nrow(comb_matrixes[[i]]$rt)) 
  loess.boot <- boot(comb_matrixes[[i]]$rt,loess.fun,R=1000,newdata=comb_matrixes[[i]]$rt[,1],span=fit$pars$span,parallel="multicore",ncpus=detectCores())
  ci=boot2ci(loess.boot)
  
  ## loess.boot:
  # t0: predicted y values
  # t: predicted y values for each iteration
  # data: original data
  
      
  
  ## Writing system
  model_db_write(loess_boot=loess.boot,
                 ci=ci,
                 ns=ns_sysmodels,
                 sysoid1=colnames(comb_matrixes[[i]]$rt)[1],
                 sysoid2=colnames(comb_matrixes[[i]]$rt)[2],
                 newest_entry=comb_matrixes[[i]]$newest_entry,
                 mean_error_abs=mean(abs(loess.boot$data[,2]-ci[,1])),
                 median_error_abs=median(abs(loess.boot$data[,2]-ci[,1])),
                 q95_error_abs=quantile(abs(loess.boot$data[,2]-ci[,1]),0.95),
                 max_error_abs=max(abs(loess.boot$data[,2]-ci[,1])),
                 mean_ci_width_abs=mean(ci[,3]-ci[,2]),
                 median_ci_width_abs=median(ci[,3]-ci[,2]),
                 q95_ci_width_abs=quantile(ci[,3]-ci[,2],0.95),
                 max_ci_width_abs=max(ci[,3]-ci[,2])
                 )
  
}






plotdata=cbind.data.frame(x=loess.boot$data[,1],
                          y=loess.boot$data[,2],
                          predicted=ci[,1],
                          lower=ci[,2],
                          upper=ci[,3],
                          name=rownames(loess.boot$data),
                          tooltip=paste0('<b>',rownames(loess.boot$data),'</b><br />','x: ',round(loess.boot$data[,1],2),'<br />y: ',round(loess.boot$data[,2],2))
)




p = plot_systems(plotdata)
p
