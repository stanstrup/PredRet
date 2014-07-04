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
system_combs_list <- lapply(1:NROW(system_combs), function(i) system_combs[i,,drop=FALSE] )

# get Comparision matrix from database
require(parallel)
cl <- makeCluster(detectCores())
clusterExport(cl, c("sys_comb_matrix","ns_rtdata"))
comb_matrixes=parLapply(cl,system_combs_list,function(x) {sys_comb_matrix(x[1],x[2],ns=ns_rtdata) })
stopCluster(cl)








# Remove compounds where we have no data in both systems and order the data
for(i in 1:length(comb_matrixes)){
  if(!is.null(comb_matrixes[[i]])){
    del = as.vector(apply(comb_matrixes[[i]],1,function(x) any(is.na(x))))
    comb_matrixes[[i]] = comb_matrixes[[i]][!del,]
    
    ord = order(comb_matrixes[[i]][,1])
    comb_matrixes[[i]] = comb_matrixes[[i]][ord,]
  }
}



# Building the model
# Using boot package
require(boot)
require("bisoreg")

fit=loess.wrapper(comb_matrixes[[i]][,1], comb_matrixes[[i]][,2], span.vals = seq(0.2, 1, by = 0.05), folds = nrow(comb_matrixes[[i]]))
loess.boot <- boot(comb_matrixes[[i]],loess.fun,R=1000,newdata=comb_matrixes[[i]][,1],span=fit$pars$span,parallel="multicore",ncpus=detectCores())
ci=boot2ci(loess.boot)

## loess.boot:
# t0: predicted y values
# t: predicted y values for each iteration
# data: original data


plot(loess.boot$data[,1],loess.boot$data[,2],pch=20)
lines(loess.boot$data[,1],ci[,1])
lines(loess.boot$data[,1],ci[,2],lty=3)
lines(loess.boot$data[,1],ci[,3],lty=3)



## Writing system
model_db_write(loess_boot=loess_boot,ci=ci,ns=ns_sysmodels,sysoid1=colnames(comb_matrixes[[i]])[1],sysoid2=colnames(comb_matrixes[[i]])[2])







# check if there are new data
# 1) deleted samples. nrow for any of the systems are not the same
# 2) an entry with a date newer than the calculation date exists. so newest data point used must be written to db too.
