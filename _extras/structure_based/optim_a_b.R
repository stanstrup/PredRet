library(parallel)
library(rgl)


a <- seq(1,60,1)
b <- seq(0,1,0.05)

ab <- expand.grid(a,b)
colnames(ab) <- c("a","b")
ab <- split(ab, 1:NROW(ab))
ab <- lapply(ab,function(x) c(a = x$a, b = x$b, col = which(x$b==b), row = which(x$a==a))     )

median_rel_error <- matrix(data = as.numeric(NA) ,ncol=length(b),nrow=length(a))






cl <- makeCluster(detectCores())
clusterExport(cl,c("db_support","db"))

error_list <- parLapply(cl,ab, function(y){
  
                                                require(pracma)
                                                
                                                RT_pred <- sapply(db_support,function(x){
                                                  w <- sigmoid(x$sim, a = y["a"], b = y["b"])
                                                  weighted.mean(x$RT, w,na.rm =T)
                                                })
                                                
                                                error_rel <- abs(RT_pred-db$recorded_rt)/db$recorded_rt
                                                summary   <- aggregate(error_rel, list(system = db$system),function(x) median(    x   ,na.rm = T ))
                                                
                                                
                                                return(c(error = median(summary$x,na.rm = T), col = y["col"], row = y["row"]))
                          
                                              }
                        )

stopCluster(cl)





for(  i in 1:length(error_list)   ){
  median_rel_error[   error_list[[i]]["row.row"] , error_list[[i]]["col.col"]  ] <- error_list[[i]]["error"]
}


ncol  <- 1000
color <- rev(rainbow(ncol, start = 0/6, end = 4/6))
zcol  <- cut(median_rel_error, ncol)

persp3d(x=a, y=b, z = median_rel_error, col=color[zcol])
rgl.postscript("_extras/structure_based/sigmoidal_optimization.eps", fmt="eps")


min(median_rel_error)
temp <- which(median_rel_error==min(median_rel_error),arr.ind = TRUE)
a[temp[1]]
b[temp[2]]
rm(temp)
