library("simpleboot")
library("bisoreg")
library("boot")
library("parallel")
library("monoProc")

ord = order(rt1)
rt1=rt1[ord]
rt2=rt2[ord]

## Normal and monotonic fit########################
fit=loess.wrapper(rt1, rt2, span.vals = seq(0.2, 1, by = 0.05), folds = length(rt1))
fit_mono = monoproc(fit, bandwidth = 0.1, mono1 = "increasing", gridsize=100) 


## Plots ########################
# normal fit
x_new = seq(from=0,to=5,by=0.01)
y_new = predict(fit,x_new)

plot(fit,pch=20)
lines(x_new,y_new,col=2)


x_test=seq(from=0.5,to=5,by=0.01)
fit_ci = cbind(
  predict(fit, x_test),
   predict(fit, x_test)    +    predict(fit, x_test, se=TRUE)$se.fit*qnorm(1-.05/2),
   predict(fit, x_test)    -    predict(fit, x_test, se=TRUE)$se.fit*qnorm(1-.05/2)
)


lines(x_test,fit_ci[,2],lty=2,col="red")
lines(x_test,fit_ci[,3],lty=2,col="red")

# monotonic
#plot(rt1,rt2,main='Retention time relationship between systems',xlab=comb_systems[i,1],ylab=comb_systems[i,2],pch=20)
lines(fit_mono@fit@x,fit_mono@fit@y,col=3)




## Bootstrap ########################


# Using boot package
newdata <- rt1
in_data <- cbind(rt1=rt1,rt2=rt2)
loess.fun <- function(in_data,inds,newdata){
  x.star <- in_data[,1][inds]
  y.star <- in_data[,2][inds]
  
  out.star <- loess(y.star ~ x.star, span=0.25)
  y_pred= monoproc(out.star, bandwidth = 0.1, mono1 = "increasing", gridsize=100,xx= newdata)@fit@y
  
  return(y_pred)
}

loess.boot <- boot(in_data,loess.fun,R=1000,newdata=newdata,parallel="multicore",ncpus=detectCores())



# with the boot.ci function
temp=list()
for( i in 1:length(newdata)){
  temp[[i]]=list()
  temp[[i]][[1]]=i
  temp[[i]][[2]]=loess.boot
}


cl=makeForkCluster(nnodes=detectCores())
ci=parLapply(cl,temp,function(x) {
  require(boot)
  temp2=boot.ci(x[[2]],index=x[[1]],type="bca")
  ci=vector(mode="numeric",length=3)
  ci[1] = temp2$t0
  ci[c(2,3)] = temp2$bca[,c(4,5)]
  return(ci)
  })
stopCluster(cl)
ci = do.call(rbind,ci)



plot(rt1,rt2,pch=20)
lines(newdata,ci[,1])
lines(newdata,ci[,2],lty=3)
lines(newdata,ci[,3],lty=3)
