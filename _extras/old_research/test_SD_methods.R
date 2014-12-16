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
fit_mono = monoproc(fit, bandwidth = 0.0001, mono1 = "increasing", gridsize=100) 


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


## Make fake loess class ########################
# # monotonic fit with predicted values at original x values
# fit_mono = monoproc(fit, bandwidth = 0.0001, mono1 = "increasing", gridsize=100,xx=rt1) # xx is values to predict. to compare original values I set the original "x values"
# 
# # move monotonic fit to original fit object
# fit_mono_fake=fit
# fit_mono_fake$fitted=fit_mono@fit@y
# fit_mono_fake$residuals=fit_mono_fake$y-fit_mono_fake$fitted
# 
# 
# lines(x_test,predict(fit_mono_fake, x_test),pch=20,col="blue")

## ---- Does not work. prediction is done based on old mode ----


## Bootstrap ########################
#Normal fit
fit.b=loess.boot(fit,R=500,new.xpts=rt1)
plot(fit.b,pch=20)




# General bootstrapping on normal fit
x=rt1
y=rt2

n <- length(x)
nboot <- 500
y_pred=matrix(nrow=n,ncol=nboot)

for (i in 1:nboot) {
  k.star <- sample(n, replace = TRUE)
  x.star <- x[k.star]
  y.star <- y[k.star]
  
  out.star <- loess(y.star ~ x.star, span=0.25)
  
  y_pred[,i]=predict(out.star, data.frame(x.star = x))
}


std = apply(y_pred,1,function(x) sd(x,na.rm = T))


ci=cbind(   predict(fit, x),
            predict(fit, x)    +    std*qnorm(1-.05/2),   # shouldn't std be divided by sqrt(n)?
            predict(fit, x)    -    std*qnorm(1-.05/2))


plot(x,y,pch=20)
lines(x,ci[,1])
lines(x,ci[,2],lty=3)
lines(x,ci[,3],lty=3)









# General bootstrapping on monotonic fit
x=rt1
y=rt2

n <- length(x)
nboot <- 500
y_pred=matrix(nrow=n,ncol=nboot)

for (i in 1:nboot) {
  k.star <- sample(n, replace = TRUE)
  x.star <- x[k.star]
  y.star <- y[k.star]
  
  out.star <- loess(y.star ~ x.star, span=0.25)
  
  #y_pred[,i]=predict(out.star, data.frame(x.star = x))
  y_pred[,i]= monoproc(out.star, bandwidth = 0.0001, mono1 = "increasing", gridsize=100,xx= x)@fit@y
}


std = apply(y_pred,1,function(x) sd(x,na.rm = T))


ci=cbind(   monoproc(fit, bandwidth = 0.0001, mono1 = "increasing", gridsize=100,xx=x)@fit@y,
            monoproc(fit, bandwidth = 0.0001, mono1 = "increasing", gridsize=100,xx=x)@fit@y    +    std*qnorm(1-.05/2),   # shouldn't std be divided by sqrt(n). but it becomes very small then?
            monoproc(fit, bandwidth = 0.0001, mono1 = "increasing", gridsize=100,xx=x)@fit@y    -    std*qnorm(1-.05/2))


plot(x,y,pch=20)
lines(x,ci[,1])
lines(x,ci[,2],lty=3)
lines(x,ci[,3],lty=3)













# Using boot package
newdata <- rt1
in_data <- cbind(rt1=rt1,rt2=rt2)
loess.fun <- function(in_data,inds,newdata){
  x.star <- rt1[inds]
  y.star <- rt2[inds]
  
  out.star <- loess(y.star ~ x.star, span=0.25)
  y_pred= monoproc(out.star, bandwidth = 0.0001, mono1 = "increasing", gridsize=100,xx= newdata)@fit@y
  
  return(y_pred)
}

loess.boot <- boot(in_data,loess.fun,R=1000,newdata=newdata,parallel="multicore",ncpus=detectCores())



# "manual" CI
# std = apply(loess.boot$t,2,function(x) sd(x,na.rm = T))
# 
# 
# ci=cbind(   monoproc(fit, bandwidth = 0.0001, mono1 = "increasing", gridsize=100,xx=newdata)@fit@y,
#             monoproc(fit, bandwidth = 0.0001, mono1 = "increasing", gridsize=100,xx=newdata)@fit@y    +    std*qnorm(1-.05/2),   # shouldn't std be divided by sqrt(n). but it becomes very small then?
#             monoproc(fit, bandwidth = 0.0001, mono1 = "increasing", gridsize=100,xx=newdata)@fit@y    -    std*qnorm(1-.05/2))
# 
# 
# plot(rt1,rt2,pch=20)
# lines(rt1,ci[,1])
# lines(rt1,ci[,2],lty=3)
# lines(rt1,ci[,3],lty=3)





# with the boot.ci function
temp=list()
for( i in 1:length(newdata)){
  temp[[i]]=list()
  temp[[i]][[1]]=i
  temp[[i]][[2]]=loess.boot
}


cl=makeForkCluster(nnodes=detectCores())
ci=parLapply(cl,temp,function(x) {
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
