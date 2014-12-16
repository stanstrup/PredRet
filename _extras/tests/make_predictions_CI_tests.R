require(boot)
require(bisoreg)
require(monoproc)

loess.fun <- function(in_data,inds,newdata,span){
  require(monoProc)
  x.star <- in_data[,1][inds]
  y.star <- in_data[,2][inds]
  
  out.star <- loess(y.star ~ x.star, span=span, control = loess.control(surface = "direct")  ) # direct is needed, otherwise it occationally blows up. I assume some border situations.
  y_pred   <- monoproc(out.star, bandwidth = 0.1, mono1 = "increasing", gridsize=100,xx= newdata)@fit@y
  
  y_pred[y_pred < 0] = 0
  
  return(y_pred)
}



boot2ci <- function(loess.boot){
  require(parallel)
  
  temp <- list()
  for( i2 in 1:length(loess.boot$t0)){
    temp[[i2]]=list()
    temp[[i2]][[1]]=i2
    temp[[i2]][[2]]=loess.boot
  }
  
  
  ci <- lapply(temp,function(x) {
    require(boot)
    temp2=boot.ci(x[[2]],index=x[[1]],type="perc")
    
    ci <- vector(mode="numeric",length=3)
    ci[1] <- temp2$t0
    ci[c(2,3)] <- temp2$percent[,c(4,5)]
    return(ci)
  })
  ci <- do.call(rbind,ci)
  
  return(ci)
}



boot2ci_PI <- function(loess.boot,newdata,alpha=0.05){
  require(parallel)
  
  # SD at each  x values (newdata) between all bootstrap iterations
  loess_sd_newdata <- apply(loess.boot$t,2,sd)
  
  
  
  ## we need to interpolate from the newdata x values to the values used for the model
  # then we calculate
  # 1) sd of the residuals between the fit and the original values
  # 2) sd of each predicted value between all bootstrap iterations
  
  loess_sd <- numeric(length=nrow(loess.boot$data))
  res_sd <- numeric(length=nrow(loess.boot$data))
  
  
  for( i in 1:nrow(loess.boot$data)){
    
    
    if(any(newdata==loess.boot$data[i,"x"])){ # the predicted points match the model building x'es
      loc <- which(newdata==loess.boot$data[i,"x"])
      res <- loess.boot$t[,loc]-loess.boot$data[i,"y"]
      res_sd[i] <- sd(res)
    }else{ # we have to interpolate to the y at the model x'es. We do it linearly between the two nearest prediction x'es
      
      lower_idx <- which(newdata < loess.boot$data[i,"x"])
      lower_idx <- lower_idx[length(lower_idx)]
      higher_idx <- which(newdata > loess.boot$data[i,"x"])[1]
      
      
      pred_y <- numeric(length=nrow(loess.boot$t)) 
      for( i2 in 1:nrow(loess.boot$t)){
        pred_y[i2] <- approx(x = c(newdata[lower_idx],newdata[higher_idx]), y = c(loess.boot$t[i2,lower_idx],loess.boot$t[i2,higher_idx]),  xout = loess.boot$data[i,"x"])$y
      }
      
      res <- pred_y-loess.boot$data[i,"y"]
      res_sd[i] <- sd(res)
      
      
      loess_sd[i] <- approx(x = c(newdata[lower_idx],newdata[higher_idx]), y = c(loess_sd_newdata[lower_idx],loess_sd_newdata[higher_idx]),  xout = loess.boot$data[i,"x"])$y
    }  
    
  }
  
  
  # Now we combine the SDs
  SD_combined <- sqrt(loess_sd^2+res_sd^2)
  
  
  
  
  
  # We now have the SDs at the x values used to build the model.
  # We now need to interpolate back to the values we have chosen to predict (newdata)
  
  SD_combined_newdata <- numeric(length=length(newdata))
  
  for(i in 1:length(newdata)){
    if(any(newdata[i]==loess.boot$data[,"x"])){ 
       loc <- which(newdata[i]==loess.boot$data[,"x"])
       SD_combined_newdata[i] <- SD_combined[loc]
      
    }else{ 
    lower_idx <- which(newdata[i] > loess.boot$data[,"x"])
    lower_idx <- lower_idx[length(lower_idx)]
    higher_idx <- which(newdata[i] < loess.boot$data[,"x"])[1]
  
    SD_combined_newdata[i] <- approx(x = c(loess.boot$data[lower_idx,"x"],loess.boot$data[higher_idx,"x"]), y = c(SD_combined[lower_idx],SD_combined[higher_idx]),  xout = newdata[i])$y
    }
  }
  
  
# Finally we make a matrix with the fitted value and the lower and upper bounds of the CI.
ci=cbind(   loess.boot$t0,
            loess.boot$t0    -    SD_combined_newdata*qnorm(1-alpha/2),
            loess.boot$t0    +    SD_combined_newdata*qnorm(1-alpha/2))


  
  return(ci)
}











# Good data
x <- c(0.51,0.52,0.53,0.54,0.55,0.55,0.56,0.56,0.57,0.57,0.57,0.57,0.57,0.57,0.57,0.58,0.58,0.58,0.59,0.6,0.6,0.63,0.66,0.66,0.68,0.68,0.68,0.71,0.72,0.75,0.75,0.75,0.79,0.8,0.84,0.85,0.88,0.92,0.97,0.97,0.99,0.99,1.02,1.02,1.03,1.06,1.16,1.17,1.19,1.26,1.26,1.27,1.28,1.28,1.28,1.28,1.29,1.3,1.31,1.34,1.35,1.37,1.37,1.37,1.38,1.39,1.39,1.4,1.43,1.43,1.44,1.45,1.45,1.47,1.5,1.5,1.53,1.53,1.54,1.61,1.63,1.65,1.68,1.68,1.69,1.69,1.7,1.7,1.7,1.71,1.72,1.74,1.77,1.78,1.78,1.81,1.87,1.91,2.03,2.08,2.13,2.13,2.26,2.41,2.58,2.61,2.63,2.76,2.85,2.98,3.06,3.33,3.38,3.4,3.46,3.46,3.56,3.92,3.92,3.94,4,4.08,4.27,4.4,4.5)
y <- c(0.4,0.41,0.42,0.44,0.45,0.43,0.47,0.46,0.46,0.46,0.47,0.47,0.48,0.47,0.47,0.47,0.48,0.48,0.48,0.48,0.49,0.51,0.51,0.5,0.53,0.53,0.54,0.53,0.59,0.53,0.62,0.63,0.66,0.55,0.73,0.67,0.71,0.72,0.72,0.75,0.62,0.62,0.86,1.04,0.79,0.92,0.74,0.69,0.86,0.79,1.18,0.54,0.86,1.18,1.27,1.45,1.2,1.63,0.97,0.86,0.97,1.32,1.54,2.14,1.19,1.69,1.46,1.72,2.31,2.02,1.63,1.92,2.56,2.41,2.33,2.09,2.65,2.68,2.66,2.92,2.23,3.13,3.38,2.86,2.54,3.06,2.92,2.95,3.2,3.23,3.17,3.1,3.1,3.17,3.44,3.4,3.44,3.3,3.45,3.49,3.56,3.52,3.63,3.69,3.79,3.84,3.87,3.88,4.26,4.05,4.24,4.46,4.2,4.46,4.23,4.44,4.61,4.61,4.47,4.62,4.51,4.51,4.6,4.7,4.72)


# Not so good data
x = c(0.5098166667,0.5165666667,0.54633333335,0.5707166667,0.57355,0.5747,0.5772666667,0.5995833333,0.6031166667,0.6264833333,0.6613333333,0.75225,0.75225,0.7850166667,0.7979333333,0.8544166667,0.92344166665,0.9694166667,0.97098333335,1.01748333335,1.3125083333,1.3445833333,1.34704166665,1.3669,1.3694666667,1.46958333335,1.4982833333,1.5025666667,1.5307833333,1.65485,1.6801,1.704675,1.7732,1.77605,1.7880166667,2.03495,2.1344833333,2.2741333333,2.7594,2.8466,3.3998833333,3.9205333333,3.9386333333)
y = c(1.18,1.1,1.215,1.22,1.305,1.4,1.315,1.55,1.225,1.595,1.9,1.62,2.975,3.91,2.185,2.8,6.085,3.79,7.005,11.5,4.98,11.005,4.615,10.24,15.3,16,15.5,12.29,17.38,20.37,23.9,20.7,17.28,20.3,21.9,21.505,24.6,25.12,36.605,42.3,46.805,49.4,49.6)






newdata    <- seq(from=min(x),to=max(x),length.out=500)
fit        <-loess.wrapper(x, y, span.vals = seq(0.2, 1, by = 0.05), folds = length(x)) 
loess.boot <- boot(cbind(x,y),loess.fun,R=1000,newdata=newdata,span=fit$pars$span,parallel="multicore",ncpus=detectCores())



### Using boot.ci to get the CI ########################
# 
ci         <- boot2ci(loess.boot)

plot(loess.boot$data[,1],loess.boot$data[,2],pch=20)
lines(newdata,ci[,1])
lines(newdata,ci[,2],lty=3,col="red")
lines(newdata,ci[,3],lty=3,col="red")





# Manually calculating residual variance and model variance between bootstrap iterations.
ci2 <- boot2ci_PI(loess.boot,newdata,alpha=0.05)
#ci2 <- boot2ci_PI(loess.boot,newdata,alpha=10^-8)


plot(loess.boot$data[,1],loess.boot$data[,2],pch=20)
lines(newdata,ci2[,1])
lines(newdata,ci2[,2],lty=3,col="red")
lines(newdata,ci2[,3],lty=3,col="red")











### Using cobs ########################
library(cobs)
library(quantreg)


a      <- 0.05
lambda <- 0
#nknots <- 100
knots  <- seq(from=min(x),to=max(x),by=.5)
knots  <- c(knots,max(x))
nknots <- length(knots)

Rbs.upper     <- cobs(x,y, constraint="increase",tau=1-a ,lambda=lambda,ic="SIC",nknots=nknots,knots=knots) 
Rbs.lower     <- cobs(x,y, constraint="increase",tau=a   ,lambda=lambda,ic="SIC",nknots=nknots,knots=knots) 
Rbs.median    <- cobs(x,y, constraint="increase",tau=0.50,lambda=lambda,ic="SIC",nknots=nknots,knots=knots)

plot(x,y,pch=20)
lines(predict(Rbs.median,nz=500), col = "black", lwd = 1.5)
lines(predict(Rbs.upper,nz=500),  col = "red",   lwd = 1.5)
lines(predict(Rbs.lower,nz=500),  col = "red",   lwd = 1.5)







prediction = predict(Rbs.median,nz=500,interval="both")

lines(prediction[,"z"],prediction[,"cb.lo"], col = "green", lwd = 1.5) #simultaneous confidence band
lines(prediction[,"z"],prediction[,"cb.up"], col = "green", lwd = 1.5)

lines(prediction[,"z"],prediction[,"ci.lo"], col = "blue", lwd = 1.5) #pointwise confidence intervals
lines(prediction[,"z"],prediction[,"ci.up"], col = "blue", lwd = 1.5)













### Using locfit ########################
fit <- locfit(y~lp(x,nn=0.25))
#crit(fit) <- crit(fit,cov=0.99)
crit(fit) <- kappa0(y~x,cov=0.95)
plot(fit,band="local")
points(x,y,pch=20)