require(boot)



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



boot2ci_PI <- function(loess.boot,newdata){
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
            loess.boot$t0    -    SD_combined_newdata*qnorm(1-.05/2),
            loess.boot$t0    +    SD_combined_newdata*qnorm(1-.05/2))


  
  return(ci)
}












x <- c(0.51,0.52,0.53,0.54,0.55,0.55,0.56,0.56,0.57,0.57,0.57,0.57,0.57,0.57,0.57,0.58,0.58,0.58,0.59,0.6,0.6,0.63,0.66,0.66,0.68,0.68,0.68,0.71,0.72,0.75,0.75,0.75,0.79,0.8,0.84,0.85,0.88,0.92,0.97,0.97,0.99,0.99,1.02,1.02,1.03,1.06,1.16,1.17,1.19,1.26,1.26,1.27,1.28,1.28,1.28,1.28,1.29,1.3,1.31,1.34,1.35,1.37,1.37,1.37,1.38,1.39,1.39,1.4,1.43,1.43,1.44,1.45,1.45,1.47,1.5,1.5,1.53,1.53,1.54,1.61,1.63,1.65,1.68,1.68,1.69,1.69,1.7,1.7,1.7,1.71,1.72,1.74,1.77,1.78,1.78,1.81,1.87,1.91,2.03,2.08,2.13,2.13,2.26,2.41,2.58,2.61,2.63,2.76,2.85,2.98,3.06,3.33,3.38,3.4,3.46,3.46,3.56,3.92,3.92,3.94,4,4.08,4.27,4.4,4.5)
y <- c(0.4,0.41,0.42,0.44,0.45,0.43,0.47,0.46,0.46,0.46,0.47,0.47,0.48,0.47,0.47,0.47,0.48,0.48,0.48,0.48,0.49,0.51,0.51,0.5,0.53,0.53,0.54,0.53,0.59,0.53,0.62,0.63,0.66,0.55,0.73,0.67,0.71,0.72,0.72,0.75,0.62,0.62,0.86,1.04,0.79,0.92,0.74,0.69,0.86,0.79,1.18,0.54,0.86,1.18,1.27,1.45,1.2,1.63,0.97,0.86,0.97,1.32,1.54,2.14,1.19,1.69,1.46,1.72,2.31,2.02,1.63,1.92,2.56,2.41,2.33,2.09,2.65,2.68,2.66,2.92,2.23,3.13,3.38,2.86,2.54,3.06,2.92,2.95,3.2,3.23,3.17,3.1,3.1,3.17,3.44,3.4,3.44,3.3,3.45,3.49,3.56,3.52,3.63,3.69,3.79,3.84,3.87,3.88,4.26,4.05,4.24,4.46,4.2,4.46,4.23,4.44,4.61,4.61,4.47,4.62,4.51,4.51,4.6,4.7,4.72)



newdata    <- seq(from=min(x),to=max(x),length.out=500)
fit        <-loess.wrapper(x, y, span.vals = seq(0.2, 1, by = 0.05), folds = length(x)) 
loess.boot <- boot(cbind(x,y),loess.fun,R=1000,newdata=newdata,span=fit$pars$span,parallel="multicore",ncpus=detectCores())

# Using boot.ci to get the CI
ci         <- boot2ci(loess.boot)

plot(loess.boot$data[,1],loess.boot$data[,2],pch=20)
lines(newdata,ci[,1])
lines(newdata,ci[,2],lty=3,col="red")
lines(newdata,ci[,3],lty=3,col="red")





# Manually calculating residual variance and model variance between bootstrap iterations.
ci2 <- boot2ci_PI(loess.boot,newdata)


plot(loess.boot$data[,1],loess.boot$data[,2],pch=20)
lines(newdata,ci2[,1])
lines(newdata,ci2[,2],lty=3,col="red")
lines(newdata,ci2[,3],lty=3,col="red")
