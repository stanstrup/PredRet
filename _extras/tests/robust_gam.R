library(mgcv)
library(pracma)

# get data
# comb_matrix = sys_comb_matrix(oid1 = unique(data$sys_id[data$system=="RIKEN"]),oid2 = unique(data$sys_id[data$system=="FEM_long"]),ns=ns_rtdata)
# del = as.vector(apply(comb_matrix$rt,1,function(x) any(is.na(x))))
# comb_matrix$rt = comb_matrix$rt[!del,,drop=F]
# comb_matrix$inchi = comb_matrix$inchi[!del,drop=F]
# 
# ord = order(comb_matrix$rt[,1])
# comb_matrix$rt = comb_matrix$rt[ord,,drop=F]
# comb_matrix$inchi = comb_matrix$inchi[ord,drop=F]


comb_matrix <- readRDS("test.dat.rds")







plot(comb_matrix$rt)


# standard gam with monotonic contraint
newdata = seq(from=min(comb_matrix$rt[,1]),to=max(comb_matrix$rt[,1]),length.out=1000)
x.star <- comb_matrix$rt[,1]
y.star <- comb_matrix$rt[,2]

dat <- data.frame(x=x.star,y=y.star)



f.ug <- gam(y~s(x,k=min(length(unique(x.star)),10),bs="tp"),data=dat)
w  <-  1 -     (abs(f.ug$residuals)/max(abs(f.ug$residuals)))
w <- sigmoid(w, a = 50, b = 0.9)

sm <- smoothCon(s(x,k=min(length(unique(x.star)),10),bs="cr"),dat,knots=NULL)[[1]]
con <- mono.con(sm$xp);   # get constraints
G <- list(X=sm$X,C=matrix(0,0,0),sp=f.ug$sp,p=sm$xp,y=y.star,w = w   )
G$Ain <- con$A
G$bin <- con$b
G$S <- sm$S
G$off <- 0
p <- pcls(G)

fv<-Predict.matrix(sm,data.frame(x=newdata))%*%p
y_pred <- as.numeric(fv)
y_pred[y_pred < 0] = 0



lines(newdata,y_pred)



# robust

f.ug2 <- DoubleRobGam(y~bsp(x),data=dat)
lines(dat$x,f.ug2$fitted.values,col="red")

