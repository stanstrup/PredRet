models_extended = get_models(ns = ns_sysmodels, include.loess = FALSE, 
                             include.ci = TRUE, include.newdata = TRUE, include.xy_mat = TRUE)

sys_models_oid1 <- sapply(models_extended,function(x) x$oid_sys1)
sys_models_oid2 <- sapply(models_extended,function(x) x$oid_sys2)

select_model <- which(sys_oid2name(ns=ns_chrom_systems,sys_models_oid1)=="LIFE_old" & sys_oid2name(ns=ns_chrom_systems,sys_models_oid2)=="LIFE_new")


x <- models_extended[[select_model]]$newdata
y <- as.numeric(models_extended[[select_model]]$ci$pred)

plot(x,y,type="l",lwd=5)
#abline(a=0,b=1,col="blue")
abline(h=1,col="blue")
lines(x[1:(length(x)-1)]+diff(x),diff(y)/diff(x)  ,col="red",lwd=5)
