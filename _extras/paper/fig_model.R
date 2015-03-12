## Data for figures
select_data <- matrix(ncol=2,nrow=6)
select_data[1,] <- c("LIFE_old","LIFE_new") # very good
select_data[2,] <- c("FEM_long","RIKEN") # good but outliers
select_data[3,] <- c("RIKEN","FEM_orbitrap_plasma") #sparse
select_data[4,] <- c("RIKEN","MTBLS20") #not enough

select_data[5,] <- c("RIKEN","IPB_Halle") #for testing
select_data[6,] <- c(sys_oid2name("53f326f9045fe65526aabcc3"),sys_oid2name("5464740bf29f4b93b411a489")) #for testing

letter_name <- unique(as.vector(t(select_data)))
letter_name <- cbind(letter_name,    LETTERS[1:length(letter_name)])



for(i in 1:nrow(select_data)){

## get data
data <- get_user_data(ns=ns_rtdata)
comb_matrix = sys_comb_matrix(oid1 = unique(data$sys_id[data$system==select_data[i,1]]),oid2 = unique(data$sys_id[data$system==select_data[i,2]]),ns=ns_rtdata)
del = as.vector(apply(comb_matrix$rt,1,function(x) any(is.na(x))))
comb_matrix$rt = comb_matrix$rt[!del,,drop=F]
comb_matrix$inchi = comb_matrix$inchi[!del,drop=F]

ord = order(comb_matrix$rt[,1])
comb_matrix$rt = comb_matrix$rt[ord,,drop=F]
comb_matrix$inchi = comb_matrix$inchi[ord,drop=F]


newdata = seq(from=min(comb_matrix$rt[,1]),to=max(comb_matrix$rt[,1]),length.out=1000)

loess.boot <- boot(comb_matrix$rt,gam.mono.con.fun,R=1000,newdata=newdata,parallel="multicore",ncpus=detectCores())
ci=boot2ci(loess.boot,alpha=0.01)


plotstuff <- function(){
  xlab <- paste0("RT for System ",letter_name[    letter_name[,1]==select_data[i,1]      ,2]," (min)")
  ylab <- paste0("RT for System ",letter_name[    letter_name[,1]==select_data[i,2]      ,2]," (min)")
  
  plot(comb_matrix$rt,xlab=xlab,ylab=ylab,type="n",font.lab=2)
  polygon(c(rev(newdata), newdata), c(rev(ci[ ,3]), ci[ ,2]), col = adjustcolor("grey80", alpha=0.5), border = NA)
  points(comb_matrix$rt,pch=20)
  lines(newdata,ci[,1])
  lines(newdata,ci[,2],lty=3)
  lines(newdata,ci[,3],lty=3) 
  }

plotstuff()
# plot different formats
# cairo_ps(file=paste0('/home/jan/gDrive/predret paper/fig_model_',i,'.eps'), width=600/75, height=500/75)
# plotstuff()
# dev.off()
# 
# png(file=paste0('/home/jan/gDrive/predret paper/fig_model_',i,'.png'), width=600, height=500)
# plotstuff()
# dev.off()
# 
# 
# svg(file=paste0('/home/jan/gDrive/predret paper/fig_model_',i,'.svg'), width=600/75, height=500/75)
# plotstuff()
# dev.off()


}