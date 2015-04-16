library(boot)
library(parallel)
source("settings/mongodb.R")



## Common theme elements #############################
plottheme <- list(
  theme_bw(),
  theme(axis.title.y = element_text(hjust=0.45,vjust=1,face = "bold")   ),
  theme(axis.title.x = element_text(vjust=4,face = "bold")    ),
  theme(axis.text.x  = element_text(colour="black",size = 9,angle=45,vjust=1,hjust=1)   ),
  theme(panel.grid.major.x = element_blank() , panel.grid.minor.y = element_line(size=0.25,color="white" )    ),
  theme(panel.border = element_blank()),
  theme(axis.line = element_line(color = 'black')),
  theme(plot.title = element_text(face="bold"))
)








## Data for figures #############################
select_data <- matrix(ncol=2,nrow=6)
select_data[1,] <- c("LIFE_old","LIFE_new") # very good
select_data[2,] <- c("FEM_long","RIKEN") # good but outliers
select_data[3,] <- c("RIKEN","FEM_orbitrap_plasma") #sparse
select_data[4,] <- c("RIKEN","MTBLS20") #not enough

select_data[5,] <- c("RIKEN","IPB_Halle") #for testing
select_data[6,] <- c(sys_oid2name(ns=ns_chrom_systems,"53f326f9045fe65526aabcc3"),sys_oid2name(ns=ns_chrom_systems,"5464740bf29f4b93b411a489")) #for testing

letter_name <- unique(as.vector(t(select_data)))
letter_name <- cbind(letter_name,    LETTERS[1:length(letter_name)])





## Plot #############################
p = list()
for(i in 1:nrow(select_data)){

## get data 
data <- get_user_data(ns=ns_rtdata,ns_chrom_systems=ns_chrom_systems)
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

colnames(ci) <- c("med","low","up")
ci <- data.frame(ci,newdata)

colnames(comb_matrix$rt) <- c("x","y")


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


plotstuff2 <- function(){
p <- ggplot()
p <- p + geom_ribbon(data=ci, aes(x = newdata, y = med, ymin = low, ymax = up),fill = 'grey80', alpha = 1)
p <- p +   geom_line(data=ci, aes(x = newdata, y = med, ymin = low, ymax = up),color = 'black')
p <- p +   geom_point(data=as.data.frame(comb_matrix$rt),aes(x=x,y=y),color = 'black')
p <- p + plottheme
p <- p + theme(axis.text.x  = element_text(angle=0,hjust=0.5)   )
p <- p + theme(axis.title.x = element_text(vjust=0,face = "bold",size=16)    )
p <- p + theme(axis.title.y = element_text(size=16)    )
p <- p + labs(x=paste0("RT for ",select_data[i,1]," (min)"),y=paste0("RT for ",select_data[i,2]," (min)"))
p
}

p[[i]] <- plotstuff2()




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







## 2x2 plots ############################
p2 <- list()
p2[[1]] <- p[[1]]   + geom_text(aes(x=Inf, y=-Inf,hjust=1.5, vjust=-0.5, label = "A"),size=10)
p2[[2]] <- p[[2]]   + geom_text(aes(x=Inf, y=-Inf,hjust=1.5, vjust=-0.5, label = "B"),size=10)
p2[[3]] <- p[[3]]   + geom_text(aes(x=Inf, y=-Inf,hjust=1.5, vjust=-0.5, label = "C"),size=10)
p2[[4]] <- p[[4]]   + geom_text(aes(x=Inf, y=-Inf,hjust=1.5, vjust=-0.5, label = "D"),size=10)



library(Cairo)
Cairo(file="_extras/paper/model_ex.png", 
      type="png",
      units="in", 
      width=12, 
      height=10, 
      pointsize=12, 
      dpi=300,
      bg="white")



grid.arrange(p2[[1]],p2[[2]],p2[[3]],p2[[4]],
             ncol=2, 
             #main="Prediction statistics"
             main=""
)
dev.off()




cairo_ps("_extras/paper/model_ex.eps",width=12, height=10, pointsize=12,bg="white")
grid.arrange(p2[[1]],p2[[2]],p2[[3]],p2[[4]],
             ncol=2, 
             #main="Prediction statistics"
             main=""
)
dev.off()







## 4x1 plots ############################
p3 <- list()
p3[[1]] <- p[[1]]   + geom_text(aes(x=Inf, y=-Inf,hjust=1.5, vjust=-0.5, label = "A"),size=10)
p3[[2]] <- p[[2]]   + geom_text(aes(x=Inf, y=-Inf,hjust=1.5, vjust=-0.5, label = "B"),size=10)
p3[[3]] <- p[[3]]   + geom_text(aes(x=Inf, y=-Inf,hjust=1.5, vjust=-0.5, label = "C"),size=10)
p3[[4]] <- p[[4]]   + geom_text(aes(x=Inf, y=-Inf,hjust=1.5, vjust=-0.5, label = "D"),size=10)





library(Cairo)
Cairo(file="_extras/paper/model_ex2.png", 
      type="png",
      units="in", 
      width=12*1.5, 
      height=3*1.5, 
      pointsize=12, 
      dpi=300,
      bg="white")



grid.arrange(p3[[1]],p3[[2]],p3[[3]],p3[[4]],
             ncol=4, 
             #main="Prediction statistics"
             main=""
)
dev.off()





cairo_ps("_extras/paper/model_ex2.eps",width=12*1.5, height=3*1.5, pointsize=12,bg="white")
grid.arrange(p3[[1]],p3[[2]],p3[[3]],p3[[4]],
             ncol=4, 
             #main="Prediction statistics"
             main=""
)
dev.off()
