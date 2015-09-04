library(boot)
library(parallel)
library(PredRetR)
library(ggplot2)
library(Rplot.extra)
library(gridExtra)
library(Cairo)



## Data for figures #############################
select_data <- matrix(ncol=2,nrow=6)
select_data[1,] <- c("LIFE_old","LIFE_new") # very good
select_data[2,] <- c("FEM_long","RIKEN") # good but outliers
select_data[3,] <- c("RIKEN","FEM_orbitrap_plasma") #sparse
select_data[4,] <- c("RIKEN","MTBLS20") #not enough
select_data[5,] <- c("RIKEN","IPB_Halle") #for testing
select_data[6,] <- c(sys_oid2name("53f326f9045fe65526aabcc3"),sys_oid2name("5464740bf29f4b93b411a489")) #for testing




## Plot #############################
data <- get_user_data() ## get data 
p = list()


for(i in 1:nrow(select_data)){
  
  comb_matrix = sys_comb_matrix(oid1 = unique(data$sys_id[data$system==select_data[i,1]]),oid2 = unique(data$sys_id[data$system==select_data[i,2]]))
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
  
  
  

  p[[i]] <- ggplot()
  p[[i]] <- p[[i]] + geom_ribbon(data=ci, aes(x = newdata, y = med, ymin = low, ymax = up),fill = 'grey80', alpha = 1)
  p[[i]] <- p[[i]] + geom_line(data=ci, aes(x = newdata, y = med, ymin = low, ymax = up),color = 'black')
  p[[i]] <- p[[i]] + geom_point(data=as.data.frame(comb_matrix$rt),aes(x=x,y=y),color = 'black')
  p[[i]] <- p[[i]] + theme_bw_nice
  p[[i]] <- p[[i]] + theme(axis.text.x  = element_text(angle=0,hjust=0.5)   )
  p[[i]] <- p[[i]] + theme(axis.title.x = element_text(vjust=0,face = "bold",size=16)    )
  p[[i]] <- p[[i]] + theme(axis.title.y = element_text(size=16)    )
  p[[i]] <- p[[i]] + labs(x=paste0("RT for ",select_data[i,1]," (min)"),y=paste0("RT for ",select_data[i,2]," (min)"))
    p[[i]]
  
  
}







## 2x2 plots ############################
p2 <- list()
p2[[1]] <- p[[1]]   + geom_text(aes(x=Inf, y=-Inf,hjust=1.5, vjust=-0.5, label = "A"),size=10)
p2[[2]] <- p[[2]]   + geom_text(aes(x=Inf, y=-Inf,hjust=1.5, vjust=-0.5, label = "B"),size=10)
p2[[3]] <- p[[3]]   + geom_text(aes(x=Inf, y=-Inf,hjust=1.5, vjust=-0.5, label = "C"),size=10)
p2[[4]] <- p[[4]]   + geom_text(aes(x=Inf, y=-Inf,hjust=1.5, vjust=-0.5, label = "D"),size=10)



# png
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
             top=""
)
dev.off()



# eps
cairo_ps("_extras/paper/model_ex.eps",width=12, height=10, pointsize=12,bg="white")
grid.arrange(p2[[1]],p2[[2]],p2[[3]],p2[[4]],
             ncol=2,
             top=""
)
dev.off()







## 4x1 plots ############################
p3 <- list()
p3[[1]] <- p[[1]]   + geom_text(aes(x=Inf, y=-Inf,hjust=1.5, vjust=-0.5, label = "A"),size=10)
p3[[2]] <- p[[2]]   + geom_text(aes(x=Inf, y=-Inf,hjust=1.5, vjust=-0.5, label = "B"),size=10)
p3[[3]] <- p[[3]]   + geom_text(aes(x=Inf, y=-Inf,hjust=1.5, vjust=-0.5, label = "C"),size=10)
p3[[4]] <- p[[4]]   + geom_text(aes(x=Inf, y=-Inf,hjust=1.5, vjust=-0.5, label = "D"),size=10)




# png
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
             top=""
)
dev.off()



# eps
cairo_ps("_extras/paper/model_ex2.eps",width=12*1.5, height=3*1.5, pointsize=12,bg="white")
grid.arrange(p3[[1]],p3[[2]],p3[[3]],p3[[4]],
             ncol=4,
             top=""
)
dev.off()
