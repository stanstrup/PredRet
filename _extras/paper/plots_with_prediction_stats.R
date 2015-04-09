library(PredRetR)
library(gridExtra)
library(plyr)
library(reshape2)
library(ggplot2)



# function to create geom_ploygon calls
#from http://stackoverflow.com/questions/22278951/combining-violin-plot-with-box-plot
fill_viol<-function(df_gr,df_data,v,gr,width=1){
  
  scale_f <- (width^-1)*2
  
  df_gr_n <- as.numeric(as.factor(as.character(df_gr)))
  
  quants<-mutate(v,x.l=x-violinwidth/scale_f,x.r=x+violinwidth/scale_f,cuts=cut(y,    quantile(   df_data[df_gr_n==gr]     )  )      ) # add 1/2 width each way to each x value
  
  plotquants<-data.frame(x=c(quants$x.l,rev(quants$x.r)),   # left x bottom to top, then right x top to bottom
                         y=c(quants$y,rev(quants$y)),       # double up the y values to match
                         id=c(quants$cuts,rev(quants$cuts)))# cut by quantile to create polygon id
  
  geom_polygon(aes(x,y,fill= as.factor(id)),data=plotquants) # return the geom_ploygon object
}


geom_violin_quantile_fill <-function(p,df_gr,df_data,width=1){
  
  coords<-ggplot_build(p)$data        # use ggbuild to get the outline co-ords
  d<-coords[[1]]                      # this gets the df in a usable form
  groups<-unique(d$group)             # get the unique "violin" ids
  
  lapply(groups,function(x) fill_viol(df_gr,df_data,d[d$group==x,],x,width=width))
  
}















source("settings/mongodb.R")

data <- get_user_data(ns=ns_rtdata,ns_chrom_systems=ns_chrom_systems)



## Stats on predictions where RT is known
data$error_abs <- with(data, abs(predicted_rt-recorded_rt))
data$error_rel <- with(data, abs((predicted_rt-recorded_rt)/recorded_rt)      )
data$select <- !is.na(data$predicted_rt) & !is.na(data$recorded_rt)
data$system <- as.factor(data$system)



stats <- 
ddply(data, .(system), summarise, N         = sum(!is.na(predicted_rt)),                                      # # predictions
                                  N_ex      = sum(!is.na(predicted_rt) & is.na(recorded_rt))            ,     # # predictions with unknown RT
                                  error_abs = mean(error_abs[select])        ,                                                        # abs error
                                  error_rel = mean(error_rel[select])        ,                                                        # rel error
      .drop=F)









p1 <- ggplot(data[data$select,], aes( x = system, y = error_abs ) )
p1 <- p1 + scale_x_discrete("system", breaks=levels(data$system), drop=FALSE)
p1 <- p1 + geom_boxplot( alpha = 0.6, outlier.colour = c("grey40") , outlier.size=3.5 ) 
p1 <- p1 + theme_bw()
p1 <- p1 + labs(title="Absolute prediction errors",x="Chromatographic system", y="Error (min)")
p1 <- p1 + theme(axis.text.x = element_text(colour="black",size = 11,face = "bold.italic",angle=45,vjust=1,hjust=1)   )
plot(p1)





p2 <- ggplot( data[data$select,], aes( x = system, y = error_rel*100 ) )
p2 <- p2 + scale_x_discrete("system", breaks=levels(data$system), drop=FALSE)
p2 <- p2 + geom_boxplot( alpha = 0.6, outlier.colour = c("grey40") , outlier.size=3.5) 
p2 <- p2 + theme_bw()
p2 <- p2 + labs(title="Relative prediction errors",x="Chromatographic system", y="Error (%)")
p2 <- p2 + theme(axis.text.x = element_text(colour="black",size = 11,face = "bold.italic",angle=45,vjust=1,hjust=1)   )
plot(p2)






plotdata <- data[data$select,c("system","error_rel")]
plotdata[,"error_rel"] <- plotdata[,"error_rel"]*100


violin_width=0.8

p5 <- ggplot( plotdata, aes( x = system, y = error_rel ) )
p5 <- p5 + scale_x_discrete("system", breaks=levels(data$system), drop=FALSE)
p5 <- p5 + geom_violin(trim=TRUE, fill='black', color="black",adjust=0.3,scale="width",size=0,width=violin_width)
p5 <- p5 + theme_bw()
p5 <- p5 + labs(title="Relative prediction errors",x="Chromatographic system", y="Error (%)",fill="Quantiles")
p5 <- p5 + theme(axis.text.x = element_text(colour="black",size = 11,face = "bold.italic",angle=45,vjust=1,hjust=1)   )


p5 <- p5 + geom_violin_quantile_fill(p=p5,df_gr = plotdata[,"system"],df_data = plotdata[,"error_rel"],width=violin_width)
p5 <- p5 + scale_fill_manual(values = c("lightgrey","darkgrey","darkgrey","lightgrey"),labels = c("25", "50", "75","100"))
# p5 <- p5 + scale_fill_brewer(palette="Reds",        name="Quantile\n",   labels=c("25","50","75","100")    )

p5 <- p5 + geom_boxplot( outlier.size = 1,width=0, size=0) 
p5 <- p5 + stat_summary(fun.y=median, geom="point", shape=23, size=2, color="black",fill="black")
#p5 <- p5 + stat_summary(fun.y=mean, geom="point", shape=16, size=2, color="black",fill="black")

p5 <- p5 + theme(legend.position=c(1,1),legend.justification=c(1,1),
        legend.direction="horizontal",
        legend.box="horizontal",
        legend.box.just = c("top")  )

plot(p5)











plotdata <- data[data$select,c("system","error_abs")]
violin_width=0.8

p6 <- ggplot( plotdata, aes( x = system, y = error_abs ) )
p6 <- p6 + scale_x_discrete("system", breaks=levels(data$system), drop=FALSE)
p6 <- p6 + geom_violin(trim=TRUE, fill='black', color="black",adjust=0.3,scale="width",size=0,width=violin_width)
p6 <- p6 + theme_bw()
p6 <- p6 + labs(title="Absolute prediction errors",x="Chromatographic system", y="Error (min)",fill="Quantiles")
p6 <- p6 + theme(axis.text.x = element_text(colour="black",size = 11,face = "bold.italic",angle=45,vjust=1,hjust=1)   )


p6 <- p6 + geom_violin_quantile_fill(p=p6,df_gr = plotdata[,"system"],df_data = plotdata[,"error_abs"],width=violin_width)
p6 <- p6 + scale_fill_manual(values = c("lightgrey","darkgrey","darkgrey","lightgrey"),labels = c("25", "50", "75","100"))
# p6 <- p6 + scale_fill_brewer(palette="Reds",        name="Quantile\n",   labels=c("25","50","75","100")    )

p6 <- p6 + geom_boxplot( outlier.size = 1,width=0, size=0) 
p6 <- p6 + stat_summary(fun.y=median, geom="point", shape=23, size=2, color="black",fill="black")
#p6 <- p6 + stat_summary(fun.y=mean, geom="point", shape=16, size=2, color="black",fill="black")

p6 <- p6 + theme(legend.position=c(1,1),legend.justification=c(1,1),
                 legend.direction="horizontal",
                 legend.box="horizontal",
                 legend.box.just = c("top")  )

plot(p6)














temp <- stats
temp$N <- with(temp,N-N_ex)
temp <- melt(temp[,c("system","N_ex","N")])
p3 <- ggplot(temp, aes(x = system, y=value,fill=variable))
p3 <- p3 + geom_bar(stat = "identity", position = "stack")
p3 <- p3 + theme_bw()
p3 <- p3 + labs(fill="",y="# Predictions")
p3 <- p3 + theme(axis.text.x = element_text(colour="black",size = 11,face = "bold.italic",angle=45,vjust=1,hjust=1)   )
p3 <- p3 + scale_fill_discrete(labels = c("Predictions where experimental RT is unknown","Predictions where experimental RT is known"))
p3 <- p3 + theme(legend.position=c(1,1),legend.direction="vertical",legend.justification=c(1,1))
plot(p3)




temp <- as.data.frame(table(data[data$generation==0,"system"]))
colnames(temp) <- c("system","N")

p4 <- ggplot(temp, aes(x = system, y=N))
p4 <- p4 + geom_bar(stat = "identity")
p4 <- p4 + theme_bw()
p4 <- p4 + labs(y="# Experimental RTs")
p4 <- p4 + theme(axis.text.x = element_text(colour="black",size = 11,face = "bold.italic",angle=45,vjust=1,hjust=1)   )
p4 <- p4 + scale_fill_discrete(labels = c("Predictions where experimental RT is unknown","Total predictions"))
plot(p4)









grid.arrange(p4, p3, p6, p5, ncol=2, 
             main="Prediction statistics")






library(Cairo)
Cairo(file="_extras/paper/prediction_stats.png", 
      type="png",
      units="in", 
      width=12, 
      height=10, 
      pointsize=12, 
      dpi=300,
      bg="white")

grid.arrange(p4, p3, p6, p5, ncol=2, 
             main="Prediction statistics")
dev.off()









## Investigate compounds with large error
bad <- which(data$error_abs>0.9)
bad <- data$inchi %in% data$inchi[bad]

data_bad <- data[bad,]
data_bad <- split(data_bad,data_bad$inchi)



data_bad[[1]]

data[grepl("/C15H12O7/",data[,"inchi"]),]
