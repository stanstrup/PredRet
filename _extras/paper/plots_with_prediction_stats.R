library(PredRetR)
library(gridExtra)
library(plyr)
library(reshape2)
library(ggplot2)



## function to create geom_ploygon calls #################################
#from http://stackoverflow.com/questions/22278951/combining-violin-plot-with-box-plot
fill_viol<-function(df_gr,df_data,v,gr,width=1){
  
  scale_f <- (width^-1)*2
  
  df_gr_n <- as.numeric(as.factor(as.character(df_gr)))
  
  quants<-mutate(v,x.l=x-violinwidth/scale_f,x.r=x+violinwidth/scale_f,cuts=.bincode(y,    quantile(   df_data[df_gr_n==gr]     )  )      ) # add 1/2 width each way to each x value
  quants$cuts <- as.factor(quants$cuts)
  
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




## Function to make negative numbers positive in ggplots ############################
commapos <- function(x, ...) {
  format(abs(x), big.mark = ",", trim = TRUE,
         scientific = FALSE, ...)
}



## Function to get unweighted R from robust regression ############################
library(MASS)
r2 <- function(x){  
  SSe <- sum((x$resid)^2);  
  observed <- x$resid+x$fitted;  
  SSt <- sum((observed-mean(observed))^2);  
  value <- 1-SSe/SSt;  
  return(value);  
}  


## Function to replicate ggplot2's default colors
ggplotColours <- function(n=6, h=c(0, 360) +15){
  if ((diff(h)%%360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}


## Load data and make some stats ############################
data <- PredRet_get_db()



## Stats on predictions where RT is known
data$error_abs <- with(data, abs(predicted_rt-recorded_rt))
data$error_rel <- with(data, abs((predicted_rt-recorded_rt)/recorded_rt)      )
data$ci_width_abs <- with(data, ci_upper-ci_lower    )
data$ci_width_rel <- with(data, (ci_upper-ci_lower)/predicted_rt    )
data$select <- !is.na(data$predicted_rt) & !is.na(data$recorded_rt)
data$system <- as.factor(data$system)



stats <- 
ddply(data, .(system), summarise, N         = sum(!is.na(predicted_rt)),                                      # # predictions
                                  N_ex      = sum(!is.na(predicted_rt) & is.na(recorded_rt))            ,     # # predictions with unknown RT
                           error_abs_median = median(error_abs[select])        ,                                # abs error
      error_rel_median = median(error_rel[select])        ,                                # abs error
                                  error_abs = mean(error_abs[select])        ,                                # abs error
                                  error_rel = mean(error_rel[select])        ,                                # rel error
                                  ci_abs = mean(ci_width_abs,na.rm = T)        ,                                                       
                                  ci_rel = mean(ci_width_rel,na.rm = T)        ,   
                                  ci_abs_median = median(ci_width_abs,na.rm = T)        ,
      ci_rel_median = median(ci_width_rel,na.rm = T)        ,

      .drop=F)



mean(data$error_abs,na.rm = T)
median(data$error_abs,na.rm = T)

mean(data$error_rel,na.rm = T)
median(data$error_rel,na.rm = T)


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




  
  

## boxplot absolute prediction error ############################

p1 <- ggplot(data[data$select,], aes( x = system, y = error_abs ) )
p1 <- p1 + scale_x_discrete("system", breaks=levels(data$system), drop=FALSE)
p1 <- p1 + geom_boxplot( alpha = 0.6, outlier.colour = c("grey40") , outlier.size=3.5 ) 
p1 <- p1 + theme_bw()
p1 <- p1 + labs(title="Absolute prediction errors",x="Chromatographic system", y="Error (min)")
p1 <- p1 + theme(axis.text.x = element_text(colour="black",size = 11,face = "bold.italic",angle=45,vjust=1,hjust=1)   )
plot(p1)




## boxplot relative prediction error ############################
p2 <- ggplot( data[data$select,], aes( x = system, y = error_rel*100 ) )
p2 <- p2 + scale_x_discrete("system", breaks=levels(data$system), drop=FALSE)
p2 <- p2 + geom_boxplot( alpha = 0.6, outlier.colour = c("grey40") , outlier.size=3.5) 
p2 <- p2 + theme_bw()
p2 <- p2 + labs(title="Relative prediction errors",x="Chromatographic system", y="Error (%)")
p2 <- p2 + theme(axis.text.x = element_text(colour="black",size = 11,face = "bold.italic",angle=45,vjust=1,hjust=1)   )
plot(p2)





## Violin plot relative prediction error ############################
plotdata <- data[data$select,c("system","error_rel")]
plotdata[,"error_rel"] <- plotdata[,"error_rel"]*100


violin_width=0.8

p5 <- ggplot( plotdata, aes( x = system, y = error_rel ) )
p5 <- p5 + scale_x_discrete(breaks=levels(data$system), drop=FALSE)
p5 <- p5 + scale_y_continuous(breaks = seq(0, 100, 2))
p5 <- p5 + geom_violin(trim=TRUE, fill='black', color="black",adjust=0.3,scale="width",size=0,width=violin_width)
p5 <- p5 + labs(title="Relative prediction errors",x="Chromatographic systems", y="Error (%)",fill="Quartiles")
p5 <- p5 + plottheme
p5 <- p5 + geom_violin_quantile_fill(p=p5,df_gr = plotdata[,"system"],df_data = plotdata[,"error_rel"],width=violin_width)
p5 <- p5 + scale_fill_manual(values = c("lightgrey","darkgrey","darkgrey","lightgrey"),labels = c("25", "50", "75","100"))
# p5 <- p5 + scale_fill_brewer(palette="Reds",        name="Quantile\n",   labels=c("25","50","75","100")    )

p5 <- p5 + geom_boxplot( outlier.size = 1,width=0, size=0) 
p5 <- p5 + stat_summary(fun.y=median, geom="point", shape=23, size=2, color="black",fill="black")
#p5 <- p5 + stat_summary(fun.y=mean, geom="point", shape=16, size=2, color="black",fill="black")

p5 <- p5 + theme(legend.position=c(1,1),legend.justification=c(1,1),
        legend.direction="horizontal",
        legend.box="horizontal",
        legend.box.just = c("top"),
        legend.background = element_rect(fill="transparent"))


plot(p5)










## Violin plot absolute prediction error ############################
plotdata <- data[data$select,c("system","error_abs")]
violin_width=0.8

p6 <- ggplot( plotdata, aes( x = system, y = error_abs ) )
p6 <- p6 + scale_x_discrete(breaks=levels(data$system), drop=FALSE)
p6 <- p6 + scale_y_continuous(breaks = seq(0, 10, 0.1))
p6 <- p6 + geom_violin(trim=TRUE, fill='black', color="black",adjust=0.3,scale="width",size=0,width=violin_width)
p6 <- p6 + labs(title="Absolute prediction errors", y="Error (min)",fill="Quartiles")
p6 <- p6 + plottheme
p6 <- p6 + geom_violin_quantile_fill(p=p6,df_gr = plotdata[,"system"],df_data = plotdata[,"error_abs"],width=violin_width)
p6 <- p6 + scale_fill_manual(values = c("lightgrey","darkgrey","darkgrey","lightgrey"),labels = c("25", "50", "75","100"))
# p6 <- p6 + scale_fill_brewer(palette="Reds",        name="Quantile\n",   labels=c("25","50","75","100")    )

p6 <- p6 + geom_boxplot( outlier.size = 1,width=0, size=0) 
p6 <- p6 + stat_summary(fun.y=median, geom="point", shape=23, size=2, color="black",fill="black")
#p6 <- p6 + stat_summary(fun.y=mean, geom="point", shape=16, size=2, color="black",fill="black")

p6 <- p6 + theme(legend.position=c(1,1),legend.justification=c(1,1),
                 legend.direction="horizontal",
                 legend.box="horizontal",
                 legend.box.just = c("top"),
                 legend.background = element_rect(fill="transparent"))


p6 <- p6 + labs(x="Chromatographic systems")


plot(p6)













## barplot for number of predictions ############################
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



## barplot for number of RTs in the database ############################
temp <- as.data.frame(table(data[!data$predicted,"system"]))
colnames(temp) <- c("system","N")

p4 <- ggplot(temp, aes(x = system, y=N))
p4 <- p4 + geom_bar(stat = "identity")
p4 <- p4 + theme_bw()
p4 <- p4 + labs(y="# Experimental RTs")
p4 <- p4 + theme(axis.text.x = element_text(colour="black",size = 11,face = "bold.italic",angle=45,vjust=1,hjust=1)   )
plot(p4)





## barplot for number of predictions combined with number of RTs in the database ############################
temp <- stats
temp$N <- with(temp,N-N_ex)
temp <- melt(temp[,c("system","N_ex","N")])


temp2 <- as.data.frame(table(data[!data$predicted,"system"]))
colnames(temp2) <- c("system","N")

temp2 <- data.frame(system = temp2$system,
                   variable = "N_sys",
                   value = temp2$N
                   )


temp <- rbind.data.frame(temp,temp2)

temp2 <- dcast(temp,system~variable)
temp2 <- mutate(temp2,N_ex_N_sys_ratio = N_ex/N_sys)



      
p7 <- ggplot(temp, aes(x = system, y=value,fill=variable))
p7 <- p7 + geom_bar(data = subset(temp, variable %in% c("N_ex","N")),stat = "identity", position = "stack")
p7 <- p7 + geom_bar(data = subset(temp, variable %in% c("N_sys")),aes(x = system, y=-value),stat = "identity")
p7 <- p7 + plottheme
p7 <- p7 + labs(title="Number of RTs and predictions made",fill="",y="# Experimental RTs        # Predictions")
p7 <- p7 + theme(legend.position=c(0.85,0.37),legend.direction="vertical",legend.justification=c(1,1),legend.background = element_rect(fill="transparent"))
p7 <- p7 + scale_fill_discrete(labels = c("Predictions where experimental RT is known","Predictions where experimental RT is unknown","RTs in database"))

p7 <- p7 +  scale_y_continuous(breaks=seq(-1000,1000,100),labels = commapos) # make negative scale positive
p7 <- p7 +  geom_hline(yintercept = 0,colour = "grey90")

p7 <- p7 + labs(x="Chromatographic systems")

plot(p7)




temp$variable <- factor(temp$variable,levels = c("N_sys", "N","N_ex"))
temp <- temp[order(temp$variable),]
temp3 <- subset(temp, variable %in% c("N_ex","N_sys"))
temp3$variable <- droplevels(temp3$variable)


p12 <- ggplot(temp3, aes(x = system, y=value,fill=variable))
p12 <- p12 + geom_bar(stat = "identity", position = "stack")
p12 <- p12 + plottheme
p12 <- p12 + labs(title="Number of RTs in database and predictions made",fill="",y="# Compounds")
p12 <- p12 + theme(legend.position=c(0.8,0.95),legend.direction="vertical",legend.justification=c(1,1),legend.background = element_rect(fill="transparent"))
p12 <- p12 + scale_fill_manual(values = ggplotColours(n=3)[c(2,3)],labels = c("RTs in database","Predicted RTs"))

p12 <- p12 +  scale_y_continuous(breaks=seq(-1000,1000,100))
p12 <- p12 +  geom_hline(yintercept = 0,colour = "grey90")

p12 <- p12 + labs(x="Chromatographic systems")

plot(p12)








temp <- mutate(temp,type= ifelse( variable %in% c("N_ex","N") , "# Predictions" ,"# Experimental RTs in database" ) )
temp$type <- factor(temp$type,levels=c("# Experimental RTs in database","# Predictions"))


p11 <- ggplot(temp, aes(x = system, y=value,fill=variable))
p11 <- p11 + geom_bar(data = subset(temp, variable %in% c("N_ex","N","N_sys")),stat = "identity", position = "stack")
p11 <- p11 + plottheme
p11 <- p11 + facet_wrap( ~ type,nrow=2)
p11 <- p11 + labs(title="Number of RTs and predictions made",fill="",y="")
p11 <- p11 + theme(legend.position=c(0.85,0.37),legend.direction="vertical",legend.justification=c(1,1),legend.background = element_rect(fill="transparent"))
p11 <- p11 + scale_fill_manual(values = ggplotColours(n=3)[c(2,1,3)], labels = c("Experimental RT is unknown","Experimental RT is known","RTs in database") )
p11 <- p11 + labs(x="Chromatographic systems")

plot(p11)




## Violin plot for ci relative width ############################
plotdata <- data[data$predicted==TRUE & data$suspect==FALSE,c("system","ci_width_rel")]
plotdata[,"ci_width_rel"] <- plotdata[,"ci_width_rel"]*100




violin_width=0.8

p8 <- ggplot( plotdata, aes( x = system, y = ci_width_rel ) )
p8 <- p8 + scale_x_discrete(breaks=levels(plotdata$system), drop=FALSE)
p8 <- p8 + scale_y_continuous(breaks = seq(0, 100, 2))
p8 <- p8 + geom_violin(trim=TRUE, fill='black', color="black",adjust=0.3,scale="width",size=0,width=violin_width)
p8 <- p8 + plottheme
p8 <- p8 + labs(title="Relative prediction PI width",x="Chromatographic systems", y="PI width (%)",fill="Quartiles")

p8 <- p8 + geom_violin_quantile_fill(p=p8,df_gr = plotdata[,"system"],df_data = plotdata[,"ci_width_rel"],width=violin_width)
p8 <- p8 + scale_fill_manual(values = c("lightgrey","darkgrey","darkgrey","lightgrey"),labels = c("25", "50", "75","100"))
# p8 <- p8 + scale_fill_brewer(palette="Reds",        name="Quantile\n",   labels=c("25","50","75","100")    )

p8 <- p8 + geom_boxplot( outlier.size = 1,width=0, size=0) 
p8 <- p8 + stat_summary(fun.y=median, geom="point", shape=23, size=2, color="black",fill="black")
#p8 <- p8 + stat_summary(fun.y=mean, geom="point", shape=16, size=2, color="black",fill="black")

p8 <- p8 + theme(legend.position=c(1,1),legend.justification=c(1,1),
                 legend.direction="horizontal",
                 legend.box="horizontal",
                 legend.box.just = c("top"),
                 legend.background = element_rect(fill="transparent")
                 )


plot(p8)




## Violin plot for ci absolute width ############################
plotdata <- data[data$predicted==TRUE & data$suspect==FALSE,c("system","ci_width_abs")]



violin_width=0.8

p9 <- ggplot( plotdata, aes( x = system, y = ci_width_abs ) )
p9 <- p9 + scale_x_discrete(breaks=levels(data$system), drop=FALSE)
p9 <- p9 + scale_y_continuous(breaks = seq(0, 1000, 0.2))
p9 <- p9 + geom_violin(trim=TRUE, fill='black', color="black",adjust=0.3,scale="width",size=0,width=violin_width)
p9 <- p9 + plottheme
p9 <- p9 + labs(title="Absolute prediction PI width",x="Chromatographic systems", y="PI width (min)",fill="Quartiles")

p9 <- p9 + geom_violin_quantile_fill(p=p9,df_gr = plotdata[,"system"],df_data = plotdata[,"ci_width_abs"],width=violin_width)
p9 <- p9 + scale_fill_manual(values = c("lightgrey","darkgrey","darkgrey","lightgrey"),labels = c("25", "50", "75","100"))
# p9 <- p9 + scale_fill_brewer(palette="Reds",        name="Quantile\n",   labels=c("25","50","75","100")    )

p9 <- p9 + geom_boxplot( outlier.size = 1,width=0, size=0) 
p9 <- p9 + stat_summary(fun.y=median, geom="point", shape=23, size=2, color="black",fill="black")
#p9 <- p9 + stat_summary(fun.y=mean, geom="point", shape=16, size=2, color="black",fill="black")

p9 <- p9 + theme(legend.position=c(1,1),legend.justification=c(1,1),
                 legend.direction="horizontal",
                 legend.box="horizontal",
                 legend.box.just = c("top"),
                 legend.background = element_rect(fill="transparent")
                 )


plot(p9)




## experimental vs. predicted regression plot ##################################
select <- !is.na(data$predicted_rt) & !is.na(data$recorded_rt)
data_sub <- data[select,]
#data_sub <- data_sub[order(data_sub$recorded_rt),]


# correlation plot
reg_model <- lm(predicted_rt ~ recorded_rt,data=data_sub)
rreg_model <- rlm(predicted_rt ~ recorded_rt,data=data_sub)
summary(reg_model)$r.squared
r2(rreg_model)

df <- data.frame(x = data_sub$recorded_rt,y = data_sub$predicted_rt, down = data_sub$ci_lower,  up = data_sub$ci_upper,text=1:length(data_sub$name))

p10 <- ggplot(df, aes(x = x, y = y, ymin = down, ymax = up,label=text) )
p10 <- p10 + geom_ribbon(fill = 'grey80', alpha = 1)
p10 <- p10 +   geom_point(color = 'black', linetype = 'dashed')
p10 <- p10 + plottheme
# p <- p + scale_x_log10() 
# p <- p + scale_y_log10(limits = c(0.1,max(df[,"y"]))) 
# p <- p + annotation_logticks()
#p <- p + geom_abline(intercept=0, slope=1) 
#p <- p + geom_abline(data=data.frame(a = coef(rreg_model)[1],b = coef(rreg_model)[2]), aes(intercept=a, slope=b),color="red")
p10 <- p10 + geom_abline(data=data.frame(a = coef(reg_model)[1],b = coef(reg_model)[2]), aes(intercept=a, slope=b),color="black")
p10 <- p10 + annotate("text",x=0,y=30,hjust=-0.2,vjust=0,label= paste0("R^{2}==",round(summary(reg_model)$r.squared,4) ),parse = TRUE         )
#p <- p + geom_text(hjust=-0.2, vjust=0.3),
p10 <- p10 + labs( x = "Experimental RTs", y = "Predicted RTs",title="Regression curve for predicted RT vs. experimental RT")

p10 <- p10 + theme(axis.text.x  = element_text(angle=0)   )
p10 <- p10 + theme(axis.title.x = element_text(vjust=0,face = "bold")    )
plot(p10)






# error ###################
plot( data_sub$recorded_rt,       (data_sub$predicted_rt-data_sub$recorded_rt)   ,pch=20)


# rel error ###################
plot( data_sub$recorded_rt,       (data_sub$predicted_rt-data_sub$recorded_rt)/data_sub$recorded_rt    ,pch=20)






## merge plots ############################
p12 <- p12   + geom_text(aes(x=-Inf, y=Inf,hjust=-0.5, vjust=1.5, label = "A"),size=10)
#p11 <- p11   + geom_text(aes(x=-Inf, y=Inf,hjust=-0.5, vjust=1.5, label = "A"),size=10)
p10 <- p10 + geom_text(aes(x=-Inf, y=Inf,hjust=-0.5, vjust=1.5, label = "B"),size=10)
p6 <- p6   + geom_text(aes(x=-Inf, y=Inf,hjust=-0.5, vjust=1.5, label = "C"),size=10)
p5 <- p5   + geom_text(aes(x=-Inf, y=Inf,hjust=-0.5, vjust=1.5, label = "D"),size=10)
p9 <- p9   + geom_text(aes(x=-Inf, y=Inf,hjust=-0.5, vjust=1.5, label = "E"),size=10)
p8 <- p8   + geom_text(aes(x=-Inf, y=Inf,hjust=-0.5, vjust=1.5, label = "F"),size=10)

# guide=FALSE turns off the legend

p6$scales$scales[[3]]$guide=FALSE
p5$scales$scales[[3]]$guide=FALSE
p9$scales$scales[[3]]$guide=FALSE
p8$scales$scales[[3]]$guide=FALSE



grid.arrange(p12, p10, p6, p5,p9,p8 ,
             ncol=2, 
             #top="Prediction statistics"
             top=""
)



library(Cairo)
Cairo(file="_extras/paper/prediction_stats.png", 
      type="png",
      units="in", 
      width=12, 
      height=15, 
      pointsize=12, 
      dpi=300,
      bg="white")



grid.arrange(p12, p10, p6, p5,p9,p8 ,
             ncol=2, 
             #top="Prediction statistics"
             top=""
             )
dev.off()




cairo_ps("_extras/paper/prediction_stats.eps",width=12, height=15, pointsize=12,bg="white")
grid.arrange(p12, p10, p6, p5,p9,p8 ,
             ncol=2, 
             #main="Prediction statistics"
             top=""
)
dev.off()







## Investigate compounds with large error ##########################
bad <- which(data$error_abs>0.9)
bad <- data$inchi %in% data$inchi[bad]

data_bad <- data[bad,]
data_bad <- split(data_bad,data_bad$inchi)



data_bad[[1]]

data[grepl("/C15H12O7/",data[,"inchi"]),]
