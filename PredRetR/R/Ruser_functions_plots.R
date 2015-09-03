# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
db.errors <- function(data){
  predicted_rt <- recorded_rt <- ci_upper <- ci_lower <- NULL # making package check happy
  
  out <- mutate(data, 
                error_abs    = abs(predicted_rt-recorded_rt),
                error_rel    = abs((predicted_rt-recorded_rt)/recorded_rt) ,
                ci_width_abs = ci_upper-ci_lower,
                ci_width_rel = (ci_upper-ci_lower)/predicted_rt,
                select       = !is.na(predicted_rt) & !is.na(recorded_rt),
                system       = as.factor(system)
  )
  
  return(out)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
db.stats <- function(data){
  predicted_rt <- recorded_rt <- error_abs <- select <- error_rel <- ci_width_abs <- ci_width_rel <- NULL  # making package check happy
  
  stats <- ddply(data, .(system), summarise,
                 N                = sum(!is.na(predicted_rt)),                               # predictions
                 N_ex             = sum(!is.na(predicted_rt) & is.na(recorded_rt))   ,       # predictions with unknown RT
                 error_abs_median = median(error_abs[select])        ,                       # abs error
                 error_rel_median = median(error_rel[select])        ,                       # abs error
                 error_abs        = mean(error_abs[select])        ,                         # abs error
                 error_rel        = mean(error_rel[select])        ,                         # rel error
                 ci_abs           = mean(ci_width_abs,na.rm = T)        ,                                                       
                 ci_rel           = mean(ci_width_rel,na.rm = T)        ,   
                 ci_abs_median    = median(ci_width_abs,na.rm = T)        ,
                 ci_rel_median    = median(ci_width_rel,na.rm = T)        ,
                 .drop=F)
  
  return(stats)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
PredRet_plot.db.count <- function(data = PredRet_get_db()){
  value <- variable <- NULL  # making package check happy
  
  data <- db.errors(data)
  stats <- db.stats(data)
  
  # Add total count of compounds in db
  stats <- mutate(stats,N_sys = as.data.frame(table(data[!data$predicted,"system"]))[,2])
  
  # melt
  temp <- melt(stats[,c("system","N_ex","N","N_sys")],id.vars = "system")
  
  # Order the factors
  temp <- mutate(temp,variable = factor(variable,levels = c("N_sys","N_ex")))
  temp <- temp[order(temp$variable),]
  
  
  # Do the plotting
  p <- ggplot(temp, aes(x = system, y=value,fill=variable))
  p <- p + geom_bar(stat = "identity", position = "stack")
  p <- p + theme_bw_nice
  p <- p + labs(title="Number of RTs in database and predictions made",fill="",y="# Compounds")
  p <- p + theme(legend.position=c(0.8,0.95),legend.direction="vertical",legend.justification=c(1,1),legend.background = element_rect(fill="transparent"))
  p <- p + scale_fill_manual(values = ggplotColours(n=3)[c(2,3)],labels = c("Experimental RTs in database","Predicted RTs"))
  
  p <- p +  scale_y_continuous(breaks=seq(-1000,1000,100))
  p <- p +  geom_hline(yintercept = 0,colour = "grey90")
  
  p <- p + labs(x="Chromatographic systems")
  
  
  return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
PredRet_plot.pred.cor <- function(data = PredRet_get_db()){
  x <- y <- down <- up <- a <- b <- NULL  # making package check happy
  
  data <- db.errors(data)
  select <- !is.na(data$predicted_rt) & !is.na(data$recorded_rt)
  data_sub <- data[select,]
  
  
  # correlation plot
  reg_model <- lm(predicted_rt ~ recorded_rt,data=data_sub)
  #rreg_model <- rlm(predicted_rt ~ recorded_rt,data=data_sub)
  #summary(reg_model)$r.squared
  #r2(rreg_model)
  
  
  
  df <- data.frame(x = data_sub$recorded_rt,y = data_sub$predicted_rt, down = data_sub$ci_lower,  up = data_sub$ci_upper,text=1:length(data_sub$name))
  p <- ggplot(df, aes(x = x, y = y, ymin = down, ymax = up,label=text) )
  p <- p + geom_ribbon(fill = 'grey80', alpha = 1)
  p <- p +   geom_point(color = 'black', linetype = 'dashed')
  p <- p + theme_bw_nice
  p <- p + geom_abline(data=data.frame(a = coef(reg_model)[1],b = coef(reg_model)[2]), aes(intercept=a, slope=b),color="black")
  p <- p + annotate("text",x=0,y=30,hjust=-0.2,vjust=0,label= paste0("R^{2}==",round(summary(reg_model)$r.squared,4) ),parse = TRUE         )
  p <- p + labs( x = "Experimental RTs", y = "Predicted RTs",title="Regression curve for predicted RT vs. experimental RT")
  p <- p + theme(axis.text.x  = element_text(angle=0)   )
  p <- p + theme(axis.title.x = element_text(vjust=0,face = "bold")    )
  
  return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
PredRet_plot.pred.error.abs <- function(data = PredRet_get_db()){
  system <- error_abs <- NULL  # making package check happy
  
  data <- db.errors(data)
  plotdata <- data[data$select,c("system","error_abs")]
  
  temp <- plotdata
  temp$system = as.factor("ALL DATA")
  plotdata <- rbind.data.frame(temp,plotdata)
  
  violin_width=0.8
  
  p <- ggplot( plotdata, aes( x = system, y = error_abs ) )
  p <- p + scale_x_discrete(breaks=levels(plotdata$system), drop=FALSE)
  p <- p + scale_y_continuous(breaks = seq(0, 10, 0.1))
  p <- p + geom_violin(trim=TRUE, fill='black', color="black",adjust=0.3,scale="width",size=0,width=violin_width)
  p <- p + labs(title="Absolute prediction errors", y="Error (min)",fill="Quartiles")
  p <- p + theme_bw_nice
  p <- p + geom_violin_quantile_fill(p=p,df_gr = plotdata[,"system"],df_data = plotdata[,"error_abs"],width=violin_width)
  p <- p + scale_fill_manual(values = c("lightgrey","darkgrey","darkgrey","lightgrey"),labels = c("25", "50", "75","100"))
  p <- p + geom_boxplot( outlier.size = 1,width=0, size=0) 
  p <- p + stat_summary(fun.y=median, geom="point", shape=23, size=2, color="black",fill="black")

  p <- p + theme(legend.position=c(1,1),legend.justification=c(1,1),
                 legend.direction="horizontal",
                 legend.box="horizontal",
                 legend.box.just = c("top"),
                 legend.background = element_rect(fill="transparent"))
  
  
  p <- p + labs(x="Chromatographic systems")
  
  return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
PredRet_plot.pred.error.rel <- function(data = PredRet_get_db()){
  system <- error_rel <- NULL  # making package check happy
  
  data <- db.errors(data)
  plotdata <- data[data$select,c("system","error_rel")]
  plotdata[,"error_rel"] <- plotdata[,"error_rel"]*100
  
  temp <- plotdata
  temp$system = as.factor("ALL DATA")
  plotdata <- rbind.data.frame(temp,plotdata)
  
  
  violin_width=0.8
  
  p <- ggplot( plotdata, aes( x = system, y = error_rel ) )
  p <- p + scale_x_discrete(breaks=levels(plotdata$system), drop=FALSE)
  p <- p + scale_y_continuous(breaks = seq(0, 100, 2))
  p <- p + geom_violin(trim=TRUE, fill='black', color="black",adjust=0.3,scale="width",size=0,width=violin_width)
  p <- p + labs(title="Relative prediction errors",x="Chromatographic systems", y="Error (%)",fill="Quartiles")
  p <- p + theme_bw_nice
  p <- p + geom_violin_quantile_fill(p=p,df_gr = plotdata[,"system"],df_data = plotdata[,"error_rel"],width=violin_width)
  p <- p + scale_fill_manual(values = c("lightgrey","darkgrey","darkgrey","lightgrey"),labels = c("25", "50", "75","100"))
  p <- p + geom_boxplot( outlier.size = 1,width=0, size=0) 
  p <- p + stat_summary(fun.y=median, geom="point", shape=23, size=2, color="black",fill="black")
  
  p <- p + theme(legend.position=c(1,1),legend.justification=c(1,1),
                   legend.direction="horizontal",
                   legend.box="horizontal",
                   legend.box.just = c("top"),
                   legend.background = element_rect(fill="transparent"))
  
  
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
PredRet_plot.pred.pi.abs <- function(data = PredRet_get_db()){
  system <- ci_width_abs <- NULL  # making package check happy
  
  data <- db.errors(data)
  plotdata <- data[data$predicted==TRUE & data$suspect==FALSE,c("system","ci_width_abs")]
  
  temp <- plotdata
  temp$system = as.factor("ALL DATA")
  plotdata <- rbind.data.frame(temp,plotdata)
  
  violin_width=0.8
  
  p <- ggplot( plotdata, aes( x = system, y = ci_width_abs ) )
  p <- p + scale_x_discrete(breaks=levels(plotdata$system), drop=FALSE)
  p <- p + scale_y_continuous(breaks = seq(0, 1000, 0.2))
  p <- p + geom_violin(trim=TRUE, fill='black', color="black",adjust=0.3,scale="width",size=0,width=violin_width)
  p <- p + theme_bw_nice
  p <- p + labs(title="Absolute prediction PI width",x="Chromatographic systems", y="PI width (min)",fill="Quartiles")
  p <- p + geom_violin_quantile_fill(p=p,df_gr = plotdata[,"system"],df_data = plotdata[,"ci_width_abs"],width=violin_width)
  p <- p + scale_fill_manual(values = c("lightgrey","darkgrey","darkgrey","lightgrey"),labels = c("25", "50", "75","100"))
  p <- p + geom_boxplot( outlier.size = 1,width=0, size=0) 
  p <- p + stat_summary(fun.y=median, geom="point", shape=23, size=2, color="black",fill="black")
  
  p <- p + theme(legend.position=c(1,1),legend.justification=c(1,1),
                 legend.direction="horizontal",
                 legend.box="horizontal",
                 legend.box.just = c("top"),
                 legend.background = element_rect(fill="transparent")
  )
  
  return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
PredRet_plot.pred.pi.rel <- function(data = PredRet_get_db()){
  system <- ci_width_rel <- NULL  # making package check happy
  
  data <- db.errors(data)
  plotdata <- data[data$predicted==TRUE & data$suspect==FALSE,c("system","ci_width_rel")]
  plotdata[,"ci_width_rel"] <- plotdata[,"ci_width_rel"]*100
  
  temp <- plotdata
  temp$system = as.factor("ALL DATA")
  plotdata <- rbind.data.frame(temp,plotdata)
  
  
  violin_width=0.8
  
  p <- ggplot( plotdata, aes( x = system, y = ci_width_rel ) )
  p <- p + scale_x_discrete(breaks=levels(plotdata$system), drop=FALSE)
  p <- p + scale_y_continuous(breaks = seq(0, 100, 2))
  p <- p + geom_violin(trim=TRUE, fill='black', color="black",adjust=0.3,scale="width",size=0,width=violin_width)
  p <- p + theme_bw_nice
  p <- p + labs(title="Relative prediction PI width",x="Chromatographic systems", y="PI width (%)",fill="Quartiles")
  p <- p + geom_violin_quantile_fill(p=p,df_gr = plotdata[,"system"],df_data = plotdata[,"ci_width_rel"],width=violin_width)
  p <- p + scale_fill_manual(values = c("lightgrey","darkgrey","darkgrey","lightgrey"),labels = c("25", "50", "75","100"))
  p <- p + geom_boxplot( outlier.size = 1,width=0, size=0) 
  p <- p + stat_summary(fun.y=median, geom="point", shape=23, size=2, color="black",fill="black")
  
  p <- p + theme(legend.position=c(1,1),legend.justification=c(1,1),
                 legend.direction="horizontal",
                 legend.box="horizontal",
                 legend.box.just = c("top"),
                 legend.background = element_rect(fill="transparent")
  )
  
  return(p)
}    
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
