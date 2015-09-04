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












# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
PredRet_plot.db.graph <- function(database = PredRet_get_db(exp_pred = "exp"),
                                  models = PredRet_get_models(),
                                  sys_db = PredRet_get_chrom_systems(),
                                  circular = TRUE,
                                  only_connected = TRUE              ){
  
  
  
  
  
  # Make edges
  d <- data.frame(from  =   sapply(models,function(x) x$predict_from)    ,
                  to    =   sapply(models,function(x) x$predict_to)      ,
                  common=   sapply(models,function(x) x$stats["n_points"])
  )
  
  
  
  # Attempt to do better ordering manually
  # d$from <- factor(d$from,levels= c("RIKEN","MTBLS20","LIFE_new","LIFE_old","IPB_Halle","MTBLS87","Cao_HILIC" ,"Eawag_XBridgeC18","UFZ_Phenomenex","UniToyama_Atlantis",
  #                                   "FEM_orbitrap_urine","FEM_lipids","FEM_orbitrap_plasma","FEM_short","FEM_long","MPI_Symmetry",
  #                                   "MTBLS39",
  #                                   "MTBLS38","MTBLS36"
  #                                   
  #                                   )
  #                 )
  
  #d <- d[order(as.numeric(d$from)),]
  
  
  # Some sorting to attempt to make it look better
  d      <- d[order(d$common,decreasing = T),]
  d$from <- factor(d$from,levels=unique(as.vector(as.matrix(t(d[,c("from","to")])))))
  d      <- d[order(as.numeric(d$from)),]
  
  
  
  # Make node sizes
  temp     <- as.matrix(table(database$system))
  N        <- as.numeric(temp)
  names(N) <- rownames(temp)
  
  
  
  # create graph
  g           <- graph.data.frame(d, directed=T)
  connected_v <- length((V(g)))
  g           <- addVertIfNotPresent(g,names(N)[!(names(N) %in% V(g)$name)]) 
  
  
  
  # Set node size
  order     <- match(V(g)$name,names(N))
  V(g)$size <- normalize_range(     log(as.numeric(N)[order])     ,10,30)
  
  
  
  # edge width  
  E(g)$width       <- normalize_range(     log(d$common)     ,3,20)
  E(g)$arrow.width <-  0
  E(g)$arrow.size  <-  0
  
  
  
  # edge color scale
  color <- normalize01(rank(log(d$common)))
  color <- c(-.4,color)
  color <- normalize01(color)
  color <- colorRamp(c("yellow","pink","blue"))(   color     )
  color <- color[-1,]
  color <- apply(color,1,function(x) rgb(x[1],x[2],x[3],maxColorValue=255))
  E(g)$color <- color
  
  
  
  # mark hilic
  system_column_type <- data.frame(system_name        = sapply(sys_db,function(x) x$system_name),
                                   system_column_type = sapply(sys_db,function(x) x$system_column_type),stringsAsFactors = F)
  
  to_mark <- subset(system_column_type,system_column_type=="HILIC")[,"system_name"]
  to_mark <- match(to_mark,names(V(g)))
  
  
  
  
# Setting some plot settings
  margin <- rep(-0.2,4)

  if(only_connected){
    g <- delete.vertices(g,   connected_v + 1:(length(V(g))-connected_v)    )
  }
  
  
  if(circular){
    layout <- layout.circle
  }else{
    layout <- layout.fruchterman.reingold(g, niter=10000)
  }
  

  
  # make gradient
  gradient <- cbind.data.frame(scale = d$common,color = E(g)$color,stringsAsFactors=FALSE)
  gradient <- gradient[order(gradient$scale),]
  gradient <- unique(gradient)
  gradient <- interpolate_gradient(gradient)
  
  
  
  
  # make the plot
  def.par <- par(no.readonly = TRUE) # save default, for resetting...
  graphics::layout(matrix(c(1,2), 2, 1, byrow = TRUE),  heights=c(7,1)  )
  
  
  par(xpd = TRUE)
  plot(g,vertex.color="white",
       layout=layout,
       margin=margin,
       mark.groups=list(to_mark), mark.col="#C5E5E7", mark.border=NA)
  
  
  legend(x=-1.2, y=-1, c("HILIC"), pch=21,
         col="#777777", pt.bg="#C5E5E7", pt.cex=3, cex=1.5, bty="n", ncol=1)
  
  
  
  par(mar=c(3,10,0,10))  
  plot(gradient$scale,seq(1,2,length.out = length(gradient$scale)),type="n",log="x",axes=FALSE,ylab="",xlab="")
  rect(xleft = gradient$scale-0.505 ,  xright= gradient$scale+0.505, ybottom=1, ytop =2,col=gradient$color,lwd=0,border=NA)
  labels = seq(min(gradient$scale),max(gradient$scale),10)
  axis(1,at=labels,labels= labels      )
  
  
  
  par(def.par)  #- reset to default
  
  
  
  
  
  
  
  
  
  
  
  return(list(d = d,g = g))
}

  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #












# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
PredRet_plot.model.fit <- function(from,to,
                                   database = PredRet_get_db(exp_pred = "exp")
                                   ){
  
  
  x <- pred <- upper <- lower <- y <- NULL  # making package check happy
  
  
  # query data
  models   = PredRet_get_models(from,to)
  
  
  # Select the right model
  db_from_to <-  data.frame(predict_from = sapply(models,function(x) x$predict_from)  , predict_to = sapply(models,function(x) x$predict_to)  )
  select     <-  which(db_from_to$predict_from == from & db_from_to$predict_to == to)
  
  
  
  # Get all the compounds they have in common
  comb_matrix       <- sys_comb_matrix(oid1 = name2sys_oid(from),oid2 = name2sys_oid(to)    ,include.suspect=TRUE)
  del = as.vector(apply(comb_matrix$rt,1,function(x) any(is.na(x))))
  comb_matrix$rt    <- comb_matrix$rt[!del,,drop=F]
  comb_matrix$inchi <- comb_matrix$inchi[!del,drop=F]
  
  ord = order(comb_matrix$rt[,1])
  comb_matrix$rt = comb_matrix$rt[ord,,drop=F]
  comb_matrix$inchi = comb_matrix$inchi[ord,drop=F]
  
  
  
  # make a data.frame with the cis
  ci <- models[[select]]$model_fit
  ci <- as.data.frame(lapply(ci,unlist))
  
  
  
  # get the points used to fit the model
  model_points           <-   models[[select]]$model_points[,c(1,2)]
  colnames(model_points) <-   c("x","y")
  
  
  
  # get the points that was not used in the model
  select <- !(   comb_matrix$inchi %in% as.character(models[[select]]$model_points$inchi)    )
  comb_matrix$inchi <- comb_matrix$inchi[select]
  comb_matrix$rt <- as.data.frame(comb_matrix$rt[select,])
  colnames(comb_matrix$rt) <-   c("x","y")
  
  
  
  # do the plot
  p <- ggplot()
  p <- p + geom_ribbon(data=ci, aes(x = x, y = pred, ymin = lower, ymax = upper),fill = 'grey80', alpha = 1)
  p <- p + geom_line(data=ci, aes(x = x, y = pred, ymin = lower, ymax = upper),color = 'black',size=1)
  p <- p + geom_point(data=model_points,aes(x=x,y=y),color = 'black')
  p <- p + geom_point(data=comb_matrix$rt,aes(x=x,y=y),color = 'red')
  p <- p + theme_bw_nice
  p <- p + theme(axis.text.x  = element_text(angle=0,hjust=0.5)   )
  p <- p + theme(axis.title.x = element_text(vjust=0,face = "bold",size=16)    )
  p <- p + theme(axis.title.y = element_text(size=16)    )
  p <- p + labs(x=paste0("RT for ",from," (min)"),y=paste0("RT for ",to," (min)"))
  
  return(p)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
















