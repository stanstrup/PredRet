setwd("_extras/structure_based/PredRet_QSRR/")

library(plyr)
library(dplyr)
library(tidyr)
library(Rplot.extra)
library(parallel)
library(caret)
library(doParallel)
library(qdapTools)


#load("dataset_all.RData")





system_desc <- dataset %>% 
                group_by(system,inchi) %>% 
                summarise( recorded_rt = median(recorded_rt)) %>% 
                spread(inchi,recorded_rt)

# table(apply(select(system_desc,-system),2,function(x) sum(!is.na(x))))
colnames(system_desc) <- c("system",paste0("desc_comp_",1:(ncol(system_desc)-1)))
  
dataset <- left_join(dataset,system_desc,by="system")


camb_dataset <- dataset %>% 
                do(camb_dataset = SplitSet(.$name, dplyr::select(.,starts_with("desc_")), .$recorded_rt, percentage = 20)) %>% 
                unlist(recursive = FALSE,use.names=FALSE) %>%  unlist(recursive = FALSE) %>% 
                GetCVTrainControl(folds = 5)










cl <- makePSOCKcluster(6)
registerDoParallel(cl)



method <- "gbm"
tune.grid <- expand.grid(n.trees = c(100,200,800), interaction.depth = c(25), shrinkage = c(0.01,0.05,0.1),n.minobsinnode = 10)
model <- train(camb_dataset$x.train, camb_dataset$y.train, method,
               tuneGrid = tune.grid, trControl = camb_dataset$trControl)
saveRDS(model, file = paste(method, ".rds", sep = ""))



stopCluster(cl)



plot(model, metric = "RMSE")





preds <- predict(model,newdata = camb_dataset$x.holdout)

plotdata <- cbind.data.frame(id = as.character(camb_dataset$ids[camb_dataset$holdout.indexes]),
                             system = dataset$system[camb_dataset$holdout.indexes] , 
                             error_abs = abs(preds-camb_dataset$y.holdout)
                             )


theme_common <- list(
  theme(axis.title.y = element_text(hjust=0.45,vjust=1)   ),
  theme(axis.title.x = element_text(vjust=4)    ),
  theme(axis.text.x  = element_text(angle=45,vjust=1,hjust=1)   ),
  theme(panel.grid.major.x = element_blank() , panel.grid.minor.y = element_line(size=0.25,color="white" )    ),
  theme(panel.border = element_blank()),
  theme(axis.line = element_line(color = 'black')),
  theme(plot.title = element_text(face="bold"))
)



violin_width=0.8

p <- ggplot( plotdata, aes( x = system, y = error_abs ) )
p <- p + scale_x_discrete(breaks=levels(plotdata$system), drop=FALSE)
p <- p + scale_y_continuous(breaks = seq(0, 50, 1),limits=c(0,2))
p <- p + geom_violin(trim=TRUE, fill='black', color="black",adjust=0.3,scale="width",size=0,width=violin_width)
p <- p + labs(title="Absolute prediction errors", y="Error",fill="Quartiles")
p <- p + theme_bw_nice + theme_common
p <- p + geom_violin_quantile_fill(p=p,df_gr = plotdata[,"system"],df_data = plotdata[,"error_abs"],width=violin_width)
p <- p + scale_fill_manual(values = c("lightgrey","darkgrey","darkgrey","lightgrey"),labels = c("25", "50", "75","100"))
p <- p + geom_boxplot( outlier.size = 1,width=0, size=0) 
p <- p + stat_summary(fun.y=median, geom="point", shape=23, size=2, color="black",fill="black")
p <- p + theme(legend.position=c(1,1),legend.justification=c(1,1),
               legend.direction="horizontal",
               legend.box="horizontal",
               legend.box.just = c("top"),
               legend.background = element_rect(fill="transparent"))


p <- p + labs(x="System")
p





