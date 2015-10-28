# Change working dir ------------------------------------------------------
setwd("_extras/structure_based_prediction/PredRet_QSRR/")


# Load packages -----------------------------------------------------------
library(plyr)
library(dplyr)
library(tidyr)
library(Rplot.extra) # devtools::install_github("stanstrup/Rplot.extra")
library(parallel)
library(caret)
library(doParallel)
library(qdapTools)
library(magrittr)


# Load the database (can also be pulled directly) -------------------------
load("dataset_all_with_pred.RData")
load("camb functions.RData") # since the camb package doesn't run on windows




# Set loop to run through all systems -------------------------------------
systems <- unique(dataset$system)
systems <- expand.grid(pred = c(FALSE,TRUE),systems = systems)
models <- list()




# Make models for all systems (this will take some time) ------------------
#models <- readRDS(models, file = "models.rds") # To save time you can load the data from here too

cl <- makePSOCKcluster(detectCores()-1)
registerDoParallel(cl)


for(i in 1:nrow(systems)){
  
  camb_dataset[[i]] <- dataset %>% filter(system==systems[i,"systems"])
  
  if(!systems[i,"pred"]){    camb_dataset[[i]] %<>% select(-predicted_rt) %>% filter(!is.na(recorded_rt))    }
  if( systems[i,"pred"]){    camb_dataset[[i]] %<>% mutate(recorded_rt = ifelse(  is.na(recorded_rt),predicted_rt,recorded_rt) )      }
    
  camb_dataset[[i]] %<>%  do(camb_dataset = SplitSet(.$name, select(.,starts_with("desc_")), .$recorded_rt, percentage = 20)) %>% 
                          unlist(recursive = FALSE,use.names=FALSE) %>%  unlist(recursive = FALSE) %>% 
                          RemoveNearZeroVarianceFeatures(frequencyCutoff = 30) %>% 
                          RemoveHighlyCorrelatedFeatures(correlationCutoff = 0.95) %>%
                          GetCVTrainControl(folds = 5)
  
  
  
  method <- "gbm"
  tune.grid <- expand.grid(n.trees = c(300,600,800), interaction.depth = c(10,15,30,40), shrinkage = c(0.01,0.02,0.05),n.minobsinnode = c(5,10,15,25))
  
  models[[i]] <- try(train(camb_dataset[[i]]$x.train, camb_dataset[[i]]$y.train, method,
                      tuneGrid = tune.grid, trControl = camb_dataset[[i]]$trControl),silent = TRUE)

}


stopCluster(cl)

#saveRDS(models, file = "models.rds")






# RMSE plots for the models -----------------------------------------------
plot(models[[1]], metric = "RMSE") # remember to change the number to select the model you want







# Make data for plotting --------------------------------------------------
plotdata <- lapply(1:length(models), function(i) {
  
  if(class(models[[i]])=="try-error") return(NULL)
  
  preds <- predict(models[[i]],newdata = camb_dataset[[i]]$x.holdout)
  
  plotdata <- cbind.data.frame(id        = as.character(camb_dataset[[i]]$ids[camb_dataset[[i]]$holdout.indexes])    ,
                               system    = as.character(systems[i,"systems"])    ,
                               preds     = systems[i,"pred"]    , 
                               error_abs = abs(preds-camb_dataset[[i]]$y.holdout),
                               error_rel = abs(preds-camb_dataset[[i]]$y.holdout) / camb_dataset[[i]]$y.holdout
  )
  
  return(plotdata) 
  
})



plotdata <- do.call(rbind.data.frame,plotdata)





# Make a plot of the accuracy ---------------------------------------------
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

p <- ggplot( plotdata, aes( x = preds, y = error_rel ) )
#p <- p + scale_x_discrete(breaks=levels(interaction(plotdata$system,plotdata$preds)), drop=FALSE)
p <- p + scale_y_continuous(breaks = seq(0, 50, 0.2),limits=c(0,2))
p <- p + geom_violin(trim=TRUE, adjust=0.3,scale="width",size=0,width=violin_width)
p <- p + labs(title="Relative prediction errors", y="Error",fill="Quartiles")
p <- p + theme_bw_nice + theme_common
#p <- p + geom_violin_quantile_fill(p=p,df_gr = interaction(plotdata[,"preds"],plotdata[,"system"]),df_data = plotdata[,"error_abs"],width=violin_width)
p <- p + scale_fill_manual(values = c("lightgrey","darkgrey","darkgrey","lightgrey"),labels = c("25", "50", "75","100"))
p <- p + geom_boxplot( outlier.size = 1,width=0, size=0) 
p <- p + stat_summary(fun.y=median, geom="point", shape=23, size=2, color="black",fill="black")
p <- p + theme(legend.position=c(1,1),legend.justification=c(1,1),
               legend.direction="horizontal",
               legend.box="horizontal",
               legend.box.just = c("top"),
               legend.background = element_rect(fill="transparent"))
 p <- p + facet_grid (~system)

p <- p + labs(x="System")
p


