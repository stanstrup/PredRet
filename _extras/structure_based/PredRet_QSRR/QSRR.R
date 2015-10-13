setwd("_extras/structure_based/PredRet_QSRR/")

library(camb)
library(impute)
library(doMC)
library(PredRetR)
library(obabel2R)
library(ChemmineR)
library(dplyr)
library(tidyr)
library(Rplot.extra)




dataset <-   PredRet_get_db(exp_pred = "exp",include_suspect = TRUE)
sdf <- inchi2sdf(dataset$inchi)
write.SDF(sdf, file="PredRet.sdf")
rm(sdf)

StandardiseMolecules(structures.file="PredRet.sdf",
                    standardised.file="PredRet_std.sdf",
                    removed.file="PredRet_rem.sdf",
                    properties.file = "properties.csv")




properties <- read.table("properties.csv", header=TRUE, sep="\t")
dataset <- dataset[properties$Kept==1, ]
rm(properties)




DensityResponse(dataset$recorded_rt) + xlab("RT Target Distribution")



descriptors <- GeneratePadelDescriptors(standardised.file = "PredRet_std.sdf",
                                        types = "2D", threads = 3)
descriptors <- RemoveStandardisedPrefix(descriptors)
saveRDS(descriptors, file = "descriptors.rds")



 descriptors       <- descriptors %>% 
                      # separate(col = "Name",into=c("Name","cmp_number")) %>% 
                     mutate(Name = as.numeric(Name)) %>% 
                     arrange(Name) %>% 
                     select(-Name) %>% 
                     ReplaceInfinitesWithNA %>% ImputeFeatures

colnames(descriptors) <- paste0("desc_",colnames(descriptors))


dataset <- cbind.data.frame(dataset,descriptors)
rm(descriptors)



dataset <- dataset %>% filter(system=="LIFE_old")






camb_dataset <- dataset %>% do(camb_dataset = SplitSet(.$name, select(.,starts_with("desc_")), .$recorded_rt, percentage = 20)) %>% 
                unlist(recursive = FALSE,use.names=FALSE) %>%  unlist(recursive = FALSE) %>% 
                RemoveNearZeroVarianceFeatures(frequencyCutoff = 30) %>% 
                RemoveHighlyCorrelatedFeatures(correlationCutoff = 0.95) %>% 
                PreProcess %>% 
                GetCVTrainControl(folds = 5)




#registerDoMC(cores = 3) # doesn't work for windows
library(doParallel) 
library(caret)
library(kernlab)

cl <- makePSOCKcluster(6)
registerDoParallel(cl)


method <- "svmRadial"
tune.grid <- expand.grid(.sigma = expGrid(-8, 4, 2,2), .C = c(1e-04, 0.001, 0.01, 0.1, 1, 10, 100))
model <- train(camb_dataset$x.train, camb_dataset$y.train, method,
               tuneGrid = tune.grid, trControl = camb_dataset$trControl)
saveRDS(model, file = paste(method, ".rds", sep = ""))





library(randomForest)
method <- "rf"
tune.grid <- expand.grid(.mtry = seq(5, 100, 5))
model <- train(camb_dataset$x.train, camb_dataset$y.train, method,
               tuneGrid = tune.grid, trControl = camb_dataset$trControl)
saveRDS(model, file = paste(method, ".rds", sep = ""))



library(gbm)
method <- "gbm"
tune.grid <- expand.grid(n.trees = c(500, 1000), interaction.depth = c(25), shrinkage = c(0.01, 0.02, 0.04, 0.08),n.minobsinnode = 10)
model <- train(camb_dataset$x.train, camb_dataset$y.train, method,
               tuneGrid = tune.grid, trControl = camb_dataset$trControl)
saveRDS(model, file = paste(method, ".rds", sep = ""))



method <- "pls"
model <- train(camb_dataset$x.train, camb_dataset$y.train, method,tuneLength = 15,
               tune.grid = NULL, trControl = camb_dataset$trControl,preProcess = c("center","scale"))
saveRDS(model, file = paste(method, ".rds", sep = ""))





method <- "nnet"
tune.grid <- expand.grid(.decay = c(5e-6,5e-5,5e-4,5e-3), .size = seq(1,5,1))
model <- train(x = camb_dataset$x.train,
               y= camb_dataset$y.train,
               method=method,
               tuneGrid = tune.grid, 
               trControl = camb_dataset$trControl
               )

saveRDS(model, file = paste(method, ".rds", sep = ""))





method <- "mlpWeightDecay"
tune.grid <- expand.grid(decay = c(5e-6,5e-5,5e-4,5e-3,5e-3), size = seq(1,8,1))
model <- train(x = camb_dataset$x.train,
               y= camb_dataset$y.train,
               method=method,
               tuneGrid = tune.grid, 
               trControl = camb_dataset$trControl
)

saveRDS(model, file = paste(method, ".rds", sep = ""))




method <- "svmPoly"
tune.grid <- expand.grid(degree=2, scale=c(0.001,0.005,0.01,0.05),C = c(1e-05,1e-04, 0.001, 0.01, 0.1,0.5))
model <- train(camb_dataset$x.train, camb_dataset$y.train, method,
               tuneGrid = tune.grid, trControl = camb_dataset$trControl)
saveRDS(model, file = paste(method, ".rds", sep = ""))






method <- "neuralnet"
tune.grid <- expand.grid(.layer1=c(7), .layer2=c(1:5), .layer3=c(1:5))
model <- train(camb_dataset$x.train, camb_dataset$y.train, method,
               tuneGrid = tune.grid, trControl = camb_dataset$trControl)
saveRDS(model, file = paste(method, ".rds", sep = ""))








stopCluster(cl)



plot(model, metric = "RMSE")
print(RMSE_CV(model, digits = 3))





holdout.predictions <- as.vector(predict(model$finalModel,
                                         newdata = camb_dataset$x.holdout))



CorrelationPlot(pred = holdout.predictions, obs = camb_dataset$y.holdout,
                PointSize = 3, ColMargin = "blue", TitleSize = 26,
                XAxisSize = 20, YAxisSize = 20, TitleAxesSize = 24,
                margin = 2, PointColor = "black", PointShape = 16,
                MarginWidth = 1, AngleLab = 0, xlab = "Observed",
                ylab = "Predicted")



















all.models <- list()
all.models[[length(all.models) + 1]] <- readRDS("gbm.rds")
all.models[[length(all.models) + 1]] <- readRDS("svmRadial.rds")
all.models[[length(all.models) + 1]] <- readRDS("rf.rds")
all.models[[length(all.models) + 1]] <- readRDS("pls.rds")
all.models[[length(all.models) + 1]] <- readRDS("nnet.rds")
all.models[[length(all.models) + 1]] <- readRDS("mlpWeightDecay.rds")
all.models[[length(all.models) + 1]] <- readRDS("svmPoly.rds")
all.models[[length(all.models) + 1]] <- readRDS("neuralnet.rds")
# sort the models from lowest to highest RMSE
names(all.models) <- sapply(all.models, function(x) x$method)
sort(sapply(all.models, function(x) min(as.vector(na.omit(x$results$RMSE)))))

# means tell another story
sort(apply(abs(preds - camb_dataset$y.holdout),2,median ))
sort(apply(abs(preds - camb_dataset$y.holdout),2,mean ))



greedy <- caretEnsemble(all.models, iter = 1000)
sort(greedy$weights, decreasing = TRUE)
saveRDS(greedy, file = "greedy.rds")
greedy$error



linear <- caretStack(all.models, method = "glm", trControl = trainControl(method = "cv"))
saveRDS(linear, file = "linear.rds")
linear$error



tune.grid <- expand.grid(.mtry = seq(1, length(all.models),1))
nonlinear <- caretStack(all.models, method = "rf",
                        trControl = trainControl(method = "cv"), tune.grid = tune.grid)


saveRDS(nonlinear, file = "nonlinear.rds")
nonlinear$error


library(pbapply)

preds <- data.frame(sapply(all.models, predict, newdata = camb_dataset$x.holdout))
preds$ENS_greedy <- predict(greedy, newdata = camb_dataset$x.holdout)
preds$ENS_linear <- predict(linear, newdata = camb_dataset$x.holdout)
preds$ENS_nonlinear <- predict(nonlinear, newdata = camb_dataset$x.holdout)
sort(sqrt(colMeans((preds - camb_dataset$y.holdout)^2)))



# means tell another story
sort(apply(abs(preds - camb_dataset$y.holdout),2,median ))
sort(apply(abs(preds - camb_dataset$y.holdout),2,mean ))





plotdata <- cbind.data.frame(id = as.character(camb_dataset$ids[camb_dataset$holdout.indexes])  , abs(preds-camb_dataset$y.holdout)) %>% 
  gather("method","error_abs",-id)



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

p <- ggplot( plotdata, aes( x = method, y = error_abs ) )
p <- p + scale_x_discrete(breaks=levels(plotdata$method), drop=FALSE)
p <- p + scale_y_continuous(breaks = seq(0, 10, 0.1))
p <- p + geom_violin(trim=TRUE, fill='black', color="black",adjust=0.3,scale="width",size=0,width=violin_width)
p <- p + labs(title="Absolute prediction errors", y="Error",fill="Quartiles")
p <- p + theme_bw_nice + theme_common
p <- p + geom_violin_quantile_fill(p=p,df_gr = plotdata[,"method"],df_data = plotdata[,"error_abs"],width=violin_width)
p <- p + scale_fill_manual(values = c("lightgrey","darkgrey","darkgrey","lightgrey"),labels = c("25", "50", "75","100"))
p <- p + geom_boxplot( outlier.size = 1,width=0, size=0) 
p <- p + stat_summary(fun.y=median, geom="point", shape=23, size=2, color="black",fill="black")

p <- p + theme(legend.position=c(1,1),legend.justification=c(1,1),
               legend.direction="horizontal",
               legend.box="horizontal",
               legend.box.just = c("top"),
               legend.background = element_rect(fill="transparent"))


p <- p + labs(x="Method")
p
