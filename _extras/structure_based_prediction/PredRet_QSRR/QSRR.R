
# Change working directory ------------------------------------------------
setwd("_extras/structure_based_prediction/PredRet_QSRR/")


# Load some libraries -----------------------------------------------------
library(plyr)
library(dplyr)
library(camb)
library(impute)
#library(doMC) # not on windows
library(PredRetR)
library(obabel2R)
library(ChemmineR)
library(tidyr)
library(Rplot.extra)
library(magrittr)
library(doParallel) 
library(caret)
library(pbapply)
library(parallel)



# Get the data from PredRet and write SDF file ----------------------------
dataset <-   PredRet_get_db(exp_pred = c("exp","pred"),include_suspect = TRUE)
sdf <- inchi2sdf(dataset$inchi)
write.SDF(sdf, file="PredRet.sdf")
rm(sdf)



# Use camb to standardize molecules ---------------------------------------
StandardiseMolecules(structures.file="PredRet.sdf",
                    standardised.file="PredRet_std.sdf",
                    removed.file="PredRet_rem.sdf",
                    properties.file = "properties.csv")


properties <- read.table("properties.csv", header=TRUE, sep="\t")
dataset <- dataset[properties$Kept==1, ]
rm(properties)




# Density of the target variable ------------------------------------------
DensityResponse(dataset$recorded_rt) + xlab("RT Target Distribution")




# Calculate descriptors with camb/Padel -----------------------------------
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

colnames(descriptors) <- paste0("desc_padel_",colnames(descriptors))


dataset <- cbind.data.frame(dataset,descriptors)
rm(descriptors)




# get Rcdk descriptors ----------------------------------------------------
# need to restart. dunno what the hell the incompatibility is.
library(rcdk)
mols <- load.molecules("PredRet_std.sdf")
rcdk_desc <- get.desc.categories() %>% sapply(get.desc.names) %>% unlist %>% as.character
rcdk_desc <- eval.desc(mols, rcdk_desc, verbose=TRUE)

# cl <- makeCluster(detectCores()-1)
# clusterExport(cl, "mols")
# rcdk_desc <- parLapply(cl,rcdk_desc,function(x){ require(rcdk); eval.desc(mols, x, verbose=FALSE)})
# stopCluster(cl)

rcdk_desc <- rcdk_desc[,apply(rcdk_desc,2,function(x) !all(is.na(x)))]
colnames(rcdk_desc) <- paste0("desc_cdk_",colnames(rcdk_desc))

dataset <- cbind.data.frame(dataset,rcdk_desc)
rm(rcdk_desc)



# Add jchem descriptors ---------------------------------------------------
library(jchemR)
cxcalc_add_3D_sdffile(infile="PredRet_std.sdf",outfile="PredRet_std_3D.sdf",verbose=T)
properties <- cxcalc_add_property_sdffile(infile="PredRet_std_3D.sdf",outfile="PredRet_std_3D_prop.sdf",pH_step = 0.5,verbose=T )
saveRDS(properties, file = "calc_properties.rds")




# Select only one CS for this testing -------------------------------------
dataset <- dataset %>% filter(system=="LIFE_old")




# Make a list of the dataset containing the data and tuning parame --------
camb_dataset <- dataset %>% filter(!is.na(recorded_rt)) %>% 
                do(camb_dataset = SplitSet(.$name, select(.,starts_with("desc_padel_")), .$recorded_rt, percentage = 20)) %>% 
                #do(camb_dataset = SplitSet(.$name, dplyr::select(.,one_of(var_select)), .$recorded_rt, percentage = 20)) %>% 
                unlist(recursive = FALSE,use.names=FALSE) %>%  unlist(recursive = FALSE) %>% 
                RemoveNearZeroVarianceFeatures(frequencyCutoff = 30) %>% 
                RemoveHighlyCorrelatedFeatures(correlationCutoff = 0.95) %>% 
                PreProcess %>% 
                GetCVTrainControl(folds = 5)






# Run a large number of models --------------------------------------------

#registerDoMC(cores = 3) # doesn't work for windows
cl <- makePSOCKcluster(6)
registerDoParallel(cl)


method <- "svmRadial"
tune.grid <- expand.grid(.sigma = expGrid(-8, 4, 2,2), .C = c(1e-04, 0.001, 0.01, 0.1, 1, 10, 100))
model <- train(camb_dataset$x.train, camb_dataset$y.train, method,
               tuneGrid = tune.grid, trControl = camb_dataset$trControl)
saveRDS(model, file = paste(method, ".rds", sep = ""))


method <- "rf"
tune.grid <- expand.grid(.mtry = seq(5, 100, 5))
model <- train(camb_dataset$x.train, camb_dataset$y.train, method,
               tuneGrid = tune.grid, trControl = camb_dataset$trControl)
saveRDS(model, file = paste(method, ".rds", sep = ""))


method <- "gbm"
tune.grid <- expand.grid(n.trees = c(200,300,400,500), interaction.depth = c(15,25,30), shrinkage = c(0.01,0.02, 0.04, 0.08),n.minobsinnode = c(10))
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




# Plot parameter effect on RMSE (last model) ------------------------------
plot(model, metric = "RMSE")




# predict RT for holdout compounds and plot correlation -------------------
holdout.predictions <- as.vector(predict(model$finalModel,
                                         newdata = camb_dataset$x.holdout))



CorrelationPlot(pred = holdout.predictions, obs = camb_dataset$y.holdout,
                PointSize = 3, ColMargin = "blue", TitleSize = 26,
                XAxisSize = 20, YAxisSize = 20, TitleAxesSize = 24,
                margin = 2, PointColor = "black", PointShape = 16,
                MarginWidth = 1, AngleLab = 0, xlab = "Observed",
                ylab = "Predicted")






# Make a list of all the models -------------------------------------------
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




# Create Ensemble models --------------------------------------------------
greedy <- caretEnsemble(all.models, iter = 1000)
sort(greedy$weights, decreasing = TRUE)
saveRDS(greedy, file = "greedy.rds")


linear <- caretStack(all.models, method = "glm", trControl = trainControl(method = "cv"))
saveRDS(linear, file = "linear.rds")


tune.grid <- expand.grid(.mtry = seq(1, length(all.models),1))
nonlinear <- caretStack(all.models, method = "rf",
                        trControl = trainControl(method = "cv"), tune.grid = tune.grid)
saveRDS(nonlinear, file = "nonlinear.rds")





# Make predictions for all the models -------------------------------------
preds <- data.frame(sapply(all.models, predict, newdata = camb_dataset$x.holdout))
preds$ENS_greedy <- predict(greedy, newdata = camb_dataset$x.holdout)
preds$ENS_linear <- predict(linear, newdata = camb_dataset$x.holdout)
preds$ENS_nonlinear <- predict(nonlinear, newdata = camb_dataset$x.holdout)



# Show the performance of each model --------------------------------------
sort(sqrt(colMeans((preds - camb_dataset$y.holdout)^2)))
sort(apply(abs(preds - camb_dataset$y.holdout),2,median ))
sort(apply(abs(preds - camb_dataset$y.holdout),2,mean ))




# Plot the accuracy of each model for each compound in a violin plot ------
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
p <- p + scale_y_continuous(breaks = seq(0, 10, 0.1),limits=c(0,2))
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













# Variable importance for gbm and optimizing new model --------------------
model_select <- all.models[[1]]
model_select %>% varImp
model_select %>% varImp %>% plot(20)

var_select <- model_select %>% varImp %$% importance %>% add_rownames("variable") %>%  filter(Overall>5) %>% select_("variable") %>% 
              as.matrix %>% as.character


camb_dataset_var_select <- dataset %>% do(camb_dataset = SplitSet(.$name, dplyr::select(.,one_of(var_select)), .$recorded_rt, percentage = 20)) %>% 
                          unlist(recursive = FALSE,use.names=FALSE) %>%  unlist(recursive = FALSE) %>% 
                          GetCVTrainControl(folds = 5)




method <- "gbm"
tune.grid <- expand.grid(n.trees = c(200,300,400,500), interaction.depth = c(15,25,30), shrinkage = c(0.01,0.02, 0.04, 0.08),n.minobsinnode = c(10))
model <- train(camb_dataset_var_select$x.train, camb_dataset_var_select$y.train, method,
               tuneGrid = tune.grid, trControl = camb_dataset_var_select$trControl)


plot(model)



min(all.models[[1]]$results$RMSE) # whole model from before
min(model$results$RMSE) # select variable model
