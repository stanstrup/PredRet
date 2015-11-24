RemoveHighlyCorrelatedFeatures <- 
function(dataset, correlationCutoff = 0.95,...) {
  hc.columns <- findCorrelation(cor(dataset$x.train,use="pairwise.complete.obs"), correlationCutoff,...)
  if (length(hc.columns) != 0) {
    message(paste(length(hc.columns), "features removed with correlation above cutoff"))
    hc.columns[which(is.na(hc.columns))] <- which(is.na(hc.columns))
    dataset$x.train <- dataset$x.train[, -hc.columns]
    dataset$x.holdout <- dataset$x.holdout[, -hc.columns]
  }
  else {
    message("no features removed")
  }
  dataset
}
