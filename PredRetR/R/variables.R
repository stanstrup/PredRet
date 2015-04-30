## Make environment ##################
PredRet.env <- new.env()


## Connect locally or not ##################
PredRet.env$predret_local <- FALSE


## Namespaces ##################
PredRet.env$namespaces$ns_chrom_systems           <- "predret.chrom_systems"
PredRet.env$namespaces$ns_pred_stats              <- "predret.pred_stats"
PredRet.env$namespaces$ns_rtdata                  <- "predret.rtdata"
PredRet.env$namespaces$ns_sysmodels               <- "predret.sysmodels"
PredRet.env$namespaces$ns_sysmodels_log           <- "predret.sysmodels_log"


## Prediction ##################
PredRet.env$prediction$ci_width_limit             <- 2
PredRet.env$prediction$ci_width_limit_rel         <- 0.2
PredRet.env$prediction$predict_near_x_bw_mult     <- 0.03
PredRet.env$prediction$predict_near_x_density_lim <- 0.01


## Mark suspect values ##################
PredRet.env$suspect$suspect_CI_multiplier      <- 2
