setwd("/srv/shiny-server/shiny.apps/apps/retpred_shiny/retdb_admin/scheduled_tasks")
source("../settings/mongodb.R",local=TRUE)
source("../settings/predictions.R",local=TRUE)
library(PredRetR)

purge_predictions(ns_rtdata=ns_rtdata,ns_pred_stats=ns_pred_stats) 

source("calc_all_models.R")
source("mark_suspect_values.R")
source("make_predictions.R")

source("calc_all_models.R")
source("mark_suspect_values.R")
source("make_predictions.R")

setwd("/srv/shiny-server/shiny.apps/apps/retpred_shiny")






setwd("~/git_repositories/predret_shiny/retdb_admin/scheduled_tasks")
source("../settings/mongodb.R",local=TRUE)
source("../settings/predictions.R",local=TRUE)
library(PredRetR)

purge_predictions(ns_rtdata=ns_rtdata,ns_pred_stats=ns_pred_stats) 

source("calc_all_models.R")
source("mark_suspect_values.R")
source("make_predictions.R")

source("calc_all_models.R")
source("mark_suspect_values.R")
source("make_predictions.R")

setwd("/home/jan/git_repositories/predret_shiny")





