setwd("/srv/shiny-server/shiny.apps/apps/retpred_shiny/retdb_admin/scheduled_tasks")
library(PredRetR)
PredRet.env$predret_local <- TRUE
purge_predictions() 

for(noi in 1:10){
  source("calc_all_models.R",local = T)
  source("mark_suspect_values.R",local = T)
  source("make_predictions.R",local = T)
}

setwd("/srv/shiny-server/shiny.apps/apps/retpred_shiny")






setwd("~/git_repositories/predret_shiny/retdb_admin/scheduled_tasks")
library(PredRetR)
PredRet.env$predret_local <- TRUE

purge_predictions() 

for(noi in 1:10){
  source("calc_all_models.R",local = T)
  source("mark_suspect_values.R",local = T)
  source("make_predictions.R",local = T)
}

setwd("/home/jan/git_repositories/predret_shiny")
