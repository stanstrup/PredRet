library(shiny)
library(shinyBS)
library(xlsx)
library(rmongodb)
library(rmongodb.quick)
library(chemhelper)
library(Rdisop)
library(stringr)
library(PredRetR)


shinyServer(function(input, output,session) {

  
  ## get username ##################
  userID <- reactive({   as.integer(input$userID) })
  username <- reactive({   input$username })
  user_logged_in <- reactive({   input$user_logged_in })
  
  
  ## get user time ##################
  client_time <- reactive(as.numeric(input$client_time) / 1000) # in s
  time_zone_offset <- reactive(as.numeric(input$client_time_zone_offset) * 60 ) # in s 
  
  
  observe({
    
    if(is.null(user_logged_in())) return(NULL) # when not set yet
    if(  user_logged_in() == ""    ) return(NULL) # when not set yet
    if(  !(as.logical(user_logged_in()))  ) return(NULL) # if not logged in
    
    
  ## Basic settings ##################
  source("settings/mongodb.R",local=TRUE)
  source("settings/predictions.R",local=TRUE)
  
  
  
  ## functions ##################
  source("functions/inputTextarea.R",local=TRUE)
  
  
  
  ## Tabs ##################
  source("server/server_upload_tab.R",local=TRUE)
  source("server/server_system_tab.R",local=TRUE)
  source("server/server_manage.R",local=TRUE)
  source("server/server_suspect_values.R",local=TRUE)
  source("server/server_predictions_tab.R",local=TRUE)
  source("server/server_data_tab.R",local=TRUE)
  
  
  # some shared reactives ##################
  models <- reactive({get_models(ns = ns_sysmodels,include.loess=FALSE,include.ci = FALSE,include.newdata = FALSE )})

  })

  
  
  
})
