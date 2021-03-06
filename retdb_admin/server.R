library(shiny)
library(rCharts)
library(shinyIncubator)
library(PredRetR)


shinyServer(function(input, output,session) {


    
  ## get username ##################
  if(PredRet.env$auth$wordpress_auth){
    userID <- reactive({   input$userID })
    username <- reactive({   input$username })
    user_logged_in <- reactive({   input$user_logged_in })
    is_admin <- reactive({   input$is_admin })
  }
  
  
  ## get user time ##################
  client_time <- reactive(as.numeric(input$client_time) / 1000) # in s
  time_zone_offset <- reactive(as.numeric(input$client_time_zone_offset) * 60 ) # in s 
  
  
   observe({
     
     if(PredRet.env$auth$wordpress_auth){
        if(is.null(user_logged_in())) return(NULL) # when not set yet
        if(is.null(is_admin())) return(NULL) # when not set yet
        if(  user_logged_in() == ""    ) return(NULL) # when not set yet
        if(  is_admin() == ""    ) return(NULL) # when not set yet
        if(  !(as.logical(user_logged_in()))  ) return(NULL) # if not logged in
        if(  !(as.logical(is_admin()))  ) return(NULL) # if not admin
     }
  
  
  # some shared reactives ##################
  models <- reactive({get_models(include.loess=FALSE,include.ci = FALSE,include.newdata = FALSE ,include.xy_mat=TRUE)})
  
  
    
  ## Tabs ##################
  source("server_explore.R",local=TRUE)
  source("server_modelstats.R",local=TRUE)
  source("server_predstats.R",local=TRUE)
  source("server_build.R",local=TRUE)

   })
  




})
