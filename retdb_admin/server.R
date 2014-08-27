library(rCharts)

shinyServer(function(input, output,session) {


    
  ## get username ##################
  userID <- reactive({   input$userID })
  username <- reactive({   input$username })
  user_logged_in <- reactive({   input$user_logged_in })
  is_admin <- reactive({   input$is_admin })
  
  
  
  
   observe({
      if(is.null(user_logged_in())) return(NULL) # when not set yet
      if(is.null(is_admin())) return(NULL) # when not set yet
      if(  user_logged_in() == ""    ) return(NULL) # when not set yet
      if(  is_admin() == ""    ) return(NULL) # when not set yet
      if(  !(as.logical(user_logged_in()))  ) return(NULL) # if not logged in
      if(  !(as.logical(is_admin()))  ) return(NULL) # if not admin
     
    
  ## Basic settings ##################
  source("settings/mongodb.R",local=TRUE)
  
  
  ## functions ##################
  source("functions/FUNCTIONS.R",local=TRUE)

  
  # some shared reactives ##################
  models <- reactive({get_models(include.loess=FALSE,include.ci = FALSE,include.newdata = FALSE )})
  
  
    
  ## Tabs ##################
  source("server_explore.R",local=TRUE)
  source("server_modelstats.R",local=TRUE)
  source("server_build.R",local=TRUE)

   })
  




})