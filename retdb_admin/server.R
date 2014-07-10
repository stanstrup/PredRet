shinyServer(function(input, output,session) {


  
  ## Basic settings ##################
  source("settings/mongodb.R",local=TRUE)
  
  
  ## get username ##################
  userID <- reactive({   input$userID })
  username <- reactive({   input$username })
  
  
  ## functions ##################
  source("functions/FUNCTIONS.R",local=TRUE)

  
  # some shared reactives ##################
  models <- reactive({get_models()})
  

  ## Tabs ##################
  source("server_explore.R",local=TRUE)
  source("server_modelstats.R",local=TRUE)
  source("server_build.R",local=TRUE)


})