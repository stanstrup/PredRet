shinyServer(function(input, output,session) {


  
  ## Basic settings ##################
  source("settings/mongodb.R",local=TRUE)
  
  
  ## get username ##################
  userID <- reactive({   input$userID })
  username <- reactive({   input$username })
  
  
  ## functions ##################
  source("functions/FUNCTIONS.R",local=TRUE)
  

  ## Explore data ##################
  source("server_explore.R",local=TRUE)



})