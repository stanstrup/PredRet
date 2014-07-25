library(shiny)
library(xlsx)
library(rmongodb)
library(rmongodb.quick)
library(chemhelper)


shinyServer(function(input, output,session) {

  
  ## get username ##################
  userID <- reactive({   input$userID })
  username <- reactive({   input$username })
  user_logged_in <- reactive({   input$user_logged_in })
  
  
  
  observe({
    
    if(is.null(user_logged_in())) return(NULL) # when not set yet
    if(  user_logged_in() == ""    ) return(NULL) # when not set yet
    if(  !(as.logical(user_logged_in()))  ) return(NULL) # if not logged in
    
    
  ## Basic settings ##################
  source("settings/mongodb.R",local=TRUE)
  
  
  
  ## functions ##################
  source("functions/inputTextarea.R",local=TRUE)
  source("functions/FUNCTIONS.R",local=TRUE)
  
  
  ## Upload data ##################
  source("server_upload_tab.R",local=TRUE)
  
  
  ## Define new system ##################
  source("server_system_tab.R",local=TRUE)

  ## Define new system ##################
  source("server_manage.R",local=TRUE)

  })

  
  
  
})
