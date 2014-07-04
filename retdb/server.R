library(shiny)
library(xlsx)
library(rmongodb)
library(rmongodb.quick)
library(chemhelper)


shinyServer(function(input, output,session) {

  
  ## Basic settings ##################
  source("settings/mongodb.R",local=TRUE)
  
  
  ## get username ##################
  userID <- reactive({   input$userID })
  username <- reactive({   input$username })
  
  
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
