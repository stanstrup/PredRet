library(shiny)
library(xlsx)
library(rmongodb)
library(rmongodb.quick)


shinyServer(function(input, output,session) {

  ## get username ##################
  userID <- reactive({   input$userID })
  username <- reactive({   input$username })
  
  
  ## functions ##################
  source("inputTextarea.R",local=TRUE)
  
  
  ## Upload data ##################
  source("server_upload_tab.R",local=TRUE)
  
  
  ## Define new system ##################
  source("server_system_tab.R",local=TRUE)
  

  

  
  
  
})
