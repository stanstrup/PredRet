library(shiny)
library(xlsx)
library(rmongodb)
library(rmongodb.quick)


shinyServer(function(input, output,session) {

  
  
  ## Upload data ##################
  source("server_upload_tab.R",local=TRUE)
  
  
  ## Define new system ##################
  source("server_system_tab.R",local=TRUE)
  


  
  
  
})
