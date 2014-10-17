## Drop down menu ###########################
output$system_upload <- renderUI({
  
  sys_to_show = as.character(unlist(lapply(systems_in_db(),function(x) x$system_name)))    
  
  if(input$only_own_systems_upload){
    sys_userid = as.integer(unlist(lapply(systems_in_db(),function(x) x$userID)))
    sys_to_show =   sys_to_show[       sys_userid == userID()       ]
    
    if(all(is.na(sys_to_show))) {sys_to_show = ""}
  }
  
  
  
  selectInput(inputId = 'system_upload',label= 'Select existing system',choices=c("", sys_to_show   ),selected="",selectize=TRUE)
})





## Table with info on uploaded file ###########################
# output$filetable <- renderTable({
#   if (is.null(input$upload_go_Button))    return(NULL) # button is pushed. It is NULL before it is properly initialized
#   if ((input$upload_go_Button)==0)    return(NULL) # button is pushed. It is 0 before the button is pushed the first time
#   if (is.null(input$files))    return(NULL)
#   # User has not uploaded a file yet
#   
#   
#   input$files
# })





## Clean up the uploaded data and extract the relevant data ###########################
source("server_upload_tab_process_data.R",local=TRUE)




## Add the data to the database and output true when done ###########################
data_has_been_written <- reactiveValues()
 observe({
   if (is.null(input$upload_go_Button))    return(NULL) # button is pushed. It is NULL before it is properly initialized
   if ((input$upload_go_Button)==0)    return(NULL) # button is pushed. It is 0 before the button is pushed the first time
  if (is.null(input$files))    return(NULL)
  if(any(unlist(lapply(data_cleaned()$errors,function(x) x$error==1))))  return(NULL)
  
  isolate({
    
    # reset "done" if new file is selected or button is pushed
    if(is.null(data_has_been_written$done)){
      data_has_been_written$done <- 0L
    }else{
      data_has_been_written$done <- 0L
    }
    
    
    
  # Convert data.frame to bson
  bson_data = mongo.bson.from.df(data_cleaned()$data)
  
  
  # add to table
  mongo <- mongo.create()
  wrote = mongo.insert.batch(mongo, ns_rtdata, bson_data)
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
  
  data_has_been_written$done <- as.numeric(wrote)

  
  })
})



## Text saying if the data was uploaded ###########################
output$is_written <- renderUI({  
  if (is.null(input$upload_go_Button))    return(NULL) # button is pushed. It is NULL before it is properly initialized
  if ((input$upload_go_Button)==0)    return(NULL) # button is pushed. It is 0 before the button is pushed the first time
  if(is.null(input$files))    return(NULL)   # User has not uploaded a file yet
  if(is.null(data_has_been_written$done))   return(NULL)
  
  if((data_has_been_written$done==1)){   
    createAlert(session,inputId="upload_complete",alertId="alertID_upload_done",type="success",title=c("Done!"),message="Data written to database",dismiss=TRUE,append = FALSE)
  }
  
})



## Text saying if errors ###########################
old_error_msg <- reactiveValues()

observe({
  if (is.null(input$upload_go_Button))    return(NULL) # button is pushed. It is NULL before it is properly initialized
  if ((input$upload_go_Button)==0)    return(NULL) # button is pushed. It is 0 before the button is pushed the first time
  
  # close previous alerts from other runs
  if(!is.null(old_error_msg$N)){
    if(old_error_msg$N != 0){
      for(i in 1:old_error_msg$N){
        closeAlert(session,alertId=paste0("alertId",i))
      }
    }
  }
  
  
  # Get new errors
  msgtype <- sapply(data_cleaned()$errors, function(x) x$error)
  msg     <- sapply(data_cleaned()$errors, function(x) x$msg)
    

  if(length(msgtype)>0){
    for(i in 1:length(msgtype)){
      createAlert(session,inputId="upload_alerts",alertId=paste0("alertId",i),title=c("Errors","Information")[msgtype[i]],message=as.character(msg[i]),type=c("danger","info")[msgtype[i]],dismiss=TRUE,append = TRUE)
    }
    old_error_msg$N <- length(msgtype)
  }else{
    old_error_msg$N <- 0L
  }
 
  



})



## Read all data back and display it ###########################
# output$data <- renderTable({
#   if (is.null(input$upload_go_Button))    return(NULL) # button is pushed. It is NULL before it is properly initialized
#   if ((input$upload_go_Button)==0)    return(NULL) # button is pushed. It is 0 before the button is pushed the first time
#   if(is.null(input$files))     return(NULL)   # User has not uploaded a file yet
#   if(is.null(data_has_been_written$done))  return(NULL)
#   if(!(data_has_been_written$done))      return(NULL)
#   if(any(unlist(lapply(data_cleaned()$errors,function(x) x$error==1))))  return(NULL)
#        
#         mongo <- mongo.create()
#         
#         data_back = mongo.find.all2(mongo=mongo, ns=ns_rtdata,query=mongo.bson.empty(),data.frame=T,mongo.oid2character=T)
#         row.names(data_back) <- seq(nrow(data_back))
#         
#         del <- mongo.disconnect(mongo)
#         del <- mongo.destroy(mongo)
#         
#   
#   data_back = data_back[         max(data_back$time) == data_back$time      ,,drop=F]
#   
#   data_back$time = as.character(as.POSIXct(data_back$time,origin="1970-01-01"))
#   
#   data_back = subset(data_back, select=-c(`_id`))
#         
#      return(data_back)
# })
