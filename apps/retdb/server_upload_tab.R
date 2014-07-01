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
output$filetable <- renderTable({
  if (is.null(input$files))    return(NULL)
  # User has not uploaded a file yet
  
  
  input$files
})





## Clean up the uploaded data and extract the relevant data ###########################
source("server_upload_tab_process_data.R",local=TRUE)




## Add the data to the database and output true when done ###########################
data_has_been_written <- reactive({
  if (is.null(input$files))    return(NULL)
  if(!is.data.frame(data_cleaned())) return(NULL)
 
  
  # Convert data.frame to bson
  bson_data = dataframe2bson(data_cleaned())
  
  
  # add to table
  mongo <- mongo.create()
  wrote = mongo.insert.batch(mongo, ns_rtdata, bson_data)
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
  return(wrote)
})



## Text saying if the data was uploaded ###########################
output$is_written <- renderUI({  
  if(is.null(input$files))    return(NULL)   # User has not uploaded a file yet
  if(is.null(data_has_been_written()))   return(NULL)
  
  if((data_has_been_written())){   div("Data written to database")  }
  
})



## Text saying if errors ###########################
output$error_msg <- renderUI({  
  if(is.null(input$files))    return(NULL)   # User has not uploaded a file yet
  if(is.data.frame(data_cleaned())) return(NULL)
   
  out = paste0('<p><strong>',as.character(lapply(data_cleaned(),function(x) x$msg)),'</strong></p>',collapse="<br />")
  out = div(HTML(out),style="background-color:red")
  
})



## Read all data back and display it ###########################
output$data <- renderTable({
  if(is.null(input$files))     return(NULL)   # User has not uploaded a file yet
  if(is.null(data_has_been_written()))  return(NULL)
  if(!(data_has_been_written()))      return(NULL)
  if(!is.data.frame(data_cleaned())) return(NULL)
 
       
        mongo <- mongo.create()
        
        data_back = mongo.find.all2(mongo=mongo, ns=ns_rtdata,query=mongo.bson.empty(),data.frame=T,mongo.oid2character=T)
        row.names(data_back) <- seq(nrow(data_back))
        
        del <- mongo.disconnect(mongo)
        del <- mongo.destroy(mongo)
        
  
  data_back = data_back[         max(data_back$time) == data_back$time      ,,drop=F]
  
  data_back$time = as.character(as.POSIXct(data_back$time,origin="1970-01-01"))
  
  data_back = subset(data_back, select=-c(`_id`))
        
     return(data_back)
})

