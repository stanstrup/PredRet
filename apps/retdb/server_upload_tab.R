## Drop down menu
output$system_upload <- renderUI({
  selectInput(inputId = 'system_upload',label= 'Select existing system',choices=c("",   as.character(unlist(lapply(systems_in_db(),function(x) x$system_name)))          ),selected="",selectize=TRUE)
})



## Table with info on uploaded file
output$filetable <- renderTable({
  if (is.null(input$files))    return(NULL)
  # User has not uploaded a file yet
  
  
  input$files
})




## Clean up the uploaded data and extract the relevant data
data_cleaned <- reactive({
  if (is.null(input$files))    return(NULL)
  # User has not uploaded a file yet
  
  
  # read data
  temp_data = read.csv(input$files$datapath,stringsAsFactors=F)
  
  # limit data shown used
  if (nrow(temp_data)==1){
    temp_data =      temp_data
  }else{
    temp_data   =   temp_data[1:200,]
  }
  
  
  # get only interesting columns and rename them
  colnames = tolower(colnames(temp_data))
  
  select=rep(NA,5)
  select[1] = grep("compound",colnames,fixed = T)[1]
  select[2] = grep("rt",colnames,fixed = T)[1]
  select[3] = grep("method",colnames,fixed = T)[1]
  select[4] = grep("pubchem",colnames,fixed = T)[1]
  select[5] = grep("inchi",colnames,fixed = T)[1]
  
  temp_data   =    temp_data[,select]
  colnames(temp_data) = c("name","rt","system_name","pubchem","inchi")
  
  temp_data[,"pubchem"] = as.integer(temp_data[,"pubchem"])
  
  # get the time
  time = Sys.time()
  
  # get the system ID from select if present. otherwise get from file.
  sys_name = as.character(unlist(lapply(systems_in_db(),function(x) x$system_name)))  
  sys_id = unlist(lapply(systems_in_db(),function(x) as.character.mongo.oid(x$`_id`))  )

  if(input$system_upload==""){  
    idx = match(temp_data[,"system_name"],sys_name)
    }else{
  idx = input$system_upload==sys_name
  }
  
  sys_id = sys_id[idx]
  
  # But everything together in a dataframe.
  temp_data =data.frame(sys_id,temp_data,time=time,userID=as.integer(userID()),username=as.character(username()),stringsAsFactors= FALSE)
  
  
  
  return(temp_data)
})




## Add the data to the database and output true when done
data_has_been_written <- reactive({
  if (is.null(input$files))    return(NULL)
  
  # Convert data.frame to bson
  bson_data = dataframe2bson(data_cleaned())
  
  
  # add to table
  mongo <- mongo.create()
  
  ns <- "test2.rtdata"
  
  wrote = mongo.insert.batch(mongo, ns, bson_data)
  
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
  wrote
})



## Text saying if the data was uploaded
output$is_written <- renderUI({  
  if(is.null(input$files))    return(NULL)   # User has not uploaded a file yet
  if(is.null(data_has_been_written()))   return(NULL)
  if(!(data_has_been_written()))      return(NULL)
 
    div("Data written to database")    
})



## Read all data back and display it
output$data <- renderTable({
  if(is.null(input$files))     return(NULL)   # User has not uploaded a file yet
  if(is.null(data_has_been_written()))  return(NULL)
  if(!(data_has_been_written()))      return(NULL)
  
  
       
        mongo <- mongo.create()
        ns <- "test2.rtdata"
        
        data_back = mongo.find.all2(mongo=mongo, ns=ns,query=mongo.bson.empty(),data.frame=T,mongo.oid2character=T)
        row.names(data_back) <- seq(nrow(data_back))
        
        del <- mongo.disconnect(mongo)
        del <- mongo.destroy(mongo)
        
       subset(data_back, select=-c(`_id`))
        
     
})

