## Drop down menu
output$system_upload <- renderUI({
  
  sys_to_show = as.character(unlist(lapply(systems_in_db(),function(x) x$system_name)))    
  
  if(input$only_own_systems_upload){
    sys_userid = as.integer(unlist(lapply(systems_in_db(),function(x) x$userID)))
    sys_to_show =   sys_to_show[       sys_userid == userID()       ]
    
    if(all(is.na(sys_to_show))) {sys_to_show = ""}
  }
  
  
  
  selectInput(inputId = 'system_upload',label= 'Select existing system',choices=c("", sys_to_show   ),selected="",selectize=TRUE)
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
  
  # Make error messages
  errors = list(error=F,msg='')
  
  
  # limit data shown used
  if (nrow(temp_data)<200){
    temp_data =      temp_data
  }else{
    temp_data   =   temp_data[1:200,]
  }
  
  
  # get only interesting columns and rename them
  colnames = tolower(colnames(temp_data))
  cols_to_get = c("compound","rt","method","pubchem","inchi")
  select=rep(NA,length(cols_to_get))
  for(i in 1:length(cols_to_get)){
    select[i] = grep(cols_to_get[i],colnames,fixed = T)[1]
  }

  temp_data   =    temp_data[,select[!is.na(select)]]
  
  
  # change column names
  colnames(temp_data) = c("name","rt","system_name","pubchem","inchi")[!is.na(select)]
    
  
  # Make sure pubchem id is treated as integer
  if(any(colnames(temp_data)=="pubchem")){
  temp_data[,"pubchem"] = as.integer(temp_data[,"pubchem"])
  }
  
  
  # Check if all relevant columns are present
  if(any(is.na(select))){
    
    if(   (input$system_upload=="")     &      (  any(is.na(select[c(1,2,4)]))  )     ){
      errors = list(error=T,msg=paste0('The following column(s) were not found: ',   paste0(cols_to_get[is.na(select)],'. "compound","rt", "method" and "pubchem" required',collapse=", ")  ,'.'     ))
    }
  
    if(   (!(input$system_upload==""))  &      (  any(is.na(select[c(1:4)]))    )     ){
      errors = list(error=T,msg=paste0('The following column(s) were not found: ',   paste0(cols_to_get[is.na(select)],'. "compound","rt" and "pubchem" required',collapse=", ")  ,'.'     ))
    }
    
  }
  
  

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
  
  
  
  # check if all methods have a database match
  if(any(is.na(idx))){
    errors = list(error=T,msg=paste0('No system(s) called "',paste(unique(temp_data[is.na(idx),"system_name"])  , collapse=", "),'" (found in the csv file) was/were found in the database. Create a system with the corresponding name or select a single system in the upload column.'))
  }
  
  
  # Put everything together in a dataframe.
  temp_data =data.frame(sys_id,temp_data,time=time,userID=as.integer(userID()),username=as.character(username()),stringsAsFactors= FALSE)
  
  
# output error or data
if(errors$error){return(errors)}

  
  return(temp_data)
})




## Add the data to the database and output true when done
data_has_been_written <- reactive({
  if (is.null(input$files))    return(NULL)
  if(!is.data.frame(data_cleaned())) return(NULL)
  if(!is.null(data_cleaned()$error)){         if((data_cleaned()$error))  return(NULL)         }
  
  
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
  
  if((data_has_been_written())){   div("Data written to database")  }
  
})



## Text saying if errors
output$error_msg <- renderUI({  
  if(is.null(input$files))    return(NULL)   # User has not uploaded a file yet
  if(is.data.frame(data_cleaned())) return(NULL)
  if(is.null(data_cleaned()$error)){   return(NULL)         }
  
  
  if((data_cleaned()$error)){
  div(data_cleaned()$msg)
  }
  
})



## Read all data back and display it
output$data <- renderTable({
  if(is.null(input$files))     return(NULL)   # User has not uploaded a file yet
  if(is.null(data_has_been_written()))  return(NULL)
  if(!(data_has_been_written()))      return(NULL)
  if(!is.data.frame(data_cleaned())) return(NULL)
  if(!is.null(data_cleaned()$error)){         if((data_cleaned()$error))  return(NULL)         }
  
  
       
        mongo <- mongo.create()
        ns <- "test2.rtdata"
        
        data_back = mongo.find.all2(mongo=mongo, ns=ns,query=mongo.bson.empty(),data.frame=T,mongo.oid2character=T)
        row.names(data_back) <- seq(nrow(data_back))
        
        del <- mongo.disconnect(mongo)
        del <- mongo.destroy(mongo)
        
       subset(data_back, select=-c(`_id`))
        
     
})

