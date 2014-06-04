output$filetable <- renderTable({
  if (is.null(input$files))    return(NULL)
  # User has not uploaded a file yet
  
  
  input$files
})



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
  
  
  
  
  
  return(temp_data)
})





observe({
  if (is.null(input$files))    return(NULL)
  
  # Convert data.frame to bson
  bson_data = dataframe2bson(data_cleaned())
  
  
  # add to table
  mongo <- mongo.create()
  
  ns <- "test2.rtdata"
  
  mongo.insert.batch(mongo, ns, bson_data)
  
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
  output$is_written <- renderUI({
    div("Data written to database")
  })
  
  
  
  output$data <- renderTable({
    if (is.null(input$files))    return(NULL)
    # User has not uploaded a file yet
    
    
    #data_cleaned()
    
    
    
    mongo <- mongo.create()
    ns <- "test2.rtdata"
    
    data_back = mongo.find.all2(mongo=mongo, ns=ns,query=mongo.bson.empty(),data.frame=T,mongo.oid2character=T)
    row.names(data_back) <- seq(nrow(data_back))
    
    del <- mongo.disconnect(mongo)
    del <- mongo.destroy(mongo)
    
    subset(data_back, select=-c(`_id`))
    
  })
  
  
  
  
})
