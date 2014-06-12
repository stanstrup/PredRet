## Get list of systems already in the database ###########################
systems_in_db <- reactive({
  # Make sure it updates on submission
  input$submit_system
  
  # Connect to db
  mongo <- mongo.create()
  ns <- "test2.chrom_systems"
  
  
  # select fields (think columns)
  fields = mongo.bson.buffer.create()
  mongo.bson.buffer.append(fields, "_id", 1L)
  mongo.bson.buffer.append(fields, "system_name", 1L)
  mongo.bson.buffer.append(fields, "system_desc", 1L)
  mongo.bson.buffer.append(fields, "userID", 1L)
  mongo.bson.buffer.append(fields, "username", 1L)
  fields = mongo.bson.from.buffer(fields)
  
  data_back = mongo.find.all2(mongo, ns=ns,fields=fields)
  
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
  data_back
})




## Make UI elements ###########################
## Drop down menu
output$system_name_select <- renderUI({
  selectInput(inputId = 'system_name_select',label= 'Select existing system',choices=c("",   as.character(unlist(lapply(systems_in_db(),function(x) x$system_name)))          ),selected="",selectize=TRUE)
})

## System name text box
output$system_name <- renderUI({
  textInput('system_name', label='System name',value = input$system_name_select)
})


## Text area with system desc
output$system_desc <- renderUI({
  if (is.null(input$system_name_select))    return(NULL)
  
# if a system is selected fetch the description
  if(!(input$system_name_select=="")){
  #sys_id = as.character(unlist(lapply(systems_in_db(),function(x) x$`_id`)))  
  sys_name = as.character(unlist(lapply(systems_in_db(),function(x) x$system_name)))  
  sys_desc = as.character(unlist(lapply(systems_in_db(),function(x) x$system_desc)))  
  idx = input$system_name_select==sys_name
  desc_shown = sys_desc[idx]
  
  }else{
    desc_shown = "Describe column, solvents, modifiers and gradient etc."
  }
  
  inputTextarea('system_desc','System description', desc_shown ,300,600)
})







## Write to the db when button is pushed ###########################
# Make the object
system_desc_bson <- reactive({
  if(input$submit_system == 0) return(NULL)
  
  isolate({
    buf <- mongo.bson.buffer.create()
    mongo.bson.buffer.append(buf, "time", Sys.time())
    mongo.bson.buffer.append(buf, "userID", as.integer(userID()))
    mongo.bson.buffer.append(buf, "username", username())
    mongo.bson.buffer.append(buf, "system_name", input$system_name)
    mongo.bson.buffer.append(buf, "system_desc", input$system_desc)
    mongo.bson.from.buffer(buf)
  })
})


# Actually write the object
observe({
  if(input$submit_system == 0) return(NULL)
  
  
  isolate({
  # make db connection
  mongo <- mongo.create()
  ns <- "test2.chrom_systems"
  
  
  # Systems already in the db
  sys_name = as.character(unlist(lapply(systems_in_db(),function(x) x$system_name)))
  
  if(any(sys_name==input$system_name)){ # the system is already in the db
    idx = input$system_name==sys_name
    
    buf <- mongo.bson.buffer.create()
    mongo.bson.buffer.append(buf, "_id", systems_in_db()[[which(idx)]]$`_id`)
    criteria <- mongo.bson.from.buffer(buf)
    
    mongo.update(mongo, ns, criteria, system_desc_bson())

  }else{
    mongo.insert.batch(mongo, ns, list(system_desc_bson()))
  }
  
  
  
  
  # Disconnect from db
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  })
})