## Get list of systems already in the database ###########################
systems_in_db <- reactive({
  # Make sure it updates on submission
  input$submit_system
  
  data_back <- get_systems()
  return(data_back)
})




## Make UI elements ###########################
## Drop down menu
output$system_name_select <- renderUI({

  sys_to_show = as.character(unlist(lapply(systems_in_db(),function(x) x$system_name)))    
  
  if(input$only_own_systems){
  sys_userid = as.integer(unlist(lapply(systems_in_db(),function(x) x$userID)))
  sys_to_show =   sys_to_show[       sys_userid == userID()       ]
  }
  
  selectInput(inputId = 'system_name_select',label= 'Select existing system',choices=c("",  sys_to_show      ),selected="",selectize=TRUE)
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


## submit button
output$submit_system <- renderUI({
  sys_name = as.character(unlist(lapply(systems_in_db(),function(x) x$system_name)))
 
  if(any(sys_name==input$system_name)){buttontext = "Update system"}  
  if(!any(sys_name==input$system_name)){buttontext = "Add system"}  
  
  
  return( actionButton("submit_system",buttontext)  )
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
  if(length(input$submit_system) == 0) return(NULL)
  if(input$submit_system == 0) return(NULL)
  
  
  isolate({
    # make db connection
    mongo <- mongo.create()

    sys_name = as.character(unlist(lapply(systems_in_db(),function(x) x$system_name)))
    
    if(any(sys_name==input$system_name)){ # the system is already in the db
      idx = input$system_name==sys_name
      
#           
#       validate({
#       need(   systems_in_db()[[which(idx)]]$userID == userID()   ,message="You can only update systems that you added yourself")
#       })
#            
      
      buf <- mongo.bson.buffer.create()
      mongo.bson.buffer.append(buf, "_id", systems_in_db()[[which(idx)]]$`_id`)
      criteria <- mongo.bson.from.buffer(buf)
      
      mongo.update(mongo, ns_chrom_systems, criteria, system_desc_bson())
      
    }else{
      mongo.insert.batch(mongo, ns_chrom_systems, list(system_desc_bson()))
    }
    
    
    # Disconnect from db
    del <- mongo.disconnect(mongo)
    del <- mongo.destroy(mongo)
  })
})