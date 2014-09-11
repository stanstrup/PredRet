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
  textInput('system_name', label=strong('System name'),value = input$system_name_select)
})



## System eluents + modifiers
output$SYSTEM_eluent_select <- renderUI({
  if (is.null(input$system_name_select))    return(NULL)
  
  
  sys_eluent = as.character(unlist(lapply(systems_in_db(),function(x) x$system_eluent)))  
  
  # if a system is selected fetch the description
  if(!(input$system_name_select=="")){
    sys_name = as.character(unlist(lapply(systems_in_db(),function(x) x$system_name)))  
    idx = input$system_name_select==sys_name
    eluent_shown = sys_eluent[idx]
    
  }else{
    eluent_shown = ""
  }
  
  selectInput(inputId = 'SYSTEM_eluent_select',label = 'Suggested descriptions', choices = c("",unique(sys_eluent)),selected=eluent_shown,width="100%")
})




output$SYSTEM_eluent_name <- renderUI({
    textInput(inputId   = 'SYSTEM_eluent_name', label='New eluents and modifier description',value = input$SYSTEM_eluent_select)
})







## System column
output$SYSTEM_column_select <- renderUI({
  if (is.null(input$system_name_select))    return(NULL)
  
  
  sys_column = as.character(unlist(lapply(systems_in_db(),function(x) x$system_column)))  
  
  # if a system is selected fetch the description
  if(!(input$system_name_select=="")){
    sys_name = as.character(unlist(lapply(systems_in_db(),function(x) x$system_name)))  
    idx = input$system_name_select==sys_name
    column_shown = sys_column[idx]
    
  }else{
    column_shown = ""
  }
  
  selectInput(inputId = 'SYSTEM_column_select',label = 'Suggested columns', choices = c("",unique(sys_column)),selected=column_shown,width="100%")
})




output$SYSTEM_column_name <- renderUI({
  textInput(inputId   = 'SYSTEM_column_name', label='New column',value = input$SYSTEM_column_select)
})








## System reference (link or doi)
output$SYSTEM_ref <- renderUI({
  if (is.null(input$system_name_select))    return(NULL)
  
  
  sys_ref = as.character(unlist(lapply(systems_in_db(),function(x) x$system_ref)))  
  
  # if a system is selected fetch the description
  if(!(input$system_name_select=="")){
    sys_name = as.character(unlist(lapply(systems_in_db(),function(x) x$system_name)))  
    idx = input$system_name_select==sys_name
    ref_shown = sys_ref[idx]
    
  }else{
    ref_shown = "doi or link"
  }
  
  textInput(inputId   = 'SYSTEM_ref', label=strong('Reference'),value = ref_shown)  
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
  
  inputTextarea('system_desc',strong('System description'), desc_shown ,"300px","100%")
})


## submit button
output$submit_system <- renderUI({
  if(is.null(input$system_name)) return(NULL)
  if(is.null(input$SYSTEM_eluent_name)) return(NULL)
  if(is.null(input$SYSTEM_column_name)) return(NULL)
  if(input$system_name=="") return(NULL)
  if(input$SYSTEM_eluent_name=="") return(NULL)
  if(input$SYSTEM_column_name=="") return(NULL)
  
  

  
  sys_name = as.character(unlist(lapply(systems_in_db(),function(x) x$system_name)))
 
  if(any(sys_name==input$system_name)){buttontext = "Update system"}  
  if(!any(sys_name==input$system_name)){buttontext = "Add system"}  
  
  
  return( actionButton("submit_system",buttontext)  )
})




##

## submit button
output$submit_system_warnings <- renderUI({
  
  if(is.null(input$system_name)) return(NULL)
  if(is.null(input$SYSTEM_eluent_name)) return(NULL)
  if(is.null(input$SYSTEM_column_name)) return(NULL)
  
  if(!(input$system_name=="") & !(input$SYSTEM_eluent_name=="")   &   !(input$SYSTEM_column_name=="")) return(NULL)
  
  warning_text = vector(length = 3,mode="list")
  
  if(input$system_name=="")         warning_text[[1]] <- "You must specify a system name"
  if(input$SYSTEM_eluent_name=="")  warning_text[[2]] <- "You must specify eluents"
  if(input$SYSTEM_column_name=="")  warning_text[[3]] <- "You must specify a column"
  
  warning_text <-   warning_text[    !sapply(warning_text,is.null)     ]
  warning_text <- paste(warning_text,collapse="<br />")
  
  
  div_style <- "color: #b94a48;background-color: #f2dede;border-color: #eed3d7;padding: 8px 35px 8px 14px;margin-bottom: 20px;text-shadow: 0 1px 0 rgba(255,255,255,0.5);border: 1px solid #fbeed5;border-radius: 4px;"
  p_style   <- "margin-bottom:0px"
  return(div(p(HTML(warning_text),style=p_style),style=div_style))
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
    mongo.bson.buffer.append(buf, "system_eluent", input$SYSTEM_eluent_name)
    mongo.bson.buffer.append(buf, "system_column", input$SYSTEM_column_name)
    mongo.bson.buffer.append(buf, "system_ref", input$SYSTEM_ref)
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