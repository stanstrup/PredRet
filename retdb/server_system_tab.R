## Get list of systems already in the database ###########################
systems_in_db <- reactive({
  # Make sure it updates on submission
  input$submit_system
  
  data_back <- get_systems()
  return(data_back)
})




## Make UI elements ############################
# Drop down menu
output$system_name_select <- renderUI({

  sys_to_show = as.character(unlist(lapply(systems_in_db(),function(x) x$system_name)))    
  
  if(input$only_own_systems){
  sys_userid = as.integer(unlist(lapply(systems_in_db(),function(x) x$userID)))
  sys_to_show =   sys_to_show[       sys_userid == userID()       ]
  }
  

  selectInput(inputId = 'system_name_select',label= strong('Select existing system (update or use as template)'),choices=c("",  sys_to_show      ),selected="",selectize=TRUE)
})



# System name text box 
output$system_name <- renderUI({
  textInput('system_name', label='',value = input$system_name_select)
})







# System column type
output$SYSTEM_column_type_select <- renderUI({
  if (is.null(input$system_name_select))    return(NULL)
  
  
  sys_column_type = as.character(unlist(lapply(systems_in_db(),function(x) x$system_column_type)))  
  
  # if a system is selected fetch the description
  if(!(input$system_name_select=="")){
    sys_name = as.character(unlist(lapply(systems_in_db(),function(x) x$system_name)))  
    idx = input$system_name_select==sys_name
    column_type_shown = sys_column_type[idx]
    
  }else{
    column_type_shown = ""
  }
  
  selectInput(inputId = 'SYSTEM_column_type_select',label = '', choices = c("",unique(sys_column_type)),selected=column_type_shown,width="100%")
})




output$SYSTEM_column_type_name <- renderUI({
  textInput(inputId   = 'SYSTEM_column_type_name', label='',value = input$SYSTEM_column_type_select)
})







# System column
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
  
  selectInput(inputId = 'SYSTEM_column_select',label = '', choices = c("",unique(sys_column)),selected=column_shown,width="100%")
})




output$SYSTEM_column_name <- renderUI({
  textInput(inputId   = 'SYSTEM_column_name', label='',value = input$SYSTEM_column_select)
})








# System eluents
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
  
  selectInput(inputId = 'SYSTEM_eluent_select',label = '', choices = c("",unique(sys_eluent)),selected=eluent_shown,width="100%")
})




output$SYSTEM_eluent_name <- renderUI({
  textInput(inputId   = 'SYSTEM_eluent_name', label='',value = input$SYSTEM_eluent_select)
})







# System eluent pH
output$SYSTEM_eluent_pH_select <- renderUI({
  if (is.null(input$system_name_select))    return(NULL)
  
  
  sys_eluent_pH = as.character(unlist(lapply(systems_in_db(),function(x) x$system_eluent_pH)))  
  
  # if a system is selected fetch the description
  if(!(input$system_name_select=="")){
    sys_name = as.character(unlist(lapply(systems_in_db(),function(x) x$system_name)))  
    idx = input$system_name_select==sys_name
    eluent_pH_shown = sys_eluent_pH[idx]
    
  }else{
    eluent_pH_shown = ""
  }
  
  selectInput(inputId = 'SYSTEM_eluent_pH_select',label = '', choices = c("",unique(sys_eluent_pH)),selected=eluent_pH_shown,width="100%")
})




output$SYSTEM_eluent_pH_name <- renderUI({
  textInput(inputId   = 'SYSTEM_eluent_pH_name', label='',value = input$SYSTEM_eluent_pH_select)
})





# System eluent additive
output$SYSTEM_eluent_additive_select <- renderUI({
  if (is.null(input$system_name_select))    return(NULL)
  
  
  sys_eluent_additive = as.character(unlist(lapply(systems_in_db(),function(x) x$system_eluent_additive)))  
  
  # if a system is selected fetch the description
  if(!(input$system_name_select=="")){
    sys_name = as.character(unlist(lapply(systems_in_db(),function(x) x$system_name)))  
    idx = input$system_name_select==sys_name
    eluent_additive_shown = sys_eluent_additive[idx]
    
  }else{
    eluent_additive_shown = ""
  }
  
  selectInput(inputId = 'SYSTEM_eluent_additive_select',label = '', choices = c("",unique(sys_eluent_additive)),selected=eluent_additive_shown,width="100%")
})




output$SYSTEM_eluent_additive_name <- renderUI({
  textInput(inputId   = 'SYSTEM_eluent_additive_name', label='',value = input$SYSTEM_eluent_additive_select)
})



# System reference (link or doi)
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







# Text area with system desc
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


# submit button
output$submit_system <- renderUI({
  if(is.null(input$system_name))                 return(NULL)
  if(is.null(input$SYSTEM_column_type_name))     return(NULL)
  if(is.null(input$SYSTEM_column_name))          return(NULL)
  if(is.null(input$SYSTEM_eluent_name))          return(NULL)
  if(is.null(input$SYSTEM_eluent_pH_name))       return(NULL)
  if(is.null(input$SYSTEM_eluent_additive_name)) return(NULL)
  
  if(input$system_name=="")                      return(NULL)
  if(input$SYSTEM_column_type_name=="")          return(NULL)
  if(input$SYSTEM_column_name=="")               return(NULL)
  if(input$SYSTEM_eluent_name=="")               return(NULL)
  if(input$SYSTEM_eluent_pH_name=="")            return(NULL)
  if(input$SYSTEM_eluent_additive_name=="")      return(NULL)
  
  

  
  sys_name  <- as.character(sapply(systems_in_db(),function(x) x$system_name))
  sys_owner <- as.character(sapply(systems_in_db(),function(x) x$userID))
  
  if(!any(sys_name==input$system_name)){buttontext = "Add system"}  
  
  if( any(sys_name==input$system_name)){
    if(any(sys_name==input$system_name  &    sys_owner==userID())){
      buttontext = "Update system"  
    }else{
      return(NULL) 
    }
  }  
  
  
  
  
  
  
  return( actionButton("submit_system",buttontext)  )
})






# submit button warnings
output$submit_system_warnings <- renderUI({
  
  if(is.null(input$system_name)) return(NULL)
  if(is.null(input$SYSTEM_eluent_name)) return(NULL)
  if(is.null(input$SYSTEM_eluent_pH_name)) return(NULL)
  if(is.null(input$SYSTEM_eluent_additive_name)) return(NULL)
  if(is.null(input$SYSTEM_column_name)) return(NULL)
  if(is.null(input$SYSTEM_column_type_name)) return(NULL)
  

  
  
  warning_text = vector(length = 7,mode="list")
  
  # check if the system name has been used by someone else
  sys_name  <- as.character(sapply(systems_in_db(),function(x) x$system_name))
  sys_owner <- as.character(sapply(systems_in_db(),function(x) x$userID))
  
  if( any(sys_name==input$system_name)){
    if(any(sys_name==input$system_name  &    sys_owner==userID())){
      other_users_system <- TRUE
    }else{
      warning_text[[1]] <- "A system already exists with the specified name. However, since it was added by someone else you cannot edit this system. Please choose another name"
    }
  }  
  
  
  # check if all required fields were filled
  if(input$system_name=="")                   warning_text[[2]] <- "You must specify a system name"
  if(input$SYSTEM_column_type_name=="")       warning_text[[3]] <- "You must specify a column type"
  if(input$SYSTEM_column_name=="")            warning_text[[4]] <- "You must specify a column"
  if(input$SYSTEM_eluent_name=="")            warning_text[[5]] <- "You must specify eluents"
  if(input$SYSTEM_eluent_pH_name=="")         warning_text[[6]] <- "You must specify eluent pH"
  if(input$SYSTEM_eluent_additive_name=="")   warning_text[[7]] <- "You must specify eluent additives"
  
  
    
  # If no warnings then don't output anything  
  if(all(sapply(warning_text,is.null))) return(NULL)
  
  
  
  # make warnings into HTML
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
    
    buf=list()
    buf[["time"]] <- Sys.time()
    buf[["userID"]] <- as.integer(userID())
    buf[["username"]] <- username()
    buf[["system_name"]] <- input$system_name
    buf[["system_desc"]] <- input$system_desc
    buf[["system_eluent"]] <- input$SYSTEM_eluent_name
    buf[["system_eluent_pH"]] <- input$SYSTEM_eluent_pH_name
    buf[["system_eluent_additive"]] <- input$SYSTEM_eluent_additive_name    
    buf[["system_column"]] <- input$SYSTEM_column_name
    buf[["system_column_type"]] <- input$SYSTEM_column_type_name
    buf[["system_ref"]] <- input$SYSTEM_ref
    
    return(buf)
    
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
      idx = (input$system_name==sys_name)
      
      buf <- mongo.bson.buffer.create()
      mongo.bson.buffer.append(buf, "_id", systems_in_db()[[which(idx)]]$`_id`)
      criteria <- mongo.bson.from.buffer(buf)
      
      mongo.update(mongo, ns_chrom_systems, criteria, system_desc_bson()   )
    }else{
      mongo.insert(mongo, ns_chrom_systems, system_desc_bson()    )
    }
    
    
    
    
    
    # Disconnect from db
    del <- mongo.disconnect(mongo)
    del <- mongo.destroy(mongo)
  })
})