
## Select boxes ################# 

systems <- reactive({get_systems()})


output$build_sys1 <- renderUI({
  return(selectInput(inputId = 'build_sys1',label= 'Select first system',choices=c("",as.character(lapply(systems(),function(x) x$system_name))),selected="",selectize=TRUE))
})


output$build_sys2 <- renderUI({
  return(selectInput(inputId = 'build_sys2',label= 'Select second system',     choices=c("", as.character(lapply(systems(),function(x) x$system_name))  ),     selected="",selectize=TRUE))
})






## Info table ################# 
build_table_settings <- reactive({
  colwidths <- c("NA","NA", "NA", "100px", "150px","150px")
  #col.names <- c("Select","Name","RT","System","Date added","Pubchem","InChI")
  aoColumnDefs <- list(NULL)
  for(i in 1:length(colwidths)){
    #column <- list(sWidth=colwidths[i], sTitle=col.names[i], aTargets=list(i-1))
    column <- list(sWidth=colwidths[i], aTargets=list(i-1))
    aoColumnDefs[[i]] <- column
  }
  
  return(aoColumnDefs)
})




output$build_table <- renderDataTable({
  
  # Make a table with all relevant data
  models_table <- lapply(models(),function(x) data.frame(x$oid_sys1,x$oid_sys2,x$n_points,x$status,x$time,x$newest_entry,stringsAsFactors=F))
  
  models_table <- do.call(rbind,models_table)
  
  colnames(models_table)=c("Prediction from",
                           "Prediction to",
                           "# Common compounds",
                           "Build status",
                           "Latest build time",
                           "Newest data point"
  )
  
  
  # Change to local timezone
  models_table[,"Latest build time"] <- as.POSIXct(as.numeric(models_table[,"Latest build time"]), origin = "1970-01-01", tz = "GMT") - time_zone_offset()
  models_table[,"Newest data point"] <- as.POSIXct(as.numeric(models_table[,"Newest data point"]), origin = "1970-01-01", tz = "GMT") - time_zone_offset()
  
  
  # Change sysid to sysname
  sys_names = sys_oid2name(as.character(as.matrix(models_table[,c("Prediction from","Prediction to")])))
  dim(sys_names)=c(length(sys_names)/2,2)
  models_table[,c("Prediction from","Prediction to")] <- sys_names
  
  
  
  
  models_table
  
},options=list(iDisplayLength = 15,aoColumnDefs=build_table_settings(), aoColumns=NULL,bAutoWidth=FALSE    )
)








## Rebuild selected model ################# 
observe({
  if(input$build_specific==0) return(NULL)  
  
  isolate({
    if(is.null(input$build_sys1)) return(NULL)
    if(is.null(input$build_sys2)) return(NULL)
    if(input$build_sys1=="") return(NULL)
    if(input$build_sys2=="") return(NULL)  
    if(input$build_sys1==input$build_sys2) return(NULL)  # Added some error message here
    
    
    # get oids for selected systems
    sys_names = as.character(lapply(systems(),function(x) x$system_name))
    sys_oids  = as.character(lapply(systems(),function(x) x$`_id`))
        
    oid1 = sys_oids[input$build_sys1==sys_names]
    oid2 = sys_oids[input$build_sys2==sys_names]
    
    
    build_model(oid1=oid1,oid2=oid2,ns_sysmodels=ns_sysmodels,ns_rtdata=ns_rtdata,ns_sysmodels_log=ns_sysmodels_log,force=input$build_force_recalc,session=session) 

  })
})







## Rebuild ALL model ################# 
observe({
  if(input$build_all==0) return(NULL)  
  
  isolate({
    
    # get oids for selected systems
    sys_oids  = as.character(lapply(systems(),function(x) x$`_id`))
    
    system_combs = t(combn(sys_oids,2))
    system_combs = as.matrix(system_combs)
    system_combs = rbind(system_combs,system_combs[,c(2,1)])
    
    for(i in 1:nrow(system_combs)){
    build_model(oid1=system_combs[i,1],oid2=system_combs[i,2],ns_sysmodels=ns_sysmodels,ns_rtdata=ns_rtdata,ns_sysmodels_log=ns_sysmodels_log,force=input$build_force_recalc_all,session=session) 
    }
    
    
    
    
  })
})



## Purge predictions ################# 
observe({
  if(input$purge_predictions==0) return(NULL)  
  
  isolate({
    mongo <- mongo.create()
    
    mongo.remove(mongo, ns=ns_rtdata, criteria = list(generation = 1))
    
    del <- mongo.disconnect(mongo)
    del <- mongo.destroy(mongo) 
  })
})







## Log table ################# 
build_log_settings <- reactive({
  colwidths <- c("150px","150px", "150px", "NA")
  #col.names <- c("Select","Name","RT","System","Date added","Pubchem","InChI")
  aoColumnDefs <- list(NULL)
  for(i in 1:length(colwidths)){
    #column <- list(sWidth=colwidths[i], sTitle=col.names[i], aTargets=list(i-1))
    column <- list(sWidth=colwidths[i], aTargets=list(i-1))
    aoColumnDefs[[i]] <- column
  }
  
  return(aoColumnDefs)
})


build_log <- reactivePoll(10*1000,session=session,function() log_count(ns=ns_sysmodels_log),function(x) get_build_log(ns_sysmodels_log,time_offset = time_zone_offset()))

output$build_log <- renderDataTable(build_log(),
                                    options=list(iDisplayLength = 10,aoColumnDefs=build_log_settings(), aoColumns=NULL,bAutoWidth=FALSE    )
                                    )


  

