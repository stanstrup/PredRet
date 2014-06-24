## Upper options ##############################
output$MANAGE_filter_select <- renderUI({
  if(input$MANAGE_filter_by=="system"){
                                          selectInput(inputId = 'MANAGE_filter_select',label= 'Show only',choices=c("",unique(users_data()$system)),selected="",selectize=TRUE)
  }
})
  






## Get all user data from database ##############################

users_data =  reactive({ 
# Update if file is uploaded
  input$files
  
  ## Only get users own data
  
  
  mongo <- mongo.create()
  ns <- "test2.rtdata"
  
  data_all = mongo.find.all2(mongo=mongo, ns=ns,query=mongo.bson.empty(),data.frame=T,mongo.oid2character=T)
  row.names(data_all) <- seq(nrow(data_all))
  
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
  
  # Take some data directly
  data = data_all[,c("name","rt","pubchem","inchi")]
  
  
  # Get correctly formatted time
  data = cbind.data.frame(data , `data added` = as.POSIXct(data_all[,"time"],origin="1970-01-01")     ,stringsAsFactors = F)
  
  
  # Get system name from system ID
  sys_id_data = as.character(data_all[,"sys_id"])
  sys_id_db = unlist(lapply(systems_in_db(),function(x) as.character.mongo.oid(x$`_id`))  )
  sys_name = as.character(unlist(lapply(systems_in_db(),function(x) x$system_name)))  
  
  data = cbind.data.frame(data , system = sys_name[match(sys_id_data,sys_id_db)]          ,stringsAsFactors = F)
  
  
  # Format RT data
  data[,"rt"]      =     round(data[,"rt"],digits=2)
  
  
  # sort columns
  data = data[,  c("name","rt","system","data added","pubchem","inchi")    ]
  #data[,  "system"  ] = as.character(data[,  "system"  ])
  
  # Add checkboxcolumn
  addCheckBoxes <- paste0('<input type="checkbox" checked="','yes','" name="row" value="', 1:nrow(data), '">')
  data = cbind.data.frame(Pick=addCheckBoxes,data           ,stringsAsFactors = F)
  
  return(data)
  
})


## Make settings for table and display it ##############################

## Make settings
manage_table_settings = reactive({
  colwidths <- c("30px","600px", "60px", "150px", "150px", "100px","NA")
  col.names <- names(users_data())
  aoColumnDefs <- list(NULL)
  for(i in 1:ncol(users_data())){
    column <- list(sWidth=colwidths[i], sTitle=col.names[i], aTargets=list(i-1))
    aoColumnDefs[[i]] <- column
  }
  
  return(aoColumnDefs)
})


## Display the table
output$MANAGE_data <- renderDataTable(users_data(),options=list(iDisplayLength = 15,aoColumnDefs=manage_table_settings(), aoColumns=NULL,bAutoWidth=FALSE    )
)




## Show selected in terminal
observe({
  print(as.numeric(input$row))
}) 
                                              