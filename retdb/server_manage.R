## Upper options ##############################
output$MANAGE_filter_select <- renderUI({

  if(!(input$MANAGE_filter_by=="")){
  selectInput(inputId = 'MANAGE_filter_select',label= 'Select',choices=c("",as.character(unique(users_data()[[input$MANAGE_filter_by]]))),selected="",selectize=TRUE)
  }
  
})
  


## Get all user data from database ##############################
users_data <-  reactive({ 
  data_has_been_written$done # Update if data written to db
  data_was_deleted$done # database deletions are done
  
  data <- get_user_data(ns=ns_rtdata,userID=userID(),generation=0) 
  #data <- data[,                c("name","recorded_rt","system","date added","pubchem","inchi")         ]
  
  if(is.null(data)){
  data = matrix(,nrow=0,ncol=6)
  colnames(data) = c("name","recorded_rt","system","date added","pubchem","inchi")
  data = as.data.frame(data)
  }
  
  # Change to local timezone
  data[,"date added"] <- as.POSIXct(as.numeric(data[,"date added"]), origin = "1970-01-01", tz = "GMT") - time_zone_offset()
  
  return(data)
})




## Make settings for table and display it ##############################

## Make settings
manage_table_settings <- reactive({
  colwidths <- c("60px","600px", "60px", "150px", "150px", "100px","NA")
  col.names <- c("Select","Name","RT","System","Date added","Pubchem","InChI")
  aoColumnDefs <- list(NULL)
  for(i in 1:length(col.names)){
    column <- list(sWidth=colwidths[i], sTitle=col.names[i], aTargets=list(i-1))
    aoColumnDefs[[i]] <- column
  }
  
  return(aoColumnDefs)
})


## Display the table
output$MANAGE_data <- renderDataTable({
  # if(exists("data_was_deleted")) data_was_deleted()
  
  data_to_show <- users_data()
  data_to_show <- data_to_show[,                c("name","recorded_rt","system","date added","pubchem","inchi")         ]
  
  data_to_show[,"recorded_rt"]      <-     round(data_to_show[,"recorded_rt"],digits=2)
  
  
  
  if(!(nrow(data_to_show)==0)){ # only do something if there is actually data returned from the database.
    # Select rows
    checked=rep('',nrow(users_data()))
    if(!(input$MANAGE_filter_by=="") & !is.null(input$MANAGE_filter_select)){
      if(!(input$MANAGE_filter_select=="")){
        checked[        as.character(data_to_show[[input$MANAGE_filter_by]]) == as.character(input$MANAGE_filter_select)    ]   ='checked="checked"'
      }
    }
    
    # Make sure Inchi is not too long
    data_to_show[,"inchi"] = 
      paste0('<div style= "-o-text-overflow: ellipsis; text-overflow: ellipsis;  overflow:hidden;  white-space:nowrap;   width: 500px;">'
             ,data_to_show[,"inchi"],'</div>')
    
    
    # Add checkbox column
    addCheckBoxes <- paste0('<input type="checkbox" ',' id=' ,'row',1:nrow(data_to_show),' ',checked,' name="row" value="', 1:nrow(users_data()), '">')
    cbind.data.frame(Select=addCheckBoxes,data_to_show[,c("name","recorded_rt","system","date added","pubchem","inchi")]           ,stringsAsFactors = F)
  }
}


,options=list(iDisplayLength = 15,aoColumnDefs=manage_table_settings(), aoColumns=NULL,bAutoWidth=FALSE    )
)



## Show selected rows in terminal
observe({
  print(as.numeric(input$row))
}) 








## Delete selected rows when actionButton is clicked  ##############################
data_was_deleted <- reactiveValues()

observe({
  if(input$del_data == 0) return(NULL)
  
  isolate({
    if(length(input$row)==0) return()
    
    to_del = as.numeric(input$row)
    mongo_del_oid(ns=ns_rtdata,     oids = users_data()[to_del,"_id"]     )
    data_was_deleted$done <- input$del_data
  })
}) 



            







## Download button  ##############################
output$downloadData <- downloadHandler(
  filename = function() {
    paste('data-', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    write.csv(users_data()[,c("name","recorded_rt","system","date added","pubchem","inchi")], con)
  }
)
