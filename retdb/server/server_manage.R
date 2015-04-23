## Upper options ##############################
output$MANAGE_filter_select <- renderUI({

  if(!(input$MANAGE_filter_by=="")){
  selectInput(inputId = 'MANAGE_filter_select',label= strong('Select'),choices=c("",as.character(unique(users_data()[[input$MANAGE_filter_by]]))),selected="",selectize=TRUE)
  }
  
})
  


## Get all user data from database ##############################
users_data <-  reactive({ 
  data_has_been_written$done # Update if data written to db
  data_was_deleted$done # database deletions are done
  
  data <- get_user_data(ns=ns_rtdata,ns_chrom_systems = ns_chrom_systems,userID=userID(),generation=0) 
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



# Get masses.
masses <- reactive({
  masses <- sapply(str_split_fixed(users_data()[,"inchi"],"/",3)[,2], function(x) getMass(getMolecule(x))   )
  return(masses)
  })



## Display the table
output$MANAGE_data <- renderDataTable({
  # if(exists("data_was_deleted")) data_was_deleted()
  
  
  data_to_show <- users_data()
  data_to_show <- data_to_show[,                c("name","recorded_rt","system","date added","pubchem","inchi")         ]
  data_to_show[,"recorded_rt"]      <-     round(data_to_show[,"recorded_rt"],digits=2)
  select_rows <- rep(TRUE,nrow(data_to_show))
  
  
  
  # Limit mass range
  if(!is.na(input$MANAGE_massrange_min) | !is.na(input$MANAGE_massrange_max)){ # so that we only calculate the mass if we need it
    in_mass_range <- is.between(masses(),input$MANAGE_massrange_min,input$MANAGE_massrange_max)
    select_rows[!in_mass_range] <- FALSE
  }
  
  
  # Limit based on exact mass
  if(!is.na(input$MANAGE_exactmass) & !is.na(input$MANAGE_ppm)){
  exact_mass_match <- (abs(masses()-input$MANAGE_exactmass)/input$MANAGE_exactmass)*1e6 <  input$MANAGE_ppm
  select_rows[!exact_mass_match] <- FALSE
  }
  
  
  # Limit RT range
  in_RT_range   <-  is.between(data_to_show[,"recorded_rt"],input$MANAGE_rtrange_min,input$MANAGE_rtrange_max)
  select_rows[!in_RT_range] <- FALSE
  
  # limit based on above criteria
  data_to_show <-     data_to_show[    select_rows       ,,drop=FALSE]
  
  
  
  if(!(nrow(data_to_show)==0)){ # only do something if there is actually data returned from the database.
    # Select rows
    checked=rep('',nrow(data_to_show))
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
    addCheckBoxes <- paste0('<input type="checkbox" ',' id=' ,'row',1:nrow(data_to_show),' ',checked,' name="row" value="', 1:nrow(data_to_show), '">')
    cbind.data.frame(Select=addCheckBoxes,data_to_show[,c("name","recorded_rt","system","date added","pubchem","inchi")]           ,stringsAsFactors = F)
  }
}


,options=list(pageLength = 15,aoColumnDefs=manage_table_settings(), columns=NULL,AutoWidth=FALSE    )
,escape=FALSE
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
