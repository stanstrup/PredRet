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
  
  data <- get_user_data(userID=userID(),generation=0) 
  rownames(data) <- data[,"_id"]
  
  if(is.null(data)){
  data = matrix(,nrow=0,ncol=6)
  colnames(data) = c("name","recorded_rt","system","date added","pubchem","inchi")
  data = as.data.frame(data)
  }
  
  # Change to local timezone
  data[,"date added"] <- as.POSIXct(as.numeric(data[,"date added"]), origin = "1970-01-01", tz = "GMT") - time_zone_offset()
  
  return(data)
})


## Make settings for table ##############################
manage_table_settings <- reactive({
  colwidths <- c("0px","600px", "60px", "150px", "150px", "100px","NA")
  col.names <- c("rownames","Name","RT","System","Date added","Pubchem","InChI")
  aoColumnDefs <- list(NULL)
  for(i in 1:length(col.names)){
      aoColumnDefs[[i]] <- list(sWidth=colwidths[i], sTitle=col.names[i], visible=if(i==1){FALSE}else{TRUE},aTargets=list(i-1))
  }
  
  return(aoColumnDefs)
})


# Get masses.  ##############################
masses <- reactive({
  masses <- sapply(str_split_fixed(users_data()[,"inchi"],"/",3)[,2], function(x) getMass(getMolecule(x))   )
  return(masses)
  })



## Filter user data ##############################
users_data_filtered <-  reactive({ 
  
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


return(data_to_show)
})



## Select rows  ##############################
users_data_select_rows <-  reactive({ 
  if(input$MANAGE_filter_by=="")          return(  list(mode = 'multiple')  ) 
  if(is.null(input$MANAGE_filter_select)) return(  list(mode = 'multiple')  )
  if(input$MANAGE_filter_select=="")      return(  list(mode = 'multiple')  )
  
  data_to_show <- users_data_filtered()
  out <-    as.character(data_to_show[,input$MANAGE_filter_by]) == as.character(input$MANAGE_filter_select)
  out <-    rownames(data_to_show)[out]
  out <-    list(mode = 'multiple', selected = out) 
  return(out)
})




## Display the table  ##############################
output$MANAGE_data <- renderDataTable({
  
  data_to_show <- users_data_filtered()
  if(nrow(data_to_show)==0) return(NULL) # only do something if there is actually data returned from the database.
  
  
  # Make sure Inchi is not too long
  data_to_show[,"inchi"] = 
    paste0('<div style= "-o-text-overflow: ellipsis; text-overflow: ellipsis;  overflow:hidden;  white-space:nowrap;   width: 500px;">'
           ,data_to_show[,"inchi"],'</div>')
  
  
  # Object to return
  data_to_show <- data_to_show[,c("name","recorded_rt","system","date added","pubchem","inchi")]
  
  datatable(data_to_show,
            options=list(pageLength = 15,aoColumnDefs=manage_table_settings(), columns=NULL,AutoWidth=FALSE    ),
            escape=FALSE,
            filter="top",
            selection = users_data_select_rows()
            )
  
  
})



## Show selected rows  ##############################

# output$MANAGE_data_show_selected = renderPrint({
#   s = input$MANAGE_data_rows_selected
#   
#     cat('These rows were selected:\n\n')
#     cat(s, sep = '\n')
#   
#   cat("str", sep = '\n')
#     cat(str(s), sep = '\n')
#   
# 
#   cat("raw str", sep = '\n')
#   cat(str(users_data_select_rows()), sep = '\n')
#   
#   cat("filter by", sep = '\n')
#   cat(str(input$MANAGE_filter_by), sep = '\n')
#   
#   cat("filter select", sep = '\n')
#   cat(str(input$MANAGE_filter_select), sep = '\n')
#     
#   cat("button", sep = '\n')
#   cat(str(input$del_data), sep = '\n')
#   
#   
# })



## Delete selected rows when actionButton is clicked  ##############################
data_was_deleted <- reactiveValues()

observe({
  if(input$del_data == 0) return(NULL)
  
  isolate({
    s <- input$MANAGE_data_rows_selected
    if(length(s)==0) return()
    if(is.null(s)) return()
    
    mongo_del_oid(ns=PredRet.env$namespaces$ns_rtdata,     oids = s     )
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
