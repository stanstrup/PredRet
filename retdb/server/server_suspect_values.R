## Get all user data from database ##############################
suspect_users_data <-  reactive({ 
  data_has_been_written$done # Update if data written to db
  data_was_deleted$done # database deletions are done
  
  data <- get_user_data(ns=ns_rtdata,ns_sysmodels=ns_sysmodels,userID=userID(),generation=0L,suspect=TRUE) 
  
  
  
  
  ## Get matching predicted values if possible
  predicted_rt <- rep(as.numeric(NA),length=nrow(data))
  
  mongo <- PredRet_connect()
    
  for(i in 1:length(predicted_rt)){
    temp <- mongo.find.all(
                                      mongo=mongo, 
                                      ns=ns_rtdata,
                                      query=list(  generation=1L,inchi=data[i,"inchi"]  ,sys_id=data[i,"sys_id"] ),
                                      fields = list(predicted_rt=1L,`_id`=0L),
                                      data.frame=T,
                                      mongo.oid2character=T
                                    )
    if(!is.null(temp))   predicted_rt[i] <- as.numeric(temp)
    
  }
  
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo) 
  
  
  data <- cbind(data,predicted_rt)
  
  
  
  if(is.null(data)){
  data = matrix(,nrow=0,ncol=6)
  colnames(data) = c("name","recorded_rt","system","date added","pubchem","inchi","predicted_rt")
  data = as.data.frame(data)
  }
  
  # Change to local timezone
  data[,"date added"] <- as.POSIXct(as.numeric(data[,"date added"]), origin = "1970-01-01", tz = "GMT") - time_zone_offset()
  
  return(data)
})




## Make settings for table and display it ##############################

## Make settings
manage_suspect_table_settings <- reactive({
  colwidths <- c("485px", "118px","118px", "150px", "150px", "119px","NA")
  col.names <- c("Name","Recorded RT","Predicted RT","System","Date added","Pubchem","InChI")
  aoColumnDefs <- list(NULL)
  for(i in 1:length(col.names)){
    column <- list(sWidth=colwidths[i], sTitle=col.names[i], aTargets=list(i-1))
    aoColumnDefs[[i]] <- column
  }
  
  return(aoColumnDefs)
})





## Display the table
output$SUSPECT_data <- renderDataTable({
  # if(exists("data_was_deleted")) data_was_deleted()
  
  
  data_to_show <- suspect_users_data()
  data_to_show <- data_to_show[,                c("name","recorded_rt","predicted_rt","system","date added","pubchem","inchi")         ]
  data_to_show[,"recorded_rt"]      <-     round(data_to_show[,"recorded_rt"],digits=2)
  data_to_show[,"predicted_rt"]      <-     round(data_to_show[,"predicted_rt"],digits=2)
  select_rows <- rep(TRUE,nrow(data_to_show))
  
  
  
    
  if(!(nrow(data_to_show)==0)){ # only do something if there is actually data returned from the database.

    # Make sure Inchi is not too long
    data_to_show[,"inchi"] = 
      paste0('<div style= "-o-text-overflow: ellipsis; text-overflow: ellipsis;  overflow:hidden;  white-space:nowrap;   width: 500px;">'
             ,data_to_show[,"inchi"],'</div>')
    
    

    data_to_show[,c("name","recorded_rt","predicted_rt","system","date added","pubchem","inchi")]
  }
}


,options=list(pageLength = 15,aoColumnDefs=manage_suspect_table_settings(), columns=NULL,AutoWidth=FALSE    )
,escape=FALSE
)








## Download button  ##############################
output$download_suspect_Data <- downloadHandler(
  filename = function() {
    paste('data-', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    write.csv(suspect_users_data()[,c("name","recorded_rt","predicted_rt","system","date added","pubchem","inchi")], con)
  }
)
