DATA_data <- reactive({
  if(is.null(input$DATA_select_system)) return(NULL)
  if(input$DATA_select_system == "") return(NULL)
  
  
  if(length(input$DATA_select_system)==1){
    
  criteria <- list(generation  =    0L, 
                   sys_id      =    input$DATA_select_system
                  )
  
  mongo <- mongo.create()
  data_all <- mongo.find.all(mongo,ns=ns_rtdata,query=criteria,data.frame = TRUE)
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
  
  }else{
    
  criteria <- as.vector(unlist(input$DATA_select_system))
  
  criteria <- list(generation  =    0L, 
                   sys_id      =    list('$in'=criteria)
                  )
  
  mongo <- mongo.create()
  data_all <- mongo.find.all(mongo,ns=ns_rtdata,query=criteria,data.frame = TRUE)
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
  }
  
  
  

  
  
  # Change to local timezone
  data_all[,"time"] <- as.POSIXct(as.numeric(data_all[,"time"]), origin = "1970-01-01", tz = "GMT") - time_zone_offset()
  
  data_all <- data_all[c("name","sys_id","username","recorded_rt","pubchem","inchi","time")]
  data_all[,"sys_id"] <- sys_oid2name(data_all[,"sys_id"])
  colnames(data_all) <- c("Name","System","Username","RT","Pubchem","InChI","Time")
  
  
  
  return(data_all)
})






## UI elements ##########################

output$DATA_select_system <- renderUI({
  
  
  mongo <- mongo.create()
  DATA_sys_oids <-   mongo.distinct(mongo=mongo,ns=ns_rtdata, "sys_id")
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo) 
  
  
  DATA_sys_oids_name <-  sys_oid2name(DATA_sys_oids)
  
  
  
  # make list with options and oid as output
  DATA_sys_oids      <- c("",DATA_sys_oids)
  DATA_sys_oids_name <- c("",DATA_sys_oids_name)
  
  DATA_opt_list <- as.list(DATA_sys_oids)
  names(DATA_opt_list) <- DATA_sys_oids_name
  
  selectInput(inputId = 'DATA_select_system',label= strong('Select system(s)'),choices=DATA_opt_list,selected="",selectize=FALSE,multiple=TRUE)
  
})






output$DATA_download <- downloadHandler(
    
  filename = function() {
    paste('data-', Sys.Date(), '.csv', sep='')
    
  },
  
  content = function(file) {
    write.csv(DATA_data(), file)
  },
  
  
  contentType = "text/csv"
)


output$DATA_download_ui <- renderUI({
  if(is.null(DATA_data())) return(NULL)
  
  downloadButton('DATA_download', 'Download data')
  
})





## Make settings
DATA_table_settings <- reactive({
  colwidths <- c("30em", "8em","8em", "8em","8em","NA","11em")
  col.names <- c("Name","System","Username","RT","Pubchem","InChI","Time")
  aoColumnDefs <- list(NULL)
  for(i in 1:length(col.names)){
    column <- list(sWidth=colwidths[i], sTitle=col.names[i], aTargets=list(i-1))
    aoColumnDefs[[i]] <- column
  }
  
  return(aoColumnDefs)
})


## Display the table
output$DATA_download_table <- renderDataTable({
  if(is.null(DATA_data())) return(NULL)
  
  table_data <- DATA_data()
  table_data[,"RT"]      <-     round(table_data[,"RT"],digits=2)
    
  
  # Make sure Inchi is not too long
  table_data[,"InChI"] <- 
    paste0('<div style= "-o-text-overflow: ellipsis; text-overflow: ellipsis;  overflow:hidden;  white-space:nowrap;   width: 38em;">'
           ,table_data[,"InChI"],'</div>')
  
  
  
  return(table_data)
  
}
,options=list(pageLength = 15,aoColumnDefs=DATA_table_settings(), columns=NULL,AutoWidth=FALSE    )
,escape=FALSE
)






