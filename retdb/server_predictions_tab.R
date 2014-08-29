predicted_data <- reactive({
  if(is.null(input$PREDICTIONS_select_system)) return(NULL)
  if(input$PREDICTIONS_select_system == "") return(NULL)
  

  predicted_data <- predict_RT(input$PREDICTIONS_select_system)
  
  return(predicted_data)
  
})






## UI elements ##########################

output$PREDICTIONS_select_system <- renderUI({
  
  sys_models_oid1 <- sapply(models(),function(x) x$oid_sys1)
  sys_models_oid1_name <-  sys_oid2name(sys_models_oid1)
  sys_models_oid1_name <- sys_models_oid1_name[!duplicated(sys_models_oid1)]
  sys_models_oid1 <- sys_models_oid1[!duplicated(sys_models_oid1)]
  
  
  # make list with options and oid as output
  sys_models_oid1      <- c("",sys_models_oid1)
  sys_models_oid1_name <- c("",sys_models_oid1_name)
  
  opt_list <- as.list(sys_models_oid1)
  names(opt_list) <- sys_models_oid1_name
  
  selectInput(inputId = 'PREDICTIONS_select_system',label= 'Select system you want retention times for',choices=opt_list,selected="",selectize=TRUE)
  
})






output$download_predicted <- downloadHandler(
    
  filename = function() {
    paste('data-', Sys.Date(), '.csv', sep='')
  },
  content = function(file) {
    write.csv(predicted_data()[c("name","recorded_rt","predicted_rt","ci_lower","ci_upper","pubchem","inchi")], file)
  }
)


output$download_predicted_ui <- renderUI({
  if(is.null(predicted_data())) return(NULL)
  
  downloadButton('download_predicted', 'Download predicted data')
  
})





## Make settings
predict_table_settings <- reactive({
  colwidths <- c("500px", "120px","120px", "120px","120px","120px","NA")
  col.names <- c("Name","Recorded RT","Predicted RT","CI lower","CI upper","Pubchem","InChI")
  aoColumnDefs <- list(NULL)
  for(i in 1:length(col.names)){
    column <- list(sWidth=colwidths[i], sTitle=col.names[i], aTargets=list(i-1))
    aoColumnDefs[[i]] <- column
  }
  
  return(aoColumnDefs)
})


## Display the table
output$PREDICTIONS_data <- renderDataTable({
  if(is.null(predicted_data())) return(NULL)
  
  
  predicted_data <- predicted_data()[c("name","recorded_rt","predicted_rt","ci_lower","ci_upper","pubchem","inchi")]
  
  predicted_data[,"recorded_rt"]      <-     round(predicted_data[,"recorded_rt"],digits=2)
  predicted_data[,"predicted_rt"]     <-     round(predicted_data[,"predicted_rt"],digits=2)
  predicted_data[,"ci_lower"]         <-     round(predicted_data[,"ci_lower"],digits=2)
  predicted_data[,"ci_upper"]         <-     round(predicted_data[,"ci_upper"],digits=2)
  
  
  # Make sure Inchi is not too long
  predicted_data[,"inchi"] = 
    paste0('<div style= "-o-text-overflow: ellipsis; text-overflow: ellipsis;  overflow:hidden;  white-space:nowrap;   width: 500px;">'
           ,predicted_data[,"inchi"],'</div>')
  
  
  
  return(predicted_data)
  
}
,options=list(iDisplayLength = 15,aoColumnDefs=predict_table_settings(), aoColumns=NULL,bAutoWidth=FALSE    )
)