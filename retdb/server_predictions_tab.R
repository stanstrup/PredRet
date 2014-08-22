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
    write.csv(predicted_data(), file)
  }
)






output$PREDICTIONS_data <- renderDataTable({
  
  return(   predicted_data()   )
  
})