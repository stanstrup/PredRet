predicted_data <- reactive({
  if(is.null(input$PREDICTIONS_select_system)) return(NULL)
  if(input$PREDICTIONS_select_system == "") return(NULL)
  
  
  criteria <- mongo.bson.buffer.create()
  mongo.bson.buffer.append(criteria, "sys_id", input$PREDICTIONS_select_system)
  mongo.bson.buffer.append(criteria, "generation", 1L)
  criteria <- mongo.bson.from.buffer(criteria)
  
  mongo <- mongo.create()
  predicted_data <- mongo.find.all(mongo,ns=ns_rtdata,query=criteria,data.frame = TRUE)
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
  
  return(predicted_data)
})






## UI elements ##########################

output$PREDICTIONS_select_system <- renderUI({
  
  
  mongo <- mongo.create()
  prediction_to_oids <- mongo.find.all(mongo=mongo,ns=ns_pred_stats,fields=list(sys_oid=1L),data.frame=T)
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo) 
  
  
  sys_models_oid1 <- prediction_to_oids[,"sys_oid"]
  sys_models_oid1_name <-  sys_oid2name(ns=ns_chrom_systems,sys_models_oid1)
  
  
  
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
,options=list(pageLength = 15,aoColumnDefs=predict_table_settings(), columns=NULL,AutoWidth=FALSE    )
,escape=FALSE
)







## Statistics table
predstats <- reactive({
  if(is.null(input$PREDICTIONS_select_system)) return(NULL)
  if(input$PREDICTIONS_select_system == "") return(NULL)
  
  predstats <- pred_stat_get(sys_oid=input$PREDICTIONS_select_system,ns=ns_pred_stats)
  
  return(predstats)
  
})



output$pred_stats_table = renderTable({
                                        if(is.null(predstats())) return(NULL)
                                        predstats()          
                                      }
                                        ,digits=cbind(rep(0,nrow(predstats())),c(0,0,2,2,2,2,2,2,2,2)),     include.colnames=FALSE,floating=FALSE,   sanitize.rownames.function =  bold.allrows,           align=paste0("l",rep("r",ncol(predstats()) )   )
                                     )






output$pred_stats_table_title <- renderUI({
  if(is.null(predstats())) return(NULL)
  h4("Prediction statistics")
})


output$pred_stats_table_text <- renderUI({
                                            if(is.null(predstats())) return(NULL)
                                            helpText("*Based on compounds for which the experimental RT is known.")
})
