predicted_data <- reactive({
  if(is.null(input$PREDICTIONS_select_system)) return(NULL)
  if(input$PREDICTIONS_select_system == "") return(NULL)
  
  # get systems the selection has models to
  models_extended = get_models(include.loess=FALSE,include.ci = TRUE,include.newdata = TRUE )
  sys_models_oid1 <- sapply(models_extended,function(x) x$oid_sys1)
  sys_models_oid2 <- sapply(models_extended,function(x) x$oid_sys2)
  
  target_systems = sys_models_oid1[input$PREDICTIONS_select_system == sys_models_oid2]
  
  
  # get all rt data in database
  mongo <- mongo.create()
  ns    <- ns_rtdata
  data_all = mongo.find.all2(mongo=mongo, ns=ns,query=mongo.bson.empty(),data.frame=T,mongo.oid2character=T) # This can probably be done smarter so only the data we need is queried
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)  
  
  
  select <- data_all[,"sys_id"] %in% target_systems
  data_all <- data_all[select,,drop=F]
  
  unique_inchi <- unique(data_all[,"inchi"])
  
  
  
  
  
  predicted_data = as.data.frame(matrix(nrow=length(unique_inchi),ncol=6))
  colnames(predicted_data)=c("name","predicted_rt","ci_lower","ci_upper","pubchem","inchi")
  
  
  for(i in 1:length(unique_inchi)){    
    single_inchi_data <-  data_all[unique_inchi[i]==data_all[,"inchi"],,drop=FALSE]
    single_inchi_data <-  data.frame(single_inchi_data,predicted=NA,ci_lower=NA,ci_upper=NA)
    
    for(i2 in 1:nrow(single_inchi_data)){   
      current_model <- models_extended[[   which(       (sys_models_oid1 == single_inchi_data[i2,"sys_id"])    &   (sys_models_oid2 == input$PREDICTIONS_select_system)                 )     ]]
      close_rt <- which.min(abs(single_inchi_data[i2,"rt"]-current_model$newdata))
      
      single_inchi_data[i2,"predicted"] <- current_model$ci[close_rt,1]
      single_inchi_data[i2,"ci_lower"]  <- current_model$ci[close_rt,2]
      single_inchi_data[i2,"ci_upper"]  <- current_model$ci[close_rt,3]
    }
    
    
    # For the moment we select just the one with the most narrow CI
    best_pred <- which.min(single_inchi_data[,"ci_upper"]-single_inchi_data[,"ci_lower"])
    
    predicted_data[i,"name"]         <- single_inchi_data[best_pred,"name"]
    predicted_data[i,"predicted_rt"] <- single_inchi_data[best_pred,"predicted"]
    predicted_data[i,"ci_lower"]     <- single_inchi_data[best_pred,"ci_lower"]
    predicted_data[i,"ci_upper"]     <- single_inchi_data[best_pred,"ci_upper"]
    predicted_data[i,"pubchem"]      <- single_inchi_data[best_pred,"pubchem"]
    predicted_data[i,"inchi"]        <- single_inchi_data[best_pred,"inchi"]
    
  }
  
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