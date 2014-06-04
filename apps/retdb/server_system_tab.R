systems_in_db <- reactive({
  
  mongo <- mongo.create()
  ns <- "test2.chrom_systems"
  
  
  # select fields (think columns)
  fields = mongo.bson.buffer.create()
  mongo.bson.buffer.append(fields, "system_name", 1L)
  mongo.bson.buffer.append(fields, "_id", 0L)
  fields = mongo.bson.from.buffer(fields)
  
  data_back = mongo.find.all2(mongo, ns=ns,fields=fields)
  
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
  as.character(unlist(data_back))
})




output$system_name_select <- renderUI({
  selectInput(inputId = 'system_name_select',label= 'Select existing system',choices=c("",systems_in_db()),selected="",selectize=TRUE)
})

output$system_name <- renderUI({
  textInput('system_name', label='System name',value = input$system_name_select)
})







## Get inputs and write to database
system_desc_bson <- reactive({
  if(input$submit_system == 0) return(NULL)
  
  isolate({
    buf <- mongo.bson.buffer.create()
    mongo.bson.buffer.append(buf, "system_name", input$system_name)
    mongo.bson.buffer.append(buf, "system_desc", input$system_desc)
    mongo.bson.from.buffer(buf)
  })
})



observe({
  if(input$submit_system == 0) return(NULL)
  
  mongo <- mongo.create()
  ns <- "test2.chrom_systems"
  mongo.insert.batch(mongo, ns, list(system_desc_bson()))
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
})