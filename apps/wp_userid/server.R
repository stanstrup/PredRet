shinyServer(function(input, output, session) {
  
  
  
  output$results = renderPrint({
    input$mydata
  })
  
})