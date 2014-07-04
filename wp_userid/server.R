shinyServer(function(input, output, session) {
  
  
  
  userID <- reactive({   input$userID })
  username <- reactive({   input$username })
  
  output$view <- renderText(     paste0(  "User ID is: ",userID()," and username is: ",username()  )      )

})