library(shiny)

shinyUI( bootstrapPage(

  
  # include the js code
  includeScript("../../scripts/get_user_id.js"),
  
  # a div named mydiv
  tags$div(id="mydiv",
           style="width: 50px; height :50px; left: 100px; top: 100px;
           background-color: gray; position: absolute"),
  
  # an element for unformatted text
  verbatimTextOutput("results")
))
