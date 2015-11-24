library(shiny)
library(DT)

jsfunc <- "function() {arrIndexes=[1,3,4,8,12]; $('#tbl tbody tr').filter(function(index) {return arrIndexes.indexOf(index) > -1;}).click()}"

shinyApp(
  
  
  ui = fluidPage(DT::dataTableOutput('tbl'),verbatimTextOutput('x4')),
  
  
  
  
  server = function(input, output,session) {
    
    iris2 <- iris
    rownames(iris2) <- paste0("row",rownames(iris))
    
        output$tbl = DT::renderDataTable(iris2, rownames = TRUE, server=TRUE,selection = "multiple", options = list(initComplete = JS(jsfunc))   )
    
    
    output$x4 = renderPrint({
      s = input$tbl_rows_selected
      if (length(s)) {
        cat('These rows were selected:\n\n')
        cat(s, sep = '\n')
      }
    })
    
  }
  
    
)
