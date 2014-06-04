inputTextarea <- function(inputId, label,value="", nrows, ncols) {
  tagList(
    singleton(tags$head(tags$script(src = "textarea.js"))),
    tags$label(`for` = inputId,
                  as.character(label)
    ),
    tags$textarea(id = inputId,
                  class = "inputtextarea",
                  as.character(value),
                  style=paste0("height:",nrows,"px;width:",ncols,"px")
                  )
  )
}



shinyUI(
  navbarPage("",
                        tabPanel('Upload data',
                                                  pageWithSidebar(
                                                                    headerPanel("File input test"),
                                                                    sidebarPanel(
                                                                                    fileInput("files", "File data", multiple=T,accept=c("text/csv"))
                                                                                ),
                                                                    mainPanel(
                                                                              uiOutput("is_written"),
                                                                              tableOutput("filetable"),
                                                                              tableOutput("data")
                                                                            )
                                                                )
                        ),
                        
                        
                        
                        tabPanel('Add or modify system',
                                                           fluidRow(
                                                                               column(3,
                                                                                             wellPanel(
                                                                                                        style="margin-top:60px",
                                                                                                        uiOutput("system_name_select")
                                                                                                       
                                                                                             )
                                                                               ),
                                                                               column(5,
                                                                                             headerPanel("Add or modify system"),
                                                                                              uiOutput("system_name"),
                                                                                             inputTextarea('system_desc','System description', 'Describe column, solvents, modifiers and gradient etc.',300,600),
                                                                                             actionButton("submit_system","Add/update system")
                                                                               )
                                                                    ) 
                                 
                                 )
                      
                        
                        
          )
)



