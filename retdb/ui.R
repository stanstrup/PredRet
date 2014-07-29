library(shinyBS)

navbarPage("",
             
             tabPanel('Introduction',
                                      fluidPage(
                                       titlePanel("Introduction"),
                                       includeMarkdown("markdowns/intro_text.md")
                                     )
                      ),
                      
                      
                        tabPanel('Upload data',

                                                   # Hidden input boxes to save the variable to
                                                   HTML(' <input type="text" id="userID" name="userID" style="display: none;"> '),
                                                   HTML(' <input type="text" id="username" name="username" style="display: none;"> '),
                                                   HTML(' <input type="text" id="user_logged_in" name="user_logged_in" style="display: none;"> '),
                                 
                                 
                                                   # include the js code
                                                   includeScript("scripts/get_user_id.js"),
                                                   
                                 
                                                  pageWithSidebar(
                                                                    headerPanel("Upload data"),
                                                                    sidebarPanel(
                                                                                  checkboxInput("only_own_systems_upload","Only show your own systems?",value=TRUE),
                                                                                  uiOutput("system_upload"),              
                                                                                  fileInput("files", "File data", multiple=F,accept=c("text/csv")),
                                                                                  bsCollapse(bsCollapsePanel("Show help",includeMarkdown("markdowns/upload_help.md")))
                                                                                ),
                                                                    mainPanel(
                                                                              uiOutput("is_written"),
                                                                              uiOutput("error_msg"),
                                                                              tableOutput("filetable"),
                                                                              tableOutput("data")
                                                                            )
                                                                )
                        ),
                        
                        
                        
                        tabPanel('Add or modify system',
                                                           fluidRow(
                                                                               headerPanel("Add or modify system"),
                                                                               column(3,
                                                                                             wellPanel(
                                                                                                        style="margin-top:60px",
                                                                                                        checkboxInput("only_own_systems","Only show your own systems?",value=TRUE),
                                                                                                        uiOutput("system_name_select"),
                                                                                                        bsCollapse(bsCollapsePanel("Show help",includeMarkdown("markdowns/addsys_help.md")))
                                                                                                       
                                                                                             )
                                                                               ),
                                                                               column(5,
                                                                                              uiOutput("system_name"),
                                                                                      
                                                                                      
                                                                                      
                                                                                      
                                                                                      
                                                                                              div(strong("Eluents and modifiers"),style="padding-top:20px"),
                                                                                      
                                                                                              div(style="display:flex",
                                                                                                  div(style="width: 300px;",         
                                                                                                      uiOutput("SYSTEM_eluent_select")
                                                                                                  ),
                                                                                                  div(style="flex-grow: 1;",
                                                                                                      uiOutput("SYSTEM_eluent_name"),
                                                                                                      tags$style(type='text/css', "#SYSTEM_eluent_name { width: 100%; }")
                                                                                                  )
                                                                                              ),
                                                                                              
                                                                                      
                                                                                      
                                                                                      
                                                                                      
                                                                                      div(strong("Column"),style="padding-top:20px"),
                                                                                      
                                                                                      div(style="display:flex",
                                                                                          div(style="width: 300px;",         
                                                                                              uiOutput("SYSTEM_column_select")
                                                                                          ),
                                                                                          div(style="flex-grow: 1;",
                                                                                              uiOutput("SYSTEM_column_name"),
                                                                                              tags$style(type='text/css', "#SYSTEM_column_name { width: 100%; }")
                                                                                          )
                                                                                      ),
                                                                                      
                                                                                      
                                                                                      
                                                                                      div(style="padding-top:20px"),
                                                                                      uiOutput("SYSTEM_ref"),
                                                                                      tags$style(type='text/css', "#SYSTEM_ref { width: 100%; }"),
                                                                                      div(style="padding-top:20px"),
                                                                                      
                                                                                      
                                                                                              
                                                                                              uiOutput("system_desc"),
                                                                                              uiOutput("submit_system")
                                                                               )
                                                                    ) 
                                 
                                 ),
             
             
                       tabPanel('Manage data',
                                HTML('<script>document.domain = "predret.com"</script>'),
                                
                                fluidPage(
                                  
                                  div(style="display:flex",
                                      div(style="width: 300px;",         
                                                                                selectInput(inputId = 'MANAGE_filter_by',label= 'Select by',choices=c("","system", "date added"),selected="",selectize=TRUE)
                                          ),
                                      div(style="flex-grow: 1;",
                                                                                uiOutput("MANAGE_filter_select")
                                          )
                                      ),
                                  
                                    actionButton("del_data","Delete selected data"),
                                    div(downloadButton('downloadData', 'Download your data'),style="text-align: right;margin-top:-30px"),
                                    div(style="padding:30px"),
                                    dataTableOutput("MANAGE_data")
                                  
                                )
                                ,HTML('<script type="text/javascript" src="http://predret.com/scripts/iframeResizer.contentWindow.min.js"></script>')
                       )
             
                      
                        
                        
          )