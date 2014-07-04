shinyUI(
  navbarPage("",
             
             
                        tabPanel('Upload data',

                                                   # Hidden input boxes to save the variable to
                                                   HTML(' <input type="text" id="userID" name="userID" style="display: none;"> '),
                                                   HTML(' <input type="text" id="username" name="username" style="display: none;"> '),
                                                   
                                                   # include the js code
                                                   includeScript("../scripts/get_user_id.js"),
                                                   
                                 
                                                  pageWithSidebar(
                                                                    headerPanel("Upload data"),
                                                                    sidebarPanel(
                                                                                  checkboxInput("only_own_systems_upload","Only show your own systems?",value=TRUE),
                                                                                  uiOutput("system_upload"),              
                                                                                  fileInput("files", "File data", multiple=F,accept=c("text/csv"))
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
                                                                               column(3,
                                                                                             wellPanel(
                                                                                                        style="margin-top:60px",
                                                                                                        checkboxInput("only_own_systems","Only show your own systems?",value=TRUE),
                                                                                                        uiOutput("system_name_select")
                                                                                                       
                                                                                             )
                                                                               ),
                                                                               column(5,
                                                                                             headerPanel("Add or modify system"),
                                                                                              uiOutput("system_name"),
                                                                                              uiOutput("system_desc"),
                                                                                              uiOutput("submit_system")
                                                                               )
                                                                    ) 
                                 
                                 ),
             
             
                       tabPanel('Manage data',
                                
                                basicPage(
                                  
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
                       )
             
                      
                        
                        
          )
)



