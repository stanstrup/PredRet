library(shinyBS)

navbarPage("",
             
           
           
             tabPanel('Introduction',
                                      fluidPage(
                                       titlePanel("Introduction"),
                                       includeMarkdown("markdowns/intro_text.md")
                                     )
                      ),
                      
                      
           
           
           
                        tabPanel('Upload data',

                                                   # Hidden input boxes to save java variable to
                                                   HTML(' <input type="text" id="userID" name="userID" style="display: none;"> '),
                                                   HTML(' <input type="text" id="username" name="username" style="display: none;"> '),
                                                   HTML(' <input type="text" id="user_logged_in" name="user_logged_in" style="display: none;"> '),
                                 
                                                   HTML('<input type="text" id="client_time" name="client_time" style="display: none;"> '),
                                                   HTML('<input type="text" id="client_time_zone_offset" name="client_time_zone_offset" style="display: none;"> '),
                                 
                                 
                                                   # js code to get user info
                                                   includeScript("scripts/get_user_id.js"),
                                                   includeScript("scripts/user_time.js"),
                                                   
                                 
                                                  pageWithSidebar(
                                                                    headerPanel("Upload data"),
                                                                    sidebarPanel(
                                                                                  checkboxInput("only_own_systems_upload","Only show your own systems?",value=TRUE),
                                                                                  uiOutput("system_upload"),              
                                                                                  fileInput("files", "File data", multiple=F,accept=c("text/csv")),
                                                                                  actionButton("upload_go_Button", "Process & add data"),
                                                                                  div(style="margin-bottom:50px"),
                                                                                  bsCollapse(bsCollapsePanel("Show help",includeMarkdown("markdowns/upload_help.md")))
                                                                                ),
                                                                    mainPanel(
                                                                              bsProgressBar("uploadprogress", value = 0,visible=FALSE),
                                                                              uiOutput("is_written"),
                                                                              bsAlert(inputId="upload_complete"),
                                                                              bsAlert(inputId="upload_alerts")#,
                                                                              #tableOutput("filetable"),
                                                                              #tableOutput("data")
                                                                            )
                                                                )
                        ),
                        
                        
           
           
           
           
                        
                        tabPanel('Add or modify system',
                                                           fluidRow(
                                                                               headerPanel("Add or modify system"),
                                                                               column(3,
                                                                                             wellPanel(
                                                                                                        style="margin-top:60px",
                                                                                                        #checkboxInput("only_own_systems","Only show your own systems?",value=TRUE),
                                                                                                        uiOutput("system_name_select"),
                                                                                                        bsCollapse(bsCollapsePanel("Show help",includeMarkdown("markdowns/addsys_help.md")))
                                                                                                       
                                                                                             )
                                                                               ),
                                                                               column(5,
                                                                                              uiOutput("system_name"),
                                                                                      
                                                                                      
                                                                                      
                                                                                      
                                                                                      
                                                                                              div(strong("Eluents and modifiers"),style="padding-top:20px"),
                                                                                      
                                                                                              div(style="display:flex",
                                                                                                  div(style="width: 50%;",         
                                                                                                      uiOutput("SYSTEM_eluent_select")
                                                                                                  ),
                                                                                                  div(style="flex-grow: 1;",
                                                                                                      uiOutput("SYSTEM_eluent_name"),
                                                                                                      tags$style(type='text/css', "#SYSTEM_eluent_name { width: 100%; }")
                                                                                                  )
                                                                                              ),
                                                                                              
                                                                                      
                                                                                      
                                                                                      
                                                                                      
                                                                                      div(strong("Column"),style="padding-top:20px"),
                                                                                      
                                                                                      div(style="display:flex",
                                                                                          div(style="width: 50%;",         
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
                                                                                              uiOutput("submit_system"),
                                                                                              uiOutput("submit_system_warnings")
                                                                               )
                                                                    ) 
                                 
                                 ),
             
             
           
           
           
                       tabPanel('Manage your data',
                                HTML('<script>document.domain = "predret.org"</script>'),
                                
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
                                ,HTML('<script type="text/javascript" src="http://predret.org/scripts/iframeResizer.contentWindow.min.js"></script>')
                       ),
           
           
           
           
           
           
                       tabPanel('Get predictions',
                                fluidPage(
                                            uiOutput("PREDICTIONS_select_system"),
                                            
                                            div(style="margin: 0 auto;width:500px",
                                                                                    uiOutput("pred_stats_table_title"),
                                                                                    tableOutput("pred_stats_table"),
                                                                                    uiOutput("pred_stats_table_text")
                                               ),
                                            
                                            div(uiOutput('download_predicted_ui'),style="text-align: right;margin-bottom: 30px;margin-top: 30px"),
                                            dataTableOutput("PREDICTIONS_data"),
                                            div(style="height:200px")
                                          )
             ),
             
                      
                
           
           
           tabPanel('Get all data',
                    fluidPage(
                      
                      tags$head(
                        tags$style(type="text/css", "select#DATA_select_system { height: 20em; }")
                      ),
                      
                      div(style="margin: 0 auto;text-align: center;",
                      uiOutput("DATA_select_system")
                      ),
                      
                      div(uiOutput('DATA_download_ui'),style="text-align: right;margin-bottom: 30px;margin-top: 30px"),
                      dataTableOutput("DATA_download_table"),
                      div(style="height:200px")
                    )
           )
           
           
                        
          )