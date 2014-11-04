library(shinyBS)

navbarPage("",
             

           
             tabPanel('Introduction',
                                      fluidPage(
                                        
                                        tags$head(
                                          tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
                                        ),
                                        
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
                                                                                                      uiOutput("SYSTEM_eluent_name")
                                                                                                  )
                                                                                              ),
                                                                                              
                                                                                      
                                                                                      
                                                                                      
                                                                                      
                                                                                      div(strong("Column"),style="padding-top:20px"),
                                                                                      
                                                                                      div(style="display:flex",
                                                                                          div(style="width: 50%;",         
                                                                                              uiOutput("SYSTEM_column_select")
                                                                                          ),
                                                                                          div(style="flex-grow: 1;",
                                                                                              uiOutput("SYSTEM_column_name")
                                                                                          )
                                                                                      ),
                                                                                      
                                                                                      
                                                                                      
                                                                                      div(style="padding-top:20px"),
                                                                                      uiOutput("SYSTEM_ref"),
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
                                  
                                  div(id="selectfilter_container",
                                              div(
                                                  div(class="well white_no_pad_marg",style="margin-right: 20px;margin-bottom: 20px;",
                                                           h4("Select specific data",style="text-align: center"),
                                                           wellPanel(id="selectby",style="min-width: 490px;padding: 15px 10px 0px 19px;margin-bottom: 0px;",
                                                                      div(style="display:flex",
                                                                           div(style="margin-right: 50px;",     selectInput(inputId = 'MANAGE_filter_by',label= strong('Select by'),choices=c("","system", "date added"),selected="",selectize=TRUE)   ),
                                                                           div(                                 uiOutput("MANAGE_filter_select")    )
                                                                         )
                                                          
                                                                      
                                                                    )
                                                  )  ,
                                                           div(style="text-align: center;",actionButton("del_data","Delete selected data"))
                                                   
                                                   ),
                                          
                                      
                                              div(class="well white_no_pad_marg",
                                                            h4("Filter based on mass and retention time",style="text-align: center"),
                                                              wellPanel(style="padding: 8px 30px 0px 19px;margin-bottom: 0px;",
                                                                        
                                                                        div(style="display:flex",
                                                                            p(class="inputlabel",     strong("RT range:")),
                                                                            div(class="smallnuminput_left",     numericInput("MANAGE_rtrange_min","",value=NA,step=0.01,min=0)),
                                                                            div(p(style="line-height: 120%;font-size: 2em;",       "-")),
                                                                            div(class="smallnuminput_right",     numericInput("MANAGE_rtrange_max","",value=NA,step=0.01,min=0)    )
                                                                           ),
                                                                        
                                                                        
                                                                        div(style="display:flex",
                                                                            p(class="inputlabel",strong("Mass range (Da):")),
                                                                            div(class="smallnuminput_left",     numericInput("MANAGE_massrange_min","",value=NA,step=0.0001,min=0)),
                                                                            div(p(style="line-height: 120%;font-size: 2em;",       "-")),
                                                                            div(class="smallnuminput_right",     numericInput("MANAGE_massrange_max","",value=NA,step=0.0001,min=0)    )
                                                                           ),
                                                                        
                                                                        
                                                                        div(style="display:flex",
                                                                            p(class="inputlabel",     strong("Exact mass:")),
                                                                            div(class="smallnuminput_left",     numericInput("MANAGE_exactmass","",value=NULL,step=0.0001,min=0)),
                                                                            div(p(class="inputlabel",style="width:3em", strong("ppm:"))),
                                                                            div(class="smallnuminput_right",style="width:57px",     numericInput("MANAGE_ppm","",value=NULL,step=1,min=0)    )
                                                                        )
                                                                        
                                                                        
                                                                        
                                                                        )
                                                      
                                              )
                                      
                                  ),
                                  
                                  
                                    div(downloadButton('downloadData', 'Download your data'),style="text-align: right;margin-bottom:20px"),
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
                                            
                                            div(uiOutput('download_predicted_ui'),style="text-align: right;margin-bottom: 20px;margin-top: 30px"),
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
                      
                      div(uiOutput('DATA_download_ui'),style="text-align: right;margin-bottom: 20px;margin-top: 30px"),
                      dataTableOutput("DATA_download_table"),
                      div(style="height:200px")
                    )
           )
           
           
                        
          )