library(shiny)
library(rCharts)
library(shinyIncubator)
library(PredRetR)





navbarPage("",
           tabPanel('Explore models',
                    
                    # Hidden input boxes to save java variable to
                    HTML(' <input type="text" id="userID" name="userID" style="display: none;"> '),
                    HTML(' <input type="text" id="username" name="username" style="display: none;"> '),
                    HTML(' <input type="text" id="user_logged_in" name="user_logged_in" style="display: none;"> '),
                    HTML(' <input type="text" id="is_admin" name="is_admin" style="display: none;"> '),
                    
                    HTML('<input type="text" id="client_time" name="client_time" style="display: none;"> '),
                    HTML('<input type="text" id="client_time_zone_offset" name="client_time_zone_offset" style="display: none;"> '),
                    
                    
                    # include the js code
                    includeScript("scripts/get_user_id.js"),
                    includeScript("scripts/user_time.js"),
                    
                    
                    fluidRow(
                      div(headerPanel("Explore the models build between systems"),style="margin-left:47.9px"),
                      
                      column(3,style="margin-top:60px;width:300px",
                             wellPanel(
                               
                               uiOutput("explore_sys1"),
                               uiOutput("explore_sys2")
                             )
                             
                      ),
                      
                      column(5,
                             style="margin-top:60px",
                             showOutput("chart1", "highcharts")
                             
                      ) 
                      
                    )
           ),
           
           
           
           
           
           
           
           
           
           
           
           tabPanel('Model statistics',
                    
                    basicPage(
                      
                      dataTableOutput("MODELSTATS_table") 
                      
                    )
           ),            
           
           
           tabPanel('Prediction statistics',
                    
                    basicPage(
                      
                      dataTableOutput("PREDSTATS_table") 
                      
                    )
           ),         
           
           
           
           
           
           tabPanel('Build models',
                    HTML('<script>document.domain = "predret.org"</script>'),
                    fluidRow(progressInit(),
                             column(3,
                                    style="width:310px",
                                    
                                    wellPanel(
                                      uiOutput("build_sys1"),
                                      uiOutput("build_sys2"),
                                      checkboxInput("build_force_recalc", "Force re-calculation", value = FALSE),
                                      div(actionButton("build_specific","(Re-)build model between systems") ,style="width:180px;margin: 0 auto;")
                                    ),
                                    
                                    wellPanel(
                                      checkboxInput("build_force_recalc_all", "Force re-calculation", value = FALSE),
                                      div(actionButton("build_all","(Re-)build models between ALL systems") ,style="width:180px;margin: 0 auto;")
                                    ),
                                    
                                    wellPanel(
                                      div(actionButton("purge_predictions","purge ALL predictions") ,style="text-align: center;margin: 0 auto;margin-bottom: 15px;"),
                                      div(actionButton("purge_models","purge ALL models") ,style="text-align: center;margin: 0 auto;")
                                    )
                             ),
                             
                             column(7,
                                    dataTableOutput("build_table"),
                                    div(style="margin-top:100px",h2("Build log")),
                                    dataTableOutput("build_log")
                             )
                             
                             
                    )
                    ,HTML('<script type="text/javascript" src="http://predret.org/scripts/iframeResizer.contentWindow.min.js"></script>')
           )    
           
           
           
           
           
           
           
           
)     
