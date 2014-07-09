require(rCharts)

shinyUI(
  navbarPage("",
                        tabPanel('Explore models',

                                                   # Hidden input boxes to save the variable to
                                                   HTML(' <input type="text" id="userID" name="userID" style="display: none;"> '),
                                                   HTML(' <input type="text" id="username" name="username" style="display: none;"> '),
                                                   
                                                   # include the js code
                                                   includeScript("scripts/get_user_id.js"),
                                                   
                                 
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
                         )             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             


            )
)