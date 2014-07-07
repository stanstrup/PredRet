require(rCharts)

shinyUI(
  navbarPage("",
                        tabPanel('Explore models',

                                                   # Hidden input boxes to save the variable to
                                                   HTML(' <input type="text" id="userID" name="userID" style="display: none;"> '),
                                                   HTML(' <input type="text" id="username" name="username" style="display: none;"> '),
                                                   
                                                   # include the js code
                                                   includeScript("scripts/get_user_id.js"),
                                                   
                                 
                                                   basicPage(
                                                     showOutput("chart1", "highcharts")
                                                            )
                      
                        
                        
                                )
         )



)