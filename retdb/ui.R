library(shiny)
library(shinyBS)
library(xlsx)
library(rmongodb)
library(rmongodb.quick)
library(chemhelper)
library(Rdisop)
library(stringr)
library(PredRetR)
library(DT)

navbarPage("",
             

             ## Introduction ################
             tabPanel('Introduction',      source("ui/ui_intro.R", local=TRUE)$value        ),
                      
                      

             ## Upload ################
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
                                       
                                       tabPanel('Inspect suspect values',   source("ui/ui_upload_tab.R", local=TRUE)$value     )

            ),
                        
                        
           
           
           ## Add or modify system ################
           tabPanel('Add or modify system',   source("ui/ui_system_tab.R", local=TRUE)$value      ),

           
           ## Manage your data ################
           tabPanel('Manage your data',
                    HTML('<script>document.domain = "predret.org"</script>'),
                    source("ui/ui_manage.R", local=TRUE)$value,
                    HTML('<script type="text/javascript" src="http://predret.org/scripts/iframeResizer.contentWindow.min.js"></script>')
                    ),
           
           
           ## Inspect suspect values ################
           tabPanel('Inspect suspect values',   source("ui/ui_suspect_values.R", local=TRUE)$value     ),
           
           
 
           ## Get predictions ################
           tabPanel('Get predictions',   source("ui/ui_predictions_tab.R", local=TRUE)$value      ),
             
                      
           
           ## Get all data ################
           tabPanel('Get all data',     source("ui/ui_data_tab.R", local=TRUE)$value       )
           
           
                        
          )
