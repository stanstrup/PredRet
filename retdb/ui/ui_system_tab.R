fluidPage(style="margin: 0 auto;max-width: 800px;",
          #headerPanel("Add or modify system"),
          
          wellPanel( style="margin-top:30px;padding-bottom:0px;",
                     
                     div(class="flexpad",
                         div(uiOutput("system_name_select")),
                         div(style="padding-top: 20px;padding-left: 30px;",checkboxInput("only_own_systems","Only show your own systems?",value=TRUE))
                     ),
                     
                     bsCollapse(bsCollapsePanel("Show help",includeMarkdown("markdowns/addsys_help.md")))
                     
          ),
          
          
          
          
          
          div(class="flexpad",style="margin-bottom: 30px;margin-top: 40px;",
              div(class="SYSTEM_varname",      strong('System name')                            ),
              div(class="SYSTEM_varselect1",           uiOutput("system_name")                         ),
              div(class="SYSTEM_varselect2"                                                      )
          ),    
          
          
          div(class="flexpad",
              div(class="SYSTEM_varname"                      ),
              div(class="SYSTEM_varselect1",    strong("Suggested descriptions")                 ),
              div(class="SYSTEM_varselect2",    strong("Your/selected description")                )
          ),                                                                                      
          
          
          
          div(class="flexpad",
              div(class="SYSTEM_varname",              strong("Column type")                                  ),
              div(class="SYSTEM_varselect1",            uiOutput("SYSTEM_column_type_select")                  ),
              div(class="SYSTEM_varselect2",            uiOutput("SYSTEM_column_type_name")                    )
          ),
          
          
          div(class="flexpad",
              div(class="SYSTEM_varname",              strong("Column")                                  ),
              div(class="SYSTEM_varselect1",            uiOutput("SYSTEM_column_select")                  ),
              div(class="SYSTEM_varselect2",            uiOutput("SYSTEM_column_name")                    )
          ),
          
          
          
          div(class="flexpad",
              div(class="SYSTEM_varname",      strong("Eluents")                                ),
              div(class="SYSTEM_varselect1",    uiOutput("SYSTEM_eluent_select")                 ),
              div(class="SYSTEM_varselect2",    uiOutput("SYSTEM_eluent_name")                   )
          ),
          
          div(class="flexpad",
              div(class="SYSTEM_varname",      strong("Eluent pH")                                ),
              div(class="SYSTEM_varselect1",    uiOutput("SYSTEM_eluent_pH_select")                 ),
              div(class="SYSTEM_varselect2",    uiOutput("SYSTEM_eluent_pH_name")                   )
          ),
          
          div(class="flexpad",
              div(class="SYSTEM_varname",      strong("Eluent additives")                                ),
              div(class="SYSTEM_varselect1",    uiOutput("SYSTEM_eluent_additive_select")                 ),
              div(class="SYSTEM_varselect2",    uiOutput("SYSTEM_eluent_additive_name")                   )
          ),
          
          
          
          div(style="padding-top:20px",uiOutput("SYSTEM_ref")),
          
          
          
          
          div(style="padding-top:60px",uiOutput("system_desc")),
          div(uiOutput("submit_system")),
          div(uiOutput("submit_system_warnings"))
          
) 