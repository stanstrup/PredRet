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
                        div(style="margin-left: 10px;",p(class="inputlabel",style="width:3em", strong("ppm:"))),
                        div(class="smallnuminput_right",style="width:57px;padding-left: 0px;",     numericInput("MANAGE_ppm","",value=NULL,step=1,min=0)    )
                    )
                    
                    
                    
          )
          
      )
      
  ),
  
  
  div(downloadButton('downloadData', 'Download your data'),style="text-align: right;margin-bottom:20px"),
  dataTableOutput("MANAGE_data")
  
)