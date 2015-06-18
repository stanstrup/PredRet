fluidPage(
  

  div(style="margin: 0 auto;text-align: center;width:300px;",
      uiOutput("DATA_select_system")
  ),
  
  div(uiOutput('DATA_download_ui'),style="text-align: right;margin-bottom: 20px;margin-top: 30px"),
  dataTableOutput("DATA_download_table"),
  div(style="height:200px")
)