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
