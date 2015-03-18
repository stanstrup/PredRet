fluidPage(style="margin: 0 auto",
          wellPanel( style="margin: 0 auto;margin-top:30px;padding-bottom:0px;max-width: 600px;",
                     includeMarkdown("markdowns/suspect_text.md")
          ),
          div(downloadButton('download_suspect_Data', 'Download your data'),style="text-align: right;margin-bottom:20px"),
          dataTableOutput("SUSPECT_data")
          
)