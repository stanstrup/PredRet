pageWithSidebar(
  headerPanel("Upload data"),
  sidebarPanel(style="max-width: 340px;",
    checkboxInput("only_own_systems_upload","Only show your own systems?",value=TRUE),
    uiOutput("system_upload"),              
    fileInput("files", "File data", multiple=F,accept=c("text/csv")),
    actionButton("upload_go_Button", "Process & add data"),
    div(style="margin-bottom:50px"),
    bsCollapse(bsCollapsePanel("Show help",includeMarkdown("markdowns/upload_help.md")))
  ),
  mainPanel(
    #bsProgressBar("uploadprogress", value = 0,visible=FALSE),
    uiOutput("is_written"),
    bsAlert("X1"),
    bsAlert("X2")#,
    #tableOutput("filetable"),
    #tableOutput("data")
  )
)