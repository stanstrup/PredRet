fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  titlePanel("Introduction"),
  includeMarkdown("markdowns/intro_text.md")
)