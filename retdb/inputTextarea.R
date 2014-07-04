inputTextarea <- function(inputId, label,value="", nrows, ncols) {
  tagList(
    singleton(tags$head(tags$script(src = "textarea.js"))),
    tags$label(`for` = inputId,
               as.character(label)
    ),
    tags$textarea(id = inputId,
                  class = "inputtextarea",
                  as.character(value),
                  style=paste0("height:",nrows,"px;width:",ncols,"px")
    )
  )
}