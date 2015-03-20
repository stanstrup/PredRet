bold.allrows <- function(x) {
  #h <- paste('\\textbf{',x,'}', sep ='')
  h <- paste0('<strong>',x,'</strong>')
  h
}



is.between <- function(x,a,b) {
  
  if(!is.na(a) & !is.na(b)) return(x>=a & x<=b)
  if(is.na(a) & is.na(b))   return(rep(TRUE,length(x))) 
  if(is.na(a)) return(x<=b)
  if(is.na(b)) return(x>=a)
  
}
