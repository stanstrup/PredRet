output$explore_sys1 <- renderUI({
  oids = do.call(rbind,lapply(models(),function(x) c(oid_sys1=x$oid_sys1,oid_sys2=x$oid_sys2)))
  sys_name = sys_oid2name(as.character(oids))
  dim(sys_name)=c(length(sys_name)/2,2)  
  sys_name_unique1 = unique(sys_name[,1])
    
   return(selectInput(inputId = 'explore_sys1',label= 'Select first system',choices=c("",sys_name_unique1),selected="",selectize=TRUE))
})


output$explore_sys2 <- renderUI({
  if(is.null(input$explore_sys1)) return(NULL)
  if(input$explore_sys1=="") return(NULL)
  
  oids = do.call(rbind,lapply(models(),function(x) c(oid_sys1=x$oid_sys1,oid_sys2=x$oid_sys2)))
  sys_name = sys_oid2name(as.character(oids))
  dim(sys_name)=c(length(sys_name)/2,2)  
  
  return(selectInput(inputId = 'explore_sys2',label= 'Select second system',     choices=c("",sys_name[  input$explore_sys1==sys_name[,1]    ,2]),     selected="",selectize=TRUE))
})




output$chart1 <- renderChart2({
  if(is.null(input$explore_sys1)) return(Highcharts$new())
  if(input$explore_sys1=="") return(Highcharts$new())
  if(is.null(input$explore_sys2)) return(Highcharts$new())
  if(input$explore_sys2=="") return(Highcharts$new())
  
  
  
  
  oids = do.call(rbind,lapply(models(),function(x) c(oid_sys1=x$oid_sys1,oid_sys2=x$oid_sys2)))
  sys_name = sys_oid2name(as.character(oids))
  dim(sys_name)=c(length(sys_name)/2,2)
  
  
  model_select =   input$explore_sys1==sys_name[,1]  & input$explore_sys2==sys_name[,2] 
  if(all(!model_select)) return(Highcharts$new())
  
  
  loess.boot   =   models()[[which(model_select)]]$loess_boot
  ci   =   models()[[which(model_select)]]$ci
  select_names = sys_oid2name(colnames(loess.boot$data))
  
  plotdata=list(
    title = paste0('RT (',select_names[2],') vs. RT (',select_names[1],')'),
    xlab = paste0('RT (',select_names[1],')'),
    ylab = paste0('RT (',select_names[2],')'),
    
    
    data = cbind.data.frame(x=loess.boot$data[,1],
                            y=loess.boot$data[,2],
                            predicted=ci[,1],
                            lower=ci[,2],
                            upper=ci[,3],
                            name=rownames(loess.boot$data),
                            tooltip=paste0('<b>',rownames(loess.boot$data),'</b><br />','x: ',round(loess.boot$data[,1],2),'<br />y: ',round(loess.boot$data[,2],2))
                            )
  )
  
  
  
  
  p = plot_systems(plotdata)
  return(p)
  
})




