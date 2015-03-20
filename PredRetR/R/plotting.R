
plot_systems <- function(plotdata) {
  #devtools:::install_github("ramnathv/rCharts")
  
  
  # Simple R plot ################
  # plot(loess.boot$data[,1],loess.boot$data[,2],pch=20)
  # lines(newdata,ci[,1])
  # lines(newdata,ci[,2],lty=3)
  # lines(newdata,ci[,3],lty=3)
  
  
  
  
  # attempt with rPlot ##############################
  # p1 <- rPlot(x = "x", y = "y",  data = plotdata, type = 'point', size = list(const = 3))
  # p1$layer(x = "x", y = "predicted",   data = plotdata, type = 'line', size = list(const = 2),color = list(const = 'black'))
  # p1$layer(x = "x", y = "upper", data = plotdata, type = 'line', size = list(const = 1),color = list(const = 'red'))
  # p1$layer(x = "x", y = "lower", data = plotdata, type = 'line', size = list(const = 1),color = list(const = 'red'))
  # 
  # p1$params$width=800
  # p1$params$height=600
  # p1
  
  
  
  
  # hPlot with tooltip ######################
  # Plot the points
  p <- hPlot(y ~ x, data = plotdata$data, type = "scatter")
  
  # fix data format
  p$params$series[[1]]$data <- toJSONArray(cbind.data.frame(x = plotdata$data$x, y = plotdata$data$y,name = plotdata$data$name,tooltip = plotdata$data$tooltip), json = F)
  
  # add tooltip formatter
  p$tooltip(formatter = "#! function() {return(this.point.tooltip);} !#")
  
  # Set priority higher than the overlay
  p$params$series[[1]]$zIndex=2
  
  
  p$title(style=list(fontSize='24px'),text=plotdata$title)
  
  p$xAxis(title=list(text=plotdata$xlab,style=list(fontSize='18px')))
  p$yAxis(title=list(text=plotdata$ylab,style=list(fontSize='18px')))
  
  
  p$series(
    data = toJSONArray2(cbind.data.frame(plotdata$data$newdata,plotdata$data[['predicted']]), names = F, json = F),
    type = 'line',
    zIndex = 1,
    marker=list(enabled=F,states=list(hover=list(enabled=F)))
  )
  
  
  p$series(
    data = toJSONArray2(cbind.data.frame(plotdata$data$newdata,plotdata$data[['lower']],plotdata$data[['upper']]), names = F, json = F),
    type = 'arearange',
    fillOpacity = 0.3,
    lineWidth = 0,
    color = 'lightblue',
    zIndex = 0
  )
  
  
  
  p$set(tooltip = list(
    crosshairs =  c(T,T),
    shared = T))
  
  
  # set plot size
  p$params$width=800*0.8
  p$params$height=600*0.8
  
  # plot
  invisible(p)
  
  
}

