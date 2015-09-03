library(PredRetR)
library(ggplot2)
library(gridExtra)


# Pull database
data <- PredRet_get_db()



## Number of compounds and predictions in the database ############################
p12 <- PredRet_plot.db.count(data)
plot(p12)


## experimental vs. predicted regression plot ##################################
p10 <- PredRet_plot.pred.cor(data = data)
plot(p10)


## Violin plot absolute prediction error ############################
p6    <- PredRet_plot.pred.error.abs(data = data)
plot(p6)


## Violin plot relative prediction error ############################
p5    <- PredRet_plot.pred.error.rel(data = data)
plot(p5)


## Violin plot for ci absolute width ############################
p9 <- PredRet_plot.pred.pi.abs(data = data)
plot(p9)


## Violin plot for ci relative width ############################
p8 <- PredRet_plot.pred.pi.rel(data = data)
plot(p8)






## merge plots ############################
p12 <- p12 + geom_text(aes(x=-Inf, y=Inf,hjust=-0.5, vjust=1.5, label = "A"),size=10)
p10 <- p10 + geom_text(aes(x=-Inf, y=Inf,hjust=-0.5, vjust=1.5, label = "B"),size=10)
p6 <- p6   + geom_text(aes(x=-Inf, y=Inf,hjust=-0.5, vjust=1.5, label = "C"),size=10)
p5 <- p5   + geom_text(aes(x=-Inf, y=Inf,hjust=-0.5, vjust=1.5, label = "D"),size=10)
p9 <- p9   + geom_text(aes(x=-Inf, y=Inf,hjust=-0.5, vjust=1.5, label = "E"),size=10)
p8 <- p8   + geom_text(aes(x=-Inf, y=Inf,hjust=-0.5, vjust=1.5, label = "F"),size=10)

# guide=FALSE turns off the legend

p6$scales$scales[[3]]$guide=FALSE
p5$scales$scales[[3]]$guide=FALSE
p9$scales$scales[[3]]$guide=FALSE
p8$scales$scales[[3]]$guide=FALSE



grid.arrange(p12, p10, p6, p5,p9,p8 ,
             ncol=2,
             top=""
)







## write plot to png  ############################
library(Cairo)
Cairo(file="prediction_stats.png", 
      type="png",
      units="in", 
      width=12, 
      height=15, 
      pointsize=12, 
      dpi=300,
      bg="white")



grid.arrange(p12, p10, p6, p5,p9,p8 ,
             ncol=2,
             top=""
)
dev.off()




## write plot to eps  ############################
cairo_ps("prediction_stats.eps",width=12, height=15, pointsize=12,bg="white")
grid.arrange(p12, p10, p6, p5,p9,p8 ,
             ncol=2,
             top=""
)
dev.off()
