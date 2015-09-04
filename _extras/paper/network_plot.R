library(PredRetR)
library(Rplot.extra)
library(igraph)



# Pull data from PredRet database. Takes a while ###################
database <- PredRet_get_db(exp_pred = "exp")
models <- PredRet_get_models()
sys_db <- PredRet_get_chrom_systems()



# make the plot ###################
g <- PredRet_plot.db.graph(database          = database,
                              models         = models,
                              sys_db         = sys_db,
                              circular       = TRUE,
                              only_connected = TRUE
)








# write pdf  ###################
pdf("network_circular_connected.pdf", fonts=c("serif", "Palatino"),width=1000/72,height=1000/72, useDingbats=FALSE)

g <- PredRet_plot.db.graph(database          = database,
                           models         = models,
                           sys_db         = sys_db,
                           circular       = TRUE,
                           only_connected = TRUE
)

dev.off()




# Make a separate gradient for the plot ###################
gradient <- cbind.data.frame(scale = g$d$common,color = E(g$g)$color,stringsAsFactors=FALSE)
gradient <- gradient[order(gradient$scale),]
gradient <- unique(gradient)


gradient <- interpolate_gradient(gradient)


pdf("gradient.pdf", fonts=c("serif", "Palatino"),width=500/72,height=200/72, useDingbats=FALSE,pointsize = 8)
plot(gradient$scale,seq(1,2,length.out = length(gradient$scale)),type="n",log="x",axes=FALSE,ylab="",xlab="")
rect(xleft = gradient$scale-0.505 ,  xright= gradient$scale+0.505, ybottom=1, ytop =2,col=gradient$color,lwd=0,border=NA)
labels = seq(min(gradient$scale),max(gradient$scale),10)
axis(1,at=labels,labels= labels      )
dev.off()




