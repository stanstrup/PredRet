library(PredRetR)




# Pull data from PredRet database. Takes a while
database <- PredRet_get_db(exp_pred = "exp")
models <- PredRet_get_models()
sys_db <- PredRet_get_chrom_systems()



# make the plot
d <- PredRet_plot.db.graph(database          = database,
                              models         = models,
                              sys_db         = sys_db,
                              circular       = TRUE,
                              only_connected = TRUE
)








# write pdf
pdf("network_circular_connected.pdf", fonts=c("serif", "Palatino"),width=1000/72,height=1000/72, useDingbats=FALSE)

PredRet_plot.db.graph(database = database,
                      models = models,
                      sys_db = sys_db,
                      circular = TRUE,
                      only_connected = TRUE
)

dev.off()






# Make a gradient for the plot ###################
gradient <- cbind.data.frame(count = d$common,color = E(g)$color,stringsAsFactors=FALSE)
gradient <- gradient[order(gradient$count),]
gradient <- unique(gradient)


col_min <- min(gradient$count)
col_max <- max(gradient$count)

col_range <- col_min:col_max


while(!all(col_range %in% gradient$count)){
  
  missing <- min(col_range[!(col_range %in% gradient$count)])
  missing_lower <- max(gradient$count[gradient$count-missing<0])
  missing_higher <- min(gradient$count[gradient$count-missing>0])
  new_cols <- colorRampPalette(c(gradient$color[gradient$count==missing_lower],gradient$color[gradient$count==missing_higher]))(   (missing_higher-missing_lower)-1     )  
    
  new_count <- missing_lower:missing_higher
  new_count <- new_count[-c(1,length(new_count))]
  
  gradient <- rbind.data.frame(gradient,cbind.data.frame(count = new_count, color = new_cols))
  gradient <- gradient[order(gradient$count),]
}




pdf("gradient.pdf", fonts=c("serif", "Palatino"),width=500/72,height=200/72, useDingbats=FALSE,pointsize = 8)
plot(gradient$count,seq(1,2,length.out = length(gradient$count)),type="n",log="x",axes=FALSE)
rect(xleft = gradient$count-0.505 ,  xright= gradient$count+0.505, ybottom=1, ytop =2,col=gradient$color,lwd=0,border=NA)
labels = seq(min(gradient$count),max(gradient$count),10)
axis(1,at=labels,labels= labels      )
dev.off()



