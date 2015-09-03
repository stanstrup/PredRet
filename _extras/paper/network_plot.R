#devtools::install_github("stanstrup/predret_shiny",subdir="PredRetR")
library(PredRetR)
library(igraph)

setwd(paste0(getwd(),"/_extras/paper"))


# some functions we need
addVertIfNotPresent <- function(g, ...){
  names2add <- setdiff(list(...),V(g)$name)
  v2add <- do.call(vertices,names2add)
  g <- g + v2add
}


normalize01 <- function(v){
  m <- min(v)
  range <- max(v) - m
  v <- (v - m) / range
  return(v)
}

  
normalize_range <- function(v, x, y){
  # Normalize to [0, 1]:
  v <- normalize01(v)
  
  # Then scale to [x,y]:
  range2 <- y - x
  normalized <- (v*range2) + x
  
  return(normalized)
}





# Pull data from PredRet database. Takes a while
models <- PredRet_get_models()
database <- PredRet_get_db(exp_pred = "exp")
sys_db <- PredRet_get_chrom_systems()


# Make edges
d <- data.frame(from  =   sapply(models,function(x) x$predict_from)    ,
                to    =   sapply(models,function(x) x$predict_to)      ,
                common=   sapply(models,function(x) x$stats["n_points"])
                )



# Attempt to do better ordering manually
# d$from <- factor(d$from,levels= c("RIKEN","MTBLS20","LIFE_new","LIFE_old","IPB_Halle","MTBLS87","Cao_HILIC" ,"Eawag_XBridgeC18","UFZ_Phenomenex","UniToyama_Atlantis",
#                                   "FEM_orbitrap_urine","FEM_lipids","FEM_orbitrap_plasma","FEM_short","FEM_long","MPI_Symmetry",
#                                   "MTBLS39",
#                                   "MTBLS38","MTBLS36"
#                                   
#                                   )
#                 )

#d <- d[order(as.numeric(d$from)),]



d <- d[order(d$common,decreasing = T),]
d$from <- factor(d$from,levels=unique(as.vector(as.matrix(t(d[,c("from","to")])))))
d <- d[order(as.numeric(d$from)),]



# Make node sizes
temp <- as.matrix(table(database$system))
N <- as.numeric(temp)
names(N) <- rownames(temp)
rm(temp)


# create graph
g <- graph.data.frame(d, directed=T)
connected_v <- length((V(g)))
g <- addVertIfNotPresent(g,names(N)[!(names(N) %in% V(g)$name)]) 



# Set node size
order <- match(V(g)$name,names(N))
V(g)$size <- normalize_range(     log(as.numeric(N)[order])     ,10,30)



# edge width  
E(g)$width <- normalize_range(     log(d$common)     ,3,20)
E(g)$arrow.width <-  0
E(g)$arrow.size  <-  0


# edge color scale
color <- normalize01(rank(log(d$common)))
color <- c(-.4,color)
color <- normalize01(color)
color <- colorRamp(c("yellow","pink","blue"))(   color     )
color <- color[-1,]
color <- apply(color,1,function(x) rgb(x[1],x[2],x[3],maxColorValue=255))
E(g)$color <- color



# mark hilic
system_column_type <- data.frame(system_name        = sapply(sys_db,function(x) x$system_name),
                                 system_column_type = sapply(sys_db,function(x) x$system_column_type),stringsAsFactors = F)
  
  to_mark <- subset(system_column_type,system_column_type=="HILIC")[,"system_name"]
to_mark <- match(to_mark,names(V(g)))





# Plot all
plot(g,vertex.color="white",
     layout=layout.circle,
     margin=c(-0.32,-0.82,-0.22,-0.67), # below, left, over, right
     mark.groups=list(to_mark), mark.col="#C5E5E7", mark.border=NA
     )


plot(g,
     vertex.color="white",
     layout=layout.fruchterman.reingold(g, niter=10000),
     margin=c(-0.32,-0.82,-0.22,-0.67),
     mark.groups=list(to_mark), mark.col="#C5E5E7", mark.border=NA)



# Plot only connected
g2 <- delete.vertices(g,   connected_v + 1:(length(V(g))-connected_v)    )


plot(g2,vertex.color="white",
     layout=layout.circle,
     margin=c(-0.3,-0.75,-0.2,-0.62),
     mark.groups=list(to_mark), mark.col="#C5E5E7", mark.border=NA)


plot(g2,vertex.color="white",
     layout=layout.fruchterman.reingold(g2, niter=10000),
     margin=c(-0.32,-0.82,-0.22,-0.67),
     mark.groups=list(to_mark), mark.col="#C5E5E7", mark.border=NA)
     )



legend(x=-1.3, y=-0.8, c("HILIC"), pch=21,
       col="#777777", pt.bg="#C5E5E7", pt.cex=3, cex=1.5, bty="n", ncol=1)







# write pdf
pdf("network_circular_connected.pdf", fonts=c("serif", "Palatino"),width=1000/72,height=1000/72, useDingbats=FALSE)
plot(g2,vertex.color="white",margin=c(-0.3,-0.75,-0.2,-0.62),layout=layout.circle)
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



