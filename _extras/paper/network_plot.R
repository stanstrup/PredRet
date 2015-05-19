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


# Make edges
d <- data.frame(from  =   sapply(models,function(x) x$predict_from)    ,
                to    =   sapply(models,function(x) x$predict_to)      ,
                common=   sapply(models,function(x) x$stats["n_points"])
                )


# Attempt to do better ordering
d$from <- factor(d$from,levels= c("RIKEN","FEM_long","FEM_short","FEM_orbitrap_plasma","FEM_lipids","FEM_orbitrap_urine",
                                  "MTBLS36","MTBLS38","MTBLS39","LIFE_old","LIFE_new","MTBLS20" )
                )

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
#V(g)$size2 <- normalize_range(     log(as.numeric(N)[order])     ,10,30) *2



# edge width  
E(g)$width <- normalize_range(     log(d$common)     ,3,25)
#E(g)$arrow.width <-  log(d$common) /4
E(g)$arrow.width <-  0


# edge color scale
color <- normalize01(log(d$common))
color <- colorRamp(c("green","red"))(   color     )
color <- apply(color,1,function(x) rgb(x[1],x[2],x[3],maxColorValue=255))
E(g)$color <- color


# Plot all
plot(g,vertex.color="white",layout=layout.circle,margin=c(-0.32,-0.82,-0.22,-0.67)) # below, left, over, right
plot(g,vertex.color="white",margin=c(-0.32,-0.82,-0.22,-0.67),layout=layout.fruchterman.reingold(g, niter=10000, area=5000*vcount(g)^2))

# Plot only connected
g2 <- delete.vertices(g,   connected_v + 1:(length(V(g))-connected_v)    )
plot(g2,vertex.color="white",margin=c(-0.3,-0.75,-0.2,-0.62),layout=layout.circle)
plot(g2,vertex.color="white",margin=c(-0.32,-0.82,-0.22,-0.67),layout=layout.fruchterman.reingold(g2, niter=10000, area=5000*vcount(g2)^2))



# write pdf
pdf("network_circular_connected.pdf", fonts=c("serif", "Palatino"),width=1000/72,height=1000/72, useDingbats=FALSE)
plot(g2,vertex.color="white",margin=c(-0.3,-0.75,-0.2,-0.62),layout=layout.circle)
dev.off()

