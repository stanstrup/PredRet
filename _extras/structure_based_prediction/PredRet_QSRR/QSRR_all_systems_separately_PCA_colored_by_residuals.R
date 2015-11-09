library(Rplot.extra)

preds     <- predict(models[[1]],newdata = camb_dataset[[1]]$x.holdout)
error_rel <- abs(preds-camb_dataset[[1]]$y.holdout)/camb_dataset[[1]]$y.holdout

colors <- rgb(colorRamp(c("blue", "red"))( error_rel/max(error_rel) ) , maxColorValue=256)


p <- PCA_biplot(data=camb_dataset[[1]]$x.holdout,
           # The PCA calculation
           pcs = c(1,2),
           scale = "uv",
           method="nipals",
           
           # Plotting
           foreground="scores",
           
           fg_geom = "dots",
           fg_color= colors,
           fg_class = colors,
           fg_labels = camb_dataset[[1]]$ids[camb_dataset[[1]]$holdout.indexes],
           
           bg_geom = "lines",
           bg_color = "grey",
           bg_class = "descriptors",
           bg_labels = colnames(camb_dataset[[1]]$x.holdout),
           
           # Fine tune plot
           loading_quantile = 0.99,
           text_offset=0.005
)

p


# thiamine is very wrong since the removal of charges creates a radical.



# Investigate effect of number of nitrogens -------------------------------
nN <- camb_dataset[[1]]$x.holdout %>% select(desc_nN) %>% as.matrix %>% as.vector

dens_mat <- lapply(0:max(nN),function(x) {
  #temp <- density(error_rel[nN==x],bw = 0.01,from=0)
  #data.frame(nN=x,x=temp$x,y=temp$y)
  temp <- hist(error_rel[nN==x],breaks=seq(from=0,to=ceiling(max(error_rel)),by=0.02))
  data.frame(nN=x,x=temp$mids,y=temp$counts)
  
  })

dens_mat <- do.call(rbind,dens_mat)



ggplot( dens_mat, aes( x = x, y = y ,fill=as.factor(nN)) ) + geom_area(size=1) + facet_wrap(~nN,ncol=1,scales="free_y")
table(nN)


# Cannot do this with less than 3 points in some
#ggplot( data.frame(x=nN,y=error_rel), aes( x = x )) + geom_density(size=1) + facet_wrap(~y,ncol=1,scales="free_y")


