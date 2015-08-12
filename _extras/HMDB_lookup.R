HMdata <- readRDS("/media/jan/storage/FEM/Other_projects/Luca/database/HMdata.RData")


data <- PredRet_get_data()



data$origin <- c()

for(i in 1:nrow(data)){
  
  temp <- HMdata$origin[data$inchi[i]==HMdata$inchi]
  
  if(length(temp) == 1)   data$origin[i] <- as.character(temp)
  
}




data$pka_strongest_acidic <- c()
for(i in 1:nrow(data)){
  temp <- HMdata$pka_strongest_acidic[data$inchi[i]==HMdata$inchi]
  if(length(temp) == 1)   data$pka_strongest_acidic[i] <- as.numeric(temp)
}


data$pka_strongest_basic <- c()
for(i in 1:nrow(data)){
  temp <- HMdata$pka_strongest_basic[data$inchi[i]==HMdata$inchi]
  if(length(temp) == 1)   data$pka_strongest_basic[i] <- as.numeric(temp)
}

data$logP <- c()
for(i in 1:nrow(data)){
  temp <- HMdata$logP[data$inchi[i]==HMdata$inchi]
  if(length(temp) == 1)   data$logP[i] <- as.numeric(temp)
}





plot(density(data$pka_strongest_acidic,na.rm = TRUE),ylim=c(0,0.08),panel.first=grid(), xaxt = "n")
axis(side = 1, at = -5:20)



plot(density(data$pka_strongest_basic,na.rm = TRUE),panel.first=grid(), xaxt = "n")
axis(side = 1, at = -20:20)



plot(density(data$logP,na.rm = TRUE),ylim=c(0,0.08),panel.first=grid(), xaxt = "n")
axis(side = 1, at = -5:20)















## attempt with JChem ##################
library(chemhelper)
SDF <- inchi2sdf(data$inchi)
write.SDF(SDF, file="PredRet.SDF")



jchemdir = "/home/jan/ChemAxon/JChem/bin"
command=paste("\"",jchemdir,'/cxcalc',"\"",' -S -o ',"\"",'test.sdf',"\"",' ',"\"",'PredRet.SDF',"\"",' logD -H ',"2.68",' ','logP'   ,sep="")
temp = system(command, intern = TRUE)




SDF_properties <- read.SDFset("test.sdf")


# fix logD
temp <- as.numeric(sapply(SDF_properties@SDF,function(x) strsplit(x@datablock["LOGD"],"\t")[[1]][2]))

for(i in 1:length(temp)){
SDF_properties@SDF[[i]]@datablock["LOGD"] <- temp[i]
}









## obabel solution ##################
data <- PredRet_get_db()

library(chemhelper)
library(ChemmineR)
SDF <- inchi2sdf(data$inchi)
write.SDF(SDF, file="PredRet.SDF")



command=paste("obabel -p 2.68 --gen3D -isdf PredRet.SDF -osdf -O PredRet_charged.SDF",sep="")
temp = system(command, intern = TRUE)


SDF_charged <- read.SDFset("PredRet_charged.SDF")
data$charges <- sapply(bonds(SDF_charged, type="charge"), length) # length here counts how many atoms are charged. change to sum to get the overall charge
data$charged <- data$charges != 0 



## Stats on predictions where RT is known
data$error_abs <- with(data, abs(predicted_rt-recorded_rt))
data$error_rel <- with(data, abs((predicted_rt-recorded_rt)/recorded_rt)      )
data$ci_width_abs <- with(data, ci_upper-ci_lower    )
data$ci_width_rel <- with(data, (ci_upper-ci_lower)/predicted_rt    )
data$select <- !is.na(data$predicted_rt) & !is.na(data$recorded_rt)
data$system <- as.factor(data$system)





## Violin plot relative prediction error ############################
plotdata <- data[data$select,c("system","error_rel","charged")]
plotdata[,"error_rel"] <- plotdata[,"error_rel"]*100


violin_width=0.8

p5 <- ggplot( plotdata, aes( x = interaction(charged,system), y = error_rel ) )
p5 <- p5 + scale_x_discrete(breaks=levels(interaction(plotdata$charged,plotdata$system)), drop=FALSE)
p5 <- p5 + scale_y_continuous(breaks = seq(0, 100, 2))
p5 <- p5 + geom_violin(trim=TRUE, fill='black', color="black",adjust=0.3,scale="width",size=0,width=violin_width)
p5 <- p5 + labs(title="Relative prediction errors",x="Chromatographic systems", y="Error (%)",fill="Quartiles")
p5 <- p5 + plottheme
p5 <- p5 + geom_violin_quantile_fill(p=p5,df_gr = interaction(plotdata$charged,plotdata$system),df_data = plotdata[,"error_rel"],width=violin_width)
p5 <- p5 + scale_fill_manual(values = c("lightgrey","darkgrey","darkgrey","lightgrey"),labels = c("25", "50", "75","100"))
# p5 <- p5 + scale_fill_brewer(palette="Reds",        name="Quantile\n",   labels=c("25","50","75","100")    )

p5 <- p5 + geom_boxplot( outlier.size = 1,width=0, size=0) 
p5 <- p5 + stat_summary(fun.y=median, geom="point", shape=23, size=2, color="black",fill="black")
#p5 <- p5 + stat_summary(fun.y=mean, geom="point", shape=16, size=2, color="black",fill="black")

p5 <- p5 + theme(legend.position=c(1,1),legend.justification=c(1,1),
                 legend.direction="horizontal",
                 legend.box="horizontal",
                 legend.box.just = c("top"),
                 legend.background = element_rect(fill="transparent"))


plot(p5)

