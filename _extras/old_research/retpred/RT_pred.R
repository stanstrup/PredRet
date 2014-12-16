## Libraries ############################################
options(java.parameters = "-Xmx4g" )
library(monoProc)
library(XLConnect)
library(bisoreg) # use it to uptimize span automatically. though it always seems to give lowest possible span. Maybe make something myself that uses highest "good" span
# bisoreg has monotonic fit too and automatic optimization of some parameters. But I cannot make it work. I think this is a global method. not a local one
library(ChemmineR)
library(rpubchem)
library(limma)




## working directory ############################################
#setwd('/media/jan/storage/FEM/RT mapping project/predictor_dev')



## Load data ############################################
## Data should have the following columns
## c("system_name","name","rt","inchi")


# FEM data
source("load_FEM_data.R")

# cph data
source("load_LIFE_data.R")

# Merge data
data=rbind(cph_data,FEM_data)
rm(cph_data,FEM_data)





## Keep only largest continues part of molecule (that is remove salts) ############################################
#Look for Na salts
#select = grep('Na',data[,'inchi'])

# plot a structure
#sdf=inchi2sdf(data[,'inchi'])
#plotStruc(sdf[[select[1]]])


# Do the fixing
data[,'inchi'] = inchi.keep.cont(data[,'inchi'])


#rm(sdf)



## remove stereochemistry and charges ############################################
data[,'inchi'] = inchi.rem.stereo(data[,'inchi'])

data[,'inchi'] = inchi.rem.charges(data[,'inchi'])





# get average of rt for  compounds with unique inchi id but per system.
data_list=split(data,data[,'system_name'])


for (i in 1:length(data_list)){
  inchi_unique = unique(data_list[[i]][,'inchi'])
  
  
  for (i2 in 1:length(inchi_unique)){
    select = inchi_unique[i2]==data_list[[i]][,'inchi']
    rt_avg = data_list[[i]][ select  ,'rt']
    rt_avg = mean(as.numeric(rt_avg),na.rm=T)
    
    data_list[[i]][ select  ,'rt'] =rt_avg  
  }
  
}


data = do.call(rbind,data_list)
rm(i,i2,inchi_unique,rt_avg,select,data_list)





# remove rows with same inchi. but per system.
data_list=split(data,data[,'system_name'])


for (i in 1:length(data_list)){
  data_list[[i]] = data_list[[i]][       !duplicated(data_list[[i]][,'inchi'])      ,     ]
}

data = do.call(rbind,data_list)
rm(data_list,i)











## remove entries with NaN retention times ############################################
data[,"rt"] = as.numeric(data[,"rt"])
data = data [        !is.na(data[,"rt"])     ,]




## Find matching compounds and make Venn "diagram" of system overlap ############################################
# setup comparision matrix
unique_inchi = unique(data[,'inchi'])
unique_systems = unique(data[,'system_name'])
unique_names = data[!duplicated(data[,'inchi']),'name']

inchi_matrix = matrix(nrow=length(unique_inchi),ncol=length(unique_systems))
colnames(inchi_matrix) = unique_systems
rownames(inchi_matrix) = unique_names


rt_matrix = matrix(nrow=length(unique_inchi),ncol=length(unique_systems))
colnames(rt_matrix) = unique_systems
rownames(rt_matrix) = unique_names


for (i in 1:length(unique_inchi)){
  for (i2 in 1:length(unique_systems)){
    
    select = unique_inchi[i] == data[,'inchi']        &     unique_systems[i2] == data[,'system_name']
    
    if (any(select)){  
      inchi_matrix[i,i2]=1   
      rt_matrix[i,i2]=data[select,'rt']
    }else{   
      inchi_matrix[i,i2]=0    
      rt_matrix[i,i2]=NA
    }
    
    
  }
}

rm(unique_inchi,unique_systems,unique_names,i,i2,select)



inchi_comparison = vennCounts(inchi_matrix)
vennDiagram(inchi_comparison)


rm(inchi_matrix,inchi_comparison)





## Make normal and monotonic fit of all combinations ############################################
unique_system_names = unique(data[,"system_name"])
comb_systems=expand.grid(unique_system_names,unique_system_names,stringsAsFactors=F)
comb_systems = comb_systems[        !(comb_systems[,1]==comb_systems[,2])            ,]



for (i in 1:nrow(comb_systems)){
  rt1  =  rt_matrix[,comb_systems[i,1]]
  rt2  =  rt_matrix[,comb_systems[i,2]]
  
  common = (!is.na(rt1) & !is.na(rt2))
  
  if(sum(common)<10){next}
  
  rt1  =  rt1[common]
  rt2  =  rt2[common]
  
  fit=loess.wrapper(rt1, rt2, span.vals = seq(0.2, 1, by = 0.05), folds = 10)
  fit_mono = monoproc(fit, bandwidth = 0.0001, mono1 = "increasing", gridsize=1000)
  
  
  png(file = paste('plots/ret_relationships/',comb_systems[i,1],'_vs_',comb_systems[i,2],'.png',sep='')     ,width = 800*3, height = 600*3,pointsize=12,res=300)
  plot(rt1,rt2,main='Retention time relationship between systems',xlab=comb_systems[i,1],ylab=comb_systems[i,2],pch=20)
  lines(fit_mono@fit@x,fit_mono@fit@y,col=2)
  dev.off()
  
  
  setEPS()
  postscript(paste('plots/ret_relationships/',comb_systems[i,1],'_vs_',comb_systems[i,2],'.eps',sep='')     ,width = 800/100, height = 600/100)
  plot(rt1,rt2,main='Retention time relationship between systems',xlab=comb_systems[i,1],ylab=comb_systems[i,2],pch=20)
  lines(fit_mono@fit@x,fit_mono@fit@y,col=2)
  dev.off()
  
}










## Plot the fits ############################################

png(file = "chrom_fit.png",width = 800*3, height = 600*3,pointsize=12,res=300)

plot(ret_old,ret_new,main='Retention time conversion between systems',xlab="Rt (old)",ylab="Rt (new)",pch=20)
lines(fit_mono@fit@x,fit_mono@fit@y,col=2)

dev.off()



setEPS()
postscript("chrom_fit.eps",width = 800/100, height = 600/100)

plot(ret_old,ret_new,main='Retention time conversion between systems',xlab="Rt (old)",ylab="Rt (new)",pch=20)
lines(fit_mono@fit@x,fit_mono@fit@y,col=2)

dev.off()

#rm(ret_new_pred)






## plot deviation ############################################

setEPS()
postscript("chrom_fit_error.eps",width = 800/100, height = 600/100)

fit_mono_pred = monoproc(fit, bandwidth = 0.0001, mono1 = "increasing", gridsize=1000,xx=ret_old)
pred_y    = fit_mono_pred@fit@y

plot_y_lim=ceiling(max(abs(ret_new-pred_y))*2) / 2

plot(ret_new,ret_new-pred_y, main="Deviation",pch=20,,xlab="Rt (new)",ylab="Error (min)",ylim=c(-plot_y_lim,plot_y_lim))
abline(h = seq(from=-2,to=2,by=0.5),col='gray')
abline(h = 0)
abline(            h=mean(abs(ret_new-pred_y))                            ,col='red')
abline(            h=median(abs(ret_new-pred_y))                          ,col='blue')
abline(            h=quantile(abs(ret_new-pred_y),0.95)                   ,col='darkgreen')


legend('topright',legend=c('Mean','Median','95% percentile'),col=c('red','blue','darkgreen'),lty = 1)

dev.off()






png(file = "chrom_fit_error.png",width = 800*3, height = 600*3,pointsize=12,res=300)


fit_mono_pred = monoproc(fit, bandwidth = 0.0001, mono1 = "increasing", gridsize=1000,xx=ret_old)
pred_y    = fit_mono_pred@fit@y

plot_y_lim=ceiling(max(abs(ret_new-pred_y))*2) / 2

plot(ret_new,ret_new-pred_y, main="Deviation",pch=20,,xlab="Rt (new)",ylab="Error (min)",ylim=c(-plot_y_lim,plot_y_lim))
abline(h = seq(from=-2,to=2,by=0.5),col='gray')
abline(h = 0)
abline(            h=mean(abs(ret_new-pred_y))                            ,col='red')
abline(            h=median(abs(ret_new-pred_y))                          ,col='blue')
abline(            h=quantile(abs(ret_new-pred_y),0.95)                   ,col='darkgreen')


legend('topright',legend=c('Mean','Median','95% percentile'),col=c('red','blue','darkgreen'),lty = 1)



dev.off()






rm(fit_mono_pred,plot_y_lim,pred_y)






## Get all values from old system and predict, POS mode ############################################

wb        = loadWorkbook("FEM_data/standard_RP_pos_20130121.xls", create = FALSE)
pred_x   = readWorksheet(wb, sheet = "ESI_pos", startRow = 2, endRow = 520, startCol = 12, endCol = 12)
pred_x=as.numeric(as.matrix(pred_x))


#Prediction need to be done in a loop because NA is not supported by monoproc
pred_y=c()

for (i in 1:length(pred_x)){
  
  if (is.na(pred_x[i])){
    pred_y[i]=NA
    
  }else{
    fit_mono_pred = monoproc(fit, bandwidth = 0.0001, mono1 = "increasing", gridsize=1000,xx=pred_x[i])
    pred_y[i]    = fit_mono_pred@fit@y
  }
}

pred_y=as.data.frame(pred_y)
names(pred_y)='RT pred, new system'

rm(fit_mono_pred,i)


# write back the result
writeWorksheet(wb,pred_y,'ESI_pos',2,19,header=TRUE)
saveWorkbook(wb)








## Get all values from old system and predict, NEG mode ############################################

wb        = loadWorkbook("FEM_data/standard_RP_neg_20130121.xls", create = FALSE)
pred_x   = readWorksheet(wb, sheet = "ESI_neg", startRow = 2, endRow = 521, startCol = 12, endCol = 12)
pred_x=as.numeric(as.matrix(pred_x))


#Prediction need to be done in a loop because NA is not supported by monoproc
pred_y=c()

for (i in 1:length(pred_x)){
  
  if (is.na(pred_x[i])){
    pred_y[i]=NA
    
  }else{
    fit_mono_pred = monoproc(fit, bandwidth = 0.0001, mono1 = "increasing", gridsize=1000,xx=pred_x[i])
    pred_y[i]    = fit_mono_pred@fit@y
  }
}

pred_y=as.data.frame(pred_y)
names(pred_y)='RT pred, new system'

rm(fit_mono_pred,i)


# write back the result
writeWorksheet(wb,pred_y,'ESI_neg',2,19,header=TRUE)
saveWorkbook(wb)
