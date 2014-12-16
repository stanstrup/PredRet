#setwd('/home/jan/git_repositories/retpred')
library(chemhelper)

# Read file
wb        = XLConnect:::loadWorkbook("/media/jan/storage/FEM/RT mapping project/LIFE_process_standards/analysed.xlsx", create = FALSE)

# read data
#neg   = readWorksheet(wb, sheet = "NEG", startRow = 1, endRow = 3894, startCol = 1, endCol = 12,autofitRow=F)
#pos   = readWorksheet(wb, sheet = "POS", startRow = 1, endRow = 5162, startCol = 1, endCol = 12,autofitRow=F)


# merge
#cph_data = rbind(pos,neg)
#rm(neg,pos,wb)



cph_data   = readWorksheet(wb, sheet = "Sheet1", startRow = 1, startCol = 1, endCol = 12,autofitRow=F)
rm(wb)

# extract only data we care about
cph_data=cph_data[,c('Method','Mode','Compound.name','RT','Pubchem','InChI')]
colnames(cph_data)=c('method','mode','name','rt','pubchem','inchi')


# remove rows with no inchi id
temp = grepl('InChI',cph_data[,'inchi'],fixed=T)
cph_data =     cph_data[temp,]
rm(temp)





# get average of rt for  compounds with unique inchi id but per system.
cph_data_list=split(cph_data,cph_data[,'method'])


for (i in 1:length(cph_data_list)){
  inchi_unique = unique(cph_data_list[[i]][,'inchi'])
  
  
  for (i2 in 1:length(inchi_unique)){
    select = inchi_unique[i2]==cph_data_list[[i]][,'inchi']
    rt_avg = cph_data_list[[i]][ select  ,'rt']
    rt_avg = median(as.numeric(rt_avg),na.rm=T)
    
    cph_data_list[[i]][ select  ,'rt'] =rt_avg  
  }

}


cph_data = do.call(rbind,cph_data_list)
rm(i,i2,inchi_unique,rt_avg,select,cph_data_list)





# remove rows with same inchi. but per system.
cph_data_list=split(cph_data,cph_data[,'method'])


for (i in 1:length(cph_data_list)){
  cph_data_list[[i]] = cph_data_list[[i]][       !duplicated(cph_data_list[[i]][,'inchi'])      ,     ]
}

cph_data = do.call(rbind,cph_data_list)
rm(cph_data_list,i)



# Rename columns
colnames(cph_data) = c("system_name","mode","name","rt","pubchem_cid","inchi")


# Give better system name
cph_data[         cph_data[,"system_name"] =="new"           ,"system_name"] = 'LIFE_new'
cph_data[         cph_data[,"system_name"] =="old"           ,"system_name"] = 'LIFE_old'


# Select and order columns
cph_data = cph_data[,c("system_name","name","rt","inchi")]


