# Setup the output matrix  ############################################

FEM_data = as.data.frame(matrix(nrow=0,ncol=4))
colnames(FEM_data)= c("system_name","name","rt","inchi")







# First the matched data  ############################################
#POS
wb        = XLConnect:::loadWorkbook("FEM_data/standard_RS_pos_20130701.xls", create = FALSE)
ret_old_pos   = readWorksheet(wb, sheet = "ESI_pos", startRow = 2, endRow = 66, startCol = 12, endCol = 12,autofitRow=F)
ret_new_pos   = readWorksheet(wb, sheet = "ESI_pos", startRow = 2, endRow = 66, startCol = 13, endCol = 13,autofitRow=F)
names   = readWorksheet(wb, sheet = "ESI_pos", startRow = 2, endRow = 66, startCol = 4, endCol = 4,autofitRow=F)
inchi=   readWorksheet(wb, sheet = "ESI_pos", startRow = 2, endRow = 66, startCol = 3, endCol = 3,autofitRow=F)

ret_old_pos     = as.numeric(as.matrix(ret_old_pos))
ret_new_pos   = as.numeric(as.matrix(ret_new_pos))

temp_old=data.frame(system_name='FEM_long',name=names,rt=ret_old_pos,inchi=inchi)
temp_new=data.frame(system_name='FEM_short',name=names,rt=ret_new_pos,inchi=inchi)
FEM_data=rbind(FEM_data,temp_old,temp_new)
rm(wb,temp_old,temp_new,ret_old_pos,ret_new_pos,names,inchi)



#NEG
wb        = XLConnect:::loadWorkbook("FEM_data/standard_RS_neg_20130701.xls", create = FALSE)
ret_old_neg   = readWorksheet(wb, sheet = "ESI_neg", startRow = 2, endRow = 62, startCol = 12, endCol = 12,autofitRow=F)
ret_new_neg   = readWorksheet(wb, sheet = "ESI_neg", startRow = 2, endRow = 62, startCol = 13, endCol = 13,autofitRow=F)
names   = readWorksheet(wb, sheet = "ESI_neg", startRow = 2, endRow = 62, startCol = 4, endCol = 4,autofitRow=F)
inchi=   readWorksheet(wb, sheet = "ESI_neg", startRow = 2, endRow = 62, startCol = 3, endCol = 3,autofitRow=F)

ret_old_neg     = as.numeric(as.matrix(ret_old_neg))
ret_new_neg   = as.numeric(as.matrix(ret_new_neg))

temp_old=data.frame(system_name='FEM_long',name=names,rt=ret_old_neg,inchi=inchi)
temp_new=data.frame(system_name='FEM_short',name=names,rt=ret_new_neg,inchi=inchi)
FEM_data=rbind(FEM_data,temp_old,temp_new)
rm(wb,temp_old,temp_new,ret_old_neg,ret_new_neg,names,inchi)



# Extra polar compounds

# NEG
wb        = XLConnect:::loadWorkbook("FEM_data/extra_polars_metabolites.xls", create = FALSE)
ret_extra_new_neg   = readWorksheet(wb, sheet = "neg", startRow = 2, endRow = 11, startCol = 12, endCol = 12,autofitRow=F)
names   = readWorksheet(wb, sheet = "neg", startRow = 2, endRow = 11, startCol = 4, endCol = 4,autofitRow=F)
inchi=   readWorksheet(wb, sheet = "neg", startRow = 2, endRow = 11, startCol = 3, endCol = 3,autofitRow=F)

ret_extra_new_neg   = as.numeric(as.matrix(ret_extra_new_neg))

temp_new=data.frame(system_name='FEM_short',name=names,rt=ret_extra_new_neg,inchi=inchi)
FEM_data=rbind(FEM_data,temp_new)
rm(temp_new,ret_extra_new_neg,names,inchi)



# POS
ret_extra_new_pos   = readWorksheet(wb, sheet = "pos", startRow = 1, endRow = 14, startCol = 12, endCol = 12,autofitRow=F)
names   = readWorksheet(wb, sheet = "pos", startRow = 1, endRow = 14, startCol = 4, endCol = 4,autofitRow=F)
inchi=   readWorksheet(wb, sheet = "pos", startRow = 1, endRow = 14, startCol = 3, endCol = 3,autofitRow=F)

ret_extra_new_pos   = as.numeric(as.matrix(ret_extra_new_pos))

temp_new=data.frame(system_name='FEM_short',name=names,rt=ret_extra_new_pos,inchi=inchi)
FEM_data=rbind(FEM_data,temp_new)
rm(wb,temp_new,ret_extra_new_pos,names,inchi)








# Now the long list of compounds in the old system  ############################################

#POS
wb        = XLConnect:::loadWorkbook("FEM_data/standard_RP_pos_20130121.xls", create = FALSE)
ret   = readWorksheet(wb, sheet = "ESI_pos", startRow = 2, endRow = 520, startCol = 12, endCol = 12,autofitRow=F)
names   = readWorksheet(wb, sheet = "ESI_pos", startRow = 2, endRow = 520, startCol = 4, endCol = 4,autofitRow=F)
inchi=   readWorksheet(wb, sheet = "ESI_pos", startRow = 2, endRow = 520, startCol = 3, endCol = 3,autofitRow=F)

ret     = as.numeric(as.matrix(ret))


temp=data.frame(system_name='FEM_long',name=names,rt=ret,inchi=inchi)

FEM_data=rbind(FEM_data,temp)
rm(wb,temp,ret,names,inchi)


#NEG
wb        = XLConnect:::loadWorkbook("FEM_data/standard_RP_neg_20130121.xls", create = FALSE)
ret   = readWorksheet(wb, sheet = "ESI_neg", startRow = 2, endRow = 521, startCol = 12, endCol = 12,autofitRow=F)
names   = readWorksheet(wb, sheet = "ESI_neg", startRow = 2, endRow = 521, startCol = 4, endCol = 4,autofitRow=F)
inchi=   readWorksheet(wb, sheet = "ESI_neg", startRow = 2, endRow = 521, startCol = 3, endCol = 3,autofitRow=F)

ret     = as.numeric(as.matrix(ret))


temp=data.frame(system_name='FEM_long',name=names,rt=ret,inchi=inchi)

FEM_data=rbind(FEM_data,temp)
rm(wb,temp,ret,names,inchi)









# delete rows with NA rts   ############################################

temp = !(is.na(FEM_data[,'rt']))
FEM_data = FEM_data[temp,]
rm(temp)



# fix column names ############################################
colnames(FEM_data) = c("system_name","name","rt","inchi")





# get average of rt for  compounds with unique inchi id but per system  ############################################
FEM_data_list=split(FEM_data,FEM_data[,'system_name'])


for (i in 1:length(FEM_data_list)){
  inchi_unique = unique(FEM_data_list[[i]][,'inchi'])
  
  
  for (i2 in 1:length(inchi_unique)){
    select = inchi_unique[i2]==FEM_data_list[[i]][,'inchi']
    rt_avg = FEM_data_list[[i]][ select  ,'rt']
    rt_avg = median(rt_avg)
    
    FEM_data_list[[i]][ select  ,'rt'] =rt_avg  
  }
  
}


FEM_data = do.call(rbind,FEM_data_list)
rm(i,i2,inchi_unique,rt_avg,select,FEM_data_list)





# remove rows with same inchi. but per system.   ############################################
FEM_data_list=split(FEM_data,FEM_data[,'system_name'])


for (i in 1:length(FEM_data_list)){
  FEM_data_list[[i]] = FEM_data_list[[i]][       !duplicated(FEM_data_list[[i]][,'inchi'])      ,     ]
}

FEM_data = do.call(rbind,FEM_data_list)
rm(FEM_data_list,i)

