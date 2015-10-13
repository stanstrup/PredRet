library(rcdk)
library(PredRetR)
library(chemhelper)
library(plyr)
library(pracma)
library(obabel2R)


fp.sim.matrix_partial <- function(fps, f, method='tanimoto'){
  require(rcdk)
  
  fps_split <- split(fps,f=f)
  fp.sim <- matrix(as.numeric(NA),length(fps),length(fps))
  fp.sim_partial <- lapply(fps_split,function(x)  fp.sim.matrix(x, method=method))

  
  for(  i in 1:length(fp.sim_partial)   ){
    idx <- which(names(fp.sim_partial)[i] == f)
    fp.sim[idx,idx] <- fp.sim_partial[[i]]
  }
  
  return(fp.sim)
}








# get the database
db <- PredRet_get_db(exp_pred        = c("exp","pred"), 
                     include_suspect = TRUE
                    )

# convert inchi to smiles
db <- mutate(db, smiles = inchi2smile(db$inchi,verbose=F))

# make rcdk molecules
mols <- parse.smiles(db$smiles)

# make fingerprints for all molecules
fps <- lapply(mols, get.fingerprint, type='extended')

# doesn't work for some reason
# cl <- makeCluster(detectCores())
# fps <- parLapply(cl,mols, function(x){require(rcdk); return(get.fingerprint(x,type='extended'))}   )
# stopCluster(cl)



# calculate similarity matrix
#fp.sim <- fp.sim.matrix(fps, method='tanimoto')
fp.sim <- fp.sim.matrix_partial(fps=fps, f=db$system, method='tanimoto')



db_support <- lapply(1:nrow(db), function(i){
  
    select <- which(db$system[i]==db$system) # get all other molecules from same system
    select <- select[ -which(db$inchi[i] == db$inchi[select])] # remove itself
    
    data.frame(RT = db$recorded_rt[select], sim = fp.sim[select,i])
    
  })


# RT weighted by similary
db$RT_pred_sim_weight <- sapply(db_support,function(x){
  w <- sigmoid(x$sim, a = 50, b = 0.95)
  weighted.mean(x$RT, w,na.rm =T)
  })


# choosing just the RT from the most similar compound
db$RT_pred_closest <- sapply(db_support,function(x){
  x$RT[which.max(x$sim)]
})



db <- mutate(db, sim_weight_error_abs =  abs(RT_pred_sim_weight-recorded_rt) ,  sim_weight_error_rel =   abs(RT_pred_sim_weight-recorded_rt)/recorded_rt, closest_error_rel =   abs(RT_pred_closest-recorded_rt)/recorded_rt  )


stats <- 
  ddply(db, .(system), summarise, 
        N                        = sum(!is.na(RT_pred_sim_weight)),
        sim_weight_error_abs         = median(    sim_weight_error_abs   ,na.rm = T ),
        sim_weight_error_rel         = median(    sim_weight_error_rel     ,na.rm = T),
        closest_error_rel        = median(    closest_error_rel     ,na.rm = T)
  )
        


plot(stats$sim_weight_error_rel,stats$closest_error_rel)
abline(a=0,b=1)








# testing of relationship between w and error
db$w_sum <- sapply(db_support,function(x){
  sum(sigmoid(x$sim, a = 50, b = 0.95))
})


plot(db$w_sum,db$sim_weight_error_rel,pch=20)




# using descriptors as weights

desc <- c("org.openscience.cdk.qsar.descriptors.molecular.XLogPDescriptor",
          "org.openscience.cdk.qsar.descriptors.molecular.TPSADescriptor",
          "org.openscience.cdk.qsar.descriptors.molecular.MannholdLogPDescriptor",
          "org.openscience.cdk.qsar.descriptors.molecular.APolDescriptor",                     
          "org.openscience.cdk.qsar.descriptors.molecular.ALOGPDescriptor"
         )


desc_out <- eval.desc(mols, desc)
plot(desc_out)
db <- cbind.data.frame(db,desc_out)


desc_range <- diff(range(db$MLogP))
desc_mat <- outer(db$MLogP,db$MLogP, function(x,y) desc_range/abs(x-y)   )

desc_mat[is.infinite(desc_mat)] <- NA



# RT weighted by LogP
for(i in 1:nrow(db)){
 
  select <- which(db$system[i]==db$system) # get all other molecules from same system
  select <- select[ -which(db$inchi[i] == db$inchi[select])] # remove itself
  
  w <- desc_mat[select,i]
   w <- sigmoid(w, a = 10, b = 50)
db$RT_pred_desc_weight[i] <- weighted.mean(db$recorded_rt[select], w,na.rm =T)
  
}



  ddply(db, .(system), summarise, 
        error_rel        = median(     abs(RT_pred_desc_weight-recorded_rt)/recorded_rt     ,na.rm = T)
  )

