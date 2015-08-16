library(rcdk)
library(PredRetR)
library(chemhelper)
library(plyr)
library(pracma)


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



db$RT_pred <- sapply(db_support,function(x){
  w <- sigmoid(x$sim, a = 20, b = 0.75)
  weighted.mean(x$RT, w,na.rm =T)
  })

db <- mutate(db, error_abs =  abs(RT_pred-recorded_rt) ,  error_rel =   abs(RT_pred-recorded_rt)/recorded_rt  )


stats <- 
  ddply(db, .(system), summarise, 
        N                        = sum(!is.na(RT_pred)),
        error_median_abs         = median(    error_abs   ,na.rm = T ),
        error_median_rel         = median(    error_rel     ,na.rm = T)
  )
        


plot(stats_20_075_extended_tanimoto_with_pred$error_median_rel,stats$error_median_rel)
abline(a=0,b=1)