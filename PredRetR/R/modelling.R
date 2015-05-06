

# loess.fun <- function(in_data,inds,newdata,span){
#   #devtools:::install_github("cran/monoProc")
#   require(monoProc)
#   x.star <- in_data[,1][inds]
#   y.star <- in_data[,2][inds]
#   
#   #inds_saved[[length(inds_saved)+1]] <<- inds
#   #counter[length(counter)+1] <<- counter+1
#   
#   out.star <- loess(y.star ~ x.star, span=span, control = loess.control(surface = "direct")  ) # direct is needed, otherwise it occationally blows up. I assume some border situations.
#   y_pred = monoproc(out.star, bandwidth = 0.1, mono1 = "increasing", gridsize=100,xx= newdata)@fit@y
#   
#   y_pred[y_pred < 0] = 0
#   
#   return(y_pred)
# }






gam.mono.con.fun <- function(in_data,inds,newdata){
  x <- NULL
  
  
  x.star <- in_data[,1][inds]
  y.star <- in_data[,2][inds]
  
  #   inds_saved[[length(inds_saved)+1]] <<- inds
  #   counter[length(counter)+1] <<- length(counter)+1
  
  # We need at least 4 unique x-values to do the fit. So we add a small amount of jitter if it is not the case.
  if(length(unique(x.star))<4){
    x.star <- jitter(x.star,amount=0.01)
  }
  
  dat <- data.frame(x=x.star,y=y.star)
  f.ug <- gam(y~s(x,k=min(length(unique(x.star)),10),bs="tp"),data=dat)
  
  
  w <- f.ug$residuals
  # w  <-  1 -     (abs(w)/max(abs(w)))
  #w  <-  1 -     abs(w)   /  max(  in_data[,2]   )
  # w <- sigmoid(w, a = 50, b = quantile(w,0.05))
  # w <- sigmoid(w, a = 50, b = 0.9)
  
  w <- abs(w)   /  max(  in_data[,2]   )
  w <- sigmoid(w, a = -30, b = 0.1)
  
  
  sm <- smoothCon(s(x,k=min(length(unique(x.star)),10),bs="cr"),dat,knots=NULL)[[1]]
  con <- mono.con(sm$xp);   # get constraints
  
  
  G <- list(X=sm$X,C=matrix(0,0,0),sp=f.ug$sp,p=sm$xp,y=y.star,w=w)
  G$Ain <- con$A
  G$bin <- con$b
  G$S <- sm$S
  G$off <- 0
  
  p <- pcls(G);  # fit spline (using s.p. from unconstrained fit)
  
  
  if(any(is.na(p))){ # some models fail when waited. dunno why
    G$w <- rep(1,length(G$w))
    p <- pcls(G);  # fit spline (using s.p. from unconstrained fit)
  }
  
  
  
  
  fv<-Predict.matrix(sm,data.frame(x=newdata))%*%p
  
  
  y_pred <- as.numeric(fv)
  y_pred[y_pred < 0] = 0
  
  return(y_pred)
  
  
}








boot2ci <- function(loess.boot,alpha = 0.01){
  
  # with the boot.ci function
  temp=list()
  for( i2 in 1:length(loess.boot$t0)){
    temp[[i2]]=list()
    temp[[i2]][[1]]=i2
    temp[[i2]][[2]]=loess.boot
  }
  
  
  # cl <- makeCluster(detectCores()) # multithreaded makes it break on the server. --> seems ok now. But it uses too much memory.
  
  #ci=parLapply(cl,temp,function(x) {
  ci=lapply(temp,function(x) {
    temp2=boot.ci(x[[2]],index=x[[1]],type="perc",conf = 1-alpha)
    
    ci=vector(mode="numeric",length=3)
    ci[1] = temp2$t0
    ci[c(2,3)] = temp2$percent[,c(4,5)]
    return(ci)
  })
  # stopCluster(cl)
  ci = do.call(rbind,ci)
  
  return(ci)
}





boot2ci_PI <- function(loess.boot,newdata,alpha=0.05){
  
  # SD at each  x values (newdata) between all bootstrap iterations
  loess_sd_newdata <- apply(loess.boot$t,2,sd)
  
  
  
  ## we need to interpolate from the newdata x values to the values used for the model
  # then we calculate
  # 1) sd of the residuals between the fit and the original values
  # 2) sd of each predicted value between all bootstrap iterations
  
  loess_sd <- numeric(length=nrow(loess.boot$data))
  res_sd <- numeric(length=nrow(loess.boot$data))
  
  
  for( i in 1:nrow(loess.boot$data)){
    
    
    if(any(newdata==loess.boot$data[i,1])){ # the predicted points match the model building x'es
      loc <- which(newdata==loess.boot$data[i,1])
      res <- loess.boot$t[,loc]-loess.boot$data[i,2]
      res_sd[i] <- sd(res)
    }else{ # we have to interpolate to the y at the model x'es. We do it linearly between the two nearest prediction x'es
      
      lower_idx <- which(newdata < loess.boot$data[i,1])
      lower_idx <- lower_idx[length(lower_idx)]
      higher_idx <- which(newdata > loess.boot$data[i,1])[1]
      
      
      pred_y <- numeric(length=nrow(loess.boot$t)) 
      for( i2 in 1:nrow(loess.boot$t)){
        pred_y[i2] <- approx(x = c(newdata[lower_idx],newdata[higher_idx]), y = c(loess.boot$t[i2,lower_idx],loess.boot$t[i2,higher_idx]),  xout = loess.boot$data[i,1])$y
      }
      
      res <- pred_y-loess.boot$data[i,2]
      res_sd[i] <- sd(res)
      
      
      loess_sd[i] <- approx(x = c(newdata[lower_idx],newdata[higher_idx]), y = c(loess_sd_newdata[lower_idx],loess_sd_newdata[higher_idx]),  xout = loess.boot$data[i,1])$y
    }  
    
  }
  
  
  # Now we combine the SDs
  SD_combined <- sqrt(loess_sd^2+res_sd^2)
  
  
  
  
  
  # We now have the SDs at the x values used to build the model.
  # We now need to interpolate back to the values we have chosen to predict (newdata)
  
  SD_combined_newdata <- numeric(length=length(newdata))
  
  for(i in 1:length(newdata)){
    if(any(newdata[i]==loess.boot$data[,1])){ 
      loc <- which(newdata[i]==loess.boot$data[,1])
      SD_combined_newdata[i] <- SD_combined[loc][1] # The [1] is in case there are several identical x values. They would have same SD anyway
      
    }else{ 
      lower_idx <- which(newdata[i] > loess.boot$data[,1])
      lower_idx <- lower_idx[length(lower_idx)]
      higher_idx <- which(newdata[i] < loess.boot$data[,1])[1]
      
      SD_combined_newdata[i] <- approx(x = c(loess.boot$data[lower_idx,1],loess.boot$data[higher_idx,1]), y = c(SD_combined[lower_idx],SD_combined[higher_idx]),  xout = newdata[i])$y
    }
  }
  
  
  # Finally we make a matrix with the fitted value and the lower and upper bounds of the CI.
  ci=cbind(   loess.boot$t0,
              loess.boot$t0    -    SD_combined_newdata*qnorm(1-alpha/2),
              loess.boot$t0    +    SD_combined_newdata*qnorm(1-alpha/2))
  
  
  
  return(ci)
}





build_model <- function(oid1,oid2,force=FALSE,withProgress=TRUE,session) {
  
  
  # get Comparision matrix from database
  if(withProgress){
    progress <- Progress$new(session, min=1, max=100)
    on.exit(progress$close())
    progress$set(message = 'Calculation in progress (progress is not accurately followed)',
                 detail = 'Retrieving database values',
                 value=10)
  }
  
  comb_matrix = sys_comb_matrix(oid1,oid2)
  
  if(is.null(comb_matrix)){
    wrote_model_log(msg="No RT data found for one or both systems",sysoid1=oid1,sysoid2=oid2)
    return(NULL)
  }
  
  
  # Remove compounds where we have no data in one or both systems and order the data
  if(withProgress){
    progress$set(message = 'Calculation in progress (progress is not accurately followed)',
                 detail = 'Getting common compounds',
                 value=20)
  }
  
  if(!is.null(comb_matrix$rt)){
    del = as.vector(apply(comb_matrix$rt,1,function(x) any(is.na(x))))
    comb_matrix$rt = comb_matrix$rt[!del,,drop=F]
    comb_matrix$inchi = comb_matrix$inchi[!del,drop=F]
    
    ord = order(comb_matrix$rt[,1])
    comb_matrix$rt = comb_matrix$rt[ord,,drop=F]
    comb_matrix$inchi = comb_matrix$inchi[ord,drop=F]
  }
  
  if(   is.null(comb_matrix$rt) ){
    wrote_model_log(msg="Systems have no compounds in common. No model can be calculated",sysoid1=oid1,sysoid2=oid2)
    return(NULL)
  }
  
  if(  nrow(comb_matrix$rt)<10    ){
    wrote_model_log(msg="Systems have less than 10 compounds in common. No model will be calculated",sysoid1=oid1,sysoid2=oid2)
    return(NULL)
  }
  
  
  # get info about current models
  sys_models = get_models(include.loess=FALSE,include.ci = FALSE,include.newdata = FALSE )
  
  sys_models_newest_entry = lapply(sys_models,function(x) x$newest_entry)
  sys_models_n_points = sapply(sys_models,function(x) x$n_points)
  
  sys_models_oid1 = sapply(sys_models,function(x) x$oid_sys1)
  sys_models_oid2 = sapply(sys_models,function(x) x$oid_sys2)
  
  
  # check if we already have newest data point in the calculation or if data was deleted.
  select    = oid1==sys_models_oid1 & oid2==sys_models_oid2
  
  
  
  
  if(any(select)){
    if(       !(length(sys_models)==0)    &     !is.null(sys_models_newest_entry[[which(select)]])         ){ # there are no systems at all
      is_newer  = sys_models_newest_entry[[which(select)]]<comb_matrix$newest_entry
      same_nrow = sys_models_n_points[select] == nrow(comb_matrix$rt) 
      
      if(   !(is_newer | !same_nrow)  & !force  ){
        wrote_model_log(msg="There is no newer data available to build the model. Model will not be re-calculated.",sysoid1=oid1,sysoid2=oid2)
        return(NULL)
      }
    }
  }
  
  # Building the model
  set_model_status(sysoid1=oid1,sysoid2=oid2,status="calculating")
  
  if(withProgress){
    progress$set(message = 'Calculation in progress (progress is not accurately followed)',
                 detail = paste0('Model being build for ',sys_oid2name(oid2),' vs. ',sys_oid2name(oid1)),
                 value=30)
  }
  
  newdata = seq(from=min(comb_matrix$rt[,1]),to=max(comb_matrix$rt[,1]),length.out=1000)
  #fit=loess.wrapper(comb_matrix$rt[,1,drop=F], comb_matrix$rt[,2,drop=F], span.vals = seq(0.2, 1, by = 0.05), folds = nrow(comb_matrix$rt)) 
  #loess.boot <- boot(comb_matrix$rt,loess.fun,R=1000,newdata=newdata,span=fit$pars$span,parallel="multicore",ncpus=detectCores())
  #ci=boot2ci_PI(loess.boot,newdata,alpha=10^-8) # this crazy alpha value is there till a better solution is found
  
  
  loess.boot <- boot(comb_matrix$rt,gam.mono.con.fun,R=1000,newdata=newdata,parallel="multicore",ncpus=detectCores())
  ci=boot2ci(loess.boot,alpha=0.01)
  
  
  
  ## loess.boot:
  # t0: predicted y values
  # t: predicted y values for each iteration
  # data: original data
  
  if(withProgress){
    progress$set(message = 'Calculation in progress (progress is not accurately followed)',
                 detail = 'Writing data to the database.',
                 value=90)
  }
  
  
  
  predicted_at_orig_x = apply(loess.boot$data,1,function(x) which.min(abs((x[1]-newdata))))
  
  xy_mat <- cbind.data.frame(comb_matrix$rt,inchi=as.character(comb_matrix$inchi),stringsAsFactors=F)
  
  
  model_db_write(loess_boot=loess.boot,
                 xy_mat=xy_mat,
                 ci=ci,
                 newdata=as.numeric(newdata),
                 sysoid1=oid1,
                 sysoid2=oid2,
                 newest_entry=comb_matrix$newest_entry,
                 mean_error_abs=mean(abs(loess.boot$data[,2]-ci[predicted_at_orig_x,1])),
                 median_error_abs=median(abs(loess.boot$data[,2]-ci[predicted_at_orig_x,1])),
                 q95_error_abs=quantile(abs(loess.boot$data[,2]-ci[predicted_at_orig_x,1]),0.95),
                 max_error_abs=max(abs(loess.boot$data[,2]-ci[predicted_at_orig_x,1])),
                 mean_ci_width_abs=mean(ci[,3]-ci[,2]),
                 median_ci_width_abs=median(ci[,3]-ci[,2]),
                 q95_ci_width_abs=quantile(ci[,3]-ci[,2],0.95),
                 max_ci_width_abs=max(ci[,3]-ci[,2])
  )
  
  
  
  
}





predict_RT <- function(predict_to_system) {
  
  
  
  # get systems the selection has models to
  models_extended = get_models(include.loess=FALSE,include.ci = TRUE,include.newdata = TRUE ,include.xy_mat=TRUE)
  sys_models_oid1 <- sapply(models_extended,function(x) x$oid_sys1)
  sys_models_oid2 <- sapply(models_extended,function(x) x$oid_sys2)
  
  target_systems = sys_models_oid1[predict_to_system == sys_models_oid2]
  
  
  # get all rt data in database
  mongo <- PredRet_connect()
  
  
  query <- list(generation=0L,                     # only experimental data. predicted data is not used to predict in other systems. Yet...
                suspect=FALSE,                     # only use non-suspect data for prediction
                sys_id=list('$in'=c(target_systems,predict_to_system)) # only compound we know in systems where we are able to make models. and the RTs from the predict_to_system system
  )
  
  
  data_all = mongo.find.all(mongo=mongo, ns=PredRet.env$namespaces$ns_rtdata,query=query,data.frame=T,mongo.oid2character=T)
  
  
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo) 
  
  
  # only compound we know in systems where we are able to make models
  select <- data_all[,"sys_id"] %in% target_systems 
  data_target <- data_all[select,,drop=F]
  
  
  
  # Run through each unique target inchi
  unique_inchi <- unique(data_target[,"inchi"])
  predicted_data = as.data.frame(matrix(nrow=length(unique_inchi),ncol=8))
  colnames(predicted_data)=c("name","recorded_rt","predicted_rt","ci_lower","ci_upper","pubchem","inchi","generation")
  
  
  for(i in 1:length(unique_inchi)){ 
    
    # Get the data with the current target inchi
    single_inchi_data <-  data_target[unique_inchi[i]==data_target[,"inchi"],,drop=FALSE]
    single_inchi_data <-  data.frame(single_inchi_data,predicted=NA,ci_lower=NA,ci_upper=NA)
    
    
    
    # average out rt in the same system
    unique_targer_id = unique(single_inchi_data[,"sys_id"])
    
    if(length(unique_targer_id) != nrow(single_inchi_data)){
      for(i3 in 1:length(unique_targer_id)){
        unique_targer_id_loc <- which(unique_targer_id[i3]==single_inchi_data[,"sys_id"])
        
        if(length(unique_targer_id_loc)>1){
          single_inchi_data[unique_targer_id_loc,"recorded_rt"] <- mean(single_inchi_data[unique_targer_id_loc,"recorded_rt"])
          rem <- which(duplicated(single_inchi_data[,"sys_id"]) & (unique_targer_id[i3]==single_inchi_data[,"sys_id"]))
          single_inchi_data <- single_inchi_data[-rem,,drop=F]
        }
      }
    }
    
    
    # Make predictions for each system
    for(i2 in 1:nrow(single_inchi_data)){   
      current_model <- models_extended[[   which(       (sys_models_oid1 == single_inchi_data[i2,"sys_id"])    &   (sys_models_oid2 == predict_to_system)                 )     ]]
      close_rt <- which.min(abs(single_inchi_data[i2,"recorded_rt"]-current_model$newdata))
      
      single_inchi_data[i2,"predicted"] <- current_model$ci[close_rt,1]
      single_inchi_data[i2,"ci_lower"]  <- current_model$ci[close_rt,2]
      single_inchi_data[i2,"ci_upper"]  <- current_model$ci[close_rt,3]
    }
    
        
    
    
    
    
    # Set limits on the width of the CI interval at the point of prediction
    # ci_width_limit and ci_width_limit_rel are stored in the R package
    ci_width <- single_inchi_data$ci_upper-single_inchi_data$ci_lower
    ci_width_rel <-     ci_width   /   single_inchi_data$predicted
    
    select <- ci_width < PredRet.env$prediction$ci_width_limit & ci_width_rel < PredRet.env$prediction$ci_width_limit_rel
    
    if (!any(select)) next
    single_inchi_data <- single_inchi_data[select,,drop=FALSE]
    
    
    
    
    
    # Throw out predictions based on a recorded_rt with too low number of observations (density) in that RT area.
    models_oids   <- t(sapply(models_extended,function(x) c(x$oid_sys1,x$oid_sys2)))
    models_select <-  (models_oids[,1] %in%  single_inchi_data[,"sys_id"])        &            (predict_to_system == models_oids[,2])
    
    order <- match(single_inchi_data$sys_id, models_oids[models_select,1]) # when we run through the models we need to do it in the order they appear in single_inchi_data
    models_select <- which(models_select)[order]
    
    
    select = NULL
    for(i2 in 1:length(models_select)){
      xy_mat <- models_extended[[models_select[i2]]]$xy_mat
      
      dens <- density(xy_mat[,1], n = 512 * 8,bw=PredRet.env$prediction$predict_near_x_bw_mult*max(xy_mat[,1]))
      dens_fun <- with(dens, approxfun(x = x, y = y))
      
      select[i2] <- dens_fun(single_inchi_data[i2,"recorded_rt"]) > PredRet.env$prediction$predict_near_x_density_lim
    }
    
    
    if (!any(select,na.rm = TRUE)) next
    
    select <- which(select) # if it is NA it means it is outside the range so we remove it too. Need to turn it into indeces
    
    single_inchi_data <- single_inchi_data[select,,drop=FALSE]
    
    
    
    
    
    
    
    # We select just the one with the most narrow CI (relative value)
    #best_pred <- which.min(single_inchi_data[,"ci_upper"]-single_inchi_data[,"ci_lower"])
    best_pred <- which.min((single_inchi_data[,"ci_upper"]-single_inchi_data[,"ci_lower"])/single_inchi_data[,"predicted"]) # using the relative seems to give worse results
    
    single_inchi_data <- single_inchi_data[best_pred,,drop=F]
    
    
    
    
    
    
    
    #     plot(xy_mat,pch=20)
    #     
    #     plot(xy_mat[,1],dens_fun(xy_mat[,1]),type="p",pch=20,ylim=c(0,max(dens_fun(xy_mat[,1]))))
    #     lines(seq(from=min(xy_mat[,1]),to=max(xy_mat[,1]),by=0.01),dens_fun(seq(from=min(xy_mat[,1]),to=max(xy_mat[,1]),by=0.01)))    
    #     abline(h=0.01)
    
    
    
    
    
    
    
    
    # Add the prediction to the complete table
    predicted_data[i,"name"]         <- single_inchi_data[,"name"]
    predicted_data[i,"predicted_rt"] <- single_inchi_data[,"predicted"]
    predicted_data[i,"ci_lower"]     <- single_inchi_data[,"ci_lower"]
    predicted_data[i,"ci_upper"]     <- single_inchi_data[,"ci_upper"]
    predicted_data[i,"pubchem"]      <- single_inchi_data[,"pubchem"]
    predicted_data[i,"inchi"]        <- single_inchi_data[,"inchi"]
    predicted_data[i,"generation"]   <- as.integer(1)
    
    
    
    # Get experimental value if it exists.
    select <- (predicted_data[i,"inchi"] == data_all[,"inchi"])    &   (predict_to_system == data_all[,"sys_id"])
    
    # query <- list(generation = 0L,
    #               sys_id     = predict_to_system,
    #               inchi      = predicted_data[i,"inchi"]
    # )
    # 
    # 
    # rec_rt = mongo.find.all(mongo=mongo, ns=PredRet.env$namespaces$ns_rtdata,query=query,field=list(recorded_rt = 1L,sys_id=1L,inchi=1L,generation=1L),data.frame=T,mongo.oid2character=T)
    
    
    
    if(any(select)){
      predicted_data[i,"recorded_rt"] <- median(data_all[which(select),"recorded_rt"])
    }else{
      predicted_data[i,"recorded_rt"] <- as.numeric(NA)
    }
    
    
    
  }
  
  
  
  
  
  
  # Remove compounds for which no prediction could be made
  predicted_data <- predicted_data[        !is.na(predicted_data$predicted_rt)       ,   ,drop=FALSE]
  
  # if no predictions could be made at all
  if(nrow(predicted_data)==0){return(NULL)}
  
  
  predicted_data <- cbind.data.frame(sys_id = predict_to_system,predicted_data,time = Sys.time(),userID=as.integer(0),username="",suspect=FALSE)
  predicted_data$username <- as.character(predicted_data$username)
  predicted_data$sys_id <- as.character(predicted_data$sys_id)
  
  
  return(predicted_data)
}

