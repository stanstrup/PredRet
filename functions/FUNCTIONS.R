## Functions for database query #####################


get_user_data <- function(ns,userID=NULL,generation=NULL) {
  require(rmongodb)
  
  # System data to be able to convert sysid to sysname
  dbsystems <- get_systems()
  
  
  
  
  # Select which items to get
  if(    (!is.null(userID))        |              (!is.null(generation))                ){
    buf <- mongo.bson.buffer.create()
    
    
    
    if(!is.null(userID)){
      mongo.bson.buffer.append(buf, "userID", userID)
    }
    
    if(!is.null(generation)){
      mongo.bson.buffer.append(buf, "generation", generation)
    }
    
    
    query <- mongo.bson.from.buffer(buf)
    
  }else{
    query <- mongo.bson.empty()
  }
  
  
  
  
  # Select which columns/fields to get
  buf <- mongo.bson.buffer.create()
  
  fields_to_get=c("_id","sys_id","name","pubchem","inchi","time","userID","username","generation")
  
  
  if(!is.null(generation)){
    if(generation==0L){
      fields_to_get = c(fields_to_get,"recorded_rt")
    }else{
      fields_to_get = c(fields_to_get,c("predicted_rt","ci_lower","ci_upper"))
    }
  }else{
    fields_to_get = c(fields_to_get,"recorded_rt",c("predicted_rt","ci_lower","ci_upper"))
  }
  
  for(i in 1:length(fields_to_get)){
    mongo.bson.buffer.append(buf, fields_to_get[i], 1L)
  }
  
  
  fields <- mongo.bson.from.buffer(buf)
  
  
  # Read the data
  mongo <- mongo.create()
  data_all = mongo.find.all(mongo=mongo, ns=ns,query = query,fields = fields  ,data.frame=T,mongo.oid2character=T)
  
  if(is.null(data_all)){
    del <- mongo.disconnect(mongo)
    del <- mongo.destroy(mongo)
    return(NULL)
  }
  
  row.names(data_all) <- seq(nrow(data_all))
  
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
  
  # Take some data directly
  #data = data_all[,c("_id","sys_id","name","recorded_rt","pubchem","inchi","generation")]
  
  # Remove the time column
  data <- subset(data_all,select=-c(time))
  
  # Get correctly formatted time
  data = cbind.data.frame(data , `date added` = as.POSIXct(data_all[,"time"],origin="1970-01-01")     ,stringsAsFactors = F)
  
  
  # Get system name from system ID
  sys_id_data = as.character(data_all[,"sys_id"])
  sys_id_db = unlist(lapply(dbsystems,function(x) x$`_id`)  )
  sys_name = as.character(unlist(lapply(dbsystems,function(x) x$system_name)))  
  
  data = cbind.data.frame(data , system = sys_name[match(sys_id_data,sys_id_db)]          ,stringsAsFactors = F)
  
  

  
  return(data)
}



get_systems <- function() {
  require(rmongodb)
  
  
  
  # Connect to db
  mongo <- mongo.create()
  ns <- "test2.chrom_systems"
  
  
  # select fields (think columns)
  fields = mongo.bson.buffer.create()
  mongo.bson.buffer.append(fields, "_id", 1L)
  mongo.bson.buffer.append(fields, "sys_id", 1L)
  mongo.bson.buffer.append(fields, "system_name", 1L)
  mongo.bson.buffer.append(fields, "system_desc", 1L)
  mongo.bson.buffer.append(fields, "userID", 1L)
  mongo.bson.buffer.append(fields, "username", 1L)
  mongo.bson.buffer.append(fields, "system_eluent", 1L)
  mongo.bson.buffer.append(fields, "system_column", 1L)
  mongo.bson.buffer.append(fields, "system_ref", 1L)
  fields = mongo.bson.from.buffer(fields)
  
  data_back = mongo.find.all(mongo, ns=ns,fields=fields)
  
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
  return(data_back)
}








mongo_del_oid = function(ns,oids)  {
  
  mongo <- mongo.create()
  
  # loop through rows to delete
  for(i in 1:length(oids)){
    buf <- mongo.bson.buffer.create()
    mongo.bson.buffer.append(buf, "_id", mongo.oid.from.string(oids[i]))
    criteria <- mongo.bson.from.buffer(buf)
    mongo.remove(mongo, ns, criteria)
  }
  
  # close connection
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
}
















sys_comb_matrix = function(oid1,oid2,ns)  {
  require(rmongodb)
  
  
  
  ## get data for the combination of systems ################
  mongo <- mongo.create()
  
  buf <- mongo.bson.buffer.create()
  mongo.bson.buffer.append(buf, "sys_id", oid1)
  query <- mongo.bson.from.buffer(buf)
  rt_sys1 = mongo.find.all(mongo=mongo, ns=ns,query=query,data.frame=T,mongo.oid2character=T)
  rt_sys1 <- rt_sys1[rt_sys1$generation==0,] # only experimental data. not predicted is used for the model.
  
  
  buf <- mongo.bson.buffer.create()
  mongo.bson.buffer.append(buf, "sys_id", oid2)
  query <- mongo.bson.from.buffer(buf)
  rt_sys2 = mongo.find.all(mongo=mongo, ns=ns,query=query,data.frame=T,mongo.oid2character=T)
  rt_sys2 <- rt_sys2[rt_sys2$generation==0,] # only experimental data. not predicted is used for the model.
  
  
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
  if(is.null(rt_sys1) | is.null(rt_sys2)) return(NULL)
  if(nrow(rt_sys1)==0 | nrow(rt_sys2)==0 ) return(NULL)
  
  data <- rbind(rt_sys1,rt_sys2)
    
  
  
  
  
  
  
  ## setup comparision matrix ################
  unique_inchi = unique(data[,'inchi'])
  unique_systems = unique(data[,'sys_id'])
  unique_names = data[!duplicated(data[,'inchi']),'name']
  
  inchi_matrix = matrix(nrow=length(unique_inchi),ncol=length(unique_systems))
  colnames(inchi_matrix) = unique_systems
  rownames(inchi_matrix) = unique_names
  
  
  rt_matrix = matrix(nrow=length(unique_inchi),ncol=length(unique_systems))
  colnames(rt_matrix) = unique_systems
  rownames(rt_matrix) = unique_names
  
  
  for (i in 1:length(unique_inchi)){
    for (i2 in 1:length(unique_systems)){
      
      select = unique_inchi[i] == data[,'inchi']        &     unique_systems[i2] == data[,'sys_id']
      
      if (any(select)){  
        inchi_matrix[i,i2]=1   
        rt_matrix[i,i2]=mean(data[select,'recorded_rt'])
      }else{   
        inchi_matrix[i,i2]=0    
        rt_matrix[i,i2]=NA
      }
    }
  }
  
  
  
  return(list(rt=rt_matrix,newest_entry = max(c(rt_sys1$time,rt_sys2$time))))
  
}





get_ns <- function(ns){

    mongo <- mongo.create()
    data_back = mongo.find.all(mongo, ns=ns)
    del <- mongo.disconnect(mongo)
    del <- mongo.destroy(mongo)
    
    return(data_back)
  }
  








get_models <- function(include.loess=FALSE,include.ci=FALSE,include.newdata=FALSE) {
  require(rmongodb)
  
  
  
  # select fields (think columns)
  fields = mongo.bson.buffer.create()
  mongo.bson.buffer.append(fields, "_id", 1L)
  if(include.loess){ mongo.bson.buffer.append(fields, "loess_boot", 1L) }
  if(include.ci){ mongo.bson.buffer.append(fields, "ci", 1L) }
  if(include.newdata){ mongo.bson.buffer.append(fields, "newdata", 1L) }
  
    mongo.bson.buffer.append(fields, "oid_sys1", 1L)
  mongo.bson.buffer.append(fields, "oid_sys2", 1L)
  mongo.bson.buffer.append(fields, "status", 1L)
  mongo.bson.buffer.append(fields, "n_points", 1L)
  mongo.bson.buffer.append(fields, "time", 1L)
  mongo.bson.buffer.append(fields, "newest_entry", 1L)
  
  
  mongo.bson.buffer.append(fields, "mean_error_abs", 1L)
  mongo.bson.buffer.append(fields, "median_error_abs", 1L)
  mongo.bson.buffer.append(fields, "q95_error_abs", 1L)
  mongo.bson.buffer.append(fields, "max_error_abs", 1L)
  mongo.bson.buffer.append(fields, "mean_ci_width_abs", 1L)
  mongo.bson.buffer.append(fields, "median_ci_width_abs", 1L)
  mongo.bson.buffer.append(fields, "q95_ci_width_abs", 1L)
  mongo.bson.buffer.append(fields, "max_ci_width_abs", 1L)
  
  fields = mongo.bson.from.buffer(fields)
  
    
  
  # Connect to db
  mongo <- mongo.create()
  ns <- ns_sysmodels
  data_back = mongo.find.all(mongo, ns=ns,fields=fields)
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
  
  # Unserialize models
  if(include.loess){ 
  data_back = lapply(data_back,function(x) {
    x$loess_boot=unserialize(x$loess_boot)
    return(x)
    })
  }
  
  return(data_back)
}







sys_oid2name <- function(sys_id_data){
  
  dbsystems <- get_systems()
  sys_id_db = unlist(lapply(dbsystems,function(x) x$`_id`)  )
  sys_name = as.character(unlist(lapply(dbsystems,function(x) x$system_name)))  
  
  system = sys_name[match(sys_id_data,sys_id_db)]
  return(system)
}





wrote_model_log <- function(sysoid1,sysoid2,msg,ns){
  buf <- mongo.bson.buffer.create()
  mongo.bson.buffer.append(buf, "oid_sys1", sysoid1)
  mongo.bson.buffer.append(buf, "oid_sys2", sysoid2)
  mongo.bson.buffer.append(buf, "msg", msg)
  mongo.bson.buffer.append(buf, "time", Sys.time())
  buf <- mongo.bson.from.buffer(buf)
  
  mongo <- mongo.create()
  mongo.insert(mongo, ns, buf)
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo) 
}







log_count <- function(ns){
  require(rmongodb)
  
  mongo <- mongo.create()
  n = mongo.count(mongo, ns=ns_sysmodels_log )
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
  return(n)
}






get_build_log <- function(ns,time_offset=0){
  require(rmongodb)
  
  
  mongo <- mongo.create()
  
  # Get the data
  fields = mongo.bson.buffer.create()
  mongo.bson.buffer.append(fields, "_id", 0L)
  fields = mongo.bson.from.buffer(fields)
  
  sysmodel_log = mongo.find.all(mongo, ns=ns,fields = fields,limit=200L,sort = list(time=-1L))
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
  # Make it into a table
  sysmodel_log <- lapply(sysmodel_log,function(x) data.frame(time=x$time,oid_sys1=x$oid_sys1,oid_sys2=x$oid_sys2,msg=x$msg,stringsAsFactors=F))
  sysmodel_log <- do.call(rbind,sysmodel_log)
  
  # Change sys oids to names  
  log_sys_names = sys_oid2name(as.character(as.matrix(sysmodel_log[,c("oid_sys1","oid_sys2")])))
  dim(log_sys_names) <- c(length(log_sys_names)/2,2)
  sysmodel_log[,c("oid_sys1","oid_sys2")] <- log_sys_names
  
  
  # sort by date
  order        <- order(sysmodel_log[,"time"],decreasing = TRUE)
  sysmodel_log <- sysmodel_log[order,]
  
  # change to users time zone
  sysmodel_log[,"time"] <- as.POSIXct(as.numeric(sysmodel_log[,"time"]), origin = "1970-01-01", tz = "GMT") - time_offset
  
  return(sysmodel_log)
}






set_model_status <- function(sysoid1,sysoid2,status,ns){
  
  mongo <- mongo.create()

  db_models_oids = mongo.find.all(mongo, ns=ns,fields = list(oid_sys1=1L,oid_sys2=1L))
  db_models_oids_hit = unlist(lapply(db_models_oids,function(x) x$oid_sys1==sysoid1 & x$oid_sys2==sysoid2))
  
  
  if(any(db_models_oids_hit)){
    oids_to_update = lapply(db_models_oids, function(x) x$`_id`)
    oids_to_update = oids_to_update[[which(db_models_oids_hit)]]
    
    criteria <- mongo.bson.buffer.create()
    mongo.bson.buffer.append(criteria, "_id", oids_to_update)
    criteria <- mongo.bson.from.buffer(criteria)
    
    buf = mongo.bson.from.list(list("$set"=list("status"=status)))
    
    status = mongo.update(mongo, ns, criteria, objNew=buf)
    
    
  }else{
    buf <- mongo.bson.buffer.create()
    mongo.bson.buffer.append(buf, "oid_sys1", sysoid1)
    mongo.bson.buffer.append(buf, "oid_sys2", sysoid2)
    mongo.bson.buffer.append(buf, "status", status)
    buf <- mongo.bson.from.buffer(buf)
    
    status = mongo.insert(mongo, ns, buf)
  }
  
  
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
}














## Functions for prediction #####################

loess.fun <- function(in_data,inds,newdata,span){
  require(monoProc)
  x.star <- in_data[,1][inds]
  y.star <- in_data[,2][inds]
  
  #inds_saved[[length(inds_saved)+1]] <<- inds
  #counter[length(counter)+1] <<- counter+1
  
  out.star <- loess(y.star ~ x.star, span=span, control = loess.control(surface = "direct")  ) # direct is needed, otherwise it occationally blows up. I assume some border situations.
  y_pred = monoproc(out.star, bandwidth = 0.1, mono1 = "increasing", gridsize=100,xx= newdata)@fit@y
  
  y_pred[y_pred < 0] = 0
  
  return(y_pred)
}








gam.mono.con.fun <- function(in_data,inds,newdata,span){
  require(mgcv)
  x.star <- in_data[,1][inds]
  y.star <- in_data[,2][inds]
  
  #inds_saved[[length(inds_saved)+1]] <<- inds
  #counter[length(counter)+1] <<- length(counter)+1
  
  # We need at least 4 unique x-values to do the fit. So we add a small amount of jitter if it is not the case.
  if(length(unique(x.star))<4){
    x.star <- jitter(x.star,amount=0.01)
  }
  
  dat <- data.frame(x=x.star,y=y.star)
  f.ug <- gam(y~s(x,k=min(length(unique(x.star)),10),bs="tp"),data=dat)
  sm <- smoothCon(s(x,k=min(length(unique(x.star)),10),bs="cr"),dat,knots=NULL)[[1]]
  con <- mono.con(sm$xp);   # get constraints
  G <- list(X=sm$X,C=matrix(0,0,0),sp=f.ug$sp,p=sm$xp,y=y.star,w=y.star*0+1)
  G$Ain <- con$A
  G$bin <- con$b
  G$S <- sm$S
  G$off <- 0
  
  p <- pcls(G);  # fit spline (using s.p. from unconstrained fit)
  
  fv<-Predict.matrix(sm,data.frame(x=newdata))%*%p
  
  
  y_pred <- as.numeric(fv)
  y_pred[y_pred < 0] = 0
  
  return(y_pred)
  
    
}








boot2ci <- function(loess.boot,alpha = 0.01){
  require(parallel)
  
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
    require(boot)
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
  require(parallel)
  
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


















model_db_write <- function(loess_boot,
                           xy_mat,
                           ci,
                           newdata,
                           ns_sysmodels,
                           ns_sysmodels_log,
                           sysoid1,
                           sysoid2,
                           newest_entry,
                           mean_error_abs,
                           median_error_abs,
                           q95_error_abs,
                           max_error_abs,
                           mean_ci_width_abs,
                           median_ci_width_abs,
                           q95_ci_width_abs,
                           max_ci_width_abs
                           ){
  
  
  
  
  # Serialize loess_boot
  temp = loess_boot
  temp$statistic <- NULL  # This need to be removed since it references the whole calling environment
  temp$t <- NULL  # we don't need the result from all the simulations.
  temp = serialize(temp, connection = NULL, ascii = FALSE)
  #print(object.size(temp),units = "Kb")
  
  
  
  
  # Make the object to write to the db
  mongo <- mongo.create()
  buf <- mongo.bson.buffer.create()
  
  mongo.bson.buffer.append(buf, "loess_boot", temp)
  
  mongo.bson.buffer.append(buf, "ci", ci)
  mongo.bson.buffer.append(buf, "xy_mat", xy_mat)
  mongo.bson.buffer.append(buf, "newdata", newdata)
  mongo.bson.buffer.append(buf, "oid_sys1", sysoid1)
  mongo.bson.buffer.append(buf, "oid_sys2", sysoid2)
  mongo.bson.buffer.append(buf, "status", "done")
  mongo.bson.buffer.append(buf, "time", Sys.time())
  mongo.bson.buffer.append(buf, "n_points", nrow(loess_boot$data))
  mongo.bson.buffer.append(buf, "newest_entry", newest_entry)
  
  mongo.bson.buffer.append(buf, "mean_error_abs", mean_error_abs)
  mongo.bson.buffer.append(buf, "median_error_abs", median_error_abs)
  mongo.bson.buffer.append(buf, "q95_error_abs", as.numeric(q95_error_abs))
  mongo.bson.buffer.append(buf, "max_error_abs", max_error_abs)
  
  mongo.bson.buffer.append(buf, "mean_ci_width_abs", mean_ci_width_abs)
  mongo.bson.buffer.append(buf, "median_ci_width_abs", median_ci_width_abs)
  mongo.bson.buffer.append(buf, "q95_ci_width_abs", as.numeric(q95_ci_width_abs))
  mongo.bson.buffer.append(buf, "max_ci_width_abs", max_ci_width_abs)
  
  buf <- mongo.bson.from.buffer(buf)
  
  
  db_models_oids = mongo.find.all(mongo, ns=ns_sysmodels,fields = list("_id"=1L,oid_sys1=1L,oid_sys2=1L),mongo.oid2character = FALSE)
  db_models_oids_hit = unlist(lapply(db_models_oids,function(x) x$oid_sys1==sysoid1 & x$oid_sys2==sysoid2))
  
  if(any(db_models_oids_hit)){
    oids_to_update = lapply(db_models_oids, function(x) x$`_id`)
    oids_to_update = oids_to_update[[which(db_models_oids_hit)]]
    
    criteria <- mongo.bson.buffer.create()
    mongo.bson.buffer.append(criteria, "_id", oids_to_update)
    criteria <- mongo.bson.from.buffer(criteria)
    
    success <- mongo.update(mongo, ns_sysmodels, criteria, buf)


    
  }else{
    success <- mongo.insert(mongo, ns_sysmodels, buf)
  }
  #print(success)


  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
  if(success){
    wrote_model_log(msg="New model data was successfully written to the database.",sysoid1=sysoid1,sysoid2=sysoid2,ns=ns_sysmodels_log)
  }else{
    wrote_model_log(msg="Attempt to write new model data to the database failed.",sysoid1=sysoid1,sysoid2=sysoid2,ns=ns_sysmodels_log)
  }
  
 return(success)
}






plot_systems <- function(plotdata) {
  
  # Simple R plot ################
  # plot(loess.boot$data[,1],loess.boot$data[,2],pch=20)
  # lines(newdata,ci[,1])
  # lines(newdata,ci[,2],lty=3)
  # lines(newdata,ci[,3],lty=3)
  
  
  
  
  # attempt with rPlot ##############################
  # p1 <- rPlot(x = "x", y = "y",  data = plotdata, type = 'point', size = list(const = 3))
  # p1$layer(x = "x", y = "predicted",   data = plotdata, type = 'line', size = list(const = 2),color = list(const = 'black'))
  # p1$layer(x = "x", y = "upper", data = plotdata, type = 'line', size = list(const = 1),color = list(const = 'red'))
  # p1$layer(x = "x", y = "lower", data = plotdata, type = 'line', size = list(const = 1),color = list(const = 'red'))
  # 
  # p1$params$width=800
  # p1$params$height=600
  # p1
  
  
  
  
  # hPlot with tooltip ######################
  # Plot the points
  p <- hPlot(y ~ x, data = plotdata$data, type = "scatter")
  
  # fix data format
  p$params$series[[1]]$data <- toJSONArray(cbind.data.frame(x = plotdata$data$x, y = plotdata$data$y,name = plotdata$data$name,tooltip = plotdata$data$tooltip), json = F)
  
  # add tooltip formatter
  p$tooltip(formatter = "#! function() {return(this.point.tooltip);} !#")
  
  # Set priority higher than the overlay
  p$params$series[[1]]$zIndex=2
  
  
  p$title(style=list(fontSize='24px'),text=plotdata$title)
  
  p$xAxis(title=list(text=plotdata$xlab,style=list(fontSize='18px')))
  p$yAxis(title=list(text=plotdata$ylab,style=list(fontSize='18px')))
  
  
  p$series(
    data = toJSONArray2(cbind.data.frame(plotdata$data$newdata,plotdata$data[['predicted']]), names = F, json = F),
    type = 'line',
    zIndex = 1,
    marker=list(enabled=F,states=list(hover=list(enabled=F)))
  )
  
  
  p$series(
    data = toJSONArray2(cbind.data.frame(plotdata$data$newdata,plotdata$data[['lower']],plotdata$data[['upper']]), names = F, json = F),
    type = 'arearange',
    fillOpacity = 0.3,
    lineWidth = 0,
    color = 'lightblue',
    zIndex = 0
  )
  
  
  
  p$set(tooltip = list(
    crosshairs =  c(T,T),
    shared = T))
  
  
  # set plot size
  p$params$width=800*0.8
  p$params$height=600*0.8
  
  # plot
  invisible(p)
  
  
}
  
  
  



build_model <- function(oid1,oid2,ns_sysmodels,ns_rtdata,ns_sysmodels_log,force=FALSE,withProgress=TRUE,session) {
  require("boot")
  require("bisoreg")
  require("parallel")
  
     
  # get Comparision matrix from database
  if(withProgress){
  progress <- Progress$new(session, min=1, max=100)
  on.exit(progress$close())
  progress$set(message = 'Calculation in progress (progress is not accurately followed)',
               detail = 'Retrieving database values',
               value=10)
  }
  
  comb_matrix = sys_comb_matrix(oid1,oid2,ns=ns_rtdata)
  
  if(is.null(comb_matrix)){
    wrote_model_log(msg="No RT data found for one or both systems",sysoid1=oid1,sysoid2=oid2,ns=ns_sysmodels_log)
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
    
    ord = order(comb_matrix$rt[,1])
    comb_matrix$rt = comb_matrix$rt[ord,,drop=F]
  }
  
  if(   is.null(comb_matrix$rt) ){
    wrote_model_log(msg="Systems have no compounds in common. No model can be calculated",sysoid1=oid1,sysoid2=oid2,ns=ns_sysmodels_log)
    return(NULL)
  }
  
  if(  nrow(comb_matrix$rt)<10    ){
    wrote_model_log(msg="Systems have less than 10 compounds in common. No model will be calculated",sysoid1=oid1,sysoid2=oid2,ns=ns_sysmodels_log)
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
        wrote_model_log(msg="There is no newer data available to build the model. Model will not be re-calculated.",sysoid1=oid1,sysoid2=oid2,ns=ns_sysmodels_log)
        return(NULL)
      }
    }
  }
  
  # Building the model
  set_model_status(sysoid1=oid1,sysoid2=oid2,status="calculating",ns=ns_sysmodels)
  
  if(withProgress){
  progress$set(message = 'Calculation in progress (progress is not accurately followed)',
               detail = paste0('Model being build for ',sys_oid2name(oid2),' vs. ',sys_oid2name(oid1)),
               value=30)
  }
    
    newdata = seq(from=min(comb_matrix$rt[,1]),to=max(comb_matrix$rt[,1]),length.out=500)
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
    
  
  model_db_write(loess_boot=loess.boot,
                 xy_mat=comb_matrix$rt,
                 ci=ci,
                 newdata=as.numeric(newdata),
                 ns_sysmodels=ns_sysmodels,
                 ns_sysmodels_log=ns_sysmodels_log,
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
  models_extended = get_models(include.loess=FALSE,include.ci = TRUE,include.newdata = TRUE )
  sys_models_oid1 <- sapply(models_extended,function(x) x$oid_sys1)
  sys_models_oid2 <- sapply(models_extended,function(x) x$oid_sys2)
  
  target_systems = sys_models_oid1[predict_to_system == sys_models_oid2]
  
  
  # get all rt data in database
  mongo <- mongo.create()
  ns    <- ns_rtdata
  data_all = mongo.find.all(mongo=mongo, ns=ns,query=mongo.bson.empty(),data.frame=T,mongo.oid2character=T) # This can probably be done smarter so only the data we need is queried
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)  
  
  
  # only experimental data. predicted data is not used to predict in other systems. Yet...
  data_all <- data_all[data_all$generation==0,]
  
  
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
    
    
        
    # We select just the one with the most narrow CI (relative value)
    best_pred <- which.min(single_inchi_data[,"ci_upper"]-single_inchi_data[,"ci_lower"])
    #best_pred <- which.min((single_inchi_data[,"ci_upper"]-single_inchi_data[,"ci_lower"])/single_inchi_data[,"predicted"]) # using the relative seems to give worse results
    
    
    single_inchi_data <- single_inchi_data[best_pred,,drop=F]
    
    
    # Set limits on the width of the CI interval at the point of prediction
    # ci_width_limit and ci_width_limit_rel are stored in /settings/predictions.R
    ci_width <- single_inchi_data$ci_upper-single_inchi_data$ci_lower
    ci_width_rel <-     ci_width   /   single_inchi_data$predicted
    
    select <- ci_width < ci_width_limit & ci_width_rel < ci_width_limit_rel
    
    if (!any(select)) next
    single_inchi_data <- single_inchi_data[select,]
    
    
    
    
    
    
    # Add the prediction to the complete table
    predicted_data[i,"name"]         <- single_inchi_data[,"name"]
    predicted_data[i,"predicted_rt"] <- single_inchi_data[,"predicted"]
    predicted_data[i,"ci_lower"]     <- single_inchi_data[,"ci_lower"]
    predicted_data[i,"ci_upper"]     <- single_inchi_data[,"ci_upper"]
    predicted_data[i,"pubchem"]      <- single_inchi_data[,"pubchem"]
    predicted_data[i,"inchi"]        <- single_inchi_data[,"inchi"]
    
    # Get experimental value if it exists.
    select <- (predicted_data[i,"inchi"] == data_all[,"inchi"])    &   (predict_to_system == data_all[,"sys_id"])
    
    if(any(select)){
      predicted_data[i,"recorded_rt"] <- data_all[which(select)[1],"recorded_rt"]
    }else{
      predicted_data[i,"recorded_rt"] <- as.numeric(NA)
    }
    
    predicted_data[i,"generation"] <- as.integer(1)
    
  }
  
  
  # Remove compounds for which no prediction could be made
  predicted_data <- predicted_data[        !is.na(predicted_data$predicted_rt)       ,   ]
  
  # if no predictions could be made at all
  if(nrow(predicted_data)==0){return(NULL)}
  
  
  predicted_data <- cbind.data.frame(sys_id = predict_to_system,predicted_data,time = Sys.time(),userID=as.integer(0),username="")
  predicted_data$username <- as.character(predicted_data$username)
  predicted_data$sys_id <- as.character(predicted_data$sys_id)
  
  
  return(predicted_data)
}



pred_stat_make <- function(predicted_data) {

  
  predstats <- as.data.frame(matrix(ncol=1,nrow=10))
  colnames(predstats)=c(" ")
  rownames(predstats)=c("# Predictions made",
                        "# Predictions made where experimental RT is unknown",
                        "Mean prediction error*",
                        "Median prediction error*",
                        "95 % percentile prediction error*",
                        "Max prediction error*",
                        "Mean width of 95 % CI",
                        "Median width of 95 % CI",
                        "95 % percentile of 95 % CI width",
                        "Max width of 95 % CI")
  
  
  predstats[1,1] <- nrow(  predicted_data  )
  predstats[2,1] <- sum(is.na(predicted_data[,"recorded_rt"]))
  
  predstats[3,1] <- mean(abs(         predicted_data[,"recorded_rt"]   -    predicted_data[,"predicted_rt"]         ),na.rm = TRUE)
  predstats[4,1] <- median(abs(       predicted_data[,"recorded_rt"]   -    predicted_data[,"predicted_rt"]         ),na.rm = TRUE)
  predstats[5,1] <- quantile(abs(     predicted_data[,"recorded_rt"]   -    predicted_data[,"predicted_rt"]         ),probs = 0.95,na.rm = TRUE)
  predstats[6,1] <- max(abs(          predicted_data[,"recorded_rt"]   -    predicted_data[,"predicted_rt"]         ),na.rm = TRUE)
  
  predstats[7,1] <- mean(abs(          predicted_data[,"ci_upper"]   -    predicted_data[,"ci_lower"]         ),na.rm = TRUE)
  predstats[8,1] <- median(abs(        predicted_data[,"ci_upper"]   -    predicted_data[,"ci_lower"]         ),na.rm = TRUE)
  predstats[9,1] <- quantile(abs(      predicted_data[,"ci_upper"]   -    predicted_data[,"ci_lower"]         ),probs = 0.95,na.rm = TRUE)
  predstats[10,1] <- max(abs(          predicted_data[,"ci_upper"]   -    predicted_data[,"ci_lower"]         ),na.rm = TRUE)
  
  return(predstats)
  
}





pred_stat_write <- function(predstats,sys_oid,ns) {

  predstats_list <- as.list(t(predstats))
  predstats_list <- c(sys_oid,predstats_list)
  names(predstats_list) <- c("sys_oid",rownames(predstats))
  
  criteria <- list(sys_oid=sys_oid)
  
  
  mongo <- mongo.create()
  
  status <- mongo.update(mongo, ns, criteria, objNew=predstats_list,mongo.update.upsert)
  
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo) 
}




pred_stat_get <- function(sys_oid,ns) {
    
  query <- list(sys_oid=sys_oid)
  
  fields=list()
  fields[["_id"]]=0L
  field_names = c("# Predictions made",
                 "# Predictions made where experimental RT is unknown",
                 "Mean prediction error*",
                 "Median prediction error*",
                 "95 % percentile prediction error*",
                 "Max prediction error*",
                 "Mean width of 95 % CI",
                 "Median width of 95 % CI",
                 "95 % percentile of 95 % CI width",
                 "Max width of 95 % CI")
  
  for(i in 1:length(field_names)){
    fields[[field_names[i]]]=1L 
  }
  
    
  mongo <- mongo.create()
  
  pred_stats <- mongo.find.all(mongo, ns, query = query, data.frame = F, mongo.oid2character = TRUE,fields=fields   )
  pred_stats <- as.matrix(unlist(pred_stats),ncol=1)
  
  
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo) 
  
  return(pred_stats)
}




## Other functions #####################
bold.allrows <- function(x) {
  #h <- paste('\\textbf{',x,'}', sep ='')
  h <- paste0('<strong>',x,'</strong>')
  h
}