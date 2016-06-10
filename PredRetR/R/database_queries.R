# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

PredRet_connect <- function() {
  
    mongo <- mongo.create(host     = PredRet.env$mongo$host, 
                          name     = PredRet.env$mongo$name, 
                          username = PredRet.env$mongo$username,
                          password = PredRet.env$mongo$password,
                          db       = PredRet.env$mongo$db
                          )

  return(mongo)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

log_count <- function(){
  
  mongo <- PredRet_connect()
  n <- mongo.count(mongo, ns=PredRet.env$namespaces$ns_sysmodels_log )
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
  return(n)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

system_count <- function(sys_id){
  
  mongo <- PredRet_connect()
  n <- mongo.count(mongo, ns=PredRet.env$namespaces$ns_rtdata ,query=list(sys_id=sys_id))
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
  return(n)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

get_ns <- function(ns){
  
  mongo <- PredRet_connect()
  data_back = mongo.find.all(mongo, ns=ns)
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
  return(data_back)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

get_models <- function(include.loess=FALSE,include.ci=FALSE,include.newdata=FALSE,include.xy_mat=FALSE,from_oid=NULL,to_oid=NULL) {
  
  
  # Select which fields to get
  fields       <- c("_id",
                    "oid_sys1",
                    "oid_sys2",
                    "status",
                    "n_points",
                    "time",
                    "newest_entry",
                    "mean_error_abs",
                    "median_error_abs",
                    "q95_error_abs",
                    "max_error_abs",
                    "mean_ci_width_abs",
                    "median_ci_width_abs",
                    "q95_ci_width_abs",
                    "max_ci_width_abs")
  
  
  if(include.loess){   fields <- c(fields,"loess_boot") }
  if(include.ci){      fields <- c(fields,"ci")         }
  if(include.newdata){ fields <- c(fields,"newdata")    }
  if(include.xy_mat){  fields <- c(fields,"xy_mat")     }
  fields <- as.list(sapply(fields,function(x) x=1L))
  
  
  
  query <- list()
  if(  !is.null(from_oid) & !is.null(to_oid)    )  {   query <- c(query,list(oid_sys1=from_oid,oid_sys2=to_oid)) }
  
  
  # Connect to db
  mongo <- PredRet_connect()
  data_back <- mongo.find.all(mongo, ns=PredRet.env$namespaces$ns_sysmodels,fields=fields,query = query)
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
  
  # Unserialize models
  if(include.loess){ 
    data_back = lapply(data_back,function(x) {
      x$loess_boot=unserialize(x$loess_boot)
      return(x)
    })
  }
  
  
  
  # convert xy_mat back to data.frame
  if(include.xy_mat){  
    data_back = lapply(data_back,function(x) {
      x$xy_mat=do.call(rbind.data.frame,x$xy_mat)
      return(x)
    })
  }
  
  
  # convert ci back to data.frame
  if(include.ci){  
    data_back = lapply(data_back,function(x) {
      x$ci=as.data.frame(t(do.call(cbind,x$ci)))
      return(x)
    })
  }
  
  
  
  return(data_back)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

get_build_log <- function(time_offset=0){
  
  
  mongo <- PredRet_connect()
  
  # Get the data
  fields = mongo.bson.buffer.create()
  mongo.bson.buffer.append(fields, "_id", 0L)
  fields = mongo.bson.from.buffer(fields)
  
  sysmodel_log = mongo.find.all(mongo, ns=PredRet.env$namespaces$ns_sysmodels_log,fields = fields,limit=200L,sort = list(time=-1L))
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

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

sys_oid2name <- function(sys_id_data){
  
  dbsystems <- get_systems()
  sys_id_db = unlist(lapply(dbsystems,function(x) as.character.mongo.oid(x$`_id`))  )
  sys_name = as.character(unlist(lapply(dbsystems,function(x) x$system_name)))  
  
  system = sys_name[match(sys_id_data,sys_id_db)]
  return(system)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

name2sys_oid <- function(sys_name_data){
  
  dbsystems <- get_systems()
  sys_id_db = unlist(lapply(dbsystems,function(x) as.character.mongo.oid(x$`_id`))  )
  sys_name = as.character(unlist(lapply(dbsystems,function(x) x$system_name)))  
  
  system = sys_id_db[match(sys_name_data,sys_name)]
  return(system)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

get_systems <- function() {  
  
  # Connect to db
  mongo <- PredRet_connect()  
  
  # select fields (think columns)
  fields = mongo.bson.buffer.create()
  mongo.bson.buffer.append(fields, "_id", 1L)
  mongo.bson.buffer.append(fields, "sys_id", 1L)
  mongo.bson.buffer.append(fields, "system_name", 1L)
  mongo.bson.buffer.append(fields, "system_desc", 1L)
  mongo.bson.buffer.append(fields, "userID", 1L)
  mongo.bson.buffer.append(fields, "username", 1L)
  mongo.bson.buffer.append(fields, "system_eluent", 1L)
  mongo.bson.buffer.append(fields, "system_eluent_pH", 1L)
  mongo.bson.buffer.append(fields, "system_eluent_additive", 1L)
  mongo.bson.buffer.append(fields, "system_column", 1L)
  mongo.bson.buffer.append(fields, "system_column_type", 1L)
  mongo.bson.buffer.append(fields, "system_ref", 1L)
  fields = mongo.bson.from.buffer(fields)
  
  data_back = mongo.find.all(mongo, ns=PredRet.env$namespaces$ns_chrom_systems,fields=fields,mongo.oid2character = FALSE)
  
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo)
  
  return(data_back)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

get_user_data <- function(userID=NULL,generation=NULL,suspect=NULL,sys_id=NULL) {  
  
  
  # Select which items to get
  query <- list()    
  
  if(!is.null(userID)){
    query[["userID"]] <- userID
  }
  
  if(!is.null(generation)){
    query[["generation"]] <- generation
  }
  
  
  if(!is.null(suspect)){
    query[["suspect"]] <- suspect
  }
  
  if(!is.null(sys_id)){
    if(length(sys_id)>1){
      query[["sys_id"]] <- list('$in'=sys_id)
    }else{
      query[["sys_id"]] <- sys_id
    }
  }
  
  
  # Select which columns/fields to get
  fields <- list()
  fields_to_get=c("_id","sys_id","name","pubchem","inchi","time","userID","username","generation","suspect")
  
  
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
    fields[[  fields_to_get[i]  ]]   <- 1L
  }
  
  
  
  
  
  # Read the data
  mongo <- PredRet_connect()
  data_all = mongo.find.all(mongo=mongo, ns=PredRet.env$namespaces$ns_rtdata,query = query,fields = fields  ,data.frame=T,mongo.oid2character=T)
  
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
  data = cbind.data.frame(data , system = sys_oid2name(data_all[,"sys_id"])          ,stringsAsFactors = F)
  
  
  
  
  return(data)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

sys_comb_matrix = function(oid1,oid2,include.suspect=FALSE)  {
  
  
  ## get data for the combination of systems ################
  
  
  
  mongo <- PredRet_connect()
  rt_sys1 = mongo.find.all(mongo=mongo, ns=PredRet.env$namespaces$ns_rtdata, query = list(sys_id = oid1, generation = 0L, suspect = if(include.suspect){list('$in'=c(TRUE,FALSE))}else{FALSE}      )      ,data.frame=T,mongo.oid2character=T)
  rt_sys2 = mongo.find.all(mongo=mongo, ns=PredRet.env$namespaces$ns_rtdata, query = list(sys_id = oid2, generation = 0L, suspect = if(include.suspect){list('$in'=c(TRUE,FALSE))}else{FALSE}      )      ,data.frame=T,mongo.oid2character=T)
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
  
  
  
  return(    list(    rt=rt_matrix,    newest_entry = max(c(rt_sys1$time,rt_sys2$time)),  inchi=unique_inchi      )      )
  
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

pred_stat_get <- function(sys_oid) {
  
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
  
  
  mongo <- PredRet_connect()
  
  pred_stats <- mongo.find.all(mongo, ns=PredRet.env$namespaces$ns_pred_stats, query = query, data.frame = F, mongo.oid2character = TRUE,fields=fields   )
  pred_stats <- as.matrix(unlist(pred_stats),ncol=1)
  
  
  del <- mongo.disconnect(mongo)
  del <- mongo.destroy(mongo) 
  
  return(pred_stats)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
