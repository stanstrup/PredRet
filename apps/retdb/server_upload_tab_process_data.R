data_cleaned <- reactive({
  if (is.null(input$files))    return(NULL)
  # User has not uploaded a file yet
  
  
  # read data
  temp_data = read.csv(input$files$datapath,stringsAsFactors=F)
  
  # Make error messages (1= error that cannot be recovered. 2: warning with message to be displayed)
  errors = list()
  
  
  # limit data shown used
#   if (nrow(temp_data)<200){
#     temp_data =      temp_data
#   }else{
#     temp_data   =   temp_data[1:200,]
#   }
  
  
  # get only interesting columns and rename them
  colnames = tolower(colnames(temp_data))
  cols_to_get = c("compound","rt","method","pubchem","inchi")
  select=rep(NA,length(cols_to_get))
  for(i in 1:length(cols_to_get)){
    select[i] = grep(cols_to_get[i],colnames,fixed = T)[1]
  }
  
  temp_data   =    temp_data[,select[!is.na(select)]]
  
  
  # change column names
  colnames(temp_data) = c("name","rt","system_name","pubchem","inchi")[!is.na(select)]
  
  
  # Make sure pubchem id is treated as integer
  if(any(colnames(temp_data)=="pubchem")){
    temp_data[,"pubchem"] = as.integer(temp_data[,"pubchem"])
  }
  
  # Make sure rt is treated as numeric
    temp_data[,"rt"] = as.numeric(temp_data[,"rt"])
  
  
  # Check if all relevant columns are present
  if(any(is.na(select))){
    
    if(   (input$system_upload=="")     &      (  any(is.na(select[c(1,2,4)]))  )     ){
      errors$col_miss = list(error=1,msg=paste0('The following column(s) were not found: ',   paste0(cols_to_get[is.na(select)],'. "compound","rt", "method" and "pubchem" required',collapse=", ")  ,'.'     ))
    }
    
    if(   (!(input$system_upload==""))  &      (  any(is.na(select[c(1:4)]))    )     ){
      errors$col_miss = list(error=1,msg=paste0('The following column(s) were not found: ',   paste0(cols_to_get[is.na(select)],'. "compound","rt" and "pubchem" required',collapse=", ")  ,'.'     ))
    }
    
  }
  

  # Just return what we got if errors here
  if(any(unlist(   lapply(errors,function(x) x$error==1)    ))){    return(list(data=temp_data,errors=errors))    }
  
  
  
  ## Delete rows without enough data
  # Delete rows that don't contain any rt data.
  no_rt = is.na(temp_data[,"rt"]) | is.nan(temp_data[,"rt"])
  if(any(no_rt)){
    errors$no_rt = list(error=2,msg=paste0('No rt data was found in rows ',paste(which(no_rt),collapse=", "),'. Rows have been removed.'))
  }
  
  # Delete rows that don't contain pubchem or inchi
  no_id = (is.na(temp_data[,"pubchem"]) | is.nan(temp_data[,"pubchem"])) & !grepl("InChI",temp_data[,"inchi"],fixed=T)
  if(any(no_id)){
    errors$no_id = list(error=2,msg=paste0('Neither pubchem id nor inchi was found in rows ',paste(which(no_id),collapse=", "),'. Rows have been removed.'))
  }
  
  temp_data =    temp_data[!(no_id | no_rt),,drop=F]
  
  
  
  # Get inchi from pubchem
  no_inchi = !grepl("InChI",temp_data[,"inchi"],fixed=T)   &    !(is.na(temp_data[,"pubchem"]) | is.nan(temp_data[,"pubchem"]))
  temp_data[no_inchi,"inchi"] = pubchem2inchi(    temp_data[no_inchi,"pubchem"]       )
  
  # remove stereochemistry
  temp_data[,"inchi"] = inchi.rem.stereo(temp_data[,"inchi"])
  
# remove charges
  temp_data[,"inchi"] = inchi.rem.charges(temp_data[,"inchi"])

  
  # get the time
  time = Sys.time()
  
  
  
  
  # get the system ID from select if present. otherwise get from file.
  sys_name = as.character(unlist(lapply(systems_in_db(),function(x) x$system_name)))  
  sys_id = unlist(lapply(systems_in_db(),function(x) as.character.mongo.oid(x$`_id`))  )
  
  if(input$system_upload==""){  
    idx = match(temp_data[,"system_name"],sys_name)
  }else{
    idx = input$system_upload==sys_name
  }
  
  sys_id = sys_id[idx]
  
  
  
  # check if all methods have a database match
  if(any(is.na(idx))){
    errors$sys_not_in_db = list(error=1,msg=paste0('No system(s) called "',paste(unique(temp_data[is.na(idx),"system_name"])  , collapse=", "),'" (found in the csv file) was/were found in the database. Create a system with the corresponding name or select a single system in the upload column.'))
  }
  
  
  # Put everything together in a dataframe.
  temp_data =data.frame(sys_id,temp_data,time=time,userID=as.integer(userID()),username=as.character(username()),stringsAsFactors= FALSE)
  
    
  return(list(data=temp_data,errors=errors))
})