# When the package is loaded read an ini file to get config parame --------
.onLoad <- function(libname = find.package("PredRetR"), pkgname = "PredRetR") {

  #### read ini file
  
  # search locations
  ini_file <- c("./PredRet.conf", # if file in working dir use that
                "~/PredRet.conf", # if file in home folder
                system.file("extdata", "PredRet.conf", package = "PredRetR") # If no file found use the one from the package --> connect to predret.org
  )
  
  
  # check if we can find any config file at all
  if(all(!file.exists(ini_file))) stop("No PredRet.conf found.\nThis should not happen since the packages comes with a default configuration file.\n")
  
  
  # Get the first of the files in the above list
  ini_file <- ini_file[file.exists(ini_file)][1]
  
  # read the ini file
  cat(paste0("Using PredRet configuration file in: ",normalizePath(ini_file),"\n"))
  ini <- read.ini(normalizePath(ini_file))
  
  
  
  #### Read ini file data into environment
  
  PredRet.env                     <<- new.env()
  PredRet.env$auth                <<- ini$auth
  PredRet.env$auth$wordpress_auth <<- as.logical(ini$auth$wordpress_auth)
  PredRet.env$auth$userID         <<- as.numeric(ini$auth$userID)
  PredRet.env$mongo               <<- ini$mongo
  PredRet.env$namespaces          <<- ini$namespaces
  PredRet.env$prediction          <<- lapply(ini$prediction,as.numeric)
  PredRet.env$suspect             <<- lapply(ini$suspect,as.numeric)
  
  rm(ini,ini_file)
  
}



# Remove config environment on unload -------------------------------------
.onUnload <- function(libname = find.package("PredRetR"), pkgname = "PredRetR") {
  rm(PredRet.env,envir = globalenv())
}
