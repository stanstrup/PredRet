library(stringr)
library(Rdisop)
library(chemhelper)



# function to collapse groups
collapse_idx <- function(x){

y <- lapply(1:nrow(x), function(i){
  temp <- x %in% x[i,]
  dim(temp) <- dim(x)
  temp <- x[which(temp[,1]),]
  temp <- sort(unique(as.numeric(temp)))
  return(temp)
}
)


y <- lapply(1:length(y), function(i){
  temp <-  sapply(y,function(x) any(y[[i]] %in% x))
  temp <-  sort(unique(as.numeric(unlist(y[temp]))))
  return(temp)
})


# remove duplicates
y <- unique(y)


return(y)
}

















# read data
data <- get_user_data(generation=1)

# get the difference between all masses
masses <- sapply(str_split_fixed(data[,"inchi"],"/",3)[,2], function(x) getMass(getMolecule(x))   )

mz_diff_mat <- outer(masses, masses, "-")
mz_diff_mat_abs <- abs(mz_diff_mat)

colnames(mz_diff_mat_abs) <- rownames(mz_diff_mat_abs) <- 1:nrow(data)


# Find entries with similar masses
mz_diff_mat_abs_close <- mz_diff_mat_abs < 0.01

diag(mz_diff_mat_abs_close) <- FALSE # set diagonal to false

mz_diff_mat_abs_close_idx <- which(mz_diff_mat_abs_close, arr.ind=TRUE) # index

# only one pair of indexes
mz_diff_mat_abs_close_idx <- t(apply(mz_diff_mat_abs_close_idx,1,sort))
mz_diff_mat_abs_close_idx <- unique(mz_diff_mat_abs_close_idx)


# check if the pairs are in the same system
select <- data$sys_id[as.numeric(mz_diff_mat_abs_close_idx[,1])] == data$sys_id[as.numeric(mz_diff_mat_abs_close_idx[,2])]
mz_diff_mat_abs_close_idx_same_sys <- mz_diff_mat_abs_close_idx[select,]



# collapse the groups such that all isomer pairs are together (this is a VERY inefficient and stupid way...)
isomer_idx <- collapse_idx(mz_diff_mat_abs_close_idx_same_sys)




# get matching isomers
isomer_list <- lapply(isomer_idx,function(is){
  cbind( data[  is              ,c("name","predicted_rt","ci_lower","ci_upper","system")]          ,  mass = masses[is]      )        
})





# find examples with non-overlapping CIs
no_overlap <- sapply(isomer_list, function(x){
  min(x[,"ci_upper"])  <    max(x[,"ci_lower"])
})

isomer_list_no_overlap <- isomer_list[no_overlap]



# sort each group by RT
isomer_list_no_overlap <- lapply(isomer_list_no_overlap, function(x){
  order <- order(x[,"predicted_rt"])
  x[order,]
})




# Mark which are different


isomer_list_no_overlap <- lapply(isomer_list_no_overlap,function(comp_group){
  
  g <- edges2diff.mat(comp_group$ci_lower, comp_group$ci_upper)
  letter_notation <- diff.mat2letter.not(g)
  cbind(comp_group,letter_notation)
  
  })







# see how many pairs can be descriminated in each system
table(sapply(isomer_list_no_overlap,function(x) x[1,"system"]))
