library(PredRetR)
library(gridExtra)
library(plyr)
library(reshape2)
library(ggplot2)
library(Rplot.extra)



## Function to make negative numbers positive in ggplots ############################
commapos <- function(x, ...) {
  format(abs(x), big.mark = ",", trim = TRUE,
         scientific = FALSE, ...)
}



## Function to get unweighted R from robust regression ############################
library(MASS)
r2 <- function(x){  
  SSe <- sum((x$resid)^2);  
  observed <- x$resid+x$fitted;  
  SSt <- sum((observed-mean(observed))^2);  
  value <- 1-SSe/SSt;  
  return(value);  
}  





## Load data and make some stats ############################
data <- PredRet_get_db()



## Stats on predictions where RT is known
data$error_abs <- with(data, abs(predicted_rt-recorded_rt))
data$error_rel <- with(data, abs((predicted_rt-recorded_rt)/recorded_rt)      )
data$ci_width_abs <- with(data, ci_upper-ci_lower    )
data$ci_width_rel <- with(data, (ci_upper-ci_lower)/predicted_rt    )
data$select <- !is.na(data$predicted_rt) & !is.na(data$recorded_rt)
data$system <- as.factor(data$system)



stats <- 
ddply(data, .(system), summarise, N         = sum(!is.na(predicted_rt)),                                      # # predictions
                                  N_ex      = sum(!is.na(predicted_rt) & is.na(recorded_rt))            ,     # # predictions with unknown RT
                           error_abs_median = median(error_abs[select])        ,                                # abs error
      error_rel_median = median(error_rel[select])        ,                                # abs error
                                  error_abs = mean(error_abs[select])        ,                                # abs error
                                  error_rel = mean(error_rel[select])        ,                                # rel error
                                  ci_abs = mean(ci_width_abs,na.rm = T)        ,                                                       
                                  ci_rel = mean(ci_width_rel,na.rm = T)        ,   
                                  ci_abs_median = median(ci_width_abs,na.rm = T)        ,
      ci_rel_median = median(ci_width_rel,na.rm = T)        ,

      .drop=F)



mean(data$error_abs,na.rm = T)
median(data$error_abs,na.rm = T)

mean(data$error_rel,na.rm = T)
median(data$error_rel,na.rm = T)







## barplot for number of predictions ############################
temp <- stats
temp$N <- with(temp,N-N_ex)
temp <- melt(temp[,c("system","N_ex","N")])
p3 <- ggplot(temp, aes(x = system, y=value,fill=variable))
p3 <- p3 + geom_bar(stat = "identity", position = "stack")
p3 <- p3 + theme_bw()
p3 <- p3 + labs(fill="",y="# Predictions")
p3 <- p3 + theme(axis.text.x = element_text(colour="black",size = 11,face = "bold.italic",angle=45,vjust=1,hjust=1)   )
p3 <- p3 + scale_fill_discrete(labels = c("Predictions where experimental RT is unknown","Predictions where experimental RT is known"))
p3 <- p3 + theme(legend.position=c(1,1),legend.direction="vertical",legend.justification=c(1,1))
plot(p3)



## barplot for number of RTs in the database ############################
temp <- as.data.frame(table(data[!data$predicted,"system"]))
colnames(temp) <- c("system","N")

p4 <- ggplot(temp, aes(x = system, y=N))
p4 <- p4 + geom_bar(stat = "identity")
p4 <- p4 + theme_bw()
p4 <- p4 + labs(y="# Experimental RTs")
p4 <- p4 + theme(axis.text.x = element_text(colour="black",size = 11,face = "bold.italic",angle=45,vjust=1,hjust=1)   )
plot(p4)





## barplot for number of predictions combined with number of RTs in the database ############################
temp <- stats
temp$N <- with(temp,N-N_ex)
temp <- melt(temp[,c("system","N_ex","N")])


temp2 <- as.data.frame(table(data[!data$predicted,"system"]))
colnames(temp2) <- c("system","N")

temp2 <- data.frame(system = temp2$system,
                   variable = "N_sys",
                   value = temp2$N
                   )


temp <- rbind.data.frame(temp,temp2)

temp2 <- dcast(temp,system~variable)
temp2 <- mutate(temp2,N_ex_N_sys_ratio = N_ex/N_sys)



      
p7 <- ggplot(temp, aes(x = system, y=value,fill=variable))
p7 <- p7 + geom_bar(data = subset(temp, variable %in% c("N_ex","N")),stat = "identity", position = "stack")
p7 <- p7 + geom_bar(data = subset(temp, variable %in% c("N_sys")),aes(x = system, y=-value),stat = "identity")
p7 <- p7 + theme_bw_nice
p7 <- p7 + labs(title="Number of RTs and predictions made",fill="",y="# Experimental RTs        # Predictions")
p7 <- p7 + theme(legend.position=c(0.85,0.37),legend.direction="vertical",legend.justification=c(1,1),legend.background = element_rect(fill="transparent"))
p7 <- p7 + scale_fill_discrete(labels = c("Predictions where experimental RT is known","Predictions where experimental RT is unknown","Experimental RTs in database"))

p7 <- p7 +  scale_y_continuous(breaks=seq(-1000,1000,100),labels = commapos) # make negative scale positive
p7 <- p7 +  geom_hline(yintercept = 0,colour = "grey90")

p7 <- p7 + labs(x="Chromatographic systems")

plot(p7)









temp <- mutate(temp,type= ifelse( variable %in% c("N_ex","N") , "# Predictions" ,"# Experimental RTs in database" ) )
temp$type <- factor(temp$type,levels=c("# Experimental RTs in database","# Predictions"))


p11 <- ggplot(temp, aes(x = system, y=value,fill=variable))
p11 <- p11 + geom_bar(data = subset(temp, variable %in% c("N_ex","N","N_sys")),stat = "identity", position = "stack")
p11 <- p11 + theme_bw_nice
p11 <- p11 + facet_wrap( ~ type,nrow=2)
p11 <- p11 + labs(title="Number of RTs and predictions made",fill="",y="")
p11 <- p11 + theme(legend.position=c(0.85,0.37),legend.direction="vertical",legend.justification=c(1,1),legend.background = element_rect(fill="transparent"))
p11 <- p11 + scale_fill_manual(values = ggplotColours(n=3)[c(2,1,3)], labels = c("Experimental RT is unknown","Experimental RT is known","RTs in database") )
p11 <- p11 + labs(x="Chromatographic systems")

plot(p11)




# error ###################
plot( data_sub$recorded_rt,       (data_sub$predicted_rt-data_sub$recorded_rt)   ,pch=20)


# rel error ###################
plot( data_sub$recorded_rt,       (data_sub$predicted_rt-data_sub$recorded_rt)/data_sub$recorded_rt    ,pch=20)









## Investigate compounds with large error ##########################
bad <- which(data$error_abs>0.9)
bad <- data$inchi %in% data$inchi[bad]

data_bad <- data[bad,]
data_bad <- split(data_bad,data_bad$inchi)



data_bad[[1]]

data[grepl("/C15H12O7/",data[,"inchi"]),]
