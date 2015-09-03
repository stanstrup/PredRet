library(ggplot2)
library(reshape2)
library(Rplot.extra)



db.errors <- function(data){
predicted_rt <- recorded_rt <- ci_upper <- ci_lower <- NULL # making package check happy
  
out <- mutate(data, 
               error_abs    = abs(predicted_rt-recorded_rt),
               error_rel    = abs((predicted_rt-recorded_rt)/recorded_rt) ,
               ci_width_abs = ci_upper-ci_lower,
               ci_width_rel = (ci_upper-ci_lower)/predicted_rt,
               select       = !is.na(predicted_rt) & !is.na(recorded_rt),
               system       = as.factor(system)
)

return(out)
}


db.stats <- function(data){
predicted_rt <- recorded_rt <- error_abs <- select <- error_rel <- ci_width_abs <- ci_width_rel <- NULL  # making package check happy
  
stats <- ddply(data, .(system), summarise,
        N         = sum(!is.na(predicted_rt)),                                      # predictions
        N_ex      = sum(!is.na(predicted_rt) & is.na(recorded_rt))            ,     # predictions with unknown RT
        error_abs_median = median(error_abs[select])        ,                       # abs error
        error_rel_median = median(error_rel[select])        ,                       # abs error
        error_abs = mean(error_abs[select])        ,                                # abs error
        error_rel = mean(error_rel[select])        ,                                # rel error
        ci_abs = mean(ci_width_abs,na.rm = T)        ,                                                       
        ci_rel = mean(ci_width_rel,na.rm = T)        ,   
        ci_abs_median = median(ci_width_abs,na.rm = T)        ,
        ci_rel_median = median(ci_width_rel,na.rm = T)        ,
        .drop=F)

return(stats)
}











plot_predret.db.count <- function(data = PredRet_get_db()){
value <- variable <- NULL  # making package check happy

data <- db.errors(data)
stats <- db.stats(data)

# Add total count of compounds in db
stats <- mutate(stats,N_sys = as.data.frame(table(data[!data$predicted,"system"]))[,2])

# melt
temp <- melt(stats[,c("system","N_ex","N","N_sys")],id.vars = "system")

# Order the factors
temp <- mutate(temp,variable = factor(variable,levels = c("N_sys","N_ex")))
temp <- temp[order(temp$variable),]


# Do the plotting
p <- ggplot(temp, aes(x = system, y=value,fill=variable))
p <- p + geom_bar(stat = "identity", position = "stack")
p <- p + theme_bw_nice
p <- p + labs(title="Number of RTs in database and predictions made",fill="",y="# Compounds")
p <- p + theme(legend.position=c(0.8,0.95),legend.direction="vertical",legend.justification=c(1,1),legend.background = element_rect(fill="transparent"))
p <- p + scale_fill_manual(values = ggplotColours(n=3)[c(2,3)],labels = c("Experimental RTs in database","Predicted RTs"))

p <- p +  scale_y_continuous(breaks=seq(-1000,1000,100))
p <- p +  geom_hline(yintercept = 0,colour = "grey90")

p <- p + labs(x="Chromatographic systems")


return(p)
}
