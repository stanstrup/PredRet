library(ggplot2)
library(PredRetR)


source("settings/mongodb.R")


library(MASS)
r2 <- function(x){  
  SSe <- sum((x$resid)^2);  
  observed <- x$resid+x$fitted;  
  SSt <- sum((observed-mean(observed))^2);  
  value <- 1-SSe/SSt;  
  return(value);  
}  



data <- get_user_data(ns=ns_rtdata)

select <- !is.na(data$predicted_rt) & !is.na(data$recorded_rt)
data_sub <- data[select,]
#data_sub <- data_sub[order(data_sub$recorded_rt),]



# correlation plot
reg_model <- lm(predicted_rt ~ recorded_rt,data=data_sub)
rreg_model <- rlm(predicted_rt ~ recorded_rt,data=data_sub)
summary(reg_model)$r.squared
r2(rreg_model)

df <- data.frame(x <- data_sub$recorded_rt,y = data_sub$predicted_rt, down <- data_sub$ci_lower,  up <- data_sub$ci_upper,text=1:length(data_sub$name))

p <- ggplot(df, aes(x = x, y = y, ymin = down, ymax = up,label=text) )
p <- p + geom_ribbon(fill = 'grey80', alpha = 1)
p <- p +   geom_point(color = 'black', linetype = 'dashed')
p <- p + theme_bw()
# p <- p + scale_x_log10() 
# p <- p + scale_y_log10(limits = c(0.1,max(df[,"y"]))) 
# p <- p + annotation_logticks()
#p <- p + geom_abline(intercept=0, slope=1) 
#p <- p + geom_abline(data=data.frame(a = coef(rreg_model)[1],b = coef(rreg_model)[2]), aes(intercept=a, slope=b),color="red")
p <- p + geom_abline(data=data.frame(a = coef(reg_model)[1],b = coef(reg_model)[2]), aes(intercept=a, slope=b),color="blue")
p <- p + annotate("text",x=0,y=60,hjust=-0.2,vjust=0,label= paste0("R^{2}==",round(summary(reg_model)$r.squared,3) ),parse = TRUE         )
#p <- p + geom_text(hjust=-0.2, vjust=0.3)

plot(p)






# error
plot( data_sub$recorded_rt,       (data_sub$predicted_rt-data_sub$recorded_rt)   ,pch=20)


# rel error
plot( data_sub$recorded_rt,       (data_sub$predicted_rt-data_sub$recorded_rt)/data_sub$recorded_rt    ,pch=20)


