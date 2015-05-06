test <- get_user_data()
test <- test[test$generation==1,]


## CI width
hist((test[,"ci_upper"]-test[,"ci_lower"]),20,ylim=c(0,75))
hist((test[is.na(test[,"recorded_rt"]),"ci_upper"]-test[is.na(test[,"recorded_rt"]),"ci_lower"]),20,ylim=c(0,75))

# prediction error fwhen rt is known
error <- abs(test[,"recorded_rt"]-test[,"predicted_rt"])
error <- error[!is.na(error)]

hist(error,50)

quantile(error,seq(0,1,0.1))



# How many with known rt are within predicted CI
test_know <- test[!is.na(test[,"recorded_rt"]),]
withinCI <- (test_know[,"ci_lower"] < test_know[,"recorded_rt"] )    &    (test_know[,"ci_upper"] > test_know[,"recorded_rt"] )
sum(withinCI)/length(withinCI)



par("mar")->oldmar


par(mar=c(5,14,4,2))
plot(c(min(test_know[,c("recorded_rt","ci_lower")])*0.9,max(test_know[,c("recorded_rt","ci_upper")])*1.1), c(1,nrow(test_know)), type='n', xlab='RT', ylab='', axes=FALSE)
axis(1,at=seq(floor(min(test_know[,c("recorded_rt","ci_lower")])),ceiling(max(test_know[,c("recorded_rt","ci_upper")])),1))
axis(2, at=1:nrow(test_know), labels=test_know[,"name"], las=2, cex.axis=.8)

for(i in 1:nrow(test_know))     abline(h=i,lty=2,col="grey",lwd=0.5)  

segments(test_know[,"ci_lower"], 1:nrow(test_know), test_know[,"ci_upper"], 1:nrow(test_know),col=1)
points(test_know[,"predicted_rt"],1:nrow(test_know), pch=22, cex=1, col="black")

points(test_know[,"recorded_rt"],1:nrow(test_know), pch=20, cex=1, col="black")
points(test_know[!withinCI,"recorded_rt"],(1:nrow(test_know))[!withinCI], pch=20, cex=1, col="red")
