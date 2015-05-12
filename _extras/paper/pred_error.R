library(plotrix)
library(PredRetR)


test <- PredRet_get_db()
test <- test[test$predicted==TRUE,]


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







# Quercetin family
test_select <- test[grepl("C21H20O12",test$inchi) & test$system == "FEM_short",]
xtics <- c(seq(12,14,0.2),seq(19,21,0.2))
gap=c(13.5,19.3)
height=250/72


# coumaric acid family
test_select <- test[grepl("C9H8O3",test$inchi) & test$system == "FEM_orbitrap_urine",]
xtics <- c(seq(6,11,0.2))
gap=c(12.5,13)
height=220/72




withinCI <- (test_select[,"ci_lower"] < test_select[,"recorded_rt"] )    &    (test_select[,"ci_upper"] > test_select[,"recorded_rt"] )
sum(withinCI)/length(withinCI)



pdf("isomers2.pdf", fonts=c("serif", "Palatino"),width=800/72,height=height, useDingbats=FALSE)
par(mar=c(5,14,4,2))
par(bty="n")

gap.plot(c(floor(min(test_select[,c("recorded_rt","ci_lower")],na.rm = T)),ceiling(max(test_select[,c("recorded_rt","ci_upper")],na.rm = T))),
         c(1,nrow(test_select)),
         gap=gap,
         gap.axis="x",
         type='n',
         xlab='Retention time (min)',
         ylab='',
         ytics=1:nrow(test_select),
         xtics=xtics,
         xticlab=xtics,
         yticlab=rep("",nrow(test_select)),
         ylim=c(0.7,nrow(test_select))
         )

axis(2, at=1:nrow(test_select), labels=test_select[,"name"], las=2, cex.axis=.8)
abline(v=seq(min(gap)-0.01,min(gap)+0.06,.001), col="white")  # hiding vertical lines
axis.break(axis=1,breakpos=min(gap),style="slash")

for(i in 1:nrow(test_select))     abline(h=i,lty=2,col="grey",lwd=0.5)

select <- c("recorded_rt","predicted_rt","ci_lower","ci_upper")
test_select[, select] <- apply(test_select[, select], 2, function(x) ifelse(x > max(gap), x-diff(gap), x))


segments(test_select[,"ci_lower"], 1:nrow(test_select),test_select[,"ci_upper"], 1:nrow(test_select),col=1)
points(test_select[,"predicted_rt"],1:nrow(test_select), pch=22, cex=1, col="black")

points(test_select[,"recorded_rt"],1:nrow(test_select), pch=20, cex=1, col="black")
points(test_select[!withinCI,"recorded_rt"],(1:nrow(test_select))[!withinCI], pch=20, cex=1, col="red")

dev.off()