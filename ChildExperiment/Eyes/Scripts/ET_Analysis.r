require(lme4)
# Requires the QUD_Summary scripts to be run first.

QUD.Expand$Window <- as.factor(ifelse(QUD.Expand$TimeFrame <700, "EarlyWindow","LateWindow"))
QUD.Window <- summaryBy(Inst~Cond+QCond+Trial+Name.+Age+Window, data = QUD.Expand[QUD.Expand$TimeFrame >= 200 & QUD.Expand$TimeFrame < 1200,], FUN = c(mean),keep.names = T,na.rm = T)
QUD.Window$Inst <- ifelse(QUD.Window$Inst > 0,1,0)
contrasts(QUD.Window$QCond)[1] <- -1
contrasts(QUD.Window$Age)[1] <- -1
contrasts(QUD.Window$Cond)[1] <- -1
contrasts(QUD.Window$Window)[1] <- -1
summaryBy(Inst~Age+Window+Cond+QCond, data = QUD.Window)


# Overall
summary(glmer(Inst~Age*Cond*QCond*Window + (1|Name.) +(0+Window|Name.)+ (1|Trial)+(0+QCond+Window+Age|Trial), data = QUD.Window, family = "binomial",control=glmerControl(optCtrl=list(maxfun=20000), optimizer = "bobyqa" )))x

#Stats
#Early window
summary(glmer(Inst~Age*Cond*QCond + (1|Name.) + (1|Trial)+(0+QCond+Age|Trial), data = subset(QUD.Window, Window == "EarlyWindow"), family = "binomial",control=glmerControl(optCtrl=list(maxfun=20000), optimizer = "bobyqa" )))
#Produces interaction btwn age and VerbCond
#Check each age group
summary(glmer(Inst~Cond*QCond + (1|Name.) + (1|Trial)+(0+QCond|Trial), data = subset(QUD.Window, Window == "EarlyWindow" & Age == "5-years"), family = "binomial",control=glmerControl(optCtrl=list(maxfun=20000), optimizer = "bobyqa" )))
summary(glmer(Inst~Cond*QCond + (1|Name.) + (1|Trial)+(0+QCond|Trial), data = subset(QUD.Window, Window == "EarlyWindow" & Age == "7-years"), family = "binomial",control=glmerControl(optCtrl=list(maxfun=20000), optimizer = "bobyqa" )))
#Produces effect of Cond for youngest, and QCond for older.

#Late window
summary(glmer(Inst~Age*Cond*QCond + (1|Name.) + (1|Trial)+(0+QCond+Age|Trial), data = subset(QUD.Window, Window == "LateWindow"), family = "binomial",control=glmerControl(optCtrl=list(maxfun=20000), optimizer = "bobyqa" )))
#Produces interaction btwn age and VerbCond

summary(glmer(Inst~Cond*QCond + (1|Name.) + (1|Trial)+(0+QCond|Trial), data = subset(QUD.Window, Window == "LateWindow" & Age == "5-years"), family = "binomial",control=glmerControl(optCtrl=list(maxfun=20000), optimizer = "bobyqa" )))
summary(glmer(Inst~Cond*QCond + (1|Name.) + (1|Trial)+(0+QCond|Trial), data = subset(QUD.Window, Window == "LateWindow" & Age == "7-years"), family = "binomial",control=glmerControl(optCtrl=list(maxfun=20000), optimizer = "bobyqa" )))
# Youngest show effect of both QCond and Cond
# Oldest only show effect of QCond


k <- summaryBy(Inst~QCond+Cond+Age+Name.+Window, data = QUD.Window, FUN = c(mean,sd))
k <- summaryBy(Inst.mean~QCond+Cond+Age+Window, data = k, FUN = c(mean,sd))
k$SE = k$Inst.mean.sd/sqrt(6)
levels(k$QCond)[levels(k$QCond)=="Inst"] <- "Instrument Question"
levels(k$QCond)[levels(k$QCond)=="Mod"] <- "Modifier Question"
levels(k$Cond)[levels(k$Cond)=="Inst"] <- "Instrument-bias Verbs"
levels(k$Cond)[levels(k$Cond)=="Equi"] <- "Equi-bias Verbs"
k$Cond <- relevel(k$Cond, "Instrument-bias Verbs")
k$QCond <- relevel(k$QCond, "Instrument Question")
tapply(k$Inst.mean.mean, list(k$QCond,k$Window,k$Cond,k$Age), FUN = mean) -> o
tapply(k$SE, list(k$QCond,k$Window,k$Cond,k$Age), FUN = mean) -> se



#barplot(o, beside =T , ylim = c(610,650), col = "white", border = NA, ylab = "Naming Time (ms)", xlab = "Distance from associate to target word") -> p
old.par <- par(mfrow=c(2, 2))
for (i in c(1:2)){
	for (j in c(1:2)){
 barplot(o[,,1,1], beside =T , ylim = c(-0.1,1.1),col = "white",  border = NA, ylab = "Proportion trials gazing at instrument",  names.arg = c("Early Time Window", "Late Time Window"))
 if (i == 1){
 	if (j == 1){
 	 title(main = "5-year-olds hearing \n Instrument-biased verbs")
	 legend(1,1, legend = c("Instrument Question","Modifier Question"),  bty = "n", col = c("black","grey"), pch = 20)
	 }else{
	 title(main = "5-year-olds hearing \n Equi-biased verbs")}
	 }else{
	 if (j ==1){
	 title(main = "7-year-olds hearing \n Instrument-biased verbs")
	 }else{
	 title(main = "7-year-olds hearing \n Equi-biased verbs")
	 }
	 }
 points(c(1.8,4.8), o[1,,j,i], pch = 20, cex = 2, col = "black")
 points(c(2.2,5.2), o[2,,j,i], pch = 20, cex = 2, col = "grey",lwd = 2)
 grid(nx = NA, ny = NULL, col = "gray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
  arrows(c(1.8,2.2,4.8,5.2), (c(o[,,j,i]) + c(se[,,j,i])+0.01), c(1.8,2.2,4.8,5.2),  (c(o[,,j,i]) - c(se[,,j,i])-0.01), code = 0)
lines(c(1.8,4.8), o[1,,j,i], pch = 15, cex = 5, col = "black", lty = 3)
lines(c(2.2,5.2), o[2,,j,i], pch = 15, cex = 5, col = "black", lty = 3)
}
} 
par(old.par)