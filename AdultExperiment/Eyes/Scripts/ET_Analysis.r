require(lme4)
# Requires the QUD_Summary scripts to be run first.

QUD.Expand$Window <- as.factor(ifelse(QUD.Expand$TimeFrame <700, "EarlyWindow","LateWindow"))
QUD.Window <- summaryBy(Inst~Cond+Trial+Name+Window, data = QUD.Expand[QUD.Expand$TimeFrame >= 200 & QUD.Expand$TimeFrame < 1200,], FUN = c(mean),keep.names = T,na.rm = T)
QUD.Window$Inst <- ifelse(QUD.Window$Inst > 0,1,0)
contrasts(QUD.Window$Cond)[1] <- -1
summaryBy(Inst~Age+Window+Cond+QCond, data = QUD.Window)

#Stats

summary(glmer(Inst~Cond*Window+ (1|Name) + (1|Trial)+(0+Cond+Window|Trial), data = QUD.Window, family = "binomial",control=glmerControl(optCtrl=list(maxfun=20000), optimizer = "bobyqa" )))

summary(glmer(Inst~Cond+ (1|Name) + (1|Trial)+(0+Cond|Trial), data = subset(QUD.Window, Window == "EarlyWindow" ), family = "binomial",control=glmerControl(optCtrl=list(maxfun=20000), optimizer = "bobyqa" )))
summary(glmer(Inst~Cond + (1|Name) +(1|Trial)+ (0+Cond|Trial), data = subset(QUD.Window, Window == "LateWindow" ), family = "binomial",control=glmerControl(optCtrl=list(maxfun=20000), optimizer = "bobyqa" )))

q <- summaryBy(Inst~Cond+Window+Name, data = QUD.Window, FUN = c(mean,sd))
q <- summaryBy(Inst.mean~Cond+Window, data = q, FUN = c(mean,sd))
q$SE = q$Inst.mean.sd/sqrt(12)
tapply(q$Inst.mean.mean, list(q$Window, q$Cond), FUN = mean) -> o
tapply(q$SE, list(q$Window,q$Cond), FUN = mean) -> se

 barplot(o, beside =T , ylim = c(-0.1,1.1),col = "white",  border = NA, ylab = "Proportion trials gazing at instrument", xlab = "Window",names.arg = c("200ms-700ms", "700ms-1200ms"))
 legend(1,0.8, legend = c("Instrument Question", "Modifier Question"), bty = "n", col = c("black","grey"), pch = 20)
 points(c(1.8,4.8), o[,1], pch = 20, cex = 2, col = "black")
 points(c(2.2,5.2), o[,2], pch = 20, cex = 2, col = "grey",lwd = 2)
 grid(nx = NA, ny = NULL, col = "gray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
  arrows(c(1.8,4.8,2.2,5.2), (c(o) + c(se)), c(1.8,4.8,2.2,5.2), (c(o) - c(se)), code = 0)
lines(c(1.8,4.8), o[,1], pch = 15, cex = 5, col = "black", lty = 3)
lines(c(2.2,5.2), o[,2], pch = 15, cex = 5, col = "black", lty = 3)

