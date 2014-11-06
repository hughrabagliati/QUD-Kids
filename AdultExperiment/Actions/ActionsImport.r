require(doBy)
require(lme4)
require(XLConnect)
require(gdata)
QUD.Act <-  read.xls("./Actions/Combined.xlsx", sheet=1)
names(QUD.Act)[names(QUD.Act)=="Instrument.do.they.use.the.big.instrument.."] <- "Inst"
contrasts(QUD.Act$Condition)[1] <- -1
contrasts(QUD.Act$Block)[1] <- -1

summary(glmer(Inst~Condition*Block + (1+Condition|SubjNo) + (1+Condition|ItemNo), data = QUD.Act, family = "binomial"))
summary(glmer(Inst~Condition + (1+Condition|SubjNo) + (1+Condition|ItemNo), data = QUD.Act[QUD.Act$Block == "First",], family = "binomial"))
summary(glmer(Inst~Condition + (1+Condition|SubjNo) + (1+Condition|ItemNo), data = QUD.Act[QUD.Act$Block == "Second",], family = "binomial"))

q <- summaryBy(Inst~Condition+Block+SubjNo, data = QUD.Act, FUN = c(mean,sd))
q <- summaryBy(Inst.mean~Condition+Block, data = q, FUN = c(mean,sd))
q$SE = q$Inst.mean.sd/sqrt(12)
tapply(q$Inst.mean.mean, list(q$Block, q$Condition), FUN = mean) -> o
tapply(q$SE, list(q$Block,q$Condition), FUN = mean) -> se

 barplot(o, beside =T , ylim = c(-0.1,1.1),col = "white",  border = NA, ylab = "Proportion actions using Instrument", names.arg = c("First Block", "Second Block"))
 legend(2.,0.6, legend = c("Instrument Question", "Modifier Question"), bty = "n", col = c("black","grey"), pch = 20)
 points(c(1.5,4.5), o[,1], pch = 20, cex = 2, col = "black")
 points(c(2.5,5.5), o[,2], pch = 20, cex = 2, col = "grey")
 grid(nx = NA, ny = NULL, col = "gray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
  arrows(c(1.5,4.5,2.5,5.5), (c(o) + c(se)+0.01), c(1.5,4.5,2.5,5.5), (c(o) - c(se)-0.01), code = 0)
