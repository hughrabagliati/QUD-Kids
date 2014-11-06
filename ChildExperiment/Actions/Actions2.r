library(doBy)
library(lme4)

qud.a <- read.csv("./Actions/actions.csv", header = T)
qud.a$Age <- as.factor(ifelse(qud.a$Age.group ==1, "five","seven"))
qud.a <- qud.a[!is.na(qud.a$Instr),]
contrasts(qud.a$Age)[1] <- -1
contrasts(qud.a$QuestCond)[1] <- -1
contrasts(qud.a$VerbCond)[1] <- -1


summary(glmer(Instr~Age*VerbCond*QuestCond + (1|Participant.no) + (1+QuestCond|bag.number), data = qud.a, family = "binomial",glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))))
# Ugh! GLMMs don't like the separation here - too many conditions with minimal variance. One option is to add a bit of noise to the variance-less conditions and then run 
# a GLM rather than a GLMM. Another is to do an ANOVA. 
# Or maybe an exact logistic regression
x <- xtabs(~Instr + interaction(Age, VerbCond, QuestCond), data = qud.a)
Act.Dat <- data.frame(Age = rep(c(0,1,0,1), 2), VerbCond = rep(c(0,0,1,1), 2), QuestCond = rep(c(0,0,1,1), each= 2),
    Instr = x[2, ], ntrials = colSums(x))
#m.apcalc <- elrm(formula = Instr/ntrials ~ VerbCond*QuestCond*Age, interest = ~VerbCond*QuestCond*Age, iter = 22000, dataset = Act.Dat, burnIn = 2000)
#Nope, still doesn't work

#How about an ANOVA using permutation tests to calculate p values?
summaryBy(Instr~Participant.no+VerbCond+QuestCond+Age, data = qud.a, keep.names = T) -> qud.sum
nreps <- 5000 	
Act.ANOVA = matrix(data = NA, nrow = nreps, ncol = 7)
summary(aov(lm(Instr~Age*VerbCond*QuestCond, data = qud.sum))) -> perm.orig
for (j in 1:7){
  Act.ANOVA[1,j] <- perm.orig[[1]]$"F value"[j]
  }
for (i in 2:nreps) {
  newInstr <- sample(qud.sum$Instr, length(qud.sum$Instr))
  mod2 <- lm(newInstr~qud.sum$Age*qud.sum$VerbCond*qud.sum$QuestCond)
  b <- summary(aov(mod2))
  for (j in 1:7){
  Act.ANOVA[i,j] <- b[[1]]$"F value"[j]
  	}
  }
for (i in 1:7){
print(length(Act.ANOVA[,i][Act.ANOVA[,i]>=Act.ANOVA[1,i]])/nreps)
 }

# This might work!

#Now by Age
for (k in unique(qud.a$Age)){
summaryBy(Instr~Participant.no+VerbCond+QuestCond, data = qud.a[qud.a$Age == k,], keep.names = T) -> qud.sum
nreps <- 5000 	
Act.ANOVA = matrix(data = NA, nrow = nreps, ncol = 3)
summary(aov(lm(Instr~VerbCond*QuestCond, data = qud.sum))) -> perm.orig
print(paste(k," years: ",sep = ""))
print(perm.orig)
for (j in 1:3){
  Act.ANOVA[1,j] <- perm.orig[[1]]$"F value"[j]
  }
for (i in 2:nreps) {
  newInstr <- sample(qud.sum$Instr, length(qud.sum$Instr))
  mod2 <- lm(newInstr~qud.sum$VerbCond*qud.sum$QuestCond)
  b <- summary(aov(mod2))
  for (j in 1:3){
  Act.ANOVA[i,j] <- b[[1]]$"F value"[j]
  	}
  }
print(paste("Age = ", k, sep = ""))
for (i in 1:3){
print(length(Act.ANOVA[,i][Act.ANOVA[,i]>=Act.ANOVA[1,i]])/nreps)
 }
}

# Now for 5-year-olds
for (k in unique(qud.a$VerbCond)){
summaryBy(Instr~Participant.no+VerbCond+QuestCond, data = qud.a[qud.a$Age == "five" & qud.a$VerbCond == k,], keep.names = T) -> qud.sum
nreps <- 1	
#nreps <- 5000 	
Act.ANOVA = matrix(data = NA, nrow = nreps, ncol = 3)
summary(aov(lm(Instr~QuestCond, data = qud.sum))) -> perm.orig
print(paste(k," years: ",sep = ""))
print(perm.orig)
for (j in 1:1){
  Act.ANOVA[1,j] <- perm.orig[[1]]$"F value"[j]
  }
for (i in 2:nreps) {
  newInstr <- sample(qud.sum$Instr, length(qud.sum$Instr))
  mod2 <- lm(newInstr~qud.sum$QuestCond)
  b <- summary(aov(mod2))
  for (j in 1:1){
  Act.ANOVA[i,j] <- b[[1]]$"F value"[j]
  	}
  }
print(paste("VerbCond = ", k, sep = ""))
for (i in 1:1){
print(length(Act.ANOVA[,i][Act.ANOVA[,i]>=Act.ANOVA[1,i]])/nreps)
 }
}

# This might work!


k <- summaryBy(Instr~QuestCond+VerbCond+Age+Participant.no, data = qud.a, FUN = c(mean,sd))
k <- summaryBy(Instr.mean~QuestCond+VerbCond+Age, data = k, FUN = c(mean,sd))
k$SE = k$Instr.mean.sd/sqrt(6)

k$VerbCond = factor(k$VerbCond, levels(k$VerbCond)[c(2,1)])

tapply(k$Instr.mean.mean, list(k$QuestCond,k$VerbCond,k$Age), FUN = mean) -> o
tapply(k$SE, list(k$QuestCond,k$VerbCond,k$Age), FUN = mean) -> se

#barplot(o, beside =T , ylim = c(610,650), col = "white", border = NA, ylab = "Naming Time (ms)", xlab = "Distance from associate to target word") -> p
old.par <- par(mfrow=c(1, 2))
 barplot(o[,,1], beside =T , ylim = c(-0.1,1.1),col = "white",  border = NA, ylab = "Proportion actions using Instrument",  main = "5-year-olds",names.arg = c("Instrument-bias verb", "Equi-bias verb"))
 legend(1.8,0.5, legend = c("Instrument Question", "Modifier Question"), bty = "n", col = c("black","grey"), pch = 20)
 points(c(1.5,4.5), o[1,,1], pch = 20, cex = 2, col = "black")
 points(c(2.5,5.5), o[2,,1], pch = 20, cex = 2, col = "grey")
 grid(nx = NA, ny = NULL, col = "gray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
  arrows(c(1.5,2.5,4.5,5.5), (c(o[,,1]) + c(se[,,1])), c(1.5,2.5,4.5,5.5), (c(o[,,1]) - c(se[,,1])), code = 0)

 barplot(o[,,2], beside =T , ylim = c(-0.1,1.1),col = "white",  border = NA, ylab = "Proportion actions using Instrument",  main = "7-year-olds",names.arg = c("Instrument-bias verb", "Equi-bias verb"))
 points(c(1.5,4.5), o[1,,2], pch = 20, cex = 2, col = "black")
 points(c(2.5,5.5), o[2,,2], pch = 20, cex = 2, col = "grey")
 grid(nx = NA, ny = NULL, col = "gray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
 arrows(c(1.5,2.5,4.5,5.5), (c(o[,,2]) + c(se[,,2])), c(1.5,2.5,4.5,5.5), (c(o[,,2]) - c(se[,,2])), code = 0)
 par(old.par)