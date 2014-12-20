library(reshape2)
library(plyr)
library(car)
require(gdata)
library(ggplot2)
library(lme4)
require(longitudinalData)
# QUD1 Reliabiliaty analysis scripts
Timing = read.csv("./Eyes/Timing.csv",header = T)
PlaceCodes = read.csv("./Eyes/PlaceCodes.csv",header =  T)

# Original items
QUD1 = ET_Import.NoExpand("./Eyes/ReliabilityCoding/OriginalItems",Timing,PlaceCodes)
QUD1 <- QUD1[with(QUD1, order(Name,Trial,TimeFrame)),]
ddply(QUD1, .(Name,Trial,Cond,Vb,Prep,NP2,PrepFrame,NP2Frame), summarize, TimeFrame = c(0:max(TimeFrame))) -> QUD1.Expand
QUD1.Expand <- merge(QUD1,QUD1.Expand, by = c("Name","Trial","Cond","TimeFrame","Vb","Prep","NP2","PrepFrame","NP2Frame"), all= TRUE)
QUD1.Expand$Inst <- t(imputation(matrix(QUD1.Expand$Inst, nrow = 1),method = "locf"))
QUD1.Expand$TA <- t(imputation(matrix(QUD1.Expand$TA, nrow = 1),method = "locf"))
QUD1.Expand$DA <- t(imputation(matrix(QUD1.Expand$DA, nrow = 1),method = "locf"))
QUD1.Expand$DI <- t(imputation(matrix(QUD1.Expand$DI, nrow = 1),method = "locf"))
QUD1.Expand$Marker.Name <- t(imputation(matrix(QUD1.Expand$Marker.Name, nrow = 1),method = "locf"))

QUD1.Expand <- ddply(QUD1.Expand, .(Name,Trial), transform, Period = ifelse(TimeFrame >= NP2Frame,"NP2",ifelse(TimeFrame >= PrepFrame,"Prep","Verb"))) 
QUD1.Expand <- ddply(QUD1.Expand, .(Name,Trial), transform, TimeFrame = TimeFrame - NP2Frame) 
QUD1.Expand$TimeFrame <- QUD1.Expand$TimeFrame* 30

# Reliability items
QUD2 = ET_Import.NoExpand("./Eyes/ReliabilityCoding/RecodedItems",Timing,PlaceCodes)
QUD2 <- QUD2[with(QUD2, order(Name,Trial,TimeFrame)),]
ddply(QUD2, .(Name,Trial,Cond,Vb,Prep,NP2,PrepFrame,NP2Frame), summarize, TimeFrame = c(0:max(TimeFrame))) -> QUD2.Expand
QUD2.Expand <- merge(QUD2,QUD2.Expand, by = c("Name","Trial","Cond","TimeFrame","Vb","Prep","NP2","PrepFrame","NP2Frame"), all= TRUE)
QUD2.Expand$Inst <- t(imputation(matrix(QUD2.Expand$Inst, nrow = 1),method = "locf"))
QUD2.Expand$TA <- t(imputation(matrix(QUD2.Expand$TA, nrow = 1),method = "locf"))
QUD2.Expand$DA <- t(imputation(matrix(QUD2.Expand$DA, nrow = 1),method = "locf"))
QUD2.Expand$DI <- t(imputation(matrix(QUD2.Expand$DI, nrow = 1),method = "locf"))
QUD2.Expand$Marker.Name <- t(imputation(matrix(QUD2.Expand$Marker.Name, nrow = 1),method = "locf"))
QUD2.Expand <- ddply(QUD2.Expand, .(Name,Trial), transform, Period = ifelse(TimeFrame >= NP2Frame,"NP2",ifelse(TimeFrame >= PrepFrame,"Prep","Verb"))) 
QUD2.Expand <- ddply(QUD2.Expand, .(Name,Trial), transform, TimeFrame = TimeFrame - NP2Frame) 
QUD2.Expand$TimeFrame <- QUD2.Expand$TimeFrame* 30

# Calculate agreement
agree <- c()
for (i in (unique(QUD1.Expand$Name))){
  for (j in unique(QUD1.Expand[QUD1.Expand$Name == i,]$Trial)){
    
    minsize <- min(c(length(QUD1.Expand[QUD1.Expand$Name == i & QUD1.Expand$Trial == j,]$Marker.Name),length(QUD2.Expand[QUD2.Expand$Name == i & QUD2.Expand$Trial == j,]$Marker.Name)))
    print(mean(QUD1.Expand[QUD1.Expand$Name == i & QUD1.Expand$Trial == j,]$Marker.Name[1:minsize] == QUD2.Expand[QUD2.Expand$Name == i & QUD2.Expand$Trial == j,]$Marker.Name[1:minsize],na.rm = T))
    agree <- c(agree,mean(QUD1.Expand[QUD1.Expand$Name == i & QUD1.Expand$Trial == j,]$Marker.Name[1:minsize] == QUD2.Expand[QUD2.Expand$Name == i & QUD2.Expand$Trial == j,]$Marker.Name[1:minsize],na.rm = T))
    
  } 
  
}
print(mean(agree))