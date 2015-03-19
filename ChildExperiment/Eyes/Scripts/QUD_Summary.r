library(reshape2)
library(plyr)
library(car)
require(gdata)
require(ggplot2)
require(longitudinalData)

# QUD Processing scripts
Timing = read.csv("./Eyes/Timing.csv",header = T)
PlaceCodes = read.csv("./Eyes/PlaceCodes.csv",header =  T)
QUD7 = ET_Import.NoExpand("./Eyes/SubjData/7YQUD/",Timing,PlaceCodes)
QUD7$Age = "7-years"
QUD5 = ET_Import.NoExpand("./Eyes/SubjData/5yQUD/",Timing,PlaceCodes)
QUD5$Age = "5-years"
QUD = rbind(QUD5,QUD7)
QUD$Age <- as.factor(QUD$Age)
QUD <- QUD[with(QUD, order(Name.,Trial,TimeFrame)),]


ddply(QUD, .(Name.,Trial,Cond,QCond,Age,Vb,Prep,NP2,PrepFrame,NP2Frame), summarize, TimeFrame = c(0:max(TimeFrame))) -> QUD.Expand
QUD.Expand <- merge(QUD,QUD.Expand, by = c("Name.","Trial","Cond","QCond","Age","TimeFrame","Vb","Prep","NP2","PrepFrame","NP2Frame"), all= TRUE)
QUD.Expand$Inst <- t(imputation(matrix(QUD.Expand$Inst, nrow = 1),method = "locf"))
QUD.Expand$TA <- t(imputation(matrix(QUD.Expand$TA, nrow = 1),method = "locf"))
QUD.Expand$DA <- t(imputation(matrix(QUD.Expand$DA, nrow = 1),method = "locf"))
QUD.Expand$DI <- t(imputation(matrix(QUD.Expand$DI, nrow = 1),method = "locf"))

QUD.Expand <- ddply(QUD.Expand, .(Name.,Trial), transform, Period = ifelse(TimeFrame >= NP2Frame,"NP2",ifelse(TimeFrame >= PrepFrame,"Prep","Verb"))) 
QUD.Expand <- ddply(QUD.Expand, .(Name.,Trial), transform, TimeFrame = TimeFrame - NP2Frame) 
QUD.Expand$TimeFrame <- QUD.Expand$TimeFrame* 24

save(list = "QUD.Expand", file = "QUD_Expand_Child.RDATA")


se <- function(x){
	x <- sd(x)/sqrt(6)
	return(x)
	}

QUD.Graph <- summaryBy(Inst+TA+DA+DI~TimeFrame+Cond+QCond+Name.+Age, data = QUD.Expand[QUD.Expand$TimeFrame > -300 & QUD.Expand$TimeFrame <=1500,], FUN = c(mean),keep.names = T)
QUD.Graph <- summaryBy(Inst+TA+DA+DI~TimeFrame+Cond+QCond+Age, data = QUD.Graph, FUN = c(mean,se))
levels(QUD.Graph$QCond)[levels(QUD.Graph$QCond)=="Inst"] <- "Instrument Question"
levels(QUD.Graph$QCond)[levels(QUD.Graph$QCond)=="Mod"] <- "Modifier Question"
levels(QUD.Graph$Cond)[levels(QUD.Graph$Cond)=="Inst"] <- "Instrument-bias Verbs"
levels(QUD.Graph$Cond)[levels(QUD.Graph$Cond)=="Equi"] <- "Equi-bias Verbs"
QUD.Graph$Cond <- relevel(QUD.Graph$Cond, "Instrument-bias Verbs")
QUD.Graph$QCond <- relevel(QUD.Graph$QCond, "Instrument Question")


ggplot(QUD.Graph,aes(TimeFrame,Inst.mean,linetype = QCond)) + facet_wrap(~Age+Cond) + stat_summary(fun.y = mean, geom = "line", size = 2) + geom_ribbon(aes(ymin=(Inst.mean - Inst.se), ymax=(Inst.mean + Inst.se)), alpha=0.2)+ theme(legend.title=element_blank(),legend.position="bottom")+labs(x = "Time (ms)",y = "Proportion of Looks")

QUD.Graph2 <- melt(QUD.Graph,
        # ID variables - all the variables to keep but not split apart on
    id.vars=c("TimeFrame","Cond","QCond","Age"),
        # The source columns
    measure.vars=c("Inst.mean","TA.mean", "DA.mean", "DI.mean" ),
        # Name of the destination column that will identify the original
        # column that the measurement came from
    value.name="Prop",
    variable.name ="Quadrant"
    )
QUD.Graph2$Quadrant <- revalue(QUD.Graph2$Quadrant, c("Inst.mean"="Target Instrument", "TA.mean"="Target Animal", "DA.mean" = "Distractor Animal","DI.mean" = "Distractor Instrument"))    
QUD.Graph2$Quadrant <- ordered(QUD.Graph2$Quadrant, levels = c("Target Animal", "Distractor Animal", "Target Instrument", "Distractor Instrument"))
ggplot(QUD.Graph2,aes(TimeFrame,Prop,linetype = Quadrant)) + facet_wrap(~Age+Cond+QCond, nrow = 4, ncol = 2) + stat_summary(fun.y = mean, geom = "line", size = 1) + theme(legend.title=element_blank(),legend.position="bottom")+ theme(legend.title=element_blank(),legend.position="bottom")+scale_linetype_manual(values=c(1,2,3,4))+labs(x = "Time (ms)",y = "Proportion of Looks")