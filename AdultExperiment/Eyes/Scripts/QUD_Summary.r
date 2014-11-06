library(reshape2)
library(plyr)
library(car)
require(gdata)
library(ggplot2)
library(lme4)
require(longitudinalData)
# QUD Processing scripts
Timing = read.csv("./Eyes/Timing.csv",header = T)
PlaceCodes = read.csv("./Eyes/PlaceCodes.csv",header =  T)
QUD = ET_Import.NoExpand("./Eyes/SubjData/",Timing,PlaceCodes)
QUD <- QUD[with(QUD, order(Name.,Trial,TimeFrame)),]

QUD.Look = LooksToInst(QUD)



one.way.plot(QUD.Look$Inst, QUD.Look$Cond, QUD.Look$Name, x.label = "Question Type", main.label = "", y.label = "Trials with looks to Instrument")

summary(glmer(Inst~Cond + (1+Cond|Name.)+(1+Cond|ItemNo), data= QUD.Look, family = "binomial"))



# Only analyzing the first block, as there are perseveration effects
QUD <- QUD[QUD$Block == "First",]



ddply(QUD, .(Name.,Trial,Cond,Vb,Prep,NP2,PrepFrame,NP2Frame), summarize, TimeFrame = c(0:max(TimeFrame))) -> QUD.Expand
QUD.Expand <- merge(QUD,QUD.Expand, by = c("Name.","Trial","Cond","TimeFrame","Vb","Prep","NP2","PrepFrame","NP2Frame"), all= TRUE)
QUD.Expand$Inst <- t(imputation(matrix(QUD.Expand$Inst, nrow = 1),method = "locf"))
QUD.Expand$TA <- t(imputation(matrix(QUD.Expand$TA, nrow = 1),method = "locf"))
QUD.Expand$DA <- t(imputation(matrix(QUD.Expand$DA, nrow = 1),method = "locf"))
QUD.Expand$DI <- t(imputation(matrix(QUD.Expand$DI, nrow = 1),method = "locf"))

QUD.Expand <- ddply(QUD.Expand, .(Name.,Trial), transform, Period = ifelse(TimeFrame >= NP2Frame,"NP2",ifelse(TimeFrame >= PrepFrame,"Prep","Verb"))) 
QUD.Expand <- ddply(QUD.Expand, .(Name.,Trial), transform, TimeFrame = TimeFrame - NP2Frame) 
QUD.Expand$TimeFrame <- QUD.Expand$TimeFrame* 30

save(list = "QUD.Expand", file = "QUD_Expand_Adult.RDATA")

# Functions below plot graphs etc.

se <- function(x){
	x <- sd(x)/sqrt(12)
	return(x)
	}

QUD.Graph <- summaryBy(Inst+TA+DA+DI~TimeFrame+Cond+Name., data = QUD.Expand[QUD.Expand$TimeFrame > -300 & QUD.Expand$TimeFrame <=1500,], FUN = c(mean),keep.names = T)
QUD.Graph <- summaryBy(Inst+TA+DA+DI~TimeFrame+Cond, data = QUD.Graph, FUN = c(mean,se))
levels(QUD.Graph$Cond)[levels(QUD.Graph$Cond)=="Inst"] <- "Instrument Question"
levels(QUD.Graph$Cond)[levels(QUD.Graph$Cond)=="Mod"] <- "Modifier Question"


ggplot(QUD.Graph,aes(TimeFrame,Inst.mean,linetype = Cond)) + stat_summary(fun.y = mean, geom = "line", size = 2) + geom_ribbon(aes(ymin=(Inst.mean - Inst.se), ymax=(Inst.mean + Inst.se)), alpha=0.2)+ theme(legend.title=element_blank(),legend.position="bottom")+labs(x = "Time (ms)",y = "Proportion of Looks")
#+ facet_wrap(~Age+Cond) 

QUD.Graph2 <- melt(QUD.Graph,
        # ID variables - all the variables to keep but not split apart on
    id.vars=c("TimeFrame","Cond"),
        # The source columns
    measure.vars=c("Inst.mean","TA.mean", "DA.mean", "DI.mean" ),
        # Name of the destination column that will identify the original
        # column that the measurement came from
    value.name="Prop",
    variable.name ="Quadrant"
    )
QUD.Graph2$Quadrant <- revalue(QUD.Graph2$Quadrant, c("Inst.mean"="Target Instrument", "TA.mean"="Target Animal", "DA.mean" = "Distractor Animal","DI.mean" = "Distractor Instrument"))    
QUD.Graph2$Quadrant <- ordered(QUD.Graph2$Quadrant, levels = c("Target Animal", "Distractor Animal", "Target Instrument", "Distractor Instrument"))
#levels(QUD.Graph2$Cond)[levels(QUD.Graph2$Cond)=="Inst"] <- "Instrument Question"
#levels(QUD.Graph2$Cond)[levels(QUD.Graph2$Cond)=="Mod"] <- "Modifier Question"

ggplot(QUD.Graph2,aes(TimeFrame,Prop,linetype = Quadrant)) + facet_wrap(~Cond, nrow = 1, ncol = 2) + stat_summary(fun.y = mean, geom = "line", size = 1) + theme(legend.title=element_blank(),legend.position="bottom")+ theme_bw()+theme(legend.title=element_blank(),legend.position="bottom",legend.key = element_rect(linetype=0))+scale_linetype_manual(values=c(1,2,3,4))+labs(x = "Time (ms)",y = "Proportion of Looks")