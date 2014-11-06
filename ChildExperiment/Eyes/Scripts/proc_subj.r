# Functions for basic data analysis.
# Function to tell you whether participants looked to Instrument on each trial
# The last SummaryBy statement can be modified to look at only the first X gazes, currently set at 4 to capture the first 3 eye movements (1 is starting point), currently 
LooksToInst = function(data){
require(plyr)
data = ddply(data,~Name.+Trial,transform,NextCode = c(Code[2:(length(Code))],Code[length(Code)]))
data$SwitInst = 0
data[data$NextCode %in% c("TI"),]$SwitInst = 1
data = ddply(data,~Name.+Trial,transform,LookEnd = c(TimeFrame[2:(length(TimeFrame))],TimeFrame[length(TimeFrame)]))
data$LookTime = data$LookEnd - data$TimeFrame
if ("RC" %in% colnames(data)){
data.Look <- summaryBy(Inst+SwitInst~Age+QCond+Cond+Pop+Name.+ItemNo+ExOrd, data = data[ data$LookTime > 0,], FUN = sum) # This and the above 3 lines ensure that we don't count any 1-frame-long looks, eg the last line of each trial in the datafile. 
}
else data.Look <- summaryBy(Inst+SwitInst~Age+QCond+Cond+Name.+ItemNo, data = data[ data$LookTime > 0,], FUN = sum) # This and the above 3 lines ensure that we don't count any 1-frame-long looks, eg the last line of each trial in the datafile. 
data.Look$Inst = 0
data.Look[data.Look$Inst.sum > 0,]$Inst = 1
return(data.Look)
}



#######



proc_subj.NoExpand = function(filename,Timing,PlaceCodes){
require(gdata)
subj = read.xls(filename, sheet=2)
#subj = filename
PlaceCodes$Cond = "Inst"
PlaceCodes$QCond = "Inst"
if (length(subj$Action.) > 1){
  if (grepl("-e",filename) == TRUE){ 
 	PlaceCodes$Cond = "Equi"} 
	
  if (grepl("-m",filename) == TRUE){ 
  	PlaceCodes$QCond = "Mod"} 
  	
  	
  PlaceCodes$Cond = as.factor(PlaceCodes$Cond)
  PlaceCodes$QCond = as.factor(PlaceCodes$QCond)
  subj = merge(subj,PlaceCodes, by.x = "Trial",by.y = "ItemNo", sort = FALSE)

  subj = merge(subj,Timing, by.x = "Verb",by.y = "Verb", sort = FALSE)
}

subj$Marker.Name = as.character(subj$Marker.Name)
subj$Marker.Name = gsub(" ","",subj$Marker.Name)
subj$Code = NA
subj$Code = subj$Marker.Name

for (i in unique(PlaceCodes$ItemNo)){
	if (length(subj[subj$Trial == i & subj$Marker.Name == "UL",]$Code) > 0){subj[subj$Trial == i & subj$Marker.Name == "UL",]$Code = as.character(PlaceCodes[PlaceCodes$ItemNo == i ,]$UL)}
	if (length(subj[subj$Trial == i & subj$Marker.Name == "UR",]$Code) > 0){subj[subj$Trial == i & subj$Marker.Name == "UR",]$Code = as.character(PlaceCodes[PlaceCodes$ItemNo == i ,]$UR)}
	if (length(subj[subj$Trial == i & subj$Marker.Name == "LL",]$Code) > 0){subj[subj$Trial == i & subj$Marker.Name == "LL",]$Code = as.character(PlaceCodes[PlaceCodes$ItemNo == i ,]$LL)}
	if (length(subj[subj$Trial == i & subj$Marker.Name == "LR",]$Code) > 0){subj[subj$Trial == i & subj$Marker.Name == "LR",]$Code = as.character(PlaceCodes[PlaceCodes$ItemNo == i ,]$LR)}
	}

subj$PrepFrame = round((subj$Prep - subj$Vb)*30)
subj$NP2Frame = round((subj$NP2 - subj$Vb)*30)

subj$Hour = read.table(textConnection(as.character(subj$Start)), sep = ":")[,1]
subj$Min = read.table(textConnection(as.character(subj$Start)), sep = ":")[,2]
subj$Sec = read.table(textConnection(as.character(subj$Start)), sep = ":")[,3]
subj$Frame = read.table(textConnection(as.character(subj$Start)), sep = ":")[,4]
subj$FullTimeFrame = subj$Frame+(subj$Sec*30)+(subj$Min*1798)+(subj$Hour*107892)
subj$TimeFrame = NA
for (i in unique(subj$Trial)){
	
	subj[subj$Trial == i,]$TimeFrame = subj[subj$Trial == i,]$FullTimeFrame - min(subj[subj$Trial == i,]$FullTimeFrame)
	}	



subj -> FullSubj

FullSubj$Inst = 0
if (length(FullSubj[FullSubj$Code == "TI",]$Inst>0)){FullSubj[FullSubj$Code == "TI",]$Inst = 1}

FullSubj$TA = 0
if (length(FullSubj[FullSubj$Code == "TA",]$TA>0)){FullSubj[FullSubj$Code == "TA",]$TA =  1}

FullSubj$DI = 0
if (length(FullSubj[FullSubj$Code == "DI",]$DI>0)){FullSubj[FullSubj$Code == "DI",]$DI = 1}

FullSubj$DA = 0
if (length(FullSubj[FullSubj$Code == "DA",]$DA > 0)){FullSubj[FullSubj$Code == "DA",]$DA = 1}

return(FullSubj)
}




#######


