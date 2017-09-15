#################################Program Description################################
#Name: Matching and Analyzing (Round 2)                                            #
#Author: Traymon Beavers                                                           #
#Date Created: 3/29/2017                                                           #
#Purpose: To match the data based on gender, race, age, baseline average score and #
#         type of stroke for mobility, activity, and cognitive ability and then    #
#         perform analysis with matched data groups; 2nd round of data received    #
#Functions: matching                                                               #
#                                                                                  #
####################################################################################

################NOTES ABOUT DATA##################
###For some pairs of DaysId (i,j) where j>i, the days after onset for DaysID==i
###is greater than the days after onset for DaysID==j
###Stroke Severity Number is missing for 126 patients


#################################DATA PREPARATION###################################

###Upload Data in R
OldMaster=read.csv("Data/DEID_All2.csv")

###Delete extraneous varibale
OldMaster=OldMaster[,-1]

###Delete repeated observations

OldMaster=OldMaster[-seq(1086,1100,2),]

Master=OldMaster

#Create consistent missing value indicators for each functional outcome

for (i in 1:dim(Master)[1]){
  
  if(Master[i,"PT.AM.PAC.Basic.Mobility.Score"]==9999
     | Master[i,"PT.AM.PAC.Basic.Mobility.Score"]==8888
     | is.na(Master[i,"PT.AM.PAC.Basic.Mobility.Score"])==1){
    
    Master[i,"PT.AM.PAC.Basic.Mobility.Score"]=NA
    
  }
  
  if(Master[i,"OT.AM.Daily.Activity.Score"]==9999
     | Master[i,"OT.AM.Daily.Activity.Score"]==8888
     | Master[i,"OT.AM.Daily.Activity.Score"]==9909
     | is.na(Master[i,"OT.AM.Daily.Activity.Score"])==1){
    
    Master[i,"OT.AM.Daily.Activity.Score"]=NA
    
  }
  
  if(Master[i,"ST.AM.Applied.Cogn.Score"]==9999
     | Master[i,"ST.AM.Applied.Cogn.Score"]==8888
     | is.na(Master[i,"ST.AM.Applied.Cogn.Score"])==1){
    
    Master[i,"ST.AM.Applied.Cogn.Score"]=NA
    
  }
  
}

#Create a dataset for deceased people
DeceasedGroup=Master[(Master[,"Deceased_Y.N"]=="TRUE"),]

#Create datasets for each group 
BaselineGroup=Master[(Master[,"Group"]=="No"),]
ControlGroup=Master[(Master[,"Group"]=="Control Group"),]
StudyGroup=Master[(Master[,"Group"]=="Study Group"),]

###Create a vector for the patient IDs in each group

DeceasedGroupIDs=DeceasedGroup[DeceasedGroup[,"DaysId"]==1,"ID"]

ControlGroupIDs=unique(ControlGroup[,"ID"])
StudyGroupIDs=unique(StudyGroup[,"ID"])


###Match people in baseline group to their respective study or control groups

#Match for study group


T=1

if (BaselineGroup[dim(BaselineGroup)[1],"ID"]==StudyGroupIDs[length(StudyGroupIDs)]){
  
  NewStudyGroup=rbind(StudyGroup, BaselineGroup[dim(BaselineGroup)[1],])
  
}else {
  
  NewStudyGroup=StudyGroup
  
}

for (i in 1:(dim(BaselineGroup)[1]-1)){
  
  if (BaselineGroup[i,"ID"]==StudyGroupIDs[T]){
    
    NewStudyGroup=rbind(NewStudyGroup,BaselineGroup[i,])
    
  }
  
  if (BaselineGroup[i,"ID"]==StudyGroupIDs[T] & BaselineGroup[(i+1),"ID"]!=StudyGroupIDs[T] 
      & T!=length(StudyGroupIDs)){
    
    T=T+1
  }
  
}

#Sort by ID and days after stroke

Sort.NewStudyGroup=NewStudyGroup[order(NewStudyGroup$ID, NewStudyGroup$DaysId),]

#Match for control group

T=1

if (BaselineGroup[dim(BaselineGroup)[1],"ID"]==ControlGroupIDs[length(ControlGroupIDs)]){
  
  NewControlGroup=rbind(ControlGroup, BaselineGroup[dim(BaselineGroup)[1],])
  
}else {
  
  NewControlGroup=ControlGroup
  
}

for (i in 1:(dim(BaselineGroup)[1]-1)){
  
  if (BaselineGroup[i,"ID"]==ControlGroupIDs[T]){
    
    NewControlGroup=rbind(NewControlGroup,BaselineGroup[i,])
    
  }
  
  if (BaselineGroup[i,"ID"]==ControlGroupIDs[T] & BaselineGroup[(i+1),"ID"]!=ControlGroupIDs[T] 
      & T!=length(ControlGroupIDs)){
    
    T=T+1
  }
  
}

#Sort by ID and days after stroke

Sort.NewControlGroup=NewControlGroup[order(NewControlGroup$ID, NewControlGroup$DaysId),]

###Find the IDs for the crossovers

for (i in ControlGroupIDs){
  for (j in StudyGroupIDs){
    
    if (i==j){
      
      print(i)
      
    }
    
  }
}

#output was 103, 212

###Remove crossovers from datasets

Sort.NewStudyGroup=Sort.NewStudyGroup[Sort.NewStudyGroup[,"ID"]!=103 & Sort.NewStudyGroup[,"ID"]!=212,]
Sort.NewControlGroup=Sort.NewControlGroup[Sort.NewControlGroup[,"ID"]!=103 & Sort.NewControlGroup[,"ID"]!=212,]

###Rename races other than Black or White as "Other" in the study and control groups

for (i in 1:dim(Sort.NewStudyGroup)[1]){
  
  if (Sort.NewStudyGroup[i,"Race"]==3){
    
    Sort.NewStudyGroup[i,"New.Race"]="Black"
    
  }else if (Sort.NewStudyGroup[i,"Race"]==5){
    
    Sort.NewStudyGroup[i,"New.Race"]="White"
    
  }else {
    
    Sort.NewStudyGroup[i,"New.Race"]="Other"
    
  }  
  
}


for (i in 1:dim(Sort.NewControlGroup)[1]){
  
  if (Sort.NewControlGroup[i,"Race"]==3){
    
    Sort.NewControlGroup[i,"New.Race"]="Black"
    
  }else if (Sort.NewControlGroup[i,"Race"]==5){
    
    Sort.NewControlGroup[i,"New.Race"]="White"
    
  }else {
    
    Sort.NewControlGroup[i,"New.Race"]="Other"
    
  }  
  
}

###Combine both treatment groups back together

NewMaster=rbind(Sort.NewStudyGroup, Sort.NewControlGroup)

NewMaster=NewMaster[order(NewMaster[,"ID"], NewMaster[,"DaysId"]),]


#create a variable with one observation for each patients
NewMaster.One=rbind(NewMaster[NewMaster[,"DaysId"]==3,],
                    NewMaster[(NewMaster[,"DaysId"]==11 & NewMaster[,"ID"]==74) | 
                                (NewMaster[,"DaysId"]==10 & NewMaster[,"ID"]==214),]) 

NewMaster.One=NewMaster.One[order(NewMaster.One[,"ID"]),]

###Create variable for days after assignment to group and baseline for each score

NewMasterIDs=NewMaster.One[,"ID"]

NewMaster[,"Days.After.At.Assignment"]=rep(NA,dim(NewMaster)[1])
NewMaster[,"Baseline.Mobility"]=rep(NA,dim(NewMaster)[1])
NewMaster[,"Baseline.Activity"]=rep(NA,dim(NewMaster)[1])
NewMaster[,"Baseline.Cognitive"]=rep(NA,dim(NewMaster)[1])
NewMaster[,"Discharge.Mobility"]=rep(NA,dim(NewMaster)[1])
NewMaster[,"Discharge.Activity"]=rep(NA,dim(NewMaster)[1])
NewMaster[,"Discharge.Cognitive"]=rep(NA,dim(NewMaster)[1])


T=1

for (i in 1:dim(NewMaster)[1]){
  
  if (NewMaster[i,"ID"]!=NewMasterIDs[T] & T!=(length(NewMasterIDs))){
    
    T=T+1
    
  }
  
  if (NewMaster[i,"ID"]==74 | NewMaster[i,"ID"]==214){
    
    NewMaster[i,"Days.After.At.Assignment"]=
      NewMaster[NewMaster[,"DaysId"]==2 & NewMaster[,"ID"]==NewMasterIDs[T], "Days_afterOnset"]
    
    NewMaster[i,"Baseline.Mobility"]=NewMaster[NewMaster[,"DaysId"]==1 & NewMaster[,"ID"]==NewMasterIDs[T], "PT.AM.PAC.Basic.Mobility.Score"]
    NewMaster[i,"Baseline.Activity"]=NewMaster[NewMaster[,"DaysId"]==1 & NewMaster[,"ID"]==NewMasterIDs[T], "OT.AM.Daily.Activity.Score"]
    NewMaster[i,"Baseline.Cognitive"]=NewMaster[NewMaster[,"DaysId"]==1 & NewMaster[,"ID"]==NewMasterIDs[T], "ST.AM.Applied.Cogn.Score"]
    
    NewMaster[i,"Discharge.Mobility"]=NewMaster[NewMaster[,"DaysId"]==2 & NewMaster[,"ID"]==NewMasterIDs[T], "PT.AM.PAC.Basic.Mobility.Score"]
    NewMaster[i,"Discharge.Activity"]=NewMaster[NewMaster[,"DaysId"]==2 & NewMaster[,"ID"]==NewMasterIDs[T], "OT.AM.Daily.Activity.Score"]
    NewMaster[i,"Discharge.Cognitive"]=NewMaster[NewMaster[,"DaysId"]==2 & NewMaster[,"ID"]==NewMasterIDs[T], "ST.AM.Applied.Cogn.Score"]
    
  }else if (is.na(NewMaster[NewMaster[,"ID"]==NewMasterIDs[T] & 
                            NewMaster[,"DaysId"]==3, 
                            "PT.AM.PAC.Basic.Mobility.Score"])==1){
    
    NewMaster[i,"Days.After.At.Assignment"]=
      NewMaster[NewMaster[,"DaysId"]==2 & NewMaster[,"ID"]==NewMasterIDs[T], "Days_afterOnset"]
    
    NewMaster[i,"Baseline.Mobility"]=NewMaster[NewMaster[,"DaysId"]==1 & NewMaster[,"ID"]==NewMasterIDs[T], "PT.AM.PAC.Basic.Mobility.Score"]
    NewMaster[i,"Baseline.Activity"]=NewMaster[NewMaster[,"DaysId"]==1 & NewMaster[,"ID"]==NewMasterIDs[T], "OT.AM.Daily.Activity.Score"]
    NewMaster[i,"Baseline.Cognitive"]=NewMaster[NewMaster[,"DaysId"]==1 & NewMaster[,"ID"]==NewMasterIDs[T], "ST.AM.Applied.Cogn.Score"]
    
    NewMaster[i,"Discharge.Mobility"]=NewMaster[NewMaster[,"DaysId"]==2 & NewMaster[,"ID"]==NewMasterIDs[T], "PT.AM.PAC.Basic.Mobility.Score"]
    NewMaster[i,"Discharge.Activity"]=NewMaster[NewMaster[,"DaysId"]==2 & NewMaster[,"ID"]==NewMasterIDs[T], "OT.AM.Daily.Activity.Score"]
    NewMaster[i,"Discharge.Cognitive"]=NewMaster[NewMaster[,"DaysId"]==2 & NewMaster[,"ID"]==NewMasterIDs[T], "ST.AM.Applied.Cogn.Score"]
    
    
  }else {
    
    NewMaster[i,"Days.After.At.Assignment"]=
      NewMaster[NewMaster[,"DaysId"]==3 & NewMaster[,"ID"]==NewMasterIDs[T], "Days_afterOnset"]
    
    NewMaster[i,"Baseline.Mobility"]=NewMaster[NewMaster[,"DaysId"]==1 & NewMaster[,"ID"]==NewMasterIDs[T], "PT.AM.PAC.Basic.Mobility.Score"]
    NewMaster[i,"Baseline.Activity"]=NewMaster[NewMaster[,"DaysId"]==1 & NewMaster[,"ID"]==NewMasterIDs[T], "OT.AM.Daily.Activity.Score"]
    NewMaster[i,"Baseline.Cognitive"]=NewMaster[NewMaster[,"DaysId"]==1 & NewMaster[,"ID"]==NewMasterIDs[T], "ST.AM.Applied.Cogn.Score"]
    
    NewMaster[i,"Discharge.Mobility"]=NewMaster[NewMaster[,"DaysId"]==2 & NewMaster[,"ID"]==NewMasterIDs[T], "PT.AM.PAC.Basic.Mobility.Score"]
    NewMaster[i,"Discharge.Activity"]=NewMaster[NewMaster[,"DaysId"]==2 & NewMaster[,"ID"]==NewMasterIDs[T], "OT.AM.Daily.Activity.Score"]
    NewMaster[i,"Discharge.Cognitive"]=NewMaster[NewMaster[,"DaysId"]==2 & NewMaster[,"ID"]==NewMasterIDs[T], "ST.AM.Applied.Cogn.Score"]
    
    
  }
  
}

NewMaster[,"Days.After.Assignment"]=
  NewMaster[,"Days_afterOnset"]-NewMaster[,"Days.After.At.Assignment"]

###switch activity score and OT minutes to correct the error
NewMaster[NewMaster[,"ID"]==231 & NewMaster[,"DaysId"]==4,
          c("OT.AM.Daily.Activity.Score", "OT.Int.Treat.Minutes")]=NewMaster[NewMaster[,"ID"]==231 & NewMaster[,"DaysId"]==4,
                                                                             c("OT.Int.Treat.Minutes", "OT.AM.Daily.Activity.Score")]

NewMaster[,"Zero"]=rep(0,dim(NewMaster)[1])

NewMaster.One=rbind(NewMaster[NewMaster[,"DaysId"]==3,],
                    NewMaster[(NewMaster[,"DaysId"]==11 & NewMaster[,"ID"]==74) | 
                                (NewMaster[,"DaysId"]==10 & NewMaster[,"ID"]==214),]) 

NewMaster.One=NewMaster.One[order(NewMaster.One[,"ID"]),]

###switch NIHSS and ICH to correct the error
NewMaster.One[NewMaster.One[,"ID"]==165,
          c("ACHosp.Stroke.Severity.Number_NIHSS", "ACHosp.Stroke.Severity.Number_ICH")]=NewMaster.One[NewMaster.One[,"ID"]==165,
                                                                                                       c("ACHosp.Stroke.Severity.Number_ICH", "ACHosp.Stroke.Severity.Number_NIHSS")]
NewMaster.One[NewMaster.One[,"ID"]==162,
              c("ACHosp.Stroke.Severity.Number_NIHSS", "ACHosp.Stroke.Severity.Number_ICH")]=NewMaster.One[NewMaster.One[,"ID"]==162,
                                                                                                           c("ACHosp.Stroke.Severity.Number_ICH", "ACHosp.Stroke.Severity.Number_NIHSS")]

#View(NewMaster.One[,c("ACHosp.Stroke.Severity.Number_HH", "ACHosp.Stroke.Severity.Number_ICH", "ACHosp.Stroke.Severity.Number_NIHSS")])



###################CREATE BASELINE AVERAGES############################

###Find and demonstrate IDs missing 3rd follow up, which are 74 and 214

# length(Sort.NewStudyGroup[Sort.NewStudyGroup[,"DaysId"]==1,"ID"])
# length(Sort.NewStudyGroup[Sort.NewStudyGroup[,"DaysId"]==2,"ID"])
# length(Sort.NewStudyGroup[Sort.NewStudyGroup[,"DaysId"]==3,"ID"])
# 
# Sort.NewStudyGroup[Sort.NewStudyGroup[,"DaysId"]==2,"ID"]
# Sort.NewStudyGroup[Sort.NewStudyGroup[,"DaysId"]==3,"ID"]
# 
# 
# length(Sort.NewControlGroup[Sort.NewControlGroup[,"DaysId"]==1,"ID"])
# length(Sort.NewControlGroup[Sort.NewControlGroup[,"DaysId"]==2,"ID"])
# length(Sort.NewControlGroup[Sort.NewControlGroup[,"DaysId"]==3,"ID"])

# View(Sort.NewStudyGroup[Sort.NewStudyGroup[,"ID"]==74 | Sort.NewStudyGroup[,"ID"]==214, 
#                         c("ID", "DaysId", "Group")])
# 
# View(Sort.NewControlGroup[Sort.NewControlGroup[,"ID"]==212, c("ID", "DaysId", "Group")])

###Demonstrate all patients that have 3rd follow up containing a missing value are in the control
###group

# View(Master[is.na(Master[,"PT.AM.PAC.Basic.Mobility.Score"])==1 & Master[,"DaysId"]==3,
#             c("ID", "Group", "DaysId", "PT.AM.PAC.Basic.Mobility.Score")])



###Create a new variable for each treatment group for the average of the first three baseline scores
###except for IDs 74 and 214 which do not have 3rd follow ups and patients which have missing values
###for their third follow up, (take average of first two)

ScoreName=c("Mobility", "Activity", "Cognitive")
ScoreVarName=c("PT.AM.PAC.Basic.Mobility.Score", "OT.AM.Daily.Activity.Score", "ST.AM.Applied.Cogn.Score")

# for (j in 1:3){
#   
#   T=1
#   
#   for (i in 1:dim(Sort.NewStudyGroup)[1]){
#     
#     if (Sort.NewStudyGroup[i,"ID"]!=StudyGroupIDs[T] & T!=(length(StudyGroupIDs))){
#       
#       T=T+1
#       
#     }
#     
#     if (Sort.NewStudyGroup[i,"ID"]==74 | Sort.NewStudyGroup[i,"ID"]==214){
#     
#       Sort.NewStudyGroup[i, paste("Baseline.Average", ScoreName[j], sep=".")]=
#         
#         mean(c(Sort.NewStudyGroup[Sort.NewStudyGroup[,"DaysId"]==1 & Sort.NewStudyGroup[,"ID"]==StudyGroupIDs[T], ScoreVarName[j]], 
#                Sort.NewStudyGroup[Sort.NewStudyGroup[,"DaysId"]==2 & Sort.NewStudyGroup[,"ID"]==StudyGroupIDs[T], ScoreVarName[j]]))
#     
#   
#     }else {
#       
#       Sort.NewStudyGroup[i, paste("Baseline.Average", ScoreName[j], sep=".")]=
#         
#         mean(c(Sort.NewStudyGroup[Sort.NewStudyGroup[,"DaysId"]==1 & Sort.NewStudyGroup[,"ID"]==StudyGroupIDs[T], ScoreVarName[j]], 
#                Sort.NewStudyGroup[Sort.NewStudyGroup[,"DaysId"]==2 & Sort.NewStudyGroup[,"ID"]==StudyGroupIDs[T], ScoreVarName[j]],
#                Sort.NewStudyGroup[Sort.NewStudyGroup[,"DaysId"]==3 & Sort.NewStudyGroup[,"ID"]==StudyGroupIDs[T], ScoreVarName[j]]))
#       
#       }
#       
#     }
#   
#   
#   T=1
#   
#   for (i in 1:dim(Sort.NewControlGroup)[1]){
#     
#     if (Sort.NewControlGroup[i,"ID"]!=ControlGroupIDs[T] & T!=length(ControlGroupIDs)){
#       
#       T=T+1
#       
#     }
#     
#     if(T!=72 & T!=130){
#     
#       if (is.na(Sort.NewControlGroup[Sort.NewControlGroup[,"ID"]==ControlGroupIDs[T] & 
#                                Sort.NewControlGroup[,"DaysId"]==3, 
#                                "PT.AM.PAC.Basic.Mobility.Score"])==1){
#         
#         Sort.NewControlGroup[i, paste("Baseline.Average", ScoreName[j], sep=".")]=
#           
#           mean(c(Sort.NewControlGroup[Sort.NewControlGroup[,"DaysId"]==1 & Sort.NewControlGroup[,"ID"]==ControlGroupIDs[T], ScoreVarName[j]], 
#                  Sort.NewControlGroup[Sort.NewControlGroup[,"DaysId"]==2 & Sort.NewControlGroup[,"ID"]==ControlGroupIDs[T], ScoreVarName[j]]))
#       }else {
#     
#       Sort.NewControlGroup[i, paste("Baseline.Average", ScoreName[j], sep=".")]=
#         
#         mean(c(Sort.NewControlGroup[Sort.NewControlGroup[,"DaysId"]==1 & Sort.NewControlGroup[,"ID"]==ControlGroupIDs[T], ScoreVarName[j]], 
#                Sort.NewControlGroup[Sort.NewControlGroup[,"DaysId"]==2 & Sort.NewControlGroup[,"ID"]==ControlGroupIDs[T], ScoreVarName[j]],
#                Sort.NewControlGroup[Sort.NewControlGroup[,"DaysId"]==3 & Sort.NewControlGroup[,"ID"]==ControlGroupIDs[T], ScoreVarName[j]]))
#       
#     
#       }
#       
#     }
#   
#   }
#     
# }

################CHECK THAT THE DATA IS IN CORRECT RANGES##############

# ###Check education level range
# 
# NewMaster[which((NewMaster[,"Education.Level"]>40 | NewMaster[,"Education.Level"]<0) & NewMaster[,"Education.Level"]!=9999), "Education.Level"]
# 
# ###Check Stroke Severity Numbers: ICH 162 is 7, 165 is 21
# 
# NewMaster[which((NewMaster[,"ACHosp.Stroke.Severity.Number_NIHSS"]>42 | NewMaster[,"ACHosp.Stroke.Severity.Number_NIHSS"]<0) & 
#                   NewMaster[,"ACHosp.Stroke.Severity.Number_NIHSS"]!=9999), "ACHosp.Stroke.Severity.Number_NIHSS"]
# 
# NewMaster[which((NewMaster[,"ACHosp.Stroke.Severity.Number_ICH"]>6 | NewMaster[,"ACHosp.Stroke.Severity.Number_ICH"]<0) & 
#                   NewMaster[,"ACHosp.Stroke.Severity.Number_ICH"]!=9999), c("ID", "ACHosp.Stroke.Severity.Number_ICH")]
# 
# NewMaster[which((NewMaster[,"ACHosp.Stroke.Severity.Number_HH"]>5 | NewMaster[,"ACHosp.Stroke.Severity.Number_HH"]<0) & 
#                   NewMaster[,"ACHosp.Stroke.Severity.Number_HH"]!=9999), c("ID", "ACHosp.Stroke.Severity.Number_HH")]
# 
# ###Check Facility Adjustor
# 
# NewMaster[which((NewMaster[,"ARHosp.JRI.Facility.Adjustor"]>110 | NewMaster[,"ARHosp.JRI.Facility.Adjustor"]<101) & 
#                   NewMaster[,"ARHosp.JRI.Facility.Adjustor"]!=9999), c("ID", "ARHosp.JRI.Facility.Adjustor")]
# 
# ###Check FIM Scores
# 
# NewMaster[which((NewMaster[,"ARHosp.JRI.Adm.FIM.Motor"]>91 | NewMaster[,"ARHosp.JRI.Adm.FIM.Motor"]<12) & 
#                   NewMaster[,"ARHosp.JRI.Adm.FIM.Motor"]!=9999), "ARHosp.JRI.Adm.FIM.Motor"]
# 
# NewMaster[which((NewMaster[,"ARHosp.JRI.Adm.FIM.Cogn"]>35 | NewMaster[,"ARHosp.JRI.Adm.FIM.Cogn"]<0) & 
#                   NewMaster[,"ARHosp.JRI.Adm.FIM.Cogn"]!=9999), "ARHosp.JRI.Adm.FIM.Cogn"]
# 
# NewMaster[which((NewMaster[,"ARHosp.JRI.Adm.FIM.Total"]>126 | NewMaster[,"ARHosp.JRI.Adm.FIM.Total"]<17) & 
#                   NewMaster[,"ARHosp.JRI.Adm.FIM.Total"]!=9999), "ARHosp.JRI.Adm.FIM.Total"]
# 
# NewMaster[which((NewMaster[,"ARHosp.JRI.Dis.FIM.Motor"]>91 | NewMaster[,"ARHosp.JRI.Dis.FIM.Motor"]<12) & 
#                   NewMaster[,"ARHosp.JRI.Dis.FIM.Motor"]!=9999), "ARHosp.JRI.Dis.FIM.Motor"]
# 
# NewMaster[which((NewMaster[,"ARHosp.JRI.Dis.FIM.Cogn"]>35 | NewMaster[,"ARHosp.JRI.Dis.FIM.Cogn"]<0) & 
#                   NewMaster[,"ARHosp.JRI.Dis.FIM.Cogn"]!=9999), "ARHosp.JRI.Adm.FIM.Cogn"]
# 
# NewMaster[which((NewMaster[,"ARHosp.JRI.Dis.FIM.Total"]>126 | NewMaster[,"ARHosp.JRI.Dis.FIM.Total"]<17) & 
#                   NewMaster[,"ARHosp.JRI.Dis.FIM.Total"]!=9999), "ARHosp.JRI.Dis.FIM.Total"]
# 
# 
# ###Check height, weight, BMI, SBPHTN, DBPHTN, Fast-Nonfasting LDL, and HbA1c
# 
# NewMaster[which((NewMaster[,"Height"]>240 | NewMaster[,"Height"]<120) & 
#                   NewMaster[,"Height"]!=9999 & NewMaster[,"Height"]!=8888), 
#           c("ID", "Height") ]
# 
# NewMaster[which((NewMaster[,"Weight"]>230 | NewMaster[,"Weight"]<29) & 
#                   NewMaster[,"Weight"]!=9999 & NewMaster[,"Weight"]!=8888), 
#           c("ID", "Weight") ]
# 
# NewMaster[which((NewMaster[,"BMI"]>60 | NewMaster[,"BMI"]<10) & 
#                   NewMaster[,"BMI"]!=9999 & NewMaster[,"BMI"]!=8888), 
#           c("ID", "BMI") ]
# 
# NewMaster[which((NewMaster[,"SBP.HTN"]>250 | NewMaster[,"SBP.HTN"]<60) & 
#                     NewMaster[,"SBP.HTN"]!=9999 & NewMaster[,"SBP.HTN"]!=8888), 
#           c("ID", "SBP.HTN") ]
# 
# NewMaster[which((NewMaster[,"DBP.HTN"]>160 | NewMaster[,"DBP.HTN"]<20) & 
#                   NewMaster[,"DBP.HTN"]!=9999 & NewMaster[,"DBP.HTN"]!=8888), 
#           c("ID", "DBP.HTN") ]
# 
# NewMaster[which((NewMaster[,"Fast.Nonfasting.LDL"]>300 | NewMaster[,"Fast.Nonfasting.LDL"]<20) & 
#                   NewMaster[,"Fast.Nonfasting.LDL"]!=9999 & NewMaster[,"Fast.Nonfasting.LDL"]!=8888), 
#           c("ID", "Fast.Nonfasting.LDL") ]
# 
# NewMaster[which((NewMaster[,"HbA1c"]>20 | NewMaster[,"HbA1c"]<0) & 
#                   NewMaster[,"HbA1c"]!=9999 & NewMaster[,"HbA1c"]!=8888), 
#           c("ID", "HbA1c") ]
# 
# 
# ###Check TotNoOfMod_HighRisk
# 
# NewMaster[which((NewMaster[,"TotNoOfMod_HighRisk"]>11 | NewMaster[,"TotNoOfMod_HighRisk"]<0) & 
#                   NewMaster[,"TotNoOfMod_HighRisk"]!=9999 & NewMaster[,"HbA1c"]!=8888), 
#           c("ID", "TotNoOfMod_HighRisk")]
# 
# ###Check MoCa score
# 
# NewMaster[which((NewMaster[,"MoCA.Score"]>30 | NewMaster[,"MoCA.Score"]<0) & 
#                   NewMaster[,"MoCA.Score"]!=9999 & NewMaster[,"MoCA.Score"]!=8888), 
#           c("ID", "MoCA.Score")]
# 
# ###Check functional outcome scores and minutes: 231 Activity Score is 180
# 
# NewMaster[which((NewMaster[,"ModRankinScore"]>6 | NewMaster[,"ModRankinScore"]<0) & 
#                   NewMaster[,"ModRankinScore"]!=9999 & NewMaster[,"ModRankinScore"]!=8888), 
#           c("ID", "ModRankinScore")]
# 
# NewMaster[which((NewMaster[,"PT.AM.PAC.Basic.Mobility.Score"]>130 | NewMaster[,"PT.AM.PAC.Basic.Mobility.Score"]< -30) & 
#                   NewMaster[,"PT.AM.PAC.Basic.Mobility.Score"]!=9999 & NewMaster[,"PT.AM.PAC.Basic.Mobility.Score"]!=8888), 
#           c("ID", "PT.AM.PAC.Basic.Mobility.Score")]
# 
# NewMaster[which((NewMaster[,"OT.AM.Daily.Activity.Score"]>130 | NewMaster[,"OT.AM.Daily.Activity.Score"]< -30) & 
#                   NewMaster[,"OT.AM.Daily.Activity.Score"]!=9999 & NewMaster[,"OT.AM.Daily.Activity.Score"]!=8888), 
#           c("ID", "OT.AM.Daily.Activity.Score")]
# 
# NewMaster[which((NewMaster[,"ST.AM.Applied.Cogn.Score"]>130 | NewMaster[,"ST.AM.Applied.Cogn.Score"]< -30) & 
#                   NewMaster[,"ST.AM.Applied.Cogn.Score"]!=9999 & NewMaster[,"ST.AM.Applied.Cogn.Score"]!=8888), 
#           c("ID", "ST.AM.Applied.Cogn.Score")]
# 
# ###Check CVG scores: 125 Sessions Per Week is 19 309 Sessions Per Week is 6
# ###For Met.Mins some IDs have 99980001 and 78996544, Example IDs are 251 and 171
# 
# NewMaster[which((NewMaster[,"Cardiovascular.Group.SessionsPerWeek"]>5 | NewMaster[,"Cardiovascular.Group.SessionsPerWeek"]<0) & 
#                   NewMaster[,"Cardiovascular.Group.SessionsPerWeek"]!=9999 & NewMaster[,"Cardiovascular.Group.SessionsPerWeek"]!=8888), 
#           c("ID", "Cardiovascular.Group.SessionsPerWeek")]
# 
# NewMaster[which((NewMaster[,"Cardiovascular.Group.Tot.Sessions"]>50 | NewMaster[,"Cardiovascular.Group.Tot.Sessions"]<0) & 
#                   NewMaster[,"Cardiovascular.Group.Tot.Sessions"]!=9999 & NewMaster[,"Cardiovascular.Group.Tot.Sessions"]!=8888), 
#           c("ID", "Cardiovascular.Group.Tot.Sessions")]
# 
# NewMaster[which((NewMaster[,"Cardiovascular.Group.Intensity.Mets."]>10 | NewMaster[,"Cardiovascular.Group.Intensity.Mets."]<0) & 
#                   NewMaster[,"Cardiovascular.Group.Intensity.Mets."]!=9999 & NewMaster[,"Cardiovascular.Group.Intensity.Mets."]!=8888), 
#           c("ID", "Cardiovascular.Group.Intensity.Mets.")]
# 
# NewMaster[which((NewMaster[,"Cardiovascular.Group.Tot.Mins"]>60 | NewMaster[,"Cardiovascular.Group.Tot.Mins"]<0) & 
#                   NewMaster[,"Cardiovascular.Group.Tot.Mins"]!=9999 & NewMaster[,"Cardiovascular.Group.Tot.Mins"]!=8888), 
#           c("ID", "Cardiovascular.Group.Tot.Mins")]
# 
# NewMaster[which((NewMaster[,"Cardiovascular.Group.Met.Mins"]>500 | NewMaster[,"Cardiovascular.Group.Met.Mins"]<0) & 
#                   NewMaster[,"Cardiovascular.Group.Met.Mins"]!=9999 & NewMaster[,"Cardiovascular.Group.Met.Mins"]!=8888), 
#           c("ID", "Cardiovascular.Group.Met.Mins")]
# 
# NewMaster[which((NewMaster[,"CVG.Freq"]>3 | NewMaster[,"CVG.Freq"]<1) & 
#                   NewMaster[,"CVG.Freq"]!=9999 & NewMaster[,"CVG.Freq"]!=8888), 
#           c("ID", "CVG.Freq")]
# 
# NewMaster[which((NewMaster[,"CVG.Total"]>36 | NewMaster[,"CVG.Total"]<0) & 
#                   NewMaster[,"CVG.Total"]!=9999 & NewMaster[,"CVG.Total"]!=8888), 
#           c("ID", "CVG.Total")]



#################PROPENSITY AND MAHALANOBIS DISTANCE############


###Find propensity score for each observation for propensity score matching based on
###medical history variables

# Medical.History.Variables=colnames(NewMaster.One)[47:69]
# 
# fmla=as.formula(paste("Group ~ ", paste(Medical.History.Variables, collapse="+")))
# 
# glm.out=glm(formula=fmla, family=binomial(logit),
#             data=NewMaster.One[,c("Group", Medical.History.Variables)])
# 
# NewMaster.One[,"Propensity.Score"]=glm.out$fitted.values

NewMaster.One.Study=NewMaster.One[NewMaster.One[,"Group"]=="Study Group",]
NewMaster.One.Control=NewMaster.One[NewMaster.One[,"Group"]=="Control Group",]


###Calculate mahalonobis distances based on three functional outcome scores

#Set baseline averages for each patient as a data matrix and calculate the covariance matrix
# X=as.matrix(NewMaster.One[,c("Baseline.Average.Mobility","Baseline.Average.Activity","Baseline.Average.Cognitive")])
# SX=cov(NewMaster.One[,c("Baseline.Average.Mobility","Baseline.Average.Activity","Baseline.Average.Cognitive")])

#find the mahalanobis distance for each pair with two for loops

# mahal.dist=matrix(NA, dim(NewMaster.One)[1], dim(NewMaster.One)[1])
# 
# for (i in 1:dim(NewMaster.One)[1]){
#   for (j in 1:dim(NewMaster.One)[1]){
#     
#     mahal.dist[i,j]=mahalanobis(X[i,],X[j,],SX)
#     
#   }
# }
# 

###place all the nonzero mahalanobis distances in a vector
#mahal.dist.vec=c(mahal.dist[mahal.dist>0])

##############################INTERPOLATION##################################

#create vector of IDs in NewMaster
NewMasterIDs=NewMaster.One[,"ID"]

#intialize Interpolate.Master
Interpolate.Master=NewMaster

#cycle through IDs in NewMaster
for (j in 1:length(NewMasterIDs)){
  
  ###find which follow ups are logged for the ID
  
  #initialize DAYSID
  DAYSID=0
  
  #cycle through possible DaysId observations
  for (i in 1:8){
    
    if(length(Interpolate.Master[Interpolate.Master[,"ID"]==NewMasterIDs[j] & 
                                 Interpolate.Master[,"DaysId"]==i, 
                                 "PT.AM.PAC.Basic.Mobility.Score"])>0){
      
      DAYSID=c(DAYSID,i)
      
    }
    
  }
  
  #delete the initial 0
  DAYSID=DAYSID[-1]
  
  ###find the the follow up before the first instance of missing values in follow ups
  
  #initialize first
  first=0
  
  #cycle through the logged DaysIds for the given ID (first to penultimate)
  for (i in 1:(length(DAYSID)-1)){
    
    #check if the measurement for the current DaysId is nonmissing and the
    #the measurement for the next DaysId is missing
    if(is.na(Interpolate.Master[Interpolate.Master[,"ID"]==NewMasterIDs[j] & 
                                Interpolate.Master[,"DaysId"]==DAYSID[i] , 
                                "PT.AM.PAC.Basic.Mobility.Score"])==0 & 
       is.na(Interpolate.Master[Interpolate.Master[,"ID"]==NewMasterIDs[j] &
                                Interpolate.Master[,"DaysId"]==DAYSID[i+1], 
                                "PT.AM.PAC.Basic.Mobility.Score"])==1){
      
      #add this DaysId to the first vector
      first=c(first,DAYSID[i])
      
    }
    
  }
  
  #delete the 0
  first=first[-1]
  
  ###find the follow up after the last instance of missing values in follow ups
  
  #initialize last
  last=0
  
  #cycle through the logged DaysIds for the given ID (second to last)
  for (i in 2:length(DAYSID)){
    
    #check if the measurement for the current DaysId is nonmissing and the
    #the measurement for the previous DaysId is missing
    if(is.na(Interpolate.Master[Interpolate.Master[,"ID"]==NewMasterIDs[j] & 
                                Interpolate.Master[,"DaysId"]==DAYSID[i], 
                                "PT.AM.PAC.Basic.Mobility.Score"])==0 & 
       is.na(Interpolate.Master[Interpolate.Master[,"ID"]==NewMasterIDs[j] & 
                                Interpolate.Master[,"DaysId"]==DAYSID[i-1], 
                                "PT.AM.PAC.Basic.Mobility.Score"])==1){
      
      #add this DaysId to the last vector
      last=c(last,DAYSID[i])
      
    }
    
  }
  
  #delete the 0
  last=last[-1]
  
  #cycle through first and last vectors
  for (k in 1:length(first)){
    
    #find the number of missing values between the nonmissing observation before
    #missing values occur and the nonmissing observation after they stop occurring
    Gap.Num=(which(DAYSID==last[k]) - which(DAYSID==first[k]))
    
    ###Check to see if there were any missing values to interpolate in the first place 
    
    if (length(Gap.Num)!=0 & length(last[k]!=0)){
      
      ###find the slope for interpolation for each score
      
      y2m=Interpolate.Master[Interpolate.Master[,"ID"]==NewMasterIDs[j] & 
                               Interpolate.Master[,"DaysId"]==DAYSID[which(DAYSID==last[k])], 
                             "PT.AM.PAC.Basic.Mobility.Score"]
      
      y1m=Interpolate.Master[Interpolate.Master[,"ID"]==NewMasterIDs[j] & 
                               Interpolate.Master[,"DaysId"]==DAYSID[which(DAYSID==first[k])], 
                             "PT.AM.PAC.Basic.Mobility.Score"]
      
      mobility.slope=(y2m-y1m)/Gap.Num
      
      y2a=Interpolate.Master[Interpolate.Master[,"ID"]==NewMasterIDs[j] & 
                               Interpolate.Master[,"DaysId"]==DAYSID[which(DAYSID==last[k])], 
                             "OT.AM.Daily.Activity.Score"]
      
      y1a=Interpolate.Master[Interpolate.Master[,"ID"]==NewMasterIDs[j] & 
                               Interpolate.Master[,"DaysId"]==DAYSID[which(DAYSID==first[k])], 
                             "OT.AM.Daily.Activity.Score"]
      
      activity.slope=(y2a-y1a)/Gap.Num
      
      y2c=Interpolate.Master[Interpolate.Master[,"ID"]==NewMasterIDs[j] & 
                               Interpolate.Master[,"DaysId"]==DAYSID[which(DAYSID==last[k])], 
                             "ST.AM.Applied.Cogn.Score"]
      
      y1c=Interpolate.Master[Interpolate.Master[,"ID"]==NewMasterIDs[j] & 
                               Interpolate.Master[,"DaysId"]==DAYSID[which(DAYSID==first[k])], 
                             "ST.AM.Applied.Cogn.Score"]
      
      cognitive.slope=(y2c-y1c)/Gap.Num
      
      ###fill in the missing values with the interpolations
      
      #cycle through the observations with missing values that can be interpolated
      for (i in 1:(Gap.Num-1)){
        
        #replace NA with the interpolation
        Interpolate.Master[Interpolate.Master[,"ID"]==NewMasterIDs[j] & 
                             Interpolate.Master[,"DaysId"]==DAYSID[which(DAYSID==first[k]) + i], 
                           c("PT.AM.PAC.Basic.Mobility.Score", "OT.AM.Daily.Activity.Score", "ST.AM.Applied.Cogn.Score")]=
          c((y1m + i*mobility.slope), (y1a + i*activity.slope), (y1c + i*cognitive.slope))
      }
    }
    
  }
  
}


###Create a dataset that only contains 1 datapoint for each patient

Interpolate.Master.One=rbind(Interpolate.Master[Interpolate.Master[,"DaysId"]==3,], 
                             Interpolate.Master[(Interpolate.Master[,"DaysId"]==11 & Interpolate.Master[,"ID"]==74) | 
                                                  (Interpolate.Master[,"DaysId"]==10 & Interpolate.Master[,"ID"]==214),]) 

Interpolate.Master.One=Interpolate.Master.One[order(Interpolate.Master.One[,"ID"]),]


###Find propensity score for each observation for propensity score matching based on
###medical history variables

Medical.History.Variables=colnames(Interpolate.Master.One)[47:69]

fmla=as.formula(paste("Group ~ ", paste(Medical.History.Variables, collapse="+")))

glm.out=glm(formula=fmla, family=binomial(logit),
            data=Interpolate.Master.One[,c("Group", Medical.History.Variables)])

Interpolate.Master.One[,"Propensity.Score"]=glm.out$fitted.values


# View(NewMaster[,c("ID", "DaysId", "PT.AM.PAC.Basic.Mobility.Score", 
#                   "OT.AM.Daily.Activity.Score" ,"ST.AM.Applied.Cogn.Score")])
# 
# View(Interpolate.Master[,c("ID", "DaysId", "PT.AM.PAC.Basic.Mobility.Score", 
#                   "OT.AM.Daily.Activity.Score" ,"ST.AM.Applied.Cogn.Score")])









#####################################Function#######################################
#Name: matching                                                                    #
#Author: Traymon Beavers                                                           #
#Date Created: 3/29/2017                                                           #
#Purpose: To match the data based on gender, race, age, baseline average score and #
#         type of stroke for mobility, activity, or cognitive ability or print out #
#         matching IDs                                                             #
#Variables: AgeNum-width for age partial matching                                  #
#           BaselineNum-width for baseline average partial matching                #
#           ScoreNum-choice for which baseline average: 1 for Mobility, 2 for      #
#                    Activity, and 3 for Cognitive                                 #
#           PScoreNum-width for propensity score partial matching                  #
#           MahalNum-percentage for quantile for mahalanobis distances (based on   # 
#                    baseline averages for the three different scores): control    #
#                    patients are then only matched to study group patients if     #
#                    their mahalanobis distance is below this quantile             #       
#                                                                                  #
####################################################################################

matching=function(AgeNum=2, BaselineMobNum=20, BaselineActNum=20, 
                  BaselineCogNum=20, PScoreNum=1, FacAdjNum=5){
  
  ###Create a data matrix for the characteristics to be used for matching for each patient 
  ###in the study group
  
  Study.Characteristics=NewMaster.One[NewMaster.One[,"Group"]=="Study Group",
                                      c("ID", "Age", "Gender", "New.Race", 
                                        "Baseline.Mobility", 
                                        "Baseline.Activity",
                                        "Baseline.Cognitive",
                                        "Type.of.Stroke",
                                        "ARHosp.JRI.Facility.Adjustor")]
  
  pb=txtProgressBar(min=0, max=dim(NewMaster.One.Control)[1], initial=0)
  
  ###Create vectors for the rows corresponding to each patient's observations in the study 
  ###groupvand control group respectively
  
  # StudyRow.Index=which(NewMaster.One[,"Group"]=="Study Group")
  # ControlRow.Index=which(NewMaster.One[,"Group"]=="Control Group")
  
  ###calculate the quantile to be used when matching on mahalanobis distance
  
  #delta=quantile(mahal.dist.vec, MahalNum)

  ###initialize a vector to contain the matches
  result=c(0,0,0)
  
  ###begin the counter for pair ID
  T=1
  
  ###match control group IDs to study group IDs based on the inputs given
  for (i in 1:dim(NewMaster.One.Control)[1]){
    
    setTxtProgressBar(pb,i)
    
    for (j in 1:dim(Study.Characteristics)[1]){
      
      if (NewMaster.One.Control[i,"Age"]>=(Study.Characteristics[j,"Age"] - AgeNum) &
          NewMaster.One.Control[i,"Age"]<=(Study.Characteristics[j,"Age"] + AgeNum) &
          NewMaster.One.Control[i, "Baseline.Mobility"]>=(Study.Characteristics[j, "Baseline.Mobility"] - BaselineMobNum) &
          NewMaster.One.Control[i, "Baseline.Mobility"]<=(Study.Characteristics[j, "Baseline.Mobility"] + BaselineMobNum) &
          NewMaster.One.Control[i, "Baseline.Activity"]>=(Study.Characteristics[j, "Baseline.Activity"] - BaselineActNum) &
          NewMaster.One.Control[i, "Baseline.Activity"]<=(Study.Characteristics[j, "Baseline.Activity"] + BaselineActNum) &
          NewMaster.One.Control[i, "Baseline.Cognitive"]>=(Study.Characteristics[j, "Baseline.Cognitive"] - BaselineCogNum) &
          NewMaster.One.Control[i, "Baseline.Cognitive"]<=(Study.Characteristics[j, "Baseline.Cognitive"] + BaselineCogNum) &
          NewMaster.One.Control[i,"Gender"]==Study.Characteristics[j,"Gender"] &
          NewMaster.One.Control[i,"New.Race"]==Study.Characteristics[j,"New.Race"] &
          NewMaster.One.Control[i,"Type.of.Stroke"]==Study.Characteristics[j,"Type.of.Stroke"] &
          # NewMaster.One.Control[i,"Propensity.Score"]>=(Study.Characteristics[j,"Propensity.Score"] - PScoreNum) &
          # NewMaster.One.Control[i,"Propensity.Score"]<=(Study.Characteristics[j,"Propensity.Score"] + PScoreNum) &
          NewMaster.One.Control[i,"ARHosp.JRI.Facility.Adjustor"]>=(Study.Characteristics[j,"ARHosp.JRI.Facility.Adjustor"] - FacAdjNum) & 
          NewMaster.One.Control[i,"ARHosp.JRI.Facility.Adjustor"]<=(Study.Characteristics[j,"ARHosp.JRI.Facility.Adjustor"] + FacAdjNum)){
          #mahalanobis(X[ControlRow.Index[i],], X[StudyRow.Index[j],], SX)<=delta){
      
      result=rbind(result, c(T, Study.Characteristics[j,"ID"], 1), c(T, NewMaster.One.Control[i,"ID"], 0))
      
      T=T+1
      
      }
      
    }
    
  }
  
  close(pb)
  
  colnames(result)=c("PairID", "ID", "Group")
  
  
  ###delete the values used to initialize the vector and order the observations by study 
  ###group ID and control group ID
  result=result[-1,]

  return(result)
    
}

#################################End Function#######################################

#####Check function####

matchrows=matching(AgeNum = 5 , BaselineMobNum=20, BaselineActNum=20, 
                   BaselineCogNum=30, PScoreNum = 1, FacAdjNum = 3)

length(unique(matchrows[matchrows[,3]==1,2]))

# 
# View(matchrows)

####Check creation of unique (up to Study ID) one to one matched patients#### 



###use this for loop to find the seed that produces the most unique matches
A=0
B=0

#for (k in 1:1000){

matchrow.final=c(0,0,0)
  
for (i in unique(matchrows[matchrows[,3]==1,2])){
  
  set.seed(303)
  
  N=length(which(matchrows[,2]==i))
  
  j=ceiling(runif(1,0,N))
  
  matchrow.final=rbind(matchrow.final,
                       matchrows[which(matchrows[,2]==i)[j],],
                       matchrows[which(matchrows[,2]==i)[j]+1,])
  
}

matchrow.final=matchrow.final[-1,]

# Non.Missing.Num=0
# 
# for (j in unique(matchrow.final[,2])){
# 
#   Non.Missing.Num=Non.Missing.Num + length(Interpolate.Master[Interpolate.Master[,"ID"]==j &
#                                                               Interpolate.Master[,"DaysId"]>=1 &
#                                                               Interpolate.Master[,"DaysId"]<=8,
#                                                             "PT.AM.PAC.Basic.Mobility.Score"])-sum(is.na(Interpolate.Master[Interpolate.Master[,"ID"]==j &
#                                                                                                               Interpolate.Master[,"DaysId"]>=1 &
#                                                                                                               Interpolate.Master[,"DaysId"]<=8,
#                                                                                                             "PT.AM.PAC.Basic.Mobility.Score"]))
# 
# 
# }
# 
# A=c(A,Non.Missing.Num)
# B=c(B,length(unique(matchrow.final[matchrow.final[,3]==0,2])))
# 
# 
# if (length(unique(matchrow.final[matchrow.final[,3]==0,2]))>=33 & Non.Missing.Num>=513){
# 
#   print(k)
#   print(c(length(unique(matchrow.final[matchrow.final[,3]==0,2])),Non.Missing.Num))
# 
# }
# 
# }

max(A)
max(B)

###put different controls matched to same study the same strata

matchrow.final=matchrow.final[order(matchrow.final[,1]),]

for (i in unique(matchrow.final[matchrow.final[,3]==0,2])[1:length(unique(matchrow.final[matchrow.final[,3]==0,2]))]){
  
  if (length(which(matchrow.final[,2]==i))>1){
    
    for (j in 2:length(which(matchrow.final[,2]==i))){
      
      matchrow.final[which(matchrow.final[,2]==i)[j]-1,1]=matchrow.final[which(matchrow.final[,2]==i)[j-1]-1,1]
      matchrow.final[which(matchrow.final[,2]==i)[j],1]=matchrow.final[which(matchrow.final[,2]==i)[j-1],1]
      
    }  
    
  }
  
}

for (i in unique(matchrow.final[matchrow.final[,3]==0,2])[1:length(unique(matchrow.final[matchrow.final[,3]==0,2]))]){
  
  if (length(which(matchrow.final[,2]==i))>1){
    
      matchrow.final=matchrow.final[-which(matchrow.final[,2]==i)[2:length(which(matchrow.final[,2]==i))],]
      
  }
  
}

#View(matchrow.final[order(matchrow.final[,1], matchrow.final[,3]),])

####Check creation of unique one to one matched patients#### 

# #initialize the "first" row of the unique matchrows
# matchrows.one=c(0,0,0)
# 
# #initialize the "first" item of the control IDs that cannot be used again
# controlsdone=0
# 
# #Initialize vector for number of control matches for each study group match
# #Note: this is the largest matrix necessary since the worst case scenario
# #would be each StudyID have the same amount of matches as the StudyID
# #with the largest amount of matches
# control.match.lengths=matrix(rep(0, max(unique(matchrows[matchrows[,3]==1,2]))*2), nrow=max(unique(matchrows[matchrows[,3]==1,2]), ncol=2))
# 
# colnames(control.match.lengths)=c("Study ID", "Number of Controls Matched")
# 
# #cycle through the distinct study ID matches
# for (i in unique(matchrows[matchrows[,"Group"]==1,"ID"])){
#   
#   #place the study IDs and amount of controls they match into matrix,
#   #in that order
#   control.match.lengths[i,]=c(i,length((which(matchrows[,"ID"]==i)+1)))
#   
# }
# 
# #order the study IDs by control match lengths
# control.match.lengths=control.match.lengths[order(control.match.lengths[,"Number of Controls Matched"]),]
# 
# #only keep nonzero elements
# control.match.lengths.2=control.match.lengths[control.match.lengths[,"Study ID"]>0, ]
# 
# #initialize progress bar
# pb=txtProgressBar(min=0, max=dim(control.match.lengths.2)[1], initial=0)
# 
# #initialize progress counter
# G=0
# 
# #cycle through the Study Group IDs in order, from least amount of controls 
# #to most amount of controls
# for (i in control.match.lengths.2[,"Study ID"]){
#   
#   #make progress
#   G=G+1
#   
#   setTxtProgressBar(pb,G)
#   
#   #cycle through the row numbers of the controls matched to study ID i
#   for (j in 1:length((which(matchrows[,"ID"]==i)+1))){
#     
#     #initialize exit from next loop
#     Exit=0
#     
#     #cycle through the controls already matched
#     for (k in 1:length(unique(controlsdone))){
#   
#       Count=0
#       
#       #check if the jth control matched to study ID i is equal to
#       #the kth control in controlsdone
#       if (matchrows[(which(matchrows[,"ID"]==i)+1)[j],"ID"]==unique(controlsdone)[k] & Exit==0){
#       
#         #switch Exit to 1 to exit the for loop
#         Exit=1
#           
#       }else if (matchrows[(which(matchrows[,"ID"]==i)+1)[j],"ID"]!=controlsdone[k] & Exit==0 
#                 & (k==length(controlsdone))){
#         
#         #place the study and control match in matchrows.one
#         matchrows.one=rbind(matchrows.one,
#                             matchrows[matchrows[,"ID"]==i & matchrows[,"PairID"]==
#                                         matchrows[(which(matchrows[,"ID"]==i)+1)[j], "PairID"],],
#                             matchrows[matchrows[,"ID"]!=i & matchrows[,"PairID"]==
#                                         matchrows[(which(matchrows[,"ID"]==i)+1)[j], "PairID"],])
#         
#         #place this control ID in controlsdone
#         controlsdone=c(unique(controlsdone), matchrows[(which(matchrows[,"ID"]==i)+1)[j],"ID"])
#         
#         #switch Exit to 1 to exit the for loop
#         Exit=1
#     
#       }
#     }
#   }
# }
# 
# #close the progress bar
# close(pb)
# 
# matchrows.one=matchrows.one[-1,]
# 
# #dim(matchrows.one)[1]/2
# 
# matchrows.final=c(0,0,0)
# 
# for (i in (unique(matchrows.one[matchrows.one[,"Group"]==1,"ID"]))){
#   
#   if (length(which(matchrows.one[,"ID"]==i))==1){
#     
#     matchrows.final=rbind(matchrows.final,
#                           matchrows.one[which(matchrows.one[,"ID"]==i),],
#                           matchrows.one[which(matchrows.one[,"ID"]==i)+1,])
#                     
#   }
#   
# 
#   if (length(which(matchrows.one[,"ID"]==i))>=2){
# 
#       matchrows.final=rbind(matchrows.final,
#                             matchrows.one[which(matchrows.one[,"ID"]==i)[1],],
#                             matchrows.one[which(matchrows.one[,"ID"]==i)[1]+1,])
#   }
# 
# }
# 
# matchrows.final=matchrows.final[-1,]
# 
# matchrows.final=unique(matchrows.final[order(matchrows.final[,1], matchrows.final[,3]),])
# 
# dim(matchrows.final)[1]/2
# 
# View(matchrows.final)


# 
# for (i in 1:max(matchrows[,1])){
# 
# print(NewMaster.One[NewMaster.One[,"ID"]==matchrows[matchrows[,"PairID"]==i & matchrows[,"Group"]==1 ,2]
#                     | NewMaster.One[,"ID"]==matchrows[matchrows[,"PairID"]==i & matchrows[,"Group"]==0 ,2] ,
#                     c("ID", "Group", "Age", "Gender", "New.Race", "Baseline.Mobility",
#                       "Propensity.Score", "ARHosp.JRI.Facility.Adjustor")])
# 
# }



# for (i in 1:dim(control.match.lengths.2)[1]){
#   
#   print(control.match.lengths.2[i,1])
#   
#   print(matchrows[which(matchrows[,2]==control.match.lengths.2[i,1])+1,])
#   
# }
 
####Make matches for powerpoint####

View(NewMaster.One[NewMaster.One[,"ID"]==23 | NewMaster.One[,"ID"]==230,
              c("ID", "Group", "Age", "Gender", "New.Race", 
                "Baseline.Mobility", 
                "Baseline.Activity",
                "Baseline.Cognitive",
                "Type.of.Stroke",
                "ARHosp.JRI.Facility.Adjustor")])



##########################ANALYSIS OF CONTINUOUS PATIENT OUTCOMES##########################

#(5, 20, 20, 30, 1, 2) yields 38 study ID matches with 26 unique controls and 424 nonmissing obs
#seed 146
#max is 26 and 424

#(5, 20, 20, 30, 1, 3) yields 41 study ID matches with 29 unique controls and 453 nonmissing obs
#seed 303
#max is 29 and 453

#(5, 20, 20, 30, 1, 10) yields 45 study ID matches with 33 unique controls and 513 nonmissing obs
#seed 14
#max is 33 and 513

####follow up analysis####

follow.up.analysis=function(AgeNum = 5, BaselineMobNum=20, BaselineActNum=20, 
                            BaselineCogNum=30, PScoreNum = 1, FacAdjNum = 3,
                            ScoreNum=1, FollowUpNum=4){
  
  matchrows=matching(AgeNum = AgeNum , BaselineMobNum=BaselineMobNum, BaselineActNum=BaselineActNum, 
                     BaselineCogNum=BaselineCogNum, PScoreNum = PScoreNum, FacAdjNum = FacAdjNum)
  
  ####creation of unique (up to Control ID) one to one matched patients#### 
  
  matchrows.final=c(0,0,0)
  
  for (i in unique(matchrows[matchrows[,3]==1,2])){
    
    set.seed(303)
    
    N=length(which(matchrows[,2]==i))
    
    j=ceiling(runif(1,0,N))
    
    matchrows.final=rbind(matchrows.final,
                          matchrows[which(matchrows[,2]==i)[j],],
                          matchrows[which(matchrows[,2]==i)[j]+1,])
    
  }
  
  matchrows.final=matchrows.final[-1,]
  
  matchrows.final=matchrows.final[order(matchrows.final[,1]),]
  
  for (i in unique(matchrows.final[matchrows.final[,3]==0,2])[3:length(unique(matchrows.final[matchrows.final[,3]==0,2]))]){
    
    if (length(which(matchrows.final[,2]==i))>1){
      
      for (j in 2:length(which(matchrows.final[,2]==i))){
        
        matchrows.final[which(matchrows.final[,2]==i)[j]-1,1]=matchrows.final[which(matchrows.final[,2]==i)[j-1]-1,1]
        matchrows.final[which(matchrows.final[,2]==i)[j],1]=matchrows.final[which(matchrows.final[,2]==i)[j-1],1]
        
      }  
      
    }
    
  }
  
  matchrows.final=matchrows.final[order(matchrows.final[,1]),]
  
  Analysis.Data=c(0,0,0,0,0)
  
  drop.pair.IDs=0
  
  for (i in 1:dim(matchrows.final)[1]){
    
    if  (length(Interpolate.Master[Interpolate.Master[,"ID"]==matchrows.final[i,"ID"] & 
                            Interpolate.Master[,"DaysId"]==FollowUpNum, ScoreVarName[ScoreNum]])==1){
      
      #add the next element to the analysis dataset with their analysis variables  
      Analysis.Data=rbind(Analysis.Data, c(matchrows.final[i,1], matchrows.final[i,"ID"], 
                                           matchrows.final[i,3], Interpolate.Master[Interpolate.Master[,"ID"]==matchrows.final[i,"ID"] & 
                                                                                      Interpolate.Master[,"DaysId"]==FollowUpNum, ScoreVarName[ScoreNum]],
                                           Interpolate.Master[Interpolate.Master[,"ID"]==matchrows.final[i,"ID"] & 
                                                                Interpolate.Master[,"DaysId"]==FollowUpNum, paste("Baseline", ScoreName[ScoreNum], sep=".")]))
    
    }else{
      
      drop.pair.IDs=c(drop.pair.IDs, matchrows.final[i,1])
      
      
    }
    
  }
  
  Analysis.Data=Analysis.Data[-1,]
  
  drop.pair.IDs=drop.pair.IDs[-1]
  
  if (length(drop.pair.IDs)>0){
    
    for (j in 1:length(drop.pair.IDs)){
      
     Analysis.Data=Analysis.Data[-which(Analysis.Data[,1]==drop.pair.IDs[j]),] 
      
    }
    
  }
  
  colnames(Analysis.Data)=c("Pair ID", "ID", "Group", "Score", "Baseline")
  
  Controls=Analysis.Data[Analysis.Data[,"Group"]==0, "Score"]-Analysis.Data[Analysis.Data[,"Group"]==0, "Baseline"]
  Studys=Analysis.Data[Analysis.Data[,"Group"]==1, "Score"]-Analysis.Data[Analysis.Data[,"Group"]==1, "Baseline"]
  
  result=t.test(Studys, Controls, paired=TRUE, alternative = "greater")
  
  return(result)
  
}

for (i in 1:3){
  
  for (j in 4:7){
    
    print(follow.up.analysis(ScoreNum=i, FollowUpNum=j))
    
  }
  
}

####lmer analysis####

lmer.analysis=function(AgeNum = 5, BaselineMobNum=20, BaselineActNum=20, 
                       BaselineCogNum=30, PScoreNum = 1, FacAdjNum = 3,
                      ScoreNum=1, AverageRange=c(0,100,200), choice=1){
  
  require(lmerTest)
  
  #Store the number of ranges that will be created

  AverageNumber=length(AverageRange)
  
  #Create pairs from matching IDs so that matching is only one to one
    
  matchrows=matching(AgeNum = AgeNum , BaselineMobNum=BaselineMobNum, BaselineActNum=BaselineActNum, 
                     BaselineCogNum=BaselineCogNum, PScoreNum = PScoreNum, FacAdjNum = FacAdjNum)
  
  ####start of code to create unique one to one matches####
  # #initialize the "first" row of the unique matchrows
  # matchrows.one=c(0,0,0)
  # 
  # #initialize the "first" item of the control IDs that cannot be used again
  # controlsdone=0
  # 
  # #Initialize vector for number of control matches for each study group match
  # #Note: this is the largest matrix necessary since the worst case scenario
  # #would be each StudyID have the same amount of matches as the StudyID
  # #with the largest amount of matches
  # control.match.lengths=matrix(rep(0, max(unique(matchrows[matchrows[,3]==1,2]))*2), nrow=max(unique(matchrows[matchrows[,3]==1,2]), ncol=2))
  # 
  # colnames(control.match.lengths)=c("Study ID", "Number of Controls Matched")
  # 
  # 
  # #cycle through the distinct study ID matches
  # for (i in unique(matchrows[matchrows[,"Group"]==1,"ID"])){
  #   
  #   #place the study IDs and amount of controls they match into matrix,
  #   #in that order
  #   control.match.lengths[i,]=c(i,length((which(matchrows[,"ID"]==i)+1)))
  #   
  # }
  # 
  # #order the study IDs by control match lengths
  # control.match.lengths=control.match.lengths[order(control.match.lengths[,"Number of Controls Matched"]),]
  # 
  # #only keep nonzero elements
  # control.match.lengths.2=control.match.lengths[control.match.lengths[,"Study ID"]>0, ]
  # 
  # #initialize progress bar
  # pb=txtProgressBar(min=0, max=dim(control.match.lengths.2)[1], initial=0)
  # 
  # #initialize progress counter
  # G=0
  # 
  # #cycle through the Study Group IDs in order, from least amount of controls 
  # #to most amount of controls
  # for (i in control.match.lengths.2[,"Study ID"]){
  #   
  #   #make progress
  #   G=G+1
  #   
  #   setTxtProgressBar(pb,G)
  #   
  #   #cycle through the row numbers of the controls matched to study ID i
  #   for (j in 1:length((which(matchrows[,"ID"]==i)+1))){
  #     
  #     #initialize exit from next loop
  #     Exit=0
  #     
  #     #cycle through the controls already matched
  #     for (k in 1:length(unique(controlsdone))){
  #       
  #       Count=0
  #       
  #       #check if the jth control matched to study ID i is equal to
  #       #the kth control in controlsdone
  #       if (matchrows[(which(matchrows[,"ID"]==i)+1)[j],"ID"]==unique(controlsdone)[k] & Exit==0){
  #         
  #         #switch Exit to 1 to exit the for loop
  #         Exit=1
  #         
  #       }else if (matchrows[(which(matchrows[,"ID"]==i)+1)[j],"ID"]!=controlsdone[k] & Exit==0 
  #                 & (k==length(controlsdone))){
  #         
  #         #place the study and control match in matchrows.one
  #         matchrows.one=rbind(matchrows.one,
  #                             matchrows[matchrows[,"ID"]==i & matchrows[,"PairID"]==
  #                                         matchrows[(which(matchrows[,"ID"]==i)+1)[j], "PairID"],],
  #                             matchrows[matchrows[,"ID"]!=i & matchrows[,"PairID"]==
  #                                         matchrows[(which(matchrows[,"ID"]==i)+1)[j], "PairID"],])
  #         
  #         #place this control ID in controlsdone
  #         controlsdone=c(unique(controlsdone), matchrows[(which(matchrows[,"ID"]==i)+1)[j],"ID"])
  #         
  #         #switch Exit to 1 to exit the for loop
  #         Exit=1
  #         
  #       }
  #     }
  #   }
  # }
  # 
  # #close the progress bar
  # close(pb)
  # 
  # #delete the initial vector
  # matchrows.one=matchrows.one[-1,]
  # 
  # #initialize the "first" row of the final unique matchrows
  # matchrows.final=c(0,0,0)
  # 
  # #cycle through the distinct study group matches
  # for (i in (unique(matchrows.one[matchrows.one[,"Group"]==1,"ID"]))){
  #   
  #   #check if there is only one study group match available
  #   if (length(which(matchrows.one[,"ID"]==i))==1){
  #     
  #     #place this match in matchrows.final
  #     matchrows.final=rbind(matchrows.final,
  #                           matchrows.one[which(matchrows.one[,"ID"]==i),],
  #                           matchrows.one[which(matchrows.one[,"ID"]==i)+1,])
  #     
  #   }
  #   
  #   #check if there is more than one study group match available
  #   if (length(which(matchrows.one[,"ID"]==i))>=2){
  #     
  #     #place the first match in matchrows.final
  #     matchrows.final=rbind(matchrows.final,
  #                           matchrows.one[which(matchrows.one[,"ID"]==i)[1],],
  #                           matchrows.one[which(matchrows.one[,"ID"]==i)[1]+1,])
  #   }
  # }
  # 
  # #delete the initial vector
  # matchrows.final=matchrows.final[-1,]
  # 
  # #order by Pair ID and then Group
  # matchrows.final=unique(matchrows.final[order(matchrows.final[,"PairID"], matchrows.final[,"Group"]),])
  
  ####end code to create unique one to one matches####
  
  ####creation of unique (up to Study ID) one to one matched patients#### 
  
  matchrows.final=c(0,0,0)
  
  for (i in unique(matchrows[matchrows[,3]==1,2])){
    
    set.seed(303)
    
    N=length(which(matchrows[,2]==i))
    
    j=ceiling(runif(1,0,N))
    
    matchrows.final=rbind(matchrows.final,
                         matchrows[which(matchrows[,2]==i)[j],],
                         matchrows[which(matchrows[,2]==i)[j]+1,])
    
  }
  
  matchrows.final=matchrows.final[-1,]
  
  matchrows.final=matchrows.final[order(matchrows.final[,1]),]
  
  for (i in unique(matchrows.final[matchrows.final[,3]==0,2])[3:length(unique(matchrows.final[matchrows.final[,3]==0,2]))]){
    
    if (length(which(matchrows.final[,2]==i))>1){
      
      for (j in 2:length(which(matchrows.final[,2]==i))){
        
        matchrows.final[which(matchrows.final[,2]==i)[j]-1,1]=matchrows.final[which(matchrows.final[,2]==i)[j-1]-1,1]
        matchrows.final[which(matchrows.final[,2]==i)[j],1]=matchrows.final[which(matchrows.final[,2]==i)[j-1],1]
        
      }  
      
    }
    
  }
  
  matchrows.final=matchrows.final[order(matchrows.final[,1]),]
  
  ###Create new Interpolate.Master variable for averages of datapoints within time period
  
  Interpolate.Master.Analysis=Interpolate.Master
  
  #cycle through Average Time Index
  for (i in 1:AverageNumber){
    
    #initialize the variable for the average scores in time range i 
    Interpolate.Master[,paste("Average", i, sep=".")]=rep(NA,dim(Interpolate.Master)[1])
    
    #initialize the variable for the number of visits in time range i    
    Interpolate.Master[,paste("Num.Of.Visits", i, sep=".")]=rep(NA,dim(Interpolate.Master)[1])
    
    
  }
  
  #initialize a progress bar to track the progress of the creation of averages
  pb=txtProgressBar(min=0, max=dim(Interpolate.Master)[1], initial=0)
  
  #initialize the variable to cycle through the patient IDs
  T=1
  
  #cycle through the rows of Interpolate.Master
  for (i in 1:dim(Interpolate.Master)[1]){
    
    #update progress bar
    setTxtProgressBar(pb,i)
    
    
    #check if the current row has the same ID as the current ID
    if (Interpolate.Master[i,"ID"]!=NewMasterIDs[T] & T!=(length(NewMasterIDs))){
      
      #move to the next ID
      T=T+1
      
    }
    
    #cycle through time ranges
    for (j in 1:AverageNumber){
      
      #check if the current j is the final time range
      if(j!=AverageNumber){
    
        #calculate and store the average score for time range i for each patient
        Interpolate.Master.Analysis[i, paste("Average.Diff", j, sep="." )]=mean(Interpolate.Master.Analysis[Interpolate.Master.Analysis[,"Days.After.Assignment"]>AverageRange[j] &
                                                                    Interpolate.Master.Analysis[,"Days.After.Assignment"]<=AverageRange[j+1] & 
                                                                    Interpolate.Master.Analysis[,"DaysId"]>3 &
                                                                    Interpolate.Master.Analysis[,"ID"]==NewMasterIDs[T], 
                                                                  ScoreVarName[ScoreNum]], na.rm=TRUE)-Interpolate.Master.Analysis[Interpolate.Master.Analysis[,"ID"]==NewMasterIDs[T] 
                                                                                     & Interpolate.Master.Analysis[,"DaysId"]==1, ScoreVarName[ScoreNum]]

        #store the number of visits in time range i for each patient        
        Interpolate.Master.Analysis[i, paste("Num.Of.Visits", j, sep="." )]=length(Interpolate.Master.Analysis[Interpolate.Master.Analysis[,"Days.After.Assignment"]>AverageRange[j] &
                                                                                       Interpolate.Master.Analysis[,"Days.After.Assignment"]<=AverageRange[j+1] & 
                                                                                       Interpolate.Master.Analysis[,"DaysId"]>3 &
                                                                                       Interpolate.Master.Analysis[,"ID"]==NewMasterIDs[T], 
                                                                                     ScoreVarName[ScoreNum]])-sum(is.na(Interpolate.Master.Analysis[Interpolate.Master.Analysis[,"Days.After.Assignment"]>AverageRange[j] &
                                                                                                                                             Interpolate.Master.Analysis[,"Days.After.Assignment"]<=AverageRange[j+1] & 
                                                                                                                                             Interpolate.Master.Analysis[,"DaysId"]>3 &
                                                                                                                                             Interpolate.Master.Analysis[,"ID"]==NewMasterIDs[T], 
                                                                                                                                           ScoreVarName[ScoreNum]]))
      
      }else{
        
        #calculate and store the average score for time range i for each patient
        Interpolate.Master.Analysis[i, paste("Average.Diff", j, sep="." )]=mean(Interpolate.Master.Analysis[Interpolate.Master.Analysis[,"Days.After.Assignment"]>AverageRange[j] &
                                                                                       Interpolate.Master.Analysis[,"DaysId"]>3 &
                                                                    Interpolate.Master.Analysis[,"ID"]==NewMasterIDs[T], 
                                                                  ScoreVarName[ScoreNum] ], na.rm=TRUE)-Interpolate.Master.Analysis[Interpolate.Master.Analysis[,"ID"]==NewMasterIDs[T] 
                                                                                     & Interpolate.Master.Analysis[,"DaysId"]==1, ScoreVarName[ScoreNum]]
        
        #store the number of visits in time range i for each patient
        Interpolate.Master.Analysis[i, paste("Num.Of.Visits", j, sep="." )]=length(Interpolate.Master.Analysis[Interpolate.Master.Analysis[,"Days.After.Assignment"]>AverageRange[j] &
                                                                                               Interpolate.Master.Analysis[,"DaysId"]>3 &
                                                                                       Interpolate.Master.Analysis[,"ID"]==NewMasterIDs[T], 
                                                                                     ScoreVarName[ScoreNum]])-sum(is.na(Interpolate.Master.Analysis[Interpolate.Master.Analysis[,"Days.After.Assignment"]>AverageRange[j] &
                                                                                                                                             Interpolate.Master.Analysis[,"DaysId"]>3 &
                                                                                                                                             Interpolate.Master.Analysis[,"ID"]==NewMasterIDs[T], 
                                                                                                                                           ScoreVarName[ScoreNum]]))
        
      }
      
    }
  
  }
  
  #close the progress bar
  close(pb)
  
  
  #create a variable with one observation for each patient
  Interpolate.Master.Analysis.One=rbind(Interpolate.Master.Analysis[Interpolate.Master.Analysis[,"DaysId"]==3,], 
                               Interpolate.Master.Analysis[(Interpolate.Master.Analysis[,"DaysId"]==11 & Interpolate.Master.Analysis[,"ID"]==74) | 
                                                    (Interpolate.Master.Analysis[,"DaysId"]==10 & Interpolate.Master.Analysis[,"ID"]==214),]) 
  
  #order the dataset by patient ID
  Interpolate.Master.Analysis.One=Interpolate.Master.Analysis.One[order(Interpolate.Master.Analysis.One[,"ID"]),]
  
  ###Find propensity score for each observation in order to include propensity score as covariate
  
  # Medical.History.Variables=colnames(Interpolate.Master.Analysis.One)[47:69]
  # 
  # fmla=as.formula(paste("Group ~ ", paste(Medical.History.Variables, collapse="+")))
  # 
  # glm.out=glm(formula=fmla, family=binomial(logit),
  #             data=Interpolate.Master.Analysis.One[,c("Group", Medical.History.Variables)])
  # 
  # Interpolate.Master.Analysis.One[,"Propensity.Score"]=glm.out$fitted.values
  
  
  #set a vector for the variables to analyzed
  Analysis.Variables=c("ID", "Group", "Age", "Gender", "New.Race", "Type.of.Stroke",
                        paste("Baseline", ScoreName[ScoreNum], sep="."), 
                       "ARHosp.JRI.Facility.Adjustor")
  
  #cycle through the time indices
  for (i in 1:AverageNumber){
    
    #add the scores and number of visits for the time indices as analysis variables
    Analysis.Variables=c(Analysis.Variables, paste("Average.Diff", i, sep="."), 
                         paste("Num.Of.Visits", i, sep="."))
    
  }

  #add the first element of the first match to the analysis dataset with their analysis variables  
  Analysis.Data=Interpolate.Master.Analysis.One[Interpolate.Master.Analysis.One[,"ID"]==matchrows.final[1,"ID"],
                                   Analysis.Variables]
  
  #cycle through the elements in the final unique matches
  for (i in 2:dim(matchrows.final)[1]){
    
    #add the next element to the analysis dataset with their analysis variables  
    Analysis.Data=rbind(Analysis.Data, Interpolate.Master.Analysis.One[Interpolate.Master.Analysis.One[,"ID"]==matchrows.final[i,"ID"],
                                                                Analysis.Variables])
  }
  
  #add the pair ID for each element in the analysis dataset
  Analysis.Data[,"PairID"]=matchrows.final[,"PairID"]
  
  #make a copy of the analysis dataset for binding
  Analysis.Data.Copy=Analysis.Data

  for (i in 1:(AverageNumber-1)){

    #create a dataset to give a measurement for each time index
    Analysis.Data=rbind(Analysis.Data, Analysis.Data.Copy)

  }

  #order the analysis dataset by the Pair ID
  Analysis.Data=Analysis.Data[order(Analysis.Data[,"PairID"]),]

  #create a variable for the time index
  Analysis.Data[,"AverageNum"]=rep(rep((1:AverageNumber), each=2), (dim(matchrows.final)[1])/2)

  
  # for (l in 1:dim(Analysis.Data)[1]){
  # 
  #   if (Analysis.Data[l,"AverageNum"]==1){
  #     
  #     Analysis.Data[l,"AverageNum"]="First"
  #     
  #   }else if (Analysis.Data[l,"AverageNum"]==2){
  #     
  #     Analysis.Data[l,"AverageNum"]="Second"    
  #     
  #   }else{
  #     
  #     Analysis.Data[l,"AverageNum"]="Third"
  #     
  #   }
  # 
  # }
    
  #initialize variable columns that will be the Average Score Difference and the Number of Visits
  #for the given time index
  Analysis.Data=cbind(Analysis.Data, rep(NA, dim(Analysis.Data)[1]),
                      rep(NA, dim(Analysis.Data)[1]))

  #give column names to the variables initialized above
  colnames(Analysis.Data)=c(colnames(Analysis.Data)[1:(dim(Analysis.Data)[2]-2)],
                            "Average.Score.Diff", "Number.of.Visits")

  #create a vector for rehabilitation groups
  GroupName=c("Study Group", "Control Group")
  
  #create a vector for time index ranges
  #TimeIndex=c("First", "Second", "Third")
  
  TimeIndex=c(1,2,3)

  #cycle through the study ID matches
  for(k in unique(Analysis.Data[,"PairID"])){

    #cycle through the time indices
    for (i in 1:AverageNumber){

      #cycle through rehabilitation groups
      for (j in 1:2){

        #for pair ID k, time index i rehabilitation group j, store the
        #average score difference and number of visits for time index
        Analysis.Data[which(Analysis.Data[,"PairID"]==k & Analysis.Data[,"Group"]==GroupName[j]
                      & Analysis.Data[,"AverageNum"]==TimeIndex[i]),
                      c("Average.Score.Diff", "Number.of.Visits")]=
          c(Analysis.Data[Analysis.Data[,"PairID"]==k & Analysis.Data[,"Group"]==GroupName[j]
                          & Analysis.Data[,"AverageNum"]==TimeIndex[i]
                             , paste("Average.Diff", i, sep=".")],
            Analysis.Data[Analysis.Data[,"PairID"]==k & Analysis.Data[,"Group"]==GroupName[j]
                          & Analysis.Data[,"AverageNum"]==TimeIndex[i]
                          , paste("Num.Of.Visits", i, sep=".")])


      }

    }

  }

  
  # [1] "ID"                           "Group"                        "Age"                          "Gender"                      
  # [5] "New.Race"                     "Type.of.Stroke"               "Baseline.Mobility"            "ARHosp.JRI.Facility.Adjustor"
  # [9] "Average.Diff.1"               "Num.Of.Visits.1"              "Average.Diff.2"               "Num.Of.Visits.2"             
  # [13] "Average.Diff.3"               "Num.Of.Visits.3"              "PairID"                       "AverageNum"                  
  # [17] "Average.Score.Diff"           "Number.of.Visits"      

  #create a new set of analysis variables to include the four just created and the
  #requested covariates
  Analysis.Variables1=c(colnames(Analysis.Data)[c(2:5,7:8, (10+(2*AverageNumber)), dim(Analysis.Data)[2])])
  
  Analysis.Variables2=colnames(Analysis.Data)[c(2,16)]
  
  Analysis.Variables3=colnames(Analysis.Data)[c(2:5, 7:8)]

  fmla2=as.formula(paste("Average.Score.Diff ~ ", paste(c(Analysis.Variables1, "(ID | PairID)"), collapse="+"), "+ AverageNum*Number.of.Visits"))
  
  fmla3=as.formula(paste("Average.Score.Diff ~ ", paste(c("Group", "(ID | PairID)"), collapse="+")))

  fmla4=as.formula(paste("Average.Score.Diff ~ ", paste(c(Analysis.Variables2, "(ID | PairID)"), collapse="+"), "+ AverageNum*Group"))
    
  fmla5=as.formula(paste("Average.Score.Diff ~ ", paste(c(Analysis.Variables3, "(ID | PairID)"), collapse="+")))
  
  
  if (choice==1){
  
  result=lmerTest::lmer(data=Analysis.Data, fmla3)
  
  }else if (choice==2){
  
  result=lmerTest::lmer(data=Analysis.Data, fmla4)
  
  }else if (choice==3){
  
  result=lmerTest::lmer(data=Analysis.Data, fmla5)
  
  }

  return(result)
  
}

word=lmer.analysis(ScoreNum=1, AverageRange=0, choice=3)

summary(word)

word=lmer.analysis(ScoreNum=2, AverageRange=0, choice=3)

summary(word)

word=lmer.analysis(ScoreNum=3, AverageRange=0, choice=3)

summary(word)

word1=lmer.analysis(ScoreNum=1, AverageRange=0, choice=1)

word2=lmer.analysis(ScoreNum=1, AverageRange=0, choice=3)

anova(word1, word2)$`Pr(>Chisq)`

word1=lmer.analysis(ScoreNum=2, AverageRange=0, choice=1)

word2=lmer.analysis(ScoreNum=2, AverageRange=0, choice=3)

anova(word1, word2)$`Pr(>Chisq)`

word1=lmer.analysis(ScoreNum=3, AverageRange=0, choice=1)

word2=lmer.analysis(ScoreNum=3, AverageRange=0, choice=3)

anova(word1, word2)$`Pr(>Chisq)`


####slope analysis####

slope.analysis=function(AgeNum = 5, BaselineMobNum=20, BaselineActNum=20, 
                        BaselineCogNum=30, PScoreNum = 1, FacAdjNum = 2,
                        ScoreNum=1){
  
  ####matching part####
  
  matchrows=matching(AgeNum = AgeNum , BaselineMobNum=BaselineMobNum, BaselineActNum=BaselineActNum, 
                     BaselineCogNum=BaselineCogNum, PScoreNum = PScoreNum, FacAdjNum = FacAdjNum)
  
  matchrows.final=c(0,0,0)
  
  for (i in unique(matchrows[matchrows[,3]==1,2])){
    
    set.seed(983)
    
    N=length(which(matchrows[,2]==i))
    
    j=ceiling(runif(1,0,N))
    
    matchrows.final=rbind(matchrows.final,
                          matchrows[which(matchrows[,2]==i)[j],],
                          matchrows[which(matchrows[,2]==i)[j]+1,])
    
  }
  
  matchrows.final=matchrows.final[-1,]
  
  matchrows.final=matchrows.final[order(matchrows.final[,1]),]
  
  for (i in unique(matchrows.final[matchrows.final[,3]==0,2])[3:length(unique(matchrows.final[matchrows.final[,3]==0,2]))]){
    
    if (length(which(matchrows.final[,2]==i))>1){
      
      for (j in 2:length(which(matchrows.final[,2]==i))){
        
        matchrows.final[which(matchrows.final[,2]==i)[j]-1,1]=matchrows.final[which(matchrows.final[,2]==i)[j-1]-1,1]
        matchrows.final[which(matchrows.final[,2]==i)[j],1]=matchrows.final[which(matchrows.final[,2]==i)[j-1],1]
        
      }  
      
    }
    
  }
  
  matchrows.final=matchrows.final[order(matchrows.final[,1]),]

  ####data creation based on matching part####

  matchrows.final=matchrows.final[c(-1,-2, -25, -26, -39, -40, -41, -42, -45, -46, -51, -52, -55, -56, -73:-76  ), ]

  #add the first element of the first match to the analysis dataset with their analysis variables

  Analysis.Data=c(0,0,0,0)


  #cycle through the elements in the final unique matches
  for (i in 1:dim(matchrows.final)[1]){

    #add the next element to the analysis dataset with their analysis variables
    Analysis.Data=rbind(Analysis.Data, c(matchrows.final[i,1], matchrows.final[i,"ID"], matchrows.final[i,3],
                                         summary(lm(Interpolate.Master[Interpolate.Master[,"ID"]==matchrows.final[i,"ID"] & Interpolate.Master[,"DaysId"]<9 & Interpolate.Master[,"DaysId"]>2,ScoreVarName[ScoreNum]]~
                                                      Interpolate.Master[Interpolate.Master[,"ID"]==matchrows.final[i,"ID"] & Interpolate.Master[,"DaysId"]<9 & Interpolate.Master[,"DaysId"]>2,"Days.After.Assignment"]))$coefficients[2,1]))
  }

  Analysis.Data=Analysis.Data[-1,]

  colnames(Analysis.Data)=c("Pair ID", "ID", "Group", "Slope")

  Controls=Analysis.Data[Analysis.Data[,"Group"]==0, "Slope"]
  Studys=Analysis.Data[Analysis.Data[,"Group"]==1, "Slope"]

  result=t.test(Studys, Controls, paired=TRUE, alternative = "greater")
  
  return(result)
  
}

slope.analysis(ScoreNum=1)$p.value

slope.analysis(ScoreNum=2)$p.value

slope.analysis(ScoreNum=3)$p.value



####find patients without at least two points to compare####

matchrow.final=word


for (i in 1:dim(matchrow.final)[1]){
  
  N=length(Interpolate.Master[Interpolate.Master[,"ID"]==matchrow.final[i,2] & 
                                Interpolate.Master[,"DaysId"]>2 &
                                Interpolate.Master[,"DaysId"]<9, "PT.AM.PAC.Basic.Mobility.Score"])
  
  if (N<2 | N-sum(is.na(Interpolate.Master[Interpolate.Master[,"ID"]==matchrow.final[i,2] & 
                         Interpolate.Master[,"DaysId"]>2 &
                         Interpolate.Master[,"DaysId"]<9, "PT.AM.PAC.Basic.Mobility.Score"]))<2){
    
    print(matchrow.final[i,2])
    
  }
  
}

View(matchrow.final)


#################################PLOTS###########################

for (j in 1:3){

  #plot for Study Group
  plot(-100,-100, xlim=c(0,400), ylim=c(-30,130), ylab=paste(ScoreName[j], "Score"), 
       xlab="Days After Stroke",
       main=paste(ScoreName[j], "Score by Days After Stroke for Study Group"))
  
  for (i in StudyGroupIDs){
    
    if (i!=103 | i!=212){
      
      points(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), "Days_afterOnset"],
             Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), ScoreVarName[j]],
             pch=19,col="blue")

      points(Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), "Days_afterOnset"],
             Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), ScoreVarName[j]],
             pch=19,col="purple")
      
      
      points(Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"],
             Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]], 
             pch=19, col="green")
      
      lines(c(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), "Days_afterOnset"],
              Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), "Days_afterOnset"],
              Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"]),
            c(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), ScoreVarName[j]],
              Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), ScoreVarName[j]],
              Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]]))
    }
    
  }
  
  legend("topright", legend=c("Scores Before Group Assignment", "Score at Assignment", "Study Group"),
         col=c("blue", "purple", "green"), pch=c(19,19,19), cex=0.65)
   
  # abline(v=31, col="black")
  # abline(v=160, col="black")
  # abline(v=230, col="black")

  #plot for Control Group
  plot(-100,-100, xlim=c(0,400), ylim=c(-30,130), ylab=paste(ScoreName[j], "Score"), 
       xlab="Days After Stroke",
       main=paste(ScoreName[j], "Score by Days After Stroke for Control Group"))
  
  for (i in ControlGroupIDs){
    
    if (i!=103 | i!=212){
      
      points(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), "Days_afterOnset"],
             Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), ScoreVarName[j]],
             pch=19,col="blue")
      
      points(Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), "Days_afterOnset"],
             Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), ScoreVarName[j]],
             pch=19,col="purple")
      
      
      points(Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"],
             Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]], 
             pch=19, col="red")
      
      lines(c(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), "Days_afterOnset"],
              Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), "Days_afterOnset"],
              Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"]),
            c(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), ScoreVarName[j]],
              Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), ScoreVarName[j]],
              Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]]))
    }
    
  }
  
  legend("topright", legend=c("Scores before Assignment", "Score at Assignment", "Control Group"),
         col=c("blue", "purple", "red"), pch=c(19,19,19), cex=0.65)
  
  # abline(v=31, col="black")
  # abline(v=160, col="black")
  # abline(v=230, col="black")

}


# plot(-100,-100, xlim=c(-50,365), ylim=c(-30,130), ylab=paste("Mobility Score"), 
#      xlab="Days Since Group Assignment",
#      main=paste("Mobility Score by Days Since Group Assignment for ID's 111 and 269"))
# 
# points(Interpolate.Master[(Interpolate.Master[,"ID"]==269 & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"],
#        Interpolate.Master[(Interpolate.Master[,"ID"]==269 & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]], 
#        pch=19, col="green")
# 
# points(Interpolate.Master[(Interpolate.Master[,"ID"]==111 & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"],
#        Interpolate.Master[(Interpolate.Master[,"ID"]==111 & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]], 
#        pch=19, col="red")
# 
# legend("topright", legend=c("Scores Before Group Assignment", "Score at Assignment", "Control Group", "Study Group"),
#        col=c("blue", "purple", "red", "green"), pch=c(19,19,19, 19), cex=0.65)
# 
# 
# for (i in c(269,111)){
#   
#   if (i!=103 | i!=212){
#     
#     points(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), "Days_afterOnset"],
#            Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), ScoreVarName[j]],
#            pch=19,col="blue")
#     
#     points(Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==1)), "Zero"],
#            Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==1)), ScoreVarName[j]],
#            pch=19,col="purple")
#     
#     lines(c(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), "Days_afterOnset"],
#             Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==1)), "Zero"],
#             Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"]),
#           c(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), ScoreVarName[j]],
#             Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==1)), ScoreVarName[j]],
#             Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]]))
#   }
#   
# }




##############################ANALYSIS OF BINARY PATIENT OUTCOMES##################################

###Create cohesive variable for readmission and readmission reason

NewMaster.One[,"Readmission"]=rep(0,dim(NewMaster.One)[1])

NewMaster.One[,"Readmission.Reason.CVA"]=rep(0,dim(NewMaster.One)[1])

for (i in which(NewMaster[,"Been.Hospitalised.since.stroke"]=="Yes")){
  
    NewMaster.One[NewMaster.One[,"ID"]==i,"Readmission"]=1
    
}
  
for (i in which(NewMaster[,"Hospitalized.Reason"]=="CVA")){
  
  NewMaster.One[NewMaster.One[,"ID"]==i,"Readmission.Reason.CVA"]=1
  
}
  
sum(NewMaster.One[NewMaster.One[,"Group"]=="Study Group","Readmission.Reason.CVA"])

##################################DEMOGRAPHICS CHART#################################

# Demo Chart for entire group ####

options(digits=5)

demo.chart=as.data.frame(matrix(NA,60,5))

rownames(demo.chart)=c("Age", "Male", "Female", "Deceased", "White", "Black", "Other", 
                       colnames(NewMaster.One[c(9,12,19:25,31:34,38:45, 47:69, 75:76, 78:81, 119:121)]))

colnames(demo.chart)=c("Study Group", "Control Group", "P-value", "Conf Int. L", "Conf Int. U")


###Sample Size

demo.chart["n","Study Group"]=length(NewMaster.One[NewMaster.One[,"Group"]=="Study Group","Age"])

demo.chart["n","Control Group"]=length(NewMaster.One[NewMaster.One[,"Group"]=="Control Group","Age"])

###Age

demo.chart["Age","Study Group"]=mean(NewMaster.One[NewMaster.One[,"Group"]=="Study Group","Age"])

demo.chart["Age","Control Group"]=mean(NewMaster.One[NewMaster.One[,"Group"]=="Control Group","Age"])

demo.chart["Age",3]=t.test(NewMaster.One[NewMaster.One[,"Group"]=="Study Group", "Age"],
                           NewMaster.One[NewMaster.One[,"Group"]=="Control Group", "Age"])$p.value

demo.chart["Age",4:5]=t.test(NewMaster.One[NewMaster.One[,"Group"]=="Study Group", "Age"],
                             NewMaster.One[NewMaster.One[,"Group"]=="Control Group", "Age"])$conf.int[1:2]

###Gender

demo.chart["Male","Study Group"]=sum(NewMaster.One[,"Group"]=="Study Group" & NewMaster.One[,"Gender"]=="Male")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Study Group",])[1]

demo.chart["Male","Control Group"]=sum(NewMaster.One[,"Group"]=="Control Group" & NewMaster.One[,"Gender"]=="Male")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Control Group",])[1]

demo.chart["Female","Study Group"]=sum(NewMaster.One[,"Group"]=="Study Group" & NewMaster.One[,"Gender"]=="Female")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Study Group",])[1]

demo.chart["Female","Control Group"]=sum(NewMaster.One[,"Group"]=="Control Group" & NewMaster.One[,"Gender"]=="Female")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Control Group",])[1]

demo.chart["Female",3]=prop.test(table(NewMaster.One[NewMaster.One[,"Group"]=="Study Group" | 
                                                        NewMaster.One[,"Group"]=="Control Group", "Group"], 
                                       NewMaster.One[, "Gender"])[-2:-3,], correct=FALSE)$p.value

demo.chart["Female",4:5]=prop.test(table(NewMaster.One[NewMaster.One[,"Group"]=="Study Group" | 
                                                          NewMaster.One[,"Group"]=="Control Group", "Group"], 
                                         NewMaster.One[, "Gender"])[-2:-3,], correct=FALSE)$conf.int[1:2]

###Deceased

demo.chart["Deceased","Study Group"]=sum(NewMaster.One[NewMaster.One[,"Group"]=="Study Group","Deceased_Y.N"])/dim(NewMaster.One[NewMaster.One[,"Group"]=="Study Group",])[1]

demo.chart["Deceased","Control Group"]=sum(NewMaster.One[NewMaster.One[,"Group"]=="Control Group","Deceased_Y.N"])/dim(NewMaster.One[NewMaster.One[,"Group"]=="Control Group",])[1]

demo.chart["Deceased",3]=prop.test(table(NewMaster.One[NewMaster.One[,"Group"]=="Study Group" | 
                                                          NewMaster.One[,"Group"]=="Control Group", "Group"], 
                                         NewMaster.One[, "Deceased_Y.N"])[-2:-3,], correct=FALSE)$p.value

demo.chart["Deceased",4:5]=prop.test(table(NewMaster.One[NewMaster.One[,"Group"]=="Study Group" | 
                                                            NewMaster.One[,"Group"]=="Control Group", "Group"], 
                                           NewMaster.One[, "Deceased_Y.N"])[-2:-3,], correct=FALSE)$conf.int[1:2]

###Race

demo.chart["White","Study Group"]=sum(NewMaster.One[,"Group"]=="Study Group" & NewMaster.One[,"New.Race"]=="White")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Study Group",])[1]

demo.chart["White","Control Group"]=sum(NewMaster.One[,"Group"]=="Control Group" & NewMaster.One[,"New.Race"]=="White")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Control Group",])[1]

demo.chart["Black","Study Group"]=sum(NewMaster.One[,"Group"]=="Study Group" & NewMaster.One[,"New.Race"]=="Black")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Study Group",])[1]

demo.chart["Black","Control Group"]=sum(NewMaster.One[,"Group"]=="Control Group" & NewMaster.One[,"New.Race"]=="Black")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Control Group",])[1]

demo.chart["Other","Study Group"]=sum(NewMaster.One[,"Group"]=="Study Group" & NewMaster.One[,"New.Race"]=="Other")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Study Group",])[1]

demo.chart["Other","Control Group"]=sum(NewMaster.One[,"Group"]=="Control Group" & NewMaster.One[,"New.Race"]=="Other")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Control Group",])[1]

demo.chart["Other",3]=chisq.test(table(NewMaster.One[,c("New.Race", "Group")])[,c(1,4)])$p.value

###Hispanic

demo.chart["Hispanic.Ethnicity","Study Group"]=sum(NewMaster.One[,"Group"]=="Study Group" & NewMaster.One[,"Hispanic.Ethnicity"]=="Yes")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Study Group",])[1]

demo.chart["Hispanic.Ethnicity","Control Group"]=sum(NewMaster.One[,"Group"]=="Control Group" & NewMaster.One[,"Hispanic.Ethnicity"]=="Yes")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Control Group",])[1]

demo.chart["Hispanic.Ethnicity",3]=prop.test(table(NewMaster.One[NewMaster.One[,"Group"]=="Study Group" | 
                                                                    NewMaster.One[,"Group"]=="Control Group", "Group"], 
                                                   NewMaster.One[, "Hispanic.Ethnicity"])[-2:-3,-2], correct=FALSE)$p.value

demo.chart["Hispanic.Ethnicity",4:5]=prop.test(table(NewMaster.One[NewMaster.One[,"Group"]=="Study Group" | 
                                                                      NewMaster.One[,"Group"]=="Control Group", "Group"], 
                                                     NewMaster.One[, "Hispanic.Ethnicity"])[-2:-3,-2], correct=FALSE)$conf.int[1:2]

###Insurance Type

demo.chart["Medicaid","Study Group"]=sum(NewMaster.One[,"Group"]=="Study Group" & NewMaster.One[,"Health.Insurance.Name"]=="Medicaid")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Study Group",])[1]

demo.chart["Medicaid","Control Group"]=sum(NewMaster.One[,"Group"]=="Control Group" & NewMaster.One[,"Health.Insurance.Name"]=="Medicaid")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Control Group",])[1]

demo.chart["Medicare","Study Group"]=sum(NewMaster.One[,"Group"]=="Study Group" & NewMaster.One[,"Health.Insurance.Name"]=="Medicare")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Study Group",])[1]

demo.chart["Medicare","Control Group"]=sum(NewMaster.One[,"Group"]=="Control Group" & NewMaster.One[,"Health.Insurance.Name"]=="Medicare")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Control Group",])[1]

demo.chart["Private/VA/Champus/Other Insurance","Study Group"]=sum(NewMaster.One[,"Group"]=="Study Group" & NewMaster.One[,"Health.Insurance.Name"]=="Private/VA/Champus/Other Insurance")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Study Group",])[1]

demo.chart["Private/VA/Champus/Other Insurance","Control Group"]=sum(NewMaster.One[,"Group"]=="Control Group" & NewMaster.One[,"Health.Insurance.Name"]=="Private/VA/Champus/Other Insurance")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Control Group",])[1]

demo.chart["Self Pay/No Insurance","Study Group"]=sum(NewMaster.One[,"Group"]=="Study Group" & NewMaster.One[,"Health.Insurance.Name"]=="Self Pay/No Insurance")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Study Group",])[1]

demo.chart["Self Pay/No Insurance","Control Group"]=sum(NewMaster.One[,"Group"]=="Control Group" & NewMaster.One[,"Health.Insurance.Name"]=="Self Pay/No Insurance")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Control Group",])[1]

demo.chart["Self Pay/No Insurance",3]=chisq.test(table(NewMaster.One[,c("Health.Insurance.Name", "Group")])[2:4,c(1,4)])$p.value

###Education Level

demo.chart["Education.Level","Study Group"]=mean(NewMaster.One[NewMaster.One[,"Group"]=="Study Group" & NewMaster.One[,"Education.Level"]<100, "Education.Level"])

demo.chart["Education.Level","Control Group"]=mean(NewMaster.One[NewMaster.One[,"Group"]=="Control Group" & NewMaster.One[,"Education.Level"]<100, "Education.Level"])

demo.chart["Education.Level",3]=t.test(NewMaster.One[NewMaster.One[,"Group"]=="Study Group" & NewMaster.One[,"Education.Level"]<100, "Education.Level"],
                                       NewMaster.One[NewMaster.One[,"Group"]=="Control Group" & NewMaster.One[,"Education.Level"]<100, "Education.Level"])$p.value

demo.chart["Education.Level",4:5]=t.test(NewMaster.One[NewMaster.One[,"Group"]=="Study Group" & NewMaster.One[,"Education.Level"]<100, "Education.Level"],
                                         NewMaster.One[NewMaster.One[,"Group"]=="Control Group" & NewMaster.One[,"Education.Level"]<100, "Education.Level"])$conf.int[1:2]

###Type of Stroke

demo.chart["HEMORRHAGIC - INTRACEREBRAL/INTRACEREBELLAR/INTRAP","Study Group"]=sum(NewMaster.One[,"Group"]=="Study Group" & NewMaster.One[,"Type.of.Stroke"]=="HEMORRHAGIC - INTRACEREBRAL/INTRACEREBELLAR/INTRAP")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Study Group",])[1]

demo.chart["HEMORRHAGIC - INTRACEREBRAL/INTRACEREBELLAR/INTRAP","Control Group"]=sum(NewMaster.One[,"Group"]=="Control Group" & NewMaster.One[,"Type.of.Stroke"]=="HEMORRHAGIC - INTRACEREBRAL/INTRACEREBELLAR/INTRAP")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Control Group",])[1]

demo.chart["ISCHEMIC CVA","Study Group"]=sum(NewMaster.One[,"Group"]=="Study Group" & NewMaster.One[,"Type.of.Stroke"]=="ISCHEMIC CVA")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Study Group",])[1]

demo.chart["ISCHEMIC CVA","Control Group"]=sum(NewMaster.One[,"Group"]=="Control Group" & NewMaster.One[,"Type.of.Stroke"]=="ISCHEMIC CVA")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Control Group",])[1]

demo.chart["SUBARACHNOID HEMORRHAGE","Study Group"]=sum(NewMaster.One[,"Group"]=="Study Group" & NewMaster.One[,"Type.of.Stroke"]=="SUBARACHNOID HEMORRHAGE")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Study Group",])[1]

demo.chart["SUBARACHNOID HEMORRHAGE","Control Group"]=sum(NewMaster.One[,"Group"]=="Control Group" & NewMaster.One[,"Type.of.Stroke"]=="SUBARACHNOID HEMORRHAGE")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Control Group",])[1]

x=table(NewMaster.One[,c("Type.of.Stroke", "Group")])[c(1,2),c(1,4)] + matrix(c(table(NewMaster.One[,c("Type.of.Stroke", "Group")])[3,1],
                                                                                 0,table(NewMaster.One[,c("Type.of.Stroke", "Group")])[3,4], 0), nrow=2)

demo.chart["SUBARACHNOID HEMORRHAGE",3]=chisq.test(x)$p.value

###Hemiparesis

demo.chart["Hemiparesis_Left","Study Group"]=sum(NewMaster.One[,"Group"]=="Study Group" & NewMaster.One[,"Hemiparesis_Left"]=="Yes")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Study Group",])[1]

demo.chart["Hemiparesis_Left","Control Group"]=sum(NewMaster.One[,"Group"]=="Control Group" & NewMaster.One[,"Hemiparesis_Left"]=="Yes")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Control Group",])[1]

demo.chart["Hemiparesis_Left",3]=prop.test(table(NewMaster.One[NewMaster.One[,"Group"]=="Study Group" | 
                                                                  NewMaster.One[,"Group"]=="Control Group", "Group"], 
                                                 NewMaster.One[, "Hemiparesis_Left"])[-2:-3,], correct=FALSE)$p.value

demo.chart["Hemiparesis_Left",4:5]=prop.test(table(NewMaster.One[NewMaster.One[,"Group"]=="Study Group" | 
                                                                    NewMaster.One[,"Group"]=="Control Group", "Group"], 
                                                   NewMaster.One[, "Hemiparesis_Left"])[-2:-3,], correct=FALSE)$conf.int[1:2]


demo.chart["Hemiparesis_Right","Study Group"]=sum(NewMaster.One[,"Group"]=="Study Group" & NewMaster.One[,"Hemiparesis_Right"]=="Yes")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Study Group",])[1]

demo.chart["Hemiparesis_Right","Control Group"]=sum(NewMaster.One[,"Group"]=="Control Group" & NewMaster.One[,"Hemiparesis_Right"]=="Yes")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Control Group",])[1]

demo.chart["Hemiparesis_Right",3]=prop.test(table(NewMaster.One[NewMaster.One[,"Group"]=="Study Group" | 
                                                                   NewMaster.One[,"Group"]=="Control Group", "Group"], 
                                                  NewMaster.One[, "Hemiparesis_Right"])[-2:-3,], correct=FALSE)$p.value

demo.chart["Hemiparesis_Right",4:5]=prop.test(table(NewMaster.One[NewMaster.One[,"Group"]=="Study Group" | 
                                                                     NewMaster.One[,"Group"]=="Control Group", "Group"], 
                                                    NewMaster.One[, "Hemiparesis_Right"])[-2:-3,], correct=FALSE)$conf.int[1:2]


demo.chart["Hemiparesis_Bilateral","Study Group"]=sum(NewMaster.One[,"Group"]=="Study Group" & NewMaster.One[,"Hemiparesis_Bilateral"]=="Yes")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Study Group",])[1]

demo.chart["Hemiparesis_Bilateral","Control Group"]=sum(NewMaster.One[,"Group"]=="Control Group" & NewMaster.One[,"Hemiparesis_Bilateral"]=="Yes")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Control Group",])[1]

demo.chart["Hemiparesis_Bilateral",3]=prop.test(table(NewMaster.One[NewMaster.One[,"Group"]=="Study Group" | 
                                                                       NewMaster.One[,"Group"]=="Control Group", "Group"], 
                                                      NewMaster.One[, "Hemiparesis_Bilateral"])[-2:-3,], correct=FALSE)$p.value

demo.chart["Hemiparesis_Bilateral",4:5]=prop.test(table(NewMaster.One[NewMaster.One[,"Group"]=="Study Group" | 
                                                                         NewMaster.One[,"Group"]=="Control Group", "Group"], 
                                                        NewMaster.One[, "Hemiparesis_Bilateral"])[-2:-3,], correct=FALSE)$conf.int[1:2]


###Balance Ataxia, Speech, Dysphagia, Spasticity

for (i in 13:16){
  
  demo.chart[i,"Study Group"]=sum(NewMaster.One[,"Group"]=="Study Group" & NewMaster.One[,rownames(demo.chart)[i]]=="Yes")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Study Group",])[1]
  
  demo.chart[i,"Control Group"]=sum(NewMaster.One[,"Group"]=="Control Group" & NewMaster.One[,rownames(demo.chart)[i]]=="Yes")/dim(NewMaster.One[NewMaster.One[,"Group"]=="Control Group",])[1]
  
  demo.chart[i,3]=prop.test(table(NewMaster.One[NewMaster.One[,"Group"]=="Study Group" | 
                                                   NewMaster.One[,"Group"]=="Control Group", "Group"], 
                                  NewMaster.One[, rownames(demo.chart)[i]])[-2:-3,], correct=FALSE)$p.value
  
  demo.chart[i,4:5]=prop.test(table(NewMaster.One[NewMaster.One[,"Group"]=="Study Group" | 
                                                     NewMaster.One[,"Group"]=="Control Group", "Group"], 
                                    NewMaster.One[, rownames(demo.chart)[i]])[-2:-3,], correct=FALSE)$conf.int[1:2]
  
  
}

###ACHosp and ARHosp Variables

for (i in c(17:19,21:28)){
  
  if (length(NewMaster.One[NewMaster.One[,"Group"]=="Study Group" 
                            & NewMaster.One[,rownames(demo.chart)[i]]<200,
                            rownames(demo.chart)[i]])!=0 & length(NewMaster.One[NewMaster.One[,"Group"]=="Control Group" 
                                                                                 & NewMaster.One[,rownames(demo.chart)[i]]<200,
                                                                                 rownames(demo.chart)[i]])!=0){
    
    demo.chart[i,"Study Group"]=mean(NewMaster.One[NewMaster.One[,"Group"]=="Study Group" 
                                                    & NewMaster.One[,rownames(demo.chart)[i]]<200,
                                                    rownames(demo.chart)[i]], na.rm=TRUE)
    
    demo.chart[i,"Control Group"]=mean(NewMaster.One[NewMaster.One[,"Group"]=="Control Group"
                                                      & NewMaster.One[,rownames(demo.chart)[i]]<200,
                                                      rownames(demo.chart)[i]], na.rm=TRUE)
    
    demo.chart[i,3]=t.test(NewMaster.One[NewMaster.One[,"Group"]=="Study Group" & NewMaster.One[,rownames(demo.chart)[i]]<200, rownames(demo.chart)[i]],
                           NewMaster.One[NewMaster.One[,"Group"]=="Control Group" & NewMaster.One[,rownames(demo.chart)[i]]<200, rownames(demo.chart)[i]])$p.value
    
    demo.chart[i,4:5]=t.test(NewMaster.One[NewMaster.One[,"Group"]=="Study Group" & NewMaster.One[,rownames(demo.chart)[i]]<200, rownames(demo.chart)[i]],
                             NewMaster.One[NewMaster.One[,"Group"]=="Control Group" & NewMaster.One[,rownames(demo.chart)[i]]<200, rownames(demo.chart)[i]])$conf.int[1:2]
    
  }
  
}

###Medical History Variables

for (i in c(29:39, 41:48, 50:51)){
  
  demo.chart[i,"Study Group"]=sum(NewMaster.One[,"Group"]=="Study Group" & NewMaster.One[,rownames(demo.chart)[i]]==TRUE)/dim(NewMaster.One[NewMaster.One[,"Group"]=="Study Group",])[1]
  
  demo.chart[i,"Control Group"]=sum(NewMaster.One[,"Group"]=="Control Group" & NewMaster.One[,rownames(demo.chart)[i]]==TRUE)/dim(NewMaster.One[NewMaster.One[,"Group"]=="Control Group",])[1]
  
  if (demo.chart[i,"Study Group"]!=0 | demo.chart[i,"Control Group"]!=0){
    
    demo.chart[i,3]=prop.test(table(NewMaster.One[NewMaster.One[,"Group"]=="Study Group" |
                                                     NewMaster.One[,"Group"]=="Control Group", "Group"],
                                    NewMaster.One[, rownames(demo.chart)[i]])[-2:-3,], correct=FALSE)$p.value
    
    demo.chart[i,4:5]=prop.test(table(NewMaster.One[NewMaster.One[,"Group"]=="Study Group" |
                                                       NewMaster.One[,"Group"]=="Control Group", "Group"],
                                      NewMaster.One[, rownames(demo.chart)[i]])[-2:-3,], correct=FALSE)$conf.int[1:2]
    
  }
}

###Create and combine vectors for height, weight, SBP.HTN, DBP.HTN, Fast.NonFasting.LDL, 
###and hbA1c for study and control group 

for (j in rownames(demo.chart)[52:57]){
  
  x=0
  
  for (i in StudyGroupIDs){
    
    x=c(x, NewMaster[NewMaster[,"ID"]==i & NewMaster[,"DaysId"]==1,j])
    
  }
  
  x=x[-1]
  
  assign(paste("Study", j, sep="."), x)
  
} 

for (j in rownames(demo.chart)[52:57]){
  
  x=0
  
  for (i in ControlGroupIDs){
    
    x=c(x, NewMaster[NewMaster[,"ID"]==i & NewMaster[,"DaysId"]==1,j])
    
  }
  
  x=x[-1]
  
  assign(paste("Control", j, sep="."), x)
  
} 

StudySet=cbind(Study.Height, Study.Weight, Study.SBP.HTN, Study.DBP.HTN, Study.Fast.Nonfasting.LDL, Study.HbA1c)

ControlSet=cbind(Control.Height, Control.Weight, Control.SBP.HTN, Control.DBP.HTN, Control.Fast.Nonfasting.LDL, Control.HbA1c)


for (i in 52:57){
  
  demo.chart[i,"Study Group"]=mean(StudySet[StudySet[,i-51]<1000,i-51], na.rm=TRUE)
  
  demo.chart[i,"Control Group"]=mean(ControlSet[ControlSet[,i-51]<1000,i-51], na.rm=TRUE)
  
  demo.chart[i,3]=t.test(StudySet[StudySet[,i-51]<1000,i-51], 
                         ControlSet[ControlSet[,i-51]<1000,i-51])$p.value
  
  demo.chart[i,4:5]=t.test(StudySet[StudySet[,i-51]<1000,i-51], 
                           ControlSet[ControlSet[,i-51]<1000,i-51])$conf.int[1:2]
  
  
}

for (i in 58:60){
  
  demo.chart[i,"Study Group"]=mean(NewMaster.One[NewMaster.One[,"Group"]=="Study Group",rownames(demo.chart)[i]], na.rm=TRUE)
  
  demo.chart[i,"Control Group"]=mean(NewMaster.One[NewMaster.One[,"Group"]=="Control Group",rownames(demo.chart)[i]], na.rm=TRUE)
  
  demo.chart[i,3]=t.test(NewMaster.One[NewMaster.One[,"Group"]=="Study Group" & NewMaster.One[,rownames(demo.chart)[i]]<200, rownames(demo.chart)[i]],
                         NewMaster.One[NewMaster.One[,"Group"]=="Control Group" & NewMaster.One[,rownames(demo.chart)[i]]<200, rownames(demo.chart)[i]])$p.value
  
  demo.chart[i,4:5]=t.test(NewMaster.One[NewMaster.One[,"Group"]=="Study Group" & NewMaster.One[,rownames(demo.chart)[i]]<200, rownames(demo.chart)[i]],
                           NewMaster.One[NewMaster.One[,"Group"]=="Control Group" & NewMaster.One[,rownames(demo.chart)[i]]<200, rownames(demo.chart)[i]])$conf.int[1:2]
  
}

sqrt(var(NewMaster.One[NewMaster.One[,"Group"]=="Study Group","Age"]))

sqrt(var(NewMaster.One[NewMaster.One[,"Group"]=="Control Group","Age"]))




# Demo Chart for matched subgroup ####

match.subgroup=NewMaster.One[1,]

for (i in unique(matchrows[,2])){
  
  match.subgroup=rbind(match.subgroup, NewMaster.One[NewMaster.One[,"ID"]==i,])
  
}

match.subgroup=match.subgroup[-1,]

options(digits=5)

demo.chart=as.data.frame(matrix(NA,60,5))

rownames(demo.chart)=c("Age", "Male", "Female", "Deceased", "White", "Black", "Other", 
                       colnames(match.subgroup[c(9,12,19:25,31:34,38:45, 47:69, 75:76, 78:81, 119:121)]))

colnames(demo.chart)=c("Study Group", "Control Group", "P-value", "Conf Int. L", "Conf Int. U")


###Sample Size

demo.chart["n","Study Group"]=length(match.subgroup[match.subgroup[,"Group"]=="Study Group","Age"])

demo.chart["n","Control Group"]=length(match.subgroup[match.subgroup[,"Group"]=="Control Group","Age"])

###Age

demo.chart["Age","Study Group"]=mean(match.subgroup[match.subgroup[,"Group"]=="Study Group","Age"])

demo.chart["Age","Control Group"]=mean(match.subgroup[match.subgroup[,"Group"]=="Control Group","Age"])

demo.chart["Age",3]=t.test(match.subgroup[match.subgroup[,"Group"]=="Study Group", "Age"],
                           match.subgroup[match.subgroup[,"Group"]=="Control Group", "Age"])$p.value

demo.chart["Age",4:5]=t.test(match.subgroup[match.subgroup[,"Group"]=="Study Group", "Age"],
                             match.subgroup[match.subgroup[,"Group"]=="Control Group", "Age"])$conf.int[1:2]

###Gender

demo.chart["Male","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Gender"]=="Male")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["Male","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Gender"]=="Male")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Female","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Gender"]=="Female")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["Female","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Gender"]=="Female")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Female",3]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                                        match.subgroup[,"Group"]=="Control Group", "Group"], 
                                       match.subgroup[, "Gender"])[-2:-3,], correct=FALSE)$p.value

demo.chart["Female",4:5]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                                          match.subgroup[,"Group"]=="Control Group", "Group"], 
                                         match.subgroup[, "Gender"])[-2:-3,], correct=FALSE)$conf.int[1:2]

###Deceased

demo.chart["Deceased","Study Group"]=sum(match.subgroup[match.subgroup[,"Group"]=="Study Group","Deceased_Y.N"])/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["Deceased","Control Group"]=sum(match.subgroup[match.subgroup[,"Group"]=="Control Group","Deceased_Y.N"])/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Deceased",3]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                                          match.subgroup[,"Group"]=="Control Group", "Group"], 
                                         match.subgroup[, "Deceased_Y.N"])[-2:-3,], correct=FALSE)$p.value

demo.chart["Deceased",4:5]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                                            match.subgroup[,"Group"]=="Control Group", "Group"], 
                                           match.subgroup[, "Deceased_Y.N"])[-2:-3,], correct=FALSE)$conf.int[1:2]

###Race

demo.chart["White","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"New.Race"]=="White")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["White","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"New.Race"]=="White")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Black","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"New.Race"]=="Black")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["Black","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"New.Race"]=="Black")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Other","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"New.Race"]=="Other")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["Other","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"New.Race"]=="Other")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Other",3]=chisq.test(table(match.subgroup[,c("New.Race", "Group")])[,c(1,4)])$p.value

###Hispanic

demo.chart["Hispanic.Ethnicity","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Hispanic.Ethnicity"]=="Yes")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["Hispanic.Ethnicity","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Hispanic.Ethnicity"]=="Yes")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Hispanic.Ethnicity",3]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                                                    match.subgroup[,"Group"]=="Control Group", "Group"], 
                                                   match.subgroup[, "Hispanic.Ethnicity"])[-2:-3,-2], correct=FALSE)$p.value

demo.chart["Hispanic.Ethnicity",4:5]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                                                      match.subgroup[,"Group"]=="Control Group", "Group"], 
                                                     match.subgroup[, "Hispanic.Ethnicity"])[-2:-3,-2], correct=FALSE)$conf.int[1:2]

###Insurance Type

demo.chart["Medicaid","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Health.Insurance.Name"]=="Medicaid")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["Medicaid","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Health.Insurance.Name"]=="Medicaid")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Medicare","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Health.Insurance.Name"]=="Medicare")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["Medicare","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Health.Insurance.Name"]=="Medicare")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Private/VA/Champus/Other Insurance","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Health.Insurance.Name"]=="Private/VA/Champus/Other Insurance")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["Private/VA/Champus/Other Insurance","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Health.Insurance.Name"]=="Private/VA/Champus/Other Insurance")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Self Pay/No Insurance","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Health.Insurance.Name"]=="Self Pay/No Insurance")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["Self Pay/No Insurance","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Health.Insurance.Name"]=="Self Pay/No Insurance")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Self Pay/No Insurance",3]=chisq.test(table(match.subgroup[,c("Health.Insurance.Name", "Group")])[2:4,c(1,4)])$p.value

###Education Level

demo.chart["Education.Level","Study Group"]=mean(match.subgroup[match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Education.Level"]<100, "Education.Level"])

demo.chart["Education.Level","Control Group"]=mean(match.subgroup[match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Education.Level"]<100, "Education.Level"])

demo.chart["Education.Level",3]=t.test(match.subgroup[match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Education.Level"]<100, "Education.Level"],
                                       match.subgroup[match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Education.Level"]<100, "Education.Level"])$p.value

demo.chart["Education.Level",4:5]=t.test(match.subgroup[match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Education.Level"]<100, "Education.Level"],
                                         match.subgroup[match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Education.Level"]<100, "Education.Level"])$conf.int[1:2]

###Type of Stroke

demo.chart["HEMORRHAGIC - INTRACEREBRAL/INTRACEREBELLAR/INTRAP","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Type.of.Stroke"]=="HEMORRHAGIC - INTRACEREBRAL/INTRACEREBELLAR/INTRAP")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["HEMORRHAGIC - INTRACEREBRAL/INTRACEREBELLAR/INTRAP","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Type.of.Stroke"]=="HEMORRHAGIC - INTRACEREBRAL/INTRACEREBELLAR/INTRAP")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["ISCHEMIC CVA","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Type.of.Stroke"]=="ISCHEMIC CVA")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["ISCHEMIC CVA","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Type.of.Stroke"]=="ISCHEMIC CVA")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["SUBARACHNOID HEMORRHAGE","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Type.of.Stroke"]=="SUBARACHNOID HEMORRHAGE")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["SUBARACHNOID HEMORRHAGE","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Type.of.Stroke"]=="SUBARACHNOID HEMORRHAGE")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

x=table(match.subgroup[,c("Type.of.Stroke", "Group")])[c(1,2),c(1,4)] + matrix(c(table(match.subgroup[,c("Type.of.Stroke", "Group")])[3,1],
                                                                                 0,table(match.subgroup[,c("Type.of.Stroke", "Group")])[3,4], 0), nrow=2)

demo.chart["SUBARACHNOID HEMORRHAGE",3]=chisq.test(x)$p.value

###Hemiparesis

demo.chart["Hemiparesis_Left","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Hemiparesis_Left"]=="Yes")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["Hemiparesis_Left","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Hemiparesis_Left"]=="Yes")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Hemiparesis_Left",3]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                                                  match.subgroup[,"Group"]=="Control Group", "Group"], 
                                                 match.subgroup[, "Hemiparesis_Left"])[-2:-3,], correct=FALSE)$p.value

demo.chart["Hemiparesis_Left",4:5]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                                                    match.subgroup[,"Group"]=="Control Group", "Group"], 
                                                   match.subgroup[, "Hemiparesis_Left"])[-2:-3,], correct=FALSE)$conf.int[1:2]


demo.chart["Hemiparesis_Right","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Hemiparesis_Right"]=="Yes")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["Hemiparesis_Right","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Hemiparesis_Right"]=="Yes")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Hemiparesis_Right",3]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                                                   match.subgroup[,"Group"]=="Control Group", "Group"], 
                                                  match.subgroup[, "Hemiparesis_Right"])[-2:-3,], correct=FALSE)$p.value

demo.chart["Hemiparesis_Right",4:5]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                                                     match.subgroup[,"Group"]=="Control Group", "Group"], 
                                                    match.subgroup[, "Hemiparesis_Right"])[-2:-3,], correct=FALSE)$conf.int[1:2]


demo.chart["Hemiparesis_Bilateral","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Hemiparesis_Bilateral"]=="Yes")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["Hemiparesis_Bilateral","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Hemiparesis_Bilateral"]=="Yes")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Hemiparesis_Bilateral",3]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                                                       match.subgroup[,"Group"]=="Control Group", "Group"], 
                                                      match.subgroup[, "Hemiparesis_Bilateral"])[-2:-3,], correct=FALSE)$p.value

demo.chart["Hemiparesis_Bilateral",4:5]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                                                         match.subgroup[,"Group"]=="Control Group", "Group"], 
                                                        match.subgroup[, "Hemiparesis_Bilateral"])[-2:-3,], correct=FALSE)$conf.int[1:2]


###Balance Ataxia, Speech, Dysphagia, Spasticity

for (i in 13:16){
  
  demo.chart[i,"Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,rownames(demo.chart)[i]]=="Yes")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]
  
  demo.chart[i,"Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,rownames(demo.chart)[i]]=="Yes")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]
  
  demo.chart[i,3]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                                   match.subgroup[,"Group"]=="Control Group", "Group"], 
                                  match.subgroup[, rownames(demo.chart)[i]])[-2:-3,], correct=FALSE)$p.value
  
  demo.chart[i,4:5]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                                     match.subgroup[,"Group"]=="Control Group", "Group"], 
                                    match.subgroup[, rownames(demo.chart)[i]])[-2:-3,], correct=FALSE)$conf.int[1:2]
  
  
}

###ACHosp and ARHosp Variables

for (i in c(17:19,21:28)){
  
  if (length(match.subgroup[match.subgroup[,"Group"]=="Study Group" 
                            & match.subgroup[,rownames(demo.chart)[i]]<200,
                            rownames(demo.chart)[i]])!=0 & length(match.subgroup[match.subgroup[,"Group"]=="Control Group" 
                                                                                 & match.subgroup[,rownames(demo.chart)[i]]<200,
                                                                                 rownames(demo.chart)[i]])!=0){
    
    demo.chart[i,"Study Group"]=mean(match.subgroup[match.subgroup[,"Group"]=="Study Group" 
                                                    & match.subgroup[,rownames(demo.chart)[i]]<200,
                                                    rownames(demo.chart)[i]], na.rm=TRUE)
    
    demo.chart[i,"Control Group"]=mean(match.subgroup[match.subgroup[,"Group"]=="Control Group"
                                                      & match.subgroup[,rownames(demo.chart)[i]]<200,
                                                      rownames(demo.chart)[i]], na.rm=TRUE)
    
    demo.chart[i,3]=t.test(match.subgroup[match.subgroup[,"Group"]=="Study Group" & match.subgroup[,rownames(demo.chart)[i]]<200, rownames(demo.chart)[i]],
                           match.subgroup[match.subgroup[,"Group"]=="Control Group" & match.subgroup[,rownames(demo.chart)[i]]<200, rownames(demo.chart)[i]])$p.value
    
    demo.chart[i,4:5]=t.test(match.subgroup[match.subgroup[,"Group"]=="Study Group" & match.subgroup[,rownames(demo.chart)[i]]<200, rownames(demo.chart)[i]],
                             match.subgroup[match.subgroup[,"Group"]=="Control Group" & match.subgroup[,rownames(demo.chart)[i]]<200, rownames(demo.chart)[i]])$conf.int[1:2]
    
  }
  
}

###Medical History Variables

for (i in c(29:39, 41:48, 50:51)){
  
  demo.chart[i,"Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,rownames(demo.chart)[i]]==TRUE)/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]
  
  demo.chart[i,"Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,rownames(demo.chart)[i]]==TRUE)/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]
  
  if (demo.chart[i,"Study Group"]!=0 | demo.chart[i,"Control Group"]!=0){
    
    demo.chart[i,3]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" |
                                                     match.subgroup[,"Group"]=="Control Group", "Group"],
                                    match.subgroup[, rownames(demo.chart)[i]])[-2:-3,], correct=FALSE)$p.value
    
    demo.chart[i,4:5]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" |
                                                       match.subgroup[,"Group"]=="Control Group", "Group"],
                                      match.subgroup[, rownames(demo.chart)[i]])[-2:-3,], correct=FALSE)$conf.int[1:2]
    
  }
}

###Create and combine vectors for height, weight, SBP.HTN, DBP.HTN, Fast.NonFasting.LDL, 
###and hbA1c for study and control group 

for (j in rownames(demo.chart)[52:57]){
  
  x=0
  
  for (i in StudyGroupIDs){
    
    x=c(x, NewMaster[NewMaster[,"ID"]==i & NewMaster[,"DaysId"]==1,j])
    
  }
  
  x=x[-1]
  
  assign(paste("Study", j, sep="."), x)
  
} 

for (j in rownames(demo.chart)[52:57]){
  
  x=0
  
  for (i in ControlGroupIDs){
    
    x=c(x, NewMaster[NewMaster[,"ID"]==i & NewMaster[,"DaysId"]==1,j])
    
  }
  
  x=x[-1]
  
  assign(paste("Control", j, sep="."), x)
  
} 

StudySet=cbind(Study.Height, Study.Weight, Study.SBP.HTN, Study.DBP.HTN, Study.Fast.Nonfasting.LDL, Study.HbA1c)

ControlSet=cbind(Control.Height, Control.Weight, Control.SBP.HTN, Control.DBP.HTN, Control.Fast.Nonfasting.LDL, Control.HbA1c)


for (i in 52:57){
  
  demo.chart[i,"Study Group"]=mean(StudySet[StudySet[,i-51]<1000,i-51], na.rm=TRUE)
  
  demo.chart[i,"Control Group"]=mean(ControlSet[ControlSet[,i-51]<1000,i-51], na.rm=TRUE)
  
  demo.chart[i,3]=t.test(StudySet[StudySet[,i-51]<1000,i-51], 
                         ControlSet[ControlSet[,i-51]<1000,i-51])$p.value
  
  demo.chart[i,4:5]=t.test(StudySet[StudySet[,i-51]<1000,i-51], 
                           ControlSet[ControlSet[,i-51]<1000,i-51])$conf.int[1:2]
  
  
}

for (i in 58:60){
  
  demo.chart[i,"Study Group"]=mean(match.subgroup[match.subgroup[,"Group"]=="Study Group",rownames(demo.chart)[i]], na.rm=TRUE)
  
  demo.chart[i,"Control Group"]=mean(match.subgroup[match.subgroup[,"Group"]=="Control Group",rownames(demo.chart)[i]], na.rm=TRUE)
  
  demo.chart[i,3]=t.test(match.subgroup[match.subgroup[,"Group"]=="Study Group" & match.subgroup[,rownames(demo.chart)[i]]<200, rownames(demo.chart)[i]],
                         match.subgroup[match.subgroup[,"Group"]=="Control Group" & match.subgroup[,rownames(demo.chart)[i]]<200, rownames(demo.chart)[i]])$p.value
  
  demo.chart[i,4:5]=t.test(match.subgroup[match.subgroup[,"Group"]=="Study Group" & match.subgroup[,rownames(demo.chart)[i]]<200, rownames(demo.chart)[i]],
                           match.subgroup[match.subgroup[,"Group"]=="Control Group" & match.subgroup[,rownames(demo.chart)[i]]<200, rownames(demo.chart)[i]])$conf.int[1:2]
  
}

sqrt(var(match.subgroup[match.subgroup[,"Group"]=="Study Group","Age"]))

sqrt(var(match.subgroup[match.subgroup[,"Group"]=="Control Group","Age"]))




# Demo Chart for unique subgroup ####

match.subgroup=NewMaster.One[1,]

for (i in matchrow.final[,2]){
  
  match.subgroup=rbind(match.subgroup, NewMaster.One[NewMaster.One[,"ID"]==i,])
  
}

match.subgroup=match.subgroup[-1,]

options(digits=5)

demo.chart=as.data.frame(matrix(NA,60,5))

rownames(demo.chart)=c("Age", "Male", "Female", "Deceased", "White", "Black", "Other", 
                       colnames(match.subgroup[c(9,12,19:25,31:34,38:45, 47:69, 75:76, 78:81, 119:121)]))

colnames(demo.chart)=c("Study Group", "Control Group", "P-value", "Conf Int. L", "Conf Int. U")


###Sample Size

demo.chart["n","Study Group"]=length(match.subgroup[match.subgroup[,"Group"]=="Study Group","Age"])

demo.chart["n","Control Group"]=length(match.subgroup[match.subgroup[,"Group"]=="Control Group","Age"])

###Age

demo.chart["Age","Study Group"]=mean(match.subgroup[match.subgroup[,"Group"]=="Study Group","Age"])

demo.chart["Age","Control Group"]=mean(match.subgroup[match.subgroup[,"Group"]=="Control Group","Age"])

demo.chart["Age",3]=t.test(match.subgroup[match.subgroup[,"Group"]=="Study Group", "Age"],
       match.subgroup[match.subgroup[,"Group"]=="Control Group", "Age"])$p.value

demo.chart["Age",4:5]=t.test(match.subgroup[match.subgroup[,"Group"]=="Study Group", "Age"],
       match.subgroup[match.subgroup[,"Group"]=="Control Group", "Age"])$conf.int[1:2]

###Gender

demo.chart["Male","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Gender"]=="Male")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["Male","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Gender"]=="Male")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Female","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Gender"]=="Female")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["Female","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Gender"]=="Female")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Female",3]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                match.subgroup[,"Group"]=="Control Group", "Group"], 
                match.subgroup[, "Gender"])[-2:-3,], correct=FALSE)$p.value

demo.chart["Female",4:5]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                                     match.subgroup[,"Group"]=="Control Group", "Group"], 
                                    match.subgroup[, "Gender"])[-2:-3,], correct=FALSE)$conf.int[1:2]

###Deceased

demo.chart["Deceased","Study Group"]=sum(match.subgroup[match.subgroup[,"Group"]=="Study Group","Deceased_Y.N"])/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["Deceased","Control Group"]=sum(match.subgroup[match.subgroup[,"Group"]=="Control Group","Deceased_Y.N"])/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Deceased",3]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                match.subgroup[,"Group"]=="Control Group", "Group"], 
                match.subgroup[, "Deceased_Y.N"])[-2:-3,], correct=FALSE)$p.value

demo.chart["Deceased",4:5]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                                          match.subgroup[,"Group"]=="Control Group", "Group"], 
                                         match.subgroup[, "Deceased_Y.N"])[-2:-3,], correct=FALSE)$conf.int[1:2]

###Race

demo.chart["White","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"New.Race"]=="White")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["White","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"New.Race"]=="White")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Black","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"New.Race"]=="Black")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["Black","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"New.Race"]=="Black")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Other","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"New.Race"]=="Other")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["Other","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"New.Race"]=="Other")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Other",3]=chisq.test(table(match.subgroup[,c("New.Race", "Group")])[,c(1,4)])$p.value

###Hispanic

demo.chart["Hispanic.Ethnicity","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Hispanic.Ethnicity"]=="Yes")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["Hispanic.Ethnicity","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Hispanic.Ethnicity"]=="Yes")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Hispanic.Ethnicity",3]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                match.subgroup[,"Group"]=="Control Group", "Group"], 
                match.subgroup[, "Hispanic.Ethnicity"])[-2:-3,-2], correct=FALSE)$p.value

demo.chart["Hispanic.Ethnicity",4:5]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                                                    match.subgroup[,"Group"]=="Control Group", "Group"], 
                                                   match.subgroup[, "Hispanic.Ethnicity"])[-2:-3,-2], correct=FALSE)$conf.int[1:2]

###Insurance Type

demo.chart["Medicaid","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Health.Insurance.Name"]=="Medicaid")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["Medicaid","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Health.Insurance.Name"]=="Medicaid")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Medicare","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Health.Insurance.Name"]=="Medicare")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["Medicare","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Health.Insurance.Name"]=="Medicare")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Private/VA/Champus/Other Insurance","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Health.Insurance.Name"]=="Private/VA/Champus/Other Insurance")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["Private/VA/Champus/Other Insurance","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Health.Insurance.Name"]=="Private/VA/Champus/Other Insurance")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Self Pay/No Insurance","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Health.Insurance.Name"]=="Self Pay/No Insurance")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["Self Pay/No Insurance","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Health.Insurance.Name"]=="Self Pay/No Insurance")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Self Pay/No Insurance",3]=chisq.test(table(match.subgroup[,c("Health.Insurance.Name", "Group")])[2:4,c(1,4)])$p.value

###Education Level

demo.chart["Education.Level","Study Group"]=mean(match.subgroup[match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Education.Level"]<100, "Education.Level"])

demo.chart["Education.Level","Control Group"]=mean(match.subgroup[match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Education.Level"]<100, "Education.Level"])

demo.chart["Education.Level",3]=t.test(match.subgroup[match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Education.Level"]<100, "Education.Level"],
       match.subgroup[match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Education.Level"]<100, "Education.Level"])$p.value

demo.chart["Education.Level",4:5]=t.test(match.subgroup[match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Education.Level"]<100, "Education.Level"],
                                       match.subgroup[match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Education.Level"]<100, "Education.Level"])$conf.int[1:2]

###Type of Stroke

demo.chart["HEMORRHAGIC - INTRACEREBRAL/INTRACEREBELLAR/INTRAP","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Type.of.Stroke"]=="HEMORRHAGIC - INTRACEREBRAL/INTRACEREBELLAR/INTRAP")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["HEMORRHAGIC - INTRACEREBRAL/INTRACEREBELLAR/INTRAP","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Type.of.Stroke"]=="HEMORRHAGIC - INTRACEREBRAL/INTRACEREBELLAR/INTRAP")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["ISCHEMIC CVA","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Type.of.Stroke"]=="ISCHEMIC CVA")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["ISCHEMIC CVA","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Type.of.Stroke"]=="ISCHEMIC CVA")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["SUBARACHNOID HEMORRHAGE","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Type.of.Stroke"]=="SUBARACHNOID HEMORRHAGE")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["SUBARACHNOID HEMORRHAGE","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Type.of.Stroke"]=="SUBARACHNOID HEMORRHAGE")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

x=table(match.subgroup[,c("Type.of.Stroke", "Group")])[c(1,2),c(1,4)] + matrix(c(table(match.subgroup[,c("Type.of.Stroke", "Group")])[3,1],
                                                                                0,table(match.subgroup[,c("Type.of.Stroke", "Group")])[3,4], 0), nrow=2)

demo.chart["SUBARACHNOID HEMORRHAGE",3]=chisq.test(x)$p.value

###Hemiparesis

demo.chart["Hemiparesis_Left","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Hemiparesis_Left"]=="Yes")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["Hemiparesis_Left","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Hemiparesis_Left"]=="Yes")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Hemiparesis_Left",3]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                match.subgroup[,"Group"]=="Control Group", "Group"], 
                match.subgroup[, "Hemiparesis_Left"])[-2:-3,], correct=FALSE)$p.value

demo.chart["Hemiparesis_Left",4:5]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                                                  match.subgroup[,"Group"]=="Control Group", "Group"], 
                                                 match.subgroup[, "Hemiparesis_Left"])[-2:-3,], correct=FALSE)$conf.int[1:2]


demo.chart["Hemiparesis_Right","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Hemiparesis_Right"]=="Yes")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["Hemiparesis_Right","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Hemiparesis_Right"]=="Yes")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Hemiparesis_Right",3]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                match.subgroup[,"Group"]=="Control Group", "Group"], 
                match.subgroup[, "Hemiparesis_Right"])[-2:-3,], correct=FALSE)$p.value

demo.chart["Hemiparesis_Right",4:5]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                                                   match.subgroup[,"Group"]=="Control Group", "Group"], 
                                                  match.subgroup[, "Hemiparesis_Right"])[-2:-3,], correct=FALSE)$conf.int[1:2]


demo.chart["Hemiparesis_Bilateral","Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,"Hemiparesis_Bilateral"]=="Yes")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]

demo.chart["Hemiparesis_Bilateral","Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,"Hemiparesis_Bilateral"]=="Yes")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]

demo.chart["Hemiparesis_Bilateral",3]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                match.subgroup[,"Group"]=="Control Group", "Group"], 
                match.subgroup[, "Hemiparesis_Bilateral"])[-2:-3,], correct=FALSE)$p.value

demo.chart["Hemiparesis_Bilateral",4:5]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                                                       match.subgroup[,"Group"]=="Control Group", "Group"], 
                                                      match.subgroup[, "Hemiparesis_Bilateral"])[-2:-3,], correct=FALSE)$conf.int[1:2]


###Balance Ataxia, Speech, Dysphagia, Spasticity

for (i in 13:16){
  
  demo.chart[i,"Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,rownames(demo.chart)[i]]=="Yes")/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]
  
  demo.chart[i,"Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,rownames(demo.chart)[i]]=="Yes")/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]
  
  demo.chart[i,3]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                        match.subgroup[,"Group"]=="Control Group", "Group"], 
                        match.subgroup[, rownames(demo.chart)[i]])[-2:-3,], correct=FALSE)$p.value
  
  demo.chart[i,4:5]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" | 
                                                   match.subgroup[,"Group"]=="Control Group", "Group"], 
                                  match.subgroup[, rownames(demo.chart)[i]])[-2:-3,], correct=FALSE)$conf.int[1:2]
  
  
}

###ACHosp and ARHosp Variables

for (i in c(17:19,21:28)){
  
  if (length(match.subgroup[match.subgroup[,"Group"]=="Study Group" 
                          & match.subgroup[,rownames(demo.chart)[i]]<200,
                          rownames(demo.chart)[i]])!=0 & length(match.subgroup[match.subgroup[,"Group"]=="Control Group" 
                                                                                          & match.subgroup[,rownames(demo.chart)[i]]<200,
                                                                                          rownames(demo.chart)[i]])!=0){
  
  demo.chart[i,"Study Group"]=mean(match.subgroup[match.subgroup[,"Group"]=="Study Group" 
                                                 & match.subgroup[,rownames(demo.chart)[i]]<200,
                                                 rownames(demo.chart)[i]], na.rm=TRUE)
  
  demo.chart[i,"Control Group"]=mean(match.subgroup[match.subgroup[,"Group"]=="Control Group"
                                                   & match.subgroup[,rownames(demo.chart)[i]]<200,
                                                   rownames(demo.chart)[i]], na.rm=TRUE)
  
  demo.chart[i,3]=t.test(match.subgroup[match.subgroup[,"Group"]=="Study Group" & match.subgroup[,rownames(demo.chart)[i]]<200, rownames(demo.chart)[i]],
               match.subgroup[match.subgroup[,"Group"]=="Control Group" & match.subgroup[,rownames(demo.chart)[i]]<200, rownames(demo.chart)[i]])$p.value
  
  demo.chart[i,4:5]=t.test(match.subgroup[match.subgroup[,"Group"]=="Study Group" & match.subgroup[,rownames(demo.chart)[i]]<200, rownames(demo.chart)[i]],
                         match.subgroup[match.subgroup[,"Group"]=="Control Group" & match.subgroup[,rownames(demo.chart)[i]]<200, rownames(demo.chart)[i]])$conf.int[1:2]
  
  }
  
}

###Medical History Variables

for (i in c(29:39, 41:48, 50:51)){
  
  demo.chart[i,"Study Group"]=sum(match.subgroup[,"Group"]=="Study Group" & match.subgroup[,rownames(demo.chart)[i]]==TRUE)/dim(match.subgroup[match.subgroup[,"Group"]=="Study Group",])[1]
  
  demo.chart[i,"Control Group"]=sum(match.subgroup[,"Group"]=="Control Group" & match.subgroup[,rownames(demo.chart)[i]]==TRUE)/dim(match.subgroup[match.subgroup[,"Group"]=="Control Group",])[1]
  
  if (demo.chart[i,"Study Group"]!=0 | demo.chart[i,"Control Group"]!=0){
  
  demo.chart[i,3]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" |
                                        match.subgroup[,"Group"]=="Control Group", "Group"],
                        match.subgroup[, rownames(demo.chart)[i]])[-2:-3,], correct=FALSE)$p.value
  
  demo.chart[i,4:5]=prop.test(table(match.subgroup[match.subgroup[,"Group"]=="Study Group" |
                                                   match.subgroup[,"Group"]=="Control Group", "Group"],
                                  match.subgroup[, rownames(demo.chart)[i]])[-2:-3,], correct=FALSE)$conf.int[1:2]
  
  }
}

###Create and combine vectors for height, weight, SBP.HTN, DBP.HTN, Fast.NonFasting.LDL, 
###and hbA1c for study and control group 

for (j in rownames(demo.chart)[52:57]){
  
  x=0
  
  for (i in StudyGroupIDs){
    
    x=c(x, NewMaster[NewMaster[,"ID"]==i & NewMaster[,"DaysId"]==1,j])
    
  }
  
  x=x[-1]
  
  assign(paste("Study", j, sep="."), x)
  
} 

for (j in rownames(demo.chart)[52:57]){
  
  x=0
  
  for (i in ControlGroupIDs){
    
    x=c(x, NewMaster[NewMaster[,"ID"]==i & NewMaster[,"DaysId"]==1,j])
    
  }
  
  x=x[-1]
  
  assign(paste("Control", j, sep="."), x)
  
} 

StudySet=cbind(Study.Height, Study.Weight, Study.SBP.HTN, Study.DBP.HTN, Study.Fast.Nonfasting.LDL, Study.HbA1c)

ControlSet=cbind(Control.Height, Control.Weight, Control.SBP.HTN, Control.DBP.HTN, Control.Fast.Nonfasting.LDL, Control.HbA1c)


for (i in 52:57){
  
  demo.chart[i,"Study Group"]=mean(StudySet[StudySet[,i-51]<1000,i-51], na.rm=TRUE)
  
  demo.chart[i,"Control Group"]=mean(ControlSet[ControlSet[,i-51]<1000,i-51], na.rm=TRUE)
  
  demo.chart[i,3]=t.test(StudySet[StudySet[,i-51]<1000,i-51], 
               ControlSet[ControlSet[,i-51]<1000,i-51])$p.value
  
  demo.chart[i,4:5]=t.test(StudySet[StudySet[,i-51]<1000,i-51], 
                         ControlSet[ControlSet[,i-51]<1000,i-51])$conf.int[1:2]
  
  
}

for (i in 58:60){
  
  demo.chart[i,"Study Group"]=mean(match.subgroup[match.subgroup[,"Group"]=="Study Group",rownames(demo.chart)[i]], na.rm=TRUE)
  
  demo.chart[i,"Control Group"]=mean(match.subgroup[match.subgroup[,"Group"]=="Control Group",rownames(demo.chart)[i]], na.rm=TRUE)
  
  demo.chart[i,3]=t.test(match.subgroup[match.subgroup[,"Group"]=="Study Group" & match.subgroup[,rownames(demo.chart)[i]]<200, rownames(demo.chart)[i]],
               match.subgroup[match.subgroup[,"Group"]=="Control Group" & match.subgroup[,rownames(demo.chart)[i]]<200, rownames(demo.chart)[i]])$p.value
  
  demo.chart[i,4:5]=t.test(match.subgroup[match.subgroup[,"Group"]=="Study Group" & match.subgroup[,rownames(demo.chart)[i]]<200, rownames(demo.chart)[i]],
                         match.subgroup[match.subgroup[,"Group"]=="Control Group" & match.subgroup[,rownames(demo.chart)[i]]<200, rownames(demo.chart)[i]])$conf.int[1:2]
  
}

sqrt(var(match.subgroup[match.subgroup[,"Group"]=="Study Group","Age"]))

sqrt(var(match.subgroup[match.subgroup[,"Group"]=="Control Group","Age"]))




##########################RANDOM STUFF#######################


BaselineName=c("Baseline.Mobility", "Baseline.Activity", "Baseline.Cognitive" )
DischargeName=c("Discharge.Mobility", "Discharge.Activity", "Discharge.Cognitive" )

fortest.match=rep(0,8)

for (i in matchrow.final[,2]){
  
  fortest.match=rbind(fortest.match, 
                      NewMaster.One[NewMaster.One[,"ID"]==i, c("ID", "Group", "Baseline.Mobility",
                                                                "Baseline.Activity", "Baseline.Cognitive",
                                                                "Discharge.Mobility", "Discharge.Activity",
                                                               "Discharge.Cognitive")])
  
}

fortest.match=fortest.match[-1,]

fortest.match=cbind(matchrow.final[,1], fortest.match)

colnames(fortest.match)[1]="PairID"

#no matching paired t-test
#shows whether or not there is significant increase in score between
#JRI admission and discharge
for (i in 1:3){
  
  fortest=NewMaster.One[, DischargeName[i]]-NewMaster.One[, BaselineName[i]]
  
  print(t.test(fortest))
  
}

#no matching two sample test
#shows whether or not the group the patients will later choose has significant 
#significant effect on the increase in score between JRI admission and discharge
#before matching
for (i in 1:3){
  
  fortest.study=NewMaster.One[NewMaster.One[,"Group"]=="Study Group" , DischargeName[i]]-NewMaster.One[NewMaster.One[,"Group"]=="Study Group" , BaselineName[i]]
  
  fortest.control=NewMaster.One[NewMaster.One[,"Group"]=="Control Group" , DischargeName[i]]-NewMaster.One[NewMaster.One[,"Group"]=="Control Group" , BaselineName[i]]
  
  print(t.test(fortest.study, fortest.control))
  
}

#matching paired t-test
#shows whether or not the group the patients will later choose has significant 
#significant effect on the increase in score between JRI admission and discharge
#after matching
for (i in 1:3){
  
  fortest.study=fortest.match[fortest.match[,"Group"]=="Study Group",DischargeName[i]]-fortest.match[fortest.match[,"Group"]=="Study Group", BaselineName[i]]
  
  fortest.control=fortest.match[fortest.match[,"Group"]=="Control Group",DischargeName[i]]-fortest.match[fortest.match[,"Group"]=="Control Group", BaselineName[i]]
  
  fortest=fortest.study-fortest.control
  
  print(t.test(fortest, alternative="greater"))
  
}

#find proportion of patients and number of patients with non missing values for
#daysid 8
1-(sum(is.na(NewMaster[NewMaster[,"DaysId"]==8,ScoreVarName[1]]))/length(NewMaster[NewMaster[,"DaysId"]==8,ScoreVarName[1]]))

length(NewMaster[NewMaster[,"DaysId"]==8,ScoreVarName[1]])-sum(is.na(NewMaster[NewMaster[,"DaysId"]==8,ScoreVarName[1]]))
n
#find proportion of patients and number of patients with non missing values for
#daysid 7
1-(sum(is.na(NewMaster[NewMaster[,"DaysId"]==7,ScoreVarName[1]]))/length(NewMaster[NewMaster[,"DaysId"]==7,ScoreVarName[1]]))

length(NewMaster[NewMaster[,"DaysId"]==7,ScoreVarName[1]])-sum(is.na(NewMaster[NewMaster[,"DaysId"]==7,ScoreVarName[1]]))

#find proportion of patients and number of patients with non missing values for
#daysid 6
1-(sum(is.na(NewMaster[NewMaster[,"DaysId"]==6,ScoreVarName[1]]))/length(NewMaster[NewMaster[,"DaysId"]==6,ScoreVarName[1]]))

length(NewMaster[NewMaster[,"DaysId"]==6,ScoreVarName[1]])-sum(is.na(NewMaster[NewMaster[,"DaysId"]==6,ScoreVarName[1]]))

#find proportion of patients and number of patients with non missing values for
#daysid 5
1-(sum(is.na(NewMaster[NewMaster[,"DaysId"]==5,ScoreVarName[1]]))/length(NewMaster[NewMaster[,"DaysId"]==5,ScoreVarName[1]]))

length(NewMaster[NewMaster[,"DaysId"]==5,ScoreVarName[1]])-sum(is.na(NewMaster[NewMaster[,"DaysId"]==5,ScoreVarName[1]]))

#find proportion of patients and number of patients with non missing values for
#daysid 4
1-(sum(is.na(NewMaster[NewMaster[,"DaysId"]==4,ScoreVarName[1]]))/length(NewMaster[NewMaster[,"DaysId"]==4,ScoreVarName[1]]))

length(NewMaster[NewMaster[,"DaysId"]==4,ScoreVarName[1]])-sum(is.na(NewMaster[NewMaster[,"DaysId"]==4,ScoreVarName[1]]))

#find proportion of patients and number of patients with non missing values for
#daysid 3
1-(sum(is.na(NewMaster[NewMaster[,"DaysId"]==3,ScoreVarName[1]]))/length(NewMaster[NewMaster[,"DaysId"]==3,ScoreVarName[1]]))

length(NewMaster[NewMaster[,"DaysId"]==3,ScoreVarName[1]])-sum(is.na(NewMaster[NewMaster[,"DaysId"]==3,ScoreVarName[1]]))

#count number of patients with missing stroke severity number
length(NewMaster.One[NewMaster.One[,"ACHosp.Stroke.Severity.Number_ICH"]==9999 &
                       NewMaster.One[,"ACHosp.Stroke.Severity.Number_NIHSS"]==9999 &
                       NewMaster.One[,"ACHosp.Stroke.Severity.Number_HH"]==9999,])
                                           





K=0

for (i in unique(matchrows[matchrows[,3]==0,2])){
  
  if (NewMaster.One[NewMaster.One[,"ID"]==i,"Deceased_Y.N"]==TRUE){
    
    K=K+1
    
  }
  
}

K

###Change Point Analysis####

colornames=c("red", "green")

j=1

goodIDs=0

for (i in matchrow.final[,2]){
  
    if (sum(is.na(Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4)), ScoreVarName[j]]))<2 & 
        length(Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4)), ScoreVarName[j]])>2){
    
    #plot for Study Group
    plot(-100,-100, xlim=c(0,400), ylim=c(-30,130), ylab=paste(ScoreName[j], "Score"), 
         xlab="Days After Stroke",
         main=paste(ScoreName[j], "Score by Days After Stroke (Pair ID:", matchrow.final[matchrow.final[,2]==i,1], "& ID:", i, ")"))
    
    points(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), "Days_afterOnset"],
           Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), ScoreVarName[j]],
           pch=19,col="blue")
    
    points(Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), "Days_afterOnset"],
           Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), ScoreVarName[j]],
           pch=19,col="purple")
    
    points(Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"],
           Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]], 
           pch=19, col=colornames[matchrow.final[matchrow.final[,2]==i,3]+1])
    
    lines(c(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), "Days_afterOnset"],
            Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), "Days_afterOnset"],
            Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"]),
          c(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), ScoreVarName[j]],
            Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), ScoreVarName[j]],
            Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]]))
  
    legend("topright", legend=c("Scores Before Group Assignment", "Score at Assignment", "Study Group", "Control Group"),
           col=c("blue", "purple", "green", "red"), pch=c(19,19,19,19), cex=0.65)
  
    goodIDs=c(goodIDs, i)
      
  }
  
}

goodIDs=goodIDs[-1]

length(goodIDs)/dim(matchrow.final)[1]

length(goodIDs)

###look at Dr. Cabrera's code
###basic idea is analyze place of change point and slope before and after change point
###just look at one change point at first
###compare change point locations (?) and slopes
###goal is to see controls have more positive change point slopes
###only care about points after and including purple
###minimum number of points necessary?
###change point distribution for all patients, and by study group

goodIDs

###old changepoint way####

# library(segmented)
# 
# cpdata=as.data.frame(cbind(Interpolate.Master[Interpolate.Master[,"ID"]==121 & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=3, ScoreVarName[j]],
#              Interpolate.Master[Interpolate.Master[,"ID"]==121 & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=3, "Days_afterOnset"]))
# 
# colnames(cpdata)=c(ScoreVarName[j], "Days_afterOnset")
# 
# fit = lm(data=cpdata, PT.AM.PAC.Basic.Mobility.Score~Days_afterOnset)
# 
# segfit=segmented(fit, seg.Z=~Days_afterOnset, psi=75)
# 
# for (i in 121){
#   
#   if (sum(is.na(Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4)), ScoreVarName[j]]))<2 & 
#       length(Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4)), ScoreVarName[j]])>2){
#         
#     #plot for Study Group
#     plot(-100,-100, xlim=c(0,400), ylim=c(-30,130), ylab=paste(ScoreName[j], "Score"), 
#          xlab="Days After Stroke",
#          main=paste(ScoreName[j], "Score by Days After Stroke (Pair ID:", matchrow.final[matchrow.final[,2]==i,1], "& ID:", i, ")"))
#     
#     lines(cpdata[,2], segfit$fitted.values, lwd=3, col="orange")
#     
#     abline(v=segfit$psi[2], col="orange", lwd=3)
#     
#     abline(v=segfit$psi[2] + segfit$psi[3], col="brown", lty=2, lwd=2)
#     
#     abline(v=segfit$psi[2] - segfit$psi[3], col="brown", lty=2, lwd=2)
#     
#     
#     points(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), "Days_afterOnset"],
#            Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), ScoreVarName[j]],
#            pch=19,col="blue")
#     
#     points(Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), "Days_afterOnset"],
#            Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), ScoreVarName[j]],
#            pch=19,col="purple")
#     
#     points(Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"],
#            Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]], 
#            pch=19, col=colornames[matchrow.final[matchrow.final[,2]==i,3]+1])
#     
#     lines(c(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), "Days_afterOnset"],
#             Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), "Days_afterOnset"],
#             Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"]),
#           c(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), ScoreVarName[j]],
#             Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), ScoreVarName[j]],
#             Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]]))
#     
#     legend("topright", legend=c("Scores Before Group Assignment", "Score at Assignment", "Study Group", "Control Group"),
#            col=c("blue", "purple", "green", "red"), pch=c(19,19,19,19), cex=0.65)
#     
#   }
#   
# }
# 
# cpdata=as.data.frame(cbind(Interpolate.Master[Interpolate.Master[,"ID"]==110 & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=3, ScoreVarName[j]],
#                            Interpolate.Master[Interpolate.Master[,"ID"]==110 & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=3, "Days_afterOnset"]))
# 
# colnames(cpdata)=c(ScoreVarName[j], "Days_afterOnset")
# 
# fit = lm(data=cpdata, PT.AM.PAC.Basic.Mobility.Score~Days_afterOnset)
# 
# segfit=segmented(fit, seg.Z=~Days_afterOnset, psi=75)
# 
# 
# for (i in 110){
#   
#   if (sum(is.na(Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4)), ScoreVarName[j]]))<2 & 
#       length(Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4)), ScoreVarName[j]])>2){
#     
#     #plot for Study Group
#     plot(-100,-100, xlim=c(0,400), ylim=c(-30,130), ylab=paste(ScoreName[j], "Score"), 
#          xlab="Days After Stroke",
#          main=paste(ScoreName[j], "Score by Days After Stroke (Pair ID:", matchrow.final[matchrow.final[,2]==i,1], "& ID:", i, ")"))
#     
#     lines(cpdata[,2], segfit$fitted.values, lwd=3, col="orange")
#     
#     abline(v=segfit$psi[2], col="orange", lwd=3)
#     
#     abline(v=segfit$psi[2] + segfit$psi[3], col="brown", lty=2, lwd=2)
#     
#     abline(v=segfit$psi[2] - segfit$psi[3], col="brown", lty=2, lwd=2)
#     
#     points(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), "Days_afterOnset"],
#            Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), ScoreVarName[j]],
#            pch=19,col="blue")
#     
#     points(Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), "Days_afterOnset"],
#            Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), ScoreVarName[j]],
#            pch=19,col="purple")
#     
#     points(Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"],
#            Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]], 
#            pch=19, col=colornames[matchrow.final[matchrow.final[,2]==i,3]+1])
#     
#     lines(c(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), "Days_afterOnset"],
#             Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), "Days_afterOnset"],
#             Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"]),
#           c(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), ScoreVarName[j]],
#             Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), ScoreVarName[j]],
#             Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]]))
#     
#     legend("topright", legend=c("Scores Before Group Assignment", "Score at Assignment", "Study Group", "Control Group"),
#            col=c("blue", "purple", "green", "red"), pch=c(19,19,19,19), cex=0.65)
#     
#   }
#   
# }


###new changepoint way ####
fcp = function(tt,y,k=2) {
  library(MASS)
  n = length(tt)
  rr = NULL
  for(i in k:(n-1)) { 
    tt1 = (tt-tt[i]); tt1[tt1<0]= 0
    rr[i] = sum(rlm(y~tt1+tt,method="M")$resid^2)
  }
  which.min(rr[k:(n-1)])+k-1
}

# Study and control change point plots ####

cpinfo=as.data.frame(matrix(0, nrow=length(goodIDs), ncol=7))

colnames(cpinfo)=c("Pair ID", "ID", "Group", "CPX", "CPY", "Slope 1", "Slope 2")

j=1

k=1

par(mfrow=c(13,4), mar=c(0,0,0,0))

for (i in goodIDs){
  
  
  cpinfo[k,1]=matchrow.final[matchrow.final[,2]==i,1]
  
  cpinfo[k,2]=matchrow.final[matchrow.final[,2]==i,2]
  
  cpinfo[k,3]=matchrow.final[matchrow.final[,2]==i,3]
  
  cpdata=as.data.frame(cbind(Interpolate.Master[Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=3, ScoreVarName[j]],
                             Interpolate.Master[Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=3, "Days_afterOnset"]))
  
  colnames(cpdata)=c(ScoreVarName[j], "Days_afterOnset")
  
  cpdata[,"Label"]=(1:dim(cpdata)[1] + 2)
  
  cpdata[,2]=log(cpdata[,2])

  if (i==46 | i==53){
    
    cpdata=cpdata[-6,]
    
  }
  
  if (i==100){
    
    cpdata=cpdata[c(-5,-6),]
    
  }
  
  cprow=fcp(cpdata[which(is.na(cpdata[,1])==0),2], cpdata[which(is.na(cpdata[,1])==0),1])
  
  changepoint=cpdata[cprow,c(2,1)]
  
  cpinfo[k,4]=changepoint[1]
  
  cpinfo[k,5]=changepoint[2]
  
  cpinfo[k,6]=(changepoint[2]-cpdata[1,1])/(changepoint[1]-cpdata[1,2])
  
  cpinfo[k,7]=(changepoint[2]-cpdata[dim(cpdata[which(is.na(cpdata[,1])==0),])[1],1])/(changepoint[1]-cpdata[dim(cpdata[which(is.na(cpdata[,1])==0),])[1],2])
  
  
    #plot for Study Group
  
    plot(-100,-100, xlim=c(1,log(400)), ylim=c(-30,130), xlab="", ylab="")
  
  
    # plot(-100,-100, xlim=c(1,log(400)), ylim=c(-30,130), ylab=paste(ScoreName[j], "Score"),
    #      xlab="(ln of) Days After Stroke",
    #      main=paste(ScoreName[j], "Score by (ln of) Days After Stroke (Pair ID:", matchrow.final[matchrow.final[,2]==i,1], "& ID:", i, ")"))

        
    points(changepoint, pch=8, cex=2.5, col="orange")

    lines(c(cpdata[1,2], changepoint[1], cpdata[dim(cpdata[which(is.na(cpdata[,1])==0),])[1],2]),
          c(cpdata[1,1], changepoint[2], cpdata[dim(cpdata[which(is.na(cpdata[,1])==0),])[1],1]),
          lty=2, col="orange", lwd=2)

    points(log(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), "Days_afterOnset"]),
           Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), ScoreVarName[j]],
           pch=19,col="blue")
    
    points(log(Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), "Days_afterOnset"]),
           Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), ScoreVarName[j]],
           pch=19,col="purple")
    
    if (i==46){
      
      points(log(Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=7 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"]),
             Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=7 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]], 
             pch=19, col=colornames[matchrow.final[matchrow.final[,2]==i,3]+1])
      
    }else if (i==100){
    
    points(log(Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=6 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"]),
           Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=6 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]], 
           pch=19, col=colornames[matchrow.final[matchrow.final[,2]==i,3]+1])
    
    }else{
    
      points(log(Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"]),
             Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]], 
             pch=19, col=colornames[matchrow.final[matchrow.final[,2]==i,3]+1])

    }
      
    if (i==46){
    
    lines(c(log(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), "Days_afterOnset"]),
            log(Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), "Days_afterOnset"]),
            log(Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=7 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"])),
          c(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), ScoreVarName[j]],
            Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), ScoreVarName[j]],
            Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=7 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]]))
    
    }else if (i==100){
      
      
      lines(c(log(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), "Days_afterOnset"]),
              log(Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), "Days_afterOnset"]),
              log(Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=6 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"])),
            c(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), ScoreVarName[j]],
              Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), ScoreVarName[j]],
              Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=6 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]]))
      
    }else {
      
      
      lines(c(log(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), "Days_afterOnset"]),
              log(Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), "Days_afterOnset"]),
              log(Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"])),
            c(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), ScoreVarName[j]],
              Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), ScoreVarName[j]],
              Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]]))
      
      
      
    }
      
    # legend("topright", legend=c("Scores Before Group Assignment", "Score at Assignment", "Study Group", "Control Group", "Change Point"),
    #        col=c("blue", "purple", "green", "red", "orange"), pch=c(19,19,19,19,8), cex=0.65)

    k=k+1
    
  }

par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1))

# change point t-test ####

cptest=0

for (i in unique(cpinfo[,1])){
  
  N=length(which(cpinfo[,1]==i))
  
  if (N>1){
    
    for (j in which(cpinfo[,1]==i & cpinfo[,3]==1)){
      
      cptest=c(cptest, cpinfo[j,7]-cpinfo[cpinfo[,1]==i & cpinfo[,3]==0,7])
      
    }
    
  }
  
  
  ###run a loop for a paired t-test to compare slope 1 for matched pairs
  ###use which for multiple study patients matched to one control
  ###use dim to know how many and loop through the which vector to
  ###get difference for each pair
  
  
}

cptest=cptest[-1]

length(cptest)

t.test(cptest)

#use rlm need MASS





# Study change point plots ####
#31 studies

cpinfo=as.data.frame(matrix(0, nrow=length(intersect(goodIDs, ControlGroupIDs)), ncol=7))

colnames(cpinfo)=c("Pair ID", "ID", "Group", "CPX", "CPY", "Slope 1", "Slope 2")

k=1

par(mfrow=c(8,4), mar=c(0,0,0,0))

for (i in intersect(goodIDs, StudyGroupIDs)){
  
  
  cpinfo[k,1]=matchrow.final[matchrow.final[,2]==i,1]
  
  cpinfo[k,2]=matchrow.final[matchrow.final[,2]==i,2]
  
  cpinfo[k,3]=matchrow.final[matchrow.final[,2]==i,3]
  
  cpdata=as.data.frame(cbind(Interpolate.Master[Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=3, ScoreVarName[j]],
                             Interpolate.Master[Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=3, "Days_afterOnset"]))
  
  colnames(cpdata)=c(ScoreVarName[j], "Days_afterOnset")
  
  cpdata[,"Label"]=(1:dim(cpdata)[1] + 2)
  
  cpdata[,2]=log(cpdata[,2])
  
  if (i==46 | i==53){
    
    cpdata=cpdata[-6,]
    
  }
  
  if (i==100){
    
    cpdata=cpdata[c(-5,-6),]
    
  }
  
  cprow=fcp(cpdata[which(is.na(cpdata[,1])==0),2], cpdata[which(is.na(cpdata[,1])==0),1])
  
  changepoint=cpdata[cprow,c(2,1)]
  
  cpinfo[k,4]=changepoint[1]
  
  cpinfo[k,5]=changepoint[2]
  
  cpinfo[k,6]=(changepoint[2]-cpdata[1,1])/(changepoint[1]-cpdata[1,2])
  
  cpinfo[k,7]=(changepoint[2]-cpdata[dim(cpdata[which(is.na(cpdata[,1])==0),])[1],1])/(changepoint[1]-cpdata[dim(cpdata[which(is.na(cpdata[,1])==0),])[1],2])
  
  
  #plot for Study Group
  
  plot(-100,-100, xlim=c(0,log(400)), ylim=c(-30,130))
  
  
  # plot(-100,-100, xlim=c(0,log(400)), ylim=c(-30,130), ylab=paste(ScoreName[j], "Score"), 
  #      xlab="(ln of) Days After Stroke",
  #      main=paste(ScoreName[j], "Score by (ln of) Days After Stroke (Pair ID:", matchrow.final[matchrow.final[,2]==i,1], "& ID:", i, ")"))
  
  points(changepoint, pch=8, cex=2.5, col="orange")
  
  lines(c(cpdata[1,2], changepoint[1], cpdata[dim(cpdata[which(is.na(cpdata[,1])==0),])[1],2]),
        c(cpdata[1,1], changepoint[2], cpdata[dim(cpdata[which(is.na(cpdata[,1])==0),])[1],1]),
        lty=2, col="orange", lwd=2)
  
  points(log(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), "Days_afterOnset"]),
         Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), ScoreVarName[j]],
         pch=19,col="blue")
  
  points(log(Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), "Days_afterOnset"]),
         Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), ScoreVarName[j]],
         pch=19,col="purple")
  
  if (i==46){
    
    points(log(Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=7 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"]),
           Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=7 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]], 
           pch=19, col=colornames[matchrow.final[matchrow.final[,2]==i,3]+1])
    
  }else if (i==100){
    
    points(log(Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=6 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"]),
           Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=6 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]], 
           pch=19, col=colornames[matchrow.final[matchrow.final[,2]==i,3]+1])
    
  }else{
    
    points(log(Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"]),
           Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]], 
           pch=19, col=colornames[matchrow.final[matchrow.final[,2]==i,3]+1])
    
  }
  
  if (i==46){
    
    lines(c(log(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), "Days_afterOnset"]),
            log(Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), "Days_afterOnset"]),
            log(Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=7 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"])),
          c(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), ScoreVarName[j]],
            Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), ScoreVarName[j]],
            Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=7 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]]))
    
  }else if (i==100){
    
    
    lines(c(log(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), "Days_afterOnset"]),
            log(Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), "Days_afterOnset"]),
            log(Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=6 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"])),
          c(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), ScoreVarName[j]],
            Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), ScoreVarName[j]],
            Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=6 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]]))
    
  }else {
    
    
    lines(c(log(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), "Days_afterOnset"]),
            log(Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), "Days_afterOnset"]),
            log(Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"])),
          c(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), ScoreVarName[j]],
            Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), ScoreVarName[j]],
            Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]]))
    
    
    
  }
  
  # legend("topright", legend=c("Scores Before Group Assignment", "Score at Assignment", "Study Group", "Control Group", "Change Point"),
  #        col=c("blue", "purple", "green", "red", "orange"), pch=c(19,19,19,19,8), cex=0.65)
  
  k=k+1
  
}

par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1))

# Control change point plots ####
#20 controls

cpinfo=as.data.frame(matrix(0, nrow=length(intersect(goodIDs, ControlGroupIDs)), ncol=7))

colnames(cpinfo)=c("Pair ID", "ID", "Group", "CPX", "CPY", "Slope 1", "Slope 2")

k=1

par(mfrow=c(5,4), mar=c(0,0,0,0))

for (i in intersect(goodIDs, ControlGroupIDs)){
  
  
  cpinfo[k,1]=matchrow.final[matchrow.final[,2]==i,1]
  
  cpinfo[k,2]=matchrow.final[matchrow.final[,2]==i,2]
  
  cpinfo[k,3]=matchrow.final[matchrow.final[,2]==i,3]
  
  cpdata=as.data.frame(cbind(Interpolate.Master[Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=3, ScoreVarName[j]],
                             Interpolate.Master[Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=3, "Days_afterOnset"]))
  
  colnames(cpdata)=c(ScoreVarName[j], "Days_afterOnset")
  
  cpdata[,"Label"]=(1:dim(cpdata)[1] + 2)
  
  cpdata[,2]=log(cpdata[,2])
  
  if (i==46 | i==53){
    
    cpdata=cpdata[-6,]
    
  }
  
  if (i==100){
    
    cpdata=cpdata[c(-5,-6),]
    
  }
  
  cprow=fcp(cpdata[which(is.na(cpdata[,1])==0),2], cpdata[which(is.na(cpdata[,1])==0),1])
  
  changepoint=cpdata[cprow,c(2,1)]
  
  cpinfo[k,4]=changepoint[1]
  
  cpinfo[k,5]=changepoint[2]
  
  cpinfo[k,6]=(changepoint[2]-cpdata[1,1])/(changepoint[1]-cpdata[1,2])
  
  cpinfo[k,7]=(changepoint[2]-cpdata[dim(cpdata[which(is.na(cpdata[,1])==0),])[1],1])/(changepoint[1]-cpdata[dim(cpdata[which(is.na(cpdata[,1])==0),])[1],2])
  
  
  #plot for Study Group
  
  plot(-100,-100, xlim=c(0,log(400)), ylim=c(-30,130))
  
  
  # plot(-100,-100, xlim=c(0,log(400)), ylim=c(-30,130), ylab=paste(ScoreName[j], "Score"), 
  #      xlab="(ln of) Days After Stroke",
  #      main=paste(ScoreName[j], "Score by (ln of) Days After Stroke (Pair ID:", matchrow.final[matchrow.final[,2]==i,1], "& ID:", i, ")"))
  
  points(changepoint, pch=8, cex=2.5, col="orange")
  
  lines(c(cpdata[1,2], changepoint[1], cpdata[dim(cpdata[which(is.na(cpdata[,1])==0),])[1],2]),
        c(cpdata[1,1], changepoint[2], cpdata[dim(cpdata[which(is.na(cpdata[,1])==0),])[1],1]),
        lty=2, col="orange", lwd=2)
  
  points(log(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), "Days_afterOnset"]),
         Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), ScoreVarName[j]],
         pch=19,col="blue")
  
  points(log(Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), "Days_afterOnset"]),
         Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), ScoreVarName[j]],
         pch=19,col="purple")
  
  if (i==46){
    
    points(log(Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=7 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"]),
           Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=7 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]], 
           pch=19, col=colornames[matchrow.final[matchrow.final[,2]==i,3]+1])
    
  }else if (i==100){
    
    points(log(Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=6 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"]),
           Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=6 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]], 
           pch=19, col=colornames[matchrow.final[matchrow.final[,2]==i,3]+1])
    
  }else{
    
    points(log(Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"]),
           Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]], 
           pch=19, col=colornames[matchrow.final[matchrow.final[,2]==i,3]+1])
    
  }
  
  if (i==46){
    
    lines(c(log(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), "Days_afterOnset"]),
            log(Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), "Days_afterOnset"]),
            log(Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=7 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"])),
          c(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), ScoreVarName[j]],
            Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), ScoreVarName[j]],
            Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=7 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]]))
    
  }else if (i==100){
    
    
    lines(c(log(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), "Days_afterOnset"]),
            log(Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), "Days_afterOnset"]),
            log(Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=6 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"])),
          c(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), ScoreVarName[j]],
            Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), ScoreVarName[j]],
            Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=6 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]]))
    
  }else {
    
    
    lines(c(log(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), "Days_afterOnset"]),
            log(Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), "Days_afterOnset"]),
            log(Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),"Days_afterOnset"])),
          c(Interpolate.Master[((Interpolate.Master[,"ID"]==i & (Interpolate.Master[,"DaysId"]==1 | Interpolate.Master[,"DaysId"]==2))), ScoreVarName[j]],
            Interpolate.Master[((Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]==3)), ScoreVarName[j]],
            Interpolate.Master[(Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=4 ),ScoreVarName[j]]))
    
    
    
  }
  
  # legend("topright", legend=c("Scores Before Group Assignment", "Score at Assignment", "Study Group", "Control Group", "Change Point"),
  #        col=c("blue", "purple", "green", "red", "orange"), pch=c(19,19,19,19,8), cex=0.65)
  
  k=k+1
  
}

par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1))

# change point summary info ####

hist(exp(cpinfo[,"CPX"]), xlab = "Days After Stroke", 
     main="Distribution of Changepoint (Days After Stroke)")

hist(cpinfo[,"CPX"], xlab = "(ln of) Days After Stroke", 
     main="Distribution of Changepoint (ln of Days After Stroke)")

hist(cpinfo[,"Slope 1"], xlab= "Slope Before Changepoint", main="Distribution of Slopes Before Changepoint")

hist(cpinfo[,"Slope 2"], xlab= "Slope After Changepoint", main="Distribution of Slopes After Changepoint")

hist(exp(cpinfo[cpinfo[,3]==0,"CPX"]), xlab = "Days After Stroke", 
     main="Distribution of Changepoint (Days After Stroke) for Control Group")

hist(cpinfo[cpinfo[,3]==0,"CPX"], xlab = "(ln of) Days After Stroke", 
     main="Distribution of Changepoint (ln of Days After Stroke) for Control Group")

hist(cpinfo[cpinfo[,3]==0,"Slope 1"], xlab= "Slope Before Changepoint", main="Distribution of Slopes Before Changepoint for Control Group")

hist(cpinfo[cpinfo[,3]==0,"Slope 2"], xlab= "Slope After Changepoint", main="Distribution of Slopes After Changepoint for Control Group")

hist(exp(cpinfo[cpinfo[,3]==1,"CPX"]), xlab = "Days After Stroke", 
     main="Distribution of Changepoint (Days After Stroke) for Study Group")

hist(cpinfo[cpinfo[,3]==1,"CPX"], xlab = "(ln of) Days After Stroke", 
     main="Distribution of Changepoint (ln of Days After Stroke) for Study Group")

hist(cpinfo[cpinfo[,3]==1,"Slope 1"], xlab= "Slope Before Changepoint", main="Distribution of Slopes Before Changepoint for Study Group")

hist(cpinfo[cpinfo[,3]==1,"Slope 2"], xlab= "Slope After Changepoint", main="Distribution of Slopes After Changepoint for Study Group")


# For new plot ####

match.subgroup=Interpolate.Master[1,]

for (i in matchrow.final[,2]){
  
  match.subgroup=rbind(match.subgroup, Interpolate.Master[Interpolate.Master[,"ID"]==i,])
  
}

match.subgroup=match.subgroup[-1,]


mob.study=t.test(match.subgroup[match.subgroup[,"Group"]=="Study Group" & 
                            match.subgroup[,"DaysId"]>=4 & 
                            match.subgroup[,"DaysId"]<=8,"PT.AM.PAC.Basic.Mobility.Score"]-match.subgroup[match.subgroup[,"Group"]=="Study Group" & 
                                                                                                                    match.subgroup[,"DaysId"]>=4 & 
                                                                                                                    match.subgroup[,"DaysId"]<=8,"Baseline.Mobility"], na.rm=TRUE)$estimate[1]

mob.study.inf=t.test(match.subgroup[match.subgroup[,"Group"]=="Study Group" & 
                            match.subgroup[,"DaysId"]>=4 & 
                            match.subgroup[,"DaysId"]<=8,"PT.AM.PAC.Basic.Mobility.Score"]-match.subgroup[match.subgroup[,"Group"]=="Study Group" & 
                                                                                                                    match.subgroup[,"DaysId"]>=4 & 
                                                                                                                    match.subgroup[,"DaysId"]<=8,"Baseline.Mobility"], na.rm=TRUE)$conf.int[1:2]

mob.control=t.test(match.subgroup[match.subgroup[,"Group"]=="Control Group" & 
                            match.subgroup[,"DaysId"]>=4 & 
                            match.subgroup[,"DaysId"]<=8,"PT.AM.PAC.Basic.Mobility.Score"]-match.subgroup[match.subgroup[,"Group"]=="Control Group" & 
                                                                                                                    match.subgroup[,"DaysId"]>=4 & 
                                                                                                                    match.subgroup[,"DaysId"]<=8,"Baseline.Mobility"], na.rm=TRUE)$estimate[1]

mob.control.inf=t.test(match.subgroup[match.subgroup[,"Group"]=="Control Group" & 
                            match.subgroup[,"DaysId"]>=4 & 
                            match.subgroup[,"DaysId"]<=8,"PT.AM.PAC.Basic.Mobility.Score"]-match.subgroup[match.subgroup[,"Group"]=="Control Group" & 
                                                                                                                    match.subgroup[,"DaysId"]>=4 & 
                                                                                                                    match.subgroup[,"DaysId"]<=8,"Baseline.Mobility"], na.rm=TRUE)$conf.int[1:2]

act.study=t.test(match.subgroup[match.subgroup[,"Group"]=="Study Group" & 
                                      match.subgroup[,"DaysId"]>=4 & 
                                      match.subgroup[,"DaysId"]<=8,"OT.AM.Daily.Activity.Score"]-match.subgroup[match.subgroup[,"Group"]=="Study Group" & 
                                                                                                                              match.subgroup[,"DaysId"]>=4 & 
                                                                                                                              match.subgroup[,"DaysId"]<=8,"Baseline.Activity"], na.rm=TRUE)$estimate[1]

act.study.inf=t.test(match.subgroup[match.subgroup[,"Group"]=="Study Group" & 
                                          match.subgroup[,"DaysId"]>=4 & 
                                          match.subgroup[,"DaysId"]<=8,"OT.AM.Daily.Activity.Score"]-match.subgroup[match.subgroup[,"Group"]=="Study Group" & 
                                                                                                                                  match.subgroup[,"DaysId"]>=4 & 
                                                                                                                                  match.subgroup[,"DaysId"]<=8,"Baseline.Activity"], na.rm=TRUE)$conf.int[1:2]

act.control=t.test(match.subgroup[match.subgroup[,"Group"]=="Control Group" & 
                                        match.subgroup[,"DaysId"]>=4 & 
                                        match.subgroup[,"DaysId"]<=8,"OT.AM.Daily.Activity.Score"]-match.subgroup[match.subgroup[,"Group"]=="Control Group" & 
                                                                                                                                match.subgroup[,"DaysId"]>=4 & 
                                                                                                                                match.subgroup[,"DaysId"]<=8,"Baseline.Activity"], na.rm=TRUE)$estimate[1]

act.control.inf=t.test(match.subgroup[match.subgroup[,"Group"]=="Control Group" & 
                                            match.subgroup[,"DaysId"]>=4 & 
                                            match.subgroup[,"DaysId"]<=8,"OT.AM.Daily.Activity.Score"]-match.subgroup[match.subgroup[,"Group"]=="Control Group" & 
                                                                                                                                    match.subgroup[,"DaysId"]>=4 & 
                                                                                                                                    match.subgroup[,"DaysId"]<=8,"Baseline.Activity"], na.rm=TRUE)$conf.int[1:2]




cog.study=t.test(match.subgroup[match.subgroup[,"Group"]=="Study Group" & 
                                      match.subgroup[,"DaysId"]>=4 & 
                                      match.subgroup[,"DaysId"]<=8,"ST.AM.Applied.Cogn.Score"]-match.subgroup[match.subgroup[,"Group"]=="Study Group" & 
                                                                                                                          match.subgroup[,"DaysId"]>=4 & 
                                                                                                                          match.subgroup[,"DaysId"]<=8,"Baseline.Cognitive"], na.rm=TRUE)$estimate[1]

cog.study.inf=t.test(match.subgroup[match.subgroup[,"Group"]=="Study Group" & 
                                          match.subgroup[,"DaysId"]>=4 & 
                                          match.subgroup[,"DaysId"]<=8,"ST.AM.Applied.Cogn.Score"]-match.subgroup[match.subgroup[,"Group"]=="Study Group" & 
                                                                                                                              match.subgroup[,"DaysId"]>=4 & 
                                                                                                                              match.subgroup[,"DaysId"]<=8,"Baseline.Cognitive"], na.rm=TRUE)$conf.int[1:2]

cog.control=t.test(match.subgroup[match.subgroup[,"Group"]=="Control Group" & 
                                        match.subgroup[,"DaysId"]>=4 & 
                                        match.subgroup[,"DaysId"]<=8,"ST.AM.Applied.Cogn.Score"]-match.subgroup[match.subgroup[,"Group"]=="Control Group" & 
                                                                                                                            match.subgroup[,"DaysId"]>=4 & 
                                                                                                                            match.subgroup[,"DaysId"]<=8,"Baseline.Cognitive"], na.rm=TRUE)$estimate[1]

cog.control.inf=t.test(match.subgroup[match.subgroup[,"Group"]=="Control Group" & 
                                            match.subgroup[,"DaysId"]>=4 & 
                                            match.subgroup[,"DaysId"]<=8,"ST.AM.Applied.Cogn.Score"]-match.subgroup[match.subgroup[,"Group"]=="Control Group" & 
                                                                                                                                match.subgroup[,"DaysId"]>=4 & 
                                                                                                                                match.subgroup[,"DaysId"]<=8,"Baseline.Cognitive"], na.rm=TRUE)$conf.int[1:2]





# For new new plot ####

mob.study=t.test(Interpolate.Master.One[Interpolate.Master.One[,"Group"]=="Study Group", "Baseline.Mobility"])$estimate

mob.study.inf=t.test(Interpolate.Master.One[Interpolate.Master.One[,"Group"]=="Study Group", "Baseline.Mobility"])$conf.int[1:2]

mob.control=t.test(Interpolate.Master.One[Interpolate.Master.One[,"Group"]=="Control Group", "Baseline.Mobility"])$estimate

mob.control.inf=t.test(Interpolate.Master.One[Interpolate.Master.One[,"Group"]=="Control Group", "Baseline.Mobility"])$conf.int[1:2]

act.study=t.test(Interpolate.Master.One[Interpolate.Master.One[,"Group"]=="Study Group", "Baseline.Activity"])$estimate

act.study.inf=t.test(Interpolate.Master.One[Interpolate.Master.One[,"Group"]=="Study Group", "Baseline.Activity"])$conf.int[1:2]

act.control=t.test(Interpolate.Master.One[Interpolate.Master.One[,"Group"]=="Control Group", "Baseline.Activity"])$estimate

act.control.inf=t.test(Interpolate.Master.One[Interpolate.Master.One[,"Group"]=="Control Group", "Baseline.Activity"])$conf.int[1:2]

cog.study=t.test(Interpolate.Master.One[Interpolate.Master.One[,"Group"]=="Study Group", "Baseline.Cognitive"])$estimate

cog.study.inf=t.test(Interpolate.Master.One[Interpolate.Master.One[,"Group"]=="Study Group", "Baseline.Cognitive"])$conf.int[1:2]

cog.control=t.test(Interpolate.Master.One[Interpolate.Master.One[,"Group"]=="Control Group", "Baseline.Cognitive"])$estimate

cog.control.inf=t.test(Interpolate.Master.One[Interpolate.Master.One[,"Group"]=="Control Group", "Baseline.Cognitive"])$conf.int[1:2]







# Actual Plot####

par(cex=0.7)

plot(100,100, xlim=c(0,6), ylim=c(1.15,30), xaxt="n",
     main="Average Difference from Baseline for Rehabilitation Programs", 
     xlab="Rehabilitation Program", ylab="Average Difference from Baseline")

par(cex=1)

legend(4, 30, bty="n", 
       legend=c("Mobility", "Activity" ,"Cognitive"), 
       pch=c(15,16,17),
       col=c("blue", "red", "purple"))

lines(c(0.5, 0.5, NA, 1.5, 1.5), c(mob.study.inf, NA, mob.control.inf))

lines(c(0.35, 0.65, NA, 0.35, 0.65), c(mob.study.inf[1], mob.study.inf[1],
                                       NA, mob.study.inf[2], mob.study.inf[2]))

lines(c(1.35, 1.65, NA, 1.35, 1.65), c(mob.control.inf[1], mob.control.inf[1],
                                       NA, mob.control.inf[2], mob.control.inf[2]))

points(c(0.5, 1.5), c(mob.study, mob.control), pch=15, cex=1, col="blue")

lines(c(2.5, 2.5, NA, 3.5, 3.5), c(act.study.inf, NA, act.control.inf))

lines(c(2.35, 2.65, NA, 2.35, 2.65), c(act.study.inf[1], act.study.inf[1],
                                       NA, act.study.inf[2], act.study.inf[2]))

lines(c(3.35, 3.65, NA, 3.35, 3.65), c(act.control.inf[1], act.control.inf[1],
                                       NA, act.control.inf[2], act.control.inf[2]))

points(c(2.5, 3.5), c(act.study, act.control), pch=16, cex=1, col="red")

lines(c(4.5, 4.5, NA, 5.5, 5.5), c(cog.study.inf, NA, cog.control.inf))

lines(c(4.35, 4.65, NA, 4.35, 4.65), c(cog.study.inf[1], cog.study.inf[1],
                                       NA, cog.study.inf[2], cog.study.inf[2]))

lines(c(5.35, 5.65, NA, 5.35, 5.65), c(cog.control.inf[1], cog.control.inf[1],
                                       NA, cog.control.inf[2], cog.control.inf[2]))
points(c(4.5, 5.5), c(cog.study, cog.control), pch=17, cex=1, col="purple")

par(cex=0.75)

axis(side=1, at=seq(0.5,5.5,1), labels=rep(c("Study", "Control"), 3))







# slope analysis ####

slopeinfo=as.data.frame(matrix(0, nrow=length(goodIDs), ncol=4))

colnames(slopeinfo)=c("Pair ID", "ID", "Group", "Slope")

j=3

k=1

for (i in goodIDs){
  
  
  slopeinfo[k,1]=matchrow.final[matchrow.final[,2]==i,1]
  
  slopeinfo[k,2]=matchrow.final[matchrow.final[,2]==i,2]
  
  slopeinfo[k,3]=matchrow.final[matchrow.final[,2]==i,3]
  
  slopedata=as.data.frame(cbind(Interpolate.Master[Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=3, ScoreVarName[j]],
                             Interpolate.Master[Interpolate.Master[,"ID"]==i & Interpolate.Master[,"DaysId"]<=8 & Interpolate.Master[,"DaysId"]>=3, "Days_afterOnset"]))
  
  colnames(slopedata)=c(ScoreVarName[j], "Days_afterOnset")
  
  slopedata[,"Label"]=(1:dim(slopedata)[1] + 2)
  
  slopedata[,2]=log(slopedata[,2])
  
  if (i==46 | i==53){
    
    slopedata=slopedata[-6,]
    
  }
  
  if (i==100){
    
    slopedata=slopedata[c(-5,-6),]
    
  }
  
  slopeinfo[k,4]=lm(slopedata[,ScoreVarName[j]]~slopedata[,"Days_afterOnset"])$coefficients[2]

    
  k=k+1
  
}

# slope t-test ####

slopetest=0

for (i in unique(slopeinfo[,1])){
  
  N=length(which(slopeinfo[,1]==i))
  
  if (N>1){
    
    for (j in which(slopeinfo[,1]==i & slopeinfo[,3]==1)){
      
      slopetest=c(slopetest, slopeinfo[j,4]-slopeinfo[slopeinfo[,1]==i & slopeinfo[,3]==0,4])
      
    }
    
  }
  
  
  ###run a loop for a paired t-test to compare slope 1 for matched pairs
  ###use which for multiple study patients matched to one control
  ###use dim to know how many and loop through the which vector to
  ###get difference for each pair
  
  
}

slopetest=slopetest[-1]

t.test(slopetest)

#use rlm need MASS











