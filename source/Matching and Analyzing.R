#################################Program Description################################
#Name: Three Factor Matching and Analyzing                                         #
#Author: Traymon Beavers                                                           #
#Date Created: 3/29/2017                                                           #
#Purpose: To match the data based on gender, race, age, baseline average score and #
#         type of stroke for mobility, activity, and cognitive ability and then    #
#         perform analysis with matched data groups                                #                  #
#Functions: matching                                                               #
#                                                                                  #
####################################################################################


###Upload Data in R
OldMaster=read.csv("Data/DEID_All.csv")

###Delete extraneous varibale
OldMaster=OldMaster[,-1]

Master=OldMaster

###Delete extraneous data

#Delete repeated observations
# T=1
# 
# for (i in 1:926){
#   
#   if (Master[i,"ID"]==T){
#     T=T+1
#   }else {
#     Master[i,1:69]=rep(NA,69)   
#   }
#   
# }


#Delete Observation doctors didnt want included

Master=Master[-656:-662,]

#Create consistent missing value indicators for each functional outcome

for (i in 1:919){
  
  if(Master[i,"PT.AM.PAC.Basic.Mobility.Score"]==9999
     | Master[i,"PT.AM.PAC.Basic.Mobility.Score"]==8888
     | is.na(Master[i,"PT.AM.PAC.Basic.Mobility.Score"])==1){
    
    Master[i,"PT.AM.PAC.Basic.Mobility.Score"]=NA
    
  }
  
  if(Master[i,"OT.AM.Daily.Activity.Score"]==9999
     | Master[i,"OT.AM.Daily.Activity.Score"]==8888
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

###Create a vector for the patient IDs in each group, for each group

#Count the number of people in the deceased group

ND=1

for (i in 2:66){
  if (DeceasedGroup[i,"ID"]!=DeceasedGroup[(i-1),"ID"]){
    ND=ND+1
  }
}

#create vector to fill for study group IDs

DeceasedGroupIDs=rep(27,ND)

T=1

for (i in 2:66){
  if (DeceasedGroup[i,"ID"]!=DeceasedGroup[(i-1),"ID"]){
    T=T+1
    DeceasedGroupIDs[T]=DeceasedGroup[i,"ID"]
  }
}

#convert first instance of NAs to 0 for plot

# T=1
# 
# for (i in DeceasedGroupIDs){
# 
#     if (is.na(DeceasedGroup[(DeceasedGroup[,"ID"]==i & DeceasedGroup[,"DaysId"]==T), "PT.AM.PAC.Basic.Mobility.Score"])==0){
#       
#       T=T+1
#       
#     }else{ 
#       
#       DeceasedGroup[(DeceasedGroup[,"ID"]==i & DeceasedGroup[,"DaysId"]==T),"PT.AM.PAC.Basic.Mobility.Score"]=0
#       
#       T=1      
#     }
#        
# }

#Count the number of people in the study group

NS=1

for (i in 2:157){
  if (StudyGroup[i,"ID"]!=StudyGroup[(i-1),"ID"]){
    NS=NS+1
  }
}

#create vector to fill for study group IDs

StudyGroupIDs=rep(12,NS)

T=1

for (i in 2:157){
  if (StudyGroup[i,"ID"]!=StudyGroup[(i-1),"ID"]){
    T=T+1
    StudyGroupIDs[T]=StudyGroup[i,"ID"]
  }
}


#Count the number of people in the control group

NC=1

for (i in 2:448){
  if (ControlGroup[i,"ID"]!=ControlGroup[(i-1),"ID"]){
    NC=NC+1
  }
}

#create vector to fill for study group IDs

ControlGroupIDs=rep(1,NC)

T=1

for (i in 2:448){
  if (ControlGroup[i,"ID"]!=ControlGroup[(i-1),"ID"]){
    T=T+1
    ControlGroupIDs[T]=ControlGroup[i,"ID"]
  }
}

###Match people in baseline group to their respective study or control groups

#Match for study group

NewStudyGroup=StudyGroup

T=1

for (i in 1:314){
  
  if (BaselineGroup[i,"ID"]==StudyGroupIDs[T] && T!=32){
    
    NewStudyGroup=rbind(NewStudyGroup,BaselineGroup[i,])
    
  }
  
  if (BaselineGroup[i,"ID"]==StudyGroupIDs[T] && BaselineGroup[(i+1),"ID"]!=StudyGroupIDs[T] 
      && i!=314){
    
    T=T+1
  }
  
}

#Sort by ID and days after stroke

Sort.NewStudyGroup=NewStudyGroup[order(NewStudyGroup$ID, NewStudyGroup$DaysId),]

#Match for control group

NewControlGroup=ControlGroup

T=1

for (i in 1:314){
  
  if (BaselineGroup[i,"ID"]==ControlGroupIDs[T] && T!=91){
    
    NewControlGroup=rbind(NewControlGroup,BaselineGroup[i,])
    
  }
  
  if (BaselineGroup[i,"ID"]==ControlGroupIDs[T] && BaselineGroup[(i+1),"ID"]!=ControlGroupIDs[T] 
      && i!=314 && T!=91){
    
    T=T+1
  }
  
}

#Sort by ID and days after stroke

Sort.NewControlGroup=NewControlGroup[order(NewControlGroup$ID, NewControlGroup$DaysId),]

###Find the IDs for the crossovers

T=0

for (i in ControlGroupIDs){
  for (j in StudyGroupIDs){
    
    if (i==j){
      
      print(i)
      T=T+1
      
    }
    
  }
}

#output was 24, 46, and 124

##########################ANALYSIS WITH THREE FACTOR MATCHING###################################

###Rename races other than Black or White as "Other" in the study and control groups

for (i in 1:219){
  
  if (Sort.NewStudyGroup[i,"Race"]==3){
    
    Sort.NewStudyGroup[i,"New Race"]="Black"
    
  }else if (Sort.NewStudyGroup[i,"Race"]==5){
    
    Sort.NewStudyGroup[i,"New Race"]="White"
    
  }else {
    
    Sort.NewStudyGroup[i,"New Race"]="Other"
    
  }  
  
}


for (i in 1:628){
  
  if (Sort.NewControlGroup[i,"Race"]==3){
    
    Sort.NewControlGroup[i,"New Race"]="Black"
    
  }else if (Sort.NewControlGroup[i,"Race"]==5){
    
    Sort.NewControlGroup[i,"New Race"]="White"
    
  }else {
    
    Sort.NewControlGroup[i,"New Race"]="Other"
    
  }  
  
}


###Create a new variable for each treatment group for the average of the first two baseline scores

ScoreName=c("Mobility", "Activity", "Cognitive")
ScoreVarName=c("PT.AM.PAC.Basic.Mobility.Score", "OT.AM.Daily.Activity.Score", "ST.AM.Applied.Cogn.Score")


for (j in 1:3){

  T=1
  
  for (i in 1:219){
    
    if (Sort.NewStudyGroup[i,"ID"]!=StudyGroupIDs[T] & T!=32){
      
      T=T+1
      
    }
    
    Sort.NewStudyGroup[i, paste("Baseline Average", ScoreName[j], sep=".")]=
      
      mean(c(Sort.NewStudyGroup[Sort.NewStudyGroup[,"DaysId"]==1 & Sort.NewStudyGroup[,"ID"]==StudyGroupIDs[T], ScoreVarName[j]], 
             Sort.NewStudyGroup[Sort.NewStudyGroup[,"DaysId"]==2 & Sort.NewStudyGroup[,"ID"]==StudyGroupIDs[T], ScoreVarName[j]]))
    
  }
  
  T=1
  
  for (i in 1:628){
    
    if (Sort.NewControlGroup[i,"ID"]!=ControlGroupIDs[T] & T!=91){
      
      T=T+1
      
    }
    
    Sort.NewControlGroup[i, paste("Baseline Average", ScoreName[j], sep=".")]=
      
      mean(c(Sort.NewControlGroup[Sort.NewControlGroup[,"DaysId"]==1 & Sort.NewControlGroup[,"ID"]==ControlGroupIDs[T], ScoreVarName[j]], 
             Sort.NewControlGroup[Sort.NewControlGroup[,"DaysId"]==2 & Sort.NewControlGroup[,"ID"]==ControlGroupIDs[T], ScoreVarName[j]]))
    
    
  }

}


###Combine both treatment groups in same dataset for use with matching function

NewMaster=rbind(Sort.NewStudyGroup, Sort.NewControlGroup)

NewMaster=NewMaster[order(NewMaster[,"ID"], NewMaster[,"DaysId"]),]

for (i in 1:847){
  
  if (NewMaster[i,"Group"]=="Study Group"){
    
    NewMaster[i,"GroupNum"]=1
    
  }else if (NewMaster[i,"Group"]=="Control Group"){
    
    NewMaster[i,"GroupNum"]=2
    
  }else {
    
    NewMaster[i,"GroupNum"]=3
    
  }
  
}


###Create a dataset that only contains 1 datapoint for each patient

NewMaster.One=NewMaster[NewMaster[,"DaysId"]==3,]

for (i in 1:118){
  
  if (NewMaster.One[i,"Group"]=="Study Group"){
    
  NewMaster.One[i,"Treatment"]=0
    
  }else {
    
    NewMaster.One[i,"Treatment"]=1
    
  }
  
}

###demonstrate glitch in Matching Function

# set.seed(10301992)
# 
# matchrows=match3w(y=NewMaster.One[,"Treatment"], xpp=NewMaster.One[,c("Smoker","Current.pregnancy")], xe=NewMaster.One[,c("Gender", "New Race")], xpt=NewMaster.One[,c("Age","Baseline Average.Mobility")], kpt=c(2,20))
# 
# for (i in 1:30){
# 
# print(NewMaster.One[matchrows[i,], c("ID", "Group", "Age", "Gender", "New Race", "Baseline Average.Mobility")])
# 
# }
  
###Create a data matrix for the characteristics for each patient in the study group

Study.Characteristics=as.data.frame(matrix(nrow=31, ncol=8))


T=1

for (i in 1:219){

  if (Sort.NewStudyGroup[i,"ID"]==StudyGroupIDs[T] & T!=32){

    Study.Characteristics[T,]=Sort.NewStudyGroup[i,c("ID", "Age", "Gender", "New Race", 
                                                     "Baseline Average.Mobility", 
                                                     "Baseline Average.Activity",
                                                     "Baseline Average.Cognitive",
                                                     "Type.of.Stroke")]

    T=T+1

  }

}

colnames(Study.Characteristics)=c("ID","Age","Gender", "New Race","Baseline Average.Mobility",
                                  "Baseline Average.Activity", "Baseline Average.Cognitive",
                                  "Type of Stroke")

for (i in 1:31){

  if (Study.Characteristics[i,"Gender"]==2){

    Study.Characteristics[i,"Gender"]="Male"

  }else if (Study.Characteristics[i,"Gender"]==1){

    Study.Characteristics[i,"Gender"]="Female"

  }
  
  if (Study.Characteristics[i,"Type of Stroke"]==2){
    
    Study.Characteristics[i,"Type of Stroke"]="ISCHEMIC CVA"
    
  }else if (Study.Characteristics[i,"Type of Stroke"]==1){
    
    Study.Characteristics[i,"Type of Stroke"]="HEMORRHAGIC - INTRACEREBRAL/INTRACEREBELLAR/INTRAP"
    
  }

}

NewMaster.One.Control=NewMaster.One[NewMaster.One[,"Group"]=="Control Group",]

#####################################Function#######################################
#Name: matching                                                                    #
#Author: Traymon Beavers                                                           #
#Date Created: 3/29/2017                                                           #
#Purpose: To match the data based on gender, race, age, baseline average score and #
#         type of stroke for mobility, activity, or cognitive ability or print out #
#         matching IDs and count of distinct study group IDs                       #                           #
#Variables: AgeNum-width for age partial matching                                  #
#           BaselineNum-width for baseline average partial matching                #
#           ScoreNum-choice for which baseline average: 1 for Mobility, 2 for      #
#                    Activity, and 3 for Cognitive                                 #
#           List-print out matching IDs or count: 1 for list of matching IDs or 0  #
#                for count                                                         #
#                                                                                  #
####################################################################################

matching=function(AgeNum=2, BaselineNum=20, ScoreNum=1, List=1){
  
  ScoreName=c("Mobility", "Activity", "Cognitive")

  x=c(0,0)
  
  for (i in 1:88){
    
    for (j in 1:31){
      
      if (NewMaster.One.Control[i,"Age"]>=(Study.Characteristics[j,"Age"] - AgeNum) &
          NewMaster.One.Control[i,"Age"]<=(Study.Characteristics[j,"Age"] + AgeNum) &
          NewMaster.One.Control[i,paste("Baseline Average", ScoreName[ScoreNum],  sep=".")]>=(Study.Characteristics[j,paste("Baseline Average", ScoreName[ScoreNum],  sep=".")] - BaselineNum) &
          NewMaster.One.Control[i,paste("Baseline Average", ScoreName[ScoreNum],  sep=".")]<=(Study.Characteristics[j,paste("Baseline Average", ScoreName[ScoreNum],  sep=".")] + BaselineNum) &
          NewMaster.One.Control[i,"Gender"]==Study.Characteristics[j,"Gender"] &
          NewMaster.One.Control[i,"New Race"]==Study.Characteristics[j,"New Race"] &
          NewMaster.One.Control[i,"Type.of.Stroke"]==Study.Characteristics[j,"Type of Stroke"] &
          Study.Characteristics[j,"ID"]!=24 & NewMaster.One.Control[i,"Deceased_Y.N"]==FALSE &
          Study.Characteristics[j,"ID"]!=46 & Study.Characteristics[j,"ID"]!=124)
        
        x=rbind(x,c(Study.Characteristics[j,"ID"], NewMaster.One.Control[i,"ID"]))
      
    }
    
  }
  
  colnames(x)=c("Study ID", "Control ID")
  
  x=x[-1,]
  
  x=x[order(x[,1], x[,2]),]
  
  Count=0
  
  for (i in 2:dim(x)[1]){
    
    if (x[i,1]!=x[i-1,1]){
     
      Count=Count+1
       
    }
    
  }
  
  if (x[1,1]!=x[2,1]){
    
    Count=Count+1
    
  }

  if (List==1){
  
  return(x)
    
  }
  
  if (List==0){
    
    return(Count)
    
  }
  
}

#################################End Function#######################################

###Demonstrate issue with propensity score matching

Medical.History.Variables=colnames(NewMaster.One)[47:69]

fmla=as.formula(paste("Group ~ ", paste(Medical.History.Variables, collapse="+")))

glm.out=glm(formula=fmla, family=binomial(logit),
            data=rbind(NewMaster.One[NewMaster.One["ID"]==5 | NewMaster.One["ID"]==24 | 
                                    NewMaster.One["ID"]==60 | NewMaster.One["ID"]==64 ,])
                                      [,c("Group", Medical.History.Variables)])

glm.out$fitted.values

fmla=as.formula(paste("Group ~ ", paste(Medical.History.Variables[3:6], collapse="+")))

glm.out=glm(formula=fmla, family=binomial(logit),
            data=rbind(NewMaster.One[NewMaster.One["ID"]==5 | NewMaster.One["ID"]==24 | 
                                       NewMaster.One["ID"]==60 | NewMaster.One["ID"]==64 ,])
            [,c("Group", Medical.History.Variables[3:6])])

glm.out$fitted.values

Medical.History.Variables[3:6]


##############################MATCHING ANALYSIS##################################

###match study group patients to control group patients who are the same gender and race, 
###and are within 5 years of age and 20 baseline average points 

MatchIDs.5.20.Mobility=matching(5,20,1)
MatchIDs.5.20.Activity=matching(5,20,2)
MatchIDs.5.20.Cognitive=matching(5,20,3)

###############################FOURTH FOLLOW UP###############################


###conduct analysis for 4th day follow up for mobility scores

test4data.Mobility=0

for (i in 2:dim(MatchIDs.5.20.Mobility)[1]){
  
  if (MatchIDs.5.20.Mobility[i,1]!=MatchIDs.5.20.Mobility[i-1,1]){
    
    studydiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Mobility[i,1] & NewMaster[,"DaysId"]==4, "PT.AM.PAC.Basic.Mobility.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Mobility[i,1] & NewMaster[,"DaysId"]==4, "Baseline Average.Mobility"]
    
    controldiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Mobility[i,2] & NewMaster[,"DaysId"]==4, "PT.AM.PAC.Basic.Mobility.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Mobility[i,2] & NewMaster[,"DaysId"]==4, "Baseline Average.Mobility"]
    
    test4data.Mobility=c(test4data.Mobility, studydiff-controldiff)
    
  }
  
}

if (MatchIDs.5.20.Mobility[1,1]!=MatchIDs.5.20.Mobility[2,1]){
  
  studydiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Mobility[i,1] & NewMaster[,"DaysId"]==4, "PT.AM.PAC.Basic.Mobility.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Mobility[i,1] & NewMaster[,"DaysId"]==4, "Baseline Average.Mobility"]
  
  controldiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Mobility[i,2] & NewMaster[,"DaysId"]==4, "PT.AM.PAC.Basic.Mobility.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Mobility[i,2] & NewMaster[,"DaysId"]==4, "Baseline Average.Mobility"]
  
  test4data.Mobility=c(test4data.Mobility, studydiff-controldiff)
  
}

test4data.Mobility=test4data.Mobility[-1]

t.test(test4data.Mobility, alternative="greater")

###conduct analysis for 4th day follow up for activity scores

test4data.Activity=0

for (i in 2:dim(MatchIDs.5.20.Activity)[1]){
  
  if (MatchIDs.5.20.Activity[i,1]!=MatchIDs.5.20.Activity[i-1,1]){
    
    studydiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Activity[i,1] & NewMaster[,"DaysId"]==4, "OT.AM.Daily.Activity.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Activity[i,1] & NewMaster[,"DaysId"]==4, "Baseline Average.Activity"]
    
    controldiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Activity[i,2] & NewMaster[,"DaysId"]==4, "OT.AM.Daily.Activity.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Activity[i,2] & NewMaster[,"DaysId"]==4, "Baseline Average.Activity"]
    
    test4data.Activity=c(test4data.Activity, studydiff-controldiff)
    
  }
  
}

if (MatchIDs.5.20.Activity[1,1]!=MatchIDs.5.20.Activity[2,1]){
  
  studydiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Activity[i,1] & NewMaster[,"DaysId"]==4, "OT.AM.Daily.Activity.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Activity[i,1] & NewMaster[,"DaysId"]==4, "Baseline Average.Activity"]
  
  controldiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Activity[i,2] & NewMaster[,"DaysId"]==4, "OT.AM.Daily.Activity.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Activity[i,2] & NewMaster[,"DaysId"]==4, "Baseline Average.Activity"]
  
  test4data.Activity=c(test4data.Activity, studydiff-controldiff)
  
}

test4data.Activity=test4data.Activity[-1]

t.test(test4data.Activity, alternative="greater")

###conduct analysis for 4th day follow up for cognitive scores

test4data.Cognitive=0

for (i in 2:dim(MatchIDs.5.20.Cognitive)[1]){
  
  if (MatchIDs.5.20.Cognitive[i,1]!=MatchIDs.5.20.Cognitive[i-1,1]){
    
    studydiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Cognitive[i,1] & NewMaster[,"DaysId"]==4, "ST.AM.Applied.Cogn.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Cognitive[i,1] & NewMaster[,"DaysId"]==4, "Baseline Average.Cognitive"]
    
    controldiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Cognitive[i,2] & NewMaster[,"DaysId"]==4, "ST.AM.Applied.Cogn.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Cognitive[i,2] & NewMaster[,"DaysId"]==4, "Baseline Average.Cognitive"]
    
    test4data.Cognitive=c(test4data.Cognitive, studydiff-controldiff)
    
  }
  
}

if (MatchIDs.5.20.Cognitive[1,1]!=MatchIDs.5.20.Cognitive[2,1]){
  
  studydiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Cognitive[1,1] & NewMaster[,"DaysId"]==4, "ST.AM.Applied.Cogn.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Cognitive[1,1] & NewMaster[,"DaysId"]==4, "Baseline Average.Cognitive"]
  
  controldiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Cognitive[1,2] & NewMaster[,"DaysId"]==4, "ST.AM.Applied.Cogn.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Cognitive[1,2] & NewMaster[,"DaysId"]==4, "Baseline Average.Cognitive"]
  
  test4data.Cognitive=c(test4data.Cognitive, studydiff-controldiff)
  
  
}

test4data.Cognitive=test4data.Cognitive[-1]

t.test(test4data.Cognitive, alternative="greater")

###############################FIFTH FOLLOW UP###############################


###conduct analysis for 5th day follow up for mobility scores

test5data.Mobility=0

for (i in 2:dim(MatchIDs.5.20.Mobility)[1]){
  
  if (MatchIDs.5.20.Mobility[i,1]!=MatchIDs.5.20.Mobility[i-1,1]){
    
    studydiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Mobility[i,1] & NewMaster[,"DaysId"]==5, "PT.AM.PAC.Basic.Mobility.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Mobility[i,1] & NewMaster[,"DaysId"]==1, "Baseline Average.Mobility"]
    
    controldiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Mobility[i,2] & NewMaster[,"DaysId"]==5, "PT.AM.PAC.Basic.Mobility.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Mobility[i,2] & NewMaster[,"DaysId"]==1, "Baseline Average.Mobility"]
    
    test5data.Mobility=c(test5data.Mobility, studydiff-controldiff)
    
  }
  
}

if (MatchIDs.5.20.Mobility[1,1]!=MatchIDs.5.20.Mobility[2,1]){
  
  studydiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Mobility[i,1] & NewMaster[,"DaysId"]==5, "PT.AM.PAC.Basic.Mobility.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Mobility[i,1] & NewMaster[,"DaysId"]==1, "Baseline Average.Mobility"]
  
  controldiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Mobility[i,2] & NewMaster[,"DaysId"]==5, "PT.AM.PAC.Basic.Mobility.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Mobility[i,2] & NewMaster[,"DaysId"]==5, "Baseline Average.Mobility"]
  
  test5data.Mobility=c(test5data.Mobility, studydiff-controldiff)
  
}

test5data.Mobility=test5data.Mobility[-1]

t.test(test5data.Mobility, alternative="greater")

###conduct analysis for 5th day follow up for activity scores

test5data.Activity=0

for (i in 2:dim(MatchIDs.5.20.Activity)[1]){
  
  if (MatchIDs.5.20.Activity[i,1]!=MatchIDs.5.20.Activity[i-1,1]){
    
    studydiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Activity[i,1] & NewMaster[,"DaysId"]==5, "OT.AM.Daily.Activity.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Activity[i,1] & NewMaster[,"DaysId"]==1, "Baseline Average.Activity"]
    
    controldiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Activity[i,2] & NewMaster[,"DaysId"]==5, "OT.AM.Daily.Activity.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Activity[i,2] & NewMaster[,"DaysId"]==1, "Baseline Average.Activity"]
    
    test5data.Activity=c(test5data.Activity, studydiff-controldiff)
    
  }
  
}

if (MatchIDs.5.20.Activity[1,1]!=MatchIDs.5.20.Activity[2,1]){
  
  studydiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Activity[i,1] & NewMaster[,"DaysId"]==5, "OT.AM.Daily.Activity.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Activity[i,1] & NewMaster[,"DaysId"]==1, "Baseline Average.Activity"]
  
  controldiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Activity[i,2] & NewMaster[,"DaysId"]==5, "OT.AM.Daily.Activity.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Activity[i,2] & NewMaster[,"DaysId"]==1, "Baseline Average.Activity"]
  
  test5data.Activity=c(test5data.Activity, studydiff-controldiff)
  
}

test5data.Activity=test5data.Activity[-1]

t.test(test5data.Activity, alternative="greater")

###conduct analysis for 5th day follow up for cognitive scores

test5data.Cognitive=0

for (i in 2:dim(MatchIDs.5.20.Cognitive)[1]){
  
  if (MatchIDs.5.20.Cognitive[i,1]!=MatchIDs.5.20.Cognitive[i-1,1]){
    
    studydiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Cognitive[i,1] & NewMaster[,"DaysId"]==5, "ST.AM.Applied.Cogn.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Cognitive[i,1] & NewMaster[,"DaysId"]==1, "Baseline Average.Cognitive"]
    
    controldiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Cognitive[i,2] & NewMaster[,"DaysId"]==5, "ST.AM.Applied.Cogn.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Cognitive[i,2] & NewMaster[,"DaysId"]==1, "Baseline Average.Cognitive"]
    
    test5data.Cognitive=c(test5data.Cognitive, studydiff-controldiff)
    
  }
  
}

if (MatchIDs.5.20.Cognitive[1,1]!=MatchIDs.5.20.Cognitive[2,1]){
  
  studydiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Cognitive[1,1] & NewMaster[,"DaysId"]==5, "ST.AM.Applied.Cogn.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Cognitive[1,1] & NewMaster[,"DaysId"]==1, "Baseline Average.Cognitive"]
  
  controldiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Cognitive[1,2] & NewMaster[,"DaysId"]==5, "ST.AM.Applied.Cogn.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Cognitive[1,2] & NewMaster[,"DaysId"]==1, "Baseline Average.Cognitive"]
  
  test5data.Cognitive=c(test5data.Cognitive, studydiff-controldiff)
  
  
}

test5data.Cognitive=test5data.Cognitive[-1]

t.test(test5data.Cognitive, alternative="greater")

###############################SIXTH FOLLOW UP###############################

###conduct analysis for 6th day follow up for mobility scores

test6data.Mobility=0

for (i in 2:dim(MatchIDs.5.20.Mobility)[1]){
  
  if (MatchIDs.5.20.Mobility[i,1]!=MatchIDs.5.20.Mobility[i-1,1]){
    
    studydiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Mobility[i,1] & NewMaster[,"DaysId"]==6, "PT.AM.PAC.Basic.Mobility.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Mobility[i,1] & NewMaster[,"DaysId"]==1, "Baseline Average.Mobility"]
    
    controldiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Mobility[i,2] & NewMaster[,"DaysId"]==6, "PT.AM.PAC.Basic.Mobility.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Mobility[i,2] & NewMaster[,"DaysId"]==1, "Baseline Average.Mobility"]
    
    test6data.Mobility=c(test6data.Mobility, studydiff-controldiff)
    
  }
  
}

if (MatchIDs.5.20.Mobility[1,1]!=MatchIDs.5.20.Mobility[2,1]){
  
  studydiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Mobility[i,1] & NewMaster[,"DaysId"]==6, "PT.AM.PAC.Basic.Mobility.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Mobility[i,1] & NewMaster[,"DaysId"]==1, "Baseline Average.Mobility"]
  
  controldiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Mobility[i,2] & NewMaster[,"DaysId"]==6, "PT.AM.PAC.Basic.Mobility.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Mobility[i,2] & NewMaster[,"DaysId"]==1, "Baseline Average.Mobility"]
  
  test6data.Mobility=c(test6data.Mobility, studydiff-controldiff)
  
}

test6data.Mobility=test6data.Mobility[-1]

t.test(test6data.Mobility, alternative="greater")

###conduct analysis for 6th day follow up for activity scores

test6data.Activity=0

for (i in 2:dim(MatchIDs.5.20.Activity)[1]){
  
  if (MatchIDs.5.20.Activity[i,1]!=MatchIDs.5.20.Activity[i-1,1]){
    
    studydiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Activity[i,1] & NewMaster[,"DaysId"]==6, "OT.AM.Daily.Activity.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Activity[i,1] & NewMaster[,"DaysId"]==1, "Baseline Average.Activity"]
    
    controldiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Activity[i,2] & NewMaster[,"DaysId"]==6, "OT.AM.Daily.Activity.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Activity[i,2] & NewMaster[,"DaysId"]==6, "Baseline Average.Activity"]
    
    test6data.Activity=c(test6data.Activity, studydiff-controldiff)
    
  }
  
}

if (MatchIDs.5.20.Activity[1,1]!=MatchIDs.5.20.Activity[2,1]){
  
  studydiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Activity[i,1] & NewMaster[,"DaysId"]==6, "OT.AM.Daily.Activity.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Activity[i,1] & NewMaster[,"DaysId"]==1, "Baseline Average.Activity"]
  
  controldiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Activity[i,2] & NewMaster[,"DaysId"]==6, "OT.AM.Daily.Activity.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Activity[i,2] & NewMaster[,"DaysId"]==1, "Baseline Average.Activity"]
  
  test6data.Activity=c(test6data.Activity, studydiff-controldiff)
  
}

test6data.Activity=test6data.Activity[-1]

t.test(test6data.Activity, alternative="greater")

###conduct analysis for 6th day follow up for cognitive scores

test6data.Cognitive=0

for (i in 2:dim(MatchIDs.5.20.Cognitive)[1]){
  
  if (MatchIDs.5.20.Cognitive[i,1]!=MatchIDs.5.20.Cognitive[i-1,1]){
    
    studydiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Cognitive[i,1] & NewMaster[,"DaysId"]==6, "ST.AM.Applied.Cogn.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Cognitive[i,1] & NewMaster[,"DaysId"]==1, "Baseline Average.Cognitive"]
    
    controldiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Cognitive[i,2] & NewMaster[,"DaysId"]==6, "ST.AM.Applied.Cogn.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Cognitive[i,2] & NewMaster[,"DaysId"]==1, "Baseline Average.Cognitive"]
    
    test6data.Cognitive=c(test6data.Cognitive, studydiff-controldiff)
    
  }
  
}

if (MatchIDs.5.20.Cognitive[1,1]!=MatchIDs.5.20.Cognitive[2,1]){
  
  studydiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Cognitive[1,1] & NewMaster[,"DaysId"]==6, "ST.AM.Applied.Cogn.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Cognitive[1,1] & NewMaster[,"DaysId"]==1, "Baseline Average.Cognitive"]
  
  controldiff=NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Cognitive[1,2] & NewMaster[,"DaysId"]==6, "ST.AM.Applied.Cogn.Score"] - NewMaster[NewMaster[,"ID"]==MatchIDs.5.20.Cognitive[1,2] & NewMaster[,"DaysId"]==1, "Baseline Average.Cognitive"]
  
  test6data.Cognitive=c(test6data.Cognitive, studydiff-controldiff)
  
  
}

test6data.Cognitive=test6data.Cognitive[-1]

t.test(test6data.Cognitive, alternative="greater")



##########################MULTIVARIATE ANALYSIS############################


###############################CORRELATIONS###############################

###correlations for the three functional outcomes

cor(NewMaster[NewMaster[,"DaysId"]==1,c("Baseline Average.Mobility", "Baseline Average.Activity", "Baseline Average.Cognitive")])

cor(NewMaster[NewMaster[,"DaysId"]==1,ScoreVarName], use = "pairwise.complete.obs")

cor(NewMaster[NewMaster[,"DaysId"]==2,ScoreVarName], use = "pairwise.complete.obs")

cor(NewMaster[NewMaster[,"DaysId"]==3,ScoreVarName], use = "pairwise.complete.obs")

cor(NewMaster[NewMaster[,"DaysId"]==4,ScoreVarName], use = "pairwise.complete.obs")

cor(NewMaster[NewMaster[,"DaysId"]==5,ScoreVarName], use = "pairwise.complete.obs")

cor(NewMaster[NewMaster[,"DaysId"]==6,ScoreVarName], use = "pairwise.complete.obs")

cor(NewMaster[NewMaster[,"DaysId"]==7,ScoreVarName], use = "pairwise.complete.obs")

################################3D SCATTERPLOT#############################

install.packages("rgl")

library("rgl")

color=c("green","red")

plot3d(x = NewMaster[NewMaster[,"DaysId"]==3,"Baseline Average.Mobility"], 
          y = NewMaster[NewMaster[,"DaysId"]==3,"Baseline Average.Activity"], 
          z = NewMaster[NewMaster[,"DaysId"]==3,"Baseline Average.Cognitive"],
       xlab="Mobility", ylab="Activity", zlab="Cognitive",
       col=color[NewMaster[NewMaster[,"DaysId"]==3,"GroupNum"]])
          



