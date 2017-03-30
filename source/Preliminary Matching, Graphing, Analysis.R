#################################Program Description################################
#Name: Preliminary Matching, Graphing, and Analysis                                #
#Author: Traymon Beavers                                                           #
#Date Created: 3/10/2017                                                           #
#Purpose: To match the data based on certain characteristics and then plot and     #
#         perform analysis on matched data (one factor)                            #
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


#######################PLOT AND ANALYZE THE DATA WITH ONE FACTOR MATCHING#####################


#plot for Study Group
plot(-100,-100, xlim=c(0,365), ylim=c(0,100), ylab="Mobility Score", xlab="Days Since Stroke",
     main="Mobility Score by Days Since Stroke for Study Group")

for (i in StudyGroupIDs){
  
  if (i==24 | i==46 | i==124){
    
    points(Master[(Master[,"ID"]==i & Master[,"Group"]=="Study Group"),"Days_afterOnset"],
           Master[(Master[,"ID"]==i & Master[,"Group"]=="Study Group"),"PT.AM.PAC.Basic.Mobility.Score"], 
           pch=19, col="green")
    
    points(Master[(Master[,"ID"]==i & Master[,"Group"]=="Control Group"),"Days_afterOnset"],
           Master[(Master[,"ID"]==i & Master[,"Group"]=="Control Group"),"PT.AM.PAC.Basic.Mobility.Score"], 
           pch=19, col="red")
    
    
    points(Master[((Master[,"ID"]==i & Master[,"Group"]=="No")), "Days_afterOnset"],
           Master[((Master[,"ID"]==i & Master[,"Group"]=="No")), "PT.AM.PAC.Basic.Mobility.Score"],
           pch=19,col="blue")
      
    lines(Master[(Master[,"ID"]==i),"Days_afterOnset"],
          Master[(Master[,"ID"]==i),"PT.AM.PAC.Basic.Mobility.Score"])
     
  }else{
  
    points(Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==i),"Days_afterOnset"],
           Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==i),"PT.AM.PAC.Basic.Mobility.Score"], pch=19, col="green")
    
    lines(Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==i),"Days_afterOnset"],
          Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==i),"PT.AM.PAC.Basic.Mobility.Score"])
    
    points(Sort.NewStudyGroup[((Sort.NewStudyGroup[,"ID"]==i & Sort.NewStudyGroup[,"Group"]=="No")), "Days_afterOnset"],
           Sort.NewStudyGroup[((Sort.NewStudyGroup[,"ID"]==i & Sort.NewStudyGroup[,"Group"]=="No")), "PT.AM.PAC.Basic.Mobility.Score"],
           pch=19,col="blue")
  
  }
  
}


####################################

#plot for Control Group
plot(-100,-100, xlim=c(0,365), ylim=c(0,100), ylab="Mobility Score", xlab="Days Since Stroke",
     main="Mobility Score by Days Since Stroke for Control Group")

for (i in ControlGroupIDs){
  
  
  if (i==24 | i==46 | i==124){
    
    points(Master[(Master[,"ID"]==i & Master[,"Group"]=="Study Group"),"Days_afterOnset"],
           Master[(Master[,"ID"]==i & Master[,"Group"]=="Study Group"),"PT.AM.PAC.Basic.Mobility.Score"], 
           pch=19, col="green")
    
    points(Master[(Master[,"ID"]==i & Master[,"Group"]=="Control Group"),"Days_afterOnset"],
           Master[(Master[,"ID"]==i & Master[,"Group"]=="Control Group"),"PT.AM.PAC.Basic.Mobility.Score"], 
           pch=19, col="red")
    
    
    points(Master[((Master[,"ID"]==i & Master[,"Group"]=="No")), "Days_afterOnset"],
           Master[((Master[,"ID"]==i & Master[,"Group"]=="No")), "PT.AM.PAC.Basic.Mobility.Score"],
           pch=19,col="blue")
    
    lines(Master[(Master[,"ID"]==i),"Days_afterOnset"],
          Master[(Master[,"ID"]==i),"PT.AM.PAC.Basic.Mobility.Score"])
    
  }else{
  
  points(Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==i),"Days_afterOnset"],
         Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==i),"PT.AM.PAC.Basic.Mobility.Score"], pch=19, col="red")
  
  lines(Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==i),"Days_afterOnset"],
        Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==i),"PT.AM.PAC.Basic.Mobility.Score"])
  
  points(Sort.NewControlGroup[((Sort.NewControlGroup[,"ID"]==i & Sort.NewControlGroup[,"Group"]=="No")), "Days_afterOnset"],
         Sort.NewControlGroup[((Sort.NewControlGroup[,"ID"]==i & Sort.NewControlGroup[,"Group"]=="No")), "PT.AM.PAC.Basic.Mobility.Score"],
         pch=19,col="blue")
  
  }

}

####################################Plot data based on gender#####################################


#Create vector for genders

Gender=c("Male", "Female")

for (j in 1:2){

  #plot for Study Group and Gender
  plot(-100,-100, xlim=c(0,365), ylim=c(0,100), ylab="Mobility Score", xlab="Days Since Stroke",
       main=paste("Mobility Score by Days Since Stroke for Study Group;", Gender[j]))
  
  for (i in StudyGroupIDs){
    
    if (i==24 | i==46 | i==124){
      
      points(Master[(Master[,"ID"]==i & Master[,"Group"]=="Study Group" & Master[,"Gender"]==Gender[j]),"Days_afterOnset"],
             Master[(Master[,"ID"]==i & Master[,"Group"]=="Study Group" & Master[,"Gender"]==Gender[j]),"PT.AM.PAC.Basic.Mobility.Score"], 
             pch=19, col="green")
      
      points(Master[(Master[,"ID"]==i & Master[,"Group"]=="Control Group" & Master[,"Gender"]==Gender[j]),"Days_afterOnset"],
             Master[(Master[,"ID"]==i & Master[,"Group"]=="Control Group" & Master[,"Gender"]==Gender[j]),"PT.AM.PAC.Basic.Mobility.Score"], 
             pch=19, col="red")
      
      
      points(Master[(Master[,"ID"]==i & Master[,"Group"]=="No" & Master[,"Gender"]==Gender[j]), "Days_afterOnset"],
             Master[(Master[,"ID"]==i & Master[,"Group"]=="No" & Master[,"Gender"]==Gender[j]), "PT.AM.PAC.Basic.Mobility.Score"],
             pch=19,col="blue")
      
      lines(Master[(Master[,"ID"]==i & Master[,"Gender"]==Gender[j]),"Days_afterOnset"],
            Master[(Master[,"ID"]==i & Master[,"Gender"]==Gender[j]),"PT.AM.PAC.Basic.Mobility.Score"])
      
    }else{
      
      points(Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==i & Sort.NewStudyGroup[,"Gender"]==Gender[j]),"Days_afterOnset"],
             Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==i & Sort.NewStudyGroup[,"Gender"]==Gender[j]),"PT.AM.PAC.Basic.Mobility.Score"], pch=19, col="green")
      
      lines(Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==i & Sort.NewStudyGroup[,"Gender"]==Gender[j]),"Days_afterOnset"],
            Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==i & Sort.NewStudyGroup[,"Gender"]==Gender[j]),"PT.AM.PAC.Basic.Mobility.Score"])
      
      points(Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==i & Sort.NewStudyGroup[,"Group"]=="No" & Sort.NewStudyGroup[,"Gender"]==Gender[j]), "Days_afterOnset"],
             Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==i & Sort.NewStudyGroup[,"Group"]=="No" & Sort.NewStudyGroup[,"Gender"]==Gender[j]), "PT.AM.PAC.Basic.Mobility.Score"],
             pch=19,col="blue")
      
    }
    
  }
  
  
  #plot for Control Group and Gender
  plot(-100,-100, xlim=c(0,365), ylim=c(0,100), ylab="Mobility Score", xlab="Days Since Stroke",
       main=paste("Mobility Score by Days Since Stroke for Control Group;", Gender[j]))
  
  for (i in ControlGroupIDs){
    
    if (i==24 | i==46 | i==124){
      
      points(Master[(Master[,"ID"]==i & Master[,"Group"]=="Study Group" & Master[,"Gender"]==Gender[j]),"Days_afterOnset"],
             Master[(Master[,"ID"]==i & Master[,"Group"]=="Study Group" & Master[,"Gender"]==Gender[j]),"PT.AM.PAC.Basic.Mobility.Score"], 
             pch=19, col="green")
      
      points(Master[(Master[,"ID"]==i & Master[,"Group"]=="Control Group" & Master[,"Gender"]==Gender[j]),"Days_afterOnset"],
             Master[(Master[,"ID"]==i & Master[,"Group"]=="Control Group" & Master[,"Gender"]==Gender[j]),"PT.AM.PAC.Basic.Mobility.Score"], 
             pch=19, col="red")
      
      
      points(Master[(Master[,"ID"]==i & Master[,"Group"]=="No" & Master[,"Gender"]==Gender[j]), "Days_afterOnset"],
             Master[(Master[,"ID"]==i & Master[,"Group"]=="No" & Master[,"Gender"]==Gender[j]), "PT.AM.PAC.Basic.Mobility.Score"],
             pch=19,col="blue")
      
      lines(Master[(Master[,"ID"]==i & Master[,"Gender"]==Gender[j]),"Days_afterOnset"],
            Master[(Master[,"ID"]==i & Master[,"Gender"]==Gender[j]),"PT.AM.PAC.Basic.Mobility.Score"])
      
    }else{
      
      points(Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==i & Sort.NewControlGroup[,"Gender"]==Gender[j]),"Days_afterOnset"],
             Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==i & Sort.NewControlGroup[,"Gender"]==Gender[j]),"PT.AM.PAC.Basic.Mobility.Score"], pch=19, col="red")
      
      lines(Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==i & Sort.NewControlGroup[,"Gender"]==Gender[j]),"Days_afterOnset"],
            Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==i & Sort.NewControlGroup[,"Gender"]==Gender[j]),"PT.AM.PAC.Basic.Mobility.Score"])
      
      points(Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==i & Sort.NewControlGroup[,"Group"]=="No" & Sort.NewControlGroup[,"Gender"]==Gender[j]), "Days_afterOnset"],
             Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==i & Sort.NewControlGroup[,"Group"]=="No" & Sort.NewControlGroup[,"Gender"]==Gender[j]), "PT.AM.PAC.Basic.Mobility.Score"],
             pch=19,col="blue")
      
    }
    
  }

}  
  

###################################Plot data based on Race######################################


#create vectors for races numbers and corresponding race names
RaceNum=1:8
RaceName=c("Native American or Alaskan Native", "Asian", "Black", "Hawaiian or Pacific Islander", 
          "White", "Other", "Missing", "Hispanic")

for (j in 1:8){

  #plot for Study Group and Race
  plot(-100,-100, xlim=c(0,365), ylim=c(0,100), ylab="Mobility Score", xlab="Days Since Stroke",
       main=paste("Mobility Score by Days Since Stroke for Study Group;", RaceName[j]))
  
  for (i in StudyGroupIDs){
    
    if (i==24 | i==46 | i==124){
      
      points(Master[(Master[,"ID"]==i & Master[,"Group"]=="Study Group" & Master[,"Race"]==RaceNum[j]),"Days_afterOnset"],
             Master[(Master[,"ID"]==i & Master[,"Group"]=="Study Group" & Master[,"Race"]==RaceNum[j]),"PT.AM.PAC.Basic.Mobility.Score"], 
             pch=19, col="green")
      
      points(Master[(Master[,"ID"]==i & Master[,"Group"]=="Control Group" & Master[,"Race"]==RaceNum[j]),"Days_afterOnset"],
             Master[(Master[,"ID"]==i & Master[,"Group"]=="Control Group" & Master[,"Race"]==RaceNum[j]),"PT.AM.PAC.Basic.Mobility.Score"], 
             pch=19, col="red")
      
      
      points(Master[(Master[,"ID"]==i & Master[,"Group"]=="No" & Master[,"Race"]==RaceNum[j]), "Days_afterOnset"],
             Master[(Master[,"ID"]==i & Master[,"Group"]=="No" & Master[,"Race"]==RaceNum[j]), "PT.AM.PAC.Basic.Mobility.Score"],
             pch=19,col="blue")
      
      lines(Master[(Master[,"ID"]==i & Master[,"Race"]==RaceNum[j]),"Days_afterOnset"],
            Master[(Master[,"ID"]==i & Master[,"Race"]==RaceNum[j]),"PT.AM.PAC.Basic.Mobility.Score"])
      
    }else{
      
      points(Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==i & Sort.NewStudyGroup[,"Race"]==RaceNum[j]),"Days_afterOnset"],
             Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==i & Sort.NewStudyGroup[,"Race"]==RaceNum[j]),"PT.AM.PAC.Basic.Mobility.Score"], pch=19, col="green")
      
      lines(Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==i & Sort.NewStudyGroup[,"Race"]==RaceNum[j]),"Days_afterOnset"],
            Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==i & Sort.NewStudyGroup[,"Race"]==RaceNum[j]),"PT.AM.PAC.Basic.Mobility.Score"])
      
      points(Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==i & Sort.NewStudyGroup[,"Group"]=="No" & Sort.NewStudyGroup[,"Race"]==RaceNum[j]), "Days_afterOnset"],
             Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==i & Sort.NewStudyGroup[,"Group"]=="No" & Sort.NewStudyGroup[,"Race"]==RaceNum[j]), "PT.AM.PAC.Basic.Mobility.Score"],
             pch=19,col="blue")
      
    }
    
  }
  
  
#########################################
  
  #plot for Control Group and Race
  plot(-100,-100, xlim=c(0,365), ylim=c(0,100), ylab="Mobility Score", xlab="Days Since Stroke",
       main=paste("Mobility Score by Days Since Stroke for Control Group", RaceName[j]))
  
  for (i in ControlGroupIDs){
    
    if (i==24 | i==46 | i==124){
      
      points(Master[(Master[,"ID"]==i & Master[,"Group"]=="Study Group" & Master[,"Race"]==RaceNum[j]),"Days_afterOnset"],
             Master[(Master[,"ID"]==i & Master[,"Group"]=="Study Group" & Master[,"Race"]==RaceNum[j]),"PT.AM.PAC.Basic.Mobility.Score"], 
             pch=19, col="green")
      
      points(Master[(Master[,"ID"]==i & Master[,"Group"]=="Control Group" & Master[,"Race"]==RaceNum[j]),"Days_afterOnset"],
             Master[(Master[,"ID"]==i & Master[,"Group"]=="Control Group" & Master[,"Race"]==RaceNum[j]),"PT.AM.PAC.Basic.Mobility.Score"], 
             pch=19, col="red")
      
      
      points(Master[(Master[,"ID"]==i & Master[,"Group"]=="No" & Master[,"Race"]==RaceNum[j]), "Days_afterOnset"],
             Master[(Master[,"ID"]==i & Master[,"Group"]=="No" & Master[,"Race"]==RaceNum[j]), "PT.AM.PAC.Basic.Mobility.Score"],
             pch=19,col="blue")
      
      lines(Master[(Master[,"ID"]==i & Master[,"Race"]==RaceNum[j]),"Days_afterOnset"],
            Master[(Master[,"ID"]==i & Master[,"Race"]==RaceNum[j]),"PT.AM.PAC.Basic.Mobility.Score"])
      
    }else{
      
      points(Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==i & Sort.NewControlGroup[,"Race"]==RaceNum[j]),"Days_afterOnset"],
             Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==i & Sort.NewControlGroup[,"Race"]==RaceNum[j]),"PT.AM.PAC.Basic.Mobility.Score"], pch=19, col="red")
      
      lines(Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==i & Sort.NewControlGroup[,"Race"]==RaceNum[j]),"Days_afterOnset"],
            Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==i & Sort.NewControlGroup[,"Race"]==RaceNum[j]),"PT.AM.PAC.Basic.Mobility.Score"])
      
      points(Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==i & Sort.NewControlGroup[,"Group"]=="No" & Sort.NewControlGroup[,"Race"]==RaceNum[j]), "Days_afterOnset"],
             Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==i & Sort.NewControlGroup[,"Group"]=="No" & Sort.NewControlGroup[,"Race"]==RaceNum[j]), "PT.AM.PAC.Basic.Mobility.Score"],
             pch=19,col="blue")
      
    }
    
  }
  
} 



###################################Plot data based on Age Group#######################################


#create vectors for age group limits and corresponding age group names

AgeLimits=c(47,60,70,80,59,69,79,95)
AgeLimitListName=c("47 to 59", "60 to 69", "70 to 79", "80 to 95")

for (j in 1:4){
  
  #plot for Study Group and Race
  plot(-100,-100, xlim=c(0,365), ylim=c(0,100), ylab="Mobility Score", xlab="Days Since Stroke",
       main=paste("Mobility Score by Days Since Stroke for Study Group;", AgeLimitListName[j]))
  
  for (i in StudyGroupIDs){
    
    if (i==24 | i==46 | i==124){
      
      points(Master[(Master[,"ID"]==i & Master[,"Group"]=="Study Group" & Master[,"Age"]>=AgeLimits[j] & Master[,"Age"]<=AgeLimits[j+4]),"Days_afterOnset"],
             Master[(Master[,"ID"]==i & Master[,"Group"]=="Study Group" & Master[,"Age"]>=AgeLimits[j] & Master[,"Age"]<=AgeLimits[j+4]),"PT.AM.PAC.Basic.Mobility.Score"], 
             pch=19, col="green")
      
      points(Master[(Master[,"ID"]==i & Master[,"Group"]=="Control Group" & Master[,"Age"]>=AgeLimits[j] & Master[,"Age"]<=AgeLimits[j+4]),"Days_afterOnset"],
             Master[(Master[,"ID"]==i & Master[,"Group"]=="Control Group" & Master[,"Age"]>=AgeLimits[j] & Master[,"Age"]<=AgeLimits[j+4]),"PT.AM.PAC.Basic.Mobility.Score"], 
             pch=19, col="red")
      
      
      points(Master[(Master[,"ID"]==i & Master[,"Group"]=="No" & Master[,"Age"]>=AgeLimits[j] & Master[,"Age"]<=AgeLimits[j+4]), "Days_afterOnset"],
             Master[(Master[,"ID"]==i & Master[,"Group"]=="No" & Master[,"Age"]>=AgeLimits[j] & Master[,"Age"]<=AgeLimits[j+4]), "PT.AM.PAC.Basic.Mobility.Score"],
             pch=19,col="blue")
      
      lines(Master[(Master[,"ID"]==i & Master[,"Age"]>=AgeLimits[j] & Master[,"Age"]<=AgeLimits[j+4]),"Days_afterOnset"],
            Master[(Master[,"ID"]==i & Master[,"Age"]>=AgeLimits[j] & Master[,"Age"]<=AgeLimits[j+4]),"PT.AM.PAC.Basic.Mobility.Score"])
      
    }else{
      
      points(Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==i & Sort.NewStudyGroup[,"Age"]>=AgeLimits[j] & Sort.NewStudyGroup[,"Age"]<=AgeLimits[j+4]),"Days_afterOnset"],
             Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==i & Sort.NewStudyGroup[,"Age"]>=AgeLimits[j] & Sort.NewStudyGroup[,"Age"]<=AgeLimits[j+4]),"PT.AM.PAC.Basic.Mobility.Score"], pch=19, col="green")
      
      lines(Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==i & Sort.NewStudyGroup[,"Age"]>=AgeLimits[j] & Sort.NewStudyGroup[,"Age"]<=AgeLimits[j+4]),"Days_afterOnset"],
            Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==i & Sort.NewStudyGroup[,"Age"]>=AgeLimits[j] & Sort.NewStudyGroup[,"Age"]<=AgeLimits[j+4]),"PT.AM.PAC.Basic.Mobility.Score"])
      
      points(Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==i & Sort.NewStudyGroup[,"Group"]=="No" & Sort.NewStudyGroup[,"Age"]>=AgeLimits[j] & Sort.NewStudyGroup[,"Age"]<=AgeLimits[j+4]), "Days_afterOnset"],
             Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==i & Sort.NewStudyGroup[,"Group"]=="No" & Sort.NewStudyGroup[,"Age"]>=AgeLimits[j] & Sort.NewStudyGroup[,"Age"]<=AgeLimits[j+4]), "PT.AM.PAC.Basic.Mobility.Score"],
             pch=19,col="blue")
      
    }
    
  }

  
  #plot for Control Group and Race
  plot(-100,-100, xlim=c(0,365), ylim=c(0,100), ylab="Mobility Score", xlab="Days Since Stroke",
       main=paste("Mobility Score by Days Since Stroke for Control Group;", AgeLimitListName[j]))
  
  for (i in ControlGroupIDs){
    
    if (i==24 | i==46 | i==124){
      
      points(Master[(Master[,"ID"]==i & Master[,"Group"]=="Study Group" & Master[,"Age"]>=AgeLimits[j] & Master[,"Age"]<=AgeLimits[j+4]),"Days_afterOnset"],
             Master[(Master[,"ID"]==i & Master[,"Group"]=="Study Group" & Master[,"Age"]>=AgeLimits[j] & Master[,"Age"]<=AgeLimits[j+4]),"PT.AM.PAC.Basic.Mobility.Score"], 
             pch=19, col="green")
      
      points(Master[(Master[,"ID"]==i & Master[,"Group"]=="Control Group" & Master[,"Age"]>=AgeLimits[j] & Master[,"Age"]<=AgeLimits[j+4]),"Days_afterOnset"],
             Master[(Master[,"ID"]==i & Master[,"Group"]=="Control Group" & Master[,"Age"]>=AgeLimits[j] & Master[,"Age"]<=AgeLimits[j+4]),"PT.AM.PAC.Basic.Mobility.Score"], 
             pch=19, col="red")
      
      
      points(Master[(Master[,"ID"]==i & Master[,"Group"]=="No" & Master[,"Age"]>=AgeLimits[j] & Master[,"Age"]<=AgeLimits[j+4]), "Days_afterOnset"],
             Master[(Master[,"ID"]==i & Master[,"Group"]=="No" & Master[,"Age"]>=AgeLimits[j] & Master[,"Age"]<=AgeLimits[j+4]), "PT.AM.PAC.Basic.Mobility.Score"],
             pch=19,col="blue")
      
      lines(Master[(Master[,"ID"]==i & Master[,"Age"]>=AgeLimits[j] & Master[,"Age"]<=AgeLimits[j+4]),"Days_afterOnset"],
            Master[(Master[,"ID"]==i & Master[,"Age"]>=AgeLimits[j] & Master[,"Age"]<=AgeLimits[j+4]),"PT.AM.PAC.Basic.Mobility.Score"])
      
    }else{
      
      points(Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==i & Sort.NewControlGroup[,"Age"]>=AgeLimits[j] & Sort.NewControlGroup[,"Age"]<=AgeLimits[j+4]),"Days_afterOnset"],
             Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==i & Sort.NewControlGroup[,"Age"]>=AgeLimits[j] & Sort.NewControlGroup[,"Age"]<=AgeLimits[j+4]),"PT.AM.PAC.Basic.Mobility.Score"], pch=19, col="red")
      
      lines(Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==i & Sort.NewControlGroup[,"Age"]>=AgeLimits[j] & Sort.NewControlGroup[,"Age"]<=AgeLimits[j+4]),"Days_afterOnset"],
            Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==i & Sort.NewControlGroup[,"Age"]>=AgeLimits[j] & Sort.NewControlGroup[,"Age"]<=AgeLimits[j+4]),"PT.AM.PAC.Basic.Mobility.Score"])
      
      points(Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==i & Sort.NewControlGroup[,"Group"]=="No" & Sort.NewControlGroup[,"Age"]>=AgeLimits[j] & Sort.NewControlGroup[,"Age"]<=AgeLimits[j+4]), "Days_afterOnset"],
             Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==i & Sort.NewControlGroup[,"Group"]=="No" & Sort.NewControlGroup[,"Age"]>=AgeLimits[j] & Sort.NewControlGroup[,"Age"]<=AgeLimits[j+4]), "PT.AM.PAC.Basic.Mobility.Score"],
             pch=19,col="blue")
      
    }
    
  }
  
} 


####################################plot the data for baseline score matching######################

###Create vector for baseline scores of patients in study group and control group

Baseline.Study=Sort.NewStudyGroup[(Sort.NewStudyGroup[,"DaysId"]==1), "PT.AM.PAC.Basic.Mobility.Score"]
Baseline.Control=Sort.NewControlGroup[(Sort.NewControlGroup[,"DaysId"]==1), "PT.AM.PAC.Basic.Mobility.Score"]


#plot for Study Group
plot(-100,-100, xlim=c(0,365), ylim=c(0,100), ylab="Mobility Score", xlab="Days Since Stroke",
     main="Mobility Score by Days Since Stroke for Study Group (Baseline Score Matching)")

for (i in StudyGroupIDs){
  
  if ((i==24 | i==46 | i==124) & Master[Master[,"DaysId"]==1 & Master[,"ID"]==i, "PT.AM.PAC.Basic.Mobility.Score"]>=(min(Baseline.Study)-1) & 
      Master[Master[,"DaysId"]==1 & Master[,"ID"]==i, "PT.AM.PAC.Basic.Mobility.Score"]<=(max(Baseline.Study)+1)){
    
    points(Master[(Master[,"ID"]==i & Master[,"Group"]=="Study Group"),"Days_afterOnset"],
           Master[(Master[,"ID"]==i & Master[,"Group"]=="Study Group"),"PT.AM.PAC.Basic.Mobility.Score"], 
           pch=19, col="green")
    
    points(Master[(Master[,"ID"]==i & Master[,"Group"]=="Control Group"),"Days_afterOnset"],
           Master[(Master[,"ID"]==i & Master[,"Group"]=="Control Group"),"PT.AM.PAC.Basic.Mobility.Score"], 
           pch=19, col="red")
    
    
    points(Master[((Master[,"ID"]==i & Master[,"Group"]=="No")), "Days_afterOnset"],
           Master[((Master[,"ID"]==i & Master[,"Group"]=="No")), "PT.AM.PAC.Basic.Mobility.Score"],
           pch=19,col="blue")
    
    lines(Master[(Master[,"ID"]==i),"Days_afterOnset"],
          Master[(Master[,"ID"]==i),"PT.AM.PAC.Basic.Mobility.Score"])
    
  }else if (Sort.NewStudyGroup[Sort.NewStudyGroup[,"ID"]==i, "PT.AM.PAC.Basic.Mobility.Score"]>=(min(Baseline.Study)-1) & 
            Sort.NewStudyGroup[Sort.NewStudyGroup[,"DaysId"]==1 & Sort.NewStudyGroup[,"ID"]==i, "PT.AM.PAC.Basic.Mobility.Score"]<=(max(Baseline.Study)+1)){
    
    points(Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==i),"Days_afterOnset"],
           Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==i),"PT.AM.PAC.Basic.Mobility.Score"], pch=19, col="green")
    
    lines(Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==i),"Days_afterOnset"],
          Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==i),"PT.AM.PAC.Basic.Mobility.Score"])
    
    points(Sort.NewStudyGroup[((Sort.NewStudyGroup[,"ID"]==i & Sort.NewStudyGroup[,"Group"]=="No")), "Days_afterOnset"],
           Sort.NewStudyGroup[((Sort.NewStudyGroup[,"ID"]==i & Sort.NewStudyGroup[,"Group"]=="No")), "PT.AM.PAC.Basic.Mobility.Score"],
           pch=19,col="blue")
    
  }
  
}


#plot for Control Group
plot(-100,-100, xlim=c(0,365), ylim=c(0,100), ylab="Mobility Score", xlab="Days Since Stroke",
     main="Mobility Score by Days Since Stroke for Control Group (Baseline Score Matching)")

for (i in ControlGroupIDs){
  
  
  if ((i==24 | i==46 | i==124) & Master[Master[,"DaysId"]==1 & Master[,"ID"]==i, "PT.AM.PAC.Basic.Mobility.Score"]>=(min(Baseline.Study)-1) & 
      Master[Master[,"DaysId"]==1 & Master[,"ID"]==i, "PT.AM.PAC.Basic.Mobility.Score"]<=(max(Baseline.Study)+1)){
    
    points(Master[(Master[,"ID"]==i & Master[,"Group"]=="Study Group"),"Days_afterOnset"],
           Master[(Master[,"ID"]==i & Master[,"Group"]=="Study Group"),"PT.AM.PAC.Basic.Mobility.Score"], 
           pch=19, col="green")
    
    points(Master[(Master[,"ID"]==i & Master[,"Group"]=="Control Group"),"Days_afterOnset"],
           Master[(Master[,"ID"]==i & Master[,"Group"]=="Control Group"),"PT.AM.PAC.Basic.Mobility.Score"], 
           pch=19, col="red")
    
    
    points(Master[((Master[,"ID"]==i & Master[,"Group"]=="No")), "Days_afterOnset"],
           Master[((Master[,"ID"]==i & Master[,"Group"]=="No")), "PT.AM.PAC.Basic.Mobility.Score"],
           pch=19,col="blue")
    
    lines(Master[(Master[,"ID"]==i),"Days_afterOnset"],
          Master[(Master[,"ID"]==i),"PT.AM.PAC.Basic.Mobility.Score"])
    
  }else if (Sort.NewControlGroup[Sort.NewControlGroup[,"DaysId"]==1 & Sort.NewControlGroup[,"ID"]==i, "PT.AM.PAC.Basic.Mobility.Score"]>=(min(Baseline.Study)-1) & 
            Sort.NewControlGroup[Sort.NewControlGroup[,"DaysId"]==1 & Sort.NewControlGroup[,"ID"]==i, "PT.AM.PAC.Basic.Mobility.Score"]<=(max(Baseline.Study)+1)){
    
    points(Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==i),"Days_afterOnset"],
           Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==i),"PT.AM.PAC.Basic.Mobility.Score"], pch=19, col="red")
    
    lines(Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==i),"Days_afterOnset"],
          Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==i),"PT.AM.PAC.Basic.Mobility.Score"])
    
    points(Sort.NewControlGroup[((Sort.NewControlGroup[,"ID"]==i & Sort.NewControlGroup[,"Group"]=="No")), "Days_afterOnset"],
           Sort.NewControlGroup[((Sort.NewControlGroup[,"ID"]==i & Sort.NewControlGroup[,"Group"]=="No")), "PT.AM.PAC.Basic.Mobility.Score"],
           pch=19,col="blue")
    
  }
  
}


#plot for Deceased Group
# plot(-100,-100, xlim=c(0,365), ylim=c(0,100), ylab="Mobility Score", xlab="Days Since Stroke",
#      main="Mobility Score by Days Since Stroke for Control Group")
# 
# for (i in DeceasedGroupIDs){
#   
#   points(DeceasedGroup[(DeceasedGroup[,"ID"]==i),"Days_afterOnset"],
#          DeceasedGroup[(DeceasedGroup[,"ID"]==i),"PT.AM.PAC.Basic.Mobility.Score"], pch=19, col="red")
#   
#   lines(DeceasedGroup[(DeceasedGroup[,"ID"]==i),"Days_afterOnset"],
#         DeceasedGroup[(DeceasedGroup[,"ID"]==i),"PT.AM.PAC.Basic.Mobility.Score"])
#   
#   points(DeceasedGroup[((DeceasedGroup[,"ID"]==i & DeceasedGroup[,"Group"]=="No")), "Days_afterOnset"],
#          DeceasedGroup[((DeceasedGroup[,"ID"]==i & DeceasedGroup[,"Group"]=="No")), "PT.AM.PAC.Basic.Mobility.Score"],
#          pch=19,col="blue")
#   
#   points(DeceasedGroup[((DeceasedGroup[,"ID"]==i & DeceasedGroup[,"PT.AM.PAC.Basic.Mobility.Score"]==0)), "Days_afterOnset"],
#          DeceasedGroup[((DeceasedGroup[,"ID"]==i & DeceasedGroup[,"PT.AM.PAC.Basic.Mobility.Score"]==0)), "PT.AM.PAC.Basic.Mobility.Score"],
#          pch=19)
#   
# }


##########################Plot age distribution sorted##########################

###found missing ages by comparing IDs, then looked them up in dataset

Study.Age=Sort.NewStudyGroup[Sort.NewStudyGroup[,"DaysId"]==3,"Age"] #ID 103 age missing, 70

Study.Age=c(Study.Age,70)

Control.Age=Sort.NewControlGroup[Sort.NewControlGroup[,"DaysId"]==7,"Age"] #ID 46 age missing, 62

Control.Age=c(Control.Age,62)

###Order the ages

Study.Age=Study.Age[(order(Study.Age))]

Control.Age=Control.Age[(order(Control.Age))]

plot(-100,-100, xlim=c(0,95), ylim=c(20,100), xlab="Index", ylab="Age", 
     main="Distribution of Ages in Groups")

points(1:90, Control.Age, pch=19, col="red")

points(seq(1,92,3), Study.Age, pch=19, col="green")

abline(h=47, col="blue")
abline(h=59, col="blue")
abline(h=60, col="purple")
abline(h=69, col="purple")
abline(h=70, col="orange")
abline(h=79, col="orange")
abline(h=80)
abline(h=95)

##########################Plot the distribution of baseline scores##################

plot(-100,-100, xlim=c(0,150), ylim=c(0,60), xlab="Index", ylab="Baseline Mobility Score", 
     main="Distribution of Baseline Mobility Score in Groups")

points(1:31, Baseline.Study, pch=19, col="green")

points(60:149, Baseline.Control, pch=19, col="red")

abline(h=min(Baseline.Study)-1)
abline(h=max(Baseline.Study)+1)


#######################TWO SAMPLE T TESTS###############################

###Two Sample T-Tests with NO MATCHING

##Prep datasets for test (Study Group)

#Get the IDs for people with specified follow up DayIDs

for (i in 4:10){

  #Count the number of people who have DayID for given i
  
  N=length(Sort.NewStudyGroup[(Sort.NewStudyGroup[,"DaysId"]==i), "DaysId"])
  
  #create vector to fill for with IDs for given i
  
  x=rep(12,N)
  
  T=1
  
  for (j in StudyGroupIDs){
    if (length(Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==j) & (Sort.NewStudyGroup[,"DaysId"]==i), "DaysId"])==1){
      x[T]=j
      T=T+1
    }
  }
  
  assign(value=x, paste("Day",i,"IDs.Study",sep=""))
  
}

###Create vectors for the number of patients with 4th, 5th, and 6th follow ups and their IDs
###for the study group, respectively

Study.lengths=c(length(Day4IDs.Study),length(Day5IDs.Study),length(Day6IDs.Study))

DayIDs.Study=rbind(Day4IDs.Study,Day5IDs.Study,Day6IDs.Study)


##Calculate the differences between baseline and follow ups (Study)

for (i in 4:6){

  x=rep(0,Study.lengths[i-3])
  
  T=1
  
  for (j in DayIDs.Study[i-3,]){
    
    x[T]=Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==j) & (Sort.NewStudyGroup[,"DaysId"]==i), "PT.AM.PAC.Basic.Mobility.Score"]-Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==j) & (Sort.NewStudyGroup[,"DaysId"]==1), "PT.AM.PAC.Basic.Mobility.Score"]
    T=T+1
    
  }

  assign(value=x, paste("Diff",i,"from.1.Study",sep="."))
  
}

##Prep datasets for test (Control Group)

#Get the IDs for people with specified follow up DayIDs

for (i in 4:10){
  
  #Count the number of people who have DayID for given i
  
  N=length(Sort.NewControlGroup[(Sort.NewControlGroup[,"DaysId"]==i), "DaysId"])
  
  #create vector to fill for with IDs for given i
  
  x=rep(12,N)
  
  T=1
  
  for (j in ControlGroupIDs){
    if (length(Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==j) & (Sort.NewControlGroup[,"DaysId"]==i), "DaysId"])==1){
      x[T]=j
      T=T+1
    }
  }
  
  assign(value=x, paste("Day",i,"IDs.Control",sep=""))
  
}


###Create vectors for the number of patients with 4th, 5th, and 6th follow ups and their IDs
###for the control group, respectively

Control.lengths=c(length(Day4IDs.Control),length(Day5IDs.Control),length(Day6IDs.Control))

DayIDs.Control=rbind(Day4IDs.Control,Day5IDs.Control,Day6IDs.Control)

##Calculate the differences between baseline and follow ups (Control)

for (i in 4:6){
  
  x=rep(0,Control.lengths[i-3])
  
  T=1
  
  for (j in DayIDs.Control[i-3,]){
    
    x[T]=Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==j) & (Sort.NewControlGroup[,"DaysId"]==i), "PT.AM.PAC.Basic.Mobility.Score"]-Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==j) & (Sort.NewControlGroup[,"DaysId"]==1), "PT.AM.PAC.Basic.Mobility.Score"]
    T=T+1
    
  }
  
  assign(value=x, paste("Diff",i,"from.1.Control",sep="."))
  
}


###T Tests

#Tests for fourth, fifth, and sixth follow up comparison

t.test(Diff.4.from.1.Study, Diff.4.from.1.Control, alternative="greater")

t.test(Diff.5.from.1.Study, Diff.5.from.1.Control, alternative="greater")

t.test(Diff.6.from.1.Study, Diff.6.from.1.Control, alternative="greater")

################################Tests after matching on GENDER###########################

##Prep datasets for test (Study Group)

#Get the IDs for people with specified follow up DayIDs

for (k in 1:2){
  
  for (i in 4:10){
    
    #Count the number of people who have DayID for given i and are given k Gender
    
    N=length(Sort.NewStudyGroup[(Sort.NewStudyGroup[,"DaysId"]==i) & (Sort.NewStudyGroup[,"Gender"]==Gender[k]), "DaysId"])
    
    #create vector to fill for with IDs for given i
    
    x=rep(0,N)
    
    T=1
    
    for (j in StudyGroupIDs){
      if (length(Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==j) & (Sort.NewStudyGroup[,"DaysId"]==i) & (Sort.NewStudyGroup[,"Gender"]==Gender[k]), "DaysId"])==1){
        x[T]=j
        T=T+1
      }
    }
    
    assign(value=x, paste(Gender[k],"Day",i,"IDs.Study",sep=""))
    
  }
  
}

###Create vectors for the number of patients with 4th, 5th, and 6th follow ups and their IDs
###for the study group, respectively (for respective genders)

MaleStudy.lengths=c(length(MaleDay4IDs.Study),length(MaleDay5IDs.Study),length(MaleDay6IDs.Study))
MaleDayIDs.Study=rbind(MaleDay4IDs.Study,MaleDay5IDs.Study,MaleDay6IDs.Study)

FemaleStudy.lengths=c(length(FemaleDay4IDs.Study),length(FemaleDay5IDs.Study),length(FemaleDay6IDs.Study))
FemaleDayIDs.Study=rbind(FemaleDay4IDs.Study,FemaleDay5IDs.Study,FemaleDay6IDs.Study)


##Calculate the differences between baseline and follow ups (Black, Study)

for (i in 4:6){
  
  x=rep(0,MaleStudy.lengths[i-3])
  
  T=1
  
  for (j in MaleDayIDs.Study[i-3,]){
    
    x[T]=Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==j) & (Sort.NewStudyGroup[,"DaysId"]==i), "PT.AM.PAC.Basic.Mobility.Score"]-Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==j) & (Sort.NewStudyGroup[,"DaysId"]==1), "PT.AM.PAC.Basic.Mobility.Score"]
    T=T+1
    
  }
  
  assign(value=x, paste("Male.Diff",i,"from.1.Study",sep="."))
  
}

##Calculate the differences between baseline and follow ups (White, Study)

for (i in 4:6){
  
  x=rep(0,FemaleStudy.lengths[i-3])
  
  T=1
  
  for (j in FemaleDayIDs.Study[i-3,]){
    
    x[T]=Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==j) & (Sort.NewStudyGroup[,"DaysId"]==i), "PT.AM.PAC.Basic.Mobility.Score"]-Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==j) & (Sort.NewStudyGroup[,"DaysId"]==1), "PT.AM.PAC.Basic.Mobility.Score"]
    T=T+1
    
  }
  
  assign(value=x, paste("Female.Diff",i,"from.1.Study",sep="."))
  
}

##Prep datasets for test (Control Group)

#Get the IDs for people with specified follow up DayIDs

for (k in 1:2){
  
  for (i in 4:10){
    
    #Count the number of people who have DayID for given i
    
    N=length(Sort.NewControlGroup[(Sort.NewControlGroup[,"DaysId"]==i) & (Sort.NewControlGroup[,"Gender"]==Gender[k]), "DaysId"])
    
    #create vector to fill for with IDs for given i
    
    x=rep(0,N)
    
    T=1
    
    for (j in ControlGroupIDs){
      if (length(Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==j) & (Sort.NewControlGroup[,"DaysId"]==i) & (Sort.NewControlGroup[,"Gender"]==Gender[k]), "DaysId"])==1){
        x[T]=j
        T=T+1
      }
    }
    
    assign(value=x, paste(Gender[k], "Day",i,"IDs.Control",sep=""))
    
  }
  
}

###Create vectors for the number of patients with 4th, 5th, and 6th follow ups and their IDs
###for the control group, respectively (for respective genders)

MaleControl.lengths=c(length(MaleDay4IDs.Control),length(MaleDay5IDs.Control),length(MaleDay6IDs.Control))
MaleDayIDs.Control=rbind(MaleDay4IDs.Control,MaleDay5IDs.Control,MaleDay6IDs.Control)

FemaleControl.lengths=c(length(FemaleDay4IDs.Control),length(FemaleDay5IDs.Control),length(FemaleDay6IDs.Control))
FemaleDayIDs.Control=rbind(FemaleDay4IDs.Control,FemaleDay5IDs.Control,FemaleDay6IDs.Control)

##Calculate the differences between baseline and follow ups (Black, Control)

for (i in 4:6){
  
  x=rep(0,MaleControl.lengths[i-3])
  
  T=1
  
  for (j in MaleDayIDs.Control[i-3,]){
    
    x[T]=Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==j) & (Sort.NewControlGroup[,"DaysId"]==i), "PT.AM.PAC.Basic.Mobility.Score"]-Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==j) & (Sort.NewControlGroup[,"DaysId"]==1), "PT.AM.PAC.Basic.Mobility.Score"]
    T=T+1
    
  }
  
  assign(value=x, paste("Male.Diff",i,"from.1.Control",sep="."))
  
}


##Calculate the differences between baseline and follow ups (White, Control)

for (i in 4:6){
  
  x=rep(0,FemaleControl.lengths[i-3])
  
  T=1
  
  for (j in FemaleDayIDs.Control[i-3,]){
    
    x[T]=Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==j) & (Sort.NewControlGroup[,"DaysId"]==i), "PT.AM.PAC.Basic.Mobility.Score"]-Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==j) & (Sort.NewControlGroup[,"DaysId"]==1), "PT.AM.PAC.Basic.Mobility.Score"]
    T=T+1
    
  }
  
  assign(value=x, paste("Female.Diff",i,"from.1.Control",sep="."))
  
}


###T Tests

#Tests for fourth, fifth, and sixth follow up comparison (Male)

t.test(Male.Diff.4.from.1.Study, Male.Diff.4.from.1.Control, alternative="greater")

t.test(Male.Diff.5.from.1.Study, Male.Diff.5.from.1.Control, alternative="greater")

t.test(Male.Diff.6.from.1.Study, Male.Diff.6.from.1.Control, alternative="greater")

#Tests for fourth, fifth, and sixth follow up comparison (Female)

t.test(Female.Diff.4.from.1.Study, Female.Diff.4.from.1.Control, alternative="greater")

t.test(Female.Diff.5.from.1.Study, Female.Diff.5.from.1.Control, alternative="greater")

t.test(Female.Diff.6.from.1.Study, Female.Diff.6.from.1.Control, alternative="greater")

################################Tests after matching on RACE###########################

##Prep datasets for test (Study Group)

#Get the IDs for people with specified follow up DayIDs

for (k in c(3,5)){

  for (i in 4:10){
    
    #Count the number of people who have DayID for given i and are given k Gender
    
    N=length(Sort.NewStudyGroup[(Sort.NewStudyGroup[,"DaysId"]==i) & (Sort.NewStudyGroup[,"Race"]==k), "DaysId"])
    
    #create vector to fill for with IDs for given i
    
    x=rep(0,N)
    
    T=1
    
    for (j in StudyGroupIDs){
      if (length(Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==j) & (Sort.NewStudyGroup[,"DaysId"]==i) & (Sort.NewStudyGroup[,"Race"]==k), "DaysId"])==1){
        x[T]=j
        T=T+1
      }
    }
    
    assign(value=x, paste(RaceName[k],"Day",i,"IDs.Study",sep=""))
    
  }
  
}

###Create vectors for the number of patients with 4th, 5th, and 6th follow ups and their IDs
###for the study group, respectively (for respective races)
  
BlackStudy.lengths=c(length(BlackDay4IDs.Study),length(BlackDay5IDs.Study),length(BlackDay6IDs.Study))
BlackDayIDs.Study=rbind(BlackDay4IDs.Study,BlackDay5IDs.Study,BlackDay6IDs.Study)

WhiteStudy.lengths=c(length(WhiteDay4IDs.Study),length(WhiteDay5IDs.Study),length(WhiteDay6IDs.Study))
WhiteDayIDs.Study=rbind(WhiteDay4IDs.Study,WhiteDay5IDs.Study,WhiteDay6IDs.Study)
  
  
##Calculate the differences between baseline and follow ups (Black, Study)

for (i in 4:6){
  
  x=rep(0,BlackStudy.lengths[i-3])
  
  T=1
  
  for (j in BlackDayIDs.Study[i-3,]){
    
    x[T]=Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==j) & (Sort.NewStudyGroup[,"DaysId"]==i), "PT.AM.PAC.Basic.Mobility.Score"]-Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==j) & (Sort.NewStudyGroup[,"DaysId"]==1), "PT.AM.PAC.Basic.Mobility.Score"]
    T=T+1
    
  }
  
  assign(value=x, paste("Black.Diff",i,"from.1.Study",sep="."))
  
}

##Calculate the differences between baseline and follow ups (White, Study)

for (i in 4:6){
  
  x=rep(0,WhiteStudy.lengths[i-3])
  
  T=1
  
  for (j in WhiteDayIDs.Study[i-3,]){
    
    x[T]=Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==j) & (Sort.NewStudyGroup[,"DaysId"]==i), "PT.AM.PAC.Basic.Mobility.Score"]-Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==j) & (Sort.NewStudyGroup[,"DaysId"]==1), "PT.AM.PAC.Basic.Mobility.Score"]
    T=T+1
    
  }
  
  assign(value=x, paste("White.Diff",i,"from.1.Study",sep="."))
  
}

##Prep datasets for test (Control Group)

#Get the IDs for people with specified follow up DayIDs

for (k in c(3,5)){
  
  for (i in 4:10){
    
    #Count the number of people who have DayID for given i
    
    N=length(Sort.NewControlGroup[(Sort.NewControlGroup[,"DaysId"]==i) & (Sort.NewControlGroup[,"Race"]==k), "DaysId"])
    
    #create vector to fill for with IDs for given i
    
    x=rep(0,N)
    
    T=1
    
    for (j in ControlGroupIDs){
      if (length(Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==j) & (Sort.NewControlGroup[,"DaysId"]==i) & (Sort.NewControlGroup[,"Race"]==k), "DaysId"])==1){
        x[T]=j
        T=T+1
      }
    }
    
    assign(value=x, paste(RaceName[k], "Day",i,"IDs.Control",sep=""))
    
  }

}

###Create vectors for the number of patients with 4th, 5th, and 6th follow ups and their IDs
###for the control group, respectively (for respective races)

BlackControl.lengths=c(length(BlackDay4IDs.Control),length(BlackDay5IDs.Control),length(BlackDay6IDs.Control))
BlackDayIDs.Control=rbind(BlackDay4IDs.Control,BlackDay5IDs.Control,BlackDay6IDs.Control)

WhiteControl.lengths=c(length(WhiteDay4IDs.Control),length(WhiteDay5IDs.Control),length(WhiteDay6IDs.Control))
WhiteDayIDs.Control=rbind(WhiteDay4IDs.Control,WhiteDay5IDs.Control,WhiteDay6IDs.Control)

##Calculate the differences between baseline and follow ups (Black, Control)

for (i in 4:6){
  
  x=rep(0,BlackControl.lengths[i-3])
  
  T=1
  
  for (j in BlackDayIDs.Control[i-3,]){
    
    x[T]=Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==j) & (Sort.NewControlGroup[,"DaysId"]==i), "PT.AM.PAC.Basic.Mobility.Score"]-Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==j) & (Sort.NewControlGroup[,"DaysId"]==1), "PT.AM.PAC.Basic.Mobility.Score"]
    T=T+1
    
  }
  
  assign(value=x, paste("Black.Diff",i,"from.1.Control",sep="."))
  
}


##Calculate the differences between baseline and follow ups (White, Control)

for (i in 4:6){
  
  x=rep(0,WhiteControl.lengths[i-3])
  
  T=1
  
  for (j in WhiteDayIDs.Control[i-3,]){
    
    x[T]=Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==j) & (Sort.NewControlGroup[,"DaysId"]==i), "PT.AM.PAC.Basic.Mobility.Score"]-Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==j) & (Sort.NewControlGroup[,"DaysId"]==1), "PT.AM.PAC.Basic.Mobility.Score"]
    T=T+1
    
  }
  
  assign(value=x, paste("White.Diff",i,"from.1.Control",sep="."))
  
}


###T Tests

#Tests for fourth, fifth, and sixth follow up comparison (Black)

t.test(Black.Diff.4.from.1.Study, Black.Diff.4.from.1.Control, alternative="greater")

t.test(Black.Diff.5.from.1.Study, Black.Diff.5.from.1.Control, alternative="greater")

t.test(Black.Diff.6.from.1.Study, Black.Diff.6.from.1.Control, alternative="greater")

#Tests for fourth, fifth, and sixth follow up comparison (White)

t.test(White.Diff.4.from.1.Study, White.Diff.4.from.1.Control, alternative="greater")

t.test(White.Diff.5.from.1.Study, White.Diff.5.from.1.Control, alternative="greater")

t.test(White.Diff.6.from.1.Study, White.Diff.6.from.1.Control, alternative="greater")

################################Tests after matching on AGE GROUP###########################

##Prep datasets for test (Study Group)


AgeLimits=c(47,60,70,80,59,69,79,95)
AgeLimitName=c("First", "Second", "Third", "Fourth")


#Get the IDs for people with specified follow up DayIDs

for (k in 1:4){
  
  for (i in 4:10){
    
    #Count the number of people who have DayID for given i and are given k Gender
    
    N=length(Sort.NewStudyGroup[(Sort.NewStudyGroup[,"DaysId"]==i) & (Sort.NewStudyGroup[,"Age"]>=AgeLimits[k]) & (Sort.NewStudyGroup[,"Age"]<=AgeLimits[k+4]), "DaysId"])
    
    #create vector to fill for with IDs for given i
    
    x=rep(0,N)
    
    T=1
    
    for (j in StudyGroupIDs){
      if (length(Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==j) & (Sort.NewStudyGroup[,"DaysId"]==i) & (Sort.NewStudyGroup[,"Age"]>=AgeLimits[k]) & (Sort.NewStudyGroup[,"Age"]<=AgeLimits[k+4]), "DaysId"])==1){
        x[T]=j
        T=T+1
      }
    }
    
    assign(value=x, paste(AgeLimitName[k],"Day",i,"IDs.Study",sep=""))
    
  }
  
}

###Create vectors for the number of patients with 4th, 5th, and 6th follow ups and their IDs
###for the study group, respectively (for respective age groups)

FirstStudy.lengths=c(length(FirstDay4IDs.Study),length(FirstDay5IDs.Study),length(FirstDay6IDs.Study))
FirstDayIDs.Study=rbind(FirstDay4IDs.Study,FirstDay5IDs.Study,FirstDay6IDs.Study)

SecondStudy.lengths=c(length(SecondDay4IDs.Study),length(SecondDay5IDs.Study),length(SecondDay6IDs.Study))
SecondDayIDs.Study=rbind(SecondDay4IDs.Study,SecondDay5IDs.Study,SecondDay6IDs.Study)

ThirdStudy.lengths=c(length(ThirdDay4IDs.Study),length(ThirdDay5IDs.Study),length(ThirdDay6IDs.Study))
ThirdDayIDs.Study=rbind(ThirdDay4IDs.Study,ThirdDay5IDs.Study,ThirdDay6IDs.Study)

FourthStudy.lengths=c(length(FourthDay4IDs.Study),length(FourthDay5IDs.Study),length(FourthDay6IDs.Study))
FourthDayIDs.Study=rbind(FourthDay4IDs.Study,FourthDay5IDs.Study,FourthDay6IDs.Study)


##Calculate the differences between baseline and follow ups (First, Study)

for (i in 4:6){
  
  x=rep(0,FirstStudy.lengths[i-3])
  
  T=1
  
  for (j in FirstDayIDs.Study[i-3,]){
    
    x[T]=Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==j) & (Sort.NewStudyGroup[,"DaysId"]==i), "PT.AM.PAC.Basic.Mobility.Score"]-Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==j) & (Sort.NewStudyGroup[,"DaysId"]==1), "PT.AM.PAC.Basic.Mobility.Score"]
    T=T+1
    
  }
  
  assign(value=x, paste("First.Diff",i,"from.1.Study",sep="."))
  
}

##Calculate the differences between baseline and follow ups (Second, Study)

for (i in 4:6){
  
  x=rep(0,SecondStudy.lengths[i-3])
  
  T=1
  
  for (j in SecondDayIDs.Study[i-3,]){
    
    x[T]=Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==j) & (Sort.NewStudyGroup[,"DaysId"]==i), "PT.AM.PAC.Basic.Mobility.Score"]-Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==j) & (Sort.NewStudyGroup[,"DaysId"]==1), "PT.AM.PAC.Basic.Mobility.Score"]
    T=T+1
    
  }
  
  assign(value=x, paste("Second.Diff",i,"from.1.Study",sep="."))
  
}

##Calculate the differences between baseline and follow ups (Third, Study)

for (i in 4:6){
  
  x=rep(0,ThirdStudy.lengths[i-3])
  
  T=1
  
  for (j in ThirdDayIDs.Study[i-3,]){
    
    x[T]=Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==j) & (Sort.NewStudyGroup[,"DaysId"]==i), "PT.AM.PAC.Basic.Mobility.Score"]-Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==j) & (Sort.NewStudyGroup[,"DaysId"]==1), "PT.AM.PAC.Basic.Mobility.Score"]
    T=T+1
    
  }
  
  assign(value=x, paste("Third.Diff",i,"from.1.Study",sep="."))
  
}

##Calculate the differences between baseline and follow ups (Fourth, Study)

for (i in 4:6){
  
  x=rep(0,FourthStudy.lengths[i-3])
  
  T=1
  
  for (j in FourthDayIDs.Study[i-3,]){
    
    x[T]=Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==j) & (Sort.NewStudyGroup[,"DaysId"]==i), "PT.AM.PAC.Basic.Mobility.Score"]-Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==j) & (Sort.NewStudyGroup[,"DaysId"]==1), "PT.AM.PAC.Basic.Mobility.Score"]
    T=T+1
    
  }
  
  assign(value=x, paste("Fourth.Diff",i,"from.1.Study",sep="."))
  
}

##Prep datasets for test (Control Group)

#Get the IDs for people with specified follow up DayIDs

for (k in 1:4){
  
  for (i in 4:10){
    
    #Count the number of people who have DayID for given i
    
    N=length(Sort.NewControlGroup[(Sort.NewControlGroup[,"DaysId"]==i) & (Sort.NewControlGroup[,"Age"]>=AgeLimits[k]) & (Sort.NewControlGroup[,"Age"]<=AgeLimits[k+4]), "DaysId"])
    
    #create vector to fill for with IDs for given i
    
    x=rep(0,N)
    
    T=1
    
    for (j in ControlGroupIDs){
      if (length(Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==j) & (Sort.NewControlGroup[,"DaysId"]==i) & (Sort.NewControlGroup[,"Age"]>=AgeLimits[k]) & (Sort.NewControlGroup[,"Age"]<=AgeLimits[k+4]), "DaysId"])==1){
        x[T]=j
        T=T+1
      }
    }
    
    assign(value=x, paste(AgeLimitName[k], "Day",i,"IDs.Control",sep=""))
    
  }
  
}

###Create vectors for the number of patients with 4th, 5th, and 6th follow ups and their IDs
###for the control group, respectively (for respective age groups)

FirstControl.lengths=c(length(FirstDay4IDs.Control),length(FirstDay5IDs.Control),length(FirstDay6IDs.Control))
FirstDayIDs.Control=rbind(FirstDay4IDs.Control,FirstDay5IDs.Control,FirstDay6IDs.Control)

SecondControl.lengths=c(length(SecondDay4IDs.Control),length(SecondDay5IDs.Control),length(SecondDay6IDs.Control))
SecondDayIDs.Control=rbind(SecondDay4IDs.Control,SecondDay5IDs.Control,SecondDay6IDs.Control)

ThirdControl.lengths=c(length(ThirdDay4IDs.Control),length(ThirdDay5IDs.Control),length(ThirdDay6IDs.Control))
ThirdDayIDs.Control=rbind(ThirdDay4IDs.Control,ThirdDay5IDs.Control,ThirdDay6IDs.Control)

FourthControl.lengths=c(length(FourthDay4IDs.Control),length(FourthDay5IDs.Control),length(FourthDay6IDs.Control))
FourthDayIDs.Control=rbind(FourthDay4IDs.Control,FourthDay5IDs.Control,FourthDay6IDs.Control)


##Calculate the differences between baseline and follow ups (First, Control)

for (i in 4:6){
  
  x=rep(0,FirstControl.lengths[i-3])
  
  T=1
  
  for (j in FirstDayIDs.Control[i-3,]){
    
    x[T]=Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==j) & (Sort.NewControlGroup[,"DaysId"]==i), "PT.AM.PAC.Basic.Mobility.Score"]-Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==j) & (Sort.NewControlGroup[,"DaysId"]==1), "PT.AM.PAC.Basic.Mobility.Score"]
    T=T+1
    
  }
  
  assign(value=x, paste("First.Diff",i,"from.1.Control",sep="."))
  
}


##Calculate the differences between baseline and follow ups (Second, Control)

for (i in 4:6){
  
  x=rep(0,SecondControl.lengths[i-3])
  
  T=1
  
  for (j in SecondDayIDs.Control[i-3,]){
    
    x[T]=Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==j) & (Sort.NewControlGroup[,"DaysId"]==i), "PT.AM.PAC.Basic.Mobility.Score"]-Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==j) & (Sort.NewControlGroup[,"DaysId"]==1), "PT.AM.PAC.Basic.Mobility.Score"]
    T=T+1
    
  }
  
  assign(value=x, paste("Second.Diff",i,"from.1.Control",sep="."))
  
}


##Calculate the differences between baseline and follow ups (Third, Control)

for (i in 4:6){
  
  x=rep(0,ThirdControl.lengths[i-3])
  
  T=1
  
  for (j in ThirdDayIDs.Control[i-3,]){
    
    x[T]=Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==j) & (Sort.NewControlGroup[,"DaysId"]==i), "PT.AM.PAC.Basic.Mobility.Score"]-Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==j) & (Sort.NewControlGroup[,"DaysId"]==1), "PT.AM.PAC.Basic.Mobility.Score"]
    T=T+1
    
  }
  
  assign(value=x, paste("Third.Diff",i,"from.1.Control",sep="."))
  
}


##Calculate the differences between baseline and follow ups (Fourth, Control)

for (i in 4:6){
  
  x=rep(0,FourthControl.lengths[i-3])
  
  T=1
  
  for (j in FourthDayIDs.Control[i-3,]){
    
    x[T]=Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==j) & (Sort.NewControlGroup[,"DaysId"]==i), "PT.AM.PAC.Basic.Mobility.Score"]-Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==j) & (Sort.NewControlGroup[,"DaysId"]==1), "PT.AM.PAC.Basic.Mobility.Score"]
    T=T+1
    
  }
  
  assign(value=x, paste("Fourth.Diff",i,"from.1.Control",sep="."))
  
}

###T Tests

#Tests for fourth, fifth, and sixth follow up comparison (First)

t.test(First.Diff.4.from.1.Study, First.Diff.4.from.1.Control, alternative="greater")

t.test(First.Diff.5.from.1.Study, First.Diff.5.from.1.Control, alternative="greater")

t.test(First.Diff.6.from.1.Study, First.Diff.6.from.1.Control, alternative="greater")

#Tests for fourth, fifth, and sixth follow up comparison (Second)

t.test(Second.Diff.4.from.1.Study, Second.Diff.4.from.1.Control, alternative="greater")

t.test(Second.Diff.5.from.1.Study, Second.Diff.5.from.1.Control, alternative="greater")

t.test(Second.Diff.6.from.1.Study, Second.Diff.6.from.1.Control, alternative="greater")

#Tests for fourth, fifth, and sixth follow up comparison (Third)

t.test(Third.Diff.4.from.1.Study, Third.Diff.4.from.1.Control, alternative="greater")

t.test(Third.Diff.5.from.1.Study, Third.Diff.5.from.1.Control, alternative="greater")

t.test(Third.Diff.6.from.1.Study, Third.Diff.6.from.1.Control, alternative="greater")

#Tests for fourth, fifth, and sixth follow up comparison (Fourth)

t.test(Fourth.Diff.4.from.1.Study, Fourth.Diff.4.from.1.Control, alternative="greater")

t.test(Fourth.Diff.5.from.1.Study, Fourth.Diff.5.from.1.Control, alternative="greater")

t.test(Fourth.Diff.6.from.1.Study, Fourth.Diff.6.from.1.Control, alternative="greater")


#########################Two Sample T-Tests with Baseline Score Matching########################


##Prep datasets for test (Study Group)

#Get the IDs for people with specified follow up DayIDs

for (i in 4:6){
  
  #Count the number of people who have DayID for given i

  N=0
  
  for (l in StudyGroupIDs){
    
    if (length(Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==l) & 
                                  (Sort.NewStudyGroup[,"DaysId"]==i) & 
                                  Sort.NewStudyGroup[Sort.NewStudyGroup[,"DaysId"]==1 & Sort.NewStudyGroup[,"ID"]==l, "PT.AM.PAC.Basic.Mobility.Score"]>=(min(Baseline.Study)-1) & 
                                  Sort.NewStudyGroup[Sort.NewStudyGroup[,"DaysId"]==1 & Sort.NewStudyGroup[,"ID"]==l, "PT.AM.PAC.Basic.Mobility.Score"]<=(max(Baseline.Study)+1), "DaysId"])==1){
      
      N=N+1
      
    }
  
  }
  
  #create vector to fill for with IDs for given i
  
  x=rep(0,N)
  
  T=1
  
  for (j in StudyGroupIDs){
    if (length(Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==j) & 
                                  (Sort.NewStudyGroup[,"DaysId"]==i) & 
                                  Sort.NewStudyGroup[Sort.NewStudyGroup[,"DaysId"]==1 & Sort.NewStudyGroup[,"ID"]==j, "PT.AM.PAC.Basic.Mobility.Score"]>=(min(Baseline.Study)-1) & 
                                  Sort.NewStudyGroup[Sort.NewStudyGroup[,"DaysId"]==1 & Sort.NewStudyGroup[,"ID"]==j, "PT.AM.PAC.Basic.Mobility.Score"]<=(max(Baseline.Study)+1), "DaysId"])==1){
      x[T]=j
      T=T+1
    }
  }
  
  assign(value=x, paste("BlineDay",i,"IDs.Study",sep=""))
  
}

###Create vectors for the number of patients with 4th, 5th, and 6th follow ups and their IDs
###for the study group, respectively (for baseline scores)

BlineStudy.lengths=c(length(BlineDay4IDs.Study),length(BlineDay5IDs.Study),length(BlineDay6IDs.Study))

BlineDayIDs.Study=rbind(BlineDay4IDs.Study,BlineDay5IDs.Study,BlineDay6IDs.Study)


##Calculate the differences between baseline and follow ups (Study)

for (i in 4:6){
  
  x=rep(0, BlineStudy.lengths[i-3])
  
  T=1
  
  for (j in BlineDayIDs.Study[i-3,]){
    
    x[T]=Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==j) & (Sort.NewStudyGroup[,"DaysId"]==i), "PT.AM.PAC.Basic.Mobility.Score"]-Sort.NewStudyGroup[(Sort.NewStudyGroup[,"ID"]==j) & (Sort.NewStudyGroup[,"DaysId"]==1), "PT.AM.PAC.Basic.Mobility.Score"]
    T=T+1
    
  }
  
  assign(value=x, paste("BlineDiff",i,"from.1.Study",sep="."))
  
}

##Prep datasets for test (Control Group)

#Get the IDs for people with specified follow up DayIDs

for (i in 4:6){
  
  #Count the number of people who have DayID for given i
  
  
  N=0
  
  for (l in ControlGroupIDs){
    
    if (length(Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==l) & 
                                    (Sort.NewControlGroup[,"DaysId"]==i) & 
                                    Sort.NewControlGroup[Sort.NewControlGroup[,"ID"]==l & Sort.NewControlGroup[,"DaysId"]==1, "PT.AM.PAC.Basic.Mobility.Score"]>=(min(Baseline.Study)-1) & 
                                    Sort.NewControlGroup[Sort.NewControlGroup[,"ID"]==l & Sort.NewControlGroup[,"DaysId"]==1, "PT.AM.PAC.Basic.Mobility.Score"]<=(max(Baseline.Study)+1), "DaysId"])==1){
      
      N=N+1
      
    }
    
  }
  #create vector to fill for with IDs for given i
  
  x=rep(0,N)
  
  T=1
  
  for (j in ControlGroupIDs){
    if (length(Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==j) & 
                                    (Sort.NewControlGroup[,"DaysId"]==i) & 
                                    Sort.NewControlGroup[Sort.NewControlGroup[,"DaysId"]==1 & Sort.NewControlGroup[,"ID"]==j, "PT.AM.PAC.Basic.Mobility.Score"]>=(min(Baseline.Study)-1) & 
                                    Sort.NewControlGroup[Sort.NewControlGroup[,"DaysId"]==1 & Sort.NewControlGroup[,"ID"]==j, "PT.AM.PAC.Basic.Mobility.Score"]<=(max(Baseline.Study)+1), "DaysId"])==1){
      x[T]=j
      T=T+1
    }
  }
  
  assign(value=x, paste("BlineDay",i,"IDs.Control",sep=""))
  
}

###Create vectors for the number of patients with 4th, 5th, and 6th follow ups and their IDs
###for the control group, respectively (for baseline scores)

BlineControl.lengths=c(length(BlineDay4IDs.Control),length(BlineDay5IDs.Control),length(BlineDay6IDs.Control))

BlineDayIDs.Control=rbind(BlineDay4IDs.Control,BlineDay5IDs.Control,BlineDay6IDs.Control)

##Calculate the differences between baseline and follow ups (Control)

for (i in 4:6){
  
  x=rep(0,BlineControl.lengths[i-3])
  
  T=1
  
  for (j in BlineDayIDs.Control[i-3,]){
    
    x[T]=Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==j) & (Sort.NewControlGroup[,"DaysId"]==i), "PT.AM.PAC.Basic.Mobility.Score"]-Sort.NewControlGroup[(Sort.NewControlGroup[,"ID"]==j) & (Sort.NewControlGroup[,"DaysId"]==1), "PT.AM.PAC.Basic.Mobility.Score"]
    T=T+1
    
  }
  
  assign(value=x, paste("BlineDiff",i,"from.1.Control",sep="."))
  
}


###T Tests

#Tests for fourth, fifth, and sixth follow up comparison

t.test(BlineDiff.4.from.1.Study, BlineDiff.4.from.1.Control, alternative="greater")

t.test(BlineDiff.5.from.1.Study, BlineDiff.5.from.1.Control, alternative="greater")

t.test(BlineDiff.6.from.1.Study, BlineDiff.6.from.1.Control, alternative="greater")

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

T=1

for (i in 1:219){

  if (Sort.NewStudyGroup[i,"ID"]!=StudyGroupIDs[T] & T!=32){
    
    T=T+1
    
  }
    
  Sort.NewStudyGroup[i,"Baseline Average"]=
    
    mean(c(Sort.NewStudyGroup[Sort.NewStudyGroup[,"DaysId"]==1 & Sort.NewStudyGroup[,"ID"]==StudyGroupIDs[T],"PT.AM.PAC.Basic.Mobility.Score"], 
         Sort.NewStudyGroup[Sort.NewStudyGroup[,"DaysId"]==2 & Sort.NewStudyGroup[,"ID"]==StudyGroupIDs[T],"PT.AM.PAC.Basic.Mobility.Score"]))

}

T=1

for (i in 1:628){
  
  if (Sort.NewControlGroup[i,"ID"]!=ControlGroupIDs[T] & T!=91){
    
    T=T+1
    
  }
  
  Sort.NewControlGroup[i,"Baseline Average"]=
    
    mean(c(Sort.NewControlGroup[Sort.NewControlGroup[,"DaysId"]==1 & Sort.NewControlGroup[,"ID"]==ControlGroupIDs[T],"PT.AM.PAC.Basic.Mobility.Score"], 
           Sort.NewControlGroup[Sort.NewControlGroup[,"DaysId"]==2 & Sort.NewControlGroup[,"ID"]==ControlGroupIDs[T],"PT.AM.PAC.Basic.Mobility.Score"]))
  
  
}



###Refine the control group to only contain the baseline averages within the study group's
###baseline average

Refined.ControlGroup=Sort.NewControlGroup[Sort.NewControlGroup[,"Baseline Average"]>=min(Sort.NewStudyGroup[,"Baseline Average"]) & Sort.NewControlGroup[,"Baseline Average"]<=max(Sort.NewStudyGroup[,"Baseline Average"]),]

###Plot the distribution of baseline averages

plot(-100,-100, xlim=c(0,106), ylim=c(27,65), xlab="Index", ylab="Baseline Mobility Score", 
     main="Distribution of Baseline Averages for Mobility Score in Groups")

points(1:31, Sort.NewStudyGroup[Sort.NewStudyGroup[,"DaysId"]==1, "Baseline Average"], pch=19, col="green")

points(60:105, Refined.ControlGroup[Refined.ControlGroup[,"DaysId"]==1, "Baseline Average"], pch=19, col="red")

abline(h=min(Sort.NewStudyGroup[,"Baseline Average"]))
abline(h=max(Sort.NewStudyGroup[,"Baseline Average"]))


  
###Create a data matrix for the characteristics for each patient in the study group

Study.Characteristics=as.data.frame(matrix(nrow=31, ncol=5))


T=1

for (i in 1:219){

  if (Sort.NewStudyGroup[i,"ID"]==StudyGroupIDs[T] & T!=32){
  
    Study.Characteristics[T,]=Sort.NewStudyGroup[i,c("ID", "Age", "Gender", "New Race", "Baseline Average")]
    
    T=T+1
  
  }

}

colnames(Study.Characteristics)=c("ID","Age","Gender", "New Race", "Baseline Average")

for (i in 1:31){
  
  if (Study.Characteristics[i,"Gender"]==2){
    
    Study.Characteristics[i,"Gender"]="Male"
    
  }else if (Study.Characteristics[i,"Gender"]==1){
    
    Study.Characteristics[i,"Gender"]="Female"
    
  }
  
}

#24, 46, 124 are cross overs



###initialize a vector for containing the IDs of study patients who have a nonzero number
###of matches
match.IDs=0

###match patients in study group to patients in (refined control group) 
###based on their characteristics (without crossovers)
for (i in 1:31){

  x=Refined.ControlGroup[(Refined.ControlGroup[,"Age"]>=Study.Characteristics[i,"Age"]-2 & 
                                  Refined.ControlGroup[,"Age"]<=Study.Characteristics[i,"Age"]+2 &
                            Refined.ControlGroup[,"Baseline Average"]>=Study.Characteristics[i,"Baseline Average"]-20 &
                            Refined.ControlGroup[,"Baseline Average"]<=Study.Characteristics[i,"Baseline Average"]+20 &
                                  Refined.ControlGroup[,"Gender"]==Study.Characteristics[i,"Gender"] &
                                  Refined.ControlGroup[,"New Race"]==Study.Characteristics[i,"New Race"] &
                                  Refined.ControlGroup[,"ID"]!=24 &
                                  Refined.ControlGroup[,"ID"]!=46 &
                                  Refined.ControlGroup[,"ID"]!=124),]

  if(dim(x)[1]!=0){
    
    match.IDs=c(match.IDs,Study.Characteristics[i,"ID"])
    
  }
  
  assign(value=x, paste("match",Study.Characteristics[i,"ID"],sep="."))
  
  
}

###delete the 0 used to initialize the vector
match.IDs=match.IDs[-1]

match.IDs

###Use propensity score matching based on medical record history for each match group

Medical.History.Variables=colnames(Sort.NewStudyGroup)[47:69]

fmla=as.formula(paste("Group ~ ", paste(Medical.History.Variables, collapse="+")))

###conduct propensity score matching with R

# matchit(formula = fmla, method="subclass", distance = "logit",
#         data=rbind(Sort.NewStudyGroup[Sort.NewStudyGroup[,"ID"]==129 &
#               Sort.NewStudyGroup[,"DaysId"]==3,], match.129[match.129[,"DaysId"]==3,])[,c("Group", Medical.History.Variables)])


###demonstrate how the propensity scores for study and control vary greatly

glm.out=glm(formula=fmla, family=binomial(logit),
    data=rbind(Sort.NewStudyGroup[Sort.NewStudyGroup[,"ID"]==129 & Sort.NewStudyGroup[,"DaysId"]==3,], 
               match.129[match.129[,"DaysId"]==3,])[,c("Group", Medical.History.Variables)])

glm.out$fitted.values

###Combine study group patients with their matched control group patients

Group.17=rbind(Sort.NewStudyGroup[Sort.NewStudyGroup[,"ID"]==17,], match.17)
Group.22=rbind(Sort.NewStudyGroup[Sort.NewStudyGroup[,"ID"]==22,], match.22)
Group.38=rbind(Sort.NewStudyGroup[Sort.NewStudyGroup[,"ID"]==38,], match.38)
Group.46=rbind(Sort.NewStudyGroup[Sort.NewStudyGroup[,"ID"]==46,], match.46)
Group.54=rbind(Sort.NewStudyGroup[Sort.NewStudyGroup[,"ID"]==54,], match.54)
Group.58=rbind(Sort.NewStudyGroup[Sort.NewStudyGroup[,"ID"]==58,], match.58)
Group.74=rbind(Sort.NewStudyGroup[Sort.NewStudyGroup[,"ID"]==74,], match.74)
Group.84=rbind(Sort.NewStudyGroup[Sort.NewStudyGroup[,"ID"]==84,], match.84)
Group.99=rbind(Sort.NewStudyGroup[Sort.NewStudyGroup[,"ID"]==99,], match.99)
Group.129=rbind(Sort.NewStudyGroup[Sort.NewStudyGroup[,"ID"]==129,], match.129)
Group.147=rbind(Sort.NewStudyGroup[Sort.NewStudyGroup[,"ID"]==147,], match.147)

###show a matched group

View(Group.22[,c("ID", "Age", "Gender", "New Race", "Baseline Average")])

###Create a variable for Difference between Baseline Average and 7th follow up mobility score
###for Group.17

T=1

for (i in 1:(dim(Group.17)[1])){
    
    if (Group.17[i,"ID"]!=Group.17[Group.17[,"DaysId"]==1,"ID"][T] & T<(length(Group.17[Group.17[,"DaysId"]==1,"ID"]))){
      
      T=T+1
      
    }
    
    Group.17[i,"Day7Diff"]=
      
      Group.17[Group.17[,"DaysId"]==7 & Group.17[,"ID"]==Group.17[Group.17[,"DaysId"]==1,"ID"][T],"PT.AM.PAC.Basic.Mobility.Score"]-Group.17[Group.17[,"DaysId"]==1 & Group.17[,"ID"]==Group.17[Group.17[,"DaysId"]==1,"ID"][T],"Baseline Average"]
    
}


###Create a variable for Difference between Baseline Average and 4th and 5th follow up mobility 
###score for Group.22

for (j in 4:5){

  T=1
  
  for (i in 1:(dim(Group.22)[1])){
    
    if (Group.22[i,"ID"]!=Group.22[Group.22[,"DaysId"]==1,"ID"][T] & T<(length(Group.22[Group.22[,"DaysId"]==1,"ID"]))){
      
      T=T+1
      
    }
    
    Group.22[i, paste("Day", j, "Diff", sep="")]=
      
      Group.22[Group.22[,"DaysId"]==j & Group.22[,"ID"]==Group.22[Group.22[,"DaysId"]==1,"ID"][T],"PT.AM.PAC.Basic.Mobility.Score"]-Group.22[Group.22[,"DaysId"]==1 & Group.22[,"ID"]==Group.22[Group.22[,"DaysId"]==1,"ID"][T],"Baseline Average"]
    
  }

}

###Create a variable for Difference between Baseline Average and 4th and 5th follow up mobility 
###score for Group.46

for (j in 4:5){
  
  T=1
  
  for (i in 1:(dim(Group.46)[1])){
    
    if (Group.46[i,"ID"]!=Group.46[Group.46[,"DaysId"]==1,"ID"][T] & T<(length(Group.46[Group.46[,"DaysId"]==1,"ID"]))){
      
      T=T+1
      
    }
    
    Group.46[i, paste("Day", j, "Diff", sep="")]=
      
      Group.46[Group.46[,"DaysId"]==j & Group.46[,"ID"]==Group.46[Group.46[,"DaysId"]==1,"ID"][T],"PT.AM.PAC.Basic.Mobility.Score"]-Group.46[Group.46[,"DaysId"]==1 & Group.46[,"ID"]==Group.46[Group.46[,"DaysId"]==1,"ID"][T],"Baseline Average"]
    
  }
  
}



###Create a variable for Difference between Baseline Average and 5th follow up mobility 
###score for Group.58

for (j in 5){
  
  T=1
  
  for (i in 1:(dim(Group.58)[1])){
    
    if (Group.58[i,"ID"]!=Group.58[Group.58[,"DaysId"]==1,"ID"][T] & T<(length(Group.58[Group.58[,"DaysId"]==1,"ID"]))){
      
      T=T+1
      
    }
    
    Group.58[i, paste("Day", j, "Diff", sep="")]=
      
      Group.58[Group.58[,"DaysId"]==j & Group.58[,"ID"]==Group.58[Group.58[,"DaysId"]==1,"ID"][T],"PT.AM.PAC.Basic.Mobility.Score"]-Group.58[Group.58[,"DaysId"]==1 & Group.58[,"ID"]==Group.58[Group.58[,"DaysId"]==1,"ID"][T],"Baseline Average"]
    
  }
  
}


###Create a variable for Difference between Baseline Average and 4th and 5th follow up mobility 
###score for Group.74

for (j in 4:6){
  
  T=1
  
  for (i in 1:(dim(Group.74)[1])){
    
    if (Group.74[i,"ID"]!=Group.74[Group.74[,"DaysId"]==1,"ID"][T] & T<(length(Group.74[Group.74[,"DaysId"]==1,"ID"]))){
      
      T=T+1
      
    }
    
    Group.74[i, paste("Day", j, "Diff", sep="")]=
      
      Group.74[Group.74[,"DaysId"]==j & Group.74[,"ID"]==Group.74[Group.74[,"DaysId"]==1,"ID"][T],"PT.AM.PAC.Basic.Mobility.Score"]-Group.74[Group.74[,"DaysId"]==1 & Group.74[,"ID"]==Group.74[Group.74[,"DaysId"]==1,"ID"][T],"Baseline Average"]
    
  }
  
}


###Create a variable for Difference between Baseline Average and 4th and 5th follow up mobility 
###score for Group.99

for (j in 4:5){
  
  T=1
  
  for (i in 1:(dim(Group.99)[1])){
    
    if (Group.99[i,"ID"]!=Group.99[Group.99[,"DaysId"]==1,"ID"][T] & T<(length(Group.99[Group.99[,"DaysId"]==1,"ID"]))){
      
      T=T+1
      
    }
    
    Group.99[i, paste("Day", j, "Diff", sep="")]=
      
      Group.99[Group.99[,"DaysId"]==j & Group.99[,"ID"]==Group.99[Group.99[,"DaysId"]==1,"ID"][T],"PT.AM.PAC.Basic.Mobility.Score"]-Group.99[Group.99[,"DaysId"]==1 & Group.99[,"ID"]==Group.99[Group.99[,"DaysId"]==1,"ID"][T],"Baseline Average"]
    
  }
  
}

###Create a variable for Difference between Baseline Average and 4th, 5th, 6th follow up mobility 
###score for Group.129

for (j in 4:6){
  
  T=1
  
  for (i in 1:(dim(Group.129)[1])){
    
    if (Group.129[i,"ID"]!=Group.129[Group.129[,"DaysId"]==1,"ID"][T] & T<(length(Group.129[Group.129[,"DaysId"]==1,"ID"]))){
      
      T=T+1
      
    }
    
    Group.129[i, paste("Day", j, "Diff", sep="")]=
      
      Group.129[Group.129[,"DaysId"]==j & Group.129[,"ID"]==Group.129[Group.129[,"DaysId"]==1,"ID"][T],"PT.AM.PAC.Basic.Mobility.Score"]-Group.129[Group.129[,"DaysId"]==1 & Group.129[,"ID"]==Group.129[Group.129[,"DaysId"]==1,"ID"][T],"Baseline Average"]
    
  }
  
}


###Take difference of difference and do t test for 4th follow up

test4data=c(Group.22[Group.22[,"DaysId"]==1 & Group.22[,"ID"]==22, "Day4Diff"] -  Group.22[Group.22[,"DaysId"]==1 & Group.22[,"ID"]==6, "Day4Diff"],
            Group.46[Group.46[,"DaysId"]==1 & Group.46[,"ID"]==46, "Day4Diff"] -  Group.46[Group.46[,"DaysId"]==1 & Group.46[,"ID"]==139, "Day4Diff"],
            Group.74[Group.74[,"DaysId"]==1 & Group.74[,"ID"]==74, "Day4Diff"] -  Group.74[Group.74[,"DaysId"]==1 & Group.74[,"ID"]==7, "Day4Diff"],
            Group.99[Group.99[,"DaysId"]==1 & Group.99[,"ID"]==99, "Day4Diff"] -  Group.99[Group.99[,"DaysId"]==1 & Group.99[,"ID"]==18, "Day4Diff"],
            Group.129[Group.129[,"DaysId"]==1 & Group.129[,"ID"]==129, "Day4Diff"] -  Group.129[Group.129[,"DaysId"]==1 & Group.129[,"ID"]==10, "Day4Diff"])

test4data

t.test(test4data, alternative = "greater")


###Take difference of difference and do t test for 5th follow up

test5data=c(Group.22[Group.22[,"DaysId"]==1 & Group.22[,"ID"]==22, "Day5Diff"] -  Group.22[Group.22[,"DaysId"]==1 & Group.22[,"ID"]==6, "Day5Diff"],
            Group.46[Group.46[,"DaysId"]==1 & Group.46[,"ID"]==46, "Day5Diff"] -  Group.46[Group.46[,"DaysId"]==1 & Group.46[,"ID"]==139, "Day5Diff"],
            Group.58[Group.58[,"DaysId"]==1 & Group.58[,"ID"]==58, "Day5Diff"] -  Group.58[Group.58[,"DaysId"]==1 & Group.58[,"ID"]==39, "Day5Diff"],
            Group.74[Group.74[,"DaysId"]==1 & Group.74[,"ID"]==74, "Day5Diff"] -  Group.74[Group.74[,"DaysId"]==1 & Group.74[,"ID"]==7, "Day5Diff"],
            Group.99[Group.99[,"DaysId"]==1 & Group.99[,"ID"]==99, "Day5Diff"] -  Group.99[Group.99[,"DaysId"]==1 & Group.99[,"ID"]==125, "Day5Diff"],
            Group.129[Group.129[,"DaysId"]==1 & Group.129[,"ID"]==129, "Day5Diff"] -  Group.129[Group.129[,"DaysId"]==1 & Group.129[,"ID"]==10, "Day5Diff"])

test5data

t.test(test5data, alternative = "greater")

###Take difference of difference and do t test for final follow up

testfinaldata=c(Group.17[Group.17[,"DaysId"]==1 & Group.17[,"ID"]==17, "Day7Diff"] -  Group.17[Group.17[,"DaysId"]==1 & Group.17[,"ID"]==61, "Day7Diff"],
            Group.22[Group.22[,"DaysId"]==1 & Group.22[,"ID"]==22, "Day5Diff"] -  Group.22[Group.22[,"DaysId"]==1 & Group.22[,"ID"]==6, "Day5Diff"],
            Group.46[Group.46[,"DaysId"]==1 & Group.46[,"ID"]==46, "Day5Diff"] -  Group.46[Group.46[,"DaysId"]==1 & Group.46[,"ID"]==139, "Day5Diff"],
            Group.58[Group.58[,"DaysId"]==1 & Group.58[,"ID"]==58, "Day5Diff"] -  Group.58[Group.58[,"DaysId"]==1 & Group.58[,"ID"]==39, "Day5Diff"],
            Group.74[Group.74[,"DaysId"]==1 & Group.74[,"ID"]==74, "Day6Diff"] -  Group.74[Group.74[,"DaysId"]==1 & Group.74[,"ID"]==7, "Day6Diff"],
            Group.99[Group.99[,"DaysId"]==1 & Group.99[,"ID"]==99, "Day5Diff"] -  Group.99[Group.99[,"DaysId"]==1 & Group.99[,"ID"]==125, "Day5Diff"],
            Group.129[Group.129[,"DaysId"]==1 & Group.129[,"ID"]==129, "Day6Diff"] -  Group.129[Group.129[,"DaysId"]==1 & Group.129[,"ID"]==10, "Day6Diff"])

testfinaldata

t.test(testfinaldata, alternative = "greater")


###Demonstrate problems with follow up differences not matching up so can't take differences


# View(Group.129[,c("ID", "Age", "Gender", "New Race", "Baseline Average", "Group", "DaysId", "PT.AM.PAC.Basic.Mobility.Score", "OT.AM.Daily.Activity.Score", "ST.AM.Applied.Cogn.Score")])
# View(Group.58[,c("ID", "Age", "Gender", "New Race", "Baseline Average", "Group", "DaysId", "PT.AM.PAC.Basic.Mobility.Score")])
# View(Group.54[,c("ID", "Age", "Gender", "New Race", "Baseline Average", "Group", "DaysId", "PT.AM.PAC.Basic.Mobility.Score")])

colnames(Master)



