#################################Program Description################################
#Name: upload data                                                                 #
#Author: Traymon Beavers                                                           #
#Date Created: 7/21/2017                                                           #
#Date Updated: 9/14/2017                                                           #
#Purpose: To upload and reconfigure data from the stroke rehabilitation program    #
####################################################################################

# Upload data ####

# upload Data in R
OldMaster = read.csv("data/DEID_All4.csv")

# delete extraneous varibale
OldMaster = OldMaster[, -1]

# delete the patients that were pregnant at the time of the study
OldMaster = OldMaster[OldMaster[, "Current.pregnancy"] == FALSE, ]

# rename dataset
Master = OldMaster

# Rename races other than black or white as "Other" ####
Master[Master[,"Race"] == "BLACK OR AFRICAN AMERICAN", "New.Race"] = "Black"

Master[Master[,"Race"] == "WHITE", "New.Race"] = "White"

Master[Master[,"Race"] != "BLACK OR AFRICAN AMERICAN" & Master[,"Race"] != "WHITE", "New.Race"] = "Other"

# Create consistent missing value indicators for variables used for analysis####

# cycle through every observation in the master dataset
for (i in 1:dim(Master)[1]){
  
  # change any missing mobility scores to NA
  if(Master[i, "PT.AM.PAC.Basic.Mobility.Score"] == 9999
     | Master[i, "PT.AM.PAC.Basic.Mobility.Score"] == 8888
     | is.na(Master[i, "PT.AM.PAC.Basic.Mobility.Score"]) == 1){
    
    Master[i, "PT.AM.PAC.Basic.Mobility.Score"] = NA
    
  }
  
  # change any missing activity scores to NA
  if(Master[i, "OT.AM.Daily.Activity.Score"] == 9999
     | Master[i, "OT.AM.Daily.Activity.Score"] == 8888
     | Master[i, "OT.AM.Daily.Activity.Score"] == 9909
     | is.na(Master[i, "OT.AM.Daily.Activity.Score"]) == 1){
    
    Master[i, "OT.AM.Daily.Activity.Score"] = NA
    
  }
  
  # change any missing cognitive scores to NA
  if(Master[i, "ST.AM.Applied.Cogn.Score"] == 9999
     | Master[i, "ST.AM.Applied.Cogn.Score"] == 8888
     | is.na(Master[i, "ST.AM.Applied.Cogn.Score"]) == 1){
    
    Master[i, "ST.AM.Applied.Cogn.Score"] = NA
    
  }
  
  # change any missing CVG variables to NA
  if(Master[i, "Cardiovascular.Group.Intensity.Mets."] == 9999
     | Master[i, "Cardiovascular.Group.Intensity.Mets."] == 8888
     | is.na(Master[i, "Cardiovascular.Group.Intensity.Mets."]) == 1){
    
    Master[i, "Cardiovascular.Group.Intensity.Mets."] = NA
    
  }
  
  if(Master[i, "Cardiovascular.Group.SessionsPerWeek"] == 9999
     | Master[i, "Cardiovascular.Group.SessionsPerWeek"] == 8888
     | is.na(Master[i, "Cardiovascular.Group.SessionsPerWeek"]) == 1){
    
    Master[i, "Cardiovascular.Group.SessionsPerWeek"] = NA
    
  }
  
  if(Master[i, "Cardiovascular.Group.Met.Mins"] == 99980001
     | Master[i, "Cardiovascular.Group.Met.Mins"] == 78996544
     | is.na(Master[i, "Cardiovascular.Group.Met.Mins"]) == 1){
    
    Master[i, "Cardiovascular.Group.Met.Mins"] = NA
    
  }
  
  if(Master[i, "Cardiovascular.Group.Tot.Mins"] == 9999
     | Master[i, "Cardiovascular.Group.Tot.Mins"] == 8888
     | is.na(Master[i, "Cardiovascular.Group.Tot.Mins"]) == 1){
    
    Master[i, "Cardiovascular.Group.Tot.Mins"] = NA
    
  }  
  
  if(Master[i, "Cardiovascular.Group.Tot.Sessions"] == 9999
     | Master[i, "Cardiovascular.Group.Tot.Sessions"] == 8888
     | is.na(Master[i, "Cardiovascular.Group.Tot.Sessions"]) == 1){
    
    Master[i, "Cardiovascular.Group.Tot.Sessions"] = NA
    
  }
  
  # change missing ModRankinScores to NA
  if(Master[i, "ModRankinScore"] == 9999
     | Master[i, "ModRankinScore"] == 8888
     | is.na(Master[i, "ModRankinScore"]) == 1){
    
    Master[i, "ModRankinScore"] = NA
    
  }
  
  # change missing data for various variables to NA
  for (j in c(12,30:33,74:80)){
    
    if(Master[i, j] == 9999
       | Master[i, j] == 8888
       | is.na(Master[i, j]) == 1){
      
      Master[i, j] = NA
      
    }
    
  }
  
}

# Create a new dataset with only patients that eventually landed in relevant groups ####

# create vector only containing patients who landed in the study group
StudyGroupIDs = unique(Master[Master[, "Group"] == "Study Group", "ID"])

# create a dataset only containing these patients
Master.Study = Master[Master[, "ID"] %in% StudyGroupIDs, ]

# create vector only containing patients who landed in the control group
ControlGroupIDs = unique(Master[Master[, "Group"] == "Control Group", "ID"])

# create a dataset only containing these patients
Master.Control = Master[Master[, "ID"] %in% ControlGroupIDs, ]

# combine both treatment groups back together
NewMaster = rbind(Master.Study, Master.Control)

# order the dataset first by the patient ID and then by their visit number
NewMaster = NewMaster[order(NewMaster[, "ID"], NewMaster[, "DaysId"]), ]

# delete patients that somehow landed in both groups
NewMaster = NewMaster[-which(NewMaster[, "ID"] %in% intersect(StudyGroupIDs, ControlGroupIDs)), ]

# Create variables for days after assignment to group and important scores for each functional outcome ####

# create a vector containing each ID in the dataset created above
NewMasterIDs = unique(NewMaster[,"ID"])

# initialize the variable for every observation's number of days after their stroke
# when they were assigned to their treatment group
NewMaster[, "Days.After.At.Assignment"] = rep(NA,dim(NewMaster)[1])

# initialize the variable for every observation's score when they were first admitted
NewMaster[, "Admission.Mobility"] = rep(NA,dim(NewMaster)[1])
NewMaster[, "Admission.Activity"] = rep(NA,dim(NewMaster)[1])
NewMaster[, "Admission.Cognitive"] = rep(NA,dim(NewMaster)[1])

# initialize the variable for every observation's score when they were discharged
NewMaster[, "Discharge.Mobility"] = rep(NA,dim(NewMaster)[1])
NewMaster[, "Discharge.Activity"] = rep(NA,dim(NewMaster)[1])
NewMaster[, "Discharge.Cognitive"] = rep(NA,dim(NewMaster)[1])


# initialize a counter for scrolling through IDs
C=1

# cycle through every observation in the dataset
for (i in 1:dim(NewMaster)[1]){
  
  # if the ID for the current observation isn't equal to the ID for the previous ID, 
  # increase the counter by one to move to the next ID
  if (NewMaster[i,"ID"] != NewMasterIDs[C] & C != (length(NewMasterIDs))){
    
    C = C+1
    
  }
  
  # calculate number of days after their stroke when they were assigned to their treatment group
  # for the current observation 
  NewMaster[i,"Days.After.At.Assignment"] =
    NewMaster[NewMaster[,"DaysId"] == 2 & NewMaster[,"ID"] == NewMasterIDs[C], "Days_afterOnset"]
  
  # calculate score when they were first admitted for each outcome for current observation
  NewMaster[i, "Admission.Mobility"] = NewMaster[NewMaster[, "DaysId"] == 1 & NewMaster[, "ID"] == NewMasterIDs[C], "PT.AM.PAC.Basic.Mobility.Score"]
  NewMaster[i, "Admission.Activity"] = NewMaster[NewMaster[, "DaysId"] == 1 & NewMaster[, "ID"] == NewMasterIDs[C], "OT.AM.Daily.Activity.Score"]
  NewMaster[i, "Admission.Cognitive"] = NewMaster[NewMaster[, "DaysId"] == 1 & NewMaster[, "ID"] == NewMasterIDs[C], "ST.AM.Applied.Cogn.Score"]
  
  # calculate score when they were discharged for each outcome for current observation  
  NewMaster[i, "Discharge.Mobility"] = NewMaster[NewMaster[, "DaysId"] == 2 & NewMaster[, "ID"] == NewMasterIDs[C], "PT.AM.PAC.Basic.Mobility.Score"]
  NewMaster[i, "Discharge.Activity"] = NewMaster[NewMaster[, "DaysId"] == 2 & NewMaster[, "ID"] == NewMasterIDs[C], "OT.AM.Daily.Activity.Score"]
  NewMaster[i, "Discharge.Cognitive"] = NewMaster[NewMaster[, "DaysId"] == 2 & NewMaster[, "ID"] == NewMasterIDs[C], "ST.AM.Applied.Cogn.Score"]
  
  
}

# calculate the number of days after group assignement for each observation
NewMaster[,"Days.After.Assignment"] =
  NewMaster[,"Days_afterOnset"] - NewMaster[,"Days.After.At.Assignment"]

# Create a dataset with one observation for each patient ####
NewMaster.One = NewMaster[NewMaster[,"DaysId"] == 3,]

# create vectors for easy reference to each functional outcome
ScoreName = c("Mobility", "Activity", "Cognitive")
ScoreVarName = c("PT.AM.PAC.Basic.Mobility.Score", "OT.AM.Daily.Activity.Score", "ST.AM.Applied.Cogn.Score")


# Create a variable for survival time ####

# initialize the variables needed to calculate survival time
NewMaster.One[, "Stroke.Date.Month"] = NA
NewMaster.One[, "Stroke.Date.Day"] = NA
NewMaster.One[, "Stroke.Date.Year"] = NA
NewMaster.One[, "Discharge.Date.Number"] = NA
NewMaster.One[, "Death.Date.Month"] = NA
NewMaster.One[, "Death.Date.Day"] = NA
NewMaster.One[, "Death.Date.Year"] = NA
NewMaster.One[, "Death.Date.Number"] = NA
NewMaster.One[, "Survival.Time"] = NA

# make a vector for the number of days in each month for leap years and otherwise
Day.Vec = c(0,31,28,31,30,31,30,31,31,30,31,30,31)
Day.Vec.Leap = c(0,31,29,31,30,31,30,31,31,30,31,30,31) 

# cycle through the deceased patients
for (i in NewMaster.One[which(NewMaster.One[, "Deceased_Y.N"] == TRUE), "ID"]){
  
  # split the dates into their three components
  NewMaster.One[NewMaster.One[,"ID"] == i, "Stroke.Date.Month"] = 
    as.numeric(strsplit(paste(NewMaster.One[NewMaster.One[,"ID"] == i, "Stroke.Date"]), split = "/")[[1]][1])
  
  NewMaster.One[NewMaster.One[,"ID"] == i, "Stroke.Date.Day"] = 
    as.numeric(strsplit(paste(NewMaster.One[NewMaster.One[,"ID"] == i, "Stroke.Date"]), split = "/")[[1]][2])
  
  NewMaster.One[NewMaster.One[,"ID"] == i, "Stroke.Date.Year"] = 
    as.numeric(strsplit(paste(NewMaster.One[NewMaster.One[,"ID"] == i, "Stroke.Date"]), split = "/")[[1]][3])
  
  NewMaster.One[NewMaster.One[,"ID"] == i, "Death.Date.Month"] = 
    as.numeric(strsplit(paste(NewMaster.One[NewMaster.One[,"ID"] == i, "Deceased_Dt"]), split = "/")[[1]][1])
  
  NewMaster.One[NewMaster.One[,"ID"] == i, "Death.Date.Day"] = 
    as.numeric(strsplit(paste(NewMaster.One[NewMaster.One[,"ID"] == i, "Deceased_Dt"]), split = "/")[[1]][2])
  
  NewMaster.One[NewMaster.One[,"ID"] == i, "Death.Date.Year"] = 
    as.numeric(strsplit(paste(NewMaster.One[NewMaster.One[,"ID"] == i, "Deceased_Dt"]), split = "/")[[1]][3])
  
  # if the year is 2017 add 731 to the number to reflect the leap day and use the regular day vector
  if (NewMaster.One[NewMaster.One[,"ID"] == i, "Stroke.Date.Year"] == 2017){
    
    NewMaster.One[NewMaster.One[,"ID"] == i, "Discharge.Date.Number"] = 
      sum(Day.Vec[1:NewMaster.One[NewMaster.One[,"ID"] == i, "Stroke.Date.Month"]]) + 
      NewMaster.One[NewMaster.One[,"ID"] == i, "Stroke.Date.Day"] + 
      NewMaster[NewMaster[,"ID"] == i & NewMaster[, "DaysId"] == 2, "Days_afterOnset"] + 731
    
    # if the year is 2016 use the leap year day vector    
  }else if (NewMaster.One[NewMaster.One[,"ID"] == i, "Stroke.Date.Year"] == 2016){
    
    NewMaster.One[NewMaster.One[,"ID"] == i, "Discharge.Date.Number"] = 
      sum(Day.Vec[1:NewMaster.One[NewMaster.One[,"ID"] == i, "Stroke.Date.Month"]]) + 
      NewMaster.One[NewMaster.One[,"ID"] == i, "Stroke.Date.Day"] +
      NewMaster[NewMaster[,"ID"] == i & NewMaster[, "DaysId"] == 2, "Days_afterOnset"] + 365
    
    
    # if the year is 2015 use the regular day vector    
  }else{ 
    
    NewMaster.One[NewMaster.One[,"ID"] == i, "Discharge.Date.Number"] = 
      sum(Day.Vec[1:NewMaster.One[NewMaster.One[,"ID"] == i, "Stroke.Date.Month"]]) + 
      NewMaster.One[NewMaster.One[,"ID"] == i, "Stroke.Date.Day"] +
      NewMaster[NewMaster[,"ID"] == i & NewMaster[, "DaysId"] == 2, "Days_afterOnset"]    
    
  }
  
  # if the year is 2017 add 731 to the number to reflect the leap day and use the regular day vector
  if (NewMaster.One[NewMaster.One[,"ID"] == i, "Death.Date.Year"] == 2017){
    
    NewMaster.One[NewMaster.One[,"ID"] == i, "Death.Date.Number"] = 
      sum(Day.Vec[1:NewMaster.One[NewMaster.One[,"ID"] == i, "Death.Date.Month"]]) + 
      NewMaster.One[NewMaster.One[,"ID"] == i, "Death.Date.Day"] + 731    
    
    # if the year is 2016 use the leap year day vector    
  }else if (NewMaster.One[NewMaster.One[,"ID"] == i, "Death.Date.Year"] == 2016){
    
    NewMaster.One[NewMaster.One[,"ID"] == i, "Death.Date.Number"] = 
      sum(Day.Vec[1:NewMaster.One[NewMaster.One[,"ID"] == i, "Death.Date.Month"]]) + 
      NewMaster.One[NewMaster.One[,"ID"] == i, "Death.Date.Day"] + 365
    
    # if the year is 2015 use the regular day vector    
  }else{ 
    
    NewMaster.One[NewMaster.One[,"ID"] == i, "Death.Date.Number"] = 
      sum(Day.Vec[1:NewMaster.One[NewMaster.One[,"ID"] == i, "Death.Date.Month"]]) + 
      NewMaster.One[NewMaster.One[,"ID"] == i, "Death.Date.Day"]   
    
  }
  
  # calculate the survival time
  NewMaster.One[NewMaster.One[,"ID"] == i, "Survival.Time"] = NewMaster.One[NewMaster.One[,"ID"] == i, "Death.Date.Number"] - 
    NewMaster.One[NewMaster.One[,"ID"] == i, "Discharge.Date.Number"] 
  
}

# create a variable for the control patient with the greatest timepoint
long.last = max(NewMaster.One[,"Survival.Time"], na.rm = TRUE)

# fill in survival time for all nondeceased patients
NewMaster.One[NewMaster.One[, "Deceased_Y.N"] == FALSE, "Survival.Time"] = long.last + 1

# place an NA where the deceased date was missing
NewMaster.One[NewMaster.One[, "Deceased_Dt"] == "9/9/9999", "Survival.Time"] = NA

# Create a variable for propensity score ####

# initialize the variable for every propensity score
NewMaster.One[,"Propensity.Score"] = 0

NewMaster.One[,"Propensity.Weight"] = 0

NewMaster.One[, "Group"] = droplevels(NewMaster.One[, "Group"])

NewMaster.One[,"Group"] = relevel(NewMaster.One[,"Group"], ref = "Study Group")

Propensity.Variables = colnames(NewMaster.One)[c(2:3,8:10,18:20,23,30,37:44,67,116:121)]

Propensity.Variables2 = colnames(NewMaster.One)[c(2:3,8:10,18:20,23,30,37:44,46:68,116:121)]

fmla = as.formula(paste("Group ~ ", paste(Propensity.Variables, collapse="+")))

fmla2 = as.formula(paste("Group ~ ", paste(Propensity.Variables2, collapse="+")))

# calculate the propensity score for each patient
glm.out = glm(formula = fmla, 
              family = binomial(logit),
              data = NewMaster.One[,c("Group", Propensity.Variables)])

summary(glm.out)

NewMaster.One[,"Propensity.Score"] = glm.out$fitted.values

NewMaster.One[NewMaster.One[, "Group"] == "Study Group", "Propensity.Weight"] = 
  1/(1-NewMaster.One[NewMaster.One[, "Group"] == "Study Group", "Propensity.Score"]) 

NewMaster.One[NewMaster.One[, "Group"] == "Control Group", "Propensity.Weight"] = 
  1/NewMaster.One[NewMaster.One[, "Group"] == "Control Group", "Propensity.Score"] 

NewMaster.One[,"Sample.Weight"] = NewMaster.One[,"Propensity.Weight"]/sum(NewMaster.One[,"Propensity.Weight"])

# initialize a counter for scrolling through IDs
C=1

# cycle through every observation in the dataset
for (i in 1:dim(NewMaster)[1]){
  
  # if the ID for the current observation isn't equal to the ID for the previous ID, 
  # increase the counter by one to move to the next ID
  if (NewMaster[i,"ID"] != NewMasterIDs[C] & C != (length(NewMasterIDs))){
    
    C = C+1
    
  }
  
  # fill in the propensity score
  NewMaster[i,"Propensity.Score"] = NewMaster.One[NewMaster.One[,"ID"] == NewMasterIDs[C], "Propensity.Score"]
  
  NewMaster[i,"Propensity.Weight"] = NewMaster.One[NewMaster.One[,"ID"] == NewMasterIDs[C], "Propensity.Weight"]
  
  NewMaster[i,"Sample.Weight"] = NewMaster.One[NewMaster.One[,"ID"] == NewMasterIDs[C], "Sample.Weight"]
}

# create datasets with one observation for each group
NewMaster.One.Study = NewMaster.One[NewMaster.One[, "Group"] == "Study Group", ]
NewMaster.One.Control = NewMaster.One[NewMaster.One[, "Group"] == "Control Group", ]

