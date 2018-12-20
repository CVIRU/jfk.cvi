#################################Program Description################################
#Name: upload data                                                                 #
#Author: Traymon Beavers                                                           #
#Date Created: 7/21/2017                                                           #
#Date Updated: 10/12/2018                                                          #
#Purpose: To upload and reconfigure data from the stroke rehabilitation program;   #
#         6th round of data received                                               #
####################################################################################

# load necessary packages
library(knitr)

# Upload data ####

# upload Data in R
OldMaster = read.csv("data/DEID_All6.csv")

# check how many patients are in the original dataset
length(unique(OldMaster[, "ID"]))
# As of 6/7/2018 783 patients

# delete extraneous rownames variable
OldMaster = OldMaster[, -1]

# extract IDs of patients that were pregnant
preg.IDs = unique(OldMaster[OldMaster[, "Current.pregnancy"] == TRUE, "ID"])

# delete the patients that were pregnant at the time of the study
OldMaster = OldMaster[OldMaster[, "Current.pregnancy"] == FALSE, ]

# check how many patients are left in the original dataset
length(unique(OldMaster[, "ID"]))
# As of 6/7/2018 780 patients remain after removing those that were pregnant

# change all unknown hispanic ethnicity values to No
OldMaster[OldMaster[, "Hispanic.Ethnicity"] == "Unknown", "Hispanic.Ethnicity"] = "No"
OldMaster = droplevels(OldMaster)

# rename dataset
Master = OldMaster

# extract IDs of patients in "other" group
other.IDs = unique(Master[Master[,"Group"] == "Other","ID"])

# extract IDs of patients in "no CVG" group
noCVG.IDs = unique(Master[Master[,"Group"] == "Study - No CVG","ID"])

# Rename races other than black or white as "Other" ####

# rename Description variable as Race
colnames(Master)[10] = "Race"

# create the New.Race variable
Master[Master[, "Race"] == "BLACK OR AFRICAN AMERICAN", "New.Race"] = "Black"

Master[Master[, "Race"] == "WHITE", "New.Race"] = "White"

Master[Master[, "Race"] != "BLACK OR AFRICAN AMERICAN" & 
         Master[,"Race"] != "WHITE", "New.Race"] = "Other"

# factor the New.Race variable
Master[, "New.Race"] = factor(Master[,"New.Race"],
                              levels = c("White",
                                         "Black",
                                         "Other"))

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
  
  for (j in c("Height",
              "TotNoOfMod_HighRisk",
              "MoCA.Score",
              "PT.Int.Treat.Minutes",
              "OT.Int.Treat.Minutes",
              "ST.Int.Treat.Minutes",
              "Cardiovascular.Group.Met.Mins")){
    
    if(Master[i, j] == 9999
       | Master[i, j] == 8888
       | is.na(Master[i, j]) == 1){
      
      Master[i, j] = NA
      
    }
    
  }
  
}

# make sure the all missing values are consistent
# for (i in 1:ncol(Master)){
#   
#   if (class(Master[, i]) %in% c("numeric",
#                                "integer")){
#     
#     range.check = range(Master[, i], 
#                         na.rm = TRUE)
#     
#     if (range.check[2] > 1000){
#       
#       print(colnames(Master)[i])
#       
#       print(range.check)
#       
#     }
#     
#   }
#   
# }

# Create a new dataset with only patients that eventually landed in relevant groups ####

# create vector only containing patients who landed in the study group
StudyGroupIDs = unique(Master[Master[, "Group"] == "Study Group", "ID"])

# check how many patients are in the study group
length(StudyGroupIDs)
# As of 6/7/2018 136

# create a dataset only containing these patients
Master.Study = Master[Master[, "ID"] %in% StudyGroupIDs, ]

# create vector only containing patients who landed in the control group
ControlGroupIDs = unique(Master[Master[, "Group"] == "Control Group", "ID"])

# check how many patients are in the control group
length(ControlGroupIDs)
# As of 6/7/2018 473

# produce table of patients in each group
group.check.table = data.frame(Group = c("Pregnant",
                                         "Study",
                                         "Control",
                                         "Other",
                                         "No CVG"),
                               Number = c(length(preg.IDs),
                                          length(StudyGroupIDs),
                                          length(ControlGroupIDs),
                                          length(other.IDs),
                                          length(noCVG.IDs)))

group.check.table[,"Group"] = as.character(group.check.table[,"Group"])

group.check.table[6,] = c("None",
                          length(unique(Master[, "ID"])) - sum(group.check.table[2:5, "Number"]))

group.check.table[,"Number"] = as.numeric(group.check.table[,"Number"])

kable(group.check.table)

#   |Group    | Number|
#   |:--------|------:|
#   |Pregnant |      3|
#   |Study    |    136|
#   |Control  |    473|
#   |Other    |     71|
#   |No CVG   |     14|
#   |None     |     86|

sum(group.check.table[,2])

# create a dataset only containing these patients
Master.Control = Master[Master[, "ID"] %in% ControlGroupIDs, ]

# combine both treatment groups back together
NewMaster = rbind.data.frame(Master.Study, Master.Control)

# order the dataset first by the patient ID and then by their visit number
NewMaster = NewMaster[order(NewMaster[, "ID"], NewMaster[, "DaysId"]), ]

# delete patients that somehow landed in both groups
# NewMaster = NewMaster[-which(NewMaster[, "ID"] %in% intersect(StudyGroupIDs, ControlGroupIDs)), ]
# As of 6/7/2018 there are no patients in both groups

# delete the patients with NO CVG
# NOCVGIDs = unique(NewMaster[NewMaster[, "CVG.Participant_Descr"] == "NO CVG", "ID" ])
# NewMaster = NewMaster[!(NewMaster[, "ID"] %in% NOCVGIDs), ]
# As of 6/7/2018 there are no patients in CVG group with NO CVG

# Create variables for days after assignment to group and important scores for each functional outcome ####

# create a vector containing each ID in the dataset created above
NewMasterIDs = unique(NewMaster[, "ID"])

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
C = 1

# cycle through every observation in the dataset
for (i in 1:dim(NewMaster)[1]){
  
  # if the ID for the current observation isn't equal to the ID for the previous ID, 
  # increase the counter by one to move to the next ID
  if (NewMaster[i, "ID"] != NewMasterIDs[C] & 
      C != (length(NewMasterIDs))){
    
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

  NewMaster[i, "MRS.Baseline"] = NewMaster[NewMaster[, "DaysId"] == 3 & NewMaster[, "ID"] == NewMasterIDs[C], "ModRankinScore"]

}

# calculate the number of days after group assignement for each observation
NewMaster[, "Days.After.Assignment"] =
  NewMaster[, "Days_afterOnset"] - NewMaster[, "Days.After.At.Assignment"]

# drop unused levels
NewMaster = droplevels(NewMaster)

# Create a dataset with one observation for each patient ####
NewMaster.One = NewMaster[NewMaster[, "DaysId"] == 3, ]



# make sure all patients in NewMaster show up in NewMaster.One
length(unique(NewMaster[, "ID"]))
length(unique(NewMaster.One[, "ID"]))
# As of 6/7/2018 both have 609 patients

# check maximum follow up times for each patient in each group
not.deceased = NewMaster.One[!NewMaster.One[,"Deceased_Y.N"], "ID"]

follow.up.vec.study = max(NewMaster[NewMaster[, "ID"] == intersect(StudyGroupIDs, not.deceased)[1], "Days_afterOnset"])

for (i in 2:length(intersect(StudyGroupIDs, not.deceased))){
  
  follow.up.vec.study = c(follow.up.vec.study,
                    max(NewMaster[NewMaster[, "ID"] == intersect(StudyGroupIDs, not.deceased)[i], "Days_afterOnset"],
                        na.rm = TRUE))
  
}

min(follow.up.vec.study)
# 108

median(follow.up.vec.study)
# 365

follow.up.vec.control = max(NewMaster[NewMaster[, "ID"] == intersect(ControlGroupIDs, not.deceased)[1], "Days_afterOnset"])

for (i in 2:length(intersect(ControlGroupIDs, not.deceased))){
  
  follow.up.vec.control = c(follow.up.vec.control,
                    max(NewMaster[NewMaster[, "ID"] == intersect(ControlGroupIDs, not.deceased)[i], "Days_afterOnset"],
                        na.rm = TRUE))
  
}

min(follow.up.vec.control)
#91

median(follow.up.vec.control)
#355

follow.up.vec.all = max(NewMaster[NewMaster[, "ID"] == NewMasterIDs[1], "Days_afterOnset"])

for (i in 2:length(NewMasterIDs)){
  
  follow.up.vec.all = c(follow.up.vec.all,
                            max(NewMaster[NewMaster[, "ID"] == NewMasterIDs[i], "Days_afterOnset"],
                                na.rm = TRUE))
  
}

min(follow.up.vec.all)
# 91

median(follow.up.vec.all)
# 363

max(follow.up.vec.all)
# 422


# create vectors for easy reference to each functional outcome
ScoreName = c("Mobility", 
              "Activity", 
              "Cognitive")

ScoreVarName = c("PT.AM.PAC.Basic.Mobility.Score", 
                 "OT.AM.Daily.Activity.Score", 
                 "ST.AM.Applied.Cogn.Score")

# Create a variable for survival time ####

NewMaster.One[, "Stroke.Date.Month"] = NA
NewMaster.One[, "Stroke.Date.Day"] = NA
NewMaster.One[, "Stroke.Date.Year"] = NA
NewMaster.One[, "New.Date.of.Stroke"] = NA
NewMaster.One[, "Survival.Time"] = NA

# cycle through all patients
for (i in 1:nrow(NewMaster.One)){
  
  # split the dates into their three components
  NewMaster.One[i, "Stroke.Date.Month"] = strsplit(paste(NewMaster.One[i, "Date.of.Stroke"]), split = "/")[[1]][1]
  
  # check if the current month only has one character
  if (nchar(NewMaster.One[i, "Stroke.Date.Month"]) == 1){
    
    # if so, properly format it
    NewMaster.One[i, "Stroke.Date.Month"] = paste("0",
                                                  NewMaster.One[i, "Stroke.Date.Month"],
                                                  sep = "")
    
  }
  
  NewMaster.One[i, "Stroke.Date.Day"] = strsplit(paste(NewMaster.One[i, "Date.of.Stroke"]), split = "/")[[1]][2]
  
  NewMaster.One[i, "Stroke.Date.Year"] = strsplit(paste(NewMaster.One[i, "Date.of.Stroke"]), split = "/")[[1]][3]
  
  NewMaster.One[i, "New.Date.of.Stroke"] = paste(NewMaster.One[i, "Stroke.Date.Month"],
                                                 NewMaster.One[i, "Stroke.Date.Day"],
                                                 NewMaster.One[i, "Stroke.Date.Year"],
                                                 sep = "/")
  
}

NewMaster.One[, "New.Date.of.Stroke"] = as.Date(NewMaster.One[, "New.Date.of.Stroke"],
                                                format = "%m/%d/%Y")

NewMaster.One[, "Deceased_Dt"] = as.Date(NewMaster.One[, "Deceased_Dt"],
                                         format = "%m/%d/%Y")

NewMaster.One[NewMaster.One[, "Deceased_Y.N"] == TRUE, "Survival.Time"] = 
  difftime(NewMaster.One[NewMaster.One[, "Deceased_Y.N"] == TRUE, "Deceased_Dt"],
           NewMaster.One[NewMaster.One[, "Deceased_Y.N"] == TRUE, "New.Date.of.Stroke"],
           units = "days")

# create datasets with one observation for each group
NewMaster.One.Study = NewMaster.One[NewMaster.One[, "Group"] == "Study Group", ]
NewMaster.One.Control = NewMaster.One[NewMaster.One[, "Group"] == "Control Group", ]

# fill in survival time for all nondeceased patients
NewMaster.One.Study[NewMaster.One.Study[, "Deceased_Y.N"] == FALSE, "Survival.Time"] = 
  max(NewMaster.One.Study[NewMaster.One.Study[, "Deceased_Y.N"] == TRUE, "Survival.Time"], na.rm = TRUE) + 1

NewMaster.One.Control[NewMaster.One.Control[, "Deceased_Y.N"] == FALSE, "Survival.Time"] = 
  max(NewMaster.One.Control[NewMaster.One.Control[, "Deceased_Y.N"] == TRUE &
                              NewMaster.One.Control[, "Deceased_Dt"] != "9999-09-09", "Survival.Time"], na.rm = TRUE) + 1

# recombine the datasets
NewMaster.One = rbind.data.frame(NewMaster.One.Study,
                                 NewMaster.One.Control)

# order dataset by ID and visit number
NewMaster.One = NewMaster.One[order(NewMaster.One[, "ID"], 
                                    NewMaster.One[, "DaysId"]), ]

# place an NA in survival time where the deceased date was missing
NewMaster.One[NewMaster.One[, "Deceased_Y.N"] == TRUE &
                NewMaster.One[, "Deceased_Dt"] == "9999-09-09", "Survival.Time"] = NA

# drop unused levels
NewMaster.One = droplevels(NewMaster.One)

# Remove unnecessary values ####
rm(Master,
   Master.Control,
   Master.Study,
   OldMaster,
   C,
   i,
   j,
   group.check.table,
   follow.up.vec.study,
   follow.up.vec.control,
   follow.up.vec.all,
   not.deceased,
   other.IDs,
   preg.IDs,
   noCVG.IDs)