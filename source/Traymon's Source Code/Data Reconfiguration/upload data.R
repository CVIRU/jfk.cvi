#################################Program Description################################
#Name: upload data                                                                 #
#Author: Traymon Beavers                                                           #
#Date Created: 7/21/2017                                                           #
#Date Updated: 3/7/2018                                                            #
#Purpose: To upload and reconfigure data from the stroke rehabilitation program;   #
#         6th round of data received                                               #
####################################################################################

# Upload data ####

# upload Data in R
OldMaster = read.csv("data/DEID_All6.csv")

# delete extraneous varibale
OldMaster = OldMaster[, -1]

# delete the patients that were pregnant at the time of the study
OldMaster = OldMaster[OldMaster[, "Current.pregnancy"] == FALSE, ]

# change all unknown hispanic ethnicity values to No
OldMaster[OldMaster[, "Hispanic.Ethnicity"] == "Unknown", "Hispanic.Ethnicity"] = "No"
OldMaster = droplevels(OldMaster)

# rename dataset
Master = OldMaster

# Rename races other than black or white as "Other" ####

# rename Description variable as Race
colnames(Master)[10] = "Race"

Master[Master[, "Race"] == "BLACK OR AFRICAN AMERICAN", "New.Race"] = "Black"

Master[Master[, "Race"] == "WHITE", "New.Race"] = "White"

Master[Master[, "Race"] != "BLACK OR AFRICAN AMERICAN" & 
         Master[,"Race"] != "WHITE", "New.Race"] = "Other"

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
NewMaster = rbind.data.frame(Master.Study, Master.Control)

# order the dataset first by the patient ID and then by their visit number
NewMaster = NewMaster[order(NewMaster[, "ID"], NewMaster[, "DaysId"]), ]

# delete patients that somehow landed in both groups
# NewMaster = NewMaster[-which(NewMaster[, "ID"] %in% intersect(StudyGroupIDs, ControlGroupIDs)), ]
# As of 12/28/2017 there are no patients in both groups

# delete the patients with NO CVG
# NOCVGIDs = unique(NewMaster[NewMaster[, "CVG.Participant_Descr"] == "NO CVG", "ID" ])
# NewMaster = NewMaster[!(NewMaster[, "ID"] %in% NOCVGIDs), ]
# As of 12/28/2017 this no longer necessary

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

# Fill in the blanks for CVG score and remove certain patients from the dataset ####

# AllStudyIDs = NewMaster[NewMaster[,"Group"] == "Study Group" &
#                           NewMaster[,"DaysId"] == 3, "ID"]
# 
# CVGIDs = NewMaster[NewMaster[,"CVG.Participant_Descr"] != "" &
#                      NewMaster[,"DaysId"] == 11, "ID"]
# 
# Blank.CVGIDs = setdiff(AllStudyIDs,CVGIDs)
# 
# CVG.Corrections = read.csv("data/CVG Corrections.csv")[1:19,]
# 
# CVG.Corrections[12:13,] = CVG.Corrections[13:12,]
# 
# CVG.Corrections[CVG.Corrections[,"Education.Level"] == 9999, "Education.Level"] = NA
# 
# CVG.Corrections = droplevels(CVG.Corrections)
# 
# colnames(CVG.Corrections)[7:14] = c("CVG.Participant_Descr",
#                                     "CVG.Freq",
#                                     "CVG.Total",
#                                     "CVG.Baseline",
#                                     "CVG.9.Met.Minutes",
#                                     "CVG.18.Met.Minutes",
#                                     "CVG.27.Met.Minutes",
#                                     "CVG.36.Met.Minutes")
# 
# for.binding = droplevels(NewMaster[NewMaster[,"ID"] %in% Blank.CVGIDs &
#                                      NewMaster[,"DaysId"] == 3, ])
# 
# for.binding = for.binding[order(for.binding[,"Gender"],
#                                 for.binding[,"Age"]), ]
# 
# Age.list = intersect(for.binding[, c("Age")],
#                      CVG.Corrections[, c("Age")])
# 
# View(for.binding[(for.binding[,"Age"] %in% Age.list),
#                  c("Age",
#                    "Gender",
#                    "Hispanic.Ethnicity",
#                    "Race",
#                    "Health.Insurance.Name",
#                    "Education.Level")])
# 
# View(CVG.Corrections[(CVG.Corrections[,"Age"] %in% Age.list),
#                      c("Age",
#                        "Gender",
#                        "Hispanic.Ethnicity",
#                        "Race",
#                        "Health.Insurance.Name",
#                        "Education.Level")])
# 
# for.binding[(for.binding[,"Age"] %in% Age.list),
#                  c("Age",
#                    "Gender",
#                    "Hispanic.Ethnicity",
#                    "Race",
#                    "Health.Insurance.Name",
#                    "Education.Level")] == CVG.Corrections[(CVG.Corrections[,"Age"] %in% Age.list),
#                      c("Age",
#                        "Gender",
#                        "Hispanic.Ethnicity",
#                        "Race",
#                        "Health.Insurance.Name",
#                        "Education.Level")]
# 
# for.binding = for.binding[(for.binding[,"Age"] %in% Age.list), ]
# 
# for.binding[, c("CVG.Participant_Descr",
#                 "CVG.Freq",
#                 "CVG.Total",
#                 "CVG.Baseline",
#                 "CVG.9.Met.Minutes",
#                 "CVG.18.Met.Minutes",
#                 "CVG.27.Met.Minutes",
#                 "CVG.36.Met.Minutes")] = CVG.Corrections[(CVG.Corrections[,"Age"] %in% Age.list), 
#                                                          c("CVG.Participant_Descr",
#                                                            "CVG.Freq",
#                                                            "CVG.Total",
#                                                            "CVG.Baseline",
#                                                            "CVG.9.Met.Minutes",
#                                                            "CVG.18.Met.Minutes",
#                                                            "CVG.27.Met.Minutes",
#                                                            "CVG.36.Met.Minutes")]
# 
# for.binding[, "DaysId"] = 11
# 
# for.binding[, c(72:103)] = NA
# 
# NewMaster = rbind.data.frame(NewMaster, for.binding)
# 
# NewMaster = NewMaster[order(NewMaster[,"ID"], NewMaster[,"DaysId"]), ]
# 
# Remove.IDs = unique(NewMaster[NewMaster[, "CVG.Participant_Descr"] == "NO CVG" |
#                                 NewMaster[, "CVG.Participant_Descr"] == "OTHER", "ID"])
# 
# NewMaster = NewMaster[NewMaster[,"ID"] != 555 & 
#                         !(NewMaster[,"ID"] %in% Remove.IDs), ]
# As of 12/18/2018 may not be necessary

# Create a dataset with one observation for each patient ####
NewMaster.One = NewMaster[NewMaster[, "DaysId"] == 3, ]

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

NewMaster.One = NewMaster.One[order(NewMaster.One[, "ID"], 
                                    NewMaster.One[, "DaysId"]), ]

# place an NA in survival time where the deceased date was missing
NewMaster.One[NewMaster.One[, "Deceased_Y.N"] == TRUE &
                NewMaster.One[, "Deceased_Dt"] == "9999-09-09", "Survival.Time"] = NA

# drop unused levels
NewMaster.One = droplevels(NewMaster.One)

# Create a variable for propensity score ####
# 
# # create a vector containing each ID in the dataset created above
# NewMasterIDs = unique(NewMaster[,"ID"])
# 
# # initialize the variable for every propensity score
# NewMaster.One[,"Propensity.Score"] = 0
# 
# NewMaster.One[,"Propensity.Weight"] = 0
# 
# NewMaster.One[, "Group"] = droplevels(NewMaster.One[, "Group"])
# 
# NewMaster.One[,"Group"] = relevel(NewMaster.One[,"Group"], ref = "Study Group")
# 
# Propensity.Variables = colnames(NewMaster.One)[c(2:3,8:10,18:20,23,30,37:44,67,116:121)]
# 
# Propensity.Variables2 = colnames(NewMaster.One)[c(2:3,8:10,18:20,23,30,37:44,46:68,116:121)]
# 
# fmla = as.formula(paste("Group ~ ", paste(Propensity.Variables, collapse="+")))
# 
# fmla2 = as.formula(paste("Group ~ ", paste(Propensity.Variables2, collapse="+")))
# 
# # calculate the propensity score for each patient
# glm.out = glm(formula = fmla,
#               family = binomial(logit),
#               data = NewMaster.One[,c("Group", Propensity.Variables)])
# 
# NewMaster.One[,"Propensity.Score"] = glm.out$fitted.values
# 
# NewMaster.One[NewMaster.One[, "Group"] == "Study Group", "Propensity.Weight"] =
#   1/(1-NewMaster.One[NewMaster.One[, "Group"] == "Study Group", "Propensity.Score"])
# 
# NewMaster.One[NewMaster.One[, "Group"] == "Control Group", "Propensity.Weight"] =
#   1/NewMaster.One[NewMaster.One[, "Group"] == "Control Group", "Propensity.Score"]
# 
# NewMaster.One[,"Sample.Weight"] = NewMaster.One[,"Propensity.Weight"]/sum(NewMaster.One[,"Propensity.Weight"])
# 
# # initialize a counter for scrolling through IDs
# C=1
# 
# # cycle through every observation in the dataset
# for (i in 1:dim(NewMaster)[1]){
#   
#   # if the ID for the current observation isn't equal to the ID for the previous ID,
#   # increase the counter by one to move to the next ID
#   if (NewMaster[i,"ID"] != NewMasterIDs[C] & C != (length(NewMasterIDs))){
#     
#     C = C+1
#     
#   }
#   
#   # fill in the propensity score
#   NewMaster[i,"Propensity.Score"] = NewMaster.One[NewMaster.One[,"ID"] == NewMasterIDs[C], "Propensity.Score"]
#   
#   NewMaster[i,"Propensity.Weight"] = NewMaster.One[NewMaster.One[,"ID"] == NewMasterIDs[C], "Propensity.Weight"]
#   
#   NewMaster[i,"Sample.Weight"] = NewMaster.One[NewMaster.One[,"ID"] == NewMasterIDs[C], "Sample.Weight"]
# }
#
# As of 2/28/2017 this is no longer needed
# Remove unnecessary values ####
rm(Master,
   Master.Control,
   Master.Study,
   OldMaster,
   C,
   i,
   j)