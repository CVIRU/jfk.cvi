#################################Program Description################################
#Name: Matching and Analyzing (Round 3)                                            #
#Author: Traymon Beavers                                                           #
#Date Created: 6/29/2017                                                           #
#Date Updated: 7/3/2017                                                            #
#Purpose: To perform analysis on the stroke recovery program data by matching      #
#         patients in the more intensive group with patients in the less intensive #
#         group and then conducting various statistical procedures with respect to #
#         the matched pairs ; 3rd round of data received                           #
#Functions: matching, follow.up.analysis, lmer.analysis, slope.analysis            #
#                                                                                  #
####################################################################################

# Notes about data ####
### For some pairs of DaysId (i,j) where j>i, the days after onset for DaysID==i
### is greater than the days after onset for DaysID==j
### Stroke Severity Number is missing for 126 patients
### ICH SSN ID 239 is 7
### ID 214 has DIS FIM Cogn 85 and DIS FIM Total 158


# 1. Upload data ####

# upload Data in R
OldMaster = read.csv("Data/DEID_All3.csv")

# delete extraneous varibale
OldMaster = OldMaster[, -1]

# only non-missing observations 
# Note: need to check DEID and Combine Code to see why these are being introduced 
# in the first place
Master = OldMaster[is.na(OldMaster[,"ID"]) == 0, ]


# rename races other than black or white as "Other"
Master[Master[,"Race"] == 3, "New.Race"] = "Black"

Master[Master[,"Race"] == 5, "New.Race"] = "White"

Master[Master[,"Race"] != 3 & Master[,"Race"] != 5, "New.Race"]="Other"


# Create consistent missing value indicators for each functional outcome ####

# cycle through every observation in the master dataset
for (i in 1:dim(Master)[1]){
  
  # change any missing mobility scores to NA
  if(Master[i,"PT.AM.PAC.Basic.Mobility.Score"] == 9999
     | Master[i,"PT.AM.PAC.Basic.Mobility.Score"] == 8888
     | is.na(Master[i,"PT.AM.PAC.Basic.Mobility.Score"]) == 1){
    
    Master[i,"PT.AM.PAC.Basic.Mobility.Score"] = NA
    
  }
  
  # change any missing activity scores to NA
  if(Master[i,"OT.AM.Daily.Activity.Score"] == 9999
     | Master[i,"OT.AM.Daily.Activity.Score"] == 8888
     | Master[i,"OT.AM.Daily.Activity.Score"] == 9909
     | is.na(Master[i,"OT.AM.Daily.Activity.Score"]) == 1){
    
    Master[i,"OT.AM.Daily.Activity.Score"] = NA
    
  }
  
  # change any missing cognitive scores to NA
  if(Master[i,"ST.AM.Applied.Cogn.Score"] == 9999
     | Master[i,"ST.AM.Applied.Cogn.Score"] == 8888
     | is.na(Master[i,"ST.AM.Applied.Cogn.Score"]) == 1){
    
    Master[i,"ST.AM.Applied.Cogn.Score"] = NA
    
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

# initialize the variable for every observation's score when they were assigned to their treatment group
NewMaster[, "Baseline.Mobility"] = rep(NA,dim(NewMaster)[1])
NewMaster[, "Baseline.Activity"] = rep(NA,dim(NewMaster)[1])
NewMaster[, "Baseline.Cognitive"] = rep(NA,dim(NewMaster)[1])


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
      NewMaster[NewMaster[,"DaysId"] == 3 & NewMaster[,"ID"] == NewMasterIDs[C], "Days_afterOnset"]

  # calculate score when they were first admitted for each outcome for current observation
  NewMaster[i, "Admission.Mobility"] = NewMaster[NewMaster[, "DaysId"] == 1 & NewMaster[, "ID"] == NewMasterIDs[C], "PT.AM.PAC.Basic.Mobility.Score"]
  NewMaster[i, "Admission.Activity"] = NewMaster[NewMaster[, "DaysId"] == 1 & NewMaster[, "ID"] == NewMasterIDs[C], "OT.AM.Daily.Activity.Score"]
  NewMaster[i, "Admission.Cognitive"] = NewMaster[NewMaster[, "DaysId"] == 1 & NewMaster[, "ID"] == NewMasterIDs[C], "ST.AM.Applied.Cogn.Score"]

  # calculate score when they were discharged for each outcome for current observation  
  NewMaster[i, "Discharge.Mobility"] = NewMaster[NewMaster[, "DaysId"] == 2 & NewMaster[, "ID"] == NewMasterIDs[C], "PT.AM.PAC.Basic.Mobility.Score"]
  NewMaster[i, "Discharge.Activity"] = NewMaster[NewMaster[, "DaysId"] == 2 & NewMaster[, "ID"] == NewMasterIDs[C], "OT.AM.Daily.Activity.Score"]
  NewMaster[i, "Discharge.Cognitive"] = NewMaster[NewMaster[, "DaysId"] == 2 & NewMaster[, "ID"] == NewMasterIDs[C], "ST.AM.Applied.Cogn.Score"]

  # calculate score when they were assigned to their treatment group for each outcome for current observation
  NewMaster[i, "Baseline.Mobility"] = NewMaster[NewMaster[, "DaysId"] == 3 & NewMaster[, "ID"] == NewMasterIDs[C], "PT.AM.PAC.Basic.Mobility.Score"]
  NewMaster[i, "Baseline.Activity"] = NewMaster[NewMaster[, "DaysId"] == 3 & NewMaster[, "ID"] == NewMasterIDs[C], "OT.AM.Daily.Activity.Score"]
  NewMaster[i, "Baseline.Cognitive"] = NewMaster[NewMaster[, "DaysId"] == 3 & NewMaster[, "ID"] == NewMasterIDs[C], "ST.AM.Applied.Cogn.Score"]
    
}
  
# calculate the number of days after group assignement for each observation
NewMaster[,"Days.After.Assignment"] =
  NewMaster[,"Days_afterOnset"] - NewMaster[,"Days.After.At.Assignment"]

# create a dataset with one observation for each patient
NewMaster.One = NewMaster[NewMaster[,"DaysId"] == 3,]

# create datasets with one observation for each group
NewMaster.One.Study = NewMaster.One[NewMaster.One[, "Group"] == "Study Group", ]
NewMaster.One.Control = NewMaster.One[NewMaster.One[, "Group"] == "Control Group", ]

# initialize vectors for easy reference to each functional outcome
ScoreName=c("Mobility", "Activity", "Cognitive")
ScoreVarName=c("PT.AM.PAC.Basic.Mobility.Score", "OT.AM.Daily.Activity.Score", "ST.AM.Applied.Cogn.Score")


# # 2. Check that the data is in the correct ranges ####
# 
# # check education level range
# NewMaster[which((NewMaster[,"Education.Level"] > 40 | NewMaster[,"Education.Level"] < 0) & NewMaster[,"Education.Level"] != 9999), "Education.Level"]
# 
# # check Stroke Severity Numbers
# NewMaster[which((NewMaster[, "ACHosp.Stroke.Severity.Number_NIHSS"] > 42 | NewMaster[, "ACHosp.Stroke.Severity.Number_NIHSS"]<0) &
#                   NewMaster[, "ACHosp.Stroke.Severity.Number_NIHSS"] != 9999), "ACHosp.Stroke.Severity.Number_NIHSS"]
# 
# NewMaster[which((NewMaster[, "ACHosp.Stroke.Severity.Number_ICH"] > 6 | NewMaster[, "ACHosp.Stroke.Severity.Number_ICH"]<0) &
#                   NewMaster[, "ACHosp.Stroke.Severity.Number_ICH"] != 9999), c("ID", "ACHosp.Stroke.Severity.Number_ICH")]
# 
# NewMaster[which((NewMaster[, "ACHosp.Stroke.Severity.Number_HH"] > 5 | NewMaster[, "ACHosp.Stroke.Severity.Number_HH"]<0) &
#                   NewMaster[, "ACHosp.Stroke.Severity.Number_HH"] != 9999), c("ID", "ACHosp.Stroke.Severity.Number_HH")]
# 
# # check Facility Adjustor
# NewMaster[which((NewMaster[, "ARHosp.JRI.Facility.Adjustor"] > 110 | NewMaster[, "ARHosp.JRI.Facility.Adjustor"] < 101) &
#                   NewMaster[, "ARHosp.JRI.Facility.Adjustor"] != 9999), c("ID", "ARHosp.JRI.Facility.Adjustor")]
# 
# # check FIM Scores
# NewMaster[which((NewMaster[, "ARHosp.JRI.Adm.FIM.Motor"] > 91 | NewMaster[, "ARHosp.JRI.Adm.FIM.Motor"] < 12) &
#                   NewMaster[, "ARHosp.JRI.Adm.FIM.Motor"] != 9999), "ARHosp.JRI.Adm.FIM.Motor"]
# 
# NewMaster[which((NewMaster[, "ARHosp.JRI.Adm.FIM.Cogn"] > 35 | NewMaster[, "ARHosp.JRI.Adm.FIM.Cogn"] < 0) &
#                   NewMaster[, "ARHosp.JRI.Adm.FIM.Cogn"] != 9999), "ARHosp.JRI.Adm.FIM.Cogn"]
# 
# NewMaster[which((NewMaster[, "ARHosp.JRI.Adm.FIM.Total"] > 126 | NewMaster[, "ARHosp.JRI.Adm.FIM.Total"] < 17) &
#                   NewMaster[, "ARHosp.JRI.Adm.FIM.Total"] != 9999), "ARHosp.JRI.Adm.FIM.Total"]
# 
# NewMaster[which((NewMaster[, "ARHosp.JRI.Dis.FIM.Motor"]>91 | NewMaster[, "ARHosp.JRI.Dis.FIM.Motor"] < 12) &
#                   NewMaster[, "ARHosp.JRI.Dis.FIM.Motor"] != 9999), "ARHosp.JRI.Dis.FIM.Motor"]
# 
# NewMaster[which((NewMaster[, "ARHosp.JRI.Dis.FIM.Cogn"] > 35 | NewMaster[, "ARHosp.JRI.Dis.FIM.Cogn"] < 0) &
#                   NewMaster[, "ARHosp.JRI.Dis.FIM.Cogn"] != 9999), c("ID", "ARHosp.JRI.Dis.FIM.Cogn")]
# 
# NewMaster[which((NewMaster[, "ARHosp.JRI.Dis.FIM.Total"] > 126 | NewMaster[, "ARHosp.JRI.Dis.FIM.Total"] < 17) &
#                   NewMaster[, "ARHosp.JRI.Dis.FIM.Total"] != 9999), c("ID", "ARHosp.JRI.Dis.FIM.Total")]
# 
# # check height, weight, BMI, SBPHTN, DBPHTN, Fast-Nonfasting LDL, and HbA1c
# NewMaster[which((NewMaster[, "Height"] > 240 | NewMaster[, "Height"] < 120) &
#                   NewMaster[, "Height"] != 9999 & NewMaster[, "Height"] != 8888),
#           c("ID", "Height") ]
# 
# NewMaster[which((NewMaster[, "Weight"] > 230 | NewMaster[, "Weight"] < 29) &
#                   NewMaster[, "Weight"] != 9999 & NewMaster[, "Weight"] != 8888),
#           c("ID", "Weight") ]
# 
# NewMaster[which((NewMaster[, "BMI"] > 60 | NewMaster[,"BMI"] < 10) &
#                   NewMaster[, "BMI"] != 9999 & NewMaster[,"BMI"] != 8888),
#           c("ID", "BMI") ]
# 
# NewMaster[which((NewMaster[, "SBP.HTN"] > 250 | NewMaster[, "SBP.HTN"] < 60) &
#                     NewMaster[, "SBP.HTN"] != 9999 & NewMaster[, "SBP.HTN"] != 8888),
#           c("ID", "SBP.HTN") ]
# 
# NewMaster[which((NewMaster[, "DBP.HTN"] > 160 | NewMaster[, "DBP.HTN"] < 20) &
#                   NewMaster[, "DBP.HTN"] != 9999 & NewMaster[, "DBP.HTN"] != 8888),
#           c("ID", "DBP.HTN") ]
# 
# NewMaster[which((NewMaster[, "Fast.Nonfasting.LDL"] > 300 | NewMaster[, "Fast.Nonfasting.LDL"] < 20) &
#                   NewMaster[, "Fast.Nonfasting.LDL"] != 9999 & NewMaster[, "Fast.Nonfasting.LDL"] != 8888),
#           c("ID", "Fast.Nonfasting.LDL") ]
# 
# NewMaster[which((NewMaster[, "HbA1c"] > 20 | NewMaster[, "HbA1c"] < 0) &
#                   NewMaster[, "HbA1c"] != 9999 & NewMaster[, "HbA1c"] != 8888),
#           c("ID", "HbA1c") ]
# 
# # check TotNoOfMod_HighRisk
# NewMaster[which((NewMaster[, "TotNoOfMod_HighRisk"] > 11 | NewMaster[, "TotNoOfMod_HighRisk"] < 0) &
#                   NewMaster[, "TotNoOfMod_HighRisk"] != 9999 & NewMaster[, "TotNoOfMod_HighRisk"] != 8888),
#           c("ID", "TotNoOfMod_HighRisk")]
# 
# # check MoCa score
# NewMaster[which((NewMaster[, "MoCA.Score"] > 30 | NewMaster[, "MoCA.Score"] < 0) &
#                   NewMaster[, "MoCA.Score"] != 9999 & NewMaster[, "MoCA.Score"] != 8888),
#           c("ID", "MoCA.Score")]
# 
# # check functional outcome scores and minutes
# NewMaster[which((NewMaster[, "ModRankinScore"] > 6 | NewMaster[, "ModRankinScore"] < 0) &
#                   NewMaster[, "ModRankinScore"] != 9999 & NewMaster[, "ModRankinScore"] != 8888),
#           c("ID", "ModRankinScore")]
# 
# NewMaster[which((NewMaster[, "PT.AM.PAC.Basic.Mobility.Score"] > 130 | NewMaster[, "PT.AM.PAC.Basic.Mobility.Score"] < -30) &
#                   NewMaster[,"PT.AM.PAC.Basic.Mobility.Score"] != 9999 & NewMaster[,"PT.AM.PAC.Basic.Mobility.Score"] != 8888),
#           c("ID", "PT.AM.PAC.Basic.Mobility.Score")]
# 
# NewMaster[which((NewMaster[, "OT.AM.Daily.Activity.Score"] > 130 | NewMaster[, "OT.AM.Daily.Activity.Score"] < -30) &
#                   NewMaster[, "OT.AM.Daily.Activity.Score"] != 9999 & NewMaster[, "OT.AM.Daily.Activity.Score"] != 8888),
#           c("ID", "OT.AM.Daily.Activity.Score")]
# 
# NewMaster[which((NewMaster[,"ST.AM.Applied.Cogn.Score"] > 130 | NewMaster[, "ST.AM.Applied.Cogn.Score"] < -30) &
#                   NewMaster[,"ST.AM.Applied.Cogn.Score"] != 9999 & NewMaster[, "ST.AM.Applied.Cogn.Score"] != 8888),
#           c("ID", "ST.AM.Applied.Cogn.Score")]
# 
# NewMaster[which((NewMaster[, "Cardiovascular.Group.SessionsPerWeek"] > 5 | NewMaster[, "Cardiovascular.Group.SessionsPerWeek"] < 0) &
#                   NewMaster[, "Cardiovascular.Group.SessionsPerWeek"] != 9999 & NewMaster[, "Cardiovascular.Group.SessionsPerWeek"] != 8888),
#           c("ID", "Cardiovascular.Group.SessionsPerWeek")]
# 
# NewMaster[which((NewMaster[, "Cardiovascular.Group.Tot.Sessions"] > 50 | NewMaster[, "Cardiovascular.Group.Tot.Sessions"] < 0) &
#                   NewMaster[, "Cardiovascular.Group.Tot.Sessions"] != 9999 & NewMaster[, "Cardiovascular.Group.Tot.Sessions"] != 8888),
#           c("ID", "Cardiovascular.Group.Tot.Sessions")]
# 
# NewMaster[which((NewMaster[, "Cardiovascular.Group.Intensity.Mets."] > 10 | NewMaster[, "Cardiovascular.Group.Intensity.Mets."] < 0) &
#                   NewMaster[, "Cardiovascular.Group.Intensity.Mets."] != 9999 & NewMaster[, "Cardiovascular.Group.Intensity.Mets."] != 8888),
#           c("ID", "Cardiovascular.Group.Intensity.Mets.")]
# 
# NewMaster[which((NewMaster[, "Cardiovascular.Group.Tot.Mins"] > 60 | NewMaster[, "Cardiovascular.Group.Tot.Mins"] < 0) &
#                   NewMaster[, "Cardiovascular.Group.Tot.Mins"] != 9999 & NewMaster[, "Cardiovascular.Group.Tot.Mins"] != 8888),
#           c("ID", "Cardiovascular.Group.Tot.Mins")]
# 
# NewMaster[which((NewMaster[, "Cardiovascular.Group.Met.Mins"] > 500 | NewMaster[, "Cardiovascular.Group.Met.Mins"] < 0) &
#                   NewMaster[, "Cardiovascular.Group.Met.Mins"] != 9999 & NewMaster[, "Cardiovascular.Group.Met.Mins"] != 8888),
#           c("ID", "Cardiovascular.Group.Met.Mins")]
# 
# NewMaster[which((NewMaster[, "CVG.Freq"] > 3 | NewMaster[, "CVG.Freq"] < 1) &
#                   NewMaster[, "CVG.Freq"] != 9999 & NewMaster[, "CVG.Freq"] != 8888),
#           c("ID", "CVG.Freq")]
# 
# NewMaster[which((NewMaster[, "CVG.Total"] > 36 | NewMaster[, "CVG.Total"] < 0) &
#                   NewMaster[, "CVG.Total"] != 9999 & NewMaster[, "CVG.Total"] != 8888),
#           c("ID", "CVG.Total")]

# 3. Interpolation ####

#intialize dataset with interpolated missing values
Interpolate.Master=NewMaster

# cycle through IDs in NewMaster
for (j in 1:length(NewMasterIDs)){
  
  # create a vector called DAYSID to store which follow ups are logged for which ID ####
  
  # initialize DAYSID
  DAYSID = 0
  
  # cycle through possible DaysId observations
  for (i in 1:8){
    
    # add this DaysId observation to the DAYSID vecotr if the current ID has it
    if(length(Interpolate.Master[Interpolate.Master[,"ID"] == NewMasterIDs[j] & 
                                 Interpolate.Master[,"DaysId"] == i, 
                                 "PT.AM.PAC.Basic.Mobility.Score"]) > 0){
      
      DAYSID=c(DAYSID, i)
      
    }
    
  }
  
  # delete the initial 0
  DAYSID = DAYSID[-1]
  
  # create a vector called first to store every DaysId for which the next DaysId has a missing observation ####
  
  # initialize first
  first = 0
  
  # cycle through the logged DaysIds for the given ID (first to penultimate)
  for (i in 1:(length(DAYSID) - 1)){
    
    # add this DaysId to the first vector check if the measurement for the current DaysId is 
    # nonmissing and the measurement for the next DaysId is missing
    if(is.na(Interpolate.Master[Interpolate.Master[, "ID"] == NewMasterIDs[j] & 
                                Interpolate.Master[, "DaysId"] == DAYSID[i], 
                                "PT.AM.PAC.Basic.Mobility.Score"]) == 0 & 
       is.na(Interpolate.Master[Interpolate.Master[, "ID"] == NewMasterIDs[j] &
                                Interpolate.Master[, "DaysId"] == DAYSID[i+1], 
                                "PT.AM.PAC.Basic.Mobility.Score"]) == 1){
      
      first=c(first, DAYSID[i])
      
    }
    
  }
  
  #delete the initial 0
  first = first[-1]
  
  # create a vector called last to store every DaysId for which the previous DaysId has a missing observation ####
  
  #initialize last
  last = 0
  
  #cycle through the logged DaysIds for the given ID (second to last)
  for (i in 2:length(DAYSID)){
    
    # add this DaysId to the last vector check if the measurement for the current DaysId is 
    # nonmissing and the measurement for the next DaysId is missing
    if(is.na(Interpolate.Master[Interpolate.Master[, "ID"] == NewMasterIDs[j] & 
                                Interpolate.Master[, "DaysId"] == DAYSID[i], 
                                "PT.AM.PAC.Basic.Mobility.Score"]) == 0 & 
       is.na(Interpolate.Master[Interpolate.Master[, "ID"] == NewMasterIDs[j] & 
                                Interpolate.Master[, "DaysId"] == DAYSID[i-1], 
                                "PT.AM.PAC.Basic.Mobility.Score"]) == 1){
      
      last=c(last, DAYSID[i])
      
    }
    
  }
  
  #delete the 0
  last=last[-1]
  
  # perform interpolation ####
  
  # cycle through first and last vectors
  for (k in 1:length(first)){
    
    # find the number of missing values between the nonmissing observation before
    # missing values occur and the nonmissing observation after they stop occurring
    Gap.Num = (which(DAYSID == last[k]) - which(DAYSID == first[k]))
    
    # check if there are any missing values to interpolate
    if (length(Gap.Num) != 0 & length(last[k] != 0)){
      
      
      # find the mobility score associated with the nonmissing observation after missing values stop occurring
      y2m = Interpolate.Master[Interpolate.Master[,"ID"] == NewMasterIDs[j] & 
                               Interpolate.Master[,"DaysId"] == DAYSID[which(DAYSID == last[k])], 
                               "PT.AM.PAC.Basic.Mobility.Score"]
      
      # find the mobility score associated with the nonmissing observation after missing values begin occurring
      y1m = Interpolate.Master[Interpolate.Master[,"ID"] == NewMasterIDs[j] & 
                               Interpolate.Master[,"DaysId"] == DAYSID[which(DAYSID == first[k])], 
                               "PT.AM.PAC.Basic.Mobility.Score"]
      
      # find the slope between these two points
      mobility.slope = (y2m - y1m)/Gap.Num
      
      # find the activity score associated with the nonmissing observation after missing values stop occurring
      y2a = Interpolate.Master[Interpolate.Master[,"ID"] == NewMasterIDs[j] & 
                               Interpolate.Master[,"DaysId"] == DAYSID[which(DAYSID == last[k])], 
                               "OT.AM.Daily.Activity.Score"]
      
      # find the activity score associated with the nonmissing observation after missing values stop occurring
      y1a = Interpolate.Master[Interpolate.Master[,"ID"] == NewMasterIDs[j] & 
                               Interpolate.Master[,"DaysId"] == DAYSID[which(DAYSID == first[k])], 
                               "OT.AM.Daily.Activity.Score"]
      
      # find the slope between these two points      
      activity.slope = (y2a - y1a)/Gap.Num
      
      # find the mobility score associated with the nonmissing observation after missing values stop occurring
      y2c = Interpolate.Master[Interpolate.Master[,"ID"] == NewMasterIDs[j] & 
                               Interpolate.Master[,"DaysId"] == DAYSID[which(DAYSID == last[k])], 
                               "ST.AM.Applied.Cogn.Score"]
      
      # find the mobility score associated with the nonmissing observation after missing values stop occurring
      y1c = Interpolate.Master[Interpolate.Master[,"ID"] == NewMasterIDs[j] & 
                               Interpolate.Master[,"DaysId"] == DAYSID[which(DAYSID == first[k])], 
                               "ST.AM.Applied.Cogn.Score"]
      
      # find the slope between these two points
      cognitive.slope = (y2c - y1c)/Gap.Num
      
      #cycle through the observations with missing values that can be interpolated
      for (i in 1:(Gap.Num - 1)){
        
        #replace missing values with the interpolated value
        Interpolate.Master[Interpolate.Master[,"ID"] == NewMasterIDs[j] & 
                           Interpolate.Master[,"DaysId"] == DAYSID[which(DAYSID == first[k]) + i], 
                           c("PT.AM.PAC.Basic.Mobility.Score", "OT.AM.Daily.Activity.Score", "ST.AM.Applied.Cogn.Score")]=
          c((y1m + i*mobility.slope), (y1a + i*activity.slope), (y1c + i*cognitive.slope))
      }
      
    }
    
  }
  
}

# Create a dataset that only contains one observation for each patient (Interpolated)
Interpolate.Master.One = Interpolate.Master[Interpolate.Master[,"DaysId"] == 3,] 

# check that the interpolation worked as intended
# View(NewMaster[,c("ID", "DaysId", "PT.AM.PAC.Basic.Mobility.Score",
#                   "OT.AM.Daily.Activity.Score" ,"ST.AM.Applied.Cogn.Score")])
# 
# View(Interpolate.Master[,c("ID", "DaysId", "PT.AM.PAC.Basic.Mobility.Score",
#                   "OT.AM.Daily.Activity.Score" ,"ST.AM.Applied.Cogn.Score")])

# 4. Matching function ####
#####################################Function#######################################
#Name: matching                                                                    #
#Author: Traymon Beavers                                                           #
#Date Created: 3/29/2017                                                           #
#Date Updated: 7/3/2017                                                            #
#Purpose: To match the data based on gender, race, type of stroke, age, baseline   #
#         functional outcome scores, propensity score, and facility adjustor number#
#Variables: AgeNum-width for age partial matching                                  #
#           BaselineMobNum-width for baseline mobility score partial matching      #
#           BaselineActNum-width for baseline activity score partial matching      #
#           BaselineCogNum-width for baseline cognitive score partial matching     #
#           PScoreNum-width for propensity score partial matching                  #
#           FacAdjNum-width for facility adjustor partial matching                 #
#                                                                                  #
####################################################################################

matching = function(AgeNum = 2, 
                    BaselineMobNum = 20, 
                    BaselineActNum = 20, 
                    BaselineCogNum = 20, 
                    PScoreNum = 1, 
                    FacAdjNum = 5){
  
  # create a data matrix for the characteristics to be used for matching for each 
  # patient in the study group
  Study.Characteristics = NewMaster.One[NewMaster.One[,"Group"] == "Study Group",
                                        c("ID", 
                                          "Age", 
                                          "Gender", 
                                          "New.Race", 
                                          "Baseline.Mobility", 
                                          "Baseline.Activity",
                                          "Baseline.Cognitive",
                                          "Type.of.Stroke",
                                          "ARHosp.JRI.Facility.Adjustor")]
  
  # initialize a progress bar to check the progress of the matching
  pb = txtProgressBar(min = 0, 
                      max = dim(NewMaster.One.Control)[1], 
                      initial = 0)
  
  # initialize a vector to contain the matches
  result=c(0,0,0)
  
  # begin the counter for pair ID
  D = 1
  
  # cycle through the control group observations
  for (i in 1:dim(NewMaster.One.Control)[1]){
    
    #update progress bar to reflect that the function is attempting to match the next control
    setTxtProgressBar(pb, i)
    
    #cycle through the study characteristics
    for (j in 1:dim(Study.Characteristics)[1]){
      
      # add the matched pair and their pair ID to the list of matches if matching criteria is satisfied 
      if (is.na(NewMaster.One.Control[i, "Baseline.Mobility"]) == 0 & 
          NewMaster.One.Control[i, "Age"] >= (Study.Characteristics[j, "Age"] - AgeNum) &
          NewMaster.One.Control[i, "Age"] <= (Study.Characteristics[j, "Age"] + AgeNum) &
          NewMaster.One.Control[i, "Baseline.Mobility"] >= (Study.Characteristics[j, "Baseline.Mobility"] - BaselineMobNum) &
          NewMaster.One.Control[i, "Baseline.Mobility"] <= (Study.Characteristics[j, "Baseline.Mobility"] + BaselineMobNum) &
          NewMaster.One.Control[i, "Baseline.Activity"] >= (Study.Characteristics[j, "Baseline.Activity"] - BaselineActNum) &
          NewMaster.One.Control[i, "Baseline.Activity"] <= (Study.Characteristics[j, "Baseline.Activity"] + BaselineActNum) &
          NewMaster.One.Control[i, "Baseline.Cognitive"] >= (Study.Characteristics[j, "Baseline.Cognitive"] - BaselineCogNum) &
          NewMaster.One.Control[i, "Baseline.Cognitive"] <= (Study.Characteristics[j, "Baseline.Cognitive"] + BaselineCogNum) &
          NewMaster.One.Control[i,"Gender"] == Study.Characteristics[j,"Gender"] &
          NewMaster.One.Control[i,"New.Race"] == Study.Characteristics[j, "New.Race"] &
          NewMaster.One.Control[i,"Type.of.Stroke"] == Study.Characteristics[j, "Type.of.Stroke"] &
          # NewMaster.One.Control[i,"Propensity.Score"] >= (Study.Characteristics[j,"Propensity.Score"] - PScoreNum) &
          # NewMaster.One.Control[i,"Propensity.Score"] <= (Study.Characteristics[j,"Propensity.Score"] + PScoreNum) &
          NewMaster.One.Control[i,"ARHosp.JRI.Facility.Adjustor"] >= (Study.Characteristics[j, "ARHosp.JRI.Facility.Adjustor"] - FacAdjNum) & 
          NewMaster.One.Control[i,"ARHosp.JRI.Facility.Adjustor"] <= (Study.Characteristics[j, "ARHosp.JRI.Facility.Adjustor"] + FacAdjNum)){
        
        result=rbind(result, 
                     c(D, Study.Characteristics[j,"ID"], 1), 
                     c(D, NewMaster.One.Control[i,"ID"], 0))
        
        # increase the pair ID counter
        D=D+1
        
      }
      
    }
    
  }
  
  # finish progress bar
  close(pb)
  
  # give names to what each column represents
  colnames(result)=c("PairID", "ID", "Group")
  
  # delete the initial 0's
  result=result[-1,]
  
  # output the resulting matches
  return(result)
  
}

#################################End Function#######################################

# Check function ####

matchrows=matching(AgeNum = 5,
                   BaselineMobNum = 25,
                   BaselineActNum = 25, 
                   BaselineCogNum = 25, 
                   PScoreNum = 1, 
                   FacAdjNum = 3)

# count the number of study patients matched
length(unique(matchrows[matchrows[,3] == 1, 2]))

# Check creation of unique (up to Study ID) one to one matches #### 

# find the seed that produces the set of one to one matches with the most unique controls and the most non-missing observations ####

### seed is 90

# # initialize vector to hold the number of non-missing observations this collection of matches has
# NA.Num = 0
# 
# # initialize vector to hold the number of unique control group patients this collection of matches has
# UC.Num = 0

# cycle through 1000 seeds
#for (k in 1:1000){

  # initialize the matrix to hold the set of one to one matches
  matchrow.final = c(0,0,0)
  
  # cycle through the unique study patients in the matches created 
  for (i in unique(matchrows[matchrows[,3]==1,2])){
    
    # set the seed
    set.seed(90)
    
    # count the number of control patients matched to this study patient
    N = length(which(matchrows[,2] == i))
    
    # randomly choose which control patient will be move to the one-to-one set of matches
    j = ceiling(runif(1, 0, N))
    
    # update the set of one to one matches to include the new match
    matchrow.final = rbind(matchrow.final,
                           matchrows[which(matchrows[,2] == i)[j], ],
                           matchrows[(which(matchrows[,2] == i)[j] + 1), ])
    
  }
  
# delete the initial 0's (begin indent)
matchrow.final = matchrow.final[-1,]

# # Count the number of non missing observations this set of one to one matches contains ####
# 
# # initialize the number of non missing observations
# Non.Missing.Num = 0
# 
# # cycle through the IDs in the set of one to one matches
# for (j in unique(matchrow.final[,2])){
# 
#   # update the number of non missing observations to include the number of non missing 
#   # observations this ID has
#   Non.Missing.Num = Non.Missing.Num + length(Interpolate.Master[Interpolate.Master[, "ID"] == j &
#                                                                 Interpolate.Master[, "DaysId"] >= 1 &
#                                                                 Interpolate.Master[, "DaysId"] <= 8,
#                                                                 "PT.AM.PAC.Basic.Mobility.Score"]) - sum(is.na(Interpolate.Master[Interpolate.Master[, "ID"] == j &
#                                                                                                               Interpolate.Master[, "DaysId"] >= 1 &
#                                                                                                               Interpolate.Master[, "DaysId"] <= 8,
#                                                                                                               "PT.AM.PAC.Basic.Mobility.Score"]))
# 
# 
# }
# (end indent)
#   # update A to include the number of non missing observations for this set of one to one matches
#   NA.Num = c(NA.Num, Non.Missing.Num)
# 
#   # update B to include the number of unique controls for this set of one to one matches
#   UC.Num = c(UC.Num, length(unique(matchrow.final[matchrow.final[,3] == 0, 2])))
#   
#   # print seeds that have number of non missing observations and unique controls above a desired threshold
#   if (length(unique(matchrow.final[matchrow.final[,3] == 0, 2])) >= 42 & Non.Missing.Num >= 650){
# 
#     print(k)
#     print(c(length(unique(matchrow.final[matchrow.final[, 3] == 0, 2])), Non.Missing.Num))
# 
#   }
# 
# }

# check what the maximum number of missing observations and maximum number of unique controls are
# max(NA.Num)
# max(UC.Num)

# give different controls matched to same study the same pair ID ####

# order the set of one to one matches by their Pair IDs 
matchrow.final = matchrow.final[order(matchrow.final[,1]), ]

# cycle through the matched IDs in the control group 
for (i in unique(matchrow.final[matchrow.final[,3] == 0, 2])[1:length(unique(matchrow.final[matchrow.final[, 3] == 0, 2]))]){
  
  # check if this control patient is matched to more than one study patient
  if (length(which(matchrow.final[,2] == i)) > 1){
    
    # cycle through the the second to last ID  
    for (j in 2:length(which(matchrow.final[,2] == i))){
      
      # set each 
      matchrow.final[(which(matchrow.final[,2] == i)[j] - 1), 1] = matchrow.final[(which(matchrow.final[,2] == i)[j-1] - 1), 1]
      matchrow.final[which(matchrow.final[,2] == i)[j], 1] = matchrow.final[which(matchrow.final[,2] == i)[j-1], 1]
      
    }  
    
  }
  
}

for (i in unique(matchrow.final[matchrow.final[,3]==0,2])[1:length(unique(matchrow.final[matchrow.final[,3]==0,2]))]){
  
  if (length(which(matchrow.final[,2]==i))>1){
    
    matchrow.final=matchrow.final[-which(matchrow.final[,2]==i)[2:length(which(matchrow.final[,2]==i))],]
    
  }
  
}

# # Make matches for powerpoint ####
# View(NewMaster.One[NewMaster.One[,"ID"]==23 | NewMaster.One[,"ID"]==230,
#                    c("ID", "Group", "Age", "Gender", "New.Race", 
#                      "Baseline.Mobility", 
#                      "Baseline.Activity",
#                      "Baseline.Cognitive",
#                      "Type.of.Stroke",
#                      "ARHosp.JRI.Facility.Adjustor")])




# 5. Analysis of continous functional outcomes ####

# Notes about set of one to one matches ####
### (5, 25, 25, 25, 1, 3) yields 58 out of 85 study ID matches
### 46 unique controls 684 nonmissing obs with seed 90
### maximum is 46 and 684

# 5a. Follow up analysis ####

#####################################Function#######################################
#Name: follow.up.analysis                                                          #
#Author: Traymon Beavers                                                           #
#Date Created: 6/15/2017                                                           #
#Date Updated: 7/3/2017                                                            #
#Purpose: To match the data based on gender, race, type of stroke, age, baseline   #
#         functional outcome scores, propensity score, and facility adjustor number#
#         and then perform a paired t-test with each match acting as a pair        #
#Variables: AgeNum-width for age partial matching                                  #
#           BaselineMobNum-width for baseline mobility score partial matching      #
#           BaselineActNum-width for baseline activity score partial matching      #
#           BaselineCogNum-width for baseline cognitive score partial matching     #
#           PScoreNum-width for propensity score partial matching                  #
#           FacAdjNum-width for facility adjustor partial matching                 #
#           ScoreNum-functional outcome to be analyzed: 1 for mobility, 2 for      #
#                    activity, and 3 for cognitive                                 #
#           FollowUpNum-visit number to analyze                                    #
#                                                                                  #
####################################################################################
  
follow.up.analysis = function(AgeNum = 5, 
                              BaselineMobNum=25, 
                              BaselineActNum=25, 
                              BaselineCogNum=25, 
                              PScoreNum = 1, 
                              FacAdjNum = 3,
                              ScoreNum=1, 
                              FollowUpNum=4){
  
  matchrows=matching(AgeNum = AgeNum , BaselineMobNum=BaselineMobNum, BaselineActNum=BaselineActNum, 
                     BaselineCogNum=BaselineCogNum, PScoreNum = PScoreNum, FacAdjNum = FacAdjNum)
  
  ####creation of unique (up to Control ID) one to one matched patients#### 
  
  matchrows.final=c(0,0,0)
  
  for (i in unique(matchrows[matchrows[,3]==1,2])){
    
    set.seed(13)
    
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
  
#################################End Function#######################################

# conduct follow up analysis for each score and each time point ####
  
for (i in 1:3){
  
  for (j in 4:7){
    
    print(follow.up.analysis(ScoreNum=i, FollowUpNum=j))
    
  }
  
}


  
####lmer analysis####

lmer.analysis=function(AgeNum = 5, BaselineMobNum=25, BaselineActNum=25, 
                       BaselineCogNum=25, PScoreNum = 1, FacAdjNum = 2,
                       ScoreNum=1, choice=1){
  
  require(lmerTest)
  
  #Create pairs from matching IDs so that matching is only one to one
  
  matchrows=matching(AgeNum = AgeNum , BaselineMobNum=BaselineMobNum, BaselineActNum=BaselineActNum, 
                     BaselineCogNum=BaselineCogNum, PScoreNum = PScoreNum, FacAdjNum = FacAdjNum)
  
  ####creation of unique (up to Study ID) one to one matched patients#### 
  
  matchrows.final=c(0,0,0)
  
  for (i in unique(matchrows[matchrows[,3]==1,2])){
    
    set.seed(13)
    
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
  
  #set a vector for the variables to analyzed
  Analysis.Variables=c("ID", "Group", "Age", "Gender", "New.Race", "Type.of.Stroke",
                       paste("Baseline", ScoreName[ScoreNum], sep="."), 
                       "ARHosp.JRI.Facility.Adjustor", "Days.After.Assignment", "DaysId", 
                       ScoreVarName[ScoreNum])
  
  
  ###Create new Interpolate.Master dataset for matched observations
  Interpolate.Master.Analysis=Interpolate.Master[Interpolate.Master[,"ID"] %in% matchrows.final[,2] & Interpolate.Master[,"DaysId"]>3, Analysis.Variables]
  
  Interpolate.Master.Analysis[,"Score.Diff.from.Baseline"]=Interpolate.Master.Analysis[,ScoreVarName[ScoreNum]]-Interpolate.Master.Analysis[,paste("Baseline", ScoreName[ScoreNum], sep=".")]
  
  Interpolate.Master.Analysis[,"Follow.Up.After.Assignment"]=rep(NA, dim(Interpolate.Master.Analysis)[1])
  
  Interpolate.Master.Analysis[Interpolate.Master.Analysis[,"DaysId"]==4,"Follow.Up.After.Assignment"]="First"
  
  Interpolate.Master.Analysis[Interpolate.Master.Analysis[,"DaysId"]==5,"Follow.Up.After.Assignment"]="Second"
  
  Interpolate.Master.Analysis[Interpolate.Master.Analysis[,"DaysId"]==6,"Follow.Up.After.Assignment"]="Third"
  
  Interpolate.Master.Analysis[Interpolate.Master.Analysis[,"DaysId"]==7,"Follow.Up.After.Assignment"]="Fourth"
  
  Interpolate.Master.Analysis[Interpolate.Master.Analysis[,"DaysId"]==8,"Follow.Up.After.Assignment"]="Final"
  
  Interpolate.Master.Analysis[,"PairID"]=rep(NA, dim(Interpolate.Master.Analysis)[1])
  
  for (i in 1:dim(Interpolate.Master.Analysis)[1]){
    
    Interpolate.Master.Analysis[i,"PairID"]=matchrows.final[which(matchrows.final[,2]==Interpolate.Master.Analysis[i,"ID"])[1],1]
    
  }
  
  Analysis.Data=Interpolate.Master.Analysis
  
  fmla1=as.formula(paste("Score.Diff.from.Baseline ~ ", paste(c("Group" ,"(ID | PairID) + (1 | Follow.Up.After.Assignment)"), collapse="+")))
  
  fmla2=as.formula(paste("Score.Diff.from.Baseline ~ ", paste(c("Group", "Days.After.Assignment", "(ID | PairID)"), collapse="+"), "+ Group*Days.After.Assignment"))
  
  fmla3=as.formula(paste("Score.Diff.from.Baseline ~ ", paste(c("Group", "Days.After.Assignment", "(ID | PairID)"), collapse="+")))
  
  fmla4=as.formula(paste("Score.Diff.from.Baseline ~ ", paste(c("Group", "(ID | PairID)"), collapse="+")))
  
  if (choice==1){
    
    result=lmerTest::lmer(data=Analysis.Data, fmla1)
    
  }else if (choice==2){
    
    result=lmerTest::lmer(data=Analysis.Data, fmla2)
    
  }else if (choice==3){
    
    result=lmerTest::lmer(data=Analysis.Data, fmla3)
    
  }else if (choice==4){
    
    result=lmerTest::lmer(data=Analysis.Data, fmla4)
    
  }
  
  return(result)
  
}

word=lmer.analysis(ScoreNum=1, choice=1)

summary(word)

word=lmer.analysis(ScoreNum=2, choice=2)

summary(word)

word=lmer.analysis(ScoreNum=3, choice=2)

summary(word)

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


# PLOTS####

library(ggplot2)

library(gridExtra)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# preliminary plots ####

Master.Study[,"col.ggplot"]=rep(NA, dim(Master.Study)[1])

Master.Study[Master.Study[,"DaysId"]<=2,"col.ggplot"]="blue"

Master.Study[Master.Study[,"DaysId"]==3,"col.ggplot"]="purple"

Master.Study[Master.Study[,"DaysId"]>=4,"col.ggplot"]="green"

Master.Control[,"col.ggplot"]=rep(NA, dim(Master.Control)[1])

Master.Control[Master.Control[,"DaysId"]<=2,"col.ggplot"]="blue"

Master.Control[Master.Control[,"DaysId"]==3,"col.ggplot"]="purple"

Master.Control[Master.Control[,"DaysId"]>=4 ,"col.ggplot"]="red"


for (i in 1:3){
  # Remove NAs (DS 06/23/2017)
  vname <- ScoreVarName[i]
  tmp1 <- Master.Study[!is.na(Master.Study[, vname]), ]
  
  p1 <- ggplot(data=tmp1,
               aes(x=Days_afterOnset,
                   y=tmp1[,ScoreVarName[i]],
                   group=ID)) +
    geom_line() +
    geom_point(aes(x = Days_afterOnset,
                   y = tmp1[,ScoreVarName[i]],
                   group=col.ggplot,
                   colour = col.ggplot)) +
    scale_colour_manual("Assignment", 
                        values = c("blue", 
                                   "green",
                                   "purple"),
                        labels = c("Before Assignment",
                                   "Study",
                                   "At assignment")) +
    scale_x_continuous("Days After Stroke") +
    scale_y_continuous(paste(ScoreName[i], 
                             "Score", 
                             sep=" "),
                       limits = c(-30, 130)) +
    ggtitle(paste(ScoreName[i], 
                  "Score by Days After Stroke for Study Group",
                  sep=" ")) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "top")
  
  tmp2 <- Master.Control[!is.na(Master.Control[, vname]), ]
  
  
  
  p2 <- ggplot(data=tmp2,
               aes(x=Days_afterOnset,
                   y=tmp2[,ScoreVarName[i]],
                   group=ID)) +
    geom_line() +
    geom_point(aes(x = Days_afterOnset,
                   y = tmp2[,ScoreVarName[i]],
                   group=col.ggplot,
                   colour = col.ggplot)) +
    scale_colour_manual("Assignment", 
                        values = c("blue", 
                                   "purple",
                                   "red"),
                        labels = c("Before Assignment",
                                   "At Assignment",
                                   "Control")) +    ggtitle(paste(ScoreName[i], "Score by Days After Stroke for Control Group", sep=" ")) +
    xlab("Days After Stroke") +
    ylab(paste(ScoreName[i], "Score", sep=" ")) +
    coord_cartesian(ylim=c(-30,130)) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "top")
  
  
  tiff(filename = paste("tmp/",
                        ScoreName[i],
                        "Score by Days After Stroke.tiff"),
       height = 8,
       width = 8,
       units = 'in',
       res = 300,
       compression = "lzw+p")
  
  p3=grid.arrange(p1, p2)
  
  print(p3)
  
  graphics.off()
}


# after matching plots ####

Interpolate.Master[,"col.ggplot"]=rep(NA, dim(Interpolate.Master)[1])

Interpolate.Master[Interpolate.Master[,"DaysId"]<=2,"col.ggplot"]="blue"

Interpolate.Master[Interpolate.Master[,"DaysId"]==3,"col.ggplot"]="purple"

Interpolate.Master[Interpolate.Master[,"DaysId"]>=4 & Interpolate.Master[,"Group"]=="Control Group", "col.ggplot"]="red"

Interpolate.Master[Interpolate.Master[,"DaysId"]>=4 & Interpolate.Master[,"Group"]=="Study Group", "col.ggplot"]="green"

for (i in 1:3){
  
  tmp1=Interpolate.Master[(Interpolate.Master[,"ID"] %in% intersect(StudyGroupIDs, matchrow.final[,2])) 
                          & is.na(Interpolate.Master[,"PT.AM.PAC.Basic.Mobility.Score"])==0,]
  
  p1=ggplot(data=tmp1,
            aes(x=Days_afterOnset, 
                y=tmp1[,ScoreVarName[i]], 
                group=ID)) +
    geom_line() +    
    geom_point(aes(x = Days_afterOnset,
                   y = tmp1[,ScoreVarName[i]],
                   group=col.ggplot,
                   colour = col.ggplot)) +
    scale_colour_manual("Assignment", 
                        values = c("blue", 
                                   "green",
                                   "purple"),
                        labels = c("Before Assignment",
                                   "Study",
                                   "At assignment")) +
    ggtitle(paste(ScoreName[i], "Score by Days After Stroke for Study Group (Matched)", sep=" ")) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "top") +
    xlab("Days After Stroke") +
    ylab(paste(ScoreName[i], "Score", sep=" ")) +
    coord_cartesian(ylim=c(-30,130))  

  
  tmp2=Interpolate.Master[(Interpolate.Master[,"ID"] %in% intersect(ControlGroupIDs, matchrow.final[,2])) 
                          & is.na(Interpolate.Master[,"PT.AM.PAC.Basic.Mobility.Score"])==0,]
  
    
  p2=ggplot(data=tmp2,
            aes(x=Days_afterOnset, 
                y=tmp2[,ScoreVarName[i]], 
                group=ID)) +
    geom_line() +    
    geom_point(aes(x = Days_afterOnset,
                   y = tmp2[,ScoreVarName[i]],
                   group=col.ggplot,
                   colour = col.ggplot)) +
    scale_colour_manual("Assignment", 
                        values = c("blue", 
                                   "purple",
                                   "red"),
                        labels = c("Before Assignment",
                                   "At Assignment",
                                   "Control")) +
    ggtitle(paste(ScoreName[i], "Score by Days After Stroke for Control Group (Matched)", sep=" ")) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "top") +
    xlab("Days After Stroke") +
    ylab(paste(ScoreName[i], "Score", sep=" ")) +
    coord_cartesian(ylim=c(-30,130))  
  
  tiff(filename = paste("tmp/",
                        ScoreName[i],
                        "Score by Days After Stroke (Matched).tiff"),
       height = 8,
       width = 8,
       units = 'in',
       res = 300,
       compression = "lzw+p")
  
  p3=grid.arrange(p1, p2)
  
  print(p3)
  
  graphics.off()
  
}



# after matching plots for days since group assignment ####

Interpolate.Master[,"col.ggplot"]=rep(NA, dim(Interpolate.Master)[1])

Interpolate.Master[Interpolate.Master[,"DaysId"]<=2,"col.ggplot"]="blue"

Interpolate.Master[Interpolate.Master[,"DaysId"]==3,"col.ggplot"]="purple"

Interpolate.Master[Interpolate.Master[,"DaysId"]>=4 & Interpolate.Master[,"Group"]=="Control Group", "col.ggplot"]="red"

Interpolate.Master[Interpolate.Master[,"DaysId"]>=4 & Interpolate.Master[,"Group"]=="Study Group", "col.ggplot"]="green"

for (i in 1:3){
  
  tmp1=Interpolate.Master[(Interpolate.Master[,"ID"] %in% intersect(StudyGroupIDs, matchrow.final[,2])) 
                          & is.na(Interpolate.Master[,"PT.AM.PAC.Basic.Mobility.Score"])==0,]
  
  p1=ggplot(data=tmp1,
            aes(x=Days.After.Assignment, 
                y=tmp1[,ScoreVarName[i]], 
                group=ID)) +
    geom_line() +    
    geom_point(aes(x = Days.After.Assignment,
                   y = tmp1[,ScoreVarName[i]],
                   group=col.ggplot,
                   colour = col.ggplot)) +
    scale_colour_manual("Assignment", 
                        values = c("blue", 
                                   "green",
                                   "purple"),
                        labels = c("Before Assignment",
                                   "Study",
                                   "At assignment")) +
    ggtitle(paste(ScoreName[i], "Score by Days After Group Assignment for Study Group (Matched)", sep=" ")) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "top") +
    xlab("Days After Stroke") +
    ylab(paste(ScoreName[i], "Score", sep=" ")) +
    coord_cartesian(ylim=c(-30,130))  
  
  
  tmp2=Interpolate.Master[(Interpolate.Master[,"ID"] %in% intersect(ControlGroupIDs, matchrow.final[,2])) 
                          & is.na(Interpolate.Master[,"PT.AM.PAC.Basic.Mobility.Score"])==0,]
  
  
  p2=ggplot(data=tmp2,
            aes(x=Days.After.Assignment, 
                y=tmp2[,ScoreVarName[i]], 
                group=ID)) +
    geom_line() +    
    geom_point(aes(x = Days.After.Assignment,
                   y = tmp2[,ScoreVarName[i]],
                   group=col.ggplot,
                   colour = col.ggplot)) +
    scale_colour_manual("Assignment", 
                        values = c("blue", 
                                   "purple",
                                   "red"),
                        labels = c("Before Assignment",
                                   "At Assignment",
                                   "Control")) +
    ggtitle(paste(ScoreName[i], "Score by Days After Group Assignment for Control Group (Matched)", sep=" ")) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "top") +
    xlab("Days After Stroke") +
    ylab(paste(ScoreName[i], "Score", sep=" ")) +
    coord_cartesian(ylim=c(-30,130))  
  
  tiff(filename = paste("tmp/",
                        ScoreName[i],
                        "Score by Days After Group Assignment (Matched).tiff"),
       height = 8,
       width = 8,
       units = 'in',
       res = 300,
       compression = "lzw+p")
  
  p3=grid.arrange(p1, p2)
  
  print(p3)
  
  graphics.off()
  
}



# plots histogram of baseline scores before and after matching ####



for (i in 1:3){
    
  par(mfrow=c(2,2), mar=c(2,2,2,2))
  
  hist(NewMaster.One.Study[,paste("Baseline", ScoreName[i], sep=".")],
       main = paste("Distribution of Baseline", 
                    ScoreName[i], 
                    "Scores for Study Group",
                    sep=" "), 
       xlab=paste("Baseline", 
                  ScoreName[i], 
                  "Score",
                  sep=" "))
  
  
  hist(NewMaster.One.Study[NewMaster.One.Study[,"ID"] %in% matchrow.final[,2],paste("Baseline", ScoreName[i], sep=".")],
       main = paste("Distribution of Baseline", 
                    ScoreName[i], 
                    "Scores for Study Group (Matched)",
                    sep=" "),
       xlab=paste("Baseline", 
                  ScoreName[i], 
                  "Score",
                  sep=" "))
  
  
  hist(NewMaster.One.Control[,paste("Baseline", ScoreName[i], sep=".")], 
       main = paste("Distribution of Baseline", 
                    ScoreName[i], 
                    "Scores for Control Group",
                    sep=" "),
       xlab=paste("Baseline", 
                  ScoreName[i], 
                  "Score",
                  sep=" "))
  
  hist(NewMaster.One.Control[NewMaster.One.Control[,"ID"] %in% matchrow.final[,2],paste("Baseline", ScoreName[i], sep=".")],
       main = paste("Distribution of Baseline", 
                    ScoreName[i], 
                    "Scores for Control Group (Matched)",
                    sep=" "),
       xlab=paste("Baseline", 
                  ScoreName[i], 
                  "Score",
                  sep=" "))
  
}

par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1))

# box and whisker plot for baseline scores ####


plot1=ggplot(data = NewMaster.One,
             aes(x=Group, y=Baseline.Mobility)) +
  geom_boxplot() +
  ylab("Baseline Mobility Score") +
  ggtitle("Distribution of Baseline Mobility Scores by Group") +
  theme(plot.title = element_text(hjust = 0.5))

plot2=ggplot(data = NewMaster.One,
             aes(x=Group, y=Baseline.Activity)) +
  geom_boxplot() +
  ylab("Baseline Activity Score") +
  ggtitle("Distribution of Baseline Activity Scores by Group") +
  theme(plot.title = element_text(hjust = 0.5))

plot3=ggplot(data = NewMaster.One,
             aes(x=Group, y=Baseline.Cognitive)) +
  geom_boxplot() +
  ylab("Baseline Cognitive Score") +
  ggtitle("Distribution of Baseline Cognitive Scores by Group") +
  theme(plot.title = element_text(hjust = 0.5))

plot4=ggplot(data = NewMaster.One[NewMaster.One[,"ID"] %in% matchrow.final[,2],],
             aes(x=Group, y=Baseline.Mobility)) +
  geom_boxplot() +
  ylab("Baseline Mobility Score") +
  ggtitle("Distribution of Baseline Mobility Scores by Group (Matched)") +
  theme(plot.title = element_text(hjust = 0.5))

plot5=ggplot(data = NewMaster.One[NewMaster.One[,"ID"] %in% matchrow.final[,2],],
             aes(x=Group, y=Baseline.Activity)) +
  geom_boxplot() +
  ylab("Baseline Activity Score") +
  ggtitle("Distribution of Baseline Activity Scores by Group (Matched)") +
  theme(plot.title = element_text(hjust = 0.5))

plot6=ggplot(data = NewMaster.One[NewMaster.One[,"ID"] %in% matchrow.final[,2],],
             aes(x=Group, y=Baseline.Cognitive)) +
  geom_boxplot() +
  ylab("Baseline Cognitive Score") +
  ggtitle("Distribution of Baseline Cognitive Scores by Group (Matched)") +
  theme(plot.title = element_text(hjust = 0.5))

png(filename="tmp/Mobility Box Plot.png")

multiplot(plot1, plot4, cols=2)

dev.off()

png(filename="tmp/Activity Box Plot.png")

multiplot(plot2, plot5, cols=2)

dev.off()

png(filename="tmp/Cognitive Box Plot.png")

multiplot(plot3, plot6, cols=2)

dev.off()

# point estimate plot for mortality ####

Mortality.Data=as.data.frame(matrix(0,nrow=2, ncol=4))

colnames(Mortality.Data)=c("Group", "Mortality", "Lower", "Upper")

Mortality.Data[,"Group"]=c("No Matching", "Matching")

tmp1=prop.test(sum(NewMaster.One.Control[,"Deceased_Y.N"]), 
               length(NewMaster.One.Control[,"Deceased_Y.N"]), 
               p=0.5)

tmp2=prop.test(sum(match.subgroup[match.subgroup[,"Group"]=="Control Group","Deceased_Y.N"]), 
               length(match.subgroup[match.subgroup[,"Group"]=="Control Group","Deceased_Y.N"]), 
               p=0.5)

Mortality.Data[1,"Mortality"]=as.numeric(tmp1$estimate)

Mortality.Data[1,"Lower"]=as.numeric(tmp1$conf.int[1])

Mortality.Data[1,"Upper"]=as.numeric(tmp1$conf.int[2])

Mortality.Data[2,"Mortality"]=as.numeric(tmp2$estimate)

Mortality.Data[2,"Lower"]=as.numeric(tmp2$conf.int[1])

Mortality.Data[2,"Upper"]=as.numeric(tmp2$conf.int[2])

ggplot(Mortality.Data,
       aes(x=Group,
           y=Mortality)) +
  geom_point(cex=3) +
  geom_errorbar(aes(ymin=Lower,
                    ymax=Upper,
                    width=0.1)) +
  ggtitle("Mortality Rate With and Without Matching") +
  theme(plot.title = element_text(hjust = 0.5))

View(Mortality.Data)

max(NewMaster[,"Baseline.Mobility"])

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













