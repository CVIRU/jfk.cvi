#################################Program Description################################
#Name: check data                                                                  #
#Author: Traymon Beavers                                                           #
#Depends: upload data.R                                                            #
#Date Created: 7/21/2017                                                           #
#Date Updated: 10/12/2018                                                          #
#Purpose: To check whether data is appropriate and remove incorrect data points if #
#         necessary                                                                #
####################################################################################

# Notes about data ####
### For some pairs of DaysId (i,j) where j>i, the days after onset for DaysID==i
### is greater than the days after onset for DaysID==j, 215 instances of this, 
### ID 390 has Fast.Nonfasting.LDL of 11
### ID 858 has 20442.4 for met mins
### ID 921 does not have daysID = 3

# Load the necessary source code ####
source("source/Traymon's Source Code/Data Reconfiguration/upload data (most recent).R")

# Check that the data is in the correct ranges ####
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
#                   NewMaster[, "ARHosp.JRI.Dis.FIM.Motor"] != 9999), c("ID", "ARHosp.JRI.Dis.FIM.Motor")]
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
#                   NewMaster[, "SBP.HTN"] != 9999 & NewMaster[, "SBP.HTN"] != 8888),
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
# NewMaster[which((NewMaster[, "CVG.Freq"] > 4 | NewMaster[, "CVG.Freq"] < 1) &
#                   NewMaster[, "CVG.Freq"] != 9999 & NewMaster[, "CVG.Freq"] != 8888),
#           c("ID", "CVG.Freq")]
# 
# NewMaster[which((NewMaster[, "CVG.Total"] > 36 | NewMaster[, "CVG.Total"] < 0) &
#                   NewMaster[, "CVG.Total"] != 9999 & NewMaster[, "CVG.Total"] != 8888),
#           c("ID", "CVG.Total")]
# 
# for (i in 1:8){
# 
#   print(length(NewMaster[NewMaster[, "DaysId"] == i, "DaysId"]) ==
#           length(unique(NewMaster[NewMaster[, "DaysId"] == i, "ID"])))
# 
# }
# # As of 1/10/2019 discrepency for daysID = 7

# As of 1/10/2019 the data is within normal ranges except for IDs 390 and 858 which are fixed below ####

correct.mets = NewMaster[NewMaster[, "ID"] == 858, "Cardiovascular.Group.Intensity.Mets."][6]
correct.mins = NewMaster[NewMaster[, "ID"] == 858, "Cardiovascular.Group.Tot.Mins"][6]
NewMaster[!is.na(NewMaster[, "Cardiovascular.Group.Met.Mins"]) &
            NewMaster[, "Cardiovascular.Group.Met.Mins"] == 20442.4, "Cardiovascular.Group.Met.Mins"] = correct.mets*correct.mins

# Find the patients with days after onset entered incorrectly ####
# ErrorIDs = NewMaster[NewMaster[, "DaysId"] > 2 &
#                        NewMaster[, "DaysId"] < 9 &
#                        NewMaster[, "Days.After.Assignment"] < 0, "ID"]
# 
# View(NewMaster[(NewMaster[, "DaysId"] > 2 &
#                   NewMaster[, "DaysId"] < 9 &
#                   NewMaster[, "Days.After.Assignment"] < 0) |
#                  (NewMaster[, "DaysId"] == 2 &
#                     NewMaster[, "ID"] %in% ErrorIDs), c("ID",
#                                                         "Group",
#                                                         "DaysId",
#                                                         "Days_afterOnset",
#                                                         "Days.After.Assignment")])
# 
# table(NewMaster[(NewMaster[, "DaysId"] > 2 &
#                    NewMaster[, "DaysId"] < 9 &
#                    NewMaster[, "Days.After.Assignment"] < 0) |
#                   (NewMaster[, "DaysId"] == 2 &
#                     NewMaster[, "ID"] %in% ErrorIDs), c("Group")])
# As of 1/10/2019 there are 215 patients (165 control, 2 study)
# with incorrect observations in days after onset, all relabeled as NA below

NewMaster[NewMaster[, "DaysId"] > 2 &
            NewMaster[, "DaysId"] < 9 &
            NewMaster[, "Days.After.Assignment"] < 0, "Days.After.Assignment"] = NA

# Check the median and range of follow up days after assignment ####
# 
# median(NewMaster[NewMaster[, "DaysId"] > 2 &
#                   NewMaster[, "Days.After.Assignment"] >= 0, "Days.After.Assignment"], na.rm = TRUE)
# 
# range(NewMaster[NewMaster[, "DaysId"] > 2 &
#                   NewMaster[, "Days.After.Assignment"] >= 0, "Days.After.Assignment"], na.rm = TRUE)
# As of 10/12/2018, median is 86, range is 0 to 399

# Remove unnecessary values ####
rm(correct.mets,
   correct.mins)