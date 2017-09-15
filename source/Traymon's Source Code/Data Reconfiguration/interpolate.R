#################################Program Description################################
#Name: interpolate                                                                 #
#Author: Traymon Beavers                                                           #
#Depends: upload data.R                                                            #
#Date Created: 7/21/2017                                                           #
#Date Updated: 9/15/2017                                                           #
#Purpose: To impute missing data in the stroke rehabiliation program using linear  # 
#         interpolation                                                            #
####################################################################################

# Load the necessary source code ####
source("source/Traymon's Source Code/Data Reconfiguration/upload data.R")

# Perform interpolation ####

#intialize dataset with interpolated missing values
Interpolate.Master = NewMaster

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

# # Check that the interpolation worked as intended ####
# View(NewMaster[,c("ID",
#                   "DaysId",
#                   "PT.AM.PAC.Basic.Mobility.Score",
#                   "OT.AM.Daily.Activity.Score",
#                   "ST.AM.Applied.Cogn.Score")])
# 
# View(Interpolate.Master[,c("ID",
#                            "DaysId",
#                            "PT.AM.PAC.Basic.Mobility.Score",
#                            "OT.AM.Daily.Activity.Score",
#                            "ST.AM.Applied.Cogn.Score")])
