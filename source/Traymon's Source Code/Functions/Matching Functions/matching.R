#####################################Function#######################################
#Name: matching                                                                    #
#Author: Traymon Beavers                                                           #
#Date Created: 3/29/2017                                                           #
#Date Updated: 3/7/2018                                                            #
#Purpose: To match the data based on gender, race, type of stroke, age, baseline   #
#         functional outcome scores, propensity score, and facility adjustor       #
#         number; matches control group patients to study group patients           #
#Variables: AgeNum-width for age partial matching                                  #
#           DischargeMobNum-width for baseline mobility score partial matching     #
#           DischargeActNum-width for baseline activity score partial matching     #
#           DischargeCogNum-width for baseline cognitive score partial matching    #
#           PScoreNum-width for propensity score partial matching                  #
#           FacAdjNum-width for facility adjustor partial matching                 #
#                                                                                  #
####################################################################################

matching = function(AgeNum = 5, 
                    DischargeMobNum = 20, 
                    DischargeActNum = 20, 
                    DischargeCogNum = 20, 
                    PScoreNum = 1, 
                    FacAdjNum = 5){
  
  # create a data matrix for the characteristics to be used for matching for each 
  # patient in the study group
  Study.Characteristics = Interpolate.Master.One[Interpolate.Master.One[,"Group"] == "Study Group",
                                        c("ID", 
                                          "Age", 
                                          "Gender", 
                                          "New.Race", 
                                          "Discharge.Mobility", 
                                          "Discharge.Activity",
                                          "Discharge.Cognitive",
                                          "Type.of.Stroke",
                                          "ARHosp.JRI.Facility.Adjustor")]
  
  # initialize a progress bar to check the progress of the matching
  pb = txtProgressBar(min = 0, 
                      max = dim(Interpolate.Master.One.Control)[1], 
                      initial = 0)
  
  # initialize a vector to contain the matches
  result = rep(0,3)
  
  # begin the counter for pair ID
  D = 1
  
  # cycle through the control group observations
  for (i in 1:dim(Interpolate.Master.One.Control)[1]){
    
    #update progress bar to reflect that the function is attempting to match the next control
    setTxtProgressBar(pb, i)
    
    #cycle through the study characteristics
    for (j in 1:dim(Study.Characteristics)[1]){
      
      # add the matched pair and their pair ID to the list of matches if matching criteria is satisfied 
      if (Interpolate.Master.One.Control[i, "Age"] >= (Study.Characteristics[j, "Age"] - AgeNum) &
          Interpolate.Master.One.Control[i, "Age"] <= (Study.Characteristics[j, "Age"] + AgeNum) &
          Interpolate.Master.One.Control[i, "Discharge.Mobility"] >= (Study.Characteristics[j, "Discharge.Mobility"] - DischargeMobNum) &
          Interpolate.Master.One.Control[i, "Discharge.Mobility"] <= (Study.Characteristics[j, "Discharge.Mobility"] + DischargeMobNum) &
          Interpolate.Master.One.Control[i, "Discharge.Activity"] >= (Study.Characteristics[j, "Discharge.Activity"] - DischargeActNum) &
          Interpolate.Master.One.Control[i, "Discharge.Activity"] <= (Study.Characteristics[j, "Discharge.Activity"] + DischargeActNum) &
          Interpolate.Master.One.Control[i, "Discharge.Cognitive"] >= (Study.Characteristics[j, "Discharge.Cognitive"] - DischargeCogNum) &
          Interpolate.Master.One.Control[i, "Discharge.Cognitive"] <= (Study.Characteristics[j, "Discharge.Cognitive"] + DischargeCogNum) &
          Interpolate.Master.One.Control[i,"Gender"] == Study.Characteristics[j,"Gender"] &
          Interpolate.Master.One.Control[i,"New.Race"] == Study.Characteristics[j, "New.Race"] &
          Interpolate.Master.One.Control[i,"Type.of.Stroke"] == Study.Characteristics[j, "Type.of.Stroke"] &
          # Interpolate.Master.One.Control[i,"Propensity.Score"] >= (Study.Characteristics[j,"Propensity.Score"] - PScoreNum) &
          # Interpolate.Master.One.Control[i,"Propensity.Score"] <= (Study.Characteristics[j,"Propensity.Score"] + PScoreNum) &
          Interpolate.Master.One.Control[i,"ARHosp.JRI.Facility.Adjustor"] >= (Study.Characteristics[j, "ARHosp.JRI.Facility.Adjustor"] - FacAdjNum) & 
          Interpolate.Master.One.Control[i,"ARHosp.JRI.Facility.Adjustor"] <= (Study.Characteristics[j, "ARHosp.JRI.Facility.Adjustor"] + FacAdjNum)){
        
        result = rbind(result, 
                       c(D, 
                         Study.Characteristics[j,"ID"], 
                         1), 
                       c(D, 
                         Interpolate.Master.One.Control[i,"ID"], 
                         0))
        
        # increase the pair ID counter
        D = D+1
        
      }
      
    }
    
  }
  
  # finish progress bar
  close(pb)
  
  # give names to what each column represents
  colnames(result) = c("PairID", 
                       "ID", 
                       "Group")
  
  # delete the initial 0's
  result = result[-1, ]
  
  # output the resulting matches
  return(result)
  
}

# # Check that function works as intended ####
# 
# # create matches
# matchrows = matching()
# 
# # cycle through the pair IDs
# for (i in matchrows[, "PairID"]){
# 
#   # print the matches and the characteristics they are supposed to be matched on
#   print(Interpolate.Master.One[Interpolate.Master.One[, "ID"] %in% matchrows[which(matchrows[, "PairID"] == i), 2], c("ID",
#                                                                                                     "Group",
#                                                                                                     "Age",
#                                                                                                     "Gender",
#                                                                                                     "New.Race",
#                                                                                                     "Discharge.Mobility",
#                                                                                                     "Discharge.Activity",
#                                                                                                     "Discharge.Cognitive",
#                                                                                                     "Type.of.Stroke",
#                                                                                                     "ARHosp.JRI.Facility.Adjustor")])
# 
# }