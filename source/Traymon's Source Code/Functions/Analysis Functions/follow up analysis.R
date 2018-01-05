#####################################Function#######################################
#Name: follow.up.analysis                                                          #
#Author: Traymon Beavers                                                           #
#Depends: matching.R, final matching.R                                             #
#Date Created: 4/15/2017                                                           #
#Date Updated: 9/25/2017                                                           #
#Purpose: To match the data based on gender, race, type of stroke, age, baseline   #
#         functional outcome scores, propensity score, and facility adjustor number#
#         and then perform a paired t-test with each match acting as a pair        #
#Variables: AgeNum-width for age partial matching                                  #
#           DischargeMobNum-width for baseline mobility score partial matching     #
#           DischargeActNum-width for baseline activity score partial matching     #
#           DischargeCogNum-width for baseline cognitive score partial matching    #
#           PScoreNum-width for propensity score partial matching                  #
#           FacAdjNum-width for facility adjustor partial matching                 #
#           ScoreNum-functional outcome to be analyzed: 1 for mobility, 2 for      #
#                    activity, and 3 for cognitive                                 #
#           FollowUpNum-visit number to analyze                                    #
#           Match.seed-seed for choosing the one to one matches                    #
#                                                                                  #
####################################################################################

# Load the necessary packages and functions ####
source("source/Traymon's Source Code/Functions/Matching Functions/matching.R")
source("source/Traymon's Source Code/Functions/Matching Functions/final matching.R")

# Create function ####

follow.up.analysis = function(AgeNum = 9, 
                              DischargeMobNum = 23, 
                              DischargeActNum = 23, 
                              DischargeCogNum = 23,
                              PScoreNum = 1,
                              FacAdjNum = 2,
                              ScoreNum = 1, 
                              FollowUpNum = 4,
                              Match.seed = 965){
  
  # create the matched pairs
  matchrows = matching(AgeNum = AgeNum, 
                       DischargeMobNum = DischargeMobNum, 
                       DischargeActNum = DischargeActNum, 
                       DischargeCogNum = DischargeCogNum, 
                       PScoreNum = PScoreNum, 
                       FacAdjNum = FacAdjNum)
  
  # create the unique (up to Study ID) one to one matched patients
  matchrows.final = final.matching(Matchrows = matchrows,
                                   Match.seed = Match.seed)
  
  # Extract the data necessary for the paired t-test from the interpolated dataset ####
  
  # initialize the dataset
  Analysis.Data = rep(0,5)
  
  # initialize the vector of patients to be dropped when missing data occurs
  drop.pair.IDs = 0
  
  # cycle through the set of one to one matches
  for (i in 1:dim(matchrows.final)[1]){
    
    # if the data is non missing for this ID and visit number add the next element to the analysis dataset 
    # with their analysis variables 
    if (length(Interpolate.Master[Interpolate.Master[, "ID"] == matchrows.final[i, "ID"] & 
                                  Interpolate.Master[, "DaysId"] == FollowUpNum, ScoreVarName[ScoreNum]]) == 1){
      
      #add the next element to the analysis dataset with their analysis variables  
      Analysis.Data = rbind(Analysis.Data, 
                            c(matchrows.final[i, 1], 
                              matchrows.final[i, "ID"],
                              matchrows.final[i, 3], 
                              Interpolate.Master[Interpolate.Master[, "ID"] == matchrows.final[i, "ID"] & 
                                                   Interpolate.Master[, "DaysId"] == FollowUpNum, ScoreVarName[ScoreNum]],
                              Interpolate.Master[Interpolate.Master[, "ID"] == matchrows.final[i, "ID"] &
                                                   Interpolate.Master[, "DaysId"] == FollowUpNum, paste("Discharge", ScoreName[ScoreNum], sep=".")]))
      
    }else{
      
      # otherwise, add this patient's pair ID to the list of pair ID's to be removed before analysis
      drop.pair.IDs = c(drop.pair.IDs, matchrows.final[i, 1])
      
    }
    
  }
  
  # delete the initial 0's
  Analysis.Data = Analysis.Data[-1,]
  
  # delete the initial 0
  drop.pair.IDs = drop.pair.IDs[-1]
  
  # check if any patients need to be removed before analysis
  if (length(drop.pair.IDs)>0){
    
    #cycle through the list of patients that need to be removed before analysis and remove them
    for (j in 1:length(drop.pair.IDs)){
      
      Analysis.Data = Analysis.Data[-which(Analysis.Data[, 1] == drop.pair.IDs[j]), ] 
      
    }
    
  }
  
  # provide column names for the dataset created for analysis
  colnames(Analysis.Data)=c("PairID", 
                            "ID", 
                            "Group", 
                            "Score", 
                            "Discharge")
  
  # create the vector to be used for a paired t-test ####
  
  #initialize the vector for test data
  test.data = 0
  
  # cycle through the matched pairs and calculate their difference from baseline and then 
  # the treatment effect for that pair and add it to the vector for test data
  for (k in unique(Analysis.Data[,"PairID"])){
    
    test.data = c(test.data, (Analysis.Data[Analysis.Data[, "PairID"] == k & Analysis.Data[, "Group"] == 1, "Score"] - 
                                Analysis.Data[Analysis.Data[, "PairID"] == k & Analysis.Data[, "Group"] == 1, "Discharge"])-
                    (Analysis.Data[Analysis.Data[, "PairID"] == k & Analysis.Data[, "Group"] == 0, "Score"] - 
                       Analysis.Data[Analysis.Data[, "PairID"] == k & Analysis.Data[, "Group"] == 0, "Discharge"]))
    
  }
  
  # delete the initial 0
  test.data = test.data[-1]
  
  # conduct a one-sided t-test with the data
  result = t.test(test.data,
                  alternative = "greater")
  
  # output the result
  return(result)
  
}
