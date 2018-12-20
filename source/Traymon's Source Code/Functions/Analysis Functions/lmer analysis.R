#####################################Function#######################################
#Name: lmer.analysis                                                               #
#Author: Traymon Beavers                                                           #
#Depends: matching.R, final matching.R, lmerTest                                   #
#Date Created: 4/15/2017                                                           #
#Date Updated: 3/9/2018                                                            #
#Purpose: To match the data based on gender, race, type of stroke, age, baseline   #
#         functional outcome scores, propensity score, and facility adjustor       #
#         number and then fit a mixed linear model with matched pair ID, patient   # 
#         ID, and follow up as random effects and a user choice of fixed effects   #
#Variables: AgeNum-width for age partial matching                                  #
#           BaselineMobNum-width for baseline mobility score partial matching      #
#           BaselineActNum-width for baseline activity score partial matching      #
#           BaselineCogNum-width for baseline cognitive score partial matching     #
#           PScoreNum-width for propensity score partial matching                  #
#           FacAdjNum-width for facility adjustor partial matching                 #
#           ScoreNum-functional outcome to be analyzed: 1 for mobility, 2 for      #
#                    activity, and 3 for cognitive                                 #
#           Choice-choose which set of fixed effects to use:                       #
#                  1 = rehabilitation group                                        #
#                  2 = rehabilitation group and days after group asssignment       #
#                  3 = rehabilitation group, days after group asssignment,         #
#                      and their interaction                                       #
#                  4 = rehabilitation group and all variables used for matching    #
#           Match.seed-seed for choosing the one to one matches                    #
#                                                                                  #
####################################################################################

# Load the necessary packages and functions ####
source("source/Traymon's Source Code/Functions/Matching Functions/matching.R")
source("source/Traymon's Source Code/Functions/Matching Functions/final matching.R")
library(lmerTest)

# Create function ####
lmer.analysis = function(AgeNum = 5, 
                         DischargeMobNum = 15, 
                         DischargeActNum = 15, 
                         DischargeCogNum = 15,
                         FacAdjNum = 1,
                         ScoreNum = 1, 
                         Choice = 1,
                         Match.seed = 912){
  
  # create matches
  matchrows = matching(AgeNum = AgeNum , 
                       DischargeMobNum = DischargeMobNum, 
                       DischargeActNum = DischargeActNum, 
                       DischargeCogNum = DischargeCogNum, 
                       FacAdjNum = FacAdjNum)
  
  # creation of unique (up to Study ID) one to one matched patients  
  
  matchrows.final = final.matching(Matchrows = matchrows,
                                   Match.seed = Match.seed)
  
  
  # set a vector for the variables to analyzed
  Analysis.Variables = c("ID", 
                         "Group", 
                         "Age", 
                         "Gender", 
                         "New.Race", 
                         "Type.of.Stroke",
                         paste("Discharge", ScoreName[ScoreNum], sep="."), 
                         "ARHosp.JRI.Facility.Adjustor", 
                         "Days.After.Assignment", 
                         "DaysId", 
                         ScoreVarName[ScoreNum])
  
  
  # Create the analysis dataset for matched observations
  Interpolate.Master.Analysis = Interpolate.Master[Interpolate.Master[, "ID"] %in% matchrows.final[, 2] & 
                                                     Interpolate.Master[, "DaysId"] > 2, Analysis.Variables]
  
  Interpolate.Master.Analysis[, "Score.Diff.from.Discharge"] = Interpolate.Master.Analysis[, ScoreVarName[ScoreNum]]-
    Interpolate.Master.Analysis[, paste("Discharge", ScoreName[ScoreNum], sep=".")]
  
  Interpolate.Master.Analysis[Interpolate.Master.Analysis[, "DaysId"] == 3,"Follow.Up.After.Assignment"] = "30 Days"
  
  Interpolate.Master.Analysis[Interpolate.Master.Analysis[, "DaysId"] == 4,"Follow.Up.After.Assignment"] = "60 Days"
  
  Interpolate.Master.Analysis[Interpolate.Master.Analysis[, "DaysId"] == 5,"Follow.Up.After.Assignment"] = "90 Days"
  
  Interpolate.Master.Analysis[Interpolate.Master.Analysis[, "DaysId"] == 6,"Follow.Up.After.Assignment"] = "120 Days"
  
  Interpolate.Master.Analysis[Interpolate.Master.Analysis[, "DaysId"] == 7,"Follow.Up.After.Assignment"] = "180 Days"
  
  Interpolate.Master.Analysis[Interpolate.Master.Analysis[, "DaysId"] == 8,"Follow.Up.After.Assignment"] = "365 Days"
  
  for (i in 1:dim(Interpolate.Master.Analysis)[1]){
    
    Interpolate.Master.Analysis[i, "PairID"] = matchrows.final[which(matchrows.final[, 2] == Interpolate.Master.Analysis[i, "ID"])[1], 1]
    
  }
  
  Analysis.Data = Interpolate.Master.Analysis[Interpolate.Master.Analysis[ ,"DaysId"] < 7, ]
  
  fmla1 = as.formula(paste("Score.Diff.from.Discharge ~ ", 
                           paste(c("Group",
                                   "Age",
                                   "(ID | PairID)"), 
                                 collapse="+")))
  
  fmla2 = as.formula(paste("Score.Diff.from.Discharge ~ ", 
                           paste(c("Group", 
                                   "Age", 
                                   "(1 | PairID) + (1 | ID)"), 
                                 collapse="+")))
  
  fmla3 = as.formula(paste("Score.Diff.from.Discharge ~ ", 
                           paste(c("Group", 
                                   "Age", 
                                   "(1 | PairID)"), 
                                 collapse="+")))
  
  fmla4 = as.formula(paste("Score.Diff.from.Discharge ~ ", 
                           paste(c(Analysis.Variables[c(2:6,8,9)], 
                                   "(ID | PairID)"), 
                                 collapse="+")))
  
  
  
  if (Choice == 1){
    
    result = lmerTest::lmer(data = Analysis.Data, fmla1)
    
  }else if (Choice == 2){
    
    result = lmerTest::lmer(data = Analysis.Data, fmla2)
    
  }else if (Choice == 3){
    
    result = lmerTest::lmer(data = Analysis.Data, fmla3)
    
  }else if (Choice == 4){
    
    result = lmerTest::lmer(data = Analysis.Data, fmla4)
  }
  
  return(result)
  
}