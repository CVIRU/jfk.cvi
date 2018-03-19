#####################################Function#######################################
#Name: final.matching                                                              #
#Author: Traymon Beavers                                                           #
#Date Created: 7/4/2017                                                            #
#Date Updated: 3/7/2018                                                            #
#Purpose: To create one to one matches after the data is already matched using the #
#         matching function; unique up to study patient                            #
#Variables: Matchrows-matched pairs created using matching                         #
#           Match.seed-seed for randomly choosing the one to one matches           #
#                                                                                  #
####################################################################################

final.matching = function(Matchrows = matchrows,
                          Match.seed = 370){
  
  # initialize the matrix to hold the set of one to one matches
  matchrow.final = rep(0,3)
  
  # cycle through the unique study patients in the matches created 
  for (i in unique(Matchrows[Matchrows[,3] == 1, 2])){
    
    # set the seed
    set.seed(Match.seed)
    
    # count the number of control patients matched to this study patient
    N = length(which(Matchrows[, 2] == i))
    
    # randomly choose which control patient will be move to the one-to-one set of matches
    j = ceiling(runif(1, 
                      0, 
                      N))
    
    # update the set of one to one matches to include the new match
    matchrow.final = rbind(matchrow.final,
                           Matchrows[which(Matchrows[,2] == i)[j], ],
                           Matchrows[(which(Matchrows[,2] == i)[j] + 1), ])
    
  }
  
  # delete the initial 0's (begin indent)
  matchrow.final = matchrow.final[-1, ]
  
  # order the set of one to one matches by their Pair IDs 
  matchrow.final = matchrow.final[order(matchrow.final[, 1]), ]
  
  # cycle through the matched IDs in the control group 
  for (i in unique(matchrow.final[matchrow.final[,3] == 0, 2])[1:length(unique(matchrow.final[matchrow.final[, 3] == 0, 2]))]){
    
    # check if this control patient is matched to more than one study patient
    if (length(which(matchrow.final[,2] == i)) > 1){
      
      # cycle through the the second to last ID  
      for (j in 2:length(which(matchrow.final[,2] == i))){
        
        # give every patient matched to the same control the same Pair ID 
        matchrow.final[(which(matchrow.final[,2] == i)[j] - 1), 1] = matchrow.final[(which(matchrow.final[,2] == i)[j-1] - 1), 1]
        matchrow.final[which(matchrow.final[,2] == i)[j], 1] = matchrow.final[which(matchrow.final[,2] == i)[j-1], 1]
        
      }  
      
    }
    
  }
  
  # cycle through the matched IDs in the control group 
  for (i in unique(matchrow.final[matchrow.final[,3] == 0, 2])[1:length(unique(matchrow.final[matchrow.final[, 3] == 0, 2]))]){
    
    # check if this control patient is matched to more than one study patient
    if (length(which(matchrow.final[,2] == i)) > 1){
      
      # delete the extra entries of this control patient so hat only one is left
      matchrow.final = matchrow.final[-which(matchrow.final[, 2] == i)[2:length(which(matchrow.final[, 2] == i))], ]
      
    }
    
  }
  
  return(matchrow.final)
  
}

# # Check that function works as intended ####
# 
# # create one to one matches
# matchrow.final = final.matching()
# 
# # cycle through pair IDs
# for (i in matchrow.final[, "PairID"]){
# 
#   # print the matches and the characteristics they are supposed to be matched on
#   print(Interpolate.Master.One[Interpolate.Master.One[, "ID"] %in% matchrow.final[which(matchrow.final[, "PairID"] == i), "ID"],
#                       c("ID",
#                         "Group",
#                         "Age",
#                         "Gender",
#                         "New.Race",
#                         "Admission.Mobility",
#                         "Admission.Activity",
#                         "Admission.Cognitive",
#                         "Type.of.Stroke",
#                         "ARHosp.JRI.Facility.Adjustor")])
# 
# }