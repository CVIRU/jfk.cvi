#################################Program Description################################
#Name: functional outcomes                                                         #
#Author: Traymon Beavers                                                           #
#Depends: create matches.R, follow up analysis.R, lmer analysis.R, ggplot2,        # 
#         lmerTest, gridExtra                                                      #
#Date Created: 7/21/2017                                                           #
#Date Updated: 9/14/2017                                                           #
#Purpose: To perform analysis on the stroke rehabilitation program functional      #
#         outcome data by matching patients in the study group with patients in    #
#         the control group and then conducting various statistical procedures     #
#         with respect to the matched pairs, as well as provide a collection of    #
#         plots                                                                    #
####################################################################################

# Load the necessary source code and functions ####
source("source/Traymon's Source Code/Analysis/create matches.R")
source("source/Traymon's Source Code/Functions/Analysis Functions/follow up analysis.R")
source("source/Traymon's Source Code/Functions/Analysis Functions/lmer analysis.R")
library(ggplot2)
library(gridExtra)

# Conduct follow up analysis for each score and each time point ####

# cycle through the different functional outcomes
for (i in 1:3){
  
  # cycle through the different follow ups
  for (j in 3:6){
    
    # print the paired t-test results
    print(follow.up.analysis(ScoreNum = i, FollowUpNum = j))
    
  }
  
}

# Conduct mixed effects linear model analysis for each score ####

# cycle through the different functional outcomes
for (i in 1:3){
  
  # cycle through different choices for fixed effects
  for (j in c(1,4)){
    
    # fit a mixed effects linear model
    results = lmer.analysis(ScoreNum = i, 
                         Choice = j)
    
    # print the results
    print(summary(results))
    
  }
  
}

# Average line graph ####
# Create dataset for before matching ####

# initialize dataset
tmp1 = as.data.frame(matrix(NA, 12, 6))

# give column names to the dataset
colnames(tmp1) = c("Group", 
                  "DaysId", 
                  ScoreVarName, 
                  "col.ggplot")

# label the group variable
tmp1[, "Group"] = rep(c("Study Group", "Control Group"), each = 6)

# label the daysid variable 
tmp1[, "DaysId"] = rep(c("Admission", 
                        "Discharge", 
                        "30 Day", 
                        "60 Day", 
                        "90 Day", 
                        "120 Day"), 2)

# assign colors for each follow up
tmp1[, "col.ggplot"] = c("blue", 
                        "darkmagenta", 
                        rep("green",4),
                        "blue",
                        "darkmagenta",
                        rep("red",4))

# assign shapes for each follow up
tmp1[, "shape.ggplot"] = c(15, 
                           16, 
                           rep(17,4),
                           15,
                           16,
                           rep(25,4))

# cycle through the rehabilitation groups
for (i in tmp1[, "Group"]){
  
  # cycle through the follow ups after discharge
  for (j in tmp1[, "DaysId"][3:6]){
    
    # cycle through the functional outcomes
    for (k in ScoreVarName){
      
      # fill in the average score for the given variable combination
      tmp1[tmp1[, "Group"] == i & tmp1[, "DaysId"] == j, k] = 
        mean(Interpolate.Master[Interpolate.Master[, "Group"] == i &
                                  Interpolate.Master[, "DaysId"] == which(unique(tmp1[, "DaysId"]) == j)  ,k], na.rm = TRUE)
      
    }
    
  }
  
}

# cycle through the admission and discharge follow ups
for (j in tmp1[, "DaysId"][1:2]){
  
  # cycle through the functional outcomes
  for (k in ScoreVarName){

    # fill in the average score for the given variable combination for the study group
    tmp1[tmp1[, "Group"] == "Study Group" & tmp1[, "DaysId"] == j, k] = 
      mean(Interpolate.Master[Interpolate.Master[, "ID"] %in% StudyGroupIDs &
                                Interpolate.Master[, "DaysId"] == which(unique(tmp1[, "DaysId"]) == j), k], na.rm = TRUE)
    
  }
  
}

# cycle through the admission and discharge follow ups
for (j in tmp1[, "DaysId"][1:2]){
  
  # cycle through the functional outcomes
  for (k in ScoreVarName){

    # fill in the average score for the given variable combination for the study group    
    tmp1[tmp1[, "Group"] == "Control Group" & tmp1[, "DaysId"] == j, k] = 
      mean(Interpolate.Master[Interpolate.Master[, "ID"] %in% ControlGroupIDs &
                                Interpolate.Master[, "DaysId"] == which(unique(tmp1[, "DaysId"]) == j), k], na.rm = TRUE)
    
  }
  
}

# save the dataset as a csv file
write.csv(tmp1,
          "media/Functional Outcomes/Data Tables/AM_PAC_No_Matching.csv",
          row.names = FALSE)

# Create dataset for after matching ####

# initialize dataset
tmp2 = as.data.frame(matrix(NA, 12, 6))

# give column names to the dataset
colnames(tmp2) = c("Group", 
                   "DaysId", 
                   ScoreVarName, 
                   "col.ggplot")

# label the group variable
tmp2[, "Group"] = rep(c("Study Group", "Control Group"), each = 6)

# label the daysid variable 
tmp2[, "DaysId"] = rep(c("Admission", 
                         "Discharge", 
                         "30 Day", 
                         "60 Day", 
                         "90 Day", 
                         "120 Day"), 2)

# assign colors for each follow up
tmp2[, "col.ggplot"] = c("blue", 
                         "darkmagenta", 
                         rep("green",4),
                         "blue",
                         "darkmagenta",
                         rep("red",4))

# assign shapes for each follow up
tmp2[, "shape.ggplot"] = c(15, 
                           16, 
                           rep(17,4),
                           15,
                           16,
                           rep(25,4))


# cycle through the rehabilitation groups
for (i in tmp2[, "Group"]){
  
  # cycle through the follow ups after discharge
  for (j in tmp2[, "DaysId"][3:6]){
    
    # cycle through the functional outcomes
    for (k in ScoreVarName){
      
      # fill in the average score for the given variable combination
      tmp2[tmp2[, "Group"] == i & tmp2[, "DaysId"] == j, k] = 
        mean(Interpolate.Master[Interpolate.Master[, "ID"] %in% match.subgroup.One[, "ID"] & 
                                  Interpolate.Master[, "Group"] == i &
                                  Interpolate.Master[, "DaysId"] == which(unique(tmp2[, "DaysId"]) == j)  ,k], na.rm = TRUE)
      
    }
    
  }
  
}

# cycle through the admission and discharge follow ups
for (j in tmp2[, "DaysId"][1:2]){
  
  # cycle through the functional outcomes
  for (k in ScoreVarName){
    
    # fill in the average score for the given variable combination for the study group
    tmp2[tmp2[, "Group"] == "Study Group" & tmp2[, "DaysId"] == j, k] = 
      mean(Interpolate.Master[Interpolate.Master[, "ID"] %in% match.subgroup.One[, "ID"] & 
                                Interpolate.Master[, "ID"] %in% StudyGroupIDs &
                                Interpolate.Master[, "DaysId"] == which(unique(tmp2[, "DaysId"]) == j), k], na.rm = TRUE)
    
  }
  
}

# cycle through the admission and discharge follow ups
for (j in tmp2[, "DaysId"][1:2]){
  
  # cycle through the functional outcomes
  for (k in ScoreVarName){
    
    # fill in the average score for the given variable combination for the study group    
    tmp2[tmp2[, "Group"] == "Control Group" & tmp2[, "DaysId"] == j, k] = 
      mean(Interpolate.Master[Interpolate.Master[, "ID"] %in% match.subgroup.One[, "ID"] &
                                Interpolate.Master[, "ID"] %in% ControlGroupIDs &
                                Interpolate.Master[, "DaysId"] == which(unique(tmp2[, "DaysId"]) == j), k], na.rm = TRUE)
    
  }
  
}

# save the dataset as a csv file
write.csv(tmp2,
          "media/Functional Outcomes/Data Tables/AM_PAC_After_Matching.csv",
          row.names = FALSE)

# Combine above datasets ####

# combine the datasets
tmp3 = rbind.data.frame(tmp1, tmp2)

# label the datasets as before matching and after matching
tmp3[, "Matched"] = rep(c("Before Matching", "After Matching"), each = 12)

# relevel the data
tmp3[, "Matched"] = factor(tmp3[, "Matched"], 
                           levels = c("Before Matching", 
                                      "After Matching"))

# save the dataset as a csv file
write.csv(tmp3,
          "media/Functional Outcomes/Data Tables/AM_PAC_Both.csv",
          row.names = FALSE)

# Create grid plot for each score/matching combination ####

# create line plot for mobility score
p1 = ggplot(data = tmp3,
            aes(x = DaysId,
                y = tmp3[, ScoreVarName[1]],
                group = Group)) +
  facet_wrap(~Matched, ncol = 1) +
  geom_text(data = data.frame(Matched = c("Before Matching",
                                          "After Matching"), 
                              label = c("",
                                        "P-Value < 0.001")), 
            aes(x = 5, y = 35, label = label),
            inherit.aes = FALSE,
            size = 5) +
  geom_text(data = data.frame(Matched = c("Before Matching",
                                          "After Matching"), 
                              label = c("",
                                        "Estimate = 9.47")), 
            aes(x = 5, y = 30, label = label),
            inherit.aes = FALSE,
            size = 5) +
  ggtitle(paste("Average", ScoreName[1], 
                "Score by Follow Up Time Point",
                sep=" ")) +
  geom_line() +
  geom_point(aes(x = DaysId,
                 y = tmp3[, ScoreVarName[1]],
                 color = col.ggplot),
             size = 3) +
  scale_color_manual("", 
                      values = c("blue", 
                                 "darkmagenta",
                                 "green",
                                 "red"),
                      labels = c("Admission",
                                 "Discharge",
                                 "Study Group",
                                 "Control Group")) +
  scale_x_discrete("Follow Up Time Point",
                   limits = c("Admission", 
                              "Discharge", 
                              "30 Day", 
                              "60 Day", 
                              "90 Day", 
                              "120 Day")) +
  scale_y_continuous(paste(ScoreName[1], 
                           "Score", 
                           sep=" "),
                     limits = c(20, 65)) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5),        
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

# create line plot for activity score
p2 = ggplot(data = tmp3,
            aes(x = DaysId,
                y = tmp3[, ScoreVarName[2]],
                group = Group)) +
  facet_wrap(~Matched, ncol = 1) +
  geom_text(data = data.frame(Matched = c("Before Matching",
                                          "After Matching"), 
                              label = c("",
                                        "P-Value < 0.001")), 
            aes(x = 5, y = 35, label = label),
            inherit.aes = FALSE,
            size = 5) +
  geom_text(data = data.frame(Matched = c("Before Matching",
                                          "After Matching"), 
                              label = c("",
                                        "Estimate = 9.59")), 
            aes(x = 5, y = 30, label = label),
            inherit.aes = FALSE,
            size = 5) +
  ggtitle(paste("Average", ScoreName[2], 
                "Score by Follow Up Time Point",
                sep=" ")) +
  geom_line() +
  geom_point(aes(x = DaysId,
                 y = tmp3[, ScoreVarName[2]],
                 color = col.ggplot),
             size = 3) +
  scale_colour_manual("", 
                      values = c("blue", 
                                 "darkmagenta",
                                 "green",
                                 "red"),
                      labels = c("Admission",
                                 "Discharge",
                                 "Study Group",
                                 "Control Group")) +
  scale_x_discrete("Follow Up Time Point",
                   limits = c("Admission", 
                              "Discharge", 
                              "30 Day", 
                              "60 Day", 
                              "90 Day", 
                              "120 Day")) +
  scale_y_continuous(paste(ScoreName[2], 
                           "Score", 
                           sep=" "),
                     limits = c(20, 65)) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5),        
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

# create line plot for cognitive score
p3 = ggplot(data = tmp3,
            aes(x = DaysId,
                y = tmp3[, ScoreVarName[3]],
                group = Group)) +
  facet_wrap(~Matched, ncol = 1) +
  geom_text(data = data.frame(Matched = c("Before Matching",
                                          "After Matching"), 
                              label = c("",
                                        "P-Value < 0.001")), 
            aes(x = 5, y = 35, label = label),
            inherit.aes = FALSE,
            size = 5) +
  geom_text(data = data.frame(Matched = c("Before Matching",
                                          "After Matching"), 
                              label = c("",
                                        "Estimate = 5.73")), 
            aes(x = 5, y = 30, label = label),
            inherit.aes = FALSE,
            size = 5) +
  ggtitle(paste("Average", ScoreName[3], 
                "Score by Follow Up Time Point",
                sep=" ")) +
  geom_line() +
  geom_point(aes(x = DaysId,
                 y = tmp3[, ScoreVarName[3]],
                 color = col.ggplot),
             size = 3) +
  scale_colour_manual("", 
                      values = c("blue", 
                                 "darkmagenta",
                                 "green",
                                 "red"),
                      labels = c("Admission",
                                 "Discharge",
                                 "Study Group",
                                 "Control Group")) +
  scale_x_discrete("Follow Up Time Point",
                   limits = c("Admission", 
                              "Discharge", 
                              "30 Day", 
                              "60 Day", 
                              "90 Day", 
                              "120 Day")) +
  scale_y_continuous(paste(ScoreName[3], 
                           "Score", 
                           sep=" "),
                     limits = c(20, 65)) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5),        
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

# combine all three plots
g = arrangeGrob(p1,p2,p3, nrow = 1)

# save the plot
ggsave("media/Functional Outcomes/AM PAC Average Line Graphs (Before and After Matching).png", 
       g,
       width = 18,
       height = 9)