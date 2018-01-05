#################################Program Description################################
#Name: functional outcomes                                                         #
#Author: Traymon Beavers                                                           #
#Depends: create matches.R, follow up analysis.R, lmer analysis.R, ggplot2,        # 
#         lmerTest, gridExtra, data.table                                          #
#Date Created: 7/21/2017                                                           #
#Date Updated: 11/28/2017                                                          #
#Purpose: To perform analysis on the stroke rehabilitation program functional      #
#         outcome data by matching patients in the study group with patients in    #
#         the control group and then conducting various statistical procedures     #
#         with respect to the matched pairs, as well as provide a collection of    #
#         plots                                                                    #
####################################################################################

# Load the necessary source code, functions, and packages ####
source("source/Traymon's Source Code/Analysis/create matches.R")
source("source/Traymon's Source Code/Functions/Analysis Functions/follow up analysis.R")
source("source/Traymon's Source Code/Functions/Analysis Functions/lmer analysis.R")
library(ggplot2)
library(gridExtra)
library(data.table)

# Conduct follow up analysis for each score and each time point ####

# cycle through the different functional outcomes
for (i in 1:3){
  
  # cycle through the different follow ups
  for (j in 6){
    
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
    
    if (i == 1 & j == 4){
    
    # store data for p-value bar graph
    for.p.val.plot.mob = summary(results)$coefficients[, 5]
    
    }else if(i == 2 & j == 4){

      # store data for p-value bar graph
      for.p.val.plot.act = summary(results)$coefficients[, 5]
      
    }else if(i == 3 & j == 4){
      
      # store data for p-value bar graph
      for.p.val.plot.cog = summary(results)$coefficients[, 5]
      
    }
    
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
tmp2[, "shape.ggplot"] = c("Admission", 
                           "Discharge", 
                           rep("SRP Paricipant Group",4),
                           "Admission", 
                           "Discharge",
                           rep("Non-Paricipant Group",4))


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


tmp2 = melt(tmp2, id.vars = c("Group", 
                       "DaysId", 
                       "col.ggplot", 
                       "shape.ggplot"),
     measure.vars = c("PT.AM.PAC.Basic.Mobility.Score", 
                      "OT.AM.Daily.Activity.Score",
                      "ST.AM.Applied.Cogn.Score"))

colnames(tmp2)[5:6] = c("Score.Type", "Score")

tmp2[,"Score.Type"] =  factor(tmp2[,"Score.Type"], 
                              levels = c(levels(tmp2[,"Score.Type"]),
                                         "Basic Mobility",
                                         "Daily Activity",
                                         "Applied Cognitive"))

tmp2[1:12,"Score.Type"] = "Basic Mobility"

tmp2[13:24,"Score.Type"] = "Daily Activity"

tmp2[25:36,"Score.Type"] = "Applied Cognitive"

# save the dataset as a csv file
write.csv(tmp2,
          "media/Functional Outcomes/Data Tables/AM_PAC_After_Matching.csv",
          row.names = FALSE)

# Create line plot for each score (After Matching) ####

# create line plot
ggplot(data = tmp2,
       aes(x = DaysId,
           y = Score,
           group = Group)) +
  facet_wrap(~Score.Type, nrow = 1) +
  geom_text(data = data.frame(Score.Type = c("Basic Mobility",
                                             "Daily Activity",
                                             "Applied Cognitive"),
                              label = rep("P-Value < 0.001", 3)),
            aes(x = 5, 
                y = 37.5, 
                label = label),
            inherit.aes = FALSE,
            size = 5) +
  geom_text(data = data.frame(Score.Type = c("Basic Mobility",
                                             "Daily Activity",
                                             "Applied Cognitive"),
                              label = c("Estimate = 9.47",
                                        "Estimate = 9.59",
                                        "Estimate = 5.73")),
                              aes(x = 5, 
                                  y = 32.5, 
                                  label = label),
                              inherit.aes = FALSE,
                              size = 5) +
  ggtitle("Average Score by Follow Up Time Point") +
  geom_line() +
  geom_point(aes(x = DaysId,
                 y = Score,
                 color = col.ggplot),
             size = 3) +
  scale_color_manual("", 
                     values = c("blue", 
                                "darkmagenta",
                                "green",
                                "red"),
                     labels = c("Admission",
                                "Discharge",
                                "SRP-participant Group",
                                "nonparticipant Group")) +
  scale_x_discrete("Follow Up Time Point",
                   limits = c("Admission", 
                              "Discharge", 
                              "30 Day", 
                              "60 Day", 
                              "90 Day", 
                              "120 Day")) +
  scale_y_continuous("Score",
                     limits = c(30, 65)) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5),        
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

# save the plot
ggsave("media/Functional Outcomes/AM PAC Average Line Graphs (After Matching).tiff",
       device = "tiff",
       width = 10,
       height = 5,
       dpi = 300,
       compression = "lzw")

# Create line plot for each score (After Matching) (no color) ####

# create line plot
ggplot(data = tmp2,
       aes(x = DaysId,
           y = Score,
           group = Group)) +
  facet_wrap(~Score.Type, nrow = 1) +
  geom_text(data = data.frame(Score.Type = c("Basic Mobility",
                                             "Daily Activity",
                                             "Applied Cognitive"),
                              label = rep("P < 0.001", 3)),
            aes(x = 4, 
                y = 32.5, 
                label = label),
            inherit.aes = FALSE,
            size = 5) +
  ggtitle(expression("Average" ~ AM-PAC^{TM} ~ "Score by Follow Up Time Point")) +
  geom_line() +
  geom_point(aes(x = DaysId,
                 y = Score,
                 shape = shape.ggplot),
             fill = "black",
             size = 3) +
  scale_shape_manual("",
                     values = c(15,
                                16,
                                17,
                                8),
                     labels = c("Admission",
                                "Discharge",
                                "nonparticipant",
                                "SRP-participant")) +
  scale_x_discrete("Follow Up Time Point",
                   limits = c("Admission", 
                              "Discharge", 
                              "30 Day", 
                              "60 Day", 
                              "90 Day", 
                              "120 Day")) +
  scale_y_continuous(expression(AM-PAC^{TM} ~ "Score"),
                     limits = c(30, 65)) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5),        
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

# save the plot
ggsave("media/Functional Outcomes/AM PAC Average Line Graphs (After Matching) (no color).tiff",
       device = "tiff",
       width = 10,
       height = 5,
       dpi = 300,
       compression = "lzw")

# Create line plot for each score (After Matching) (no color) (control label) ####

# create line plot
ggplot(data = tmp2,
       aes(x = DaysId,
           y = Score,
           group = Group)) +
  facet_wrap(~Score.Type, nrow = 1) +
  geom_text(data = data.frame(Score.Type = c("Basic Mobility",
                                             "Daily Activity",
                                             "Applied Cognitive"),
                              label = rep("P < 0.001", 3)),
            aes(x = 4, 
                y = 32.5, 
                label = label),
            inherit.aes = FALSE,
            size = 5) +
  ggtitle(expression("Average" ~ AM-PAC^{TM} ~ "Score by Follow Up Time Point")) +
  geom_line() +
  geom_point(aes(x = DaysId,
                 y = Score,
                 shape = shape.ggplot),
             fill = "black",
             size = 3) +
  scale_shape_manual("",
                     values = c(15,
                                16,
                                17,
                                8),
                     labels = c("Admission",
                                "Discharge",
                                "Control",
                                "SRP-participant")) +
  scale_x_discrete("Follow Up Time Point",
                   limits = c("Admission", 
                              "Discharge", 
                              "30 Day", 
                              "60 Day", 
                              "90 Day", 
                              "120 Day")) +
  scale_y_continuous(expression(AM-PAC^{TM} ~ "Score"),
                     limits = c(30, 65)) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5),        
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

# save the plot
ggsave("media/Functional Outcomes/AM PAC Average Line Graphs (After Matching) (no color) (control label).tiff",
       device = "tiff",
       width = 10,
       height = 5,
       dpi = 300,
       compression = "lzw")

# # Combine above datasets ####
# 
# # combine the datasets
# tmp3 = rbind.data.frame(tmp1, tmp2)
# 
# # label the datasets as before matching and after matching
# tmp3[, "Matched"] = rep(c("Before Matching", "After Matching"), each = 12)
# 
# # relevel the data
# tmp3[, "Matched"] = factor(tmp3[, "Matched"], 
#                            levels = c("Before Matching", 
#                                       "After Matching"))
# 
# # save the dataset as a csv file
# write.csv(tmp3,
#           "media/Functional Outcomes/Data Tables/AM_PAC_Both.csv",
#           row.names = FALSE)
# 
# # Create grid plot for each score/matching combination ####
# 
# # create line plot for mobility score
# p1 = ggplot(data = tmp3,
#             aes(x = DaysId,
#                 y = tmp3[, ScoreVarName[1]],
#                 group = Group)) +
#   facet_wrap(~Matched, ncol = 1) +
#   geom_text(data = data.frame(Matched = c("Before Matching",
#                                           "After Matching"), 
#                               label = c("",
#                                         "P-Value < 0.001")), 
#             aes(x = 5, y = 35, label = label),
#             inherit.aes = FALSE,
#             size = 5) +
#   geom_text(data = data.frame(Matched = c("Before Matching",
#                                           "After Matching"), 
#                               label = c("",
#                                         "Estimate = 9.47")), 
#             aes(x = 5, y = 30, label = label),
#             inherit.aes = FALSE,
#             size = 5) +
#   ggtitle(paste("Average", ScoreName[1], 
#                 "Score by Follow Up Time Point",
#                 sep=" ")) +
#   geom_line() +
#   geom_point(aes(x = DaysId,
#                  y = tmp3[, ScoreVarName[1]],
#                  color = col.ggplot),
#              size = 3) +
#   scale_color_manual("", 
#                       values = c("blue", 
#                                  "darkmagenta",
#                                  "green",
#                                  "red"),
#                       labels = c("Admission",
#                                  "Discharge",
#                                  "Study Group",
#                                  "Control Group")) +
#   scale_x_discrete("Follow Up Time Point",
#                    limits = c("Admission", 
#                               "Discharge", 
#                               "30 Day", 
#                               "60 Day", 
#                               "90 Day", 
#                               "120 Day")) +
#   scale_y_continuous(paste(ScoreName[1], 
#                            "Score", 
#                            sep=" "),
#                      limits = c(20, 65)) +
#   theme(legend.position = "top",
#         plot.title = element_text(hjust = 0.5),        
#         axis.text.x = element_text(angle = 45,
#                                    hjust = 1))
# 
# # create line plot for activity score
# p2 = ggplot(data = tmp3,
#             aes(x = DaysId,
#                 y = tmp3[, ScoreVarName[2]],
#                 group = Group)) +
#   facet_wrap(~Matched, ncol = 1) +
#   geom_text(data = data.frame(Matched = c("Before Matching",
#                                           "After Matching"), 
#                               label = c("",
#                                         "P-Value < 0.001")), 
#             aes(x = 5, y = 35, label = label),
#             inherit.aes = FALSE,
#             size = 5) +
#   geom_text(data = data.frame(Matched = c("Before Matching",
#                                           "After Matching"), 
#                               label = c("",
#                                         "Estimate = 9.59")), 
#             aes(x = 5, y = 30, label = label),
#             inherit.aes = FALSE,
#             size = 5) +
#   ggtitle(paste("Average", ScoreName[2], 
#                 "Score by Follow Up Time Point",
#                 sep=" ")) +
#   geom_line() +
#   geom_point(aes(x = DaysId,
#                  y = tmp3[, ScoreVarName[2]],
#                  color = col.ggplot),
#              size = 3) +
#   scale_colour_manual("", 
#                       values = c("blue", 
#                                  "darkmagenta",
#                                  "green",
#                                  "red"),
#                       labels = c("Admission",
#                                  "Discharge",
#                                  "Study Group",
#                                  "Control Group")) +
#   scale_x_discrete("Follow Up Time Point",
#                    limits = c("Admission", 
#                               "Discharge", 
#                               "30 Day", 
#                               "60 Day", 
#                               "90 Day", 
#                               "120 Day")) +
#   scale_y_continuous(paste(ScoreName[2], 
#                            "Score", 
#                            sep=" "),
#                      limits = c(20, 65)) +
#   theme(legend.position = "top",
#         plot.title = element_text(hjust = 0.5),        
#         axis.text.x = element_text(angle = 45,
#                                    hjust = 1))
# 
# # create line plot for cognitive score
# p3 = ggplot(data = tmp3,
#             aes(x = DaysId,
#                 y = tmp3[, ScoreVarName[3]],
#                 group = Group)) +
#   facet_wrap(~Matched, ncol = 1) +
#   geom_text(data = data.frame(Matched = c("Before Matching",
#                                           "After Matching"), 
#                               label = c("",
#                                         "P-Value < 0.001")), 
#             aes(x = 5, y = 35, label = label),
#             inherit.aes = FALSE,
#             size = 5) +
#   geom_text(data = data.frame(Matched = c("Before Matching",
#                                           "After Matching"), 
#                               label = c("",
#                                         "Estimate = 5.73")), 
#             aes(x = 5, y = 30, label = label),
#             inherit.aes = FALSE,
#             size = 5) +
#   ggtitle(paste("Average", ScoreName[3], 
#                 "Score by Follow Up Time Point",
#                 sep=" ")) +
#   geom_line() +
#   geom_point(aes(x = DaysId,
#                  y = tmp3[, ScoreVarName[3]],
#                  color = col.ggplot),
#              size = 3) +
#   scale_colour_manual("", 
#                       values = c("blue", 
#                                  "darkmagenta",
#                                  "green",
#                                  "red"),
#                       labels = c("Admission",
#                                  "Discharge",
#                                  "Study Group",
#                                  "Control Group")) +
#   scale_x_discrete("Follow Up Time Point",
#                    limits = c("Admission", 
#                               "Discharge", 
#                               "30 Day", 
#                               "60 Day", 
#                               "90 Day", 
#                               "120 Day")) +
#   scale_y_continuous(paste(ScoreName[3], 
#                            "Score", 
#                            sep=" "),
#                      limits = c(20, 65)) +
#   theme(legend.position = "top",
#         plot.title = element_text(hjust = 0.5),        
#         axis.text.x = element_text(angle = 45,
#                                    hjust = 1))
# 
# # combine all three plots
# g = arrangeGrob(p1,p2,p3, nrow = 1)
# 
# # save the plot
# ggsave("media/Functional Outcomes/AM PAC Average Line Graphs (Before and After Matching).png", 
#        g,
#        width = 18,
#        height = 9)

# Create the p value plot ####
# Create dataset ####
for.p.val.plot = cbind.data.frame(for.p.val.plot.mob,
                                  for.p.val.plot.act,
                                  for.p.val.plot.cog)

for.p.val.plot[, "Variable"] = rownames(for.p.val.plot)

rownames(for.p.val.plot) = 1:nrow(for.p.val.plot)

for.p.val.plot = as.data.table(for.p.val.plot)

for.p.val.plot = melt(for.p.val.plot,
                      id.vars = 4,
                      measure.vars = 1:3)

colnames(for.p.val.plot) = c("Variable",
                             "Score Type",
                             "P-Value")

for.p.val.plot = as.data.frame(for.p.val.plot)

for.p.val.plot[for.p.val.plot[, "P-Value"] < 0.001, "P-Value"] = 30
for.p.val.plot[for.p.val.plot[, "P-Value"] < 0.01, "P-Value"] = 20
for.p.val.plot[for.p.val.plot[, "P-Value"] < 0.1, "P-Value"] = 10
for.p.val.plot[for.p.val.plot[, "P-Value"] < 1, "P-Value"] = 0

for.p.val.plot[for.p.val.plot[, "Variable"] == "GroupStudy Group", "Variable"] = "Rehabilitation Group (SRP-participant vs. Control) "
for.p.val.plot[for.p.val.plot[, "Variable"] == "GenderMale", "Variable"] = "Gender (Male vs. Female)"
for.p.val.plot[for.p.val.plot[, "Variable"] == "New.RaceOther", "Variable"] = "Race (Black vs. Other)"
for.p.val.plot[for.p.val.plot[, "Variable"] == "New.RaceWhite", "Variable"] = "Race (Black vs. White)"
for.p.val.plot[for.p.val.plot[, "Variable"] == "Type.of.StrokeISCHEMIC CVA", "Variable"] = "Type of Stroke (Ischemic vs. Hemorrhagic)"
for.p.val.plot[for.p.val.plot[, "Variable"] == "ARHosp.JRI.Facility.Adjustor", "Variable"] = "Medical Complexity"
for.p.val.plot[for.p.val.plot[, "Variable"] == "Days.After.Assignment", "Variable"] = "Days After Group Assignment"

for.p.val.plot[, "Score Type"] = factor(for.p.val.plot[, "Score Type"],
                                        levels = c(levels(for.p.val.plot[, "Score Type"]),
                                                   "Basic Mobility",
                                                   "Daily Activity",
                                                   "Applied Cognitive")[c(1:3,6:4)])

for.p.val.plot[for.p.val.plot[, "Score Type"] == "for.p.val.plot.mob", "Score Type"] = "Basic Mobility"
for.p.val.plot[for.p.val.plot[, "Score Type"] == "for.p.val.plot.act", "Score Type"] = "Daily Activity"
for.p.val.plot[for.p.val.plot[, "Score Type"] == "for.p.val.plot.cog", "Score Type"] = "Applied Cognitive"



for.p.val.plot = for.p.val.plot[for.p.val.plot[, "Variable"] != "(Intercept)",]

for.p.val.plot[, "Variable"] = factor(for.p.val.plot[, "Variable"])

for.p.val.plot[, "Variable"] = factor(for.p.val.plot[, "Variable"],
                                      levels = levels(for.p.val.plot[, "Variable"])[c(4,8,6,5,3,1,7,2)][8:1])

# Create bar graph plot (no color) ####

ggplot(data = for.p.val.plot[for.p.val.plot[, "Variable"] != "Days After Group Assignment", ], 
       aes(x = Variable,
           y = `P-Value`,
           fill = `Score Type`)) +
  scale_x_discrete("") +
  scale_y_continuous("",
                     breaks = c(0,10,20,30), 
                     labels = c("P<0.001",
                                "P<0.01",
                                "P<0.1",
                                "P<1")[4:1], 
                     limits = c(0,30),
                     expand = c(0.001,0)) +
  geom_hline(yintercept = c(0,10,20,30)) +
  ggtitle(expression("Comparison of the Impact of Variables on" ~ AM-PAC^{TM} ~ "Scores up to 120 Days")) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  scale_fill_manual(name = "",
                    values = gray.colors(3, end = 0.8)[3:1],
                    breaks = c("Basic Mobility",
                               "Daily Activity",
                               "Applied Cognitive")) +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 30,
                                   hjust = 1),
        legend.position = "bottom")

ggsave("media/Functional Outcomes/P-Value Bar Graph (no color) (no days after).tiff",
       device = "tiff",
       width = 10,
       height = 6,
       dpi = 300,
       compression = "lzw")

# ggplot(data = for.p.val.plot, 
#        aes(x = Variable,
#            y = `P-Value`,
#            fill = `Score Type`)) +
#   scale_x_discrete("") +
#   scale_y_continuous("",
#                      breaks = 0:4, 
#                      labels = c("P<0.001",
#                                 "P<0.01",
#                                 "P<0.05",
#                                 "P<0.1",
#                                 "P<1")[5:1], 
#                      limits = c(0,4),
#                      expand = c(0.001,0)) +
#   geom_hline(yintercept = 0:4) +
#   ggtitle(expression("Comparison of the Impact of Variables on" ~ AM-PAC^{TM} ~ "Scores up to 120 Days")) +
#   geom_bar(stat = "identity", position = "dodge") +
#   coord_flip() +
#   scale_fill_manual(name = "",
#                     values = gray.colors(3, end = 0.8)[3:1],
#                     breaks = c("Basic Mobility",
#                                "Daily Activity",
#                                "Applied Cognitive")) +
#   theme(plot.title = element_text(hjust = 0.5), 
#         plot.subtitle = element_text(hjust = 0.5),
#         axis.text.x = element_text(angle = 30,
#                                    hjust = 1),
#         legend.position = "bottom")
# 
# ggsave("media/Functional Outcomes/P-Value Bar Graph (no color).tiff",
#        device = "tiff",
#        width = 10,
#        height = 6,
#        dpi = 300,
#        compression = "lzw")
# 
# ggplot(data = for.p.val.plot, 
#        aes(x = Variable,
#            y = `P-Value`,
#            fill = `Score Type`)) +
#   scale_x_discrete("") +
#   scale_y_continuous("",
#                      breaks = 0:4, 
#                      labels = c("P<0.001",
#                                 "P<0.01",
#                                 "P<0.05",
#                                 "P<0.1",
#                                 "P<1")[5:1], 
#                      limits = c(0,4),
#                      expand = c(0.001,0)) +
#   geom_hline(yintercept = 0:4) +
#   ggtitle(expression("Comparison of the Impact of Variables on" ~ AM-PAC^{TM} ~ "Scores up to 120 Days")) +
#   geom_bar(stat = "identity", position = "dodge") +
#   coord_flip() +
#   scale_fill_manual(name = "",
#                     values = gray.colors(3, end = 0.8)[3:1],
#                     breaks = c("Basic Mobility",
#                                "Daily Activity",
#                                "Applied Cognitive")) +
#   geom_text(aes(label = "-",
#                 x = 4.05,
#                 y = 0.5),
#             color = "white",
#             size = 5) +
#   geom_text(aes(label = "-",
#                 x = 3.35,
#                 y = 0.5),
#             color = "white",
#             size = 5) +
#   geom_text(aes(label = "+",
#                 x = 2.3,
#                 y = 0.5),
#             color = "white",
#             size = 5) +
#   geom_text(aes(label = "+",
#                 x = 2,
#                 y = 0.5),
#             color = "white",
#             size = 5) +
#   geom_text(aes(label = "+",
#                 x = 1.7,
#                 y = 0.5),
#             color = "white",
#             size = 5) +
#   geom_text(aes(label = "+",
#                 x = 1.3,
#                 y = 0.5),
#             color = "white",
#             size = 5) +
#   geom_text(aes(label = "+",
#                 x = 1,
#                 y = 0.5),
#             color = "white",
#             size = 5) +
#   geom_text(aes(label = "+",
#                 x = 0.7,
#                 y = 0.5),
#             color = "white",
#             size = 5) +
#   theme(plot.title = element_text(hjust = 0.5), 
#         plot.subtitle = element_text(hjust = 0.5),
#         axis.text.x = element_text(angle = 30,
#                                    hjust = 1),
#         legend.position = "bottom")
# 
# ggsave("media/Functional Outcomes/P-Value Bar Graph (no color) (with direction).tiff",
#        device = "tiff",
#        width = 10,
#        height = 6,
#        dpi = 300,
#        compression = "lzw")
# 
# ggplot(data = for.p.val.plot[for.p.val.plot[, "Variable"] != "Days After Group Assignment",], 
#        aes(x = Variable,
#            y = `P-Value`,
#            fill = `Score Type`)) +
#   scale_x_discrete("") +
#   scale_y_continuous("",
#                      breaks = 0:4, 
#                      labels = c("P<0.001",
#                                 "P<0.01",
#                                 "P<0.05",
#                                 "P<0.1",
#                                 "P<1")[5:1], 
#                      limits = c(0,4),
#                      expand = c(0.001,0)) +
#   geom_hline(yintercept = 0:4) +
#   ggtitle(expression("Comparison of the Impact of Variables on" ~ AM-PAC^{TM} ~ "Scores up to 120 Days")) +
#   geom_bar(stat = "identity", position = "dodge") +
#   coord_flip() +
#   scale_fill_manual(name = "",
#                     values = gray.colors(3, end = 0.8)[3:1],
#                     breaks = c("Basic Mobility",
#                                "Daily Activity",
#                                "Applied Cognitive")) +
#   geom_text(aes(label = "-",
#                 x = 3.05,
#                 y = 0.5),
#             color = "white",
#             size = 5) +
#   geom_text(aes(label = "-",
#                 x = 2.35,
#                 y = 0.5),
#             color = "white",
#             size = 5) +
#   geom_text(aes(label = "+",
#                 x = 1.3,
#                 y = 0.5),
#             color = "white",
#             size = 5) +
#   geom_text(aes(label = "+",
#                 x = 1,
#                 y = 0.5),
#             color = "white",
#             size = 5) +
#   geom_text(aes(label = "+",
#                 x = 0.7,
#                 y = 0.5),
#             color = "white",
#             size = 5) +
#   theme(plot.title = element_text(hjust = 0.5), 
#         plot.subtitle = element_text(hjust = 0.5),
#         axis.text.x = element_text(angle = 30,
#                                    hjust = 1),
#         legend.position = "bottom")
# 
# ggsave("media/Functional Outcomes/P-Value Bar Graph (no color) (with direction) (no days after).tiff",
#        device = "tiff",
#        width = 10,
#        height = 6,
#        dpi = 300,
#        compression = "lzw")

