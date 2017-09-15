#################################Program Description################################
#Name: modified rankin score                                                       #
#Author: Traymon Beavers                                                           #
#Depends: ggplot                                                                   #
#Date Created: 7/21/2017                                                           #
#Date Updated: 9/14/2017                                                           #
#Purpose: To perform analysis on the stroke rehabilitation program mortality data  #
#         by matching patients in the study group with patients in the control     #
#         group and then conducting various statistical procedures with respect to #
#         the matched pairs, as well as provide a collection of plots              #                                                                    #
####################################################################################

# Conduct chi square test of homogeneity for all timepoints ####

modrankin.data = as.data.frame(matrix(NA, 60, 6))

colnames(modrankin.data) = c("Group", "DaysId", "Follow.Up.Id", "Score", "Percent", "Number")

modrankin.data[, "Group"] = rep(c("Study Group", "Control Group"), each = 6)

modrankin.data[, "Follow.Up.Id"] = rep(c("30 Day", 
                                  "90 Day", 
                                  "120 Day", 
                                  "180 Day",
                                  "365 Day"), each = 12)

modrankin.data[, "Follow.Up.Id"] = factor(modrankin.data[, "Follow.Up.Id"], 
                                   levels = c("30 Day", 
                                              "90 Day", 
                                              "120 Day", 
                                              "180 Day",
                                              "365 Day"))

modrankin.data[, "DaysId"] = rep(c(3,5:8), each = 12)

modrankin.data[, "Score"] = rep(as.character(c(0:5)), 10)

for (i in unique(modrankin.data[, "Group"])){
  
  for (j in unique(modrankin.data[, "DaysId"])){
    
    for (k in unique(modrankin.data[, "Score"])){
      
      modrankin.data[modrankin.data[, "Group"] == i & modrankin.data[, "DaysId"] == j & modrankin.data[, "Score"] == k, "Percent"] = 
        round(length(match.subgroup[match.subgroup[, "Group"] == i & 
                                      match.subgroup[, "DaysId"] == j & 
                                      is.na(match.subgroup[, "ModRankinScore"]) == 0 &
                                      match.subgroup[, "ModRankinScore"] == k, 
                                    "ModRankinScore"])/length(match.subgroup[match.subgroup[, "Group"] == i & 
                                                                               match.subgroup[, "DaysId"] == j & 
                                                                               is.na(match.subgroup[, "ModRankinScore"]) == 0,
                                                                             "ModRankinScore"]), 
              digits = 2)
      
      modrankin.data[modrankin.data[, "Group"] == i & modrankin.data[, "DaysId"] == j & modrankin.data[, "Score"] == k, "Number"] = 
        length(match.subgroup[match.subgroup[, "Group"] == i & 
                                match.subgroup[, "DaysId"] == j & 
                                is.na(match.subgroup[, "ModRankinScore"]) == 0 &
                                match.subgroup[, "ModRankinScore"] == k, 
                              "ModRankinScore"])
      
      
    }
    
  }
  
}

# create a table for the number of patients in each category of race
tmp.table = matrix(NA, 2, 6)

colnames(tmp.table) = c("0", "1", "2", "3", "4", "5")

rownames(tmp.table) = c("Study Group", "Control Group")

# conduct a chi square test for daysid = 3
tmp.table[1,] = modrankin.data[modrankin.data[,"Group"] == "Study Group" & 
                          modrankin.data[,"DaysId"] == 3, "Number"]          

tmp.table[2,] = modrankin.data[modrankin.data[,"Group"] == "Control Group" & 
                          modrankin.data[,"DaysId"] == 3, "Number"]
chisq.test(tmp.table[,-1])

# conduct a chi square test for daysid = 5
tmp.table[1,] = modrankin.data[modrankin.data[,"Group"] == "Study Group" & 
                          modrankin.data[,"DaysId"] == 5, "Number"]

tmp.table[2,] = modrankin.data[modrankin.data[,"Group"] == "Control Group" & 
                          modrankin.data[,"DaysId"] == 5, "Number"]


chisq.test(tmp.table)

# conduct a chi square test for daysid = 6
tmp.table[1,] = modrankin.data[modrankin.data[,"Group"] == "Study Group" & 
                                 modrankin.data[,"DaysId"] == 6, "Number"]

tmp.table[2,] = modrankin.data[modrankin.data[,"Group"] == "Control Group" & 
                                 modrankin.data[,"DaysId"] == 6, "Number"]

chisq.test(tmp.table)






# 6d. ModRankinScore plot ####
# 6d1. Split/stacked bar graph ####
# Create dataset ####

for.6d1 = as.data.frame(matrix(NA, 60, 5))

colnames(for.6d1) = c("Group", "DaysId", "Follow.Up.Id", "Score", "Percent")

for.6d1[, "Group"] = rep(c("Study Group", "Control Group"), each = 6)

for.6d1[, "Follow.Up.Id"] = rep(c("30 Day", 
                                  "90 Day", 
                                  "120 Day", 
                                  "180 Day",
                                  "365 Day"), each = 12)

for.6d1[, "Follow.Up.Id"] = factor(for.6d1[, "Follow.Up.Id"], 
                                   levels = c("30 Day", 
                                              "90 Day", 
                                              "120 Day", 
                                              "180 Day",
                                              "365 Day"))

for.6d1[, "DaysId"] = rep(c(3,5:8), each = 12)

for.6d1[, "Score"] = rep(as.character(c(0:5)), 10)

for (i in unique(for.6d1[, "Group"])){
  
  for (j in unique(for.6d1[, "DaysId"])){
    
    for (k in unique(for.6d1[, "Score"])){
      
      for.6d1[for.6d1[, "Group"] == i & for.6d1[, "DaysId"] == j & for.6d1[, "Score"] == k, "Percent"] = 
        round(length(NewMaster[NewMaster[, "Group"] == i & 
                                 NewMaster[, "DaysId"] == j & 
                                 is.na(NewMaster[, "ModRankinScore"]) == 0 &
                                 NewMaster[, "ModRankinScore"] == k, 
                               "ModRankinScore"])/length(NewMaster[NewMaster[, "Group"] == i & 
                                                                     NewMaster[, "DaysId"] == j & 
                                                                     is.na(NewMaster[, "ModRankinScore"]) == 0,
                                                                   "ModRankinScore"]), 
              digits = 2)
      
      
    }
    
  }
  
}

# write.csv(for.6d1, 
#           "tmp/Plot Data Tables/ModRankinScore.csv",
#           row.names = FALSE)



# Create plot ####

ggplot(data = for.6d1, 
       aes(x = Group,
           y = Percent,
           fill = Score,
           label = Percent)) +
  facet_wrap(~Follow.Up.Id, 
             nrow = 1) +
  scale_x_discrete("Rehabilitation Group") +
  scale_y_continuous("") +
  ggtitle("Modified Rank in Score by Follow Up Time",
          subtitle = "Area of bar color is percentage of patients with that score") +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(name = "Modified Rank in Score",
                    values = c("green", "blue", "cadetblue3", "yellow", "orange", "red")) +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "top")

ggsave("C:/git_local/jfk.cvi/Plots/Paper Plots/Mod Rankin Score Stacked Bar Graph (No Matching).png",
       width = 16,
       height = 9)

# 6d2. Split/stacked bar graph (Matching)####
# Create dataset ####
modrankin.data = as.data.frame(matrix(NA, 60, 6))

colnames(modrankin.data) = c("Group", "DaysId", "Follow.Up.Id", "Score", "Percent", "Number")

modrankin.data[, "Group"] = rep(c("Study Group", "Control Group"), each = 6)

modrankin.data[, "Follow.Up.Id"] = rep(c("30 Day", 
                                  "90 Day", 
                                  "120 Day", 
                                  "180 Day",
                                  "365 Day"), each = 12)

modrankin.data[, "Follow.Up.Id"] = factor(modrankin.data[, "Follow.Up.Id"], 
                                   levels = c("30 Day", 
                                              "90 Day", 
                                              "120 Day", 
                                              "180 Day",
                                              "365 Day"))

modrankin.data[, "DaysId"] = rep(c(3,5:8), each = 12)

modrankin.data[, "Score"] = rep(as.character(c(0:5)), 10)

for (i in unique(modrankin.data[, "Group"])){
  
  for (j in unique(modrankin.data[, "DaysId"])){
    
    for (k in unique(modrankin.data[, "Score"])){
      
      modrankin.data[modrankin.data[, "Group"] == i & modrankin.data[, "DaysId"] == j & modrankin.data[, "Score"] == k, "Percent"] = 
        round(length(match.subgroup[match.subgroup[, "Group"] == i & 
                                      match.subgroup[, "DaysId"] == j & 
                                      is.na(match.subgroup[, "ModRankinScore"]) == 0 &
                                      match.subgroup[, "ModRankinScore"] == k, 
                                    "ModRankinScore"])/length(match.subgroup[match.subgroup[, "Group"] == i & 
                                                                               match.subgroup[, "DaysId"] == j & 
                                                                               is.na(match.subgroup[, "ModRankinScore"]) == 0,
                                                                             "ModRankinScore"]), 
              digits = 2)
      
      modrankin.data[modrankin.data[, "Group"] == i & modrankin.data[, "DaysId"] == j & modrankin.data[, "Score"] == k, "Number"] = 
        length(match.subgroup[match.subgroup[, "Group"] == i & 
                                match.subgroup[, "DaysId"] == j & 
                                is.na(match.subgroup[, "ModRankinScore"]) == 0 &
                                match.subgroup[, "ModRankinScore"] == k, 
                              "ModRankinScore"])
      
      
    }
    
  }
  
}

# write.csv(modrankin.data, 
#           "tmp/Plot Data Tables/ModRankinScore(Matching).csv",
#           row.names = FALSE)



# Create plot ####

ggplot(data = modrankin.data, 
       aes(x = Group,
           y = Percent,
           fill = Score,
           label = Percent)) +
  facet_wrap(~Follow.Up.Id, 
             nrow = 1) +
  geom_text(data = data.frame(Follow.Up.Id = c("30 Day",
                                               "90 Day",
                                               "120 Day",
                                               "180 Day",
                                               "365 Day"), 
                              label = c("P-Value < 0.001",
                                        "P-Value = 0.014",
                                        "P-Value = 0.040",
                                        "P-Value = 0.320",
                                        "P-Value = 0.448")), 
            aes(x = 1.5, y = 1.05, label = label),
            inherit.aes = FALSE) +
  scale_x_discrete("Rehabilitation Group") +
  scale_y_continuous("") +
  ggtitle("Modified Rank in Score by Follow Up Time (After Matching)",
          subtitle = "Area of bar color is percentage of patients with that score") +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(name = "Modified Rank in Score",
                    values = c("green", "blue", "cadetblue3", "yellow", "orange", "red")) +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "top")

ggsave("C:/git_local/jfk.cvi/Plots/Paper Plots/Mod Rankin Score Stacked Bar Graph (After Matching).png",
       width = 16,
       height = 9)


