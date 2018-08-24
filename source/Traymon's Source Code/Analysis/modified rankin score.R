#################################Program Description################################
#Name: modified rankin score                                                       #
#Author: Traymon Beavers                                                           #
#Depends: ggplot2                                                                  #
#Date Created: 7/21/2017                                                           #
#Date Updated: 3/21/2018                                                           #
#Purpose: To perform analysis on the stroke rehabilitation program modified rankin #
#         score data by matching patients in the study group with patients in the  #
#         control group and then conducting various statistical procedures with    #
#         respect to the matched pairs, as well as provide a collection of plots   #
####################################################################################

# Load the necessary source code and functions ####
source("source/Traymon's Source Code/Analysis/create matches.R")
# source("source/Traymon's Source Code/Data Reconfiguration/interpolate.R")
library(ggplot2)
library(gridExtra)
library(lmerTest)

# line2user <- function(line, side) {
#   lh <- par('cin')[2] * par('cex') * par('lheight')
#   x_off <- diff(grconvertX(0:1, 'inches', 'user'))
#   y_off <- diff(grconvertY(0:1, 'inches', 'user'))
#   switch(side,
#          `1` = par('usr')[3] - line * y_off * lh,
#          `2` = par('usr')[1] - line * x_off * lh,
#          `3` = par('usr')[4] + line * y_off * lh,
#          `4` = par('usr')[2] + line * x_off * lh,
#          stop("side must be 1, 2, 3, or 4", call.=FALSE))
# }

# Reconfigure data for plot ####
modrankin.data = as.data.frame(matrix(NA, 60, 6))

colnames(modrankin.data) = c("Group", 
                             "DaysId", 
                             "Follow.Up.Id", 
                             "Score", 
                             "Percent", 
                             "Number")

modrankin.data[, "Group"] = rep(c("Study Group", 
                                  "Control Group"), each = 6)

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
              digits = 4)
      
      modrankin.data[modrankin.data[, "Group"] == i & modrankin.data[, "DaysId"] == j & modrankin.data[, "Score"] == k, "Number"] = 
        length(match.subgroup[match.subgroup[, "Group"] == i & 
                                match.subgroup[, "DaysId"] == j & 
                                is.na(match.subgroup[, "ModRankinScore"]) == 0 &
                                match.subgroup[, "ModRankinScore"] == k, 
                              "ModRankinScore"])
      
      
    }
    
  }
  
}

# save the data table
# write.csv(modrankin.data,
#           "media/Modified Rankin Score/Data Tables/ModRankin_After_Matching.csv",
#           row.names = FALSE)

# Calculate percentages of higher/lower scores for each group/timepoint ####

low.high = list(c(0,2), c(3,5))

low.high.name = c("High",
                  "Low")

for (i in c(3,5,6)){
    
    for (k in 1:2){
      
      for (j in c("Study Group", "Control Group")[2:1]){
      
      print(c(i,j,low.high.name[k]))
      
      print(sum(modrankin.data[modrankin.data[ , "Group"] == j &
                                 modrankin.data[ , "DaysId"] == i &
                                 modrankin.data[ , "Score"] >= low.high[[k]][1] &
                                 modrankin.data[ , "Score"] <= low.high[[k]][2], "Percent"]))
      
    }
    
  }
  
}

# Conduct chi square test of homogeneity for all timepoints ####

# create a table for the number of patients in each category of modrankin score
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

# Conduct two sample Z-test for equal proportions for all timepoints ####

# create a table for the number of patients in each category of modrankin score
tmp.table = matrix(NA, 2, 2)

colnames(tmp.table) = c("0-2", "3-5")

rownames(tmp.table) = c("Study Group", "Control Group")

# conduct a two sample Z-test for daysid = 3
tmp.table[1,1] = sum(modrankin.data[modrankin.data[,"Group"] == "Study Group" &
                                 modrankin.data[,"DaysId"] == 3 &
                                 modrankin.data[,"Score"] >= 0 &
                                 modrankin.data[,"Score"] <= 2, "Number"])

tmp.table[2,1] = sum(modrankin.data[modrankin.data[,"Group"] == "Control Group" &
                                     modrankin.data[,"DaysId"] == 3 &
                                     modrankin.data[,"Score"] >= 0 &
                                     modrankin.data[,"Score"] <= 2, "Number"])

tmp.table[1,2] = sum(modrankin.data[modrankin.data[,"Group"] == "Study Group" &
                                     modrankin.data[,"DaysId"] == 3, "Number"])  - tmp.table[1,1]

tmp.table[2,2] = sum(modrankin.data[modrankin.data[,"Group"] == "Control Group" &
                                      modrankin.data[,"DaysId"] == 3, "Number"]) - tmp.table[2,1]


test1 = prop.test(tmp.table)

# conduct a two sample Z-test for daysid = 5
tmp.table[1,1] = sum(modrankin.data[modrankin.data[,"Group"] == "Study Group" &
                                      modrankin.data[,"DaysId"] == 5 &
                                      modrankin.data[,"Score"] >= 0 &
                                      modrankin.data[,"Score"] <= 2, "Number"])

tmp.table[2,1] = sum(modrankin.data[modrankin.data[,"Group"] == "Control Group" &
                                      modrankin.data[,"DaysId"] == 5 &
                                      modrankin.data[,"Score"] >= 0 &
                                      modrankin.data[,"Score"] <= 2, "Number"])

tmp.table[1,2] = sum(modrankin.data[modrankin.data[,"Group"] == "Study Group" &
                                      modrankin.data[,"DaysId"] == 5, "Number"])  - tmp.table[1,1]

tmp.table[2,2] = sum(modrankin.data[modrankin.data[,"Group"] == "Control Group" &
                                      modrankin.data[,"DaysId"] == 5, "Number"]) - tmp.table[2,1]


test2 = prop.test(tmp.table)

# conduct a two sample Z-test for daysid = 6
tmp.table[1,1] = sum(modrankin.data[modrankin.data[,"Group"] == "Study Group" &
                                      modrankin.data[,"DaysId"] == 6 &
                                      modrankin.data[,"Score"] >= 0 &
                                      modrankin.data[,"Score"] <= 2, "Number"])

tmp.table[2,1] = sum(modrankin.data[modrankin.data[,"Group"] == "Control Group" &
                                      modrankin.data[,"DaysId"] == 6 &
                                      modrankin.data[,"Score"] >= 0 &
                                      modrankin.data[,"Score"] <= 2, "Number"])

tmp.table[1,2] = sum(modrankin.data[modrankin.data[,"Group"] == "Study Group" &
                                      modrankin.data[,"DaysId"] == 6, "Number"])  - tmp.table[1,1]

tmp.table[2,2] = sum(modrankin.data[modrankin.data[,"Group"] == "Control Group" &
                                      modrankin.data[,"DaysId"] == 6, "Number"]) - tmp.table[2,1]


test3 = prop.test(tmp.table)

# Create stacked bar graph plot (no color) ####

modrankin.data[modrankin.data[, "Group"] == "Study Group", "Group"] = "SRP-participant"

modrankin.data[modrankin.data[, "Group"] == "Control Group", "Group"] = "Non-participant"

for.lines2 = c(sum(modrankin.data[modrankin.data[,"Follow.Up.Id"] == "30 Day" &
                                    modrankin.data[,"Score"] >= 3 &
                                    modrankin.data[,"Score"] <= 5 &
                                    modrankin.data[,"Group"] == "SRP-participant", "Percent"]),

               sum(modrankin.data[modrankin.data[,"Follow.Up.Id"] == "30 Day" &
                                    modrankin.data[,"Score"] >= 3 &
                                    modrankin.data[,"Score"] <= 5 &
                                    modrankin.data[,"Group"] == "Non-participant", "Percent"]),

               sum(modrankin.data[modrankin.data[,"Follow.Up.Id"] == "90 Day" &
                                    modrankin.data[,"Score"] >= 3 &
                                    modrankin.data[,"Score"] <= 5 &
                                    modrankin.data[,"Group"] == "SRP-participant", "Percent"]),

               sum(modrankin.data[modrankin.data[,"Follow.Up.Id"] == "90 Day" &
                                    modrankin.data[,"Score"] >= 3 &
                                    modrankin.data[,"Score"] <= 5 &
                                    modrankin.data[,"Group"] == "Non-participant", "Percent"]),

               sum(modrankin.data[modrankin.data[,"Follow.Up.Id"] == "120 Day" &
                                    modrankin.data[,"Score"] >= 3 &
                                    modrankin.data[,"Score"] <= 5 &
                                    modrankin.data[,"Group"] == "SRP-participant", "Percent"]),

               sum(modrankin.data[modrankin.data[,"Follow.Up.Id"] == "120 Day" &
                                    modrankin.data[,"Score"] >= 3 &
                                    modrankin.data[,"Score"] <= 5 &
                                    modrankin.data[,"Group"] == "Non-participant", "Percent"]))

for.lines = data.frame(Follow.Up.Id = rep(c("30 Day",
                                            "90 Day",
                                            "120 Day"),
                                          each = 2),
                       Group = rep(c("SRP participant",
                                     "Non-participant"),
                                   3),
                       high.prop = for.lines2,
                       x1 = rep(c(1.55,0.55),
                                3),
                       x2 = rep(c(2.45,1.45),
                                3))

ggplot(data = modrankin.data[modrankin.data[, "Follow.Up.Id"] != "180 Day" &
                               modrankin.data[, "Follow.Up.Id"] != "365 Day", ],
       aes(x = Group,
           y = Percent,
           fill = Score,
           label = Percent)) +
  facet_wrap(~Follow.Up.Id,
             nrow = 1) +
  scale_x_discrete("Rehabilitation Group") +
  scale_y_continuous("Percent of Patients with Score",
                     labels = paste(seq(0,100,25))) +
  ggtitle("Modified Rankin Scale Score") +
  geom_bar(stat = "identity", position = "fill") +
  geom_segment(data = for.lines,
               aes(x = x1,
                   xend = x2,
                   y = high.prop,
                   yend = high.prop),
               inherit.aes = FALSE,
               lwd = 1.25) +
    geom_text(data = data.frame(Follow.Up.Id = c("30 Day",
                                                 "90 Day",
                                                 "120 Day"),
                                label = c(paste("P-Value", round(test1$p.value,3), sep = " = "),
                                          paste("P-Value", round(test2$p.value,3), sep = " = "),
                                          paste("P-Value", round(test3$p.value,3), sep = " = "))),
              aes(x = 1.5, y = 1.05, label = label),
              inherit.aes = FALSE) +
  scale_fill_manual(name = "Modified Rankin Scale Score",
                    values = gray.colors(6, end = 0.8)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 30,
                                   hjust = 1),
        legend.position = "top")

ggsave("media/Modified Rankin Score/Mod Rankin Score Stacked Bar Graph (After Matching) (6-8-2018).tiff",
       device = "tiff",
       width = 8.25,
       height = 6,
       dpi = 300,
       compression = "lzw")

# create plot without 30 day time point
ggplot(data = modrankin.data[modrankin.data[, "Follow.Up.Id"] != "30 Day" &
                               modrankin.data[, "Follow.Up.Id"] != "180 Day" &
                               modrankin.data[, "Follow.Up.Id"] != "365 Day", ],
       aes(x = Group,
           y = Percent,
           fill = Score,
           label = Percent)) +
  facet_wrap(~Follow.Up.Id,
             nrow = 1) +
  scale_x_discrete("Rehabilitation Group") +
  scale_y_continuous("Percent of Patients with Score",
                     labels = paste(seq(0,100,25))) +
  ggtitle("Modified Rankin Scale Score") +
  geom_bar(stat = "identity", position = "fill") +
  geom_segment(data = for.lines[for.lines[, "Follow.Up.Id"] != "30 Day", ],
               aes(x = x1,
                   xend = x2,
                   y = high.prop,
                   yend = high.prop),
               inherit.aes = FALSE,
               lwd = 1.25) +
  geom_text(data = data.frame(Follow.Up.Id = c("90 Day",
                                               "120 Day"),
                              label = c("P-Value = 0.058",
                                        "P-Value = 0.014")),
            aes(x = 1.5, y = 1.05, label = label),
            inherit.aes = FALSE) +
  scale_fill_manual(name = "Modified Rankin Scale Score",
                    values = gray.colors(6, end = 0.8)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 30,
                                   hjust = 1),
        legend.position = "top")

ggsave("media/Modified Rankin Score/Mod Rankin Score Stacked Bar Graph (After Matching) (No 30 Day) (4-22-2018).tiff",
       device = "tiff",
       width = 8.25,
       height = 6,
       dpi = 300,
       compression = "lzw")

# OBSOLETE CODE ####
# # Create stacked bar graph plot (no color) no ggplot ####
# 
# # modrankin.data[modrankin.data[, "Group"] == "Study Group", "Group"] = "SRP Participant Group"
# # 
# # modrankin.data[modrankin.data[, "Group"] == "Control Group", "Group"] = "Non-Participant Group"
# # 
# # # create a table for the number of patients in each category of modrankin score
# # tmp.table = matrix(NA, 2, 6)
# # 
# # colnames(tmp.table) = c("0", 
# #                         "1", 
# #                         "2", 
# #                         "3", 
# #                         "4", 
# #                         "5")
# # 
# # rownames(tmp.table) = c("SRP Group", 
# #                         "NP Group")
# # 
# # par(mfrow = c(1,4),
# #     mar = c(7,4,5.5,2) + 0.1)
# # 
# # # layout(matrix(c(1,2,3,4),
# # #               nrow = 2,
# # #               ncol = 4,
# # #               byrow = TRUE),
# # #        heights = c(0.4,0.4,0.2))
# # 
# # tmp.table[1,] = modrankin.data[modrankin.data[,"Group"] == "SRP Participant Group" & 
# #                                  modrankin.data[,"DaysId"] == 3, "Percent"]          
# # 
# # tmp.table[2,] = modrankin.data[modrankin.data[,"Group"] == "Non-Participant Group" & 
# #                                  modrankin.data[,"DaysId"] == 3, "Percent"]
# # 
# # barplot(t(tmp.table)[6:1, 2:1],
# #         density = c(7.5,20,10,15,7.5,10),
# #         angle = c(30,60,90,150,120,0),
# #         main = paste("",
# #                      "",
# #                      "P-value = 0.001",
# #                      sep = "\n"),
# #         sub = "30 Days",
# #         ylab = "Percent of Patients with Score",
# #         las = 1)
# # 
# # tmp.table[1,] = modrankin.data[modrankin.data[,"Group"] == "SRP Participant Group" & 
# #                                  modrankin.data[,"DaysId"] == 5, "Percent"]          
# # 
# # tmp.table[2,] = modrankin.data[modrankin.data[,"Group"] == "Non-Participant Group" & 
# #                                  modrankin.data[,"DaysId"] == 5, "Percent"]
# # 
# # barplot(t(tmp.table)[6:1, 2:1],
# #         density = c(7.5,20,10,15,7.5,10),
# #         angle = c(30,60,90,150,120,0),
# #         main = paste("",
# #                      "",
# #                      "P-value = 0.014",
# #                      sep = "\n"),
# #         sub = "90 Days",
# #         axes = FALSE,
# #         las = 1)
# # 
# # tmp.table[1,] = modrankin.data[modrankin.data[,"Group"] == "SRP Participant Group" & 
# #                                  modrankin.data[,"DaysId"] == 6, "Percent"]          
# # 
# # tmp.table[2,] = modrankin.data[modrankin.data[,"Group"] == "Non-Participant Group" & 
# #                                  modrankin.data[,"DaysId"] == 6, "Percent"]
# # 
# # barplot(t(tmp.table)[6:1, 2:1],
# #         density = c(7.5,20,10,15,7.5,10),
# #         angle = c(30,60,90,150,120,0),
# #         main = paste("",
# #                      "",
# #                      "P-value = 0.014",
# #                      sep = "\n"),
# #         sub = "120 Days",
# #         axes = FALSE,
# #         las = 1)
# # 
# # text(line2user(line = 4, side=2),
# #      line2user(line = 4, side = 3), "Modified Rankin Score by Follow Up Time and Rehabilitation Group", 
# #      xpd = NA, cex = 2, font = 2)
# # 
# # 
# # plot(1,
# #      type = "n",
# #      axes = FALSE,
# #      xlab = "",
# #      ylab = "")
# # 
# # legend(x = "center",
# #        inset = 0,
# #        legend = c("0","1","2","3","4","5"), 
# #        density = c(7.5,20,10,15,7.5,10)[6:1],
# #        angle = c(30,60,90,150,120,0)[6:1],
# #        horiz = FALSE,
# #        cex = 3)
# # 
# # dev.off()
# # 
# # tiff("media/Modified Rankin Score/Mod Rankin Score Stacked Bar Graph (After Matching) (no color).tiff",
# #      width = 8.25, 
# #      height = 6, 
# #      units = "in",
# #      compression = "lzw",
# #      res = 300)

# # Fit a mixed effects linear model for mobility SRP group ####
# tmp.delete = -c(3:7,12,13:15,24:28,30:36,38:44,69:87,90,92,94:112)
# 
# tmp.study = Interpolate.Master[Interpolate.Master[, "Group"] == "Study Group",tmp.delete]
# 
# tmp.study[, "MRS.Diff.From.Baseline"] = tmp.study[, "ModRankinScore"] - 
#   tmp.study[, "MRS.Baseline"]
# 
# tmp.study = tmp.study[tmp.study[, "DaysId"] <= 6 &
#                         tmp.study[, "DaysId"] >= 4, ]
# 
# #View(tmp.study[,c("DaysId","PT.AM.PAC.Basic.Mobility.Score","Discharge.Mobility","Mobility.Diff.From.Baseline")])
# 
# tmp.study = tmp.study[,-c(42:44,46,47:52)]
# 
# colnames(tmp.study)
# 
# tmp.study[,"Type.of.Stroke"] = relevel(tmp.study[,"Type.of.Stroke"],
#                                        ref = "ISCHEMIC CVA")
# 
# tmp.study[,"New.Race"] = factor(tmp.study[,"New.Race"],
#                                 levels = c("Black",
#                                            "White",
#                                            "Other"))
# 
# fmla = as.formula(paste("MRS.Diff.From.Baseline ~ ", 
#                         paste(c(colnames(tmp.study)[c(2:4,7,16,42,44)],
#                                 "(1 | ID)"), 
#                               collapse="+")))
# 
# lmer.fit = lmerTest::lmer(data = tmp.study, fmla)
# 
# lmer.sum = summary(lmer.fit)
# 
# for.table = lmer.sum$coefficients
# 
# for.table = as.data.frame(for.table)
# 
# for.table[, "Conf.int"] = paste("(",
#                                 round(for.table[, "Estimate"] - for.table[, "Std. Error"]*qt(0.975, df = for.table[, "df"]),2),
#                                 ", ",
#                                 round(for.table[, "Estimate"] + for.table[, "Std. Error"]*qt(0.975, df = for.table[, "df"]),2),
#                                 ")",
#                                 sep = "")
# 
# for.table = for.table[, -c(2:4)]
# 
# for.table = for.table[, c(1,3,2)]
# 
# for.table[,1] = round(for.table[,1],2)
# for.table[,3] = round(for.table[,3],3)
# 
# write.csv(for.table,
#           "docs/MELM Results SRP MRS.csv",
#           row.names = TRUE)
# 
# # Fit a mixed effects linear model for mobility NP group ####
# tmp.delete = -c(3:7,12,13:15,24:28,30:36,38:44,69:87,90,92,94:112)
# 
# tmp.control = Interpolate.Master[Interpolate.Master[, "Group"] == "Control Group",tmp.delete]
# 
# tmp.control[, "MRS.Diff.From.Baseline"] = tmp.control[, "ModRankinScore"] - 
#   tmp.control[, "MRS.Baseline"]
# 
# tmp.control = tmp.control[tmp.control[, "DaysId"] <= 6 &
#                             tmp.control[, "DaysId"] >= 4, ]
# 
# #View(tmp.control[,c("DaysId","PT.AM.PAC.Basic.Mobility.Score","Discharge.Mobility","Mobility.Diff.From.Baseline")])
# 
# tmp.control = tmp.control[,-c(42:44,46,47:52)]
# 
# colnames(tmp.control)
# 
# tmp.control[,"Type.of.Stroke"] = relevel(tmp.control[,"Type.of.Stroke"],
#                                        ref = "ISCHEMIC CVA")
# 
# tmp.control[,"New.Race"] = factor(tmp.control[,"New.Race"],
#                                 levels = c("Black",
#                                            "White",
#                                            "Other"))
# 
# fmla = as.formula(paste("MRS.Diff.From.Baseline ~ ", 
#                         paste(c(colnames(tmp.control)[c(2:4,7,16,42,44)],
#                                 "(1 | ID)"), 
#                               collapse="+")))
# 
# lmer.fit = lmerTest::lmer(data = tmp.control, fmla)
# 
# lmer.sum = summary(lmer.fit)
# 
# for.table = lmer.sum$coefficients
# 
# for.table = as.data.frame(for.table)
# 
# for.table[, "Conf.int"] = paste("(",
#                                 round(for.table[, "Estimate"] - for.table[, "Std. Error"]*qt(0.975, df = for.table[, "df"]),2),
#                                 ", ",
#                                 round(for.table[, "Estimate"] + for.table[, "Std. Error"]*qt(0.975, df = for.table[, "df"]),2),
#                                 ")",
#                                 sep = "")
# 
# for.table = for.table[, -c(2:4)]
# 
# for.table = for.table[, c(1,3,2)]
# 
# for.table[,1] = round(for.table[,1],2)
# for.table[,3] = round(for.table[,3],3)
# 
# write.csv(for.table,
#           "docs/MELM Results NP MRS.csv",
#           row.names = TRUE)


# # Create stacked bar graph plot (no color) (no matching) ####
# 
# modrankin.data[modrankin.data[, "Group"] == "Study Group", "Group"] = "SRP Participant"
# 
# modrankin.data[modrankin.data[, "Group"] == "Control Group", "Group"] = "Non-participant"
# 
# for.lines2 = c(sum(modrankin.data[modrankin.data[,"Follow.Up.Id"] == "30 Day" &
#                                     modrankin.data[,"Score"] >= 3 & 
#                                     modrankin.data[,"Score"] <= 5 &
#                                     modrankin.data[,"Group"] == "SRP Participant", "Percent"]),
#                
#                sum(modrankin.data[modrankin.data[,"Follow.Up.Id"] == "30 Day" &
#                                     modrankin.data[,"Score"] >= 3 & 
#                                     modrankin.data[,"Score"] <= 5 &
#                                     modrankin.data[,"Group"] == "Non-participant", "Percent"]),
#                
#                sum(modrankin.data[modrankin.data[,"Follow.Up.Id"] == "90 Day" &
#                                     modrankin.data[,"Score"] >= 3 & 
#                                     modrankin.data[,"Score"] <= 5 &
#                                     modrankin.data[,"Group"] == "SRP Participant", "Percent"]),
#                
#                sum(modrankin.data[modrankin.data[,"Follow.Up.Id"] == "90 Day" &
#                                     modrankin.data[,"Score"] >= 3 & 
#                                     modrankin.data[,"Score"] <= 5 &
#                                     modrankin.data[,"Group"] == "Non-participant", "Percent"]),
#                
#                sum(modrankin.data[modrankin.data[,"Follow.Up.Id"] == "120 Day" &
#                                     modrankin.data[,"Score"] >= 3 & 
#                                     modrankin.data[,"Score"] <= 5 &
#                                     modrankin.data[,"Group"] == "SRP Participant", "Percent"]),
#                
#                sum(modrankin.data[modrankin.data[,"Follow.Up.Id"] == "120 Day" &
#                                     modrankin.data[,"Score"] >= 3 & 
#                                     modrankin.data[,"Score"] <= 5 &
#                                     modrankin.data[,"Group"] == "Non-participant", "Percent"]))
# 
# for.lines = data.frame(Follow.Up.Id = rep(c("30 Day",
#                                             "90 Day",
#                                             "120 Day"), 
#                                           each = 2),
#                        Group = rep(c("SRP-participant",
#                                      "Control"),
#                                    3),
#                        high.prop = for.lines2,
#                        x1 = rep(c(1.55,0.55),
#                                 3),
#                        x2 = rep(c(2.45,1.45),
#                                 3))
# 
# ggplot(data = modrankin.data[modrankin.data[, "Follow.Up.Id"] != "180 Day" &
#                                modrankin.data[, "Follow.Up.Id"] != "365 Day", ], 
#        aes(x = Group,
#            y = Percent,
#            fill = Score,
#            label = Percent)) +
#   facet_wrap(~Follow.Up.Id, 
#              nrow = 1) +
#   scale_x_discrete("Rehabilitation Group") +
#   scale_y_continuous("Percent of Patients with Score",
#                      labels = paste(seq(0,100,25))) +
#   ggtitle("Modified Rankin Scale Score") +
#   geom_bar(stat = "identity", position = "fill") +
#   geom_segment(data = for.lines,
#                aes(x = x1,
#                    xend = x2,
#                    y = high.prop,
#                    yend = high.prop),
#                inherit.aes = FALSE,
#                lwd = 1.25) +
#   scale_fill_manual(name = "Modified Rankin Scale Score",
#                     values = gray.colors(6, end = 0.8)) +
#   theme(plot.title = element_text(hjust = 0.5), 
#         plot.subtitle = element_text(hjust = 0.5), 
#         axis.text.x = element_text(angle = 30,
#                                    hjust = 1),
#         legend.position = "top")
# 
# ggsave("media/Modified Rankin Score/Mod Rankin Score Stacked Bar Graph (No Matching) (no color).tiff",
#        device = "tiff",
#        width = 8.25,
#        height = 6,
#        dpi = 300, 
#        compression = "lzw")

# # Create stacked bar graph plot ####
# 
# modrankin.data[modrankin.data[, "Group"] == "Study Group", "Group"] = "SRP Participant Group"
# 
# modrankin.data[modrankin.data[, "Group"] == "Control Group", "Group"] = "Non-Participant Group"
# 
# ggplot(data = modrankin.data[modrankin.data[, "Follow.Up.Id"] != "180 Day" &
#                                modrankin.data[, "Follow.Up.Id"] != "365 Day", ], 
#        aes(x = Group,
#            y = Percent,
#            fill = Score,
#            label = Percent)) +
#   facet_wrap(~Follow.Up.Id, 
#              nrow = 1) +
#   geom_text(data = data.frame(Follow.Up.Id = c("30 Day",
#                                                "90 Day",
#                                                "120 Day"), 
#                               label = c("P-Value < 0.001",
#                                         "P-Value = 0.014",
#                                         "P-Value = 0.040")), 
#             aes(x = 1.5, y = 1.05, label = label),
#             inherit.aes = FALSE) +
#   scale_x_discrete("Rehabilitation Group") +
#   scale_y_continuous("Percent of Patients with Score") +
#   ggtitle("Modified Rankin Score by Follow Up Time") +
#   geom_bar(stat = "identity", position = "fill") +
#   scale_fill_manual(name = "Modified Rankin Score",
#                     values = c("green", "blue", "cadetblue3", "yellow", "orange", "red")) +
#   theme(plot.title = element_text(hjust = 0.5), 
#         plot.subtitle = element_text(hjust = 0.5), 
#         axis.text.x = element_text(angle = 30,
#                                    hjust = 1),
#         legend.position = "top")
# 
# ggsave("media/Modified Rankin Score/Mod Rankin Score Stacked Bar Graph (After Matching).tiff",
#        device = "tiff",
#        width = 8.25,
#        height = 6,
#        dpi = 300, 
#        compression = "lzw")
# 
