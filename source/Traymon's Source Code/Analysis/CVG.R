#################################Program Description################################
#Name: CVG                                                                         #
#Author: Traymon Beavers                                                           #
#Depends: upload data.R, ggplot2, lmerTest                                         #
#Date Created: 7/21/2017                                                           #
#Date Updated: 6/11/2018                                                           #
#Purpose: To perform analysis on the stroke rehabilitation program cardiovascular  #
#         group data, as well as provide a collection of plots                     #
####################################################################################

# Load the necessary source code and packages ####
source("source/Traymon's Source Code/Analysis/create matches.R")
library(ggplot2)
library(lmerTest)

# Reconfigure data for analysis ####

CVG.data.wide = NewMaster[NewMaster[,"Group"] == "Study Group" & 
                            NewMaster[,"DaysId"] == 11 &
                            is.na(NewMaster[,"CVG.Baseline"]) == 0 &
                            NewMaster[,"CVG.Baseline"] != 0, c("ID", 
                                                               "CVG.Baseline",
                                                               "CVG.9.Met.Minutes",
                                                               "CVG.18.Met.Minutes",
                                                               "CVG.27.Met.Minutes",
                                                               "CVG.36.Met.Minutes",
                                                               "CVG.Freq",
                                                               "CVG.Total",
                                                               "CVG.Participant_Descr")]

CVG.data.long = as.data.frame(matrix(NA,
                                     5*dim(CVG.data.wide)[1],
                                     7))

colnames(CVG.data.long) = c("ID",
                            "Follow.Up",
                            "Met.Minutes",
                            "Freq",
                            "Total",
                            "Participation Level",
                            "Baseline.Met.Minutes")

CVG.data.long[, "ID"] = rep(CVG.data.wide[, "ID"],
                            each = 5)

CVG.data.long[, "Follow.Up"] = c("Baseline",
                                 "9 Sessions",
                                 "18 Sessions",
                                 "27 Sessions",
                                 "36 Sessions")


for (i in unique(CVG.data.wide[, "ID"])){
  
  for (j in 1:5){
    
    CVG.data.long[CVG.data.long[, "ID"] == i &
                    CVG.data.long[, "Follow.Up"] == unique(CVG.data.long[, "Follow.Up"])[j], 
                  "Met.Minutes"] = CVG.data.wide[CVG.data.wide[, "ID"] == i, colnames(CVG.data.wide)[j+1]]
    
  }
  
  CVG.data.long[CVG.data.long[, "ID"] == i, "Freq"] = 
    CVG.data.wide[CVG.data.wide[, "ID"] == i, "CVG.Freq"]
  
  CVG.data.long[CVG.data.long[, "ID"] == i, "Total"] = 
    CVG.data.wide[CVG.data.wide[, "ID"] == i, "CVG.Total"]
  
  CVG.data.long[CVG.data.long[, "ID"] == i, "Baseline.Met.Minutes"] = 
    CVG.data.wide[CVG.data.wide[, "ID"] == i, "CVG.Baseline"]
  
  CVG.data.long[CVG.data.long[, "ID"] == i, "Participation Level"] = 
    as.character(CVG.data.wide[CVG.data.wide[, "ID"] == i, "CVG.Participant_Descr"])
  
  
}

CVG.data.long[, "Diff.from.Baseline"] = CVG.data.long[, "Met.Minutes"] - CVG.data.long[, "Baseline.Met.Minutes"] 

CVG.data.long[, "Percent.Improvement.from.Baseline"] = CVG.data.long[, "Diff.from.Baseline"]/CVG.data.long[, "Baseline.Met.Minutes"] 

CVG.data.long.2 = CVG.data.long[CVG.data.long[, "Follow.Up"] != "Baseline", ]

CVG.data.long.2[, "Number of Sessions"] = c(9,18,27,36)

table(CVG.data.long.2[,"Participation Level"])/4

length(unique(CVG.data.long.2[,"ID"]))

mean(CVG.data.long.2[CVG.data.long.2[,"Participation Level"] == "FULL","Total"])

mean(CVG.data.long.2[CVG.data.long.2[,"Participation Level"] == "PARTIAL","Total"])

# Fit a mixed effects linear model ####

word = lmerTest::lmer(data = CVG.data.long.2, 
                      Diff.from.Baseline ~ `Number of Sessions` + Freq + (1|ID))

lmer.sum = summary(word)

for.table = lmer.sum$coefficients

for.table = as.data.frame(for.table)

for.table[, "Conf.int"] = paste("(",
                                round(for.table[, "Estimate"] - for.table[, "Std. Error"]*qt(0.975, df = for.table[, "df"]),2),
                                ", ",
                                round(for.table[, "Estimate"] + for.table[, "Std. Error"]*qt(0.975, df = for.table[, "df"]),2),
                                ")",
                                sep = "")

for.table = for.table[, -c(2:4)]

for.table = for.table[, c(1,3,2)]

for.table[,1] = round(for.table[,1],2)
for.table[,3] = round(for.table[,3],3)

write.csv(for.table,
          "docs/MELM Results CVG.csv",
          row.names = TRUE)

# Perform a one sided one sample test ####

CI1 = t.test(CVG.data.long.2[CVG.data.long.2[, "Follow.Up"] == "9 Sessions", "Diff.from.Baseline"])

CI2 = t.test(CVG.data.long.2[CVG.data.long.2[, "Follow.Up"] == "18 Sessions", "Diff.from.Baseline"])

CI3 = t.test(CVG.data.long.2[CVG.data.long.2[, "Follow.Up"] == "27 Sessions", "Diff.from.Baseline"])

CI4 = t.test(CVG.data.long.2[CVG.data.long.2[, "Follow.Up"] == "36 Sessions", "Diff.from.Baseline"])

CI5 = t.test(CVG.data.long.2[CVG.data.long.2[, "Follow.Up"] == "9 Sessions", "Percent.Improvement.from.Baseline"])

CI6 = t.test(CVG.data.long.2[CVG.data.long.2[, "Follow.Up"] == "18 Sessions", "Percent.Improvement.from.Baseline"])

CI7 = t.test(CVG.data.long.2[CVG.data.long.2[, "Follow.Up"] == "27 Sessions", "Percent.Improvement.from.Baseline"])

CI8 = t.test(CVG.data.long.2[CVG.data.long.2[, "Follow.Up"] == "36 Sessions", "Percent.Improvement.from.Baseline"])

# Create bar graphs ####
# Create dataset ####

tmp = colMeans(CVG.data.wide[,-9], na.rm = TRUE)[-c(1,7,8)]


tmp = rbind.data.frame(tmp,
                       100*(tmp-tmp[1])/tmp[1])

colnames(tmp) = c("Baseline",
                  "9 Sessions",
                  "18 Sessions",
                  "27 Sessions",
                  "36 Sessions")

tmp = as.data.frame(t(tmp))

colnames(tmp) = c("Average.METS-Min",
                  "Average.Percent.Improvement.from.Baseline")

tmp[, "Number.of.Sessions"] = factor(rownames(tmp),
                                     levels = rownames(tmp))

tmp[, "Group"] = "Filler"

rownames(tmp) = 1:dim(tmp)[1]

tmp2 = as.data.frame(matrix(NA, nrow = 10, ncol = 3))

colnames(tmp2) = c("METS-Min", 
                   "Number.of.Sessions", 
                   "Type")

tmp2[,1] = c(tmp[1:5,1],tmp[1:5,2])

tmp2[,2] = rep(tmp[,3],2)

tmp2[,3] = rep(c("METs-min","Average Percent Improvement from Baseline"),
                 each = 5)

tmp2[,3] = factor(tmp2[,3],
                  levels = c("METs-min","Average Percent Improvement from Baseline"))

write.csv(tmp2,
          "media/CVG/Data Tables/CVGMetsMin.csv",
          row.names = FALSE)

# Side by side bar graph (no color) ####

# make font sizes for all succeeding plots bigger
theme_set(theme_grey(base_size = 15))

ggplot(data = tmp2, 
       aes(x = Number.of.Sessions,
           y = `METS-Min`)) +
  facet_wrap(~Type, nrow = 1) +
  scale_x_discrete("Number of Sessions") +
  scale_y_continuous("METs-min") +
  ggtitle("METs-min") +
  geom_bar(stat = "identity", 
           position = "identity",
           fill = "black") +
  geom_text(aes(label = c(round(tmp2[1:5, "METS-Min"],0)
                          ,paste(round(tmp2[6:10, "METS-Min"],0),
                                 "%",
                                 sep = "")),
                vjust = -0.3,
                size = 5)) +
  geom_text(aes(label = rep(c("", rep("P < 0.001",4)),2),
                x = c(1:5,1:5),
                y = rep(15,10)),
            size = 4,
            color = "white") +
  geom_text(aes(label = rep(c("", rep("95% CI",4)),2),
                x = c(1:5,1:5),
                y = rep(10,10)),
            size = 4,
            color = "white") +
  geom_text(aes(label = c("", 
                          paste("(", round(CI1$conf.int[1],2), ",", round(CI1$conf.int[2],2), ")",sep = ""),
                          paste("(", round(CI2$conf.int[1],2), ",", round(CI2$conf.int[2],2), ")",sep = ""),
                          paste("(", round(CI3$conf.int[1],2), ",", round(CI3$conf.int[2],2), ")",sep = ""),
                          paste("(", round(CI4$conf.int[1],2), ",", round(CI4$conf.int[2],2), ")",sep = ""),
                          "",
                          paste("(", round(CI5$conf.int[1],2), ",", round(CI5$conf.int[2],2), ")",sep = ""),
                          paste("(", round(CI6$conf.int[1],2), ",", round(CI6$conf.int[2],2), ")",sep = ""),
                          paste("(", round(CI7$conf.int[1],2), ",", round(CI7$conf.int[2],2), ")",sep = ""),
                          paste("(", round(CI8$conf.int[1],2), ",", round(CI8$conf.int[2],2), ")",sep = "")),
                x = c(1:5,1:5),
                y = rep(5,10)),
            size = 4,
            color = "white") +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")

ggsave("media/CVG/Side by Side Bar Graph (no color, P & CI) (6-11-18).tiff",
       device = "tiff",
       width = 13,
       height = 6, 
       dpi = 300,
       compression = "lzw")



# Matched Group ####
# Reconfigure data for analysis ####

CVG.data.wide = NewMaster[NewMaster[,"Group"] == "Study Group" & 
                            NewMaster[,"DaysId"] == 11 &
                            is.na(NewMaster[,"CVG.Baseline"]) == 0 &
                            NewMaster[,"CVG.Baseline"] != 0 & 
                            NewMaster[, "ID"] %in% match.subgroup.One[, "ID"], 
                          c("ID", 
                            "CVG.Baseline",
                            "CVG.9.Met.Minutes",
                            "CVG.18.Met.Minutes",
                            "CVG.27.Met.Minutes",
                            "CVG.36.Met.Minutes",
                            "CVG.Freq",
                            "CVG.Total",
                            "CVG.Participant_Descr")]

CVG.data.long = as.data.frame(matrix(NA,
                                     5*dim(CVG.data.wide)[1],
                                     7))

colnames(CVG.data.long) = c("ID",
                            "Follow.Up",
                            "Met.Minutes",
                            "Freq",
                            "Total",
                            "Participation Level",
                            "Baseline.Met.Minutes")

CVG.data.long[, "ID"] = rep(CVG.data.wide[, "ID"],
                            each = 5)

CVG.data.long[, "Follow.Up"] = c("Baseline",
                                 "9 Sessions",
                                 "18 Sessions",
                                 "27 Sessions",
                                 "36 Sessions")


for (i in unique(CVG.data.wide[, "ID"])){
  
  for (j in 1:5){
    
    CVG.data.long[CVG.data.long[, "ID"] == i &
                    CVG.data.long[, "Follow.Up"] == unique(CVG.data.long[, "Follow.Up"])[j], 
                  "Met.Minutes"] = CVG.data.wide[CVG.data.wide[, "ID"] == i, colnames(CVG.data.wide)[j+1]]
    
  }
  
  CVG.data.long[CVG.data.long[, "ID"] == i, "Freq"] = 
    CVG.data.wide[CVG.data.wide[, "ID"] == i, "CVG.Freq"]
  
  CVG.data.long[CVG.data.long[, "ID"] == i, "Total"] = 
    CVG.data.wide[CVG.data.wide[, "ID"] == i, "CVG.Total"]
  
  CVG.data.long[CVG.data.long[, "ID"] == i, "Baseline.Met.Minutes"] = 
    CVG.data.wide[CVG.data.wide[, "ID"] == i, "CVG.Baseline"]
  
  CVG.data.long[CVG.data.long[, "ID"] == i, "Participation Level"] = 
    as.character(CVG.data.wide[CVG.data.wide[, "ID"] == i, "CVG.Participant_Descr"])
  
  
}

CVG.data.long[, "Diff.from.Baseline"] = CVG.data.long[, "Met.Minutes"] - CVG.data.long[, "Baseline.Met.Minutes"] 

CVG.data.long[, "Percent.Improvement.from.Baseline"] = CVG.data.long[, "Diff.from.Baseline"]/CVG.data.long[, "Baseline.Met.Minutes"] 

CVG.data.long.2 = CVG.data.long[CVG.data.long[, "Follow.Up"] != "Baseline", ]

CVG.data.long.2[, "Number of Sessions"] = c(9,18,27,36)

table(CVG.data.long.2[,"Participation Level"])/4

length(unique(CVG.data.long.2[,"ID"]))

mean(CVG.data.long.2[CVG.data.long.2[,"Participation Level"] == "FULL","Total"])

mean(CVG.data.long.2[CVG.data.long.2[,"Participation Level"] == "PARTIAL","Total"])

# Fit a mixed effects linear model ####

word = lmerTest::lmer(data = CVG.data.long.2, 
                      Diff.from.Baseline ~ `Number of Sessions` + Freq + (1|ID))

lmer.sum = summary(word)

for.table = lmer.sum$coefficients

for.table = as.data.frame(for.table)

for.table[, "Conf.int"] = paste("(",
                                round(for.table[, "Estimate"] - for.table[, "Std. Error"]*qt(0.975, df = for.table[, "df"]),2),
                                ", ",
                                round(for.table[, "Estimate"] + for.table[, "Std. Error"]*qt(0.975, df = for.table[, "df"]),2),
                                ")",
                                sep = "")

for.table = for.table[, -c(2:4)]

for.table = for.table[, c(1,3,2)]

for.table[,1] = round(for.table[,1],2)
for.table[,3] = round(for.table[,3],3)

write.csv(for.table,
          "docs/MELM Results CVG.csv",
          row.names = TRUE)

# Perform a one sided one sample test ####

CI1 = t.test(CVG.data.long.2[CVG.data.long.2[, "Follow.Up"] == "9 Sessions", "Diff.from.Baseline"])

CI2 = t.test(CVG.data.long.2[CVG.data.long.2[, "Follow.Up"] == "18 Sessions", "Diff.from.Baseline"])

CI3 = t.test(CVG.data.long.2[CVG.data.long.2[, "Follow.Up"] == "27 Sessions", "Diff.from.Baseline"])

CI4 = t.test(CVG.data.long.2[CVG.data.long.2[, "Follow.Up"] == "36 Sessions", "Diff.from.Baseline"])

CI5 = t.test(CVG.data.long.2[CVG.data.long.2[, "Follow.Up"] == "9 Sessions", "Percent.Improvement.from.Baseline"])

CI6 = t.test(CVG.data.long.2[CVG.data.long.2[, "Follow.Up"] == "18 Sessions", "Percent.Improvement.from.Baseline"])

CI7 = t.test(CVG.data.long.2[CVG.data.long.2[, "Follow.Up"] == "27 Sessions", "Percent.Improvement.from.Baseline"])

CI8 = t.test(CVG.data.long.2[CVG.data.long.2[, "Follow.Up"] == "36 Sessions", "Percent.Improvement.from.Baseline"])

# Create bar graphs ####
# Create dataset ####

tmp = colMeans(CVG.data.wide[,-9], na.rm = TRUE)[-c(1,7,8)]


tmp = rbind.data.frame(tmp,
                       100*(tmp-tmp[1])/tmp[1])

colnames(tmp) = c("Baseline",
                  "9 Sessions",
                  "18 Sessions",
                  "27 Sessions",
                  "36 Sessions")

tmp = as.data.frame(t(tmp))

colnames(tmp) = c("Average.METS-Min",
                  "Average.Percent.Improvement.from.Baseline")

tmp[, "Number.of.Sessions"] = factor(rownames(tmp),
                                     levels = rownames(tmp))

tmp[, "Group"] = "Filler"

rownames(tmp) = 1:dim(tmp)[1]

tmp2 = as.data.frame(matrix(NA, nrow = 10, ncol = 3))

colnames(tmp2) = c("METS-Min", 
                   "Number.of.Sessions", 
                   "Type")

tmp2[,1] = c(tmp[1:5,1],tmp[1:5,2])

tmp2[,2] = rep(tmp[,3],2)

tmp2[,3] = rep(c("METs-min","Average Percent Improvement from Baseline"),
               each = 5)

tmp2[,3] = factor(tmp2[,3],
                  levels = c("METs-min","Average Percent Improvement from Baseline"))

write.csv(tmp2,
          "media/CVG/Data Tables/CVGMetsMin.csv",
          row.names = FALSE)

# Side by side bar graph (no color) ####

# make font sizes for all succeeding plots bigger
theme_set(theme_grey(base_size = 15))

ggplot(data = tmp2, 
       aes(x = Number.of.Sessions,
           y = `METS-Min`)) +
  facet_wrap(~Type, nrow = 1) +
  scale_x_discrete("Number of Sessions") +
  scale_y_continuous("METs-min") +
  ggtitle("METs-min") +
  geom_bar(stat = "identity", 
           position = "identity",
           fill = "black") +
  geom_text(aes(label = c(round(tmp2[1:5, "METS-Min"],0)
                          ,paste(round(tmp2[6:10, "METS-Min"],0),
                                 "%",
                                 sep = "")),
                vjust = -0.3,
                size = 5)) +
  geom_text(aes(label = rep(c("", rep("P < 0.001",4)),2),
                x = c(1:5,1:5),
                y = rep(15,10)),
            size = 4,
            color = "white") +
  geom_text(aes(label = rep(c("", rep("95% CI",4)),2),
                x = c(1:5,1:5),
                y = rep(10,10)),
            size = 4,
            color = "white") +
  geom_text(aes(label = c("", 
                          paste("(", round(CI1$conf.int[1],2), ",", round(CI1$conf.int[2],2), ")",sep = ""),
                          paste("(", round(CI2$conf.int[1],2), ",", round(CI2$conf.int[2],2), ")",sep = ""),
                          paste("(", round(CI3$conf.int[1],2), ",", round(CI3$conf.int[2],2), ")",sep = ""),
                          paste("(", round(CI4$conf.int[1],2), ",", round(CI4$conf.int[2],2), ")",sep = ""),
                          "",
                          paste("(", round(CI5$conf.int[1],2), ",", round(CI5$conf.int[2],2), ")",sep = ""),
                          paste("(", round(CI6$conf.int[1],2), ",", round(CI6$conf.int[2],2), ")",sep = ""),
                          paste("(", round(CI7$conf.int[1],2), ",", round(CI7$conf.int[2],2), ")",sep = ""),
                          paste("(", round(CI8$conf.int[1],2), ",", round(CI8$conf.int[2],2), ")",sep = "")),
                x = c(1:5,1:5),
                y = rep(5,10)),
            size = 4,
            color = "white") +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")

ggsave("media/CVG/Side by Side Bar Graph (no color, P & CI) (6-11-18) (Matched Group Portion).tiff",
       device = "tiff",
       width = 13,
       height = 6, 
       dpi = 300,
       compression = "lzw")



