#################################Program Description################################
#Name: CVG                                                                         #
#Author: Traymon Beavers                                                           #
#Depends: upload data.R, ggplot2, lmerTest                                         #
#Date Created: 7/21/2017                                                           #
#Date Updated: 9/20/2017                                                           #
#Purpose: To perform analysis on the stroke rehabilitation program cardiovascular  #
#         group data, as well as provide a collection of plots                     #
####################################################################################

# Load the necessary source code and packages ####
source("source/Traymon's Source Code/Data Reconfiguration/upload data.R")
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
                                                               "CVG.Total")]

CVG.data.long = as.data.frame(matrix(NA,
                                     5*dim(CVG.data.wide)[1],
                                     6))

colnames(CVG.data.long) = c("ID",
                            "Follow.Up",
                            "Met.Minutes",
                            "Freq",
                            "Total",
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
  
  
}

CVG.data.long[, "Diff.from.Baseline"] = CVG.data.long[, "Met.Minutes"] - CVG.data.long[, "Baseline.Met.Minutes"] 

CVG.data.long.2 = CVG.data.long[CVG.data.long[, "Follow.Up"] != "Baseline", ]

CVG.data.long.2[, "Number of Sessions"] = c(9,18,27,36)

# Fit a mixed effects linear model ####

word = lmerTest::lmer(data = CVG.data.long.2, 
                      Diff.from.Baseline ~ `Number of Sessions` + Freq + (1|ID))

summary(word)

# Perform a one sided one sample test ####

t.test(CVG.data.long.2[CVG.data.long.2[, "Follow.Up"] == "9 Weeks", "Diff.from.Baseline"],
       alternative = "greater")

t.test(CVG.data.long.2[CVG.data.long.2[, "Follow.Up"] == "18 Weeks", "Diff.from.Baseline"],
       alternative = "greater")

t.test(CVG.data.long.2[CVG.data.long.2[, "Follow.Up"] == "27 Weeks", "Diff.from.Baseline"],
       alternative = "greater")

t.test(CVG.data.long.2[CVG.data.long.2[, "Follow.Up"] == "36 Weeks", "Diff.from.Baseline"],
       alternative = "greater")



# Create bar graphs ####
# Create dataset ####

tmp = colMeans(CVG.data.wide, na.rm = TRUE)[-c(1,7,8)]

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

tmp[, "Number.of.Sessions"] = rownames(tmp)

rownames(tmp) = 1:dim(tmp)[1]

write.csv(tmp,
          "media/CVG/Data Tables/CVGMetsMin.csv",
          row.names = FALSE)

# Average METS-Min bar graph ####

# make font sizes for all succeeding plots bigger
theme_set(theme_grey(base_size = 15)) 

tmp[,3] = factor(tmp[,3],
                 levels = c("Baseline",
                            "9 Sessions",
                            "18 Sessions",
                            "27 Sessions",
                            "36 Sessions"))

ggplot(data = tmp, 
       aes(x = Number.of.Sessions,
           y = `Average.METS-Min`)) +
  scale_x_discrete("Number of Sessions") +
  scale_y_continuous("METs-Min") +
  ggtitle("Average METs-Min by Number of Sessions") +
  geom_bar(stat = "identity", 
           position = "identity",
           fill = "lightblue") +
  geom_text(aes(label = round(tmp[, "Average.METS-Min"],0),
                vjust = -0.3,
                size = 5)) +
  geom_text(aes(label = c("", rep("P-value < 0.0001",4)),
                x = 1:5,
                y = rep(5,5),
                size = 5)) +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")

ggsave("media/CVG/METs-Min Bar Graph.tiff",
       device = "tiff",
       width = 10,
       height = 5, 
       dpi = 300,
       compression = "lzw")

# Percent improvement bar graph ####

ggplot(data = tmp, 
       aes(x = Number.of.Sessions,
           y = Average.Percent.Improvement.from.Baseline)) +
  scale_x_discrete("Number of Sessions") +
  scale_y_continuous("METs-Min") +
  ggtitle("Average METs-Min by Number of Sessions",
          subtitle = "Percent Improvement from Baseline") +
  geom_bar(stat = "identity", 
           position = "identity",
           fill = "lightblue") +
  geom_text(aes(label = paste(round(tmp[, "Average.Percent.Improvement.from.Baseline"],0),
                              "%",
                              sep = ""),
                vjust = -0.3,
                size = 5)) +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")

ggsave("media/CVG/Percent Bar Graph.tiff",
       device = "tiff",
       width = 10,
       height = 5, 
       dpi = 300,
       compression = "lzw")


# Average METS-Min bar graph (no color) ####

# make font sizes for all succeeding plots bigger
theme_set(theme_grey(base_size = 15)) 

tmp[,3] = factor(tmp[,3],
                 levels = c("Baseline",
                            "9 Sessions",
                            "18 Sessions",
                            "27 Sessions",
                            "36 Sessions"))

ggplot(data = tmp, 
       aes(x = Number.of.Sessions,
           y = `Average.METS-Min`)) +
  scale_x_discrete("Number of Sessions") +
  scale_y_continuous("METs-Min") +
  ggtitle("Average METs-Min by Number of Sessions") +
  geom_bar(stat = "identity", 
           position = "identity",
           fill = "black") +
  geom_text(aes(label = round(tmp[, "Average.METS-Min"],0),
                vjust = -0.3,
                size = 5)) +
  geom_text(aes(label = c("", rep("P-value < 0.0001",4)),
                x = 1:5,
                y = rep(5,5),
                size = 5),
                color = "white") +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")

ggsave("media/CVG/METs-Min Bar Graph (no color).tiff",
       device = "tiff",
       width = 10,
       height = 5, 
       dpi = 300,
       compression = "lzw")

# Percent improvement bar graph (no color) ####

ggplot(data = tmp, 
       aes(x = Number.of.Sessions,
           y = Average.Percent.Improvement.from.Baseline)) +
  scale_x_discrete("Number of Sessions") +
  scale_y_continuous("METs-Min") +
  ggtitle("Average METs-Min by Number of Sessions",
          subtitle = "Percent Improvement from Baseline") +
  geom_bar(stat = "identity", 
           position = "identity",
           fill = "black") +
  geom_text(aes(label = paste(round(tmp[, "Average.Percent.Improvement.from.Baseline"],0),
                              "%",
                              sep = ""),
                vjust = -0.3,
                size = 5)) +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")

ggsave("media/CVG/Percent Bar Graph (no color).tiff",
       device = "tiff",
       width = 10,
       height = 5, 
       dpi = 300,
       compression = "lzw")

