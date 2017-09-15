#################################Program Description################################
#Name: CVG                                                                         #
#Author: Traymon Beavers                                                           #
#Depends: ggplot, lme4, lmerTest, MASS                                             #
#Date Created: 7/21/2017                                                           #
#Date Updated: 9/14/2017                                                           #
#Purpose: To perform analysis on the stroke rehabilitation program cardiovascular  #
#         group data, as well as provide a collection of plots                     #
####################################################################################

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
                                 "9 Weeks",
                                 "18 Weeks",
                                 "27 Weeks",
                                 "36 Weeks")


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

CVG.data.long.2[, "Time"] = c(9,18,27,36)

# Fit a mixed effects linear model ####

word = lmerTest::lmer(data = CVG.data.long.2, 
                      Diff.from.Baseline ~ Time + Freq + Total + (1|ID))

summary(word)

anova(word)

# Perform a one sided one sample test ####

t.test(CVG.data.long.2[CVG.data.long.2[, "Follow.Up"] == "9 Weeks", "Diff.from.Baseline"],
       alternative = "greater")

t.test(CVG.data.long.2[CVG.data.long.2[, "Follow.Up"] == "18 Weeks", "Diff.from.Baseline"],
       alternative = "greater")

t.test(CVG.data.long.2[CVG.data.long.2[, "Follow.Up"] == "27 Weeks", "Diff.from.Baseline"],
       alternative = "greater")

t.test(CVG.data.long.2[CVG.data.long.2[, "Follow.Up"] == "36 Weeks", "Diff.from.Baseline"],
       alternative = "greater")

1-(sum(is.na(CVG.data.long[CVG.data.long[,"Follow.Up"]=="36 Weeks","Met.Minutes"]))/length(CVG.data.long[CVG.data.long[,"Follow.Up"]=="36 Weeks","Met.Minutes"]))

length(CVG.data.long[CVG.data.long[,"Follow.Up"]=="36 Weeks","Met.Minutes"])-sum(is.na(CVG.data.long[CVG.data.long[,"Follow.Up"]=="36 Weeks","Met.Minutes"]))

length(CVG.data.long[CVG.data.long[,"Follow.Up"]=="36 Weeks","Met.Minutes"])




# 6c. CVG plot ####
# Create dataset ####
for.6c = NewMaster[is.na(NewMaster[,"CVG.Baseline"]) == 0 &
                     NewMaster[,"CVG.Baseline"] != 0,c(
                       "CVG.Baseline", 
                       "CVG.9.Met.Minutes", 
                       "CVG.18.Met.Minutes", 
                       "CVG.27.Met.Minutes",
                       "CVG.36.Met.Minutes")]

for.6c[, "B.AI"] = for.6c[, "CVG.Baseline"] - for.6c[, "CVG.Baseline"]
for.6c[, "9.AI"] = for.6c[, "CVG.9.Met.Minutes"] - for.6c[, "CVG.Baseline"]
for.6c[, "18.AI"] = for.6c[, "CVG.18.Met.Minutes"] - for.6c[, "CVG.Baseline"]
for.6c[, "27.AI"] = for.6c[, "CVG.27.Met.Minutes"] - for.6c[, "CVG.Baseline"]
for.6c[, "36.AI"] = for.6c[, "CVG.36.Met.Minutes"] - for.6c[, "CVG.Baseline"]

for.6c[, "B.PI"] = 100*for.6c[, "B.AI"]/for.6c[, "CVG.Baseline"]
for.6c[, "9.PI"] = 100*for.6c[, "9.AI"]/for.6c[, "CVG.Baseline"]
for.6c[, "18.PI"] = 100*for.6c[, "18.AI"]/for.6c[, "CVG.Baseline"]
for.6c[, "27.PI"] = 100*for.6c[, "27.AI"]/for.6c[, "CVG.Baseline"]
for.6c[, "36.PI"] = 100*for.6c[, "36.AI"]/for.6c[, "CVG.Baseline"]

for.6c.means = colMeans(for.6c, na.rm = TRUE)

for.6c.plot = cbind.data.frame(c("Baseline", "9 weeks", "18 weeks", "27 weeks", "36 weeks"), 
                               melt(for.6c.means)[1:5,], 
                               round(melt(for.6c.means)[6:10,], 
                                     digits = 1),
                               round(melt(for.6c.means)[11:15,], 
                                     digits = 1))

colnames(for.6c.plot) = c("Time", 
                          "METS.Min", 
                          "Absolute.Improvement", 
                          "Percent.Improvement")

for (i in 1:5){
  
  for.6c.plot[i,"Percent.Improvement"] = paste(for.6c.plot[i,"Percent.Improvement"], "%", sep = "")
  
}

for.6c.plot[, "METS.Min"] = round(for.6c.plot[, "METS.Min"], 0)

for.6c.plot[,"Group"] = "Dummy"

# write.csv(for.6c.plot, 
#           "tmp/Plot Data Tables/CVGMETSMin.csv",
#           row.names = FALSE)


# 6c1. Absolute improvement bar graph ####

ggplot(data = for.6c.plot, 
       aes(x = Time,
           y = METS.Min,
           fill = Time)) +
  scale_x_discrete("Follow Up Time",
                   limits = c("Baseline", 
                              "9 weeks",
                              "18 weeks",
                              "27 weeks",
                              "36 weeks")) +
  scale_y_continuous("METS/Min") +
  ggtitle("Average METS/Min by Follow Up Time",
          subtitle = "Number in bars is average absolute improvement from baseline") +
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = Absolute.Improvement,
                vjust = 2,
                size = 5)) +
  geom_text(aes(label = c("", rep("P-Value < 0.001", 4)),
                vjust = -0.5,
                size = 5)) +
  scale_fill_manual(name = "",
                    values = c("purple", 
                               "blue", 
                               "dark green", 
                               "red", 
                               "black")) +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")

ggsave("C:/git_local/jfk.cvi/Plots/Paper Plots/METS Min Bar Graph.png",
       width = 16,
       height = 9)



# 6c2. Percent improvement bar graph ####

ggplot(data = for.6c.plot[1:5,], 
       aes(x = Time,
           y = METS.Min,
           fill = Time)) +
  scale_x_discrete("Follow Up Time",
                   limits = c("Baseline", 
                              "9 weeks",
                              "18 weeks",
                              "27 weeks",
                              "36 weeks")) +
  scale_y_continuous("METS/Min") +
  ggtitle("Average METS/Min by Follow Up Time",
          subtitle = "Number in bars is average percent improvement from baseline") +
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = Percent.Improvement,
                vjust = 2)) +
  scale_fill_manual(name = "",
                    values = c("purple", "blue", "green", "red", "black")) +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")





# 6c3. METS/Min line graph ####

ggplot(data = for.6c.plot, 
       aes(x = Time,
           y = METS.Min,
           color = Time,
           group = Group)) +
  scale_x_discrete("Follow Up Time",
                   limits = c("Baseline", 
                              "9 weeks",
                              "18 weeks",
                              "27 weeks",
                              "36 weeks")) +
  scale_y_continuous("METS/Min") +
  ggtitle("Average METS/Min by Follow Up Time") +
  geom_point(size = 2) +
  geom_line(color = "black") +
  geom_text(aes(label = METS.Min,
                vjust = -0.5,
                size = 5)) +
  scale_color_manual(name = "",
                     values = c("purple", 
                                "blue", 
                                "dark green", 
                                "red", 
                                "black")) +
  annotate("text",
           x = 4,
           y = 65,
           label = "P-value < 0.001",
           size = 5) +
  annotate("text",
           x = 4,
           y = 62.5,
           label = "Estimate = 1.4",
           size = 5) +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")

ggsave("C:/git_local/jfk.cvi/Plots/Paper Plots/METS Min Line Graph.png",
       width = 16,
       height = 9)











