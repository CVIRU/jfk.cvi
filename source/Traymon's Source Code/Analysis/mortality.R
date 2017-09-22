#################################Program Description################################
#Name: mortality                                                                   #
#Author: Traymon Beavers                                                           #
#Depends: create matches.R, ggplot2, lmerTest, gridExtra, survival, data.table     #
#Date Created: 7/21/2017                                                           #
#Date Updated: 9/20/2017                                                           #
#Purpose: To perform analysis on the stroke rehabilitation program mortality data  #
#         by matching patients in the study group with patients in the control     #
#         group and then conducting various statistical procedures with respect to #
#         the matched pairs, as well as provide a collection of plots              #                                                                    #
####################################################################################

# Load the necessary source code and functions ####
source("source/Traymon's Source Code/Analysis/create matches.R")
library(ggplot2)
library(gridExtra)
library(survival)
library(data.table)

# Perform bootstrap test ####

# create dataset for bootstrap test
mortality.data = droplevels(NewMaster.One[NewMaster.One[, "Group"] == "Control Group" &
                                            NewMaster.One[, "ID"] %in% match.subgroup.One[, "ID"] &
                                            is.na(NewMaster.One[, "Survival.Time"]) == 0, ])

# create survival information for entire matched dataset
sf = survfit(Surv(mortality.data[, "Survival.Time"], 
                  mortality.data[, "Deceased_Y.N"]) ~ Group,
             data = mortality.data)  

# calculate area between observed survival curves
obs.ABC = (long.last + 1) - sum(diff(c(0, sf$time))*c(1, sf$surv[-length(sf$time)]))

# initialize vector for the bootstrapped areas for test
ABC = 0

# initialize vector for the bootstrapped areas for interval
ABC.for.int = 0

# create 5000 bootstrap samples
for (i in 1:5000){
  
  set.seed(i)
  
  # bootstrap for mock treatment group
  mock.study = mortality.data[sample(1:dim(mortality.data)[1], 
                                     length(matchrow.final[matchrow.final[, "Group"] == 1, "ID"]), 
                                     TRUE), c("Deceased_Y.N", "Survival.Time")]
  
  # label mock treatment group as such
  mock.study[, "Group"] = "Mock"
  
  # bootstrap for control group
  control = mortality.data[sample(1:dim(mortality.data)[1], 
                                  dim(mortality.data)[1], 
                                  TRUE), c("Deceased_Y.N", "Survival.Time")]
  
  # label control group as such
  control[, "Group"] = "Control"
  
  # combine the two groups into one bootstrapped dataset
  bootstrap.data = rbind.data.frame(mock.study, control)
  
  # create a survival fit object
  sf1 = survfit(Surv(Survival.Time, Deceased_Y.N) ~ Group,
                data = bootstrap.data)  
  
  # extract data from the object 
  survival.data = data.table(Time = c(0, 
                                      sf1$time[1:sf1$strata[1]],
                                      0,
                                      sf1$time[(sf1$strata[1] + 1):(sf1$strata[1]+sf1$strata[2])]),
                             Surv.Prob = c(1,
                                           sf1$surv[1:sf1$strata[1]],
                                           1,
                                           sf1$surv[(sf1$strata[1] + 1):(sf1$strata[1]+sf1$strata[2])]),
                             Group = c(rep("Control Group", sf1$strata[1] + 1),
                                       rep("Mock Study Group", sf1$strata[2] + 1)))
  
  # transform dataset into a data frame
  survival.data = as.data.frame(survival.data)
  
  # calculate the area under the survival curve for the mock study group
  AUC.mock = sum(diff(survival.data[survival.data[,"Group"] == "Mock Study Group", "Time"])*
                   survival.data[survival.data[,"Group"] == "Mock Study Group",
                                 "Surv.Prob"][-length(survival.data[survival.data[,"Group"] == "Mock Study Group", "Surv.Prob"])])
  
  # calculate the area under the survival curve for the bootstrapped control group
  AUC.control = sum(diff(survival.data[survival.data[,"Group"] == "Control Group", "Time"])*
                      survival.data[survival.data[,"Group"] == "Control Group",
                                    "Surv.Prob"][-length(survival.data[survival.data[,"Group"] == "Control Group", "Surv.Prob"])])
  
  # calculate the area between the bootstrapped survival curves and add it to the vector
  ABC = c(ABC, (AUC.mock - AUC.control))
  
  # calculate the area between the observed survival curve for the study group and 
  # the bootstrapped survival for the control group and add it to the vector
  ABC.for.int = c(ABC.for.int, ((long.last + 1) - AUC.control))
  
}

# delete the entry used to initialize the vector
ABC = ABC[-1]

# delete the entry used to initialize the vector
ABC.for.int = ABC.for.int[-1]

# create a histogram of the difference between survival curves when both groups are bootstrapped
hist(ABC, 
     main = paste("Bootstrap Distribution of Differences Between Survival Curves", 
                  "(Both Bootstrapped)", 
                  sep = "\n"),
     xlab = "Difference Between Survival Curves")

hist(ABC.for.int, 
     main = paste("Bootstrap Distribution of Differences Between Survival Curves", 
                  "(Both Bootstrapped)", 
                  sep = "\n"),
     xlab = "Difference Between Survival Curves")

# get p-value for one-sided test
mean(ABC>obs.ABC)

# get confidence interval
quantile(ABC.for.int, c(0.025, .975))

# Shaded bar graphs ####
# Create dataset for before matching ####

tmp.Total = matrix(c(length(StudyGroupIDs) - length(intersect(StudyGroupIDs,ControlGroupIDs)), 
                     length(ControlGroupIDs) - length(intersect(StudyGroupIDs,ControlGroupIDs))), 2)

tmp.Num = table(NewMaster.One[, c("Group", "Deceased_Y.N")])[,2]

tmp.Percent = tmp.Num/tmp.Total

tmp = as.data.frame(cbind.data.frame(c("Study Group", "Control Group"),
                                     tmp.Num, 
                                     tmp.Percent,
                                     tmp.Total))

colnames(tmp) = c("Group", 
                  "Number Deceased", 
                  "Percent Deceased",
                  "Total Patients in Entire Dataset")

rownames(tmp) = 1:dim(tmp)[1]

tmp.melt1 = melt(tmp, 
                   id = "Group")

write.csv(tmp,
          "media/Mortality/Data Tables/Mortality_No_Matching.csv",
          row.names = FALSE)

# Shaded bar graph (No Matching) ####

ggplot(data = tmp.melt3[c(5,6,1,2),], 
       aes(x = Group,
           y = value,
           fill = variable)) +
  scale_y_continuous("Number of Patients") +
  ggtitle("Patient Mortality by Rehabilitation Group After Matching") +
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = value,
                vjust = -0.4)) +
  annotate("text", 
           x = 1, 
           y = tmp[2,2]/2,
           color = "red",
           size = 5,
           label = paste(100*round(tmp[2,3],2), "%", 
                         sep = "")) +
  scale_fill_manual(name = "",
                    values = c("black", "gray")) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top")

ggsave("media/Mortality/Mortality Bar Before Matching.png",
       width = 16,
       height = 9)

# Create dataset for after matching ####

tmp.Total = matrix(c(dim(match.subgroup.One[match.subgroup.One[,"Group"] == "Study Group",])[1], 
                     dim(match.subgroup.One[match.subgroup.One[,"Group"] == "Control Group",])[1]), 2)

tmp.Num = table(match.subgroup.One[, c("Group", "Deceased_Y.N")])[, 2]

tmp.Percent = tmp.Num/tmp.Total

tmp = as.data.frame(cbind.data.frame(c("Study Group", "Control Group"),
                                     tmp.Num, 
                                     tmp.Percent,
                                     tmp.Total))

colnames(tmp) = c("Group", 
                  "Number Deceased", 
                  "Percent Deceased",
                  "Total Patients in Entire Dataset")

rownames(tmp) = 1:dim(tmp)[1]

tmp.melt2 = melt(tmp, 
                id = "Group")

write.csv(tmp,
          "media/Mortality/Data Tables/Mortality_After_Matching.csv",
          row.names = FALSE)

# Shaded bar graph (After Matching) ####

ggplot(data = tmp.melt2[c(5,6,1,2),], 
       aes(x = Group,
           y = value,
           fill = variable)) +
  scale_y_continuous("Number of Patients") +
  ggtitle("Patient Mortality by Rehabilitation Group After Matching") +
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = value,
                vjust = -0.4)) +
  annotate("text", 
           x = 1, 
           y = tmp[2,2]/2,
           color = "red",
           size = 5,
           label = paste(100*round(tmp[2,3],2), "%", 
                         sep = "")) +
  scale_fill_manual(name = "",
                    values = c("black", "gray")) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top")

ggsave("media/Mortality/Mortality Bar After Matching.png",
       width = 16,
       height = 9)

# Combine above datasets ####

tmp.melt3 = rbind.data.frame(tmp.melt1,
                             tmp.melt2)

tmp.melt3[, "Matched"] = rep(c("Before Matching", "After Matching"), each = 6)

tmp.melt3[, "Matched"] = factor(tmp.melt3[, "Matched"],
                                levels = c("Before Matching", 
                                           "After Matching"))

# Shaded bar graph (Before and After Matching) ####

ggplot(data = tmp.melt3[c(5,6,1,2,11,12,7,8),], 
       aes(x = Group,
           y = value,
           fill = variable)) +
  facet_wrap(~Matched, nrow = 1) +
  scale_y_continuous("Number of Patients") +
  ggtitle("Patient Mortality by Rehabilitation Group After Matching") +
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = value,
                vjust = -0.4)) +
  scale_fill_manual(name = "",
                    values = c("black", "gray")) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top")

ggsave("media/Mortality/Mortality Bar (Before and After Matching).png",
       width = 16,
       height = 9)

# Create dataset for before matching ####

tmp = droplevels(NewMaster.One[NewMaster.One[, "Group"] == "Control Group" &
                                 is.na(NewMaster.One[, "Survival.Time"]) == 0, ])

# create survival information for entire matched dataset
tmp.sf = survfit(Surv(tmp[, "Survival.Time"], 
                      tmp[, "Deceased_Y.N"]) ~ Group,
                 data = tmp)

# extract data from the survival information 
tmp.data1 = data.table(Time = c(0, 
                                347,
                                0,
                                tmp.sf$time),
                       Surv.Prob = c(1,
                                     1,
                                     1,
                                     tmp.sf$surv),
                       Group = c(rep("Study Group", 2),
                                 rep("Control Group", length(tmp.sf$surv)+1)))

write.csv(tmp.data1,
          "media/Mortality/Data Tables/Survival_Curve_No_Matching.csv",
          row.names = FALSE)


# Create dataset for after matching ####

tmp = droplevels(NewMaster.One[NewMaster.One[, "Group"] == "Control Group" &
                                 NewMaster.One[, "ID"] %in% match.subgroup.One[, "ID"] &
                                 is.na(NewMaster.One[, "Survival.Time"]) == 0, ])

# create survival information for entire matched dataset
tmp.sf = survfit(Surv(tmp[, "Survival.Time"], 
                      tmp[, "Deceased_Y.N"]) ~ Group,
                 data = tmp)

# extract data from the survival information 
tmp.data2 = data.table(Time = c(0, 
                                347,
                                0,
                                tmp.sf$time),
                       Surv.Prob = c(1,
                                     1,
                                     1,
                                     tmp.sf$surv),
                       Group = c(rep("SRP Participant Group", 2),
                                 rep("Non-Participant Group", length(tmp.sf$surv)+1)))

write.csv(tmp.data2,
          "media/Mortality/Data Tables/Survival_Curve_After_Matching.csv",
          row.names = FALSE)

# Create survival curve plot (after matching) ####

ggplot(tmp.data2,
       aes(x = Time,
           y = Surv.Prob,
           colour = Group,
           group = Group)) +
  geom_step() +
  scale_colour_manual(values = c("red",
                                 "green")) +
  scale_x_continuous("Days After Stroke") +
  scale_y_continuous("Survival",
                     limits = c(0, 1)) +
  ggtitle("Survival Curves for All-Cause Mortality by Rehabilitation Group") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top")

ggsave("media/Mortality/Survival Curve (After Matching).tiff", 
       device = "tiff",
       width = 8,
       height = 5, 
       dpi = 300,
       compression = "lzw")

# Create survival curve plot (after matching) (no color) ####

ggplot(tmp.data2,
       aes(x = Time,
           y = Surv.Prob,
           group = Group)) +
  geom_step(aes(linetype = Group)) +
  scale_x_continuous("Days After Stroke") +
  scale_y_continuous("Survival",
                     limits = c(0, 1)) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  ggtitle("Survival Curves for All-Cause Mortality by Rehabilitation Group") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top")

ggsave("media/Mortality/Survival Curve (After Matching) (no color).tiff", 
       device = "tiff",
       width = 8,
       height = 5, 
       dpi = 300,
       compression = "lzw")

# Combine above datasets ####

tmp.data3 = as.data.frame(rbind.data.frame(tmp.data1,
                             tmp.data2))

tmp.data3[, "Matched"] = c(rep("Before Matching", nrow(tmp.data1)),
                           rep("After Matching", nrow(tmp.data2)))

tmp.data3[, "Matched"] = factor(tmp.data3[, "Matched"],
                                levels = unique(tmp.data3[, "Matched"]))

write.csv(tmp.data3,
          "media/Mortality/Data Tables/Survival_Curve_Both.csv",
          row.names = FALSE)

# Create survival curve plot (before and after matching) ####

ggplot(tmp.data3,
             aes(x = Time,
                 y = Surv.Prob,
                 colour = Group,
                 group = Group)) +
  facet_wrap(~Matched, nrow = 1) +
  geom_step() +
  scale_colour_manual(values = c("red",
                                 "green")) +
  scale_x_continuous("Days After Stroke") +
  scale_y_continuous("Survival",
                     limits = c(0, 1)) +
  ggtitle("Survival Curve for All Cause Mortality by Rehabilitation Group") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top")

ggsave("media/Mortality/Survival Curve (Before and After Matching).png",
       width = 16,
       height = 9)

# Bootstrap survival curve ####
# create dataset ####

# set seed for bootstrap iteration
set.seed(50)

# bootstrap for mock treatment group
mock.study = mortality.data[sample(1:dim(mortality.data)[1], 
                                   length(matchrow.final[matchrow.final[, "Group"] == 1, "ID"]), 
                                   TRUE), c("Deceased_Y.N", "Survival.Time")]

# label mock treatment group as such
mock.study[, "Group"] = "Mock"

# bootstrap for control group
control = mortality.data[sample(1:dim(mortality.data)[1], 
                                dim(mortality.data)[1], 
                                TRUE), c("Deceased_Y.N", "Survival.Time")]

# label control group as such
control[, "Group"] = "Control"

# combine the two groups into one bootstrapped dataset
bootstrap.data = rbind.data.frame(mock.study, control)

# create a survival fit object
sf1 = survfit(Surv(Survival.Time, Deceased_Y.N) ~ Group,
              data = bootstrap.data)  

# extract data from the object 
survival.data = data.table(Time = c(0, 
                                    sf1$time[1:sf1$strata[1]],
                                    0,
                                    sf1$time[(sf1$strata[1] + 1):(sf1$strata[1]+sf1$strata[2])]),
                           Surv.Prob = c(1,
                                         sf1$surv[1:sf1$strata[1]],
                                         1,
                                         sf1$surv[(sf1$strata[1] + 1):(sf1$strata[1]+sf1$strata[2])]),
                           Group = c(rep("Control Group", sf1$strata[1] + 1),
                                     rep("Mock Study Group", sf1$strata[2] + 1)))

# transform dataset into a data frame
survival.data = as.data.frame(survival.data)

AOC.mock = sum(diff(survival.data[survival.data[,"Group"] == "Mock Study Group", "Time"])*
                 survival.data[survival.data[,"Group"] == "Mock Study Group",
                               "Surv.Prob"][-length(survival.data[survival.data[,"Group"] == "Mock Study Group", "Surv.Prob"])])


AOC.control = sum(diff(survival.data[survival.data[,"Group"] == "Control Group", "Time"])*
                    survival.data[survival.data[,"Group"] == "Control Group",
                                  "Surv.Prob"][-length(survival.data[survival.data[,"Group"] == "Control Group", "Surv.Prob"])])


AOC = c(AOC, (AOC.mock - AOC.control))


# create survival curve plot ####

# create line graph
ggplot(survival.data,
       aes(x = Time,
           y = Surv.Prob,
           colour = Group,
           group = Group)) +
  geom_step() +
  scale_colour_manual(values = c("red",
                                 "green")) +
  scale_x_continuous("Days After Stroke") +
  scale_y_continuous("Survival",
                     limits = c(0, 1)) +
  ggtitle("Bootstrapped Survival Curve for All Cause Mortality by Rehabilitation Group") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top")

ggsave("media/Mortality/Bootstrapped Survival Curve.png",
       width = 16,
       height = 9)


# save the plot