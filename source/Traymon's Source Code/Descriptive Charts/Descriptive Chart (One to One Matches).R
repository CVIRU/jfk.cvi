#################################Program Description################################
#Name: Descriptive Chart (One to One Matches)                                      #
#Author: Traymon Beavers                                                           #
#Depends: create matches.R                                                         #
#Date Created: 7/21/2017                                                           #
#Date Updated: 3/7/2018                                                            #
#Purpose: To create and save a descriptive chart comparing the study group and     #
#         control group patients; sample of one to one matches                     #
####################################################################################

# Load the necessary source code ####
source("source/Traymon's Source Code/Analysis/create matches.R")

# Initialize the descriptive chart ####

for.names = colnames(match.subgroup.One)[c(2:3,
                                               8:9,
                                               11:12,
                                               16:23,
                                               29:30,
                                               33,
                                               36:43,
                                               46:48,
                                               50:55,
                                               57:61,
                                               63:64,
                                               66:67,
                                               75:79,
                                               113,
                                               115:120)]

# initialize the descriptive chart
matched.demo.chart = as.data.frame(matrix(NA,
                                         (length(for.names) + 1),
                                         5))

# name the rows of the descriptive chart
rownames(matched.demo.chart) = c("N", 
                                for.names)

# name the columns of the descriptive chart
colnames(matched.demo.chart) = c("Study(Mean/Number)", 
                                "Study(SD/Percent)", 
                                "Control(Mean/Number)", 
                                "Control(SD/Percent)",
                                # "Total (Mean/Number)",
                                # "Total (SD/Percent)",
                                "P-Value")

# make list of continuous variables
cont.list = rownames(matched.demo.chart)[c(2,
                                          7,
                                          16:17,
                                          19:26,
                                          45:49,
                                          51:56)]

# make list of continuous variables
bin.list = rownames(matched.demo.chart)[c(3:5,
                                         9:15,
                                         18,
                                         27:44)]

# make list of continuous variables
cat.list = rownames(matched.demo.chart)[c(6,
                                         8,
                                         50)]

# Fill in the chart for continuous variables ####

# fill in the number of patients in each group
matched.demo.chart["N",] = c(dim(match.subgroup.One.Study)[1], 
                            NA,
                            dim(match.subgroup.One.Control)[1],
                            NA,
                            # dim(match.subgroup.One)[1],
                            # NA,
                            NA)

# cycle through the easy continuous variables
for (i in cont.list[c(1:12,18:23)]){
  
  # conduct a two sample t test for the current variable
  tmp = t.test(match.subgroup.One.Study[, i],
               match.subgroup.One.Control[, i])
  
  # place the sample mean for the study group in the chart
  matched.demo.chart[i, 1] = round(tmp$estimate[1], 
                                  digits = 1)
  
  # place the sample standard deviation for the study group in the chart
  matched.demo.chart[i, 2] = round(sqrt(var(match.subgroup.One.Study[, i], 
                                           na.rm = TRUE)), 
                                  digits = 1)
  
  # place the sample mean for the control group in the chart  
  matched.demo.chart[i, 3] = round(tmp$estimate[2],
                                  digits = 1)
  
  # place the sample standard deviation for the study group in the chart
  matched.demo.chart[i, 4] = round(sqrt(var(match.subgroup.One.Control[, i], 
                                           na.rm = TRUE)), 
                                  digits = 1)
  
  # # place the sample mean for the matched cohort in the chart
  # matched.demo.chart[i, 5] = round(mean(match.subgroup.One[, i],
  #                                      na.rm = TRUE), 
  #                                 digits = 1)
  # 
  # # place the sample standard deviation for the matched cohort in the chart
  # matched.demo.chart[i, 6] = round(sqrt(var(match.subgroup.One[, i], 
  #                                          na.rm = TRUE)), 
  #                                 digits = 1)
  
  # place the p value from the two sample t test in the chart  
  matched.demo.chart[i, 5] = round(tmp$p.value, 
                                  digits = 3)
  
}

# cycle through the harder continuous variables
for (i in cont.list[c(13:17)]){
  
  # conduct a two sample t test for the current variable
  tmp = t.test(Interpolate.Master[Interpolate.Master[, "ID"] %in% StudyGroupIDs & Interpolate.Master[,"DaysId"] == 1, i],
               Interpolate.Master[Interpolate.Master[, "ID"] %in% ControlGroupIDs & Interpolate.Master[,"DaysId"] == 1, i])
  
  # place the sample mean for the study group in the chart
  matched.demo.chart[i, 1] = round(tmp$estimate[1],
                                  digits = 1)
  
  # place the sample standard deviation for the study group in the chart
  matched.demo.chart[i, 2] = round(sqrt(var(Interpolate.Master[Interpolate.Master[, "ID"] %in% StudyGroupIDs & Interpolate.Master[,"DaysId"] == 1, i], 
                                           na.rm = TRUE)), 
                                  digits = 1)
  
  # place the sample mean for the control group in the chart  
  matched.demo.chart[i, 3] = round(tmp$estimate[2], 
                                  digits = 1)
  
  # place the sample standard deviation for the study group in the chart
  matched.demo.chart[i, 4] = round(sqrt(var(Interpolate.Master[Interpolate.Master[, "ID"] %in% ControlGroupIDs & Interpolate.Master[,"DaysId"] == 1, i], 
                                           na.rm = TRUE)), 
                                  digits = 1)
  
  # # place the sample mean for the matched cohort in the chart
  # matched.demo.chart[i, 5] = round(mean(Interpolate.Master[Interpolate.Master[,"DaysId"] == 1, i],
  #                                      na.rm = TRUE), 
  #                                 digits = 1)
  # 
  # # place the sample standard deviation for the matched cohort in the chart
  # matched.demo.chart[i, 6] = round(sqrt(var(Interpolate.Master[Interpolate.Master[,"DaysId"] == 1, i], 
  #                                          na.rm = TRUE)), 
  #                                 digits = 1)
  
  # place the p value from the two sample t test in the chart  
  matched.demo.chart[i, 5] = round(tmp$p.value, 
                                  digits = 3)
  
}

# Fill in the chart for binary variables ####

# cycle through the binary variables
for (i in bin.list){
  
  tmp.table = table(match.subgroup.One[, c("Group", i)])[c(2,1), c(2,1)]
  
  # construct a 1 by 2 table for the current variable
  tmp.table2 = table(match.subgroup.One[, i])[c(2,1)]
  
  # if (is.null(levels(match.subgroup.One[, i])) == 0){
  #   
  #   if( c("") %in% levels(match.subgroup.One[, i]) |
  #       c("9999") %in% levels(match.subgroup.One[, i])){ 
  #     
  #     print(i)
  #     
  #     tmp.table = table(match.subgroup.One[, c("Group", i)])[c(1,2), c(3,2)]
  #     
  #   }
  #   
  # }
  # 
  # # delete the "Unknown" option from the table
  # if (i == "Hispanic.Ethnicity"){
  #   
  #   # construct a 2 by 2 table for the current variable
  #   tmp.table = table(match.subgroup.One[, c("Group", i)])[c(2,1), c(3,1)]
  #   
  #   # construct a 1 by 2 table for the current variable
  #   tmp.table2 = table(match.subgroup.One[, i])[c(3,1)]
  #   
  # }
  
  # conduct a two sample test for proportions the current variable
  tmp = prop.test(tmp.table)
  
  # place the number of patients with this binary variable in the study group in the chart
  matched.demo.chart[i, 1] = round(tmp.table[1,1], 
                                  digits = 0)
  
  # place the sample proportion for the study group in the chart
  matched.demo.chart[i, 2] = round(tmp$estimate[1], 
                                  digits = 2)
  
  # place the number of patients with this binary variable in the control group in the chart
  matched.demo.chart[i, 3] = round(tmp.table[2,1], 
                                  digits = 0)
  
  # place the sample proportion for the control group in the chart
  matched.demo.chart[i, 4] = round(tmp$estimate[2], 
                                  digits = 2)
  
  # # place the number of patients with this binary variable in the matched cohort in the chart
  # matched.demo.chart[i, 5] = round(tmp.table2[1], 
  #                                 digits = 0)
  # 
  # # place the sample proportion for the matched cohort in the chart
  # matched.demo.chart[i, 6] = round(tmp.table2[1]/nrow(match.subgroup.One), 
  #                                 digits = 2)
  
  # place the p value from the two sample test for proportions in the chart  
  matched.demo.chart[i, 5] = round(tmp$p.value,
                                  digits = 3)
  
}

# Fill in the chart for categorical variables ####
# For race ####

# create a table for the number of patients in each category of race
tmp.table = table(match.subgroup.One[, c("Group", "New.Race")])[c(2,1), ]

# conduct a chi square test for race
tmp = chisq.test(tmp.table)

# create a slot for each category of race
for (i in colnames(tmp.table)){
  
  # place the number of patients in the current category that are in the study group in the chart
  matched.demo.chart[i, 1] = tmp.table[1, i]
  
  # place the sample proportion of patients in the current category that are in the study group in the chart
  matched.demo.chart[i, 2] = round(tmp.table[1, i]/matched.demo.chart["N", 1], 
                                  digits = 2)
  
  # place the number of patients in the current category that are in the control group in the chart
  matched.demo.chart[i, 3] = tmp.table[2, i]
  
  # place the sample proportion of patients in the current category that are in the control group in the chart
  matched.demo.chart[i, 4] = round(tmp.table[2, i]/matched.demo.chart["N", 3],
                                  digits = 2)
  
  # # place the number of patients in the current category that are in the matched cohort in the chart
  # matched.demo.chart[i, 5] = table(match.subgroup.One[, "New.Race"])[i]
  # 
  # # place the sample proportion of patients in the current category that are in the matched cohort in the chart
  # matched.demo.chart[i, 6] = round(table(match.subgroup.One[, "New.Race"])[i]/matched.demo.chart["N", 5],
  #                                 digits = 2)
  
}

# place p-value from chi squared test for race in the chart
matched.demo.chart["Race", 5] = round(tmp$p.value, digits = 3)

# re order chart to place race categories under race
matched.demo.chart = matched.demo.chart[c(1:5,
                                        60,
                                        57:59,
                                        6:56), ]

# delete the New Race row
matched.demo.chart = matched.demo.chart[-which(rownames(matched.demo.chart) == "New.Race"), ]

# For health insurance ####

# create a table for the number of patients in each category of health insurance
tmp.table = table(match.subgroup.One[, c("Group", "Health.Insurance.Name")])[c(2,1), 1:3] +
  cbind(c(0,0), 
        c(0,0),
        table(match.subgroup.One[, c("Group", "Health.Insurance.Name")])[c(2,1), 4])

tmp.table2 = table(match.subgroup.One[, "Health.Insurance.Name"])[1:3] +
  c(0,0,table(match.subgroup.One[, "Health.Insurance.Name"])[4])

# conduct a chi square test for health insurance
tmp = chisq.test(tmp.table)

# create a slot for each category of health insurance
for (i in colnames(tmp.table)){
  
  # place the number of patients in the current category that are in the study group in the chart
  matched.demo.chart[i, 1] = tmp.table[1, i]
  
  # place the sample proportion of patients in the current category that are in the study group in the chart
  matched.demo.chart[i, 2] = round(tmp.table[1, i]/matched.demo.chart["N", 1], 
                                  digits = 2)
  
  # place the number of patients in the current category that are in the control group in the chart
  matched.demo.chart[i, 3] = tmp.table[2, i]
  
  # place the sample proportion of patients in the current category that are in the control group in the chart
  matched.demo.chart[i, 4] = round(tmp.table[2, i]/matched.demo.chart["N", 3],
                                  digits = 2)
  
  # # place the number of patients in the current category that are in the matched cohort in the chart
  # matched.demo.chart[i, 5] = tmp.table2[i]
  # 
  # # place the sample proportion of patients in the current category that are in the matched cohort in the chart
  # matched.demo.chart[i, 6] = round(tmp.table2[i]/matched.demo.chart["N", 5],
  #                                 digits = 2)
  
}

# place p-value from chi squared test for health insurance in the chart
matched.demo.chart["Health.Insurance.Name", 5] = round(tmp$p.value, digits = 3)

# re order chart to place race categories under health insurance
matched.demo.chart = matched.demo.chart[c(1:10, 
                                        60:62, 
                                        11:59), ]

# For type of stroke ####

# create a table for the number of patients in each category of type of stroke
tmp.table = table(match.subgroup.One[, c("Group", "Type.of.Stroke")])[c(2,1), ]

# conduct a chi square test for type of stroke
tmp = chisq.test(tmp.table)

# create a slot for each category of type of stroke
for (i in colnames(tmp.table)){
  
  # place the number of patients in the current category that are in the study group in the chart
  matched.demo.chart[i, 1] = tmp.table[1, i]
  
  # place the sample proportion of patients in the current category that are in the study group in the chart
  matched.demo.chart[i, 2] = round(tmp.table[1, i]/matched.demo.chart["N", 1], 
                                  digits = 2)
  
  # place the number of patients in the current category that are in the control group in the chart
  matched.demo.chart[i, 3] = tmp.table[2, i]
  
  # place the sample proportion of patients in the current category that are in the control group in the chart
  matched.demo.chart[i, 4] = round(tmp.table[2, i]/matched.demo.chart["N", 3],
                                  digits = 2)
  
  # # place the number of patients in the current category that are in the matched cohort in the chart
  # matched.demo.chart[i, 5] = table(match.subgroup.One[, "Type.of.Stroke"])[i]
  # 
  # # place the sample proportion of patients in the current category that are in the matched cohort in the chart
  # matched.demo.chart[i, 6] = round(table(match.subgroup.One[, "Type.of.Stroke"])[i]/matched.demo.chart["N", 5],
  #                                 digits = 2)
  
}

# place p-value from chi squared test for type of stroke in the chart
matched.demo.chart["Type.of.Stroke", 5] = round(tmp$p.value, digits = 3)

# re order chart to place stroke categories under type of stroke
matched.demo.chart = matched.demo.chart[c(1:15, 
                                        63:65, 
                                        16:62), ]

# # For modified rankin score ####
# 
# # create a table for the number of patients in each category of race
# tmp.table = table(match.subgroup.One[, c("Group", "ModRankinScore")])
# 
# # conduct a chi square test for race
# tmp = chisq.test(tmp.table)
# 
# # create a slot for each category of race
# for (i in colnames(tmp.table)){
#   
#   # place the number of patients in the current category that are in the study group in the chart
#   matched.demo.chart[i, 1] = tmp.table[1, i]
#   
#   # place the sample proportion of patients in the current category that are in the study group in the chart
#   matched.demo.chart[i, 2] = round(tmp.table[1, i]/matched.demo.chart["N", 1], 
#                                   digits = 2)
#   
#   # place the number of patients in the current category that are in the control group in the chart
#   matched.demo.chart[i, 3] = tmp.table[2, i]
#   
#   # place the sample proportion of patients in the current category that are in the control group in the chart
#   matched.demo.chart[i, 4] = round(tmp.table[2, i]/matched.demo.chart["N", 3],
#                                   digits = 2)
#   
#   
# }
# 
# # place p-value from chi squared test for race in the chart
# matched.demo.chart["ModRankinScore", 5] = round(tmp$p.value, digits = 3)
# 
# # re order chart to place modified rankin scores under modified rankin score
# matched.demo.chart = matched.demo.chart[c(1:69, 76:81, 70:75), ]

# Give the chart the correct format ####

# modify gender variable
matched.demo.chart["Male", ] = c(matched.demo.chart["Gender", 1:4], NA)

# re order chart to place gender categories under gender
matched.demo.chart = matched.demo.chart[c(1:4, 
                                        66, 
                                        5:65), ]

# matched.demo.chart["Female", ] = c(matched.demo.chart["N", 1] - matched.demo.chart["Gender", 1],
#                                   1 - matched.demo.chart["Gender", 2],
#                                   matched.demo.chart["N", 3] - matched.demo.chart["Gender", 3],
#                                   1 - matched.demo.chart["Gender", 4],
#                                   NA)

# initialize the data frame that will hold correctly formatted chart
matched.demo.chart.final = as.data.frame(matrix(NA, 
                                               nrow(matched.demo.chart), 
                                               4))

# give the column names for the correctly formatted chart 
colnames(matched.demo.chart.final) = c("Variables",
                                      "BR",
                                      "IR",
                                      #                                      "Total",
                                      "P-Value")

# give the new chart the same row names of the old chart
rownames(matched.demo.chart.final) = rownames(matched.demo.chart)

# give the new chart the same entries for relevant columns
matched.demo.chart.final[, "Variables"] = rownames(matched.demo.chart)
matched.demo.chart.final[, "P-Value"] = matched.demo.chart[, "P-Value"]
matched.demo.chart.final["N", 2:3] = c(matched.demo.chart[1, c(3,1)]) 

# cycle through the variables in the chart
for (i in matched.demo.chart.final[, "Variables"]){
  
  # check if the variable is continuous
  if (i %in% cont.list){
    
    # place the mean plus/minus the standard deviation for the control group in the chart
    matched.demo.chart.final[i, "BR"] = 
      paste(matched.demo.chart[i, 3], 
            "\u00B1", 
            matched.demo.chart[i, 4], 
            sep = "")
    
    # place the mean plus/minus the standard deviation for the study group in the chart    
    matched.demo.chart.final[i, "IR"] = 
      paste(matched.demo.chart[i, 1], 
            "\u00B1", 
            matched.demo.chart[i, 2], 
            sep = "")
    
    # # place the mean plus/minus the standard deviation for the matched cohort in the chart    
    # matched.demo.chart.final[i, "Total"] = 
    #   paste(matched.demo.chart[i, 5], 
    #         "\u00B1", 
    #         matched.demo.chart[i, 6], 
    #         sep = "")
    
    # check if the variable is binary    
  }else if (i %in% bin.list){
    
    # place the sample proportion and number of people with the condition for the control group in the chart
    matched.demo.chart.final[i, "BR"] = 
      paste(matched.demo.chart[i, 3], 
            " (", 
            100*matched.demo.chart[i, 4], 
            ")", 
            sep = "")
    
    # place the sample proportion and number of people with the condition for the study group in the chart
    matched.demo.chart.final[i, "IR"] = 
      paste(matched.demo.chart[i, 1], 
            " (", 
            100*matched.demo.chart[i, 2], 
            ")", 
            sep = "")
    
    # # place the sample proportion and number of people with the condition for the matched cohort in the chart    
    # matched.demo.chart.final[i, "Total"] = 
    #   paste(matched.demo.chart[i, 5], 
    #         " (", 
    #         100*matched.demo.chart[i, 6], 
    #         ")", 
    #         sep = "")
    
    # check if the variable is a category
  }else if (i != "N" & 
            i != "Race" & 
            i != "Type.of.Stroke" & 
            i != "Health.Insurance.Name"){
    
    # place the sample proportion and number of people in this category for the control group in the chart
    matched.demo.chart.final[i, "BR"] = 
      paste(matched.demo.chart[i,3], 
            " (", 
            100*matched.demo.chart[i,4], 
            ")", 
            sep = "")
    
    # place the sample proportion and number of people in this category for the study group in the chart
    matched.demo.chart.final[i, "IR"] = 
      paste(matched.demo.chart[i,1], 
            " (", 
            100*matched.demo.chart[i,2], 
            ")", 
            sep = "")
    
    # # place the sample proportion and number of people with the condition for the matched cohort in the chart    
    # matched.demo.chart.final[i, "Total"] = 
    #   paste(matched.demo.chart[i, 5], 
    #         " (", 
    #         100*matched.demo.chart[i, 6], 
    #         ")", 
    #         sep = "")
    
  }
  
}

# label the p-value as significant or not significant
matched.demo.chart.final[is.na(matched.demo.chart.final[, "P-Value"]) == 0 &
                          matched.demo.chart.final[, "P-Value"] > 0.05, "P-Value"] = "NS"

# place <0.001 in the chart if the p value is less than 0.001
matched.demo.chart.final[is.na(matched.demo.chart.final[, "P-Value"]) == 0 &
                          matched.demo.chart.final[, "P-Value"] < 0.001, "P-Value"] = "<0.001"

# make the missing values in the chart blank
matched.demo.chart.final[is.na(matched.demo.chart.final)] = ""

# delete Gender numbers, only leaving significance
matched.demo.chart.final["Gender", 2:3] = rep("", 2)

# rename the rownames as numbers
rownames(matched.demo.chart.final) = 1:dim(matched.demo.chart.final)[1]

# Write chart to excel file ####
write.csv(matched.demo.chart.final, 
          "docs/Descriptive Chart After Matching 3-8-2018.csv",
          row.names = FALSE)