#################################Program Description################################
#Name: Descriptive Chart (All Patients)                                            #
#Author: Traymon Beavers                                                           #
#Depends: interpolate.R                                                            #
#Date Created: 7/21/2017                                                           #
#Date Updated: 9/15/2017                                                           #
#Purpose: To create and save a descriptive chart comparing the study group and     #
#         control group patients; entire sample                                    #
####################################################################################

# Load the necessary source code ####
source("source/Traymon's Source Code/Data Reconfiguration/interpolate.R")

# Initialize the descriptive chart ####

# initialize the descriptive chart
entire.demo.chart = as.data.frame(matrix(NA,
                                         length(colnames(NewMaster.One)[c(2:3,
                                                                          8:12,
                                                                          17:24,
                                                                          30:34,
                                                                          37:44,
                                                                          46:68,
                                                                          74:80,
                                                                          89,
                                                                          116:121)]) + 1,
                                         5))

# name the rows of the descriptive chart
rownames(entire.demo.chart) = c("N", 
                                colnames(NewMaster.One)[c(2:3,
                                                          8:12,
                                                          17:24,
                                                          30:34,
                                                          37:44,
                                                          46:68,
                                                          74:80,
                                                          89,
                                                          116:121)])


# name the columns of the descriptive chart
colnames(entire.demo.chart) = c("Study(Mean/Number)", 
                                "Study(SD/Percent)", 
                                "Control(Mean/Number)", 
                                "Control(SD/Percent)", 
                                "P-Value")

# make list of continuous variables
cont.list = rownames(entire.demo.chart)[c(2,
                                          8,
                                          17:20,
                                          22:29,
                                          53:59,
                                          61:66)]



# make list of continuous variables
bin.list = rownames(entire.demo.chart)[c(3:5,
                                         10:16,
                                         21,
                                         30:52)]


# make list of continuous variables
cat.list = rownames(entire.demo.chart)[c(6,
                                         7,
                                         9,
                                         60)]

# Fill in the chart for continuous variables ####

# fill in the number of patients in each group
entire.demo.chart["N",] = c(dim(NewMaster.One.Study)[1], 
                            NA,
                            dim(NewMaster.One.Control)[1],
                            NA,
                            NA)

# cycle through the continuous variables
for (i in cont.list[c(1:14,22:27)]){
  
  # conduct a two sample t test for the current variable
  tmp = t.test(NewMaster.One.Study[, i],
               NewMaster.One.Control[, i])
  
  # place the sample mean for the study group in the chart
  entire.demo.chart[i, 1] = round(tmp$estimate[1], 
                                  digits = 1)
  
  # place the sample standard deviation for the study group in the chart
  entire.demo.chart[i, 2] = round(sqrt(var(NewMaster.One.Study[, i], 
                                           na.rm = TRUE)), 
                                  digits = 1)
  
  # place the sample mean for the control group in the chart  
  entire.demo.chart[i, 3] = round(tmp$estimate[2],
                                  digits = 1)
  
  # place the sample standard deviation for the study group in the chart
  entire.demo.chart[i, 4] = round(sqrt(var(NewMaster.One.Control[, i], 
                                           na.rm = TRUE)), 
                                  digits = 1)
  
  # place the p value from the two sample t test in the chart  
  entire.demo.chart[i, 5] = round(tmp$p.value, 
                                  digits = 3)
  
}

for (i in cont.list[c(15:21)]){
  
  # conduct a two sample t test for the current variable
  tmp = t.test(NewMaster[NewMaster[, "ID"] %in% StudyGroupIDs & NewMaster[,"DaysId"] == 1, i],
               NewMaster[NewMaster[, "ID"] %in% ControlGroupIDs & NewMaster[,"DaysId"] == 1, i])
  
  # place the sample mean for the study group in the chart
  entire.demo.chart[i, 1] = round(tmp$estimate[1],
                                  digits = 1)
  
  # place the sample standard deviation for the study group in the chart
  entire.demo.chart[i, 2] = round(sqrt(var(NewMaster[NewMaster[, "ID"] %in% StudyGroupIDs & NewMaster[,"DaysId"] == 1, i], 
                                           na.rm = TRUE)), 
                                  digits = 1)
  
  # place the sample mean for the control group in the chart  
  entire.demo.chart[i, 3] = round(tmp$estimate[2], 
                                  digits = 1)
  
  # place the sample standard deviation for the study group in the chart
  entire.demo.chart[i, 4] = round(sqrt(var(NewMaster[NewMaster[, "ID"] %in% ControlGroupIDs & NewMaster[,"DaysId"] == 1, i], 
                                           na.rm = TRUE)), 
                                  digits = 1)
  
  # place the p value from the two sample t test in the chart  
  entire.demo.chart[i, 5] = round(tmp$p.value, 
                                  digits = 3)
  
}

# Fill in the chart for binary variables ####

# cycle through the binary variables
for (i in bin.list){
  
  # skip over these two variables since no patients had these conditions
  if (i != "Hormone.Replacement.Therapy" &i != "Sickle.Cell.Anemia" &i != "Current.pregnancy"){
    
    # construct a 2 by 2 table for the current variable
    tmp.table = table(NewMaster.One[c("Group", i)])[c(1,2),c(2,1)]
    
    # delete the "Other" option from the table
    if (i == "Hispanic.Ethnicity"){
      
      # construct a 2 by 2 table for the current variable
      tmp.table = table(NewMaster.One[c("Group", i)])[c(2,1),c(3,1)]  
      
    }
    
    # conduct a two sample test for proportions the current variable
    tmp = prop.test(tmp.table)
    
    # place the number of patients with this binary variable in the study group in the chart
    entire.demo.chart[i, 1] = round(tmp.table[1,1], 
                                    digits = 0)
    
    # place the sample proportion for the study group in the chart
    entire.demo.chart[i, 2] = round(tmp$estimate[1], 
                                    digits = 2)
    
    # place the number of patients with this binary variable in the control group in the chart
    entire.demo.chart[i, 3] = round(tmp.table[2,1], 
                                    digits = 0)
    
    # place the sample proportion for the control group in the chart
    entire.demo.chart[i, 4] = round(tmp$estimate[2], 
                                    digits = 2)
    
    # place the p value from the two sample test for proportions in the chart  
    entire.demo.chart[i, 5] = round(tmp$p.value,
                                    digits = 3)
    
  }
  
}


# Fill in the chart for categorical variables ####
# For race ####

# create a table for the number of patients in each category of race
tmp.table = table(NewMaster.One[, c("Group", "New.Race")])

# conduct a chi square test for race
tmp = chisq.test(tmp.table)

# create a slot for each category of race
for (i in colnames(tmp.table)){
  
  # place the number of patients in the current category that are in the study group in the chart
  entire.demo.chart[i, 1] = tmp.table[1, i]
  
  # place the sample proportion of patients in the current category that are in the study group in the chart
  entire.demo.chart[i, 2] = round(tmp.table[1, i]/entire.demo.chart["N", 1], 
                                  digits = 2)
  
  # place the number of patients in the current category that are in the control group in the chart
  entire.demo.chart[i, 3] = tmp.table[2, i]
  
  # place the sample proportion of patients in the current category that are in the control group in the chart
  entire.demo.chart[i, 4] = round(tmp.table[2, i]/entire.demo.chart["N", 3],
                                  digits = 2)
  
  
}

# place p-value from chi squared test for race in the chart
entire.demo.chart["Race", 5] = round(tmp$p.value, digits = 3)

# re order chart to place race categories under race
entire.demo.chart = entire.demo.chart[c(1:6, 67:69, 7:66), ]

# For health insurance ####

# create a table for the number of patients in each category of health insurance
tmp.table = table(NewMaster.One[, c("Group", "Health.Insurance.Name")])[, 1:3] +
  cbind(c(0,0), c(0,0), table(NewMaster.One[, c("Group", "Health.Insurance.Name")])[, 4])

# conduct a chi square test for health insurance
tmp = chisq.test(tmp.table)

# create a slot for each category of health insurance
for (i in colnames(tmp.table)){
  
  # place the number of patients in the current category that are in the study group in the chart
  entire.demo.chart[i, 1] = tmp.table[1, i]
  
  # place the sample proportion of patients in the current category that are in the study group in the chart
  entire.demo.chart[i, 2] = round(tmp.table[1, i]/entire.demo.chart["N", 1], 
                                  digits = 2)
  
  # place the number of patients in the current category that are in the control group in the chart
  entire.demo.chart[i, 3] = tmp.table[2, i]
  
  # place the sample proportion of patients in the current category that are in the control group in the chart
  entire.demo.chart[i, 4] = round(tmp.table[2, i]/entire.demo.chart["N", 3],
                                  digits = 2)
  
}

# place p-value from chi squared test for health insurance in the chart
entire.demo.chart["Health.Insurance.Name", 5] = round(tmp$p.value, digits = 3)

# re order chart to place race categories under health insurance
entire.demo.chart = entire.demo.chart[c(1:10, 70:72, 11:69), ]

# For type of stroke ####

# create a table for the number of patients in each category of type of stroke
tmp.table = table(NewMaster.One[, c("Group", "Type.of.Stroke")])

# conduct a chi square test for type of stroke
tmp = chisq.test(tmp.table)

# create a slot for each category of type of stroke
for (i in colnames(tmp.table)){
  
  # place the number of patients in the current category that are in the study group in the chart
  entire.demo.chart[i, 1] = tmp.table[1, i]
  
  # place the sample proportion of patients in the current category that are in the study group in the chart
  entire.demo.chart[i, 2] = round(tmp.table[1, i]/entire.demo.chart["N", 1], 
                                  digits = 2)
  
  # place the number of patients in the current category that are in the control group in the chart
  entire.demo.chart[i, 3] = tmp.table[2, i]
  
  # place the sample proportion of patients in the current category that are in the control group in the chart
  entire.demo.chart[i, 4] = round(tmp.table[2, i]/entire.demo.chart["N", 3],
                                  digits = 2)
  
}

# place p-value from chi squared test for type of stroke in the chart
entire.demo.chart["Type.of.Stroke", 5] = round(tmp$p.value, digits = 3)

# re order chart to place race categories under type of stroke
entire.demo.chart = entire.demo.chart[c(1:15, 73:75, 16:72), ]

# For modified rankin score ####

# create a table for the number of patients in each category of race
tmp.table = table(NewMaster.One[, c("Group", "ModRankinScore")])

# conduct a chi square test for race
tmp = chisq.test(tmp.table)

# create a slot for each category of race
for (i in colnames(tmp.table)){
  
  # place the number of patients in the current category that are in the study group in the chart
  entire.demo.chart[i, 1] = tmp.table[1, i]
  
  # place the sample proportion of patients in the current category that are in the study group in the chart
  entire.demo.chart[i, 2] = round(tmp.table[1, i]/entire.demo.chart["N", 1], 
                                  digits = 2)
  
  # place the number of patients in the current category that are in the control group in the chart
  entire.demo.chart[i, 3] = tmp.table[2, i]
  
  # place the sample proportion of patients in the current category that are in the control group in the chart
  entire.demo.chart[i, 4] = round(tmp.table[2, i]/entire.demo.chart["N", 3],
                                  digits = 2)
  
  
}

# place p-value from chi squared test for race in the chart
entire.demo.chart["ModRankinScore", 5] = round(tmp$p.value, digits = 3)

# re order chart to place modified rankin scores under modified rankin score
entire.demo.chart = entire.demo.chart[c(1:69, 76:81, 70:75), ]

# Give the chart the correct format ####

# modify gender variable
entire.demo.chart["Male", ] = c(entire.demo.chart["Gender", 1:4], NA)

entire.demo.chart["Female", ] = c(entire.demo.chart["N", 1] - entire.demo.chart["Gender", 1],
                                  1 - entire.demo.chart["Gender", 2],
                                  entire.demo.chart["N", 3] - entire.demo.chart["Gender", 3],
                                  1 - entire.demo.chart["Gender", 4],
                                  NA)

# initialize the data frame that will hold correctly formatted chart
entire.demo.chart.final = as.data.frame(matrix(NA, 
                                               dim(entire.demo.chart), 
                                               4))

# give the column names for the correctly formatted chart 
colnames(entire.demo.chart.final) = c("Variables",
                                      "BR",
                                      "IR",
                                      "P-Value")

# give the new chart the same row names of the old chart
rownames(entire.demo.chart.final) = rownames(entire.demo.chart)

# give the new chart the same entries for relevant columns
entire.demo.chart.final[, "Variables"] = rownames(entire.demo.chart)
entire.demo.chart.final[, "P-Value"] = entire.demo.chart[, "P-Value"]
entire.demo.chart.final["N", 2:3] = c(entire.demo.chart[1, c(3,1)]) 

# cycle through the variables in the chart
for (i in entire.demo.chart.final[, "Variables"]){
  
  # check if the variable is continuous
  if (i %in% cont.list){
    
    # place the mean plus/minus the standard deviation for the control group in the chart
    entire.demo.chart.final[i, "BR"] = 
      paste(entire.demo.chart[i, 3], 
            "\u00B1", 
            entire.demo.chart[i, 4], 
            sep = "")
    
    # place the mean plus/minus the standard deviation for the study group in the chart    
    entire.demo.chart.final[i, "IR"] = 
      paste(entire.demo.chart[i, 1], 
            "\u00B1", 
            entire.demo.chart[i, 2], 
            sep = "")
    
    # check if the variable is binary    
  }else if (i %in% bin.list){
    
    # place the sample proportion and number of people with the condition for the control group in the chart
    entire.demo.chart.final[i, "BR"] = 
      paste(entire.demo.chart[i, 3], 
            " (", 
            100*entire.demo.chart[i, 4], 
            ")", 
            sep = "")
    
    # place the sample proportion and number of people with the condition for the study group in the chart
    entire.demo.chart.final[i, "IR"] = 
      paste(entire.demo.chart[i, 1], 
            " (", 
            100*entire.demo.chart[i, 2], 
            ")", 
            sep = "")
    
    # check if the variable is a category
  }else if (i != "N" & 
            i != "Race" & 
            i != "Type.of.Stroke" & 
            i != "Health.Insurance.Name" & 
            i != "ModRankinScore"){
    
    # place the sample proportion and number of people in this category for the control group in the chart
    entire.demo.chart.final[i, "BR"] = 
      paste(entire.demo.chart[i,3], 
            " (", 
            100*entire.demo.chart[i,4], 
            ")", 
            sep = "")
    
    # place the sample proportion and number of people in this category for the study group in the chart
    entire.demo.chart.final[i, "IR"] = 
      paste(entire.demo.chart[i,1], 
            " (", 
            100*entire.demo.chart[i,2], 
            ")", 
            sep = "")
    
  }
  
}

# label the p-value as significant or not significant
entire.demo.chart.final[is.na(entire.demo.chart.final[, "P-Value"]) == 0 &
                          entire.demo.chart.final[, "P-Value"] > 0.05, "P-Value"] = "NS"

# place <0.001 in the chart if the p value is less than 0.001
entire.demo.chart.final[is.na(entire.demo.chart.final[, "P-Value"]) == 0 &
                          entire.demo.chart.final[, "P-Value"] < 0.001, "P-Value"] = "<0.001"



# make the missing values in the chart blank
entire.demo.chart.final[is.na(entire.demo.chart.final)==1] = ""

# delete Gender numbers, only leaving significance
entire.demo.chart.final["Gender", 2:3] = rep("", 2)

# delete the Sickle Cell Anemia and Hormone Replacement Therapy variables
entire.demo.chart.final = entire.demo.chart.final[-c(50,59), ] 

# remove variables requested
entire.demo.chart.final = entire.demo.chart.final[-c(28,29,39,43),]

# reorder Gender variables
entire.demo.chart.final = entire.demo.chart.final[c(1:4,76:77,5:75), ]

# rename the rownames as numbers
rownames(entire.demo.chart.final) = 1:dim(entire.demo.chart.final)[1]

# Write chart to excel file ####
write.csv(entire.demo.chart.final, 
          "docs/Descriptive Chart No Matching.csv",
          row.names = FALSE)
