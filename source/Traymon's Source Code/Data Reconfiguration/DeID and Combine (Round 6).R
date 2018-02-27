#################################Program Description################################
#Name: DEID and Combine (Round 6)                                                  #
#Author: Traymon Beavers                                                           #
#Date Created: 2/1/2018                                                            #
#Date Updated: 2/27/2018                                                           #
#Purpose: To deidentify the JFK data for use outside of the CVI and combine the    #
#         three separate datasets into one, outputting these deidentified datasets #
#         into new csv files; 6th round of data received                           #
####################################################################################

# set the working directory to retreive the data
setwd("D:/For RWJCVI - 2018Jan30_CatNo1-798plusDeceased")

# upload data
Demdata = read.csv("Demographics_01302018.csv")
Mesdata = read.csv("Measurement_01302018.csv")
MedHisdata = read.csv("MedicalHist_01302018.csv")

# look for missing patients and count them
range(Demdata[,"Cat_No"])
setdiff(1:max(Demdata[,"Cat_No"]), Demdata[,"Cat_No"])
length(setdiff(1:max(Demdata[,"Cat_No"]), Demdata[,"Cat_No"]))
# As of 2/27/2018 there are 153 unused Cat_No from 1 to 928


# look for duplicate Cat_No, check that they're different patients, and count them
View(Demdata[Demdata[,"Cat_No"] %in% Demdata[which(duplicated(Demdata[,"Cat_No"])), "Cat_No"],
             c("Cat_No", "First.Name", "Last.Name")])

dim(Demdata[Demdata[,"Cat_No"] %in% Demdata[which(duplicated(Demdata[,"Cat_No"])), "Cat_No"],
             c("Cat_No", "First.Name", "Last.Name")])[1]
# As of 2/27/2018 there are 8 duplicated Cat_No but all are different patients

# Check if patients are in some datasets but missing in others
length(Demdata$Cat_No)
length(MedHisdata$Cat_No)
length(unique(Mesdata$Medical.Record.No))
setdiff(Demdata$Cat_No, MedHisdata$Cat_No)
setdiff(unique(Mesdata$Medical.Record.No), Demdata$Medical.Record.No)
# As of 2/27/2018 all datasets have the same Cat_No

# de-Identify Data Except for Medical Record Number
Demdata.delete = -c(1:4,6:10,15:17,19,26:43)
colnames(Demdata)[Demdata.delete]
colnames(Demdata)[-Demdata.delete]

Mesdata.delete = -c(1,3:5,51:54)
colnames(Mesdata)[Mesdata.delete]
colnames(Mesdata)[-Mesdata.delete]

MedHisdata.delete = -c(1,3:8,64:68)
colnames(MedHisdata)[MedHisdata.delete]
colnames(MedHisdata)[-MedHisdata.delete]

New.Demdata = Demdata[, Demdata.delete]
New.Mesdata = Mesdata[, Mesdata.delete]
New.MedHisdata = MedHisdata[, MedHisdata.delete]

# sort by Medical Record Number
Sort.Demdata = New.Demdata[order(New.Demdata$Medical.Record.No), ]
Sort.Mesdata = New.Mesdata[order(New.Mesdata$Medical.Record.No), ]
Sort.MedHisdata = New.MedHisdata[order(New.MedHisdata$Medical.Record.No), ]

# create new ID variable
Sort.Demdata[, "ID"] = 1:dim(Demdata)[1]
Sort.MedHisdata[, "ID"] = 1:dim(MedHisdata)[1]

A = 1

Sort.Mesdata[1, "ID"] = 1

for (i in 2:dim(Mesdata)[1]){
  
  if (Sort.Mesdata[i, "Medical.Record.No"] != Sort.Mesdata[(i-1), "Medical.Record.No"]){
    
    A = A+1
    
  }
  
  Sort.Mesdata[i, "ID"] = A
  
}

# reorder data so that ID is the first variable
Order.Demdata = cbind(Sort.Demdata[, "ID"], 
                      Sort.Demdata[, 1:(dim(Sort.Demdata)[2]-1)])
Order.Mesdata = cbind(Sort.Mesdata[, "ID"], 
                      Sort.Mesdata[, 1:(dim(Sort.Mesdata)[2]-1)])
Order.MedHisdata = cbind(Sort.MedHisdata[, "ID"], 
                         Sort.MedHisdata[, 1:(dim(Sort.MedHisdata)[2]-1)])

# reset column names
colnames(Order.Demdata) = c("ID", colnames(Order.Demdata)[2:dim(Order.Demdata)[2]])
colnames(Order.Mesdata) = c("ID", colnames(Order.Mesdata)[2:dim(Order.Mesdata)[2]])
colnames(Order.MedHisdata) = c("ID", colnames(Order.MedHisdata)[2:dim(Order.MedHisdata)[2]])

# create dataset to link ID numbers to Medical Record Numbers for later reference
Link = Order.Demdata[, 1:2]

# remove Medical Record Number variable
DEID.Demdata = Order.Demdata[, -2]
DEID.Mesdata = Order.Mesdata[, -2]
DEID.MedHisdata = Order.MedHisdata[, -2]

# Combine the three datasets into a master dataset ####

# combine Demographics and Medical History
Semi.DEID.Master = cbind.data.frame(DEID.Demdata, DEID.MedHisdata)

# delete extra ID variable
Semi.DEID.Master = Semi.DEID.Master[, -(dim(DEID.Demdata)[2]+1)]

# bind each person to their respective measurements
T = 1

DEID.Master = cbind(Semi.DEID.Master[1, ], DEID.Mesdata[1, ])

for (i in 2:dim(DEID.Mesdata)[1]){
  
  if(DEID.Mesdata[i, "ID"] != T){
    
    T = T+1
    
  }
  
  DEID.Master1 = cbind(Semi.DEID.Master[T, ], DEID.Mesdata[i, ])
  
  DEID.Master = rbind(DEID.Master, DEID.Master1)
  
}

# delete extra ID variable
DEID.Master = DEID.Master[, -(dim(Semi.DEID.Master)[2]+1)]

# check that all variables used for identification are not in the deidentified dataset
colnames(DEID.Master)

#write the dataframes to csv files
write.csv(DEID.Master, "DEID_All6.csv")
write.csv(DEID.Demdata, "DEID_Demographics6.csv")
write.csv(DEID.MedHisdata, "DEID_Medical_History6.csv")
write.csv(DEID.Mesdata, "DEID_Measurements6.csv")

# For data clean up ####

# figure out Cat_No for data clean up
Demdata[Demdata[, "Medical.Record.No"] == Link[Link[,1] == 214, 2], "Cat_No"]

