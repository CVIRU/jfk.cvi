#################################Program Description################################
#Name: DEID and Combine (Round 3)                                                  #
#Author: Traymon Beavers                                                           #
#Date Created: 6/28/2017                                                           #
#Purpose: To de-identify the JFK data for use outside of the CVI and combine the   #
#         three separate datasets into one, outputting these deidentified datasets #
#         into new csv files; third round of data received                         #
####################################################################################

# upload data in R 
Demdata=read.csv("qry_TBL_Demographics_1to478Extract6212017.csv")
Mesdata=read.csv("qry_TBL_Measurement_1to478Extract_06212017.csv")
MedHisdata=read.csv("qry_TBLMedHist_1to478Extract6212017.csv")

# Check for accidental missing data and fix it ####

# look for missing patients and count them 
# setdiff(1:max(Demdata[,"Cat_No"]), Demdata[,"Cat_No"])
# 
# length(setdiff(1:max(Demdata[,"Cat_No"]), Demdata[,"Cat_No"]))
# 
### gaps in Cat_No: 52, 262, 329-331, 333-338, 379-388, 420 436 442-448 are missing (30)

# look for duplicate Cat_No, check that they're different patients, and count them
# View(Demdata[Demdata[,"Cat_No"] %in% Demdata[which(duplicated(Demdata[,"Cat_No"])), "Cat_No"], 
#              c("Cat_No", "First.Name", "Last.Name")])
# 
# dim(Demdata[Demdata[,"Cat_No"] %in% Demdata[which(duplicated(Demdata[,"Cat_No"])), "Cat_No"], 
#              c("Cat_No", "First.Name", "Last.Name")])

### 393, 394, 395, 397, 398, 400, 401, 402, 453, 455, 456, 457, 458, 459, 460, 461, 462, 463 used twice (13)

# check if patients are in some datasets but missing in others
# setdiff(Demdata$Cat_No, MedHisdata$Cat_No)

### six patients are in the demographics dataset but not in the medical history dataset
### 34 143 152 326 328 346 are their Cat_No

# setdiff(Demdata$Medical.Record.No, Mesdata$Medical.Record.No)

### one patient is in the demographics dataset but not in the measurements dataset, run above code for their
### medical record number, their Cat_No is 152

# delete patients that aren't in all three datasets
Demdata=Demdata[-which(Demdata[,"Cat_No"] %in% setdiff(Demdata$Cat_No, MedHisdata$Cat_No)),]

# Create new ID variable based on medical record number ####

# de-identify data except for medical record number
New.Demdata=Demdata[,c(-1:-4, -6:-10, -15:-17, -19, -26:-43)]
New.Mesdata=Mesdata[,c(-1, -3:-5, -51:-54)]
New.MedHisdata=MedHisdata[,c(-1, -4:-7, -64:-68)]

# sort by medical record number
Sort.Demdata=New.Demdata[order(New.Demdata$Medical.Record.No),]
Sort.Mesdata=New.Mesdata[order(New.Mesdata$Medical.Record.No),]
Sort.MedHisdata=New.MedHisdata[order(New.MedHisdata$Medical.Record.No),]

# create ID variable for demographics and medical history datasets
Sort.Demdata[,"ID"]=1:dim(Demdata)[1]
Sort.MedHisdata[,"ID"]=1:dim(MedHisdata)[1]

# initialize counter to create ID variable for measurement dataset
A=1

# assign the first ID
Sort.Mesdata[1,"ID"]=1

# cycle through every observation in the measurement dataset
for (i in 2:dim(Mesdata)[1]){
  
  # assign the same ID to every observation with the same medical record number
  if (Sort.Mesdata[i,"Medical.Record.No"]!=Sort.Mesdata[(i-1),"Medical.Record.No"]){
    A=A+1
  }
  
  Sort.Mesdata[i,"ID"]=A
  
}

# reorder data so that ID is the first variable
Order.Demdata=cbind(Sort.Demdata[,"ID"],Sort.Demdata[,1:(dim(Sort.Demdata)[2]-1)])
Order.Mesdata=cbind(Sort.Mesdata[,"ID"],Sort.Mesdata[,1:(dim(Sort.Mesdata)[2]-1)])
Order.MedHisdata=cbind(Sort.MedHisdata[,"ID"],Sort.MedHisdata[,1:(dim(Sort.MedHisdata)[2]-1)])

# reset column names
colnames(Order.Demdata)=c("ID",colnames(Order.Demdata)[2:dim(Order.Demdata)[2]])
colnames(Order.Mesdata)=c("ID",colnames(Order.Mesdata)[2:dim(Order.Mesdata)[2]])
colnames(Order.MedHisdata)=c("ID",colnames(Order.MedHisdata)[2:dim(Order.MedHisdata)[2]])

# create dataset to link ID numbers to medical record number
Link=Order.Demdata[,1:2]

# remove medical record number variable
DEID.Demdata=Order.Demdata[,-2]
DEID.Mesdata=Order.Mesdata[,-2]
DEID.MedHisdata=Order.MedHisdata[,-2]

# Combine the three datasets into a master dataset ####

# combine Demographics and Medical History
Semi.DEID.Master=cbind(DEID.Demdata, DEID.MedHisdata)

# delete extra ID variable
Semi.DEID.Master=Semi.DEID.Master[,-(dim(DEID.Demdata)[2]+1)]

# initialize counter to bind each person to their respective measurements
B=1

# bind the first measurements
DEID.Master=cbind(Semi.DEID.Master[1,], DEID.Mesdata[1,])

# cycle through every observation in the de-identified measurement dataset
for (i in 2:dim(DEID.Mesdata)[1]){
  
  # bind the same measurement to every observation with the same ID
  if(DEID.Mesdata[i,"ID"]!=B){
    B=B+1
  }
  
  DEID.Master1=cbind(Semi.DEID.Master[B,], DEID.Mesdata[i,])
  
  # update the master de-identified set with the newly binded observation
  DEID.Master=rbind(DEID.Master, DEID.Master1)
  
}

# delete extra ID variable
DEID.Master=DEID.Master[,-(dim(Semi.DEID.Master)[2]+1)]

# Write the dataframes to csv files ####
write.csv(DEID.Master, "DEID_All3.csv")
write.csv(DEID.Demdata, "DEID_Demographics3.csv")
write.csv(DEID.MedHisdata, "DEID_Medical_History3.csv")
write.csv(DEID.Mesdata, "DEID_Measurements3.csv")