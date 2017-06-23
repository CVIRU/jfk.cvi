#################################Program Description################################
#Name: DEID and Combine (Round 2)                                                  #
#Author: Traymon Beavers                                                           #
#Date Created: 4/17/2017                                                           #
#Purpose: To deidentify the JFK data for use outside of the CVI and combine the    #
#         three separate datasets into one, outputting these deidentified datasets #
#         into new csv files; 2nd round of data received                           #
####################################################################################

###Upload Data in R

Demdata=read.csv("qry_TBL_Demographics_1to318Extract - 14Apr2017.csv")
Mesdata=read.csv("qry_TBL_Measurement_1to318Extract - 14Apr2017.csv")
MedHisdata=read.csv("qry_TBLMedHist_1to318Extract - 14Apr2017.csv")

dim(Demdata)[2]


###Look for two missing patients
Demdata[order(Demdata[,"Cat_No"]),"Cat_No"]

###Gaps in Cat_No: 52, 262

###Show patient deleted manually last time is no longer there, accounts for gap 52
View(Demdata[order(Demdata[,"Medical.Record.No"]),c("ID", "Medical.Record.No")])


###De-Identify Data Except for Medical Record Number

New.Demdata=Demdata[,c(-1:-4, -6:-10, -15:-17, -19, -26:-43)]
New.Mesdata=Mesdata[,c(-1, -3:-5, -41:-54, -67:-70 )]
New.MedHisdata=MedHisdata[,c(-1, -4:-7, -64:-68)]

###Sort by Medical Record Number

Sort.Demdata=New.Demdata[order(New.Demdata$Medical.Record.No),]
Sort.Mesdata=New.Mesdata[order(New.Mesdata$Medical.Record.No),]
Sort.MedHisdata=New.MedHisdata[order(New.MedHisdata$Medical.Record.No),]

###Create new ID variable 

Sort.Demdata[,"ID"]=1:316
Sort.MedHisdata[,"ID"]=1:316

T=1

Sort.Mesdata[1,"ID"]=1

for (i in 2:2116){
  if (Sort.Mesdata[i,"Medical.Record.No"]!=Sort.Mesdata[(i-1),"Medical.Record.No"]){
    T=T+1
  }
  
  Sort.Mesdata[i,"ID"]=T

}

###Reorder Data so that ID is the first variable

Order.Demdata=cbind(Sort.Demdata[,"ID"],Sort.Demdata[,1:12])
Order.Mesdata=cbind(Sort.Mesdata[,"ID"],Sort.Mesdata[,1:48])
Order.MedHisdata=cbind(Sort.MedHisdata[,"ID"],Sort.MedHisdata[,1:58])

###Reset column names
colnames(Order.Demdata)=c("ID",colnames(Order.Demdata)[2:13])
colnames(Order.Mesdata)=c("ID",colnames(Order.Mesdata)[2:49])
colnames(Order.MedHisdata)=c("ID",colnames(Order.MedHisdata)[2:59])

###Create dataset to link ID numbers to Medical Record Number

Link=Order.Demdata[,1:2]

###Remove Medical Record Number variable

DEID.Demdata=Order.Demdata[,-2]

DEID.Mesdata=Order.Mesdata[,-2]

DEID.MedHisdata=Order.MedHisdata[,-2]


###Combine the three datasets into a master dataset

#Combine Demographics and Medical History
Semi.DEID.Master=cbind(DEID.Demdata, DEID.MedHisdata)

#Delete extra ID variable
Semi.DEID.Master=Semi.DEID.Master[,-13]

#bind each person to their respective measurements
T=1

DEID.Master=cbind(Semi.DEID.Master[1,], DEID.Mesdata[1,])

for (i in 2:2116){
  
  if(DEID.Mesdata[i,"ID"]!=T){
  T=T+1
  }
  
  DEID.Master1=cbind(Semi.DEID.Master[T,], DEID.Mesdata[i,])
  
  DEID.Master=rbind(DEID.Master, DEID.Master1)

}

#Delete extra ID variable
DEID.Master=DEID.Master[,-70]

#write the dataframes to csv files
write.csv(DEID.Master, "DEID_All2.csv")
write.csv(DEID.Demdata, "DEID_Demographics2.csv")
write.csv(DEID.MedHisdata, "DEID_Medical_History2.csv")
write.csv(DEID.Mesdata, "DEID_Measurements2.csv")