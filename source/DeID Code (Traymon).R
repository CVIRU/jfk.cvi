###Upload Data in R

Demdata=read.csv("data/Qry_Patients to Export_Demographics -2017Feb3 by CatNo.csv")
Mesdata=read.csv("data/Qry_Patients to Export_Measurement - 2017Feb3 by MedRec#.csv")
MedHisdata=read.csv("data/Qry_Patients to Export_Medical History - 2017Feb3 by CatNo.csv")

###De-Identify Data Except for Medical Record Number

New.Demdata=Demdata[,c(-1:-4, -6:-10, -15:-17, -19, -26:-43)]
New.Mesdata=Mesdata[,c(-1, -3:-5, -42:-45)]
New.MedHisdata=MedHisdata[,c(-1, -3:-7, -65:-69)]

###Sort by Medical Record Number

Sort.Demdata=New.Demdata[order(New.Demdata$Medical.Record.No),]
Sort.Mesdata=New.Mesdata[order(New.Mesdata$Medical.Record.No),]
Sort.MedHisdata=New.MedHisdata[order(New.MedHisdata$Medical.Record.No),]

###Create new ID variable 

Sort.Demdata[,"ID"]=1:158
Sort.MedHisdata[,"ID"]=1:158

T=1

Sort.Mesdata[1,"ID"]=1

for (i in 2:926){
  if (Sort.Mesdata[i,"Medical.Record.No"]!=Sort.Mesdata[(i-1),"Medical.Record.No"]){
    T=T+1
  }
  
  Sort.Mesdata[i,"ID"]=T

}

###ReOrder Data so that ID is the first variable

Order.Demdata=cbind(Sort.Demdata[,"ID"],Sort.Demdata[,1:12])
Order.Mesdata=cbind(Sort.Mesdata[,"ID"],Sort.Mesdata[,1:37])
Order.MedHisdata=cbind(Sort.MedHisdata[,"ID"],Sort.MedHisdata[,1:58])

###Reset column names
colnames(Order.Demdata)=c("ID",colnames(Order.Demdata)[2:13])
colnames(Order.Mesdata)=c("ID",colnames(Order.Mesdata)[2:38])
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

#find the first occurences in Measurement dataset for each person

place=c(1,rep(0,157))

T=1

for (i in 1:919){
  if (DEID.Mesdata[i,"ID"]!=T){
    T=T+1
    place[T]=i
  }
}


#bind each person to their respective measurements
T=1

DEID.Master=cbind(Semi.DEID.Master[1,], DEID.Mesdata[1,])

for (i in 2:926){
  
  if(DEID.Mesdata[i,"ID"]!=T){
  T=T+1
  }
  
  DEID.Master1=cbind(Semi.DEID.Master[T,], DEID.Mesdata[i,])
  
  DEID.Master=rbind(DEID.Master, DEID.Master1)

}

#Delete extra ID variable
DEID.Master=DEID.Master[,-70]

#write the dataframes to csv files
write.csv(DEID.Master, "tmp/DEID_All.csv")
write.csv(DEID.Demdata, "tmp/DEID_Demographics.csv")
write.csv(DEID.MedHisdata, "tmp/DEID_Medical_History.csv")
write.csv(DEID.Mesdata, "tmp/DEID_Measurements.csv")


###Calculate number of people in Study Group

T=1

for (i in 2:157){
  if (Mesdata[Mesdata[,"Group"]=="Study Group", "Medical.Record.No"][i]!=Mesdata[Mesdata[,"Group"]=="Study Group", "Medical.Record.No"][i-1]){
    T=T+1
  }
}

T