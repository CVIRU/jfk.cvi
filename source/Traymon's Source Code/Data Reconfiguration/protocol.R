# find out information for protocol

# upload Data in R
OldMaster = read.csv("data/DEID_All7.csv")

OldMaster = OldMaster[OldMaster[, "ID"] != 901, ]

CVG.data1 = OldMaster[OldMaster[, "DaysId"] == 10, ]

StudyGroupIDs = unique(CVG.data1[CVG.data1[, "Group"] == "Study Group" | 
                                   CVG.data1[, "Group"] == "Study - No CVG", "ID"])


CVG.data1 = CVG.data1[CVG.data1[, "ID"] %in% StudyGroupIDs, ]

CVG.data2 = OldMaster[OldMaster[, "DaysId"] == 11, ]

BTC.table = table(CVG.data1[CVG.data1[, "ID"] %in% StudyGroupIDs, "BTC_Text"])

for.table = data.frame(Reason = names(BTC.table),
                       Number = as.numeric(BTC.table))[-1, ]

for.table1 = for.table[for.table[, 2] != 0, ]

for.table1[, "Percent"] = 100*round(for.table1[, "Number"]/174,5)

for.table1[, "Category"] = NA

for (i in 1:nrow(for.table1)){
  
  text.ID = CVG.data1[as.character(CVG.data1[, "BTC_Text"]) == as.character(for.table1[i, 1]), "ID"]
  
  if (length(text.ID) == 1){
    
    if (text.ID == 126){
      
      for.table1[i, "Category"] = "NO CVG"
      
    }else{
      
      for.table1[i, "Category"] = as.character(unique(CVG.data2[CVG.data2[, "ID"] %in% text.ID, "CVG.Participant_Descr"]))
      
    }
    
  }else{
    
    for.table1[i, "Category"] = as.character(unique(CVG.data2[CVG.data2[, "ID"] %in% text.ID, "CVG.Participant_Descr"]))
    
  }
  
}




write.csv(for.table1,
          row.names = FALSE,
          "tmp/table for protocol.csv")


