# Project: JFK Stroke Recovery Program
# Description: data summary
# Author: Davit Sargsyan
# Date: 11/23/2016
#***************************************************************
require(data.table)

# Data----
dm <- fread("data/Patients to Export_Demographics 20161118.csv",
            sep = ",",
            header = TRUE)

mh <- fread("data/Patients to Export_Medical History 20161118.csv",
            sep = ",",
            header = TRUE)

vis <- fread("data/Patients to Export_Measurement 20161118.csv",
            sep = ",",
            header = TRUE)