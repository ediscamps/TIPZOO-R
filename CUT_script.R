# last edit : 9 february 2022

########################################################################################################
###### PART 1: importing your dataset, review the following steps (file names, folders, group, etc)
########################################################################################################

# set working directory
setwd("E:/Boulot/MyCore/TIPZOO/-R/TIPZOO-R")

#checking if you have the appropriate packages
require(readxl)
require(WriteXLS)
# if necessary, install PERL from https://www.perl.org/

#loading TIPZOO functions
source("source/TIPZOO.R")

## importing your Cut dataset
# the first parameter of the following function is your Excel file name
# the second parameter is the group you intend to use (e.g 'SPATIAL::USfield' or 'SPATIAL::Group1')
# the third parameter allows you to select names of the codes activities in French (CutCodesFR.csv) or English (CutCodesEN.csv)

## choose one of these two lines if you want to import reindeer data for USfield or Group1
dataset <- dataprep_Cut(file = "for_import/CutDataRANG.xlsx", group = 'SPATIAL::USfield', cut.codes = "ref_data/CutCodesEN.csv")
#dataset <- dataprep_Cut(file = "for_import/CutDataRANG.xlsx", group = 'SPATIAL::Group1', cut.codes = "ref_data/CutCodesEN.csv")

#separating the dataset in a more useful form ; you do not need to edit the following
CutData <- dataset[["CutData"]]
CutDataLong <- dataset[["CutDataLong"]]

#export XLS
WriteXLS(c("CutData","CutDataLong"), "export/CutData_PivotTables.xlsx", row.names = TRUE, col.names = TRUE)

########################################################################################################
###### END OF PART 1: an Excel file has been created in the export folder of TIPZOO-R
########################################################################################################
