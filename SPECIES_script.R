# last edit : 9 february 2°22

# first, set your working directory ; use the following if necessary:
setwd("E:/Boulot/MyCore/TIPZOO/-R/TIPZOO-R")

#checking if you have the appropriate packages
require(readxl)
require(WriteXLS)
# if necessary, install PERL from https://www.perl.org/

#loading TIPZOO functions
source("source/TIPZOO.R")

########################################################################################################
###### PART 1: importing your dataset, review the following steps (file names, folders, group)
########################################################################################################

## importing your Species dataset
# the first parameter of the following function is your Excel file name
# the second parameter is the group you intend to use (e.g 'SPATIAL::USfield' or 'SPATIAL::Group1')
# the third parameter is the list of codes you wish to use depending on the species list (by default: "ref_data/SpeciesCodes.csv")

## choose one of these two lines if you want to import data for USfield or Group1
Species <- dataprep_species(file = "for_import/SpeciesData.xlsx", group = 'SPATIAL::USfield', species.codes = "ref_data/SpeciesCodes.csv")
#Species <- dataprep_species(file = "for_import/SpeciesData.xlsx", group = 'SPATIAL::Group1', species.codes = "ref_data/SpeciesCodes.csv")

#export XLS
WriteXLS(c("Species"), "export/Species_PivotTable.xlsx", row.names = TRUE, col.names = TRUE)

########################################################################################################
###### END OF PART 1: an Excel file has been created in the export folder of TIPZOO-R
########################################################################################################