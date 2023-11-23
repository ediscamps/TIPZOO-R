# last edit : 19 december 2022

# first, set your working directory ; use the following if necessary:
setwd("E:/Boulot/MyCore/TIPZOO/-R/TIPZOO-R")

#checking if you have the appropriate packages
require(readxl)
require(DescTools)
require(ggplot2)
require(ggrepel)
require(WriteXLS)
# if necessary, install PERL from https://www.perl.org/

#loading TIPZOO functions
source("source/TIPZOO.R")

########################################################################################################
###### PART 1: importing your dataset, review the following steps (file names, folders, group, etc)
########################################################################################################

## importing your NDE dataset
# the first parameter of the following function is your Excel file name
# the second parameter is the group you intend to use (e.g 'SPATIAL::USfield' or 'SPATIAL::Group1')
# the third parameter is the list of NDEcodes you wish to use depending on the species
# the fourth parameter: by default, this script removes foetus bones from the skeletal-part analysis

### BE SURE TO SELECT THE PROPER REFERENCE DATA ACCORDING TO YOUR SPECIES
# a full list is available in the ref_data_README file in the ref_data subfolder
# e.g. BoneDensity_RANG-Lametal99 for Reindeer density values, BoneDensity_Bison-Kreutzer92 for Bison density values, etc


## choose one of these two lines if you want to import reindeer data for USfield or Group1
dataset <- dataprep_NDE("for_import/NDEDataRANG.xlsx",'SPATIAL::USfield', species.codes = "ref_data/NDEcodes_RANGwithmes.csv", exclude.foetus = "TRUE")
#dataset <- dataprep_NDE("for_import/NDEDataRANG.xlsx",'SPATIAL::Group1', species.codes = "ref_data/NDEcodes_RANGwithmes.csv", exclude.foetus = "TRUE")


#separating the dataset in a more useful form ; you do not need to edit the following
NDEData <- dataset[["NDEData"]]
MAUfromNDE <- dataset[["MAUfromNDE"]]
MNIfromNDE <- dataset[["MNIfromNDE"]]
MAUbyElement <- dataset[["MAUbyElement"]]
MAUbyElementPortion <- dataset[["MAUbyElementPortion"]]
pMAUbyElement <- dataset[["pMAUbyElement"]]
pMAUbyElementPortion <- dataset[["pMAUbyElementPortion"]]

#export XLS for NDE, MAU and MNI data
WriteXLS(c("NDEData","MAUfromNDE", "MNIfromNDE", "MAUbyElement", "MAUbyElementPortion", "pMAUbyElement","pMAUbyElementPortion"), "export/NDE-MAU_PivotTables.xlsx", row.names = TRUE, col.names = TRUE)

#export CSV (%MAU) for TIPZOO-QGIS
write.csv(pMAUbyElement, "export/pMAUbyElement_forQGIS.csv")


########################################################################################################
###### END OF PART 1: one Excel and one CSV file have been created in the export folder of TIPZOO-R
########################################################################################################
###### PART 2: select your species and referential datasets for bone density and marrow indices
########################################################################################################

### BE SURE TO SELECT THE PROPER REFERENCE DATA ACCORDING TO YOUR SPECIES
# a full list is available in the ref_data_README file in the ref_data subfolder
BoneDensityData <- bonedensity( pMAUbyElementPortion, ref = "ref_data/BoneDensity_RANG-Lametal99.csv")

MarrowUMIData <- mergeindex(pMAUbyElement, ref = "ref_data/MarrowUMI_RANG.csv")  # for RANG UMI marrow index (Morin 2007)
MarrowCavData <- mergeindex(pMAUbyElement, ref = "ref_data/MarrowCav_RANG.csv")  # for RANG marrow-cavity volume (Binford 1978, in mL)
FoodSFUIData <- mergeindex(pMAUbyElement, ref = "ref_data/FoodSFUI_RANG.csv")  # for RANG SFUI food utility index (Metcalfe & Jones 1988)

#export XLS for BoneDensity and Marrow and Food utiliy Data
WriteXLS(c("BoneDensityData","MarrowUMIData","MarrowCavData", "FoodSFUIData"), "export/BoneDensity-Marrow-Food_Data.xlsx", row.names = TRUE, col.names = TRUE)

########################################################################################################
###### END OF PART 2: an Excel file has been created in the export folder of TIPZOO-R
########################################################################################################
###### PART 3: creating plots/graphs (you can edit the group that you want to use for analysis)
########################################################################################################

### the following creates biplots and spearman tests (including pMAU=0 values) for AllGroup (replace AllGroup with the ref of your Group to test another assemblage)
# biplots

BoneDensityPlot <- ggplot(BoneDensityData, aes(x = Density, y= AllGroup)) + 
       geom_point(size=2) +
geom_text_repel(aes(label = ElementPortion),    size = 3.5) +
  xlab("Bone mineral density") + 
  ylab("%MAU calculated from NDE values (AllGroup)")

BoneDensityPlot

MarrowUMIPlot <- ggplot(MarrowUMIData, aes(x = UMI, y= AllGroup)) + 
  geom_point(size=2) +
  geom_text_repel(aes(label = Element),    size = 3.5) +
  xlab("UMI") + 
  ylab("%MAU calculated from NDE values (AllGroup)")

MarrowUMIPlot

MarrowCavPlot <- ggplot(MarrowCavData, aes(x = Marrow.cavity, y= AllGroup)) + 
  geom_point(size=2) +
  geom_text_repel(aes(label = Element),    size = 3.5)  +
  xlab("Marrow-cavity volume (mL)") + 
  ylab("%MAU calculated from NDE values (AllGroup)")

MarrowCavPlot

FoodSFUIPlot <- ggplot(FoodSFUIData, aes(x = SFUI, y= AllGroup)) + 
  geom_point(size=2) +
  geom_text_repel(aes(label = Element),    size = 3.5)  +
  xlab("SFUI") + 
  ylab("%MAU calculated from NDE values (AllGroup)")

FoodSFUIPlot

# spearman correlation tests
cor.test(BoneDensityData$Density, BoneDensityData$AllGroup, method="spearman")
cor.test(MarrowUMIData$UMI, MarrowUMIData$AllGroup, method="spearman")
cor.test(MarrowCavData$Marrow.cavity, MarrowCavData$AllGroup, method="spearman")
cor.test(FoodSFUIData$SFUI, FoodSFUIData$AllGroup, method="spearman")

###########################################################################################################
###### END OF PART 3: plots have been created, and Spearman correlations tests are available in the console
###########################################################################################################

