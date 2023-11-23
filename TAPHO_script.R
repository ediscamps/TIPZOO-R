## last modifications: 9 february 2022

# first, set your working directory ; use the following if necessary:
setwd("E:/Boulot/MyCore/TIPZOO/-R/TIPZOO-R")

#checking if you have the appropriate packages
require(readxl)
require(Rmisc)
require(dplyr)
require(DescTools)
require(ggplot2)
require(reshape2)
require(WriteXLS)
# if necessary, install PERL from https://www.perl.org/

#loading TIPZOO functions
source("source/TIPZOO.R")

########################################################################################################
###### PART 1: importing your dataset, review the following steps (file names, folders, group)
########################################################################################################

## importing your NDE dataset
# the first parameter of the following function is your Excel file name
# the second parameter is the group you intend to use (e.g 'SPATIAL::USfield' or 'SPATIAL::Group1')

## choose one of these two lines if you want to import data for USfield or Group1
dataset <- dataprep_tapho(file = "for_import/TaphoData.xlsx", group = 'SPATIAL::USfield')
#dataset <- dataprep_tapho(file = "for_import/TaphoData.xlsx", group = 'SPATIAL::Group1')

#separating the dataset in a more useful form ; you do not need to edit the following
TaphoAccu_n <- dataset[["TaphoAccu_n"]]
TaphoAccu_Ntot <- dataset[["TaphoAccu_Ntot"]]
TaphoAccu_p <- dataset[["TaphoAccu_p"]]
TaphoNat_n <- dataset[["TaphoNat_n"]]
TaphoNat_Ntot <- dataset[["TaphoNat_Ntot"]]
TaphoNat_p <- dataset[["TaphoNat_p"]]
dfTaphoAccu_n <- dataset[["dfTaphoAccu_n"]]
dfTaphoAccu_Ntot <- dataset[["dfTaphoAccu_Ntot"]]
dfTaphoAccu_p <- dataset[["dfTaphoAccu_p"]]
dfTaphoNat_n <- dataset[["dfTaphoNat_n"]]
dfTaphoNat_Ntot <- dataset[["dfTaphoNat_Ntot"]]
dfTaphoNat_p <- dataset[["dfTaphoNat_p"]]
TableFract <- dataset[["TableFract"]]
TableConcType <- dataset[["TableConcType"]]
TableBoneCOlor <- dataset[["TableBoneColor"]]
raw_dataset <- dataset[["raw_dataset"]]

#export XLS
WriteXLS(c("dfTaphoAccu_n","dfTaphoAccu_Ntot", "dfTaphoAccu_p","dfTaphoNat_n","dfTaphoNat_Ntot", "dfTaphoNat_p", "TableFract", "TableConcType", "TableBoneCOlor", "raw_dataset"), "export/Tapho_PivotTables.xlsx", row.names = TRUE, col.names = TRUE)

########################################################################################################
###### END OF PART 2: an Excel file has been created in the export folder of TIPZOO-R
########################################################################################################
###### PART 3: creating plots/graphs, check the groups/variables to be plotted, and your legends
########################################################################################################

#barchart plot for readibility 
## you can use the last argument in the persobarNat function to select the groups you want (e.g. -1 to remove the first column, c(1:3,5) to display first to third + fifth groups, etc...)
barRead <- bar_TaphoNat("Lisibilité des surfaces corticales","Read1","Read2","Read3","Read4", groupselect = ) + theme(panel.background = element_blank(), legend.title = element_text(size=10)) + scale_fill_manual(name="% surface préservée", values=c("#D1D0D2", "#A7A9AC", "#808285", "#58595B"), labels=c("0-25%", "25-50%", "50-75%","75-100%"), guide=guide_legend(reverse=T)) 
barRead

#barchart plot for BoneColor 
barColor <- bar_createPercentage("Couleur des ossements", TableBoneCOlor, groupselect = ) + theme(panel.background = element_blank(), legend.title = element_text(size=10)) + scale_fill_manual(name="couleur", values=c("#D1D0D2", "#A7A9AC", "#808285", "#58595B")) 
barColor

# creation plots // with French legends
## you can use the last argument (groupselect) to select the groups you want (e.g. -1 to remove the first column, c(1:3,5) to display first to third + fifth groups, etc...)

pCut <- plot_TaphoAccu(var = "Cut", varname= "Stries de découpe", groupselect = )
pCtot <- plot_TaphoAccu(var = "Ctot", varname= "Traces de carnivores (dig. exclu)", groupselect = )
pDig <- plot_TaphoAccu(var = "Dig", varname= "Traces de digestion", groupselect = )
pPtot <- plot_TaphoAccu(var = "Ptot", varname= "Traces de percussion/pression", groupselect = )
pBurnt <- plot_TaphoAccu(var = "Burnt", varname= "Restes brÃ»lés", groupselect = )
pCut
pCtot
pDig
pPtot
pBurnt

# not used:"Ret","BoneTool", "CutUnsure", "RetUnsure", "BoneToolUnsure", "Chop", "Rodent", "Scrape"
# not used:"C_cren", "C_furr", "C_pit", "C_punct", "C_scoop", "C_scor"
# not used:"P_cort", "P_flake", "P_tooth", "P_scar", "P_mark", "P_oppo", "P_peel", "P_spong"

pExfo <- plot_TaphoNat(var = "Exfo", varname= "Exfoliation", groupselect = ) 
pDenD <- plot_TaphoNat(var = "DenD", varname= "Dépôts dendritiques (racines ?)", groupselect = )
pDenE <- plot_TaphoNat(var = "DenE", varname= "Creusements dendritiques (racines ?)", groupselect = )
pSheet1 <- plot_TaphoNat(var = "Sheet1", varname= "Délitement (léger)", groupselect = )
pSheet2 <- plot_TaphoNat(var = "Sheet2", varname= "Délitement (important)", groupselect = )
pAbra1 <- plot_TaphoNat(var = "Abra1", varname= "Abrasion (légère)", groupselect = )
pAbra2 <- plot_TaphoNat(var = "Abra2", varname= "Abrasion (forte)", groupselect = )
pCirE <- plot_TaphoNat(var = "CirE", varname= "Creusements en cupules", groupselect = )
pCrack1 <- plot_TaphoNat(var = "Crack1", varname= "Fissures longitudinales (légères)", groupselect = )
pCrack2 <- plot_TaphoNat(var = "Crack2", varname= "Fissures longitudinales (profondes)", groupselect = )
pConc1 <- plot_TaphoNat(var = "Conc1", varname= "Concrétions (<1/3)", groupselect = )
pConc2 <- plot_TaphoNat(var = "Conc2", varname= "Concrétions (1/3 à 2/3)", groupselect = )
pConc3 <- plot_TaphoNat(var = "Conc3", varname= "Concrétions (>2/3)", groupselect = )
pBlack1 <- plot_TaphoNat(var = "Black1", varname= "Dépôts noirs (<1/3)", groupselect = )
pBlack2 <- plot_TaphoNat(var = "Black2", varname= "Dépôts noirs (1/3 à 2/3)", groupselect = )
pBlack3 <- plot_TaphoNat(var = "Black3", varname= "Dépôts noirs (>2/3)", groupselect = )
pTramp <- plot_TaphoNat(var = "Tramp", varname= "Stries de piétinement", groupselect = )
pTrampUnsure <- plot_TaphoNat(var = "TrampUnsure", varname= "Stries de piétinement incertaines", groupselect = )
pCor1 <- plot_TaphoNat(var = "Cor1", varname= "Attaque chimique (légère)", groupselect = )
pCor2 <- plot_TaphoNat(var = "Cor2", varname= "Attaque chimique (forte)", groupselect = )
pExfo 
pDenD 
pDenE 
pSheet1
pSheet2
pAbra1
pAbra2
pCirE
pCrack1 
pCrack2
pConc1
pConc2 
pConc3 
pBlack1 
pBlack2 
pBlack3 
pTramp
pCor1
pCor2

#examples of multiplots (displaying several plots in the same figure)
multiplot(pCut,pPtot,pCtot,pDig, cols = 4)

multiplot(
pExfo,pSheet1,pSheet2,pCrack1,pCrack2,
pAbra1,pAbra2,pCirE,pCor1,pCor2,
pBlack1,pBlack2,pBlack3,pDenD,pDenE,
pConc1,pConc2,pConc3,pTramp,
cols = 4)

