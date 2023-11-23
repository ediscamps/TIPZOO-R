## last modifications: 9 february 2022

# Functions used in Species script

dataprep_species <- function(file, group, species.codes)
{
# import Excel file
require(readxl)
dataset <- read_excel(file, col_types = c("numeric","guess","numeric",rep("text",9),rep("numeric",3),"guess",rep("text",9),"guess","text"))
names(dataset)[names(dataset) == group] <- 'Group'

# creating SumGroup
SumGroup <- table(dataset[,"Group"], exclude = NULL)
SumGroup.margins <- addmargins(SumGroup)

SpeciesNISP<-addmargins(table(dataset$`Group`, dataset$`SPECIES::Taxon`, exclude = NULL))
SpeciesNISP<-data.frame(cbind(t(SpeciesNISP)))

# importing species latin and common names
SpeciesCodes <- read.csv(species.codes, header=TRUE, sep=";", row.names=NULL, encoding="UTF-8")

SpeciesNISP$SpeciesCode<-row.names(SpeciesNISP)

tmp <- merge.data.frame(SpeciesCodes, SpeciesNISP, by.x = "SpeciesCode", all.y = TRUE, sort = FALSE)
rownames(tmp) <- tmp$SpeciesCode
Species <- tmp[,-1]
return(Species)
}


# Functions used in Tapho script

dataprep_tapho <- function(file, group)
{
# import Excel file (check the Excel file name)
require(readxl)
dataset <- read_excel(file, col_types = c("numeric","guess","numeric",rep("text",9),rep("numeric",3),"guess",rep("text",47),"guess","text"))

## choose the variable used for grouping remains together
names(dataset)[names(dataset) == group] <- 'Group'

# replacing NAs in the Group field
dataset[,"Group"] <- replace(dataset[,"Group"], is.na(dataset[,"Group"]),"Unknown")

# creating SumGroup
SumGroup <- table(dataset$`Group`, exclude = NULL)
SumGroup.margins <- addmargins(SumGroup)

# correcting fields that should not have been recorded (cf. manual)
tmpNotLBN <- dataset$`SKEL::Anat_Class`!="LBN"
tmpTeeth <- dataset$`SKEL::Anat`=="TTH"
tmpCarbo <- dataset$Burnt>=2
dataset$`TAPHO::Fract1`[tmpNotLBN] <- NA
dataset$`TAPHO::Fract2`[tmpNotLBN] <- NA
dataset$`TAPHO::Fract1`[tmpCarbo] <- NA
dataset$`TAPHO::Fract2`[tmpCarbo] <- NA
dataset$`TAPHO::Crack`[tmpTeeth] <- "NA"
dataset$`TAPHO::Crack`[tmpCarbo] <- "NA"
dataset$`TAPHO::Sheet`[tmpTeeth] <- "NA"
dataset$`TAPHO::Sheet`[tmpCarbo] <- "NA"
dataset$`TAPHO::Black`[tmpTeeth] <- "NA"
dataset$`TAPHO::Black`[tmpCarbo] <- "NA"
dataset$`TAPHO::DenD`[tmpCarbo] <- "NA"
dataset$`TAPHO::P_scar`[tmpTeeth] <- "NA"
dataset$`TAPHO::P_oppo`[tmpTeeth] <- "NA"
dataset$`TAPHO::P_cort`[tmpTeeth] <- "NA"
dataset$`TAPHO::P_flake`[tmpTeeth] <- "NA"
dataset$`TAPHO::P_peel`[tmpTeeth] <- "NA"
dataset$`TAPHO::P_spong`[tmpTeeth] <- "NA"
dataset$`TAPHO::C_cren`[tmpTeeth] <- "NA"
dataset$`TAPHO::C_scoop`[tmpTeeth] <- "NA"
dataset$`TAPHO::Rodent`[tmpTeeth] <- "NA"
dataset$`TAPHO::Read`[tmpTeeth] <- "NA"
dataset$`TAPHO::Exfo`[tmpTeeth] <- "NA"
dataset$`TAPHO::Exfo`[tmpCarbo] <- "NA"
dataset$`TAPHO::C_pit`[tmpTeeth] <- "NA"
dataset$`TAPHO::C_punct`[tmpTeeth] <- "NA"
dataset$`TAPHO::C_scor`[tmpTeeth] <- "NA"
dataset$`TAPHO::C_furr`[tmpTeeth] <- "NA"
rm(tmpNotLBN)
rm(tmpTeeth)
rm(tmpCarbo)

# creating TaphoAccu_n
TaphoAccu_n <- matrix(nrow = nrow(SumGroup),ncol = 27)
colnames(TaphoAccu_n) <- c("Cut","Scrape","Chop","Ret","BoneTool","Burnt","Ptot","P_scar","P_oppo","P_mark","P_flake","P_cort","P_peel","P_spong","P_tooth","Ctot","C_pit","C_punct","C_scor","C_furr","C_cren","C_scoop","Dig","Rodent","CutUnsure","RetUnsure","BoneToolUnsure")
rownames(TaphoAccu_n) <- rownames(SumGroup)

# creating TaphoAccu_Ntot
TaphoAccu_Ntot <- matrix(nrow = nrow(SumGroup),ncol = 27)
colnames(TaphoAccu_Ntot) <- c("Cut","Scrape","Chop","Ret","BoneTool","Burnt","Ptot","P_scar","P_oppo","P_mark","P_flake","P_cort","P_peel","P_spong","P_tooth","Ctot","C_pit","C_punct","C_scor","C_furr","C_cren","C_scoop","Dig","Rodent","CutUnsure","RetUnsure","BoneToolUnsure")
rownames(TaphoAccu_Ntot) <- rownames(SumGroup)

# creating TaphoAccu_p
TaphoAccu_p <- matrix(nrow = nrow(SumGroup),ncol = 27)
colnames(TaphoAccu_p) <- c("Cut","Scrape","Chop","Ret","BoneTool","Burnt","Ptot","P_scar","P_oppo","P_mark","P_flake","P_cort","P_peel","P_spong","P_tooth","Ctot","C_pit","C_punct","C_scor","C_furr","C_cren","C_scoop","Dig","Rodent","CutUnsure","RetUnsure","BoneToolUnsure")
rownames(TaphoAccu_p) <- rownames(SumGroup)

# creating TaphoNat_n
TaphoNat_n <- matrix(nrow = nrow(SumGroup),ncol = 25)
colnames(TaphoNat_n) <- c("Exfo","Sheet1","Sheet2","Crack1","Crack2","Abra1","Abra2","Black1","Black2","Black3","DenD","DenE","CirE","Cor1","Cor2","Conc1","Conc2","Conc3","Tramp","TrampUnsure","ReadNA","Read1","Read2","Read3","Read4")
rownames(TaphoNat_n) <- rownames(SumGroup)

# creating TaphoNat_Ntot
TaphoNat_Ntot <- matrix(nrow = nrow(SumGroup),ncol = 25)
colnames(TaphoNat_Ntot) <- c("Exfo","Sheet1","Sheet2","Crack1","Crack2","Abra1","Abra2","Black1","Black2","Black3","DenD","DenE","CirE","Cor1","Cor2","Conc1","Conc2","Conc3","Tramp","TrampUnsure","ReadNA","Read1","Read2","Read3","Read4")
rownames(TaphoNat_Ntot) <- rownames(SumGroup)

# creating TaphoNat_p
TaphoNat_p <- matrix(nrow = nrow(SumGroup),ncol = 25)
colnames(TaphoNat_p) <- c("Exfo","Sheet1","Sheet2","Crack1","Crack2","Abra1","Abra2","Black1","Black2","Black3","DenD","DenE","CirE","Cor1","Cor2","Conc1","Conc2","Conc3","Tramp","TrampUnsure","ReadNA","Read1","Read2","Read3","Read4")
rownames(TaphoNat_p) <- rownames(SumGroup)

# functions for _n
persosum_n1to4 <- function(group, var)
{
  persotable <- table(group, var, exclude = NULL)
  tmp <- rep(0,nrow(SumGroup))
  ifelse("1" %in% colnames(persotable), tmp<-(tmp + persotable[,c("1")]), tmp<-tmp)
  ifelse("2" %in% colnames(persotable), tmp<-(tmp + persotable[,c("2")]), tmp<-tmp)
  ifelse("3" %in% colnames(persotable), tmp<-(tmp + persotable[,c("3")]), tmp<-tmp)
  ifelse("4" %in% colnames(persotable), tmp<-(tmp + persotable[,c("4")]), tmp<-tmp)
  return(tmp)
}

persosum_n0only <- function(group, var)
{
  persotable <- table(group, var, exclude = NULL)
  tmp <- rep(0,nrow(SumGroup))
  ifelse("0" %in% colnames(persotable), tmp<-(tmp + persotable[,c("0")]), tmp<-tmp)
  return(tmp)
}

persosum_n1only <- function(group, var)
{
  persotable <- table(group, var, exclude = NULL)
  tmp <- rep(0,nrow(SumGroup))
  ifelse("1" %in% colnames(persotable), tmp<-(tmp + persotable[,c("1")]), tmp<-tmp)
  return(tmp)
}

persosum_n2only <- function(group, var)
{
  persotable <- table(group, var, exclude = NULL)
  tmp <- rep(0,nrow(SumGroup))
  ifelse("2" %in% colnames(persotable), tmp<-(tmp + persotable[,c("2")]), tmp<-tmp)
  return(tmp)
}

persosum_n3only <- function(group, var)
{
  persotable <- table(group, var, exclude = NULL)
  tmp <- rep(0,nrow(SumGroup))
  ifelse("3" %in% colnames(persotable), tmp<-(tmp + persotable[,c("3")]), tmp<-tmp)
  return(tmp)
}

persosum_n4only <- function(group, var)
{
  persotable <- table(group, var, exclude = NULL)
  tmp <- rep(0,nrow(SumGroup))
  ifelse("4" %in% colnames(persotable), tmp<-(tmp + persotable[,c("4")]), tmp<-tmp)
  return(tmp)
}

persosum_nUnsure <- function(group, var)
{
  persotable <- table(group, var, exclude = NULL)
  tmp <- rep(0,nrow(SumGroup))
  ifelse("?" %in% colnames(persotable), tmp<-(tmp + persotable[,c("?")]), tmp<-tmp)
  return(tmp)
}

# functions for _Ntot
persosum_Ntot0to4sure <- function(group, var)
{
  persotable <- table(group, var, exclude = NULL)
  tmp <- rep(0,nrow(SumGroup))
  ifelse("0" %in% colnames(persotable), tmp<-(tmp + persotable[,c("0")]), tmp<-tmp)
  ifelse("1" %in% colnames(persotable), tmp<-(tmp + persotable[,c("1")]), tmp<-tmp)
  ifelse("2" %in% colnames(persotable), tmp<-(tmp + persotable[,c("2")]), tmp<-tmp)
  ifelse("3" %in% colnames(persotable), tmp<-(tmp + persotable[,c("3")]), tmp<-tmp)
  ifelse("4" %in% colnames(persotable), tmp<-(tmp + persotable[,c("4")]), tmp<-tmp)
  return(tmp)
}

persosum_Ntot0to4unsure <- function(group, var)
{
  persotable <- table(group, var, exclude = NULL)
  tmp <- rep(0,nrow(SumGroup))
  ifelse("0" %in% colnames(persotable), tmp<-(tmp + persotable[,c("0")]), tmp<-tmp)
  ifelse("1" %in% colnames(persotable), tmp<-(tmp + persotable[,c("1")]), tmp<-tmp)
  ifelse("2" %in% colnames(persotable), tmp<-(tmp + persotable[,c("2")]), tmp<-tmp)
  ifelse("3" %in% colnames(persotable), tmp<-(tmp + persotable[,c("3")]), tmp<-tmp)
  ifelse("4" %in% colnames(persotable), tmp<-(tmp + persotable[,c("4")]), tmp<-tmp)
  ifelse("?" %in% colnames(persotable), tmp<-(tmp + persotable[,c("?")]), tmp<-tmp)
  return(tmp)
}

# populating TaphoAccu_n
TaphoAccu_n[,"BoneTool"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::BoneTool`)  
TaphoAccu_n[,"C_cren"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::C_cren`) 
TaphoAccu_n[,"C_furr"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::C_furr`) 
TaphoAccu_n[,"C_pit"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::C_pit`) 
TaphoAccu_n[,"C_punct"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::C_punct`) 
TaphoAccu_n[,"C_scoop"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::C_scoop`) 
TaphoAccu_n[,"C_scor"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::C_scor`) 
TaphoAccu_n[,"Chop"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::Chop`) 
TaphoAccu_n[,"P_cort"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::P_cort`) 
TaphoAccu_n[,"P_flake"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::P_flake`) 
TaphoAccu_n[,"P_tooth"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::P_tooth`) 
TaphoAccu_n[,"P_scar"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::P_scar`) 
TaphoAccu_n[,"P_mark"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::P_mark`) 
TaphoAccu_n[,"P_oppo"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::P_oppo`) 
TaphoAccu_n[,"P_peel"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::P_peel`) 
TaphoAccu_n[,"P_spong"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::P_spong`) 
TaphoAccu_n[,"Rodent"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::Rodent`) 
TaphoAccu_n[,"Scrape"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::Scrape`) 
TaphoAccu_n[,"Dig"]<-persosum_n3only(dataset$`Group`, dataset$`TAPHO::CorDig`)
TaphoAccu_n[,"Ctot"] <-persosum_n1only(dataset$`Group`, dataset$`TAPHO::C_SUM`) 
TaphoAccu_n[,"Ptot"] <-persosum_n1only(dataset$`Group`, dataset$`TAPHO::P_SUM`) 
TaphoAccu_n[,"Cut"] <-persosum_n1only(dataset$`Group`, dataset$`TAPHO::Cut`) 
TaphoAccu_n[,"Ret"] <-persosum_n1only(dataset$`Group`, dataset$`TAPHO::Ret`) 
TaphoAccu_n[,"Burnt"]<-persosum_n1to4(dataset$`Group`, dataset$`Burnt`)
TaphoAccu_n[,"CutUnsure"] <-persosum_nUnsure(dataset$`Group`, dataset$`TAPHO::Cut`)
TaphoAccu_n[,"RetUnsure"] <-persosum_nUnsure(dataset$`Group`, dataset$`TAPHO::Ret`) 
TaphoAccu_n[,"BoneToolUnsure"] <-persosum_nUnsure(dataset$`Group`, dataset$`TAPHO::BoneTool`) 
TaphoAccu_n <- replace(TaphoAccu_n, is.na(TaphoAccu_n),0)

# populating TaphoNat_n
TaphoNat_n[,"Exfo"] <- persosum_n1only(dataset$`Group`, dataset$`TAPHO::Exfo`)
TaphoNat_n[,"DenD"] <- persosum_n1only(dataset$`Group`, dataset$`TAPHO::DenD`)
TaphoNat_n[,"DenE"] <- persosum_n1only(dataset$`Group`, dataset$`TAPHO::DenE`)
TaphoNat_n[,"Sheet1"] <- persosum_n1only(dataset$`Group`, dataset$`TAPHO::Sheet`)
TaphoNat_n[,"Sheet2"] <- persosum_n2only(dataset$`Group`, dataset$`TAPHO::Sheet`)
TaphoNat_n[,"Abra1"] <- persosum_n1only(dataset$`Group`, dataset$`TAPHO::Abra`)
TaphoNat_n[,"Abra2"] <- persosum_n2only(dataset$`Group`, dataset$`TAPHO::Abra`)
TaphoNat_n[,"CirE"] <- persosum_n1only(dataset$`Group`, dataset$`TAPHO::CirE`)
TaphoNat_n[,"Crack1"] <- persosum_n1only(dataset$`Group`, dataset$`TAPHO::Crack`)
TaphoNat_n[,"Crack2"] <- persosum_n2only(dataset$`Group`, dataset$`TAPHO::Crack`)
TaphoNat_n[,"Conc1"] <- persosum_n1only(dataset$`Group`, dataset$`TAPHO::Conc`)
TaphoNat_n[,"Conc2"] <- persosum_n2only(dataset$`Group`, dataset$`TAPHO::Conc`)
TaphoNat_n[,"Conc3"] <- persosum_n3only(dataset$`Group`, dataset$`TAPHO::Conc`)
TaphoNat_n[,"Black1"] <- persosum_n1only(dataset$`Group`, dataset$`TAPHO::Black`)
TaphoNat_n[,"Black2"] <- persosum_n2only(dataset$`Group`, dataset$`TAPHO::Black`)
TaphoNat_n[,"Black3"] <- persosum_n3only(dataset$`Group`, dataset$`TAPHO::Black`)
TaphoNat_n[,"Tramp"] <- persosum_n1only(dataset$`Group`, dataset$`TAPHO::Tramp`)
TaphoNat_n[,"TrampUnsure"] <- persosum_nUnsure(dataset$`Group`, dataset$`TAPHO::Tramp`)
TaphoNat_n[,"Cor1"] <- persosum_n1only(dataset$`Group`, dataset$`TAPHO::CorDig`)
TaphoNat_n[,"Cor2"] <- persosum_n2only(dataset$`Group`, dataset$`TAPHO::CorDig`)
TaphoNat_n[,"ReadNA"]<-persosum_n0only(dataset$`Group`, dataset$`TAPHO::Read`)
TaphoNat_n[,"Read1"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::Read`)
TaphoNat_n[,"Read2"]<-persosum_n2only(dataset$`Group`, dataset$`TAPHO::Read`)
TaphoNat_n[,"Read3"]<-persosum_n3only(dataset$`Group`, dataset$`TAPHO::Read`)
TaphoNat_n[,"Read4"]<-persosum_n4only(dataset$`Group`, dataset$`TAPHO::Read`)
TaphoNat_n <- replace(TaphoNat_n, is.na(TaphoNat_n),0)

# populating TaphoAccu_Ntot
TaphoAccu_Ntot[,"BoneTool"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::BoneTool`)  
TaphoAccu_Ntot[,"C_cren"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::C_cren`) 
TaphoAccu_Ntot[,"C_furr"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::C_furr`) 
TaphoAccu_Ntot[,"C_pit"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::C_pit`) 
TaphoAccu_Ntot[,"C_punct"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::C_punct`) 
TaphoAccu_Ntot[,"C_scoop"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::C_scoop`) 
TaphoAccu_Ntot[,"C_scor"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::C_scor`) 
TaphoAccu_Ntot[,"Chop"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Chop`) 
TaphoAccu_Ntot[,"P_cort"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::P_cort`) 
TaphoAccu_Ntot[,"P_flake"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::P_flake`) 
TaphoAccu_Ntot[,"P_tooth"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::P_tooth`) 
TaphoAccu_Ntot[,"P_scar"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::P_scar`) 
TaphoAccu_Ntot[,"P_mark"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::P_mark`) 
TaphoAccu_Ntot[,"P_oppo"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::P_oppo`) 
TaphoAccu_Ntot[,"P_peel"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::P_peel`) 
TaphoAccu_Ntot[,"P_spong"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::P_spong`) 
TaphoAccu_Ntot[,"Rodent"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Rodent`) 
TaphoAccu_Ntot[,"Scrape"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Scrape`) 
TaphoAccu_Ntot[,"Dig"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::CorDig`)
TaphoAccu_Ntot[,"Ctot"] <-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::C_SUM`) 
TaphoAccu_Ntot[,"Ptot"] <-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::P_SUM`) 
TaphoAccu_Ntot[,"Cut"] <-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Cut`) 
TaphoAccu_Ntot[,"Ret"] <-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Ret`) 
TaphoAccu_Ntot[,"Burnt"] <-persosum_Ntot0to4sure(dataset$`Group`, dataset$`Burnt`) 
TaphoAccu_Ntot[,"CutUnsure"] <-persosum_Ntot0to4unsure(dataset$`Group`, dataset$`TAPHO::Cut`) 
TaphoAccu_Ntot[,"RetUnsure"] <-persosum_Ntot0to4unsure(dataset$`Group`, dataset$`TAPHO::Ret`)
TaphoAccu_Ntot[,"BoneToolUnsure"] <-persosum_Ntot0to4unsure(dataset$`Group`, dataset$`TAPHO::BoneTool`) 
TaphoAccu_Ntot <- replace(TaphoAccu_Ntot, is.na(TaphoAccu_Ntot),0)

# populating TaphoNat_Ntot
TaphoNat_Ntot[,"Exfo"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Exfo`)
TaphoNat_Ntot[,"DenD"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::DenD`)
TaphoNat_Ntot[,"DenE"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::DenE`)
TaphoNat_Ntot[,"Sheet1"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Sheet`)
TaphoNat_Ntot[,"Sheet2"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Sheet`)
TaphoNat_Ntot[,"Abra1"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Abra`)
TaphoNat_Ntot[,"Abra2"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Abra`)
TaphoNat_Ntot[,"CirE"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::CirE`)
TaphoNat_Ntot[,"Crack1"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Crack`)
TaphoNat_Ntot[,"Crack2"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Crack`)
TaphoNat_Ntot[,"Conc1"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Conc`)
TaphoNat_Ntot[,"Conc2"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Conc`)
TaphoNat_Ntot[,"Conc3"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Conc`)
TaphoNat_Ntot[,"Black1"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Black`)
TaphoNat_Ntot[,"Black2"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Black`)
TaphoNat_Ntot[,"Black3"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Black`)
TaphoNat_Ntot[,"Tramp"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Tramp`)
TaphoNat_Ntot[,"TrampUnsure"] <- persosum_Ntot0to4unsure(dataset$`Group`, dataset$`TAPHO::Tramp`)
TaphoNat_Ntot[,"Cor1"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::CorDig`)
TaphoNat_Ntot[,"Cor2"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::CorDig`)
TaphoNat_Ntot[,"ReadNA"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Read`)
TaphoNat_Ntot[,"Read1"]<-persosum_n1to4(dataset$`Group`, dataset$`TAPHO::Read`)
TaphoNat_Ntot[,"Read2"]<-persosum_n1to4(dataset$`Group`, dataset$`TAPHO::Read`)
TaphoNat_Ntot[,"Read3"]<-persosum_n1to4(dataset$`Group`, dataset$`TAPHO::Read`)
TaphoNat_Ntot[,"Read4"]<-persosum_n1to4(dataset$`Group`, dataset$`TAPHO::Read`)
TaphoNat_Ntot <- replace(TaphoNat_Ntot, is.na(TaphoNat_Ntot),0)

# adding margins and populating _p
TaphoAccu_n <- addmargins(TaphoAccu_n, 1)
TaphoAccu_Ntot <- addmargins(TaphoAccu_Ntot, 1)
TaphoAccu_p <- TaphoAccu_n / TaphoAccu_Ntot
TaphoNat_n <- addmargins(TaphoNat_n, 1)
TaphoNat_Ntot <- addmargins(TaphoNat_Ntot, 1)
TaphoNat_p <- TaphoNat_n / TaphoNat_Ntot

# translating and creating data frames
dfTaphoAccu_n <- data.frame(t(TaphoAccu_n), check.rows = FALSE, check.names = FALSE)
dfTaphoAccu_Ntot <- data.frame(t(TaphoAccu_Ntot), check.rows = FALSE, check.names = FALSE)
dfTaphoAccu_p <- data.frame(t(TaphoAccu_p), check.rows = FALSE, check.names = FALSE)
dfTaphoNat_n <- data.frame(t(TaphoNat_n), check.rows = FALSE, check.names = FALSE)
dfTaphoNat_Ntot <- data.frame(t(TaphoNat_Ntot), check.rows = FALSE, check.names = FALSE)
dfTaphoNat_p <- data.frame(t(TaphoNat_p), check.rows = FALSE, check.names = FALSE)

# function for calculation of fractures
persotableFract <- function(group)
{
  library(dplyr)
  tableFract1<-table(group, dataset$`TAPHO::Fract1`, exclude = NULL)
  tableFract2<-table(group, dataset$`TAPHO::Fract2`, exclude = NULL)
  Green<-addmargins(cbind(tableFract1[,grep("Green",colnames(tableFract1))],tableFract2[,grep("Green",colnames(tableFract2))]))
  Dry<-addmargins(cbind(tableFract1[,grep("Dry",colnames(tableFract1))],tableFract2[,grep("Dry",colnames(tableFract2))]))
  Recent<-addmargins(cbind(tableFract1[,grep("Recent",colnames(tableFract1))],tableFract2[,grep("Recent",colnames(tableFract2))]))
  TOT<-cbind(Green[,"Sum"],Dry[,"Sum"],Recent[,"Sum"])
  TOT<-addmargins(TOT,2)
  TOT<-cbind(TOT,TOT[,1]/(TOT[,1]+TOT[,2]))
  colnames(TOT) <- c("Green","Dry","Recent","Sum","G/(G+D)")
  return(data.frame(TOT, check.rows = FALSE, check.names = FALSE))
}

# creating the fracture table
TableFract <- persotableFract(dataset$`Group`)

# creating the conctype table
TableConcType <- data.frame(cbind(addmargins(table(dataset$`Group`, dataset$`TAPHO::ConcType`, exclude = NA))), check.rows = FALSE, check.names = FALSE)

# creating the BoneColor table
TableBoneColor <- data.frame(cbind(addmargins(table(dataset$`Group`, dataset$`BoneColor`, exclude = NA))), check.rows = FALSE, check.names = FALSE)


# creating the final list
Tapholist <- list(TaphoAccu_n, TaphoAccu_Ntot, TaphoAccu_p, TaphoNat_n,TaphoNat_Ntot, TaphoNat_p, data.frame(dfTaphoAccu_n), data.frame(dfTaphoAccu_Ntot), data.frame(dfTaphoAccu_p), data.frame(dfTaphoNat_n), data.frame(dfTaphoNat_Ntot), data.frame(dfTaphoNat_p), data.frame(TableFract), data.frame(TableConcType), data.frame(TableBoneColor), dataset, all.names = TRUE)

names(Tapholist) <- c("TaphoAccu_n","TaphoAccu_Ntot","TaphoAccu_p", "TaphoNat_n","TaphoNat_Ntot", "TaphoNat_p","dfTaphoAccu_n","dfTaphoAccu_Ntot", "dfTaphoAccu_p","dfTaphoNat_n","dfTaphoNat_Ntot", "dfTaphoNat_p", "TableFract", "TableConcType", "TableBoneColor", "raw_dataset")

return(Tapholist)


}
# Functions for Tapho plots

plot_TaphoAccu <- function(var, varname, groupselect)
{
  require(DescTools)
  require(ggplot2)
  dataforplot<- cbind(TaphoAccu_Ntot[,var], BinomCI(TaphoAccu_n[,var],
                                                    TaphoAccu_Ntot[,var],
                                                    conf.level = 0.95,
                                                    method = "wilson"))
  colnames(dataforplot) <- c("N", "p","low","up")
  data = data.frame(Group = row.names(dataforplot), dataforplot)
  data = data[groupselect,]
  print(data)
  plot <- ggplot(data, aes(x=Group, y=p, ymin=0, ymax=1)) + 
    geom_errorbar(aes(ymin=low, ymax=up),
                  width=.15,                    # Width of the error helpbars
                  position=position_dodge(.9)) +
    geom_point(shape=21, fill="white", size =1.5, stroke=0.8, stat="identity") +
    xlab(NULL) +
    ylab(NULL) +
    scale_y_continuous(labels = scales::percent) +
    ggtitle(varname) +
    theme(plot.title = element_text(size=10))
  return(plot)
}

plot_TaphoNat <- function(var, varname, groupselect)
{
  require(DescTools)
  require(ggplot2)
  dataforplot<- cbind(TaphoNat_Ntot[,var], BinomCI(TaphoNat_n[,var],
                                                   TaphoNat_Ntot[,var],
                                                   conf.level = 0.95,
                                                   method = "wilson"))
  colnames(dataforplot) <- c("N", "p","low","up")
  data = data.frame(Group = row.names(dataforplot), dataforplot)
  data <- data[groupselect,]
  print(data)
  plot <- ggplot(data, aes(x=Group, y=p, ymin=0, ymax=1)) + 
    geom_errorbar(aes(ymin=low, ymax=up),
                  width=.15,                    # Width of the error helpbars
                  position=position_dodge(.9)) +
    geom_point(shape=21, fill="white", size =1.5, stroke=0.8, stat="identity") +
    xlab(NULL) +
    ylab(NULL) +
    scale_y_continuous(labels = scales::percent) +
    ggtitle(varname) +
    theme(plot.title = element_text(size=10))
  return(plot)
}

bar_TaphoNat <- function(varname, var1, var2, var3, var4, groupselect)
{
  require(reshape2)
  require(ggplot2)
  dataforplot <- TaphoNat_p[,c(var1, var2, var3, var4)]
  dataforplot = dataforplot[groupselect,]
  print(dataforplot)
  dataforplot = data.frame(Group = row.names(dataforplot), dataforplot)
  dataforplot <- melt(dataforplot,id.vars = "Group")
  plot <- ggplot(dataforplot, aes(x=Group, y=value,ymin=0, ymax=1, fill=variable)) +
    geom_bar(stat="identity") + 
    geom_col(position = position_stack(reverse = TRUE)) +
    xlab(NULL) +
    ylab(NULL) +
    scale_y_continuous(labels = scales::percent) +
    ggtitle(varname) +
    theme(plot.title = element_text(size=10)) 
  return(plot)
}

bar_createPercentage <- function(varname, data, groupselect)
{
  require(reshape2)
  require(ggplot2)
  dataforplot <- data[,-which(names(data) == "Sum")]
  dataforplot = dataforplot[groupselect,]
  print(dataforplot)
  dataforplot <- dataforplot/rowSums(dataforplot)
  print(dataforplot)
  dataforplot = data.frame(Group = row.names(dataforplot), dataforplot)
  dataforplot <- melt(dataforplot,id.vars = "Group")
  plot <- ggplot(dataforplot, aes(x=Group, y=value,ymin=0, ymax=1, fill=variable)) +
    geom_bar(stat="identity") + 
    geom_col(position = position_stack(reverse = TRUE)) +
    xlab(NULL) +
    ylab(NULL) +
    scale_y_continuous(labels = scales::percent) +
    ggtitle(varname) +
    theme(plot.title = element_text(size=10)) 
  return(plot)
}


# Functions used in NDE script
###################dataprep_NDE
dataprep_NDE <- function(file, group, species.codes, exclude.foetus)
{
# import Excel file (check the Excel file name)
require(readxl)
dataset <- read_excel(file,col_types = c("numeric","guess","numeric",rep("guess",4),rep("text",4),rep("numeric",117),rep("text",2)))

## choose the variable used for grouping remains together
names(dataset)[names(dataset) == group] <- 'Group'

# by default, this script removes foetus bones from the skeletal-part analysis
if(exclude.foetus == "TRUE"){
dataset$`SKEL::AgeCort` <-   replace(dataset$`SKEL::AgeCort`, is.na(dataset$`SKEL::AgeCort`),"?") # replacing NAs in AgeCort, to avoid errors
tmpFoetus <- dataset$`SKEL::AgeCort`=="F"
dataset <- dataset[!tmpFoetus,]
rm(tmpFoetus)
}

# select the correct species for NDEcodes
NDEcodes<-read.csv(species.codes, header=TRUE, sep=";", encoding="UTF-8") #number of bones per individual for RANG


# reordering dataset
datatmp <- replace(dataset[,12:128], is.na(dataset[,12:128]),0) #replacing empty NDE by a value of 0
datatmp2 <- replace(dataset$Group, is.na(dataset$Group),"NA Group") # replacing NAs in Group, to avoid errors
datatmp3 <- cbind(datatmp2,dataset$`SKEL::Anat`,dataset$`SKEL::Anat_Detail`,datatmp)
dataOK <- data.frame(datatmp3)
colnames(dataOK)[1] <- 'Group'
colnames(dataOK)[2] <- 'Anat'
colnames(dataOK)[3] <- 'Anat_Detail'
rm(datatmp)
rm(datatmp2)
rm(datatmp3)
SumGroup <- table(dataOK$Group, exclude = NULL)

# creating a pivot table by group and landmark code, for the 0/1/2 landmarks (landmarks 17,30,88,89,90 and 91 excluded)
tmp <- rep(0,nrow(SumGroup))
for(i in 10:120) {
  tmpPivot<-addmargins(table(dataOK$Group, dataOK[,i], exclude = NULL),2)
  tmpCol <- data.frame(tmpPivot[,"Sum"]-tmpPivot[,"0"])
  colnames(tmpCol)<-colnames(dataOK)[i]
  tmp<-cbind(tmp,tmpCol)
}
NDEData_1<-data.frame(t(tmp[,-1]),check.names = FALSE,check.rows = FALSE)
rm(tmpPivot)
rm(tmpCol)
rm(i)
rm(tmp)

# total landmarks measurements calculations
tmpMes17 <- rowsum(dataOK$Ldmk_17, dataOK$Group)
tmpMes30 <- rowsum(dataOK$Ldmk_30, dataOK$Group)
tmpMes88 <- rowsum(dataOK$Ldmk_88mesRib, dataOK$Group)
tmpMes89 <- rowsum(dataOK$Ldmk_89mesRad, dataOK$Group)
tmpMes90 <- rowsum(dataOK$Ldmk_90mesMan, dataOK$Group)
tmpMes91 <- rowsum(dataOK$Ldmk_91mesFem, dataOK$Group)
tmpMes <- cbind(tmpMes17,tmpMes30,tmpMes88,tmpMes89,tmpMes90,tmpMes91)
colnames(tmpMes) <- c("Ldmk_17","Ldmk_30","Ldmk_88","Ldmk_89","Ldmk_90", "Ldmk_91")
NDEData_2<-data.frame(t(tmpMes),check.names = FALSE,check.rows = FALSE)
rm(tmpMes17)
rm(tmpMes30)
rm(tmpMes88)
rm(tmpMes89)
rm(tmpMes90)
rm(tmpMes91)
rm(tmpMes)

# adapt the script for cases where there is only one group
if(nrow(SumGroup) == "1"){
  colnames(NDEData_1) <-colnames(NDEData_2)
}

# final NDEtable (counts and measurements) - first step
NDEData <- rbind(NDEData_1, NDEData_2) #merging 0,1,2 data and measurements
rm(NDEData_1)
rm(NDEData_2)
NDEData$AllGroup <- rowSums(NDEData) #calculating total NDE for all groups combined

# calculation of MAU by NDE
tmpMAU <- NDEData / NDEcodes$Quantity
MAUfromNDE <- cbind(NDEcodes[,2:5],tmpMAU)
MNIfromNDE <- cbind(NDEcodes[,2:5],ceiling (tmpMAU))
rm(tmpMAU)
MAUfromNDE <- subset(MAUfromNDE, Quantity!="NA") #removing NDE landmarks that are not present in this species
MNIfromNDE <- subset(MNIfromNDE, Quantity!="NA") #removing NDE landmarks that are not present in this species

# final NDEtable - second step
NDEData <- cbind(NDEcodes[,2:5],NDEData) #merging NDEcodes and NDEdata
NDEData <- subset(NDEData, Quantity!="NA") #removing NDE landmarks that are not present in this species

# maxMAU by Element
MAUbyElement<-aggregate(MAUfromNDE[5:(nrow(SumGroup)+5)], by = list(MAUfromNDE$Element), max)
rownames(MAUbyElement)<-MAUbyElement[,1]
MAUbyElement<-MAUbyElement[,-1]

# maxMAU by Element and Portion
MAUbyElementPortion<-aggregate(MAUfromNDE[5:(nrow(SumGroup)+5)], by = list(MAUfromNDE$ElementPortion), max)
rownames(MAUbyElementPortion)<-MAUbyElementPortion[,1]
MAUbyElementPortion<-MAUbyElementPortion[,-1]

# calculating %MAUs
tmpMaxMAUbyElement <- apply(MAUbyElement, 2, max, na.rm = TRUE)
pMAUbyElement <- data.frame(t(t(MAUbyElement)/tmpMaxMAUbyElement))
tmpMaxMAUbyElementPortion <- apply(MAUbyElementPortion, 2, max, na.rm = TRUE)
pMAUbyElementPortion <- data.frame(t(t(MAUbyElementPortion)/tmpMaxMAUbyElementPortion))
rm(tmpMaxMAUbyElement)
rm(tmpMaxMAUbyElementPortion)


NDElist <- list(data.frame(NDEData), data.frame(MAUfromNDE), data.frame(MNIfromNDE), data.frame(MAUbyElement), data.frame(MAUbyElementPortion), data.frame(pMAUbyElement), data.frame(pMAUbyElementPortion), all.names = TRUE)
names(NDElist) <- c("NDEData","MAUfromNDE", "MNIfromNDE", "MAUbyElement", "MAUbyElementPortion", "pMAUbyElement","pMAUbyElementPortion")

return(NDElist)

}


dataprep_Cut <- function(file, group, cut.codes)
{

# import Excel file (check the Excel file name)
dataset <- read_excel(file, col_types = c("numeric","guess","numeric",rep("text",8),rep("numeric",411)))


## choose the variable used for grouping remains together
names(dataset)[names(dataset) == group] <- 'Group'

# selecting the codes activities in French (CutCodesFR.csv) or English (CutCodesEN.csv)
CutCodes<-read.csv(cut.codes, header=TRUE, sep=";", row.names=1, encoding="UTF-8")

# reordering dataset
datatmp <- replace(dataset[,13:422], is.na(dataset[,13:422]),0) #replacing NA by 0 in the recorded cut-marks
datatmpGroup <- replace(dataset[,"Group"], is.na(dataset[,"Group"]),"Unknown") #replacing NA by Unknown for the Group field
SumGroup <- table(datatmpGroup, exclude = NULL)
datatmp2<-cbind(datatmpGroup,datatmp)
dataOK<-data.frame(datatmp2)
rm(datatmp)
rm(datatmp2)

# creating a pivot table by group and cut code, only if coded at least once
tmp <- rep(0,3+nrow(SumGroup))
for(i in 2:411) {
  tmpPivot<-table(dataOK[,"Group"], dataOK[,i], exclude = NULL)
  if("1" %in% colnames(tmpPivot)){
    tmpCol<-data.frame(tmpPivot[,"1"])
    colnames(tmpCol)<-colnames(dataOK)[i]
    tmpCodes<-t(CutCodes[colnames(tmpCol),1:3])
    tmpCodesCol<-rbind(tmpCodes, tmpCol,stringsAsFactors = FALSE)
    tmp<-cbind(tmp,tmpCodesCol)
  }
}

rm(tmpPivot)
rm(tmpCol)
rm(tmpCodes)
rm(tmpCodesCol)
rm(i)

tmp <- data.frame(t(tmp[,-1]),stringsAsFactors = FALSE)
if(nrow(SumGroup) > 1){ ##necessary to avoid bugs if there is only one group
tmp$AllGroup <- rowSums(sapply(tmp[,-c(1:3)], as.numeric)) #summing rows to get the AllGroup column
}
CutData<-data.frame(tmp,check.names = FALSE,check.rows = FALSE)
rm(tmp)

# creating a pivot table for the proportion of longidutinal cut-marks on shafts
tmpDataSH <- dataOK[,c("Group","Fs_a","Fs_ap", "Hs_a","Hs_ap","Rs_a","Rs_ap","Rs_b","Rs_bp","Rs_bs","Rs_c","Rs_cp","Rs_cs","Ts_a","Ts_ap","Ts_b","Ts_bp","Ts_c","Ts_cp","Ts_d","Ts_dp","Ts_ds","Ts_e","Ts_ep","Ts_es")]

tmp <- rep(0,nrow(SumGroup))
for(i in 2:25) {
  tmpPivot<-table(tmpDataSH[,"Group"], tmpDataSH[,i], exclude = NULL)
  tmpCol <- data.frame(rep(0,nrow(SumGroup)))
  colnames(tmpCol)<-colnames(tmpDataSH)[i]
  if("1" %in% colnames(tmpPivot)){
    tmpCol<-data.frame(tmpPivot[,"1"])
    colnames(tmpCol)<-colnames(tmpDataSH)[i]
  }
  tmp<-cbind(tmp,tmpCol)
}

tmp$nHumSH_long <- tmp[,5]
tmp$nHumSH_all <- rowSums(tmp[,4:5])
tmp$nFemSH_long <- tmp[,3]
tmp$nFemSH_all <- rowSums(tmp[,2:3])
tmp$nRadSH_long <- rowSums(tmp[,c(7,9,12)])
tmp$nRadSH_all <- rowSums(tmp[,6:13])
tmp$nTibSH_long <- rowSums(tmp[,c(15,17,19,21,24)])
tmp$nTibSH_all <- rowSums(tmp[,14:25])
tmp$nAllSH_long <- rowSums(tmp[,c(26,28,30,32)])
tmp$nAllSH_all <- rowSums(tmp[,c(27,29,31,33)])
tmp <- rbind(tmp, "AllGroup" = colSums(tmp))
tmp$pHumSH_long <- tmp$nHumSH_long / tmp$nHumSH_all
tmp$pFemSH_long <- tmp$nFemSH_long / tmp$nFemSH_all
tmp$pRadSH_long <- tmp$nRadSH_long / tmp$nRadSH_all
tmp$pTibSH_long <- tmp$nTibSH_long / tmp$nTibSH_all
tmp$pAllSH_long <- tmp$nAllSH_long / tmp$nAllSH_all


CutDataLong <- data.frame(t(tmp[,26:40]),check.names = FALSE,check.rows = FALSE)

Cutlist <- list(data.frame(CutData), data.frame(CutDataLong), all.names = TRUE)
names(Cutlist) <- c("CutData","CutDataLong")

return(Cutlist)

}


###################skelanalysis
bonedensity <- function(file,ref)
{
BoneDensityRef <- read.csv(ref, header=TRUE, sep=";", dec=",",encoding="UTF-8")
# merging ref data and %MAU for bone density
tmp_pMAU <- file
tmp_pMAU$ElementPortion <- rownames(file)
BoneDensityData <- merge.data.frame(BoneDensityRef, tmp_pMAU, by.x = "ElementPortion")
return(BoneDensityData)
}

marrowUMI <- function(file,ref)
{
  MarrowUMIRef <- read.csv(ref, header=TRUE, sep=";", dec=",",encoding="UTF-8")
  # merging ref data and %MAU for marrow indices
  tmp_pMAU <- file
  tmp_pMAU$Element <- rownames(file)
  MarrowUMIData <- merge.data.frame(MarrowUMIRef, tmp_pMAU, by.x = "Element")
  return(MarrowUMIData)
}

marrowCav <- function(file,ref)
{
  MarrowCavRef <- read.csv(ref, header=TRUE, sep=";", dec=",",encoding="UTF-8")   
  # merging ref data and %MAU for marrow indices
  tmp_pMAU <- file
  tmp_pMAU$Element <- rownames(file)
  MarrowCavData <- merge.data.frame(MarrowCavRef, tmp_pMAU, by.x = "Element")
  return(MarrowCavData)
}

mergeindex <- function(file,ref)
{
  tableref <- read.csv(ref, header=TRUE, sep=";", dec=",",encoding="UTF-8")   
  # merging ref data and %MAU for marrow indices
  tmp_pMAU <- file
  tmp_pMAU$Element <- rownames(file)
  data <- merge.data.frame(tableref, tmp_pMAU, by.x = "Element")
  return(data)
}







