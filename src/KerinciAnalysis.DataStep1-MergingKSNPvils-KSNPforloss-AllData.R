###Data Step 1 for KSNP Tree-Cover Analysis###

###   Note: Original file saved to C:\Users\JTErbaugh\Dropbox\#michigan phd\1.dissertation\RWorkingDirectory\KerinciAnalysis.DataStep1-MergingKSNPvils-KSNPforloss-AllData.R
###         This file has additional code, including a "junkyard" and portions of the MDVPI analysis not included here
###

install.packages("dplyr")
install.packages("sandwich")
install.packages("vcov")
install.packages("ggpubr")
install.packages("tidyverse")
install.packages("haven")
install.packages("car")
install.packages("MatchIt")
install.packages("plotly")


library(dplyr)
library(sandwich)
library(vcov)
library(ggpubr)
library(tidyverse)
library(haven)
library(car)
library(MatchIt)
library(plotly)


###Spatial and PODES data for 2003 villages
#spatpod03<-read.csv("E:/Indonesian_Spatial_Data/VillageLandcoverTables/2003abc/2003dCSV/xwalk_wide_merged2003d_lags.csv")
spatpod03<-read.csv("C:/Users/JTErbaugh/Dropbox/Library/1.Data/Dissertation_Data/Spatial_Datatables/xwalk_wide_merged2003d_lags.csv")
spatpod_fullset<-read.csv("C:/Users/JTErbaugh/Dropbox/Library/1.Data/Dissertation_Data/Spatial_Datatables/xwalk_wide_mergedalld2_lags2.csv")
#rds0003c<-read.csv("E:/Indonesian_Spatial_Data/VillageLandcoverTables/2003d/2003dCSV/rds00_03d.txt")
rds1603d<-read.csv("E:/Indonesian_Spatial_Data/VillageLandcoverTables/2003d/2003dCSV/rds1603d.txt")
kspop00<-read.csv("E:/Indonesian_Spatial_Data/Sampling_Maps/kspop00.csv")
kspop05<-read.csv("E:/Indonesian_Spatial_Data/Sampling_Maps/kspop05.csv")
kspop10<-read.csv("E:/Indonesian_Spatial_Data/Sampling_Maps/kspop10.csv")
kspop15<-read.csv("E:/Indonesian_Spatial_Data/Sampling_Maps/kspop15.csv")
kspop20<-read.csv("E:/Indonesian_Spatial_Data/Sampling_Maps/kspop20.csv")

###All Spatial and PODES data for villages (2000-2014)
xwalk<-read.csv("C:/Users/jerbaugh/Dropbox/Library/1.Data/Dissertation_Data/Spatial_Datatables/xwalk_wide_mergedalld2_lags2.csv")

ksnpvils<-read.csv("E:/Indonesian_Spatial_Data/Sampling_Maps/KSNPVils03d.txt")
ksnpvca<-read.csv("C:/Users/JTErbaugh/Dropbox/#michigan phd/1.Dissertation/Ch5-Kerinci/KSNP-VilNames-ICDP_HKD.csv")

#knspforloss<-read.csv("E:/Indonesian_Spatial_Data/Sampling_Maps/ksnplos0316.txt")
#knspforloss_0003<-read.csv("E:/Indonesian_Spatial_Data/Sampling_Maps/ksnplos0003.txt")
#knspprmforloss<-read.csv("E:/Indonesian_Spatial_Data/Sampling_Maps/ksnpprmlos2.txt")
#knspsecforloss<-read.csv("E:/Indonesian_Spatial_Data/Sampling_Maps/ksnpseclos2.txt")

knspyearforloss1<-read.csv("E:/Indonesian_Spatial_Data/Sampling_Maps/YearForLossinKSNP.csv")
knspyearforloss2<-read.csv("E:/Indonesian_Spatial_Data/Sampling_Maps/KSNPforlos_tnks.csv")
knspyearforloss3<-read.csv("E:/Indonesian_Spatial_Data/Sampling_Maps/KSNPforlos_all.csv")

ksnpbuffer3<-read.csv("E:/Indonesian_Spatial_Data/Sampling_Maps/ksnp_3kmlc.txt")

ksnpconc<-read.csv("E:/Indonesian_Spatial_Data/Sampling_Maps/ks03_conc.csv")

###################################################
###DATA STEP 1: CREATING YEARLY FOREST LOSS DATA###
###################################################

#################################
###Year of forest loss table 1###
#################################
##Removing non-villages (lakes and forests)
ksnpvils<-ksnpvils[ksnpvils$ID2003_d!=1501091888 & ksnpvils$ID2003_d!=1703999999,]

##Merging Year of forest loss with village info from 2003##
yrforloss<-merge(ksnpvils,knspyearforloss1, by.x="ID2003_d", by.y="ID2003_D", all.x=T)

rm(knspyearforloss1)

#Renaming variables
yrforloss$AreaForLoss_2001<-yrforloss$VALUE_1
yrforloss$AreaForLoss_2002<-yrforloss$VALUE_2
yrforloss$AreaForLoss_2003<-yrforloss$VALUE_3
yrforloss$AreaForLoss_2004<-yrforloss$VALUE_4
yrforloss$AreaForLoss_2005<-yrforloss$VALUE_5
yrforloss$AreaForLoss_2006<-yrforloss$VALUE_6
yrforloss$AreaForLoss_2007<-yrforloss$VALUE_7
yrforloss$AreaForLoss_2008<-yrforloss$VALUE_8
yrforloss$AreaForLoss_2009<-yrforloss$VALUE_9
yrforloss$AreaForLoss_2010<-yrforloss$VALUE_10
yrforloss$AreaForLoss_2011<-yrforloss$VALUE_11
yrforloss$AreaForLoss_2012<-yrforloss$VALUE_12
yrforloss$AreaForLoss_2013<-yrforloss$VALUE_13
yrforloss$AreaForLoss_2014<-yrforloss$VALUE_14
yrforloss$AreaForLoss_2015<-yrforloss$VALUE_15
yrforloss$AreaForLoss_2016<-yrforloss$VALUE_16
#Looking at sums for different variable years
sum(yrforloss$AreaForLoss_2001, na.rm=T)
sum(yrforloss$AreaForLoss_2002, na.rm=T)
sum(yrforloss$AreaForLoss_2003, na.rm=T)
sum(yrforloss$AreaForLoss_2004, na.rm=T)
sum(yrforloss$AreaForLoss_2005, na.rm=T)
sum(yrforloss$AreaForLoss_2006, na.rm=T)
sum(yrforloss$AreaForLoss_2007, na.rm=T)
sum(yrforloss$AreaForLoss_2008, na.rm=T)
sum(yrforloss$AreaForLoss_2009, na.rm=T)
sum(yrforloss$AreaForLoss_2010, na.rm=T)
sum(yrforloss$AreaForLoss_2011, na.rm=T)
sum(yrforloss$AreaForLoss_2012, na.rm=T)
sum(yrforloss$AreaForLoss_2013, na.rm=T)
sum(yrforloss$AreaForLoss_2014, na.rm=T)
sum(yrforloss$AreaForLoss_2015, na.rm=T)
sum(yrforloss$AreaForLoss_2016, na.rm=T)

#View(yrforloss)

#################################
###Year of forest loss table 2###
#################################

###Transposing dataframe###

knspyearforloss2<-(as.data.frame(knspyearforloss2) %>%
       rownames_to_column %>% 
       gather(var, value, -rowname) %>% 
       spread(rowname, value))
#View(knspyearforloss2)

###Renaming rowid and truncating to ID2003_d info

knspyearforloss2$ID2003_d<-substr(knspyearforloss2$var,7,17)
knspyearforloss2$var<-NULL

###Renaming ForestLoss Variables###

knspyearforloss2$PxlKSNPLoss_2001<-knspyearforloss2$'1'
knspyearforloss2$PxlKSNPLoss_2002<-knspyearforloss2$'2'
knspyearforloss2$PxlKSNPLoss_2003<-knspyearforloss2$'3'
knspyearforloss2$PxlKSNPLoss_2004<-knspyearforloss2$'4'
knspyearforloss2$PxlKSNPLoss_2005<-knspyearforloss2$'5'
knspyearforloss2$PxlKSNPLoss_2006<-knspyearforloss2$'6'
knspyearforloss2$PxlKSNPLoss_2007<-knspyearforloss2$'7'
knspyearforloss2$PxlKSNPLoss_2008<-knspyearforloss2$'8'
knspyearforloss2$PxlKSNPLoss_2009<-knspyearforloss2$'9'
knspyearforloss2$PxlKSNPLoss_2010<-knspyearforloss2$'10'
knspyearforloss2$PxlKSNPLoss_2011<-knspyearforloss2$'11'
knspyearforloss2$PxlKSNPLoss_2012<-knspyearforloss2$'12'
knspyearforloss2$PxlKSNPLoss_2013<-knspyearforloss2$'13'
knspyearforloss2$PxlKSNPLoss_2014<-knspyearforloss2$'14'
knspyearforloss2$PxlKSNPLoss_2015<-knspyearforloss2$'15'
knspyearforloss2$PxlKSNPLoss_2016<-knspyearforloss2$'16'

#View(knspyearforloss2)

#################################
###Year of forest loss table 3###
#################################

###Transposing dataframe###

knspyearforloss3<-(as.data.frame(knspyearforloss3) %>%
                     rownames_to_column %>% 
                     gather(var, value, -rowname) %>% 
                     spread(rowname, value))
#View(knspyearforloss3)

###Renaming rowid and truncating to ID2003_d info

knspyearforloss3$ID2003_d<-substr(knspyearforloss3$var,7,17)
knspyearforloss3$var<-NULL

###Renaming ForestLoss Variables###

knspyearforloss3$PxlAllLoss_2001<-knspyearforloss3$'2'
knspyearforloss3$PxlAllLoss_2002<-knspyearforloss3$'3'
knspyearforloss3$PxlAllLoss_2003<-knspyearforloss3$'4'
knspyearforloss3$PxlAllLoss_2004<-knspyearforloss3$'5'
knspyearforloss3$PxlAllLoss_2005<-knspyearforloss3$'6'
knspyearforloss3$PxlAllLoss_2006<-knspyearforloss3$'7'
knspyearforloss3$PxlAllLoss_2007<-knspyearforloss3$'8'
knspyearforloss3$PxlAllLoss_2008<-knspyearforloss3$'9'
knspyearforloss3$PxlAllLoss_2009<-knspyearforloss3$'10'
knspyearforloss3$PxlAllLoss_2010<-knspyearforloss3$'11'
knspyearforloss3$PxlAllLoss_2011<-knspyearforloss3$'12'
knspyearforloss3$PxlAllLoss_2012<-knspyearforloss3$'13'
knspyearforloss3$PxlAllLoss_2013<-knspyearforloss3$'14'
knspyearforloss3$PxlAllLoss_2014<-knspyearforloss3$'15'
knspyearforloss3$PxlAllLoss_2015<-knspyearforloss3$'16'
knspyearforloss3$PxlAllLoss_2016<-knspyearforloss3$'17'


#View(knspyearforloss3)

###Merging all year loss datasets to yrloss, which contains village info and cros tabbed area loss info###
vilsforloss_merge1<-merge(yrforloss, knspyearforloss2, by="ID2003_d", all.x=T)
vilsforloss<-merge(vilsforloss_merge1, knspyearforloss3, by="ID2003_d", all.x=T)

rm(yrforloss, knspyearforloss2, vilsforloss_merge1, knspyearforloss3)

###Merge ForestLoss and VCA data###
vilsforloss_vca<-merge(vilsforloss, ksnpvca, by="ID2003_d", all.x=T)
rm(vilsforloss)

yrlossvarskeep<-c("ID2003_d", "Area2003", "VCA", "HKD", "HKD_Sisa", "HKDPer_2", "HKDII",
                  "HKDII_tdklnjt", "HKDII_tdkcair", "YearI", "YearII",  "YearIII",          
                  "YearIV", "Combined.Boundary", "AreaForLoss_2001", "AreaForLoss_2002",
                  "AreaForLoss_2003", "AreaForLoss_2004", "AreaForLoss_2005", "AreaForLoss_2006", 
                  "AreaForLoss_2007", "AreaForLoss_2008", "AreaForLoss_2009", "AreaForLoss_2010",
                  "AreaForLoss_2011", "AreaForLoss_2012", "AreaForLoss_2013", "AreaForLoss_2014",
                  "AreaForLoss_2015", "AreaForLoss_2016", "PxlKSNPLoss_2001", "PxlKSNPLoss_2002",
                  "PxlKSNPLoss_2003", "PxlKSNPLoss_2004", "PxlKSNPLoss_2005", "PxlKSNPLoss_2006", 
                  "PxlKSNPLoss_2007", "PxlKSNPLoss_2008", "PxlKSNPLoss_2009", "PxlKSNPLoss_2010",
                  "PxlKSNPLoss_2011", "PxlKSNPLoss_2012", "PxlKSNPLoss_2013", "PxlKSNPLoss_2014",
                  "PxlKSNPLoss_2015", "PxlKSNPLoss_2016", "PxlAllLoss_2001", "PxlAllLoss_2002",
                  "PxlAllLoss_2003", "PxlAllLoss_2004", "PxlAllLoss_2005", "PxlAllLoss_2006", 
                  "PxlAllLoss_2007", "PxlAllLoss_2008", "PxlAllLoss_2009", "PxlAllLoss_2010",
                  "PxlAllLoss_2011", "PxlAllLoss_2012", "PxlAllLoss_2013", "PxlAllLoss_2014",
                  "PxlAllLoss_2015", "PxlAllLoss_2016")

yrforloss2<-vilsforloss_vca[yrlossvarskeep]
setdiff(yrlossvarskeep, names(vilsforloss_vca))
#View(yrforloss2)


##Transforming to long form dataset##

yrforloss_KSNP<-reshape(yrforloss2, direction="long", 
                          idvar='ID2003_d',
                          varying=c(15:62),
                          sep='_',
                          timevar="year",
                          times=c('2001','2002','2003','2004','2005','2006', '2007', '2008', '2009', '2010', '2011',
                                  '2012', '2013', '2014', '2015', '2016'))

yrforloss_KSNP<-yrforloss_KSNP %>%
  arrange(ID2003_d)

###After identifying villages with missing forest cover change in KSNP (12), and determining that for all years
###they had no forest cover loss pixels for KSNP in their boundary, I am replacing their "NAs" with "0s"

yrforloss_KSNP$PxlKSNPLoss[yrforloss_KSNP$ID2003_d==1302080003 | yrforloss_KSNP$ID2003_d==1302100002 |
                             yrforloss_KSNP$ID2003_d==1501080018 | yrforloss_KSNP$ID2003_d==1501090031 |
                             yrforloss_KSNP$ID2003_d==1502010002 | yrforloss_KSNP$ID2003_d==1502010010 | 
                             yrforloss_KSNP$ID2003_d==1503010007 | yrforloss_KSNP$ID2003_d==1503020001 |
                             yrforloss_KSNP$ID2003_d==1503020004 | yrforloss_KSNP$ID2003_d==1702030043 |
                           yrforloss_KSNP$ID2003_d== 1702050022| yrforloss_KSNP$ID2003_d==1703131013]<-0

### Converting different measurements to Hectares ###
yrforloss_KSNP$KSNP_AreaForLoss_Ha1<-1363729*yrforloss_KSNP$AreaForLoss
sum(yrforloss_KSNP$KSNP_AreaForLoss_Ha1, na.rm=T)
summary(yrforloss_KSNP$KSNP_AreaForLoss_Ha1)
#Note:1363729 is the hectare measurement of KSNP
#This measurement contains 192 missing values, likely because extremely small values rounded down to 0

yrforloss_KSNP$KSNP_AreaForLoss_Ha2<-((yrforloss_KSNP$PxlKSNPLoss*900)/10000)
sum(yrforloss_KSNP$KSNP_AreaForLoss_Ha2, na.rm=T)

yrforloss_KSNP$All_AreaForLoss_Ha<-((yrforloss_KSNP$PxlAllLoss*900)/10000)
sum(yrforloss_KSNP$All_AreaForLoss_Ha, na.rm=T)

#View(yrforloss_KSNP)

###Creating outside forest area loss variable###

yrforloss_KSNP$nonKSNP_AreaForLoss_Ha<-yrforloss_KSNP$All_AreaForLoss_Ha-yrforloss_KSNP$KSNP_AreaForLoss_Ha2
sum(yrforloss_KSNP$nonKSNP_AreaForLoss_Ha, na.rm=T)

###Creating year of treatment variable###

yrforloss_KSNP$year_vca<-0
yrforloss_KSNP$year_vca[yrforloss_KSNP$year>2003]<-1

##Creating HKDI Only Variable##
yrforloss_KSNP$HKDI<-0
yrforloss_KSNP$HKDI[yrforloss_KSNP$VCA==1 & yrforloss_KSNP$HKDII==0]<-1
sum(yrforloss_KSNP$HKDI)
#29 are HKDII and 30 are HKDI only.

###DiD Variables###

yrforloss_KSNP$did1<-yrforloss_KSNP$year_vca*yrforloss_KSNP$VCA
yrforloss_KSNP$did2<-yrforloss_KSNP$year_vca*yrforloss_KSNP$HKD
yrforloss_KSNP$did3<-yrforloss_KSNP$year_vca*yrforloss_KSNP$HKDI
yrforloss_KSNP$did4<-yrforloss_KSNP$year_vca*yrforloss_KSNP$HKDII
yrforloss_KSNP$did5<-yrforloss_KSNP$year_vca*yrforloss_KSNP$HKDI*yrforloss_KSNP$HKD
yrforloss_KSNP$did6<-yrforloss_KSNP$year_vca*yrforloss_KSNP$HKDII*yrforloss_KSNP$HKD

###Writing dataset###

write.csv(yrforloss_KSNP,"C:/Users/JTErbaugh/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/Data/yrforloss_KSNP.csv")

################################################
###DATA STEP 2: CREATING DATASET FOR MATCHING###
################################################

###1. Subsetting spatpod dataset based on KSNP boundary villages

spatialksnp<-merge(ksnpvils, spatpod_fullset, by.x="ID2003_d", by.y="iid.2003", all.x=T)
##Merging roads 16 distance data bc it wasn't there before
spatialksnp<-merge(spatialksnp, rds1603d, by.x ="ID2003_d", by.y="ID2003_D", all.x=T)
##Renaming the variable for mean distance to roads
spatialksnp$dstrds16_MEAN.2003<-spatialksnp$MEAN

rm(ksnpvils, spatpod_fullset)

names(spatialksnp)

###Retaining only specific variables###
spatpod.ksnp_mtchvars<-c("ID2003_d", "Area2003", "elev_MEAN.2003", "ovslp_MEAN.2003", 
                         "dstrds00_MEAN.2003", "dstrds16_MEAN.2003", "pop.2003", "mdvp.2003", "PAs_MEAN.2003", 
                         "prmforlag_MEAN.2003")

spatpod.ksnp_mtch<-spatialksnp[spatpod.ksnp_mtchvars]

###Creating road change data###
spatpod.ksnp_mtch$dstrds.2001<-(15/16)*spatpod.ksnp_mtch$dstrds00_MEAN.2003+(1/16)*spatpod.ksnp_mtch$dstrds16_MEAN.2003
spatpod.ksnp_mtch$dstrds.2002<-(14/16)*spatpod.ksnp_mtch$dstrds00_MEAN.2003+(2/16)*spatpod.ksnp_mtch$dstrds16_MEAN.2003
spatpod.ksnp_mtch$dstrds.2003<-(13/16)*spatpod.ksnp_mtch$dstrds00_MEAN.2003+(3/16)*spatpod.ksnp_mtch$dstrds16_MEAN.2003
spatpod.ksnp_mtch$dstrds.2004<-(12/16)*spatpod.ksnp_mtch$dstrds00_MEAN.2003+(4/16)*spatpod.ksnp_mtch$dstrds16_MEAN.2003
spatpod.ksnp_mtch$dstrds.2005<-(11/16)*spatpod.ksnp_mtch$dstrds00_MEAN.2003+(5/16)*spatpod.ksnp_mtch$dstrds16_MEAN.2003
spatpod.ksnp_mtch$dstrds.2006<-(10/16)*spatpod.ksnp_mtch$dstrds00_MEAN.2003+(6/16)*spatpod.ksnp_mtch$dstrds16_MEAN.2003
spatpod.ksnp_mtch$dstrds.2007<-(9/16)*spatpod.ksnp_mtch$dstrds00_MEAN.2003+(7/16)*spatpod.ksnp_mtch$dstrds16_MEAN.2003
spatpod.ksnp_mtch$dstrds.2008<-(8/16)*spatpod.ksnp_mtch$dstrds00_MEAN.2003+(8/16)*spatpod.ksnp_mtch$dstrds16_MEAN.2003
spatpod.ksnp_mtch$dstrds.2009<-(7/16)*spatpod.ksnp_mtch$dstrds00_MEAN.2003+(9/16)*spatpod.ksnp_mtch$dstrds16_MEAN.2003
spatpod.ksnp_mtch$dstrds.2010<-(6/16)*spatpod.ksnp_mtch$dstrds00_MEAN.2003+(10/16)*spatpod.ksnp_mtch$dstrds16_MEAN.2003
spatpod.ksnp_mtch$dstrds.2011<-(5/16)*spatpod.ksnp_mtch$dstrds00_MEAN.2003+(11/16)*spatpod.ksnp_mtch$dstrds16_MEAN.2003
spatpod.ksnp_mtch$dstrds.2012<-(4/16)*spatpod.ksnp_mtch$dstrds00_MEAN.2003+(12/16)*spatpod.ksnp_mtch$dstrds16_MEAN.2003
spatpod.ksnp_mtch$dstrds.2013<-(3/16)*spatpod.ksnp_mtch$dstrds00_MEAN.2003+(13/16)*spatpod.ksnp_mtch$dstrds16_MEAN.2003
spatpod.ksnp_mtch$dstrds.2014<-(2/16)*spatpod.ksnp_mtch$dstrds00_MEAN.2003+(14/16)*spatpod.ksnp_mtch$dstrds16_MEAN.2003
spatpod.ksnp_mtch$dstrds.2015<-(1/16)*spatpod.ksnp_mtch$dstrds00_MEAN.2003+(15/16)*spatpod.ksnp_mtch$dstrds16_MEAN.2003
spatpod.ksnp_mtch$dstrds.2016<-spatpod.ksnp_mtch$dstrds16_MEAN.2003

##Subsetting this data and adding it to the dataset that will be used to model trends over time

rdvars<-c("ID2003_d", "dstrds.2001", "dstrds.2002", "dstrds.2003", "dstrds.2004", "dstrds.2005",
          "dstrds.2006", "dstrds.2007", "dstrds.2008", "dstrds.2009", "dstrds.2010",
          "dstrds.2011", "dstrds.2012", "dstrds.2013", "dstrds.2014", "dstrds.2015", 
          "dstrds.2016")

rdvars_set<-spatpod.ksnp_mtch[rdvars]
View(rdvars_set)
#Creating Long-Format road data

rdvars_longset<-reshape(rdvars_set, direction="long", 
                        idvar='ID2003_d',
                        varying=c(2:17),
                        sep='.',
                        timevar="year",
                        times=c('2001','2002','2003','2004','2005','2006', '2007', '2008', '2009', '2010', '2011',
                                '2012', '2013', '2014', '2015', '2016'))

#Merging dataset to "yrforloss_KSNP"
yrforloss_KSNP_rds<-merge(yrforloss_KSNP, rdvars_longset, by=c("ID2003_d", "year"))
yrforloss_KSNP<-yrforloss_KSNP_rds


###Creating population data by year (linear assumption) and then merging###

###Renaming population data from original tables###
kspop00$pop.2000<-kspop00$MEAN
kspop00_merge<-kspop00 %>% 
  select(ID2003_D, pop.2000)
kspop05$pop.2005<-kspop05$MEAN
kspop05_merge<-kspop05 %>% 
  select(ID2003_D, pop.2005)
kspop10$pop.2010<-kspop10$MEAN
kspop10_merge<-kspop10 %>% 
  select(ID2003_D, pop.2010)
kspop15$pop.2015<-kspop15$MEAN
kspop15_merge<-kspop15 %>% 
  select(ID2003_D, pop.2015)
kspop20$pop.2020<-kspop20$MEAN
kspop20_merge<-kspop20 %>% 
  select(ID2003_D, pop.2020)

###Dropping old tables###
(rm(kspop00, kspop05, kspop10, kspop15, kspop20))

###Merging population data tables together with appropriate variable names###

kspop_merge<-list(kspop00_merge, kspop05_merge, kspop10_merge, kspop15_merge, kspop20_merge) %>% reduce(left_join, by = "ID2003_D")
str(kspop_merge)

###Creating yearly population data###
kspop_merge$pop.2001<-(4/5)*kspop_merge$pop.2000+(1/5)*kspop_merge$pop.2005
kspop_merge$pop.2002<-(3/5)*kspop_merge$pop.2000+(2/5)*kspop_merge$pop.2005
kspop_merge$pop.2003<-(2/5)*kspop_merge$pop.2000+(3/5)*kspop_merge$pop.2005
kspop_merge$pop.2004<-(1/5)*kspop_merge$pop.2000+(4/5)*kspop_merge$pop.2005
kspop_merge$pop.2006<-(4/5)*kspop_merge$pop.2005+(1/5)*kspop_merge$pop.2010
kspop_merge$pop.2007<-(3/5)*kspop_merge$pop.2005+(2/5)*kspop_merge$pop.2010
kspop_merge$pop.2008<-(2/5)*kspop_merge$pop.2005+(3/5)*kspop_merge$pop.2010
kspop_merge$pop.2009<-(1/5)*kspop_merge$pop.2005+(4/5)*kspop_merge$pop.2010
kspop_merge$pop.2011<-(4/5)*kspop_merge$pop.2010+(1/5)*kspop_merge$pop.2015
kspop_merge$pop.2012<-(3/5)*kspop_merge$pop.2010+(2/5)*kspop_merge$pop.2015
kspop_merge$pop.2013<-(2/5)*kspop_merge$pop.2010+(3/5)*kspop_merge$pop.2015
kspop_merge$pop.2014<-(1/5)*kspop_merge$pop.2010+(4/5)*kspop_merge$pop.2015
kspop_merge$pop.2016<-(4/5)*kspop_merge$pop.2015+(1/5)*kspop_merge$pop.2020
str(kspop_merge)

###Dropping 2020 data
kspop_merge$pop.2020<-NULL

###   Merging kspop.2003 to matching dataframe   ###

###First, renaming PODES pop.2003 data
spatpod.ksnp_mtch$pop.2003_pds<-spatpod.ksnp_mtch$pop.2003
spatpod.ksnp_mtch$pop.2003<-NULL

###Second, merging NASA pop.2003 data
kspop03_merge<-kspop_merge[,c("ID2003_D" , "pop.2003")]

spatpod.ksnp_mtch_pop<-merge(spatpod.ksnp_mtch, kspop03_merge, by.x="ID2003_d", by.y="ID2003_D", all.x=T)

###Transforming into long format###
###NOTE: Remember this dataset does not have the year 2000 data, so only taking data from column 3 (i.e., after 2000 values)
kspop_longset<-reshape(kspop_merge, direction="long", 
                        idvar='ID2003_D',
                        varying=c(3:18),
                        sep='.',
                        timevar="year",
                        times=c('2001','2002','2003','2004','2005','2006', '2007', '2008', '2009', '2010', '2011',
                                '2012', '2013', '2014', '2015', '2016'))
kspop_longset$ID2003_d<-kspop_longset$ID2003_D
kspop_longset$ID2003_D<-NULL
str(kspop_longset)

###Merging full pop data with yearly loss data

yrforloss_KSNP_pop<-merge(yrforloss_KSNP, kspop_longset, by=c("ID2003_d", "year"))
yrforloss_KSNP<-yrforloss_KSNP_pop

### Formatting buffer area table ###
###Transposing dataframe###

ksnpbuffer<-(as.data.frame(ksnpbuffer3) %>%
                     rownames_to_column %>% 
                     gather(var, value, -rowname) %>% 
                     spread(rowname, value))
#View(ksnpbuffer)
rm(ksnpbuffer3)

###Renaming rowid and truncating to ID2003_d info
ksnpbuffer$ID2003_d<-substr(ksnpbuffer$var,7,17)
ksnpbuffer$var<-NULL

###Renaming ForestLoss Variables###
ksnpbuffer$PxlFor_3km<-ksnpbuffer$'1'
ksnpbuffer$PxlNonFor_3km<-ksnpbuffer$'2'
#View(ksnpbuffer)

###Creating the Area (Ha) variable from pixel values###
ksnpbuffer$AreaFor_3km<-ksnpbuffer$PxlFor_3km*900/10000
ksnpbuffer$AreaNonFor_3km<-ksnpbuffer$PxlNonFor_3km*900/10000
##Subsetting dataset before merge##
ksnpbuffer_fnl<-ksnpbuffer[,c("ID2003_d", "AreaNonFor_3km", "AreaFor_3km")]
rm(ksnpbuffer3)

###Merging to matching dataset###
spatpod.ksnp_mtch_bfr<-merge(spatpod.ksnp_mtch_pop, ksnpbuffer_fnl, by="ID2003_d", all.x=T)
str(spatpod.ksnp_mtch_bfr)

rm(spatpod.ksnp_mtch, ksnpbuffer_fnl)

###Merging to VCA dataset###

spatpod.ksnp_mtch_all<-merge(spatpod.ksnp_mtch_bfr, ksnpvca[,c("ID2003_d", "VCA", "HKDII")], by="ID2003_d", all.x=T)
str(spatpod.ksnp_mtch_all)
which(is.na(spatpod.ksnp_mtch_all), arr.ind=TRUE)
rm(spatpod.ksnp_mtch_bfr)

###Checking NAs
which(is.na(spatpod.ksnp_mtch_all), arr.ind=TRUE)

##1702060040 and 1702060041 are completely within the national park boundary. That's why they don't have any 
##values for the buffer area. I will set these to 0, because they are real 0s . . .
spatpod.ksnp_mtch_all[240,28]<-0
spatpod.ksnp_mtch_all[240,29]<-0
spatpod.ksnp_mtch_all[241,28]<-0
spatpod.ksnp_mtch_all[241,29]<-0

###   Adding concession area data   ###

spatpod.ksnp_mtch_all2<-merge(spatpod.ksnp_mtch_all, ksnpconc, by.x="ID2003_d", by.y="ID2003_D", all.x=T)

summary(spatpod.ksnp_mtch_all2$VALUE_1)

spatpod.ksnp_mtch_all2$AreaConc<-spatpod.ksnp_mtch_all2$VALUE_1
spatpod.ksnp_mtch_all2$AreaConc[is.na(spatpod.ksnp_mtch_all2$AreaConc)]<-0
spatpod.ksnp_mtch_all2$AreaConc<-(spatpod.ksnp_mtch_all2$AreaConc*spatpod.ksnp_mtch_all2$Area2003)/10000
summary(spatpod.ksnp_mtch_all2$AreaConc)

###   Taking out unnecessary variables   ###

spatpod.ksnp_mtch_all3<-spatpod.ksnp_mtch_all2 %>% 
  select(-dstrds.2001, -dstrds.2002, -dstrds.2004, -dstrds.2005, -dstrds.2006, -dstrds.2007, -dstrds.2008, -dstrds.2009, -dstrds.2010,
         -dstrds.2011, -dstrds.2012, -dstrds.2013, -dstrds.2014, -dstrds.2015, -dstrds.2016, -Rowid_, -VALUE_1)
str(spatpod.ksnp_mtch_all3)

###Writing dataset###

write.csv(spatpod.ksnp_mtch_all3,"C:/Users/JTErbaugh/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/Data/spatpod.ksnp_mtch_all.csv")


############################################################################################################
###   Creating Matching Weights Difference-in-Difference Analyses   ########################################
############################################################################################################

###Scaling all variables###

spatpod.ksnp_mtch_all2$Area2003_z <- (spatpod.ksnp_mtch_all2$Area2003 - mean(spatpod.ksnp_mtch_all2$Area2003)) / sd(spatpod.ksnp_mtch_all2$Area2003)
spatpod.ksnp_mtch_all2$elev_MEAN.2003_z <- (spatpod.ksnp_mtch_all2$elev_MEAN.2003 - mean(spatpod.ksnp_mtch_all2$elev_MEAN.2003)) / sd(spatpod.ksnp_mtch_all2$elev_MEAN.2003)
spatpod.ksnp_mtch_all2$ovslp_MEAN.2003_z <- (spatpod.ksnp_mtch_all2$ovslp_MEAN.2003 - mean(spatpod.ksnp_mtch_all2$ovslp_MEAN.2003)) / sd(spatpod.ksnp_mtch_all2$ovslp_MEAN.2003)
spatpod.ksnp_mtch_all2$dstrds00_MEAN.2003_z <- (spatpod.ksnp_mtch_all2$dstrds00_MEAN.2003 - mean(spatpod.ksnp_mtch_all2$dstrds00_MEAN.2003)) / sd(spatpod.ksnp_mtch_all2$dstrds00_MEAN.2003)
spatpod.ksnp_mtch_all2$PAs_MEAN.2003_z <- (spatpod.ksnp_mtch_all2$PAs_MEAN.2003 - mean(spatpod.ksnp_mtch_all2$PAs_MEAN.2003)) / sd(spatpod.ksnp_mtch_all2$PAs_MEAN.2003)
spatpod.ksnp_mtch_all2$prmforlag_MEAN.2003_z <- (spatpod.ksnp_mtch_all2$prmforlag_MEAN.2003 - mean(spatpod.ksnp_mtch_all2$prmforlag_MEAN.2003)) / sd(spatpod.ksnp_mtch_all2$prmforlag_MEAN.2003)
spatpod.ksnp_mtch_all2$AreaNonFor_3km_z <- (spatpod.ksnp_mtch_all2$AreaNonFor_3km - mean(spatpod.ksnp_mtch_all2$AreaNonFor_3km, na.rm=T)) / sd(spatpod.ksnp_mtch_all2$AreaNonFor_3km, na.rm=T)
spatpod.ksnp_mtch_all2$AreaFor_3km_z <- (spatpod.ksnp_mtch_all2$AreaFor_3km - mean(spatpod.ksnp_mtch_all2$AreaFor_3km, na.rm=T)) / sd(spatpod.ksnp_mtch_all2$AreaFor_3km, na.rm=T)
spatpod.ksnp_mtch_all2$AreaConc_z <- (spatpod.ksnp_mtch_all2$AreaConc - mean(spatpod.ksnp_mtch_all2$AreaConc, na.rm=T)) / sd(spatpod.ksnp_mtch_all2$AreaConc, na.rm=T)
spatpod.ksnp_mtch_all2$pop.2003_z <- (spatpod.ksnp_mtch_all2$pop.2003 - mean(spatpod.ksnp_mtch_all2$pop.2003, na.rm=T)) / sd(spatpod.ksnp_mtch_all2$pop.2003, na.rm=T)
#spatpod.ksnp_mtch_all2$mdvp.2003_z <- (spatpod.ksnp_mtch_all2$mdvp.2003 - mean(spatpod.ksnp_mtch_all2$mdvp.2003, na.rm=T)) / sd(spatpod.ksnp_mtch_all2$mdvp.2003, na.rm=T)


###Running propensity score analysis###

##  Kitchen Sink Propensity Score
psm1.1<-glm(VCA~Area2003_z + 
              elev_MEAN.2003_z + 
              ovslp_MEAN.2003_z + 
              dstrds00_MEAN.2003_z + 
              PAs_MEAN.2003_z + 
              AreaNonFor_3km_z + 
              AreaFor_3km_z +
              prmforlag_MEAN.2003_z + 
              AreaConc_z +
              pop.2003_z,
              #mdvp.2003_z,
            data=spatpod.ksnp_mtch_all2, family="binomial")
summary(psm1.1)

##   Land Cover and Use Propensity Score (dropping pop and mdvp)
psm1.2<-glm(VCA~Area2003_z + 
              elev_MEAN.2003_z + 
              ovslp_MEAN.2003_z + 
              dstrds00_MEAN.2003_z + 
              PAs_MEAN.2003_z + 
              AreaNonFor_3km_z + 
              AreaFor_3km_z +
              AreaConc_z+
              pop.2003_z,
            data=spatpod.ksnp_mtch_all2, family="binomial")
summary(psm1.2)

psm1.3<-glm(VCA~#Area2003_z + 
              elev_MEAN.2003_z + 
              ovslp_MEAN.2003_z + 
              dstrds00_MEAN.2003_z + 
              #prmforlag_MEAN.2003_z +
              PAs_MEAN.2003_z + 
              AreaNonFor_3km_z + 
              #AreaFor_3km_z +
              AreaConc_z+
              pop.2003_z,
            data=spatpod.ksnp_mtch_all2, family="binomial")
summary(psm1.3)

anova(psm1.3, psm1.2, psm1.1, test="Chisq")

###The difference between the prop score model with area and for. area within 3 km is very close to 
###the model that does not include these variables. However, the model that does not include them does
###have a slightly (~1 pt) lower AIC. So, it seems best to match based on a prop score from the whole set, and then
###sub match on the smaller subset of vars from psm2.

###Using the best prop score model from above to predict propensity scores for all village observations

psm1.2<-glm(VCA~#Area2003_z + 
              elev_MEAN.2003_z + 
              ovslp_MEAN.2003_z + 
              dstrds00_MEAN.2003_z + 
              prmforlag_MEAN.2003_z + 
              PAs_MEAN.2003_z + 
              AreaNonFor_3km_z + 
              #AreaFor_3km_z +
              AreaConc_z+
              pop.2003,
            data=spatpod.ksnp_mtch_all2, family="binomial")
summary(psm1.2)

vif(psm1.2)

spatpod.ksnp_mtch_all2$ps03.1<- predict(psm1.2, type = "response")

rm(psm1.1, psm1.2, psm1.3, psm2)

sum(is.na(spatpod.ksnp_mtch_all2))
which(is.na(spatpod.ksnp_mtch_all2), arr.ind=TRUE)
View(spatpod.ksnp_mtch_all2)


###   REMOVING MISSING VARIABLES FROM DATASET BEFORE MATCHING   ###

str(spatpod.ksnp_mtch_all2)
spatpod.ksnp_mtch_all2$pop.2003_pds<-NULL
spatpod.ksnp_mtch_all2$mdvp.2003<-NULL
spatpod.ksnp_mtch_all2$VALUE_1<-NULL
spatpod.ksnp_mtch_all2$Rowid_<-NULL

which(is.na(spatpod.ksnp_mtch_all2), arr.ind=TRUE)


###Match 1.0: Mahalanobis within calipers##########################################################################

mahdataset<-data.frame(spatpod.ksnp_mtch_all2)
row.names(mahdataset)
row.names(mahdataset)<- paste0('X', row.names(mahdataset))

rm(spatpod.ksnp_mtch_all2)
sum(is.na(mahdataset))

ksnp_mtch1.2<-summary(ksnp_mtch1.1<-matchit(VCA ~
                                              elev_MEAN.2003_z + 
                                              ovslp_MEAN.2003_z + 
                                              dstrds00_MEAN.2003_z + 
                                              prmforlag_MEAN.2003_z + 
                                              AreaNonFor_3km_z + 
                                              AreaConc_z+
                                              pop.2003,
                                            data=mahdataset, 
                                            method="nearest",
                                            distance="logit",
                                            discard="both",
                                            caliper=.25, 
                                            mahvars=c(#"elev_MEAN.2003_z",
                                                      "ovslp_MEAN.2003_z",
                                                      "dstrds00_MEAN.2003_z",
                                                      "PAs_MEAN.2003_z",
                                                      "AreaNonFor_3km_z",
                                                      "AreaConc_z"),
                                            ratio=3,
                                            replace=TRUE),
                      standardize=TRUE) 


#plot(ksnp_mtch1.2)
ksnp_mtch_mah2<-match.data(ksnp_mtch1.1)
ksnp_mtch1.2
#write.csv(ksnp_mtch_mah2, "C:/Users/JTErbaugh/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/Data/ksnp_mtch_mah2.csv")

### Full Matching ##############################################################################################
ksnp_mtch2.2<-summary(ksnp_mtch2.1<-matchit(VCA ~
                                              elev_MEAN.2003_z + 
                                              ovslp_MEAN.2003_z + 
                                              dstrds00_MEAN.2003_z + 
                                              prmforlag_MEAN.2003_z + 
                                              AreaNonFor_3km_z + 
                                              AreaConc_z+
                                              pop.2003,
                                            data=mahdataset, 
                                            method="full",
                                            distance="logit",
                                            discard="both",
                                            caliper=.25, 
                                            #mahvars=c(#"Area2003_z", 
                                            #          "elev_MEAN.2003_z",
                                            #          "ovslp_MEAN.2003_z",
                                            #          "dstrds00_MEAN.2003_z",
                                            #          "PAs_MEAN.2003_z",
                                            #          "AreaNonFor_3km_z"),
                                            #ratio=3,
                                            replace=TRUE),
                      standardize=TRUE) 


#plot(ksnp_mtch2.2)
ksnp_mtch_full2<-match.data(ksnp_mtch2.1)
ksnp_mtch2.2
write.csv(ksnp_mtch_full2, "C:/Users/JTErbaugh/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/Data/ksnp_mtch_full2.csv")

### Genetic Matching ###########################################################################################
ksnp_mtch3.2<-summary(ksnp_mtch3.1<-matchit(VCA ~
                                              elev_MEAN.2003_z + 
                                              ovslp_MEAN.2003_z + 
                                              dstrds00_MEAN.2003_z + 
                                              prmforlag_MEAN.2003_z + 
                                              AreaNonFor_3km_z + 
                                              AreaConc_z+
                                              pop.2003,
                                            data=mahdataset, 
                                            method="genetic",
                                            #distance="logit",
                                            discard="both",
                                            #caliper=.25, 
                                            #mahvars=c(#"Area2003_z", 
                                            #          "elev_MEAN.2003_z",
                                            #          "ovslp_MEAN.2003_z",
                                            #          "dstrds00_MEAN.2003_z",
                                            #          "PAs_MEAN.2003_z",
                                            #          "AreaNonFor_3km_z"),
                                            ratio=1,
                                            replace=FALSE),
                      standardize=TRUE) 


#plot(ksnp_mtch3.2)
ksnp_mtch_gen2<-match.data(ksnp_mtch3.1)
ksnp_mtch3.2
write.csv(ksnp_mtch_gen2, "C:/Users/JTErbaugh/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/Data/ksnp_mtch_gen2.csv")

###Adding weights to Analysis Dataset###

###Subsetting all weights from all matches###
ksnp_mtch_mah2_wts<-ksnp_mtch_mah2[,c("ID2003_d", "weights")]
#Renaming mahalanobis weights
ksnp_mtch_mah2_wts$weights_mah<-ksnp_mtch_mah2_wts$weights
ksnp_mtch_mah2_wts$weights<-NULL

ksnp_mtch_full2_wts<-ksnp_mtch_full2[,c("ID2003_d", "weights")]
#Renaming full weights
ksnp_mtch_full2_wts$weights_full<-ksnp_mtch_full2_wts$weights
ksnp_mtch_full2_wts$weights<-NULL

ksnp_mtch_gen2_wts<-ksnp_mtch_gen2[,c("ID2003_d", "weights")]
#Renaming genetic weights
ksnp_mtch_gen2_wts$weights_gen<-ksnp_mtch_gen2_wts$weights
ksnp_mtch_gen2_wts$weights<-NULL

###Merging onto Analysis dataset###

yrforloss_KSNP_wts1<-merge(yrforloss_KSNP, ksnp_mtch_mah2_wts, BY="ID2000_D", all.x=T)
yrforloss_KSNP_wts2<-merge(yrforloss_KSNP_wts1, ksnp_mtch_full2_wts, BY="ID2000_D", all.x=T)
yrforloss_KSNP_wts3<-merge(yrforloss_KSNP_wts2, ksnp_mtch_gen2_wts, BY="ID2000_D", all.x=T)

yrforloss_KSNP_wts<-yrforloss_KSNP_wts3

#write.csv(yrforloss_KSNP_wts, "C:/Users/JTErbaugh/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/Data/yrforloss_KSNP_wts.csv")

rm(mahdataset, yrforloss_KSNP, ksnp_mtch_mah2_wts, yrforloss_KSNP_wts1, ksnp_mtch_mah2, 
   ksnp_mtch_full2_wts, ksnp_mtch_full2, ksnp_mtch_gen2,
   ksnp_mtch_full2_wts, ksnp_mtch_gen2_wts, yrforloss_KSNP_wts2, ksnp_mtch_gen2_wts)


#######################################
###   PRE-ANALYSIS VISUALIZATIONS   ###
#######################################

### Forest Cover Change Plots in the short-term #############################################################

#Overall tree-cover change in villages
allloss_plot<-ggplot(yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,], aes(year, (All_AreaForLoss_Ha), color = factor(VCA))) +
  stat_summary(geom = 'line') +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.1)+
  #geom_smooth(method="loess")+
  geom_vline(xintercept = 2002) +
  geom_vline(xintercept = 2003, linetype = "dashed")+
  ylab("Inside & Outside KSNP") +
  xlab("Year")+
  scale_color_manual(name="Village Type",
                      values=c("red","blue"),
                      labels=c("No VCA", "VCA")) +
  scale_x_discrete(limits=c(2001:2016))+
  theme_bw()

#Tree-cover change outside KSNP in villages
nonKSNPloss_plot<-ggplot(yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,], aes(year, nonKSNP_AreaForLoss_Ha, color = factor(VCA))) +
  stat_summary(geom = 'line') +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.1)+
  #geom_smooth(method="loess")+
  geom_vline(xintercept = 2002) +
  geom_vline(xintercept = 2003, linetype = "dashed") +
  ylab("Outside KSNP") +
  xlab("Year")+
  scale_color_manual(name="Village Type",
                      values=c("red","blue"),
                      labels=c("No VCA", "VCA")) +
  scale_x_discrete(limits=c(2001:2016))+
  theme_bw()

#Tree-cover inside KSNP in villages
ksnploss_plot<-ggplot(yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,], aes(year, KSNP_AreaForLoss_Ha2, color = factor(VCA))) +
  stat_summary(geom = 'line') +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.1)+
  #geom_smooth(method="loess")+
  geom_vline(xintercept = 2002) +
  geom_vline(xintercept = 2003, linetype = "dashed") +
  ylab("Inside KSNP") +
  xlab("Year")+
  scale_color_manual(name="Village Type",
                     values=c("red","blue"),
                      labels=c("No VCA", "VCA")) +
  scale_x_discrete(limits=c(2001:2016))+
  theme_bw()

##Combining KSNP and overall forest loss time trend graphs##
timetrends<-ggarrange(allloss_plot, nonKSNPloss_plot, ksnploss_plot, nrow=3, common.legend=T)
annotate_figure(timetrends, left = text_grob("Mean Tree-Cover Loss (Ha)", rot = 90))
timetrends

ggsave(file="C:/Users/JTErbaugh/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/timetrends_shortterm.png",
       plot=timetrends,
       dpi=600, width=8, height=6, unit="in") 

### Forest Cover Change Plots in the long-term #############################################################

#Overall tree-cover change in villages
allloss_plot<-ggplot(yrforloss_KSNP_wts, aes(year, (All_AreaForLoss_Ha), color = factor(VCA))) +
  stat_summary(geom = 'line') +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.1)+
  #geom_smooth(method="loess")+
  geom_vline(xintercept = 2002) +
  geom_vline(xintercept = 2003, linetype = "dashed")+
  ylab("Inside & Outside KSNP") +
  xlab("Year")+
  scale_color_manual(name="Village Type",
                     values=c("red","blue"),
                     labels=c("No VCA", "VCA")) +
  scale_x_discrete(limits=c(2001:2016))+
  theme_bw()

#Tree-cover change outside KSNP in villages
nonKSNPloss_plot<-ggplot(yrforloss_KSNP_wts, aes(year, nonKSNP_AreaForLoss_Ha, color = factor(VCA))) +
  stat_summary(geom = 'line') +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.1)+
  #geom_smooth(method="loess")+
  geom_vline(xintercept = 2002) +
  geom_vline(xintercept = 2003, linetype = "dashed") +
  ylab("Outside KSNP") +
  xlab("Year")+
  scale_color_manual(name="Village Type",
                     values=c("red","blue"),
                     labels=c("No VCA", "VCA")) +
  scale_x_discrete(limits=c(2001:2016))+
  theme_bw()

#Tree-cover inside KSNP in villages
ksnploss_plot<-ggplot(yrforloss_KSNP_wts, aes(year, KSNP_AreaForLoss_Ha2, color = factor(VCA))) +
  stat_summary(geom = 'line') +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.1)+
  #geom_smooth(method="loess")+
  geom_vline(xintercept = 2002) +
  geom_vline(xintercept = 2003, linetype = "dashed") +
  ylab("Inside KSNP") +
  xlab("Year")+
  scale_color_manual(name="Village Type",
                     values=c("red","blue"),
                     labels=c("No VCA", "VCA")) +
  scale_x_discrete(limits=c(2001:2016))+
  theme_bw()

##Combining KSNP and overall forest loss time trend graphs##
timetrends<-ggarrange(allloss_plot, nonKSNPloss_plot, ksnploss_plot, nrow=3, common.legend=T)
timetrends<-annotate_figure(timetrends,left=text_grob("Mean Tree-Cover Loss (Ha)", rot = 90))
timetrends

ggsave(file="C:/Users/JTErbaugh/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/timetrends_longterm.png",
       plot=timetrends,
       dpi=600, width=8, height=6, unit="in") 


### Forest Cover Change Plots WITHOUT SUMATRA BARAT ###

#Overall tree-cover change in villages
allloss_plot_2<-ggplot(yrforloss_KSNP_wts[yrforloss_KSNP_wts$ID2003_d>1399999999,], aes(year, All_AreaForLoss_Ha*weights_mah, color = factor(VCA))) +
  stat_summary(geom = 'line') +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.1)+
  #geom_smooth(method="loess")+
  geom_vline(xintercept = 2002) +
  geom_vline(xintercept = 2003, linetype = "dashed") +
  theme_bw()

#Tree-cover change outside KSNP in villages
nonKSNPloss_plot_2<-ggplot(yrforloss_KSNP_wts[yrforloss_KSNP_wts$ID2003_d>1399999999,], aes(year, nonKSNP_AreaForLoss_Ha*weights_mah, color = factor(VCA))) +
  stat_summary(geom = 'line') +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.1)+
  #geom_smooth(method="loess")+
  geom_vline(xintercept = 2002) +
  geom_vline(xintercept = 2003, linetype = "dashed") +
  theme_bw()

#Tree-cover inside KSNP in villages
ksnploss_plot_2<-ggplot(yrforloss_KSNP_wts[yrforloss_KSNP_wts$ID2003_d>1399999999,], aes(year, KSNP_AreaForLoss_Ha2*weights_mah, color = factor(VCA))) +
  stat_summary(geom = 'line') +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.1)+
  #geom_smooth(method="loess")+
  geom_vline(xintercept = 2002) +
  theme_bw()

##Combining KSNP and overall forest loss time trend graphs##
timetrends_noSB<-ggarrange(allloss_plot_2, nonKSNPloss_plot_2, ksnploss_plot_2, nrow=3)
timetrends_noSB

ggsave(file="C:/Users/JTErbaugh/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/timetrends_longterm_NOSB.png",
       plot=timetrends_noSB,
       dpi=600, width=8, height=6, unit="in") 

### Forest Cover Change Plots only for VCA villages #########################################################

#Overall tree-cover change in villages
allloss_plot_3<-ggplot(yrforloss_KSNP_wts[yrforloss_KSNP_wts$VCA>0 & yrforloss_KSNP_wts$year<2008,], aes(year, All_AreaForLoss_Ha, color = factor(HKDII))) +
  stat_summary(geom = 'line') +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.1)+
  #geom_smooth(method="loess")+
  geom_vline(xintercept = 2002) +
  geom_vline(xintercept = 2003, linetype = "dashed") +
  theme_bw()

#Tree-cover change outside KSNP in villages
nonKSNPloss_plot_3<-ggplot(yrforloss_KSNP_wts[yrforloss_KSNP_wts$VCA>0 & yrforloss_KSNP_wts$year<2008,], aes(year, nonKSNP_AreaForLoss_Ha, color = factor(HKDII))) +
  stat_summary(geom = 'line') +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.1)+
  #geom_smooth(method="loess")+
  geom_vline(xintercept = 2002) +
  geom_vline(xintercept = 2003, linetype = "dashed") +
  theme_bw()

#Tree-cover inside KSNP in villages
ksnploss_plot_3<-ggplot(yrforloss_KSNP_wts[yrforloss_KSNP_wts$VCA>0 & yrforloss_KSNP_wts$year<2008,], aes(year, KSNP_AreaForLoss_Ha2, color = factor(HKDII))) +
  stat_summary(geom = 'line') +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.1)+
  #geom_smooth(method="loess")+
  geom_vline(xintercept = 2002) +
  geom_vline(xintercept = 2003, linetype = "dashed") +
  theme_bw()

##Combining KSNP and overall forest loss time trend graphs##
timetrends_HKDII<-ggarrange(allloss_plot_3, nonKSNPloss_plot_3, ksnploss_plot_3, nrow=3)
timetrends_HKDII

ggsave(file="C:/Users/JTErbaugh/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/timetrends_shortterm_VCAonly.png",
       plot=timetrends_HKDII,
       dpi=600, width=8, height=6, unit="in") 

############################################################################################################
###   ANALYSIS   ###########################################################################################
############################################################################################################

###   Adding district-level ID to assess district time trends###

yrforloss_KSNP_wts$distID2003_d<-substr(yrforloss_KSNP_wts$ID2003_d, 1,5)
tail(yrforloss_KSNP_wts$distID2003_d)


View(yrforloss_KSNP_wts%>%arrange(ID2003_d, year))

#############################################################################################################
###   Un-Matched Difference-in-Difference Analyses   ########################################################
#############################################################################################################

###   Basic Difference in Difference for KSNP Forest Cover Loss   ###########################################
###   Short-Term (2000-2010)   ##########################################################################################
basic_did_KSNP.1<-lm(KSNP_AreaForLoss_Ha2~factor(did1) + dstrds + pop +
                       factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                     data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,])

summary(basic_did_KSNP.1)

###Checking the above with the within plm model. It checks out, so commenting it out.
# checkplm<-plm(KSNP_AreaForLoss_Ha2~factor(did1) + dstrds + pop +factor(year), 
#                 data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,],
#                 effect="twoways",
#               model="within", 
#               index=c("ID2003_d", "year"))
# summary(checkplm)

vcovHC(basic_did_KSNP.1, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(basic_did_KSNP.1, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_basic_did_KSNP.1 <- cbind(
  Estimate = coef(basic_did_KSNP.1)
  , "RobustSE" = sandwich_se
  , z = (coef(basic_did_KSNP.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(basic_did_KSNP.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(basic_did_KSNP.1) - q.val  * sandwich_se
  , UL = coef(basic_did_KSNP.1) + q.val  * sandwich_se
)
rob_basic_did_KSNP.1
rob_basic_did_KSNP.1<-as.data.frame(rob_basic_did_KSNP.1)
rob_basic_did_KSNP.1$coeff_var<-rownames(rob_basic_did_KSNP.1)
rob_basic_did_KSNP.1$depvar<-"KSNP"
rob_basic_did_KSNP.1$did_type<-"Dichot."
rob_basic_did_KSNP.1$mtch<-"None"
rob_basic_did_KSNP.1$term<-"Short-Term (2000-2007)"

###    Long-Term (2000-2016)   #########################################################################################
basic_did_KSNP.2<-lm(KSNP_AreaForLoss_Ha2~factor(did1)+ dstrds + pop +
                       factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                     data=yrforloss_KSNP_wts)

summary(basic_did_KSNP.2)

vcovHC(basic_did_KSNP.2, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(basic_did_KSNP.2, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_basic_did_KSNP.2 <- cbind(
  Estimate = coef(basic_did_KSNP.2)
  , "RobustSE" = sandwich_se
  , z = (coef(basic_did_KSNP.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(basic_did_KSNP.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(basic_did_KSNP.2) - q.val  * sandwich_se
  , UL = coef(basic_did_KSNP.2) + q.val  * sandwich_se
)
rob_basic_did_KSNP.2
rob_basic_did_KSNP.2<-as.data.frame(rob_basic_did_KSNP.2)
rob_basic_did_KSNP.2$coeff_var<-rownames(rob_basic_did_KSNP.2)
rob_basic_did_KSNP.2$depvar<-"KSNP"
rob_basic_did_KSNP.2$did_type<-"Dichot."
rob_basic_did_KSNP.2$mtch<-"None"
rob_basic_did_KSNP.2$term<-"Long-Term (2000-2016)"

###   Basic Difference in Difference for Non-KSNP Forest Cover Loss #########################################
###   Short-Term (2000-2010)   ##########################################################################################
basic_did_NonKSNP.1<-lm(nonKSNP_AreaForLoss_Ha~factor(did1)+ dstrds + pop +
                          factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                        data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,])

summary(basic_did_NonKSNP.1)

vcovHC(basic_did_NonKSNP.1, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(basic_did_NonKSNP.1, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_basic_did_NonKSNP.1 <- cbind(
  Estimate = coef(basic_did_NonKSNP.1)
  , "RobustSE" = sandwich_se
  , z = (coef(basic_did_NonKSNP.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(basic_did_NonKSNP.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(basic_did_NonKSNP.1) - q.val  * sandwich_se
  , UL = coef(basic_did_NonKSNP.1) + q.val  * sandwich_se
)
rob_basic_did_NonKSNP.1
rob_basic_did_NonKSNP.1<-as.data.frame(rob_basic_did_NonKSNP.1)
rob_basic_did_NonKSNP.1$coeff_var<-rownames(rob_basic_did_NonKSNP.1)
rob_basic_did_NonKSNP.1$depvar<-"Non-KSNP"
rob_basic_did_NonKSNP.1$did_type<-"Dichot."
rob_basic_did_NonKSNP.1$mtch<-"None"
rob_basic_did_NonKSNP.1$term<-"Short-Term (2000-2007)"

###   Long-Term (2000-2016)   ###########################################################################################
basic_did_NonKSNP.2<-lm(nonKSNP_AreaForLoss_Ha~factor(did1)+ dstrds + pop +
                          factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                        data=yrforloss_KSNP_wts)

summary(basic_did_NonKSNP.2)

vcovHC(basic_did_NonKSNP.2, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(basic_did_NonKSNP.2, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_basic_did_NonKSNP.2 <- cbind(
  Estimate = coef(basic_did_NonKSNP.2)
  , "RobustSE" = sandwich_se
  , z = (coef(basic_did_NonKSNP.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(basic_did_NonKSNP.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(basic_did_NonKSNP.2) - q.val  * sandwich_se
  , UL = coef(basic_did_NonKSNP.2) + q.val  * sandwich_se
)
rob_basic_did_NonKSNP.2
rob_basic_did_NonKSNP.2<-as.data.frame(rob_basic_did_NonKSNP.2)
rob_basic_did_NonKSNP.2$coeff_var<-rownames(rob_basic_did_NonKSNP.2)
rob_basic_did_NonKSNP.2$depvar<-"Non-KSNP"
rob_basic_did_NonKSNP.2$did_type<-"Dichot."
rob_basic_did_NonKSNP.2$mtch<-"None"
rob_basic_did_NonKSNP.2$term<-"Long-Term (2000-2016)"

###   Basic Difference in Difference for All Forest Cover Loss   ############################################    
###   Short-Term (2000-2010)   ##########################################################################################
basic_did_All.1<-lm(All_AreaForLoss_Ha~factor(did1)+ dstrds + pop +
                      factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                    data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,])

summary(basic_did_All.1)

vcovHC(basic_did_All.1, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(basic_did_All.1, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_basic_did_All.1 <- cbind(
  Estimate = coef(basic_did_All.1)
  , "RobustSE" = sandwich_se
  , z = (coef(basic_did_All.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(basic_did_All.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(basic_did_All.1) - q.val  * sandwich_se
  , UL = coef(basic_did_All.1) + q.val  * sandwich_se
)
rob_basic_did_All.1
rob_basic_did_All.1<-as.data.frame(rob_basic_did_All.1)
rob_basic_did_All.1$coeff_var<-rownames(rob_basic_did_All.1)
rob_basic_did_All.1$depvar<-"All"
rob_basic_did_All.1$did_type<-"Dichot."
rob_basic_did_All.1$mtch<-"None"
rob_basic_did_All.1$term<-"Short-Term (2000-2007)"

###   Long-Term (2000-2016)   ###########################################################################################
basic_did_All.2<-lm(All_AreaForLoss_Ha~factor(did1)+ dstrds + pop +
                      factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                    data=yrforloss_KSNP_wts)

summary(basic_did_All.2)

vcovHC(basic_did_All.2, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(basic_did_All.2, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_basic_did_All.2 <- cbind(
  Estimate = coef(basic_did_All.2)
  , "RobustSE" = sandwich_se
  , z = (coef(basic_did_All.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(basic_did_All.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(basic_did_All.2) - q.val  * sandwich_se
  , UL = coef(basic_did_All.2) + q.val  * sandwich_se
)
rob_basic_did_All.2
rob_basic_did_All.2<-as.data.frame(rob_basic_did_All.2)
rob_basic_did_All.2$coeff_var<-rownames(rob_basic_did_All.2)
rob_basic_did_All.2$depvar<-"All"
rob_basic_did_All.2$did_type<-"Dichot."
rob_basic_did_All.2$mtch<-"None"
rob_basic_did_All.2$term<-"Long-Term (2000-2016)"


#write_dta(yrforloss_KSNP, "C:/Users/JTErbaugh/Dropbox/#michigan phd/1.Dissertation/Ch5-Kerinci/KSNP-LongSet.dta")
### Stata difference-in-difference with robust SE checks out with the sandwich estimator from R, despite the
### warnings that code produces.

###   Continuous Difference in Difference for KSNP Forest Loss Area Short-Term (2000-2010)   #############################
###   Short-Term (2000-2010)   ###########################################################################################

basic_did_KSNP2.1<-lm(KSNP_AreaForLoss_Ha2~did2+ dstrds + pop +
                        factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                      data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,])

summary(basic_did_KSNP2.1)

vcovHC(basic_did_KSNP2.1, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(basic_did_KSNP2.1, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_basic_did_KSNP2.1 <- cbind(
  Estimate = coef(basic_did_KSNP2.1)
  , "RobustSE" = sandwich_se
  , z = (coef(basic_did_KSNP2.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(basic_did_KSNP2.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(basic_did_KSNP2.1) - q.val  * sandwich_se
  , UL = coef(basic_did_KSNP2.1) + q.val  * sandwich_se
)
rob_basic_did_KSNP2.1
rob_basic_did_KSNP2.1<-as.data.frame(rob_basic_did_KSNP2.1)
rob_basic_did_KSNP2.1$coeff_var<-rownames(rob_basic_did_KSNP2.1)
rob_basic_did_KSNP2.1$depvar<-"KSNP"
rob_basic_did_KSNP2.1$did_type<-"Continuous"
rob_basic_did_KSNP2.1$mtch<-"None"
rob_basic_did_KSNP2.1$term<-"Short-Term (2000-2007)"

###   Long-Term (2000-2016)  ############################################################################################

basic_did_KSNP2.2<-lm(KSNP_AreaForLoss_Ha2~did2+ dstrds + pop +
                        factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                      data=yrforloss_KSNP_wts)

summary(basic_did_KSNP2.2)

vcovHC(basic_did_KSNP2.2, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(basic_did_KSNP2.2, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_basic_did_KSNP2.2 <- cbind(
  Estimate = coef(basic_did_KSNP2.2)
  , "RobustSE" = sandwich_se
  , z = (coef(basic_did_KSNP2.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(basic_did_KSNP2.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(basic_did_KSNP2.2) - q.val  * sandwich_se
  , UL = coef(basic_did_KSNP2.2) + q.val  * sandwich_se
)
rob_basic_did_KSNP2.2
rob_basic_did_KSNP2.2<-as.data.frame(rob_basic_did_KSNP2.2)
rob_basic_did_KSNP2.2$coeff_var<-rownames(rob_basic_did_KSNP2.2)
rob_basic_did_KSNP2.2$depvar<-"KSNP"
rob_basic_did_KSNP2.2$did_type<-"Continuous"
rob_basic_did_KSNP2.2$mtch<-"None"
rob_basic_did_KSNP2.2$term<-"Long-Term (2000-2016)"

###   Continuous Difference in Difference for nonKSNP Forest Loss Area   ####################################
###   Short-Term (2000-2010)   ##########################################################################################

basic_did_NonKSNP2.1<-lm(nonKSNP_AreaForLoss_Ha~did2+ dstrds + pop +
                           factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                         data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,])

summary(basic_did_NonKSNP2.1)

vcovHC(basic_did_NonKSNP2.1, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(basic_did_NonKSNP2.1, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_basic_did_NonKSNP2.1 <- cbind(
  Estimate = coef(basic_did_NonKSNP2.1)
  , "RobustSE" = sandwich_se
  , z = (coef(basic_did_NonKSNP2.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(basic_did_NonKSNP2.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(basic_did_NonKSNP2.1) - q.val  * sandwich_se
  , UL = coef(basic_did_NonKSNP2.1) + q.val  * sandwich_se
)
rob_basic_did_NonKSNP2.1
rob_basic_did_NonKSNP2.1<-as.data.frame(rob_basic_did_NonKSNP2.1)
rob_basic_did_NonKSNP2.1$coeff_var<-rownames(rob_basic_did_NonKSNP2.1)
rob_basic_did_NonKSNP2.1$depvar<-"Non-KSNP"
rob_basic_did_NonKSNP2.1$did_type<-"Continuous"
rob_basic_did_NonKSNP2.1$mtch<-"None"
rob_basic_did_NonKSNP2.1$term<-"Short-Term (2000-2007)"

###   Long-Term (2000-2016)  ############################################################################################
basic_did_NonKSNP2.2<-lm(nonKSNP_AreaForLoss_Ha~did2+ dstrds + pop +
                           factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                         data=yrforloss_KSNP_wts)

summary(basic_did_NonKSNP2.2)

vcovHC(basic_did_NonKSNP2.2, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(basic_did_NonKSNP2.2, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_basic_did_NonKSNP2.2 <- cbind(
  Estimate = coef(basic_did_NonKSNP2.2)
  , "RobustSE" = sandwich_se
  , z = (coef(basic_did_NonKSNP2.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(basic_did_NonKSNP2.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(basic_did_NonKSNP2.2) - q.val  * sandwich_se
  , UL = coef(basic_did_NonKSNP2.2) + q.val  * sandwich_se
)
rob_basic_did_NonKSNP2.2
rob_basic_did_NonKSNP2.2<-as.data.frame(rob_basic_did_NonKSNP2.2)
rob_basic_did_NonKSNP2.2$coeff_var<-rownames(rob_basic_did_NonKSNP2.2)
rob_basic_did_NonKSNP2.2$depvar<-"Non-KSNP"
rob_basic_did_NonKSNP2.2$did_type<-"Continuous"
rob_basic_did_NonKSNP2.2$mtch<-"None"
rob_basic_did_NonKSNP2.2$term<-"Long-Term (2000-2016)"


###   Continuous Difference in Difference for All Forest Loss Area   #########################################
###   Short-Term (2000-2010)   ############################################################################################

basic_did_All2.1<-lm(All_AreaForLoss_Ha~did2+ dstrds + pop +
                       factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                     data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,])

summary(basic_did_All2.1)

vcovHC(basic_did_All2.1, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(basic_did_All2.1, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_basic_did_All2.1 <- cbind(
  Estimate = coef(basic_did_All2.1)
  , "RobustSE" = sandwich_se
  , z = (coef(basic_did_All2.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(basic_did_All2.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(basic_did_All2.1) - q.val  * sandwich_se
  , UL = coef(basic_did_All2.1) + q.val  * sandwich_se
)
rob_basic_did_All2.1
rob_basic_did_All2.1<-as.data.frame(rob_basic_did_All2.1)
rob_basic_did_All2.1$coeff_var<-rownames(rob_basic_did_All2.1)
rob_basic_did_All2.1$depvar<-"All"
rob_basic_did_All2.1$did_type<-"Continuous"
rob_basic_did_All2.1$mtch<-"None"
rob_basic_did_All2.1$term<-"Short-Term (2000-2007)"

###   Long-Term (2000-2016)   ############################################################################################

basic_did_All2.2<-lm(All_AreaForLoss_Ha~did2+dstrds+ pop +
                       factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                     data=yrforloss_KSNP_wts)

summary(basic_did_All2.2)

vcovHC(basic_did_All2.2, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(basic_did_All2.2, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_basic_did_All2.2 <- cbind(
  Estimate = coef(basic_did_All2.2)
  , "RobustSE" = sandwich_se
  , z = (coef(basic_did_All2.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(basic_did_All2.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(basic_did_All2.2) - q.val  * sandwich_se
  , UL = coef(basic_did_All2.2) + q.val  * sandwich_se
)
rob_basic_did_All2.2
rob_basic_did_All2.2<-as.data.frame(rob_basic_did_All2.2)
rob_basic_did_All2.2$coeff_var<-rownames(rob_basic_did_All2.2)
rob_basic_did_All2.2$depvar<-"All"
rob_basic_did_All2.2$did_type<-"Continuous"
rob_basic_did_All2.2$mtch<-"None"
rob_basic_did_All2.2$term<-"Long-Term (2000-2016)"

################################################################################################################
###Re-running analyses as weighted regressions with different matching weights   ############################
################################################################################################################

###Analysis with Mahalanobis Matching###########################################################################

###   Basic Difference in Difference for KSNP Forest Cover Loss    ##########################################
###   Short-Term (2000-2010)   ##########################################################################################
did_KSNP_mah.1<-lm(KSNP_AreaForLoss_Ha2~factor(did1) + dstrds+ pop +
                     factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                   data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,],
                   weights=weights_mah)

summary(did_KSNP_mah.1)

vcovHC(did_KSNP_mah.1, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_KSNP_mah.1, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_KSNP_mah.1 <- cbind(
  Estimate = coef(did_KSNP_mah.1)
  , "RobustSE" = sandwich_se
  , z = (coef(did_KSNP_mah.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_KSNP_mah.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_KSNP_mah.1) - q.val  * sandwich_se
  , UL = coef(did_KSNP_mah.1) + q.val  * sandwich_se
)
rob_did_KSNP_mah.1
rob_did_KSNP_mah.1<-as.data.frame(rob_did_KSNP_mah.1)
rob_did_KSNP_mah.1$coeff_var<-rownames(rob_did_KSNP_mah.1)
rob_did_KSNP_mah.1$depvar<-"KSNP"
rob_did_KSNP_mah.1$did_type<-"Dichot."
rob_did_KSNP_mah.1$mtch<-"Mahal."
rob_did_KSNP_mah.1$term<-"Short-Term (2000-2007)"

###   Long-Term (2000-2016)   ##########################################################################################
did_KSNP_mah.2<-lm(KSNP_AreaForLoss_Ha2~factor(did1) + dstrds + pop +
                     factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                   data=yrforloss_KSNP_wts,
                   weights=weights_mah)

summary(did_KSNP_mah.2)

vcovHC(did_KSNP_mah.2, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_KSNP_mah.2, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_KSNP_mah.2 <- cbind(
  Estimate = coef(did_KSNP_mah.2)
  , "RobustSE" = sandwich_se
  , z = (coef(did_KSNP_mah.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_KSNP_mah.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_KSNP_mah.2) - q.val  * sandwich_se
  , UL = coef(did_KSNP_mah.2) + q.val  * sandwich_se
)
rob_did_KSNP_mah.2
rob_did_KSNP_mah.2<-as.data.frame(rob_did_KSNP_mah.2)
rob_did_KSNP_mah.2$coeff_var<-rownames(rob_did_KSNP_mah.2)
rob_did_KSNP_mah.2$depvar<-"KSNP"
rob_did_KSNP_mah.2$did_type<-"Dichot."
rob_did_KSNP_mah.2$mtch<-"Mahal."
rob_did_KSNP_mah.2$term<-"Long-Term (2000-2016)"

###   Basic Difference in Difference for Non-KSNP Forest Cover Loss    ######################################
###   Short-Term (2000-2010)   ##########################################################################################
did_NonKSNP_mah.1<-lm(nonKSNP_AreaForLoss_Ha~factor(did1)+dstrds+pop+
                        factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                      data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,],
                      weights=weights_mah)

summary(did_NonKSNP_mah.1)

vcovHC(did_NonKSNP_mah.1, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_NonKSNP_mah.1, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_NonKSNP_mah.1 <- cbind(
  Estimate = coef(did_NonKSNP_mah.1)
  , "RobustSE" = sandwich_se
  , z = (coef(did_NonKSNP_mah.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_NonKSNP_mah.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_NonKSNP_mah.1) - q.val  * sandwich_se
  , UL = coef(did_NonKSNP_mah.1) + q.val  * sandwich_se
)
rob_did_NonKSNP_mah.1
rob_did_NonKSNP_mah.1<-as.data.frame(rob_did_NonKSNP_mah.1)
rob_did_NonKSNP_mah.1$coeff_var<-rownames(rob_did_NonKSNP_mah.1)
rob_did_NonKSNP_mah.1$depvar<-"Non-KSNP"
rob_did_NonKSNP_mah.1$did_type<-"Dichot."
rob_did_NonKSNP_mah.1$mtch<-"Mahal."
rob_did_NonKSNP_mah.1$term<-"Short-Term (2000-2007)"

###   Long-Term (2000-2016)   ##########################################################################################
did_NonKSNP_mah.2<-lm(nonKSNP_AreaForLoss_Ha~factor(did1)+dstrds+pop+
                        factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                      data=yrforloss_KSNP_wts,
                      weights=weights_mah)

summary(did_NonKSNP_mah.2)

vcovHC(did_NonKSNP_mah.2, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_NonKSNP_mah.2, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_NonKSNP_mah.2 <- cbind(
  Estimate = coef(did_NonKSNP_mah.2)
  , "RobustSE" = sandwich_se
  , z = (coef(did_NonKSNP_mah.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_NonKSNP_mah.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_NonKSNP_mah.2) - q.val  * sandwich_se
  , UL = coef(did_NonKSNP_mah.2) + q.val  * sandwich_se
)
rob_did_NonKSNP_mah.2
rob_did_NonKSNP_mah.2<-as.data.frame(rob_did_NonKSNP_mah.2)
rob_did_NonKSNP_mah.2$coeff_var<-rownames(rob_did_NonKSNP_mah.2)
rob_did_NonKSNP_mah.2$depvar<-"Non-KSNP"
rob_did_NonKSNP_mah.2$did_type<-"Dichot."
rob_did_NonKSNP_mah.2$mtch<-"Mahal."
rob_did_NonKSNP_mah.2$term<-"Long-Term (2000-2016)"

###   Basic Difference in Difference for All Forest Cover Loss    ###########################################
###   Short-Term (2000-2010)   ##########################################################################################
did_All_mah.1<-lm(All_AreaForLoss_Ha~factor(did1)+dstrds+pop+
                    factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                  data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,],
                  weights=weights_mah)

summary(did_All_mah.1)

vcovHC(did_All_mah.1, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_All_mah.1, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_All_mah.1 <- cbind(
  Estimate = coef(did_All_mah.1)
  , "RobustSE" = sandwich_se
  , z = (coef(did_All_mah.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_All_mah.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_All_mah.1) - q.val  * sandwich_se
  , UL = coef(did_All_mah.1) + q.val  * sandwich_se
)
rob_did_All_mah.1
rob_did_All_mah.1<-as.data.frame(rob_did_All_mah.1)
rob_did_All_mah.1$coeff_var<-rownames(rob_did_All_mah.1)
rob_did_All_mah.1$depvar<-"All"
rob_did_All_mah.1$did_type<-"Dichot."
rob_did_All_mah.1$mtch<-"Mahal."
rob_did_All_mah.1$term<-"Short-Term (2000-2007)"

###   Long-Term (2000-2016)   ##########################################################################################
did_All_mah.2<-lm(All_AreaForLoss_Ha~factor(did1)+dstrds+pop+
                    factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                  data=yrforloss_KSNP_wts,
                  weights=weights_mah)

summary(did_All_mah.2)

vcovHC(did_All_mah.2, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_All_mah.2, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_All_mah.2 <- cbind(
  Estimate = coef(did_All_mah.2)
  , "RobustSE" = sandwich_se
  , z = (coef(did_All_mah.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_All_mah.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_All_mah.2) - q.val  * sandwich_se
  , UL = coef(did_All_mah.2) + q.val  * sandwich_se
)
rob_did_All_mah.2
rob_did_All_mah.2<-as.data.frame(rob_did_All_mah.2)
rob_did_All_mah.2$coeff_var<-rownames(rob_did_All_mah.2)
rob_did_All_mah.2$depvar<-"All"
rob_did_All_mah.2$did_type<-"Dichot."
rob_did_All_mah.2$mtch<-"Mahal."
rob_did_All_mah.2$term<-"Long-Term (2000-2016)"

###Analysis with Full Matching##################################################################################

###   Basic Difference in Difference for KSNP Forest Cover Loss    ##########################################
###   Short-Term (2000-2010)   ##########################################################################################
did_KSNP_full.1<-lm(KSNP_AreaForLoss_Ha2~factor(did1)+dstrds+pop+
                     factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                   data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,],
                   weights=weights_full)

summary(did_KSNP_full.1)

vcovHC(did_KSNP_full.1, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_KSNP_full.1, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_KSNP_full.1 <- cbind(
  Estimate = coef(did_KSNP_full.1)
  , "RobustSE" = sandwich_se
  , z = (coef(did_KSNP_full.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_KSNP_full.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_KSNP_full.1) - q.val  * sandwich_se
  , UL = coef(did_KSNP_full.1) + q.val  * sandwich_se
)
rob_did_KSNP_full.1
rob_did_KSNP_full.1<-as.data.frame(rob_did_KSNP_full.1)
rob_did_KSNP_full.1$coeff_var<-rownames(rob_did_KSNP_full.1)
rob_did_KSNP_full.1$depvar<-"KSNP"
rob_did_KSNP_full.1$did_type<-"Dichot."
rob_did_KSNP_full.1$mtch<-"Full"
rob_did_KSNP_full.1$term<-"Short-Term (2000-2007)"

###   Long-Term (2000-2016)   ##########################################################################################
did_KSNP_full.2<-lm(KSNP_AreaForLoss_Ha2~factor(did1)+dstrds+pop+
                     factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                   data=yrforloss_KSNP_wts,
                   weights=weights_full)

summary(did_KSNP_full.2)

vcovHC(did_KSNP_full.2, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_KSNP_full.2, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_KSNP_full.2 <- cbind(
  Estimate = coef(did_KSNP_full.2)
  , "RobustSE" = sandwich_se
  , z = (coef(did_KSNP_full.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_KSNP_full.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_KSNP_full.2) - q.val  * sandwich_se
  , UL = coef(did_KSNP_full.2) + q.val  * sandwich_se
)
rob_did_KSNP_full.2
rob_did_KSNP_full.2<-as.data.frame(rob_did_KSNP_full.2)
rob_did_KSNP_full.2$coeff_var<-rownames(rob_did_KSNP_full.2)
rob_did_KSNP_full.2$depvar<-"KSNP"
rob_did_KSNP_full.2$did_type<-"Dichot."
rob_did_KSNP_full.2$mtch<-"Full"
rob_did_KSNP_full.2$term<-"Long-Term (2000-2016)"

###   Basic Difference in Difference for Non-KSNP Forest Cover Loss    ######################################
###   Short-Term (2000-2010)   ##########################################################################################
did_NonKSNP_full.1<-lm(nonKSNP_AreaForLoss_Ha~factor(did1)+dstrds+pop+
                        factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                      data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,],
                      weights=weights_full)

summary(did_NonKSNP_full.1)

vcovHC(did_NonKSNP_full.1, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_NonKSNP_full.1, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_NonKSNP_full.1 <- cbind(
  Estimate = coef(did_NonKSNP_full.1)
  , "RobustSE" = sandwich_se
  , z = (coef(did_NonKSNP_full.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_NonKSNP_full.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_NonKSNP_full.1) - q.val  * sandwich_se
  , UL = coef(did_NonKSNP_full.1) + q.val  * sandwich_se
)
rob_did_NonKSNP_full.1
rob_did_NonKSNP_full.1<-as.data.frame(rob_did_NonKSNP_full.1)
rob_did_NonKSNP_full.1$coeff_var<-rownames(rob_did_NonKSNP_full.1)
rob_did_NonKSNP_full.1$depvar<-"Non-KSNP"
rob_did_NonKSNP_full.1$did_type<-"Dichot."
rob_did_NonKSNP_full.1$mtch<-"Full"
rob_did_NonKSNP_full.1$term<-"Short-Term (2000-2007)"

###   Long-Term (2000-2016)   ##########################################################################################
did_NonKSNP_full.2<-lm(nonKSNP_AreaForLoss_Ha~factor(did1)+dstrds+pop+
                        factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                      data=yrforloss_KSNP_wts,
                      weights=weights_full)

summary(did_NonKSNP_full.2)

vcovHC(did_NonKSNP_full.2, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_NonKSNP_full.2, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_NonKSNP_full.2 <- cbind(
  Estimate = coef(did_NonKSNP_full.2)
  , "RobustSE" = sandwich_se
  , z = (coef(did_NonKSNP_full.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_NonKSNP_full.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_NonKSNP_full.2) - q.val  * sandwich_se
  , UL = coef(did_NonKSNP_full.2) + q.val  * sandwich_se
)
rob_did_NonKSNP_full.2
rob_did_NonKSNP_full.2<-as.data.frame(rob_did_NonKSNP_full.2)
rob_did_NonKSNP_full.2$coeff_var<-rownames(rob_did_NonKSNP_full.2)
rob_did_NonKSNP_full.2$depvar<-"Non-KSNP"
rob_did_NonKSNP_full.2$did_type<-"Dichot."
rob_did_NonKSNP_full.2$mtch<-"Full"
rob_did_NonKSNP_full.2$term<-"Long-Term (2000-2016)"

###   Basic Difference in Difference for All Forest Cover Loss    ###########################################
###   Short-Term (2000-2010)   ##########################################################################################
did_All_full.1<-lm(All_AreaForLoss_Ha~factor(did1)+dstrds+pop+
                    factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                  data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,],
                  weights=weights_full)

summary(did_All_full.1)

vcovHC(did_All_full.1, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_All_full.1, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_All_full.1 <- cbind(
  Estimate = coef(did_All_full.1)
  , "RobustSE" = sandwich_se
  , z = (coef(did_All_full.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_All_full.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_All_full.1) - q.val  * sandwich_se
  , UL = coef(did_All_full.1) + q.val  * sandwich_se
)
rob_did_All_full.1
rob_did_All_full.1<-as.data.frame(rob_did_All_full.1)
rob_did_All_full.1$coeff_var<-rownames(rob_did_All_full.1)
rob_did_All_full.1$depvar<-"All"
rob_did_All_full.1$did_type<-"Dichot."
rob_did_All_full.1$mtch<-"Full"
rob_did_All_full.1$term<-"Short-Term (2000-2007)"

###   Long-Term (2000-2016)   ##########################################################################################
did_All_full.2<-lm(All_AreaForLoss_Ha~factor(did1)+dstrds+pop+
                    factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                  data=yrforloss_KSNP_wts,
                  weights=weights_full)

summary(did_All_full.2)

vcovHC(did_All_full.2, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_All_full.2, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_All_full.2 <- cbind(
  Estimate = coef(did_All_full.2)
  , "RobustSE" = sandwich_se
  , z = (coef(did_All_full.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_All_full.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_All_full.2) - q.val  * sandwich_se
  , UL = coef(did_All_full.2) + q.val  * sandwich_se
)
rob_did_All_full.2
rob_did_All_full.2<-as.data.frame(rob_did_All_full.2)
rob_did_All_full.2$coeff_var<-rownames(rob_did_All_full.2)
rob_did_All_full.2$depvar<-"All"
rob_did_All_full.2$did_type<-"Dichot."
rob_did_All_full.2$mtch<-"Full"
rob_did_All_full.2$term<-"Long-Term (2000-2016)"

###Analysis with Genetic Matching##################################################################################

###   Basic Difference in Difference for KSNP Forest Cover Loss    ##########################################
###   Short-Term (2000-2010)   ##########################################################################################
did_KSNP_gen.1<-lm(KSNP_AreaForLoss_Ha2~factor(did1)+dstrds+pop+
                     factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                   data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,],
                   weights=weights_gen)

summary(did_KSNP_gen.1)

vcovHC(did_KSNP_gen.1, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_KSNP_gen.1, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_KSNP_gen.1 <- cbind(
  Estimate = coef(did_KSNP_gen.1)
  , "RobustSE" = sandwich_se
  , z = (coef(did_KSNP_gen.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_KSNP_gen.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_KSNP_gen.1) - q.val  * sandwich_se
  , UL = coef(did_KSNP_gen.1) + q.val  * sandwich_se
)
rob_did_KSNP_gen.1
rob_did_KSNP_gen.1<-as.data.frame(rob_did_KSNP_gen.1)
rob_did_KSNP_gen.1$coeff_var<-rownames(rob_did_KSNP_gen.1)
rob_did_KSNP_gen.1$depvar<-"KSNP"
rob_did_KSNP_gen.1$did_type<-"Dichot."
rob_did_KSNP_gen.1$mtch<-"Genetic"
rob_did_KSNP_gen.1$term<-"Short-Term (2000-2007)"

###   Long-Term (2000-2016)   ##########################################################################################
did_KSNP_gen.2<-lm(KSNP_AreaForLoss_Ha2~factor(did1)+dstrds+pop+
                     factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                   data=yrforloss_KSNP_wts,
                   weights=weights_gen)

summary(did_KSNP_gen.2)

vcovHC(did_KSNP_gen.2, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_KSNP_gen.2, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_KSNP_gen.2 <- cbind(
  Estimate = coef(did_KSNP_gen.2)
  , "RobustSE" = sandwich_se
  , z = (coef(did_KSNP_gen.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_KSNP_gen.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_KSNP_gen.2) - q.val  * sandwich_se
  , UL = coef(did_KSNP_gen.2) + q.val  * sandwich_se
)
rob_did_KSNP_gen.2
rob_did_KSNP_gen.2<-as.data.frame(rob_did_KSNP_gen.2)
rob_did_KSNP_gen.2$coeff_var<-rownames(rob_did_KSNP_gen.2)
rob_did_KSNP_gen.2$depvar<-"KSNP"
rob_did_KSNP_gen.2$did_type<-"Dichot."
rob_did_KSNP_gen.2$mtch<-"Genetic"
rob_did_KSNP_gen.2$term<-"Long-Term (2000-2016)"

###   Basic Difference in Difference for Non-KSNP Forest Cover Loss    ######################################
###   Short-Term (2000-2010)   ##########################################################################################
did_NonKSNP_gen.1<-lm(nonKSNP_AreaForLoss_Ha~factor(did1)+dstrds+pop+
                        factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                      data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,],
                      weights=weights_gen)

summary(did_NonKSNP_gen.1)

vcovHC(did_NonKSNP_gen.1, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_NonKSNP_gen.1, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_NonKSNP_gen.1 <- cbind(
  Estimate = coef(did_NonKSNP_gen.1)
  , "RobustSE" = sandwich_se
  , z = (coef(did_NonKSNP_gen.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_NonKSNP_gen.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_NonKSNP_gen.1) - q.val  * sandwich_se
  , UL = coef(did_NonKSNP_gen.1) + q.val  * sandwich_se
)
rob_did_NonKSNP_gen.1
rob_did_NonKSNP_gen.1<-as.data.frame(rob_did_NonKSNP_gen.1)
rob_did_NonKSNP_gen.1$coeff_var<-rownames(rob_did_NonKSNP_gen.1)
rob_did_NonKSNP_gen.1$depvar<-"Non-KSNP"
rob_did_NonKSNP_gen.1$did_type<-"Dichot."
rob_did_NonKSNP_gen.1$mtch<-"Genetic"
rob_did_NonKSNP_gen.1$term<-"Short-Term (2000-2007)"

###   Long-Term (2000-2016)   ##########################################################################################
did_NonKSNP_gen.2<-lm(nonKSNP_AreaForLoss_Ha~factor(did1)+dstrds+pop+
                        factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                      data=yrforloss_KSNP_wts,
                      weights=weights_gen)

summary(did_NonKSNP_gen.2)

vcovHC(did_NonKSNP_gen.2, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_NonKSNP_gen.2, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_NonKSNP_gen.2 <- cbind(
  Estimate = coef(did_NonKSNP_gen.2)
  , "RobustSE" = sandwich_se
  , z = (coef(did_NonKSNP_gen.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_NonKSNP_gen.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_NonKSNP_gen.2) - q.val  * sandwich_se
  , UL = coef(did_NonKSNP_gen.2) + q.val  * sandwich_se
)
rob_did_NonKSNP_gen.2
rob_did_NonKSNP_gen.2<-as.data.frame(rob_did_NonKSNP_gen.2)
rob_did_NonKSNP_gen.2$coeff_var<-rownames(rob_did_NonKSNP_gen.2)
rob_did_NonKSNP_gen.2$depvar<-"Non-KSNP"
rob_did_NonKSNP_gen.2$did_type<-"Dichot."
rob_did_NonKSNP_gen.2$mtch<-"Genetic"
rob_did_NonKSNP_gen.2$term<-"Long-Term (2000-2016)"

###   Basic Difference in Difference for All Forest Cover Loss    ###########################################
###   Short-Term (2000-2010)   ##########################################################################################
did_All_gen.1<-lm(All_AreaForLoss_Ha~factor(did1)+dstrds+pop+
                    factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                  data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,],
                  weights=weights_gen)

summary(did_All_gen.1)

vcovHC(did_All_gen.1, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_All_gen.1, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_All_gen.1 <- cbind(
  Estimate = coef(did_All_gen.1)
  , "RobustSE" = sandwich_se
  , z = (coef(did_All_gen.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_All_gen.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_All_gen.1) - q.val  * sandwich_se
  , UL = coef(did_All_gen.1) + q.val  * sandwich_se
)
rob_did_All_gen.1
rob_did_All_gen.1<-as.data.frame(rob_did_All_gen.1)
rob_did_All_gen.1$coeff_var<-rownames(rob_did_All_gen.1)
rob_did_All_gen.1$depvar<-"All"
rob_did_All_gen.1$did_type<-"Dichot."
rob_did_All_gen.1$mtch<-"Genetic"
rob_did_All_gen.1$term<-"Short-Term (2000-2007)"

###   Long-Term (2000-2016)   ##########################################################################################
did_All_gen.2<-lm(All_AreaForLoss_Ha~factor(did1)+dstrds+pop+
                    factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                  data=yrforloss_KSNP_wts,
                  weights=weights_gen)

summary(did_All_gen.2)

vcovHC(did_All_gen.2, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_All_gen.2, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_All_gen.2 <- cbind(
  Estimate = coef(did_All_gen.2)
  , "RobustSE" = sandwich_se
  , z = (coef(did_All_gen.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_All_gen.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_All_gen.2) - q.val  * sandwich_se
  , UL = coef(did_All_gen.2) + q.val  * sandwich_se
)
rob_did_All_gen.2
rob_did_All_gen.2<-as.data.frame(rob_did_All_gen.2)
rob_did_All_gen.2$coeff_var<-rownames(rob_did_All_gen.2)
rob_did_All_gen.2$depvar<-"All"
rob_did_All_gen.2$did_type<-"Dichot."
rob_did_All_gen.2$mtch<-"Genetic"
rob_did_All_gen.2$term<-"Long-Term (2000-2016)"


###Continuous Difference-In-Difference Analysis with Matching####################################################

###   Analysis with Mahalanobis Matching   ################################################################
###   Difference in Difference for KSNP Forest Cover Loss    ################################################
###   Short-Term (2000-2010)   ##########################################################################################
did_KSNP_mah2.1<-lm(KSNP_AreaForLoss_Ha2~did2+dstrds+pop+
                     factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                   data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,],
                   weights=weights_mah)

summary(did_KSNP_mah2.1)

vcovHC(did_KSNP_mah2.1, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_KSNP_mah2.1, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_KSNP_mah2.1 <- cbind(
  Estimate = coef(did_KSNP_mah2.1)
  , "RobustSE" = sandwich_se
  , z = (coef(did_KSNP_mah2.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_KSNP_mah2.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_KSNP_mah2.1) - q.val  * sandwich_se
  , UL = coef(did_KSNP_mah2.1) + q.val  * sandwich_se
)
rob_did_KSNP_mah2.1
rob_did_KSNP_mah2.1<-as.data.frame(rob_did_KSNP_mah2.1)
rob_did_KSNP_mah2.1$coeff_var<-rownames(rob_did_KSNP_mah2.1)
rob_did_KSNP_mah2.1$depvar<-"KSNP"
rob_did_KSNP_mah2.1$did_type<-"Continuous"
rob_did_KSNP_mah2.1$mtch<-"Mahal."
rob_did_KSNP_mah2.1$term<-"Short-Term (2000-2007)"

###   Long-Term (2000-2016)   ##########################################################################################
did_KSNP_mah2.2<-lm(KSNP_AreaForLoss_Ha2~did2+dstrds+pop+
                     factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                   data=yrforloss_KSNP_wts,
                   weights=weights_mah)

summary(did_KSNP_mah2.2)

vcovHC(did_KSNP_mah2.2, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_KSNP_mah2.2, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_KSNP_mah2.2 <- cbind(
  Estimate = coef(did_KSNP_mah2.2)
  , "RobustSE" = sandwich_se
  , z = (coef(did_KSNP_mah2.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_KSNP_mah2.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_KSNP_mah2.2) - q.val  * sandwich_se
  , UL = coef(did_KSNP_mah2.2) + q.val  * sandwich_se
)
rob_did_KSNP_mah2.2
rob_did_KSNP_mah2.2<-as.data.frame(rob_did_KSNP_mah2.2)
rob_did_KSNP_mah2.2$coeff_var<-rownames(rob_did_KSNP_mah2.2)
rob_did_KSNP_mah2.2$depvar<-"KSNP"
rob_did_KSNP_mah2.2$did_type<-"Continuous"
rob_did_KSNP_mah2.2$mtch<-"Mahal."
rob_did_KSNP_mah2.2$term<-"Long-Term (2000-2016)"

###   Basic Difference in Difference for Non-KSNP Forest Cover Loss    ######################################
###   Short-Term (2000-2010)   ##########################################################################################
did_nonKSNP_mah2.1<-lm(nonKSNP_AreaForLoss_Ha~did2+dstrds+pop+
                        factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                      data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,],
                      weights=weights_mah)

summary(did_nonKSNP_mah2.1)

vcovHC(did_nonKSNP_mah2.1, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_nonKSNP_mah2.1, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_NonKSNP_mah2.1 <- cbind(
  Estimate = coef(did_nonKSNP_mah2.1)
  , "RobustSE" = sandwich_se
  , z = (coef(did_nonKSNP_mah2.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_nonKSNP_mah2.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_nonKSNP_mah2.1) - q.val  * sandwich_se
  , UL = coef(did_nonKSNP_mah2.1) + q.val  * sandwich_se
)
rob_did_NonKSNP_mah2.1
rob_did_NonKSNP_mah2.1<-as.data.frame(rob_did_NonKSNP_mah2.1)
rob_did_NonKSNP_mah2.1$coeff_var<-rownames(rob_did_NonKSNP_mah2.1)
rob_did_NonKSNP_mah2.1$depvar<-"Non-KSNP"
rob_did_NonKSNP_mah2.1$did_type<-"Continuous"
rob_did_NonKSNP_mah2.1$mtch<-"Mahal."
rob_did_NonKSNP_mah2.1$term<-"Short-Term (2000-2007)"

###   Long-Term (2000-2016)   ##########################################################################################
did_nonKSNP_mah2.2<-lm(nonKSNP_AreaForLoss_Ha~did2+dstrds+pop+
                        factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                      data=yrforloss_KSNP_wts,
                      weights=weights_mah)

summary(did_nonKSNP_mah2.2)

vcovHC(did_nonKSNP_mah2.2, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_nonKSNP_mah2.2, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_NonKSNP_mah2.2 <- cbind(
  Estimate = coef(did_nonKSNP_mah2.2)
  , "RobustSE" = sandwich_se
  , z = (coef(did_nonKSNP_mah2.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_nonKSNP_mah2.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_nonKSNP_mah2.2) - q.val  * sandwich_se
  , UL = coef(did_nonKSNP_mah2.2) + q.val  * sandwich_se
)
rob_did_NonKSNP_mah2.2
rob_did_NonKSNP_mah2.2<-as.data.frame(rob_did_NonKSNP_mah2.2)
rob_did_NonKSNP_mah2.2$coeff_var<-rownames(rob_did_NonKSNP_mah2.2)
rob_did_NonKSNP_mah2.2$depvar<-"Non-KSNP"
rob_did_NonKSNP_mah2.2$did_type<-"Continuous"
rob_did_NonKSNP_mah2.2$mtch<-"Mahal."
rob_did_NonKSNP_mah2.2$term<-"Long-Term (2000-2016)"

###   Basic Difference in Difference for All Forest Cover Loss    ###########################################
###   Short-Term (2000-2010)   ##########################################################################################
did_All_mah2.1<-lm(All_AreaForLoss_Ha~did2+dstrds+pop+
                    factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                  data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,],
                  weights=weights_mah)

summary(did_All_mah2.1)

vcovHC(did_All_mah2.1, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_All_mah2.1, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_All_mah2.1 <- cbind(
  Estimate = coef(did_All_mah2.1)
  , "RobustSE" = sandwich_se
  , z = (coef(did_All_mah2.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_All_mah2.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_All_mah2.1) - q.val  * sandwich_se
  , UL = coef(did_All_mah2.1) + q.val  * sandwich_se
)
rob_did_All_mah2.1
rob_did_All_mah2.1<-as.data.frame(rob_did_All_mah2.1)
rob_did_All_mah2.1$coeff_var<-rownames(rob_did_All_mah2.1)
rob_did_All_mah2.1$depvar<-"All"
rob_did_All_mah2.1$did_type<-"Continuous"
rob_did_All_mah2.1$mtch<-"Mahal."
rob_did_All_mah2.1$term<-"Short-Term (2000-2007)"

###   Long-Term (2000-2016)   ##########################################################################################
did_All_mah2.2<-lm(All_AreaForLoss_Ha~did2+dstrds+pop+
                    factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                  data=yrforloss_KSNP_wts,
                  weights=weights_mah)

summary(did_All_mah2.2)

vcovHC(did_All_mah2.2, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_All_mah2.2, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_All_mah2.2 <- cbind(
  Estimate = coef(did_All_mah2.2)
  , "RobustSE" = sandwich_se
  , z = (coef(did_All_mah2.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_All_mah2.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_All_mah2.2) - q.val  * sandwich_se
  , UL = coef(did_All_mah2.2) + q.val  * sandwich_se
)
rob_did_All_mah2.2
rob_did_All_mah2.2<-as.data.frame(rob_did_All_mah2.2)
rob_did_All_mah2.2$coeff_var<-rownames(rob_did_All_mah2.2)
rob_did_All_mah2.2$depvar<-"All"
rob_did_All_mah2.2$did_type<-"Continuous"
rob_did_All_mah2.2$mtch<-"Mahal."
rob_did_All_mah2.2$term<-"Long-Term (2000-2016)"

###Analysis with Full Matching##################################################################################

###   Basic Difference in Difference for KSNP Forest Cover Loss    ##########################################
###   Short-Term (2000-2010)   ##########################################################################################
did_KSNP_full2.1<-lm(KSNP_AreaForLoss_Ha2~did2+dstrds+pop+
                      factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                    data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,],
                    weights=weights_full)

summary(did_KSNP_full2.1)

vcovHC(did_KSNP_full2.1, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_KSNP_full2.1, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_KSNP_full2.1 <- cbind(
  Estimate = coef(did_KSNP_full2.1)
  , "RobustSE" = sandwich_se
  , z = (coef(did_KSNP_full2.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_KSNP_full2.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_KSNP_full2.1) - q.val  * sandwich_se
  , UL = coef(did_KSNP_full2.1) + q.val  * sandwich_se
)
rob_did_KSNP_full2.1
rob_did_KSNP_full2.1<-as.data.frame(rob_did_KSNP_full2.1)
rob_did_KSNP_full2.1$coeff_var<-rownames(rob_did_KSNP_full2.1)
rob_did_KSNP_full2.1$depvar<-"KSNP"
rob_did_KSNP_full2.1$did_type<-"Continuous"
rob_did_KSNP_full2.1$mtch<-"Full"
rob_did_KSNP_full2.1$term<-"Short-Term (2000-2007)"

###   Long-Term (2000-2016)   ##########################################################################################
did_KSNP_full2.2<-lm(KSNP_AreaForLoss_Ha2~did2+dstrds+pop+
                      factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                    data=yrforloss_KSNP_wts,
                    weights=weights_full)

summary(did_KSNP_full2.2)

vcovHC(did_KSNP_full2.2, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_KSNP_full2.2, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_KSNP_full2.2 <- cbind(
  Estimate = coef(did_KSNP_full2.2)
  , "RobustSE" = sandwich_se
  , z = (coef(did_KSNP_full2.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_KSNP_full2.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_KSNP_full2.2) - q.val  * sandwich_se
  , UL = coef(did_KSNP_full2.2) + q.val  * sandwich_se
)
rob_did_KSNP_full2.2
rob_did_KSNP_full2.2<-as.data.frame(rob_did_KSNP_full2.2)
rob_did_KSNP_full2.2$coeff_var<-rownames(rob_did_KSNP_full2.2)
rob_did_KSNP_full2.2$depvar<-"KSNP"
rob_did_KSNP_full2.2$did_type<-"Continuous"
rob_did_KSNP_full2.2$mtch<-"Full"
rob_did_KSNP_full2.2$term<-"Long-Term (2000-2016)"

###   Basic Difference in Difference for Non-KSNP Forest Cover Loss    ######################################
###   Short-Term (2000-2010)   ##########################################################################################
did_nonKSNP_full2.1<-lm(nonKSNP_AreaForLoss_Ha~did2+dstrds+pop+
                         factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                       data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,],
                       weights=weights_full)

summary(did_nonKSNP_full2.1)

vcovHC(did_nonKSNP_full2.1, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_nonKSNP_full2.1, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_NonKSNP_full2.1 <- cbind(
  Estimate = coef(did_nonKSNP_full2.1)
  , "RobustSE" = sandwich_se
  , z = (coef(did_nonKSNP_full2.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_nonKSNP_full2.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_nonKSNP_full2.1) - q.val  * sandwich_se
  , UL = coef(did_nonKSNP_full2.1) + q.val  * sandwich_se
)
rob_did_NonKSNP_full2.1
rob_did_NonKSNP_full2.1<-as.data.frame(rob_did_NonKSNP_full2.1)
rob_did_NonKSNP_full2.1$coeff_var<-rownames(rob_did_NonKSNP_full2.1)
rob_did_NonKSNP_full2.1$depvar<-"Non-KSNP"
rob_did_NonKSNP_full2.1$did_type<-"Continuous"
rob_did_NonKSNP_full2.1$mtch<-"Full"
rob_did_NonKSNP_full2.1$term<-"Short-Term (2000-2007)"

###   Long-Term (2000-2016)   ##########################################################################################
did_nonKSNP_full2.2<-lm(nonKSNP_AreaForLoss_Ha~did2+dstrds+pop+
                         factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                       data=yrforloss_KSNP_wts,
                       weights=weights_full)

summary(did_nonKSNP_full2.2)

vcovHC(did_nonKSNP_full2.2, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_nonKSNP_full2.2, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_NonKSNP_full2.2 <- cbind(
  Estimate = coef(did_nonKSNP_full2.2)
  , "RobustSE" = sandwich_se
  , z = (coef(did_nonKSNP_full2.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_nonKSNP_full2.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_nonKSNP_full2.2) - q.val  * sandwich_se
  , UL = coef(did_nonKSNP_full2.2) + q.val  * sandwich_se
)
rob_did_NonKSNP_full2.2
rob_did_NonKSNP_full2.2<-as.data.frame(rob_did_NonKSNP_full2.2)
rob_did_NonKSNP_full2.2$coeff_var<-rownames(rob_did_NonKSNP_full2.2)
rob_did_NonKSNP_full2.2$depvar<-"Non-KSNP"
rob_did_NonKSNP_full2.2$did_type<-"Continuous"
rob_did_NonKSNP_full2.2$mtch<-"Full"
rob_did_NonKSNP_full2.2$term<-"Long-Term (2000-2016)"

###   Basic Difference in Difference for All Forest Cover Loss    ###########################################
###   Short-Term (2000-2010)   ##########################################################################################
did_All_full2.1<-lm(All_AreaForLoss_Ha~did2+dstrds+pop+
                     factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                   data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,],
                   weights=weights_full)

summary(did_All_full2.1)

vcovHC(did_All_full2.1, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_All_full2.1, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_All_full2.1 <- cbind(
  Estimate = coef(did_All_full2.1)
  , "RobustSE" = sandwich_se
  , z = (coef(did_All_full2.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_All_full2.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_All_full2.1) - q.val  * sandwich_se
  , UL = coef(did_All_full2.1) + q.val  * sandwich_se
)
rob_did_All_full2.1
rob_did_All_full2.1<-as.data.frame(rob_did_All_full2.1)
rob_did_All_full2.1$coeff_var<-rownames(rob_did_All_full2.1)
rob_did_All_full2.1$depvar<-"All"
rob_did_All_full2.1$did_type<-"Continuous"
rob_did_All_full2.1$mtch<-"Full"
rob_did_All_full2.1$term<-"Short-Term (2000-2007)"

###   Long-Term (2000-2016)   ##########################################################################################
did_All_full2.2<-lm(All_AreaForLoss_Ha~did2+dstrds+pop+
                     factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                   data=yrforloss_KSNP_wts,
                   weights=weights_full)

summary(did_All_full2.2)

vcovHC(did_All_full2.2, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_All_full2.2, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_All_full2.2 <- cbind(
  Estimate = coef(did_All_full2.2)
  , "RobustSE" = sandwich_se
  , z = (coef(did_All_full2.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_All_full2.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_All_full2.2) - q.val  * sandwich_se
  , UL = coef(did_All_full2.2) + q.val  * sandwich_se
)
rob_did_All_full2.2
rob_did_All_full2.2<-as.data.frame(rob_did_All_full2.2)
rob_did_All_full2.2$coeff_var<-rownames(rob_did_All_full2.2)
rob_did_All_full2.2$depvar<-"All"
rob_did_All_full2.2$did_type<-"Continuous"
rob_did_All_full2.2$mtch<-"Full"
rob_did_All_full2.2$term<-"Long-Term (2000-2016)"

###Analysis with Genetic Matching##################################################################################

###   Basic Difference in Difference for KSNP Forest Cover Loss    ##########################################
###   Short-Term (2000-2010)   ##########################################################################################
did_KSNP_gen2.1<-lm(KSNP_AreaForLoss_Ha2~did2+dstrds+pop+
                     factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                   data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,],
                   weights=weights_gen)

summary(did_KSNP_gen2.1)

vcovHC(did_KSNP_gen2.1, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_KSNP_gen2.1, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_KSNP_gen2.1 <- cbind(
  Estimate = coef(did_KSNP_gen2.1)
  , "RobustSE" = sandwich_se
  , z = (coef(did_KSNP_gen2.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_KSNP_gen2.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_KSNP_gen2.1) - q.val  * sandwich_se
  , UL = coef(did_KSNP_gen2.1) + q.val  * sandwich_se
)
rob_did_KSNP_gen2.1
rob_did_KSNP_gen2.1<-as.data.frame(rob_did_KSNP_gen2.1)
rob_did_KSNP_gen2.1$coeff_var<-rownames(rob_did_KSNP_gen2.1)
rob_did_KSNP_gen2.1$depvar<-"KSNP"
rob_did_KSNP_gen2.1$did_type<-"Continuous"
rob_did_KSNP_gen2.1$mtch<-"Genetic"
rob_did_KSNP_gen2.1$term<-"Short-Term (2000-2007)"

###   Long-Term (2000-2016)   ##########################################################################################
did_KSNP_gen2.2<-lm(KSNP_AreaForLoss_Ha2~did2+dstrds+pop+
                     factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                   data=yrforloss_KSNP_wts,
                   weights=weights_gen)

summary(did_KSNP_gen2.2)

vcovHC(did_KSNP_gen2.2, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_KSNP_gen2.2, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_KSNP_gen2.2 <- cbind(
  Estimate = coef(did_KSNP_gen2.2)
  , "RobustSE" = sandwich_se
  , z = (coef(did_KSNP_gen2.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_KSNP_gen2.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_KSNP_gen2.2) - q.val  * sandwich_se
  , UL = coef(did_KSNP_gen2.2) + q.val  * sandwich_se
)
rob_did_KSNP_gen2.2
rob_did_KSNP_gen2.2<-as.data.frame(rob_did_KSNP_gen2.2)
rob_did_KSNP_gen2.2$coeff_var<-rownames(rob_did_KSNP_gen2.2)
rob_did_KSNP_gen2.2$depvar<-"KSNP"
rob_did_KSNP_gen2.2$did_type<-"Continuous"
rob_did_KSNP_gen2.2$mtch<-"Genetic"
rob_did_KSNP_gen2.2$term<-"Long-Term (2000-2016)"

###   Basic Difference in Difference for Non-KSNP Forest Cover Loss    ######################################
###   Short-Term (2000-2010)   ##########################################################################################
did_nonKSNP_gen2.1<-lm(nonKSNP_AreaForLoss_Ha~did2+dstrds+pop+
                        factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                      data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,],
                      weights=weights_gen)

summary(did_nonKSNP_gen2.1)

vcovHC(did_nonKSNP_gen2.1, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_nonKSNP_gen2.1, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_NonKSNP_gen2.1 <- cbind(
  Estimate = coef(did_nonKSNP_gen2.1)
  , "RobustSE" = sandwich_se
  , z = (coef(did_nonKSNP_gen2.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_nonKSNP_gen2.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_nonKSNP_gen2.1) - q.val  * sandwich_se
  , UL = coef(did_nonKSNP_gen2.1) + q.val  * sandwich_se
)
rob_did_NonKSNP_gen2.1
rob_did_NonKSNP_gen2.1<-as.data.frame(rob_did_NonKSNP_gen2.1)
rob_did_NonKSNP_gen2.1$coeff_var<-rownames(rob_did_NonKSNP_gen2.1)
rob_did_NonKSNP_gen2.1$depvar<-"Non-KSNP"
rob_did_NonKSNP_gen2.1$did_type<-"Continuous"
rob_did_NonKSNP_gen2.1$mtch<-"Genetic"
rob_did_NonKSNP_gen2.1$term<-"Short-Term (2000-2007)"

###   Long-Term (2000-2016)   ##########################################################################################
did_nonKSNP_gen2.2<-lm(nonKSNP_AreaForLoss_Ha~did2+dstrds+pop+
                        factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                      data=yrforloss_KSNP_wts,
                      weights=weights_gen)

summary(did_nonKSNP_gen2.2)

vcovHC(did_nonKSNP_gen2.2, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_nonKSNP_gen2.2, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_NonKSNP_gen2.2 <- cbind(
  Estimate = coef(did_nonKSNP_gen2.2)
  , "RobustSE" = sandwich_se
  , z = (coef(did_nonKSNP_gen2.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_nonKSNP_gen2.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_nonKSNP_gen2.2) - q.val  * sandwich_se
  , UL = coef(did_nonKSNP_gen2.2) + q.val  * sandwich_se
)
rob_did_NonKSNP_gen2.2
rob_did_NonKSNP_gen2.2<-as.data.frame(rob_did_NonKSNP_gen2.2)
rob_did_NonKSNP_gen2.2$coeff_var<-rownames(rob_did_NonKSNP_gen2.2)
rob_did_NonKSNP_gen2.2$depvar<-"Non-KSNP"
rob_did_NonKSNP_gen2.2$did_type<-"Continuous"
rob_did_NonKSNP_gen2.2$mtch<-"Genetic"
rob_did_NonKSNP_gen2.2$term<-"Long-Term (2000-2016)"

###   Basic Difference in Difference for All Forest Cover Loss    ###########################################
###   Short-Term (2000-2010)   ##########################################################################################
did_All_gen2.1<-lm(All_AreaForLoss_Ha~did2+dstrds+pop+
                    factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                  data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,],
                  weights=weights_gen)

summary(did_All_gen2.1)

vcovHC(did_All_gen2.1, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_All_gen2.1, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_All_gen2.1 <- cbind(
  Estimate = coef(did_All_gen2.1)
  , "RobustSE" = sandwich_se
  , z = (coef(did_All_gen2.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_All_gen2.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_All_gen2.1) - q.val  * sandwich_se
  , UL = coef(did_All_gen2.1) + q.val  * sandwich_se
)
rob_did_All_gen2.1
rob_did_All_gen2.1<-as.data.frame(rob_did_All_gen2.1)
rob_did_All_gen2.1$coeff_var<-rownames(rob_did_All_gen2.1)
rob_did_All_gen2.1$depvar<-"All"
rob_did_All_gen2.1$did_type<-"Continuous"
rob_did_All_gen2.1$mtch<-"Genetic"
rob_did_All_gen2.1$term<-"Short-Term (2000-2007)"

###   Long-Term (2000-2016)   ##########################################################################################
did_All_gen2.2<-lm(All_AreaForLoss_Ha~did2+dstrds+pop+
                    factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                  data=yrforloss_KSNP_wts,
                  weights=weights_gen)

summary(did_All_gen2.2)

vcovHC(did_All_gen2.2, type="HC0", cluster="ID2003_d")
sandwich_se<-diag(vcovHC(did_All_gen2.2, type="HC0", cluster="ID2003_d"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_All_gen2.2 <- cbind(
  Estimate = coef(did_All_gen2.2)
  , "RobustSE" = sandwich_se
  , z = (coef(did_All_gen2.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_All_gen2.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_All_gen2.2) - q.val  * sandwich_se
  , UL = coef(did_All_gen2.2) + q.val  * sandwich_se
)
rob_did_All_gen2.2
rob_did_All_gen2.2<-as.data.frame(rob_did_All_gen2.2)
rob_did_All_gen2.2$coeff_var<-rownames(rob_did_All_gen2.2)
rob_did_All_gen2.2$depvar<-"All"
rob_did_All_gen2.2$did_type<-"Continuous"
rob_did_All_gen2.2$mtch<-"Genetic"
rob_did_All_gen2.2$term<-"Long-Term (2000-2016)"

###Binding all output datasets################################################################################

did_model_output<-rbind(rob_basic_did_KSNP.1, rob_basic_did_NonKSNP.1, rob_basic_did_All.1,
                        rob_basic_did_KSNP.2, rob_basic_did_NonKSNP.2, rob_basic_did_All.2,
                        rob_basic_did_KSNP2.1, rob_basic_did_NonKSNP2.1, rob_basic_did_All2.1,
                        rob_basic_did_KSNP2.2, rob_basic_did_NonKSNP2.2, rob_basic_did_All2.2,
                        
                        rob_did_KSNP_mah.1, rob_did_NonKSNP_mah.1, rob_did_All_mah.1,
                        rob_did_KSNP_mah.2, rob_did_NonKSNP_mah.2, rob_did_All_mah.2,
                        rob_did_KSNP_full.1, rob_did_NonKSNP_full.1, rob_did_All_full.1,
                        rob_did_KSNP_full.2, rob_did_NonKSNP_full.2, rob_did_All_full.2,
                        rob_did_KSNP_gen.1, rob_did_NonKSNP_gen.1, rob_did_All_gen.1,
                        rob_did_KSNP_gen.2, rob_did_NonKSNP_gen.2, rob_did_All_gen.2,
                        
                        rob_did_KSNP_mah2.1, rob_did_NonKSNP_mah2.1, rob_did_All_mah2.1,
                        rob_did_KSNP_mah2.2, rob_did_NonKSNP_mah2.2, rob_did_All_mah2.2,
                        rob_did_KSNP_full2.1, rob_did_NonKSNP_full2.1, rob_did_All_full2.1,
                        rob_did_KSNP_full2.2, rob_did_NonKSNP_full2.2, rob_did_All_full2.2,
                        rob_did_KSNP_gen2.1, rob_did_NonKSNP_gen2.1, rob_did_All_gen2.1,
                        rob_did_KSNP_gen2.2, rob_did_NonKSNP_gen2.2, rob_did_All_gen2.2)

##Adding variable name column
table(did_model_output$coeff_va)
#subsetting data
did_plot_df<-did_model_output %>%
  filter(coeff_var=="factor(did1)1" | coeff_var == "did2")
#View(did_plot_df)

str(did_plot_df)

rm(basic_did_KSNP.1, basic_did_KSNP.2, basic_did_KSNP2.1, basic_did_KSNP2.2,
   basic_did_NonKSNP.1, basic_did_NonKSNP.2, basic_did_NonKSNP2.1, basic_did_NonKSNP2.2,
   basic_did_All.1, basic_did_All.2, basic_did_All2.1, basic_did_All2.2,
   
   did_KSNP_full.1, did_KSNP_full.2, did_KSNP_full2.1, did_KSNP_full2.2,
   did_KSNP_gen.1, did_KSNP_gen.2, did_KSNP_gen2.1, did_KSNP_gen2.2,
   did_KSNP_mah.1, did_KSNP_mah.2, did_KSNP_mah2.1, did_KSNP_mah2.2,
   did_KSNP_full.1, did_KSNP_full.2, did_KSNP_full2.1, did_KSNP_full2.2,
   did_KSNP_gen.1, did_KSNP_gen.2, did_KSNP_gen2.1, did_KSNP_gen2.2,
   
   did_NonKSNP_mah.1, did_NonKSNP_mah.2, did_NonKSNP_mah2.1, did_NonKSNP_mah2.2,
   did_nonKSNP_full.1, did_nonKSNP_full.2, did_nonKSNP_full2.1, did_nonKSNP_full2.2,
   did_nonKSNP_gen.1, did_nonKSNP_gen.2, did_nonKSNP_gen2.1, did_nonKSNP_gen2.2,
   
   did_All_mah.1, did_All_mah.2, did_All_mah2.1, did_All_mah2.2,
   did_All_full.1, did_All_full.2, did_All_full2.1, did_All_full2.2,
   did_All_gen.1, did_All_gen.2, did_All_gen2.1, did_All_gen2.2,
   
   rob_basic_did_KSNP.1, rob_basic_did_NonKSNP.1, rob_basic_did_All.1,
   rob_basic_did_KSNP.2, rob_basic_did_NonKSNP.2, rob_basic_did_All.2,
   rob_basic_did_KSNP2.1, rob_basic_did_NonKSNP2.1, rob_basic_did_All2.1,
   rob_basic_did_KSNP2.2, rob_basic_did_NonKSNP2.2, rob_basic_did_All2.2,
   
   rob_did_KSNP_mah.1, rob_did_NonKSNP_mah.1, rob_did_All_mah.1,
   rob_did_KSNP_mah.2, rob_did_NonKSNP_mah.2, rob_did_All_mah.2,
   rob_did_KSNP_full.1, rob_did_NonKSNP_full.1, rob_did_All_full.1,
   rob_did_KSNP_full.2, rob_did_NonKSNP_full.2, rob_did_All_full.2,
   rob_did_KSNP_gen.1, rob_did_NonKSNP_gen.1, rob_did_All_gen.1,
   rob_did_KSNP_gen.2, rob_did_NonKSNP_gen.2, rob_did_All_gen.2,
   
   rob_did_KSNP_mah2.1, rob_did_NonKSNP_mah2.1, rob_did_All_mah2.1,
   rob_did_KSNP_mah2.2, rob_did_NonKSNP_mah2.2, rob_did_All_mah2.2,
   rob_did_KSNP_full2.1, rob_did_NonKSNP_full2.1, rob_did_All_full2.1,
   rob_did_KSNP_full2.2, rob_did_NonKSNP_full2.2, rob_did_All_full2.2,
   rob_did_KSNP_gen2.1, rob_did_NonKSNP_gen2.1, rob_did_All_gen2.1,
   rob_did_KSNP_gen2.2, rob_did_NonKSNP_gen2.2, rob_did_All_gen2.2)

##################################################################################################################
###Investigating the non-payment of the second round###
##################################################################################################################

###   Dual Treatment Effect for Difference in Difference for KSNP Forest Loss Area   ########################
###   No Weights   ##########################################################################################

yrforloss_KSNP_wts$year_vca2<-0
yrforloss_KSNP_wts$year_vca2[yrforloss_KSNP_wts$year>2003]<-1

basic_did_KSNP3.1<-lm(KSNP_AreaForLoss_Ha2~factor(did3)+factor(did4)+dstrds+pop+
                          factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                        data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,])

summary(basic_did_KSNP3.1)

vcovHC(basic_did_KSNP3.1)
sandwich_se<-diag(vcovHC(basic_did_KSNP3.1, type="HC"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_basic_did_KSNP3.1 <- cbind(
  Estimate = coef(basic_did_KSNP3.1)
  , "RobustSE" = sandwich_se
  , z = (coef(basic_did_KSNP3.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(basic_did_KSNP3.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(basic_did_KSNP3.1) - q.val  * sandwich_se
  , UL = coef(basic_did_KSNP3.1) + q.val  * sandwich_se
)
#View(rob_basic_did_KSNP3.1)
rob_basic_did_KSNP3.1<-as.data.frame(rob_basic_did_KSNP3.1)
rob_basic_did_KSNP3.1$coeff_var<-rownames(rob_basic_did_KSNP3.1)
rob_basic_did_KSNP3.1$depvar<-"KSNP"
rob_basic_did_KSNP3.1$did_type<-"Dual"
rob_basic_did_KSNP3.1$mtch<-"None"
rob_basic_did_KSNP3.1$term<-"Short-Term (2000-2007)"

###   Dual Treatment Effect for Difference in Difference for KSNP Forest Loss Area with Mahalanobis weights  ###
did_KSNP_mah3.1<-lm(KSNP_AreaForLoss_Ha2~factor(did3)+factor(did4)+dstrds+pop+
                             factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                    data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,],
                           weights=weights_mah)

summary(did_KSNP_mah3.1)

vcovHC(did_KSNP_mah3.1, type="HC")
sandwich_se<-diag(vcovHC(did_KSNP_mah3.1, type="HC"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_KSNP_mah3.1 <- cbind(
  Estimate = coef(did_KSNP_mah3.1)
  , "RobustSE" = sandwich_se
  , z = (coef(did_KSNP_mah3.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_KSNP_mah3.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_KSNP_mah3.1) - q.val  * sandwich_se
  , UL = coef(did_KSNP_mah3.1) + q.val  * sandwich_se
)
#View(rob_did_KSNP_mah3.1)
rob_did_KSNP_mah3.1<-as.data.frame(rob_did_KSNP_mah3.1)
rob_did_KSNP_mah3.1$coeff_var<-rownames(rob_did_KSNP_mah3.1)
rob_did_KSNP_mah3.1$depvar<-"KSNP"
rob_did_KSNP_mah3.1$did_type<-"Dual"
rob_did_KSNP_mah3.1$mtch<-"Mahal."
rob_did_KSNP_mah3.1$term<-"Short-Term (2000-2007)"

###   Dual Treatment Effect for Difference in Difference for KSNP Forest Loss Area with Full weights  ###
did_KSNP_full3.1<-lm(KSNP_AreaForLoss_Ha2~factor(did3)+factor(did4)+dstrds+pop+
                             factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                     data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,],
                           weights=weights_full)

summary(did_KSNP_full3.1)

vcovHC(did_KSNP_full3.1, type="HC")
sandwich_se<-diag(vcovHC(did_KSNP_full3.1, type="HC"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_KSNP_full3.1 <- cbind(
  Estimate = coef(did_KSNP_full3.1)
  , "RobustSE" = sandwich_se
  , z = (coef(did_KSNP_full3.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_KSNP_full3.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_KSNP_full3.1) - q.val  * sandwich_se
  , UL = coef(did_KSNP_full3.1) + q.val  * sandwich_se
)
#View(rob_did_KSNP_full3.1)
rob_did_KSNP_full3.1<-as.data.frame(rob_did_KSNP_full3.1)
rob_did_KSNP_full3.1$coeff_var<-rownames(rob_did_KSNP_full3.1)
rob_did_KSNP_full3.1$depvar<-"KSNP"
rob_did_KSNP_full3.1$did_type<-"Dual"
rob_did_KSNP_full3.1$mtch<-"Full"
rob_did_KSNP_full3.1$term<-"Short-Term (2000-2007)"

###   Dual Treatment Effect for Difference in Difference for KSNP Forest Loss Area with Full weights  ###
did_KSNP_gen3.1<-lm(KSNP_AreaForLoss_Ha2~factor(did3)+factor(did4)+dstrds+pop+
                             factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                    data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2008,],
                           weights=weights_gen)

summary(did_KSNP_gen3.1)

vcovHC(did_KSNP_gen3.1, type="HC")
sandwich_se<-diag(vcovHC(did_KSNP_gen3.1, type="HC"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_KSNP_gen3.1 <- cbind(
  Estimate = coef(did_KSNP_gen3.1)
  , "RobustSE" = sandwich_se
  , z = (coef(did_KSNP_gen3.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_KSNP_gen3.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_KSNP_gen3.1) - q.val  * sandwich_se
  , UL = coef(did_KSNP_gen3.1) + q.val  * sandwich_se
)
View(rob_did_KSNP_gen3.1)
rob_did_KSNP_gen3.1<-as.data.frame(rob_did_KSNP_gen3.1)
rob_did_KSNP_gen3.1$coeff_var<-rownames(rob_did_KSNP_gen3.1)
rob_did_KSNP_gen3.1$depvar<-"KSNP"
rob_did_KSNP_gen3.1$did_type<-"Dual"
rob_did_KSNP_gen3.1$mtch<-"Genetic"
rob_did_KSNP_gen3.1$term<-"Short-Term (2000-2007)"


###   Dual Continuous Treatment Effect for Difference in Difference for KSNP Forest Loss Area   ########################
###   No Weights   ##########################################################################################

yrforloss_KSNP_wts$year_vca2<-0
yrforloss_KSNP_wts$year_vca2[yrforloss_KSNP_wts$year>2003]<-1

basic_did_nonKSNP3.1<-lm(KSNP_AreaForLoss_Ha2~did5+did6+dstrds+pop+
                           factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                         data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2005,])

summary(basic_did_nonKSNP3.1)

vcovHC(basic_did_nonKSNP3.1, type="HC")
sandwich_se<-diag(vcovHC(basic_did_nonKSNP3.1, type="HC"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_basic_did_nonKSNP3.1 <- cbind(
  Estimate = coef(basic_did_nonKSNP3.1)
  , "RobustSE" = sandwich_se
  , z = (coef(basic_did_nonKSNP3.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(basic_did_nonKSNP3.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(basic_did_nonKSNP3.1) - q.val  * sandwich_se
  , UL = coef(basic_did_nonKSNP3.1) + q.val  * sandwich_se
)
#View(rob_basic_did_nonKSNP3.1)
rob_basic_did_nonKSNP3.1<-as.data.frame(rob_basic_did_nonKSNP3.1)
rob_basic_did_nonKSNP3.1$coeff_var<-rownames(rob_basic_did_nonKSNP3.1)
rob_basic_did_nonKSNP3.1$depvar<-"KSNP"
rob_basic_did_nonKSNP3.1$did_type<-"Dual-Cont"
rob_basic_did_nonKSNP3.1$mtch<-"None"
rob_basic_did_nonKSNP3.1$term<-"Short-Term (2000-2007)"

###   Dual Treatment Effect for Difference in Difference for KSNP Forest Loss Area with Mahalanobis weights  ###
did_nonKSNP_mah3.1<-lm(KSNP_AreaForLoss_Ha2~did5+did6+dstrds+pop+
                         factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                       data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2005,],
                       weights=weights_mah)

summary(did_nonKSNP_mah3.1)

vcovHC(did_nonKSNP_mah3.1, type="HC")
sandwich_se<-diag(vcovHC(did_nonKSNP_mah3.1, type="HC"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_nonKSNP_mah3.1 <- cbind(
  Estimate = coef(did_nonKSNP_mah3.1)
  , "RobustSE" = sandwich_se
  , z = (coef(did_nonKSNP_mah3.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_nonKSNP_mah3.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_nonKSNP_mah3.1) - q.val  * sandwich_se
  , UL = coef(did_nonKSNP_mah3.1) + q.val  * sandwich_se
)
#View(rob_did_nonKSNP_mah3.1)
rob_did_nonKSNP_mah3.1<-as.data.frame(rob_did_nonKSNP_mah3.1)
rob_did_nonKSNP_mah3.1$coeff_var<-rownames(rob_did_nonKSNP_mah3.1)
rob_did_nonKSNP_mah3.1$depvar<-"KSNP"
rob_did_nonKSNP_mah3.1$did_type<-"Dual-Cont"
rob_did_nonKSNP_mah3.1$mtch<-"Mahal."
rob_did_nonKSNP_mah3.1$term<-"Short-Term (2000-2007)"

###   Dual Treatment Effect for Difference in Difference for KSNP Forest Loss Area with Full weights  ###
did_nonKSNP_full3.1<-lm(KSNP_AreaForLoss_Ha2~did5+did6+dstrds+pop+
                          factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                        data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2005,],
                        weights=weights_full)

summary(did_nonKSNP_full3.1)

vcovHC(did_nonKSNP_full3.1, type="HC")
sandwich_se<-diag(vcovHC(did_nonKSNP_full3.1, type="HC"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_nonKSNP_full3.1 <- cbind(
  Estimate = coef(did_nonKSNP_full3.1)
  , "RobustSE" = sandwich_se
  , z = (coef(did_nonKSNP_full3.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_nonKSNP_full3.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_nonKSNP_full3.1) - q.val  * sandwich_se
  , UL = coef(did_nonKSNP_full3.1) + q.val  * sandwich_se
)
#View(rob_did_nonKSNP_full3.1)
rob_did_nonKSNP_full3.1<-as.data.frame(rob_did_nonKSNP_full3.1)
rob_did_nonKSNP_full3.1$coeff_var<-rownames(rob_did_nonKSNP_full3.1)
rob_did_nonKSNP_full3.1$depvar<-"KSNP"
rob_did_nonKSNP_full3.1$did_type<-"Dual-Cont"
rob_did_nonKSNP_full3.1$mtch<-"Full"
rob_did_nonKSNP_full3.1$term<-"Short-Term (2000-2007)"

###   Dual Treatment Effect for Difference in Difference for KSNP Forest Loss Area with Full weights  ###
did_nonKSNP_gen3.1<-lm(KSNP_AreaForLoss_Ha2~did5+did6+dstrds+pop+
                         factor(year) + factor(year)*factor(distID2003_d) + factor(ID2003_d),
                       data=yrforloss_KSNP_wts[yrforloss_KSNP_wts$year<2005,],
                       weights=weights_gen)

summary(did_nonKSNP_gen3.1)

vcovHC(did_nonKSNP_gen3.1, type="HC")
sandwich_se<-diag(vcovHC(did_nonKSNP_gen3.1, type="HC"))^.5
sandwich_se

q.val <- qnorm(0.975)

rob_did_nonKSNP_gen3.1 <- cbind(
  Estimate = coef(did_nonKSNP_gen3.1)
  , "RobustSE" = sandwich_se
  , z = (coef(did_nonKSNP_gen3.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(did_nonKSNP_gen3.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(did_nonKSNP_gen3.1) - q.val  * sandwich_se
  , UL = coef(did_nonKSNP_gen3.1) + q.val  * sandwich_se
)
rob_did_nonKSNP_gen3.1
rob_did_nonKSNP_gen3.1<-as.data.frame(rob_did_nonKSNP_gen3.1)
rob_did_nonKSNP_gen3.1$coeff_var<-rownames(rob_did_nonKSNP_gen3.1)
rob_did_nonKSNP_gen3.1$depvar<-"KSNP"
rob_did_nonKSNP_gen3.1$did_type<-"Dual-Cont"
rob_did_nonKSNP_gen3.1$mtch<-"Genetic"
rob_did_nonKSNP_gen3.1$term<-"Short-Term (2000-2007)"

###Combining model output###

dualdid_model_output<-rbind(rob_basic_did_KSNP3.1, rob_did_KSNP_mah3.1, rob_did_KSNP_full3.1, rob_did_KSNP_gen3.1,
                            rob_basic_did_nonKSNP3.1, rob_did_nonKSNP_mah3.1, rob_did_nonKSNP_full3.1, rob_did_nonKSNP_gen3.1)
                            
rm(basic_did_KSNP3.1, did_KSNP_mah3.1, did_KSNP_full3.1, did_KSNP_gen3.1,
   basic_did_nonKSNP3.1, did_nonKSNP_mah3.1, did_nonKSNP_full3.1, did_nonKSNP_gen3.1,
   rob_basic_did_KSNP3.1, rob_did_KSNP_mah3.1, rob_did_KSNP_full3.1, rob_did_KSNP_gen3.1,
   rob_basic_did_nonKSNP3.1, rob_did_nonKSNP_mah3.1, rob_did_nonKSNP_full3.1, rob_did_nonKSNP_gen3.1)


#################################################################################################################
###Visualizing Model Outputs###
#################################################################################################################

###Basic Difference-in-Difference Estimator###

ksnp_did_plots1<-did_plot_df %>%
  filter(coeff_var=="factor(did1)1" & did_type=="Dichot.") %>%
  ggplot(aes(x=depvar, 
             y=Estimate, 
             ymax=UL, 
             ymin=LL,
             fill=fct_rev(mtch)))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(width=.25, position=position_dodge(0.9))+
  #scale_fill_discrete(name="Matching\nTechnique", breaks=c("None", "Full", "Prop. Score"), 
  #                   labels=c("None", "Full", "Prop. Score"), values=tol3qualitative)+
  #scale_fill_manual(name="Matching\nTechnique", breaks=c("All", "Full", "Prop. Score"), 
  #                  labels=c("None", "Full", "Prop. Score"), values=tol3qualitative)+
  labs(x = "Forest-Change Type", y="Difference in Area of Tree-Cover Loss (Ha)")+
  #coord_cartesian(ylim = c(0,-25))+
  #ggtitle("Difference in Forest Cover Change\nBetween VCA and non-VCA Villages")+
  #geom_text(aes(y=(-5),label=stars),position=position_dodge(0.9), vjust="bottom")+
  theme_bw()+
  facet_wrap(~fct_rev(term))
ksnp_did_plots1

##All Coefficients for dichotomous treatment variable
ksnp_did_plots2<-did_plot_df %>%
  filter(coeff_var=="factor(did1)1" & did_type=="Dichot.") %>%
  ggplot(aes(x=fct_rev(depvar), 
             y=Estimate, 
             ymax=UL, 
             ymin=LL,
             fill=fct_rev(mtch),
             color=fct_rev(mtch)))+
  geom_pointrange(aes(ymin=(LL), ymax=(UL), group=(mtch), color=fct_rev(mtch)),
                  position=position_dodge(width = .5)) + 
  #scale_alpha_continuous(range = c(.3, 1), guide='none')+
  #scale_y_continuous(limits=c(-0.03,0.03))+
  geom_hline(yintercept=0, color="black") + 
  theme(text = element_text(size=10)) +
  labs(fill = "Matching", color = "Matching", y="Average additional tree-cover loss in VCA villages (Ha)", x=NULL) +
  #xlab(NULL) +
  theme_bw() +
  coord_flip()+
  facet_wrap(term~.)
ksnp_did_plots2

ggsave(file="C:/Users/JTErbaugh/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/plot_dich_dist_rds_07.png",
       plot=ksnp_did_plots2,
       dpi=600, width=6.5, height=4, unit="in")

###Continuous Difference-in-Difference Estimator###
##All Coefficients
ksnp_did_plots3<-did_plot_df %>%
  filter(coeff_var=="did2" & did_type=="Continuous") %>%
  ggplot(aes(x=fct_rev(depvar), 
             y=Estimate*8936600, 
             ymax=UL, 
             ymin=LL,
             fill=fct_rev(mtch),
             color=fct_rev(mtch)))+
  geom_pointrange(aes(ymin=(LL*8936600), ymax=(UL*8936600), group=(mtch), color=fct_rev(mtch)),
                  position=position_dodge(width = .5)) + 
  #scale_alpha_continuous(range = c(.3, 1), guide='none')+
  #scale_y_continuous(limits=c(-0.03,0.03))+
  geom_hline(yintercept=0, color="black") + 
  theme(text = element_text(size=10)) +
  labs(fill = "Matching", color = "Matching", y="Average additional tree-cover loss per $1,000 (Ha)", x=NULL) +
  theme_bw() +
  coord_flip()+
facet_grid(~term)
ksnp_did_plots3
##8,936,600 is the 1/1/2003 rate for 1,000 USD per Oanda currency conversion website

ggsave(file="C:/Users/JTErbaugh/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/plot_cont_dist_rds_07.png",
       plot=ksnp_did_plots3,
       dpi=600, width=6.5, height=4, unit="in") 

##Combining the two plots into one##
ksnp_did_both<-ggarrange(ksnp_did_plots2, ksnp_did_plots3, nrow=2, common.legend = T)
ksnp_did_both<-annotate_figure(ksnp_did_both,left=text_grob("Tree-Cover Type", rot=90))
ksnp_did_both

ggsave(file="C:/Users/JTErbaugh/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/ksnp_bothplots.png",
       plot=ksnp_did_both,
       dpi=600, width=6.5, height=4, unit="in")

###Dual Difference-in-Difference Estimator##################################################################
##Only Short-Term (2000-2004)
ksnp_did_plots4<-dualdid_model_output %>%
  filter((coeff_var=="factor(did3)1" | coeff_var=="factor(did4)1") & 
           depvar=="KSNP") %>%
  ggplot(aes(x=fct_rev(depvar), 
             y=Estimate, 
             ymax=UL, 
             ymin=LL,
             fill=fct_rev(mtch)))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(width=.25, position=position_dodge(0.9))+
  #scale_fill_discrete(name="Matching\nTechnique", breaks=c("None", "Full", "Prop. Score"), 
  #                   labels=c("None", "Full", "Prop. Score"), values=tol3qualitative)+
  #scale_fill_manual(name="Matching\nTechnique", breaks=c("All", "Full", "Prop. Score"), 
  #                  labels=c("None", "Full", "Prop. Score"), values=tol3qualitative)+
  labs(x = "Forest-Change Type", y="Average Additional Tree-Cover Loss in VCA Villages (Ha)")+
  #coord_cartesian(ylim = c(0,-25))+
  #ggtitle("Difference in Forest Cover Change\nBetween VCA and non-VCA Villages")+
  #geom_text(aes(y=(-5),label=stars),position=position_dodge(0.9), vjust="bottom")+
  theme_bw()+
  facet_wrap(~coeff_var)
ksnp_did_plots4


ksnp_did_plots5<-dualdid_model_output %>%
  filter((coeff_var=="did5" | coeff_var=="did6") & 
           depvar=="KSNP") %>%
  ggplot(aes(x=fct_rev(depvar), 
             y=(Estimate*8936600), 
             ymax=(UL*8936600), 
             ymin=(LL*8936600),
             fill=fct_rev(mtch)))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(width=.25, position=position_dodge(0.9))+
  #scale_fill_discrete(name="Matching\nTechnique", breaks=c("None", "Full", "Prop. Score"), 
  #                   labels=c("None", "Full", "Prop. Score"), values=tol3qualitative)+
  #scale_fill_manual(name="Matching\nTechnique", breaks=c("All", "Full", "Prop. Score"), 
  #                  labels=c("None", "Full", "Prop. Score"), values=tol3qualitative)+
  labs(x = "Forest-Change Type", y="Average Additional Tree-Cover Loss in VCA Villages (Ha)")+
  #coord_cartesian(ylim = c(0,-25))+
  #ggtitle("Difference in Forest Cover Change\nBetween VCA and non-VCA Villages")+
  #geom_text(aes(y=(-5),label=stars),position=position_dodge(0.9), vjust="bottom")+
  theme_bw()+
  facet_wrap(~coeff_var, scales="free")
ksnp_did_plots5

ggsave(file="C:/Users/JTErbaugh/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/plot-check1.png",
       plot=ksnp_did_plots4,
       dpi=600, width=6.5, height=4, unit="in")


##Only Short-Term (2000-2004)
ksnp_did_plots5<-dualdid_model_output %>%
  filter((coeff_var=="did5" | coeff_var=="did6") & 
           depvar=="KSNP Tree-Cover Change") %>%
  ggplot(aes(x=fct_rev(depvar), 
             y=Estimate, 
             ymax=UL, 
             ymin=LL,
             fill=fct_rev(mtch)))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(width=.25, position=position_dodge(0.9))+
  #scale_fill_discrete(name="Matching\nTechnique", breaks=c("None", "Full", "Prop. Score"), 
  #                   labels=c("None", "Full", "Prop. Score"), values=tol3qualitative)+
  #scale_fill_manual(name="Matching\nTechnique", breaks=c("All", "Full", "Prop. Score"), 
  #                  labels=c("None", "Full", "Prop. Score"), values=tol3qualitative)+
  labs(x = "Forest-Change Type", y="Average Additional Tree-Cover Loss in VCA Villages (Ha)")+
  #coord_cartesian(ylim = c(0,-25))+
  #ggtitle("Difference in Forest Cover Change\nBetween VCA and non-VCA Villages")+
  #geom_text(aes(y=(-5),label=stars),position=position_dodge(0.9), vjust="bottom")+
  theme_bw()+
  facet_wrap(did_type~coeff_var, scales="free")
ksnp_did_plots4
###Adding diff-in-diff vars###



