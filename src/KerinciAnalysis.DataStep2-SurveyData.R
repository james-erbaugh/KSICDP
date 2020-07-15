###Data Step 2 for KSNP Analysis###
install.packages("ineq")
install.packages("wesanderson")
install.packages(clusrank)
install.packages("waffle")

library(haven)
library(ggplot2)
library(ggpubr)
library(ineq)
library(gridExtra)
library(Hmisc)
library(wesanderson)
library(colorBrewer)
library(clusrank)
library(sandwich)
library(clubSandwich)
library(tidyverse)
library(dplyr)
library(survey)

survey<-read_sas("C:/Users/f003r0x/Dropbox/#michigan phd/1.Dissertation/FieldWork/Survey/Data/final_recode/final_recode.sas7bdat")
ksnp_wide1<-read.csv("C:/Users/f003r0x/Dropbox/#michigan phd/1.Dissertation/Ch5-Kerinci/KSNP-Vils-2003.csv")
mdvdi<-read.csv("C:/Users/f003r0x/Dropbox/#michigan phd/1.Dissertation/Ch5-Kerinci/Data/MDVDI_Kerinci.csv")
lvwell<-read.csv("C:/Users/f003r0x/Dropbox/#michigan phd/1.Dissertation/Ch5-Kerinci/Data/lvrecode.csv")
kades<-read.csv("C:/Users/f003r0x/Dropbox/#michigan phd/1.Dissertation/Ch5-Kerinci/Data/ModulKaDes_Subset.csv")

############################################################################################
###Testing significant differences between sample and population treatment representation###
############################################################################################

##Sample has 7 of 51 villages in treatment, population has 74 of 397 in treatment##

trt<-c(7,74)
ntrt<-c(44, 323)
tot<-c(51,397)
samp1<-cbind(trt,tot)
samp1<-as.matrix(samp1)
samp2<-cbind(trt,ntrt)
samp2<-as.matrix(samp2)

prop.test(samp2)

samp1

#####################################################################
###Generating data with unique ID to calculate proximity in ArcGIS###
#####################################################################
##Create a concatenated ID string##
survey$ID<-paste(survey$SID, survey$HVCODE2,survey$HHID2, sep="")

survey$Lat_Redo<-(survey$LatDegMinSec_1+survey$LatDegMinSec_2/60+survey$LatDegMinSec_3/3600)*-1
survey$Lon_Redo<-survey$LongDegMinSec_1 +survey$LongDegMinSec_2/60+survey$LongDegMinSec_3/3600
sum(survey$Lat_Redo!=survey$Lat_Result, na.rm=T)
summary(survey$Lat_Redo)
summary(survey$Lon_Redo)

prox_table<-survey[,c("TPID", "ID","FKSETTLE","HDNAME", "Lat_Result", "Lon_Result", "Lat_Redo", "Lon_Redo")]

#write.csv(prox_table, "E:/Indonesian_Spatial_Data/Sampling_Maps/Prox2.csv")

##  This table was imported into ArcGIS, plotted (add XY data), obvious errors removed, and then Euclidean distance
##  between HH location and KSNP was calculated. The table was saved as "HHsProx-NoErrors.csv"

#############################################################
###Adding Proximity and inside KSNP treatment vars to KSNP###
#############################################################

prox<-read.csv("C:/Users/f003r0x/Dropbox/#michigan phd/1.Dissertation/Ch5-Kerinci/Data/HHsProx-NoErrors.csv")

###Subsetting prox###
prox_merge<-prox[,c("TPID", "NEAR_DIST")]
prox_merge$NEAR_DIST_KSNP<-prox_merge$NEAR_DIST
prox_merge$NEAR_DIST<-NULL

survey2<-merge(survey,prox_merge,by="TPID",all.x=T)
summary(survey2$NEAR_DIST_KSNP)

survey2$trt2<-0
survey2$trt2[survey2$NEAR_DIST_KSNP==0 | survey2$HVCODE2==36 |  survey2$HVCODE2==41]<-1
table(survey2$trt2)

##########################################################################
###Adding Proximity and inside Production Forest treatment vars to KSNP###
##########################################################################
prox2<-read.csv("C:/Users/f003r0x/Dropbox/#michigan phd/1.Dissertation/Ch5-Kerinci/Data/HHsProxHP_elev-NoErrors.csv")

###Subsetting prox###
prox_merge2<-prox2[,c("TPID", "NEAR_DIST")]
prox_merge2$NEAR_DIST_HP<-prox_merge2$NEAR_DIST
prox_merge2$NEAR_DIST<-NULL

survey3<-merge(survey2,prox_merge2,by="TPID",all.x=T)
summary(survey3$NEAR_DIST_HP)

survey3$trt3<-0
survey3$trt3[survey3$NEAR_DIST_HP==0 | survey3$HVCODE2==22 |  survey3$HVCODE2==49]<-1
summary(survey3$trt3)

##########################################################################
###Adding treatment variable for being within production forest or KSNP###
##########################################################################

survey3$trt4<-0
survey3$trt4[survey3$trt3==1 | survey3$trt2==1] <-1
summary(survey3$trt4)

#########################################
###Adding VCA Treatment Villages Codes###
#########################################

survey3$trt1<-0
survey3$trt1[survey3$HVCODE2==4 | survey3$HVCODE2==36 | survey3$HVCODE2==28 | survey3$HVCODE2==40 |
               survey3$HVCODE2==14 | survey3$HVCODE2==49 | survey3$HVCODE2==45]<-1
summary(survey3$trt1)


###########################################
###Adding nominal all treatment variable###
###########################################

survey3$trt_all<-0
survey3$trt_all[survey3$trt2==1]<-2
survey3$trt_all[survey3$trt3==1]<-3
survey3$trt_all[survey3$trt1==1]<-1
table(survey3$trt_all)

####################################
###Adding Elevation from SRTM DEM###
####################################

elev_merge<-prox2[,c("TPID", "X12_8a_clip")]
elev_merge$elev<-elev_merge$X12_8a_clip
elev_merge$X12_8a_clip<-NULL
survey4<-merge(survey3,elev_merge, by="TPID", all.x=T)


summary(survey4$FELEVATI_1)
summary(survey4$elev)
summary(survey4$FELEVATI_1-survey4$elev)

###########################################################################
###Adding binary MDVDI sub-categories to KSNP and making MDVDI variables###
###########################################################################

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

mdvdi[ mdvdi == "." ] <- NA
mdvdi$ed_attainment[mdvdi$ed_attainment=="."]<-NA
mdvdi2 <- mdvdi %>%
  mutate(ed_attainment = factor(ed_attainment, levels = c(NA, 0, 1)),
         live_toiletV2 = factor(live_toiletV2, levels = c(NA, 0, 1)),
         live_fuel = factor(live_fuel, levels = c(NA, 0, 1)),
         live_water = factor(live_water, levels = c(NA, 0, 1)),
         live_floor = factor(live_floor, levels = c(NA, 0, 1)))
mdvdi2$ed_attainment<-as.numeric.factor(mdvdi2$ed_attainment)
#summary(mdvdi2$ed_attainment)
mdvdi2$live_water<-as.numeric.factor(mdvdi2$live_water)
mdvdi2$live_toiletV2<-as.numeric.factor(mdvdi2$live_toiletV2)
mdvdi2$live_fuel<-as.numeric.factor(mdvdi2$live_fuel)
mdvdi2$live_floor<-as.numeric.factor(mdvdi2$live_floor)

survey4<-merge(survey4,mdvdi2, by="TPID", all.x=T)

###Creating education index
survey4$ei <- ifelse(is.na(survey4$ed_attainment), survey4$ed_attendenceV2, 
              ifelse(is.na(survey4$ed_attendenceV2) ,survey4$ed_attainment,
              (survey4$ed_attainment*.5)+(survey4$ed_attendenceV2*.5)))
summary(survey4$ei)

###Creating health index
survey4$hi <- ifelse(is.na(survey4$health_mortality), survey4$health_nutrition, 
                     ifelse(is.na(survey4$health_nutrition) ,survey4$health_mortality,
                            (survey4$health_mortality*.5)+(survey4$health_nutrition*.5)))
summary(survey4$hi)

###Creating livelihood index
survey4$li<-mdvdi2$live_electricity*(1/6)+mdvdi2$live_toiletV2*(1/6)+mdvdi2$live_water*(1/6)+
  mdvdi2$live_floor*(1/6)+mdvdi2$live_fuel*(1/6)+mdvdi2$live_assets*(1/6)
summary(survey4$li)

survey4$li[is.na(survey4$live_electricity)]<-mdvdi2$live_toiletV2[is.na(survey4$live_electricity)]*(1/5)+
  mdvdi2$live_water[is.na(survey4$live_electricity)]*(1/5)+
  mdvdi2$live_floor[is.na(survey4$live_electricity)]*(1/5)+
  mdvdi2$live_fuel[is.na(survey4$live_electricity)]*(1/5)+
  mdvdi2$live_assets[is.na(survey4$live_electricity)]*(1/5)
summary(survey4$li)

survey4$li[is.na(survey4$live_toiletV2)]<-mdvdi2$live_electricity[is.na(survey4$live_toiletV2)]*(1/5)+
  mdvdi2$live_water[is.na(survey4$live_toiletV2)]*(1/5)+
  mdvdi2$live_floor[is.na(survey4$live_toiletV2)]*(1/5)+
  mdvdi2$live_fuel[is.na(survey4$live_toiletV2)]*(1/5)+
  mdvdi2$live_assets[is.na(survey4$live_toiletV2)]*(1/5)
summary(survey4$li)

survey4$li[is.na(survey4$live_fuel)]<-mdvdi2$live_electricity[is.na(survey4$live_fuel)]*(1/5)+
  mdvdi2$live_water[is.na(survey4$live_fuel)]*(1/5)+
  mdvdi2$live_floor[is.na(survey4$live_fuel)]*(1/5)+
  mdvdi2$live_toiletV2[is.na(survey4$live_fuel)]*(1/5)+
  mdvdi2$live_assets[is.na(survey4$live_fuel)]*(1/5)
summary(survey4$li)

survey4$li[is.na(survey4$live_floor)]<-mdvdi2$live_electricity[is.na(survey4$live_floor)]*(1/5)+
  mdvdi2$live_water[is.na(survey4$live_floor)]*(1/5)+
  mdvdi2$live_fuel[is.na(survey4$live_floor)]*(1/5)+
  mdvdi2$live_toiletV2[is.na(survey4$live_floor)]*(1/5)+
  mdvdi2$live_assets[is.na(survey4$live_floor)]*(1/5)
summary(survey4$li)

survey4$li[is.na(survey4$live_water)]<-mdvdi2$live_electricity[is.na(survey4$live_water)]*(1/5)+
  mdvdi2$live_floor[is.na(survey4$live_water)]*(1/5)+
  mdvdi2$live_fuel[is.na(survey4$live_water)]*(1/5)+
  mdvdi2$live_toiletV2[is.na(survey4$live_water)]*(1/5)+
  mdvdi2$live_assets[is.na(survey4$live_water)]*(1/5)
summary(survey4$li)

###Creating overall mdp index###

survey4$mdpi<-survey4$ei*(1/3)+survey4$hi*(1/3)+survey4$li*(1/3)
summary(survey4$mdpi)

###Creating extreme deprivation headcount###

survey4$pov_hc<-ifelse(survey4$mdpi>=.3333,1,0)
summary(survey4$pov_hc)

####################################################
###Adding rubber and oil palm recodes to top crop###
####################################################

survey4$FTOPCROP_1_1[survey4$FTOPCROP_1_1==23 & grepl("karet",survey4$CROPS_4_TEXT,fixed=T)]<-24
summary(survey4$FTOPCROP_1_1==24)

survey4$FTOPCROP_1_1[survey4$FTOPCROP_1_1==23 & grepl("sawit",survey4$CROPS_4_TEXT,fixed=T)]<-25
summary(survey4$FTOPCROP_1_1==25)

#######################################
###Adding important crop categorires###
#######################################


survey4$rice<-0
survey4$rice[grepl("^1,|^1$|,1,",survey4$CROPS)]<-1
survey4$rice[grepl("^2,|^2$|,2,|,2$",survey4$CROPS)]<-1
summary(survey4$rice)

survey4$tubers<-0
survey4$tubers[grepl("^3,|^3$|,3,|,3$",survey4$CROPS)]<-1
survey4$tubers[grepl("^4,|^4$|,4,|,4$",survey4$CROPS)]<-1
survey4$rubberop[grepl("ubi",survey4$CROPS_4_TEXT)]<-1
summary(survey4$tubers)

survey4$coffee<-0
survey4$coffee[grepl("^18,|^18$|,18,|,18$",survey4$CROPS)]<-1
survey4$coffee[grepl("^19,|^19$|,19,|,19$",survey4$CROPS)]<-1
summary(survey4$coffee)

survey4$cinnamon<-0
survey4$cinnamon[grepl("^20,|^20$|,20,|,20$",survey4$CROPS)]<-1
summary(survey4$cinnamon)

survey4$rubberop<-0
survey4$rubberop[grepl("sawit|karet|katet",survey4$CROPS_4_TEXT)]<-1
summary(survey4$rubberop)

##########################################
###Adding simplified TopCrop Categories###
##########################################

survey4$TOPCROP_CAT<-NULL
survey4$TOPCROP_CAT[survey4$FTOPCROP_1_1==1 | survey4$FTOPCROP_1_1==2]<-1
survey4$TOPCROP_CAT[survey4$FTOPCROP_1_1==3]<-2
survey4$TOPCROP_CAT[survey4$FTOPCROP_1_1==4 | survey4$FTOPCROP_1_1==5 | 
                      survey4$FTOPCROP_1_1==6 | survey4$FTOPCROP_1_1==7 |
                      survey4$FTOPCROP_1_1==8 | survey4$FTOPCROP_1_1==9 |
                      survey4$FTOPCROP_1_1==10 | survey4$FTOPCROP_1_1==11]<-3
survey4$TOPCROP_CAT[survey4$FTOPCROP_1_1==12 | survey4$FTOPCROP_1_1==13 | 
                      survey4$FTOPCROP_1_1==14 | survey4$FTOPCROP_1_1==15 |
                      survey4$FTOPCROP_1_1==16 | survey4$FTOPCROP_1_1==17 |
                      survey4$FTOPCROP_1_1==21 | survey4$FTOPCROP_1_1==22]<-4
survey4$TOPCROP_CAT[survey4$FTOPCROP_1_1==18 | survey4$FTOPCROP_1_1==19]<-5
survey4$TOPCROP_CAT[survey4$FTOPCROP_1_1==20]<-6
survey4$TOPCROP_CAT[survey4$FTOPCROP_1_1==24 | survey4$FTOPCROP_1_1==25]<-7

survey4$TOPCROP_CAT[is.na(survey4$TOPCROP_CAT)]<-8

tab_VCACR<-table(survey4$trt1, survey4$TOPCROP_CAT)
prop.table(tab_VCACR,1)
prop.table(tab_VCACR,2)

#####################################################
###Adding simplified and binary topcrop categories###
#####################################################

survey4$coffee2<-0
survey4$coffee2[survey4$TOPCROP_CAT==5]<-1
summary(survey4$coffee2)
summary(survey4$TOPCROP_CAT==5)

survey4$cinnamon2<-0
survey4$cinnamon2[survey4$TOPCROP_CAT==6]<-1
summary(survey4$cinnamon2)
summary(survey4$TOPCROP_CAT==6)

survey4$tubers2<-0
survey4$tubers2[survey4$TOPCROP_CAT==2]<-1
summary(survey4$tubers2)

survey4$rice2<-0
survey4$rice2[survey4$TOPCROP_CAT==1]<-1
summary(survey4$rice2)

survey4$rubberop2<-0
survey4$rubberop2[survey4$TOPCROP_CAT==7]<-1
summary(survey4$rubberop2)

####################################
###Adding cash agro-crop binaries###
####################################

survey4$CASH_CROP<-0
survey4$CASH_CROP[survey4$TOPCROP_CAT>=5 & survey4$TOPCROP_CAT<8]<-1
summary(survey4$CASH_CROP)

survey4$CASH_CROP2<-0
survey4$CASH_CROP2[survey4$coffee==1 | survey4$rubberop==1 | survey4$cinnamon==1]<-1
summary(survey4$CASH_CROP2)

#########################
###Land Area Variables###
#########################

##Adding real 0s to total land area
survey4$totalLA[is.na(survey4$totalLA)]<-0
summary(survey4$totalLA)

##Creating recode variables##
survey4$land_recode<-0
survey4$land_recode[survey4$LA_recode!="No recode"]<-1
survey4$land_recode[survey4$LA_recode=="N/A"]<-0

summary(survey4$land_recode)

table(survey4$land_recode, survey4$trt1)
prop.test(table(survey4$land_recode, survey4$trt1))
wilcox.test (survey4$totalLA,  survey4$land_recode)
mean(survey4$totalLA[survey4$land_recode==1], na.rm=T)
mean(survey4$totalLA[survey4$land_recode==0], na.rm=T)
##There is a significant difference between treatment and non-treatment proportion
##of recoded land owned. However, treated land-owned has fewer recodes than non-treated,
##and the mean of recoded land-owned is greater than not recoded land. Thus, it is likely that 
##the difference below--HHs in treatment own more land than househodls ~treatment--is conservative.

###Adding overall land-ownership type variable###

##HH with formal Ag, Plantation, or Pasture title##
survey4$LandOwn_Ag<-NA
survey4$LandOwn_Ag[!is.na(survey4$FLANDOWN_3_2) | 
                      !is.na(survey4$FLANDOWN_3_7) | !is.na(survey4$FLANDOWN_3_8)]<-0
survey4$LandOwn_Ag[survey4$FLANDOWN_3_2==1 | survey4$FLANDOWN_3_2==3]<-1 
survey4$LandOwn_Ag[survey4$FLANDOWN_3_7==1 | survey4$FLANDOWN_3_7==3]<-1 
survey4$LandOwn_Ag[survey4$FLANDOWN_3_8==1 | survey4$FLANDOWN_3_8==3]<-1 
summary(survey4$LandOwn_Ag)
table(survey4$LandOwn_Ag, survey4$trt1)

##HH with no type of ownership recognition for Ag Land##
survey4$LandOwn_Ag2<-NA
survey4$LandOwn_Ag2[!is.na(survey4$FLANDOWN_3_2) | 
                     !is.na(survey4$FLANDOWN_3_7) | !is.na(survey4$FLANDOWN_3_8)]<-0
survey4$LandOwn_Ag2[survey4$FLANDOWN_3_2==4]<-1 
survey4$LandOwn_Ag2[survey4$FLANDOWN_3_7==4]<-1 
survey4$LandOwn_Ag2[survey4$FLANDOWN_3_8==4]<-1 
summary(survey4$LandOwn_Ag2)
table(survey4$LandOwn_Ag2, survey4$trt1)

##HH with formal title for any area (includes forest and home garden)##
survey4$LandOwn_All<-NA
survey4$LandOwn_All[!is.na(survey4$FLANDOWN_3_2) | !is.na(survey4$FLANDOWN_3_6) |
                      !is.na(survey4$FLANDOWN_3_7) | !is.na(survey4$FLANDOWN_3_8) |
                      !is.na(survey4$FLANDOWN_3_9)]<-0
survey4$LandOwn_All[survey4$FLANDOWN_3_2==1 | survey4$FLANDOWN_3_2==3]<-1 
survey4$LandOwn_All[survey4$FLANDOWN_3_7==1 | survey4$FLANDOWN_3_7==3]<-1 
survey4$LandOwn_All[survey4$FLANDOWN_3_8==1 | survey4$FLANDOWN_3_8==3]<-1 
survey4$LandOwn_All[survey4$FLANDOWN_3_6==1 | survey4$FLANDOWN_3_6==3]<-1 
survey4$LandOwn_All[survey4$FLANDOWN_3_9==1 | survey4$FLANDOWN_3_9==3]<-1 
summary(survey4$LandOwn_All)
table(survey4$LandOwn_All, survey4$trt1)

##HH with formal title for natural forest area##
survey4$LandOwn_For<-NA
survey4$LandOwn_For[!is.na(survey4$FLANDOWN_3_9)]<-0
survey4$LandOwn_For[survey4$FLANDOWN_3_2==1]<-1 
summary(survey4$LandOwn_For)
table(survey4$LandOwn_For, survey4$trt1)

##Adding land owned five years ago and HHHAge and HHHED##
livwl_lnd<-lvwell[c("tpid","lv_ALOWN", "LV_ALOWNX", "MHSEX", "MHAGE", "MHEDU","MHOCC")]
survey4<-merge(survey4, livwl_lnd, by.x="TPID", by.y="tpid", all.x=T)
##Checking values
summary(survey4$lv_ALOWN)
survey4$lv_ALOWN[is.na(survey4$lv_ALOWN)]<-0
sum(survey4$totalLA-survey4$lv_ALOWN, na.rm=T)
##These values are the same
##Adding 0s to 5 yrs ago land
summary(survey4$LV_ALOWNX)
survey4$LV_ALOWNX[is.na(survey4$LV_ALOWNX)]<-0
summary(survey4$LV_ALOWNX)
sum(survey4$LV_ALOWNX[survey4$LV_ALOWNX<30]-survey4$totalLA[survey4$LV_ALOWNX<30])

##Transforming MHSEX to "Female Headed Household"
survey4$MHSEX<-survey4$MHSEX-1
survey4$MHSEX[survey4$MHSEX==2]<-NA
summary(survey4$MHSEX)

##Creating land gain, loss binary variables##

survey4$lnd_loss<-0
survey4$lnd_loss[survey4$LV_ALOWNX>survey4$lv_ALOWN]<-1
summary(survey4$lnd_loss)

survey4$lnd_gain<-0
survey4$lnd_gain[survey4$LV_ALOWNX<survey4$lv_ALOWN]<-1
summary(survey4$lnd_gain)

############################################
###Adding simplified Ethnicity Categories###
############################################

survey4$ethnic<-survey4$HHHETHNIC
survey4$ethnic[survey4$HHHETHNIC>4 | survey4$HHHETHNIC==2]<-5
table(survey4$HHHETHNIC)
table(survey4$ethnic)
survey4$ethnic[!is.na(survey4$ethnic) & survey4$ethnic>1]<-(survey4$ethnic[!is.na(survey4$ethnic) & survey4$ethnic>1]-1)
survey4$ethnic[!is.na(survey4$ethnic)]<-survey4$ethnic[!is.na(survey4$ethnic)]-1
survey4$ethnic[survey4$ethnic==0]<-4
survey4$ethnic[survey4$ethnic==1]<-5
survey4$ethnic[survey4$ethnic==3]<-1
survey4$ethnic[survey4$ethnic==5]<-3
table(survey4$ethnic)

##1==Other (Minang, South Sumatran, North Sumatran, Bengkulu, etc.), 2==Javanese, 3==Melayu Jambi, 4=Kerinci

###################################
###Counting the number of adults###
###################################

x<-c("HMEMB_2_1_1","HMEMB_2_2_1", "HMEMB_2_3_1", "HMEMB_2_4_1", "HMEMB_2_5_1", "HMEMB_2_6_1","HMEMB_2_7_1",
     "HMEMB_2_8_1", "HMEMB_2_9_1", "HMEMB_2_10_1")

survey4$HHMEMB_ADULTS<-rowSums(survey4[x]>15, na.rm=T)
summary(survey4$HHMEMB_ADULTS)


##############################################
###Generating Per Capita Poverty Line Value###
##############################################

#Indonesian Poverty Line Established in 2016: 354386 IDR/Month
##So 354386*12=4252632
survey4$POVLINE1<-NA
survey4$POVLINE1[(survey4$HHMEMB_ADULTS*4252632)>=survey4$totalincome]<-1
survey4$POVLINE1[(survey4$HHMEMB_ADULTS*4252632)<survey4$totalincome]<-0
summary(survey4$POVLINE1)

##Since the mean level of income, even within non-outlier income HHs, is overwhelmingly above this 
##poverty line, I am recoding NAs as "0"
survey4$POVLINE1[is.na(survey4$POVLINE1)]<-0

##Looking at proportion of below poverty line in treatment, ~treatment
tab_VCAPOC1<-table(survey4$trt1, survey4$POVLINE1)
prop.table(tab_VCAPOC1,2)
##Indicates a graeter percentage of treatment village HHs are very poor (~20$ VCA vs ~13% !VCA)

#Global poverty line is 1.90/day
#IDR to USD on 1/1/2016=13525.65
#So, 13525.65*1.90*365=9380038
survey4$POVLINE2<-NA
survey4$POVLINE2[(survey4$HHMEMB_ADULTS*9380038)>=survey4$totalincome]<-1
survey4$POVLINE2[(survey4$HHMEMB_ADULTS*9380038)<survey4$totalincome]<-0
summary(survey4$POVLINE2)

##Since the mean level of income, even within non-outlier income HHs, is overwhelmingly above this 
##poverty line, I am recoding NAs as "0"
survey4$POVLINE2[is.na(survey4$POVLINE2)]<-0

##Looking at proportion of below poverty line in treatment, ~treatment
survey4$POVLINE1[is.na(survey4$POVLINE1)]<-0
tab_VCAPOC2<-table(survey4$trt1, survey4$POVLINE2)
prop.table(tab_VCAPOC2,2)
#Indicates a slightly larger percentage of ~VCA HHs are below this line (14.6% vs 13.3%)

########################################
###Generating Inequality Measurements###
########################################
library(ineq)

##GINI coefficient measurement##
survey4<-survey4 %>%
  group_by(HVCODE) %>%
  mutate(GINI = ineq(totalincome, na.rm=TRUE))

summary(survey4$GINI)

table(survey4$trt1, mean(survey4$GINI[survey4$trt1==0], na.rm=T))
mean(survey4$GINI[survey4$trt1==0])
mean(survey4$GINI[survey4$trt1==1])
sqrt(sd(survey4$GINI[survey4$trt1==0]))
sqrt(sd(survey4$GINI[survey4$trt1==1]))

t.test(survey4$GINI[survey4$trt1==1], survey4$GINI[survey4$trt1==0], alternative = "two.sided", var.equal = FALSE)


mean(survey4$GINI[survey4$trt1==1])+(mean(survey4$GINI[survey4$trt1==1])*sqrt(sd(survey4$GINI[survey4$trt1==1])))

##HH income difference from median village income##

survey4<-survey4 %>%
  group_by(HVCODE) %>%
  mutate(medvilincome = median(totalincome, na.rm=TRUE))

survey4$diffincome<-survey4$totalincome-survey4$medvilincome
summary(survey4$diffincome/13520)

##looking at where income outliers live
tab<-table(survey4$flag_incomeoutlier, survey4$trt1)
prop.table(tab)
mean(survey4$totalincome[survey4$flag_incomeoutlier==1&survey4$trt1==0], na.rm=T)
mean(survey4$totalincome[survey4$flag_incomeoutlier==1&survey4$trt1==1], na.rm=T)
#15% of income outliers are in VCA villages (greater percentage than are in the total population)
#Average income for outliers in ~VCA villages is higher than in VCA villages (220493681 Vs 184541176)

#############################################
###Creating village-level data and dataset###
#############################################

ksnp_wide_jambi<-ksnp_wide1[(ksnp_wide1$ID2003_d>1500000000 & ksnp_wide1$ID2003_d<1600000000),]

#write.csv(ksnp_wide_jambi, "C:/Users/f003r0x/Dropbox/#michigan phd/1.Dissertation/Ch5-Kerinci/Data/ksnp_wide_jambi.csv")

##Village names and codes##

vilnames<-survey4 %>%
  group_by(FKSETTLE, HVCODE, HVCODE2)%>%
  summarise(n=n())
#write.csv(vilnames, "C:/Users/f003r0x/Dropbox/#michigan phd/1.Dissertation/Ch5-Kerinci/Data/survey_vilnamescodes.csv")
##Created "survey_vilnamescodes_fixed.csv" in the same location as above with the correct names for each
##unique FKSETTLE, HVCODE, HVCODE2

vilnames<-read.csv("C:/Users/f003r0x/Dropbox/#michigan phd/1.Dissertation/Ch5-Kerinci/Data/survey_vilnamescodes_fixed.csv")

##Correcting some issues in 
##Merging survey and vilnames datasets##
survey5<-merge(survey4, vilnames, by=c("FKSETTLE", "HVCODE"), all.x=T)

##Summarizing by revised name and village code##
survey_vil<-survey5 %>%
  group_by(NM.REVISED,HVCODE3)%>%
  summarise(elev1=mean(FELEVATI_1, na.rm=T), 
            elev2=mean(elev, na.rm=T),
            totalincome=mean(totalincome, na.rm=T),
            GINI=mean(GINI, na.rm=T),
            diffincome=mean(diffincome, na.rm=T),
            VCA=names(which.max(table(trt1))),
            topcrop=names(which.max(table(TOPCROP_CAT))),
            
            coffee_prod1=names(which.max(table(coffee))),
            coffee_prod2=mean(coffee, na.rm=T),
            coffee_tc1=names(which.max(table(coffee2))),
            coffee_tc2=mean(coffee2, na.rm=T),
            
            cinnamon_prod1=names(which.max(table(cinnamon))),
            cinnamon_prod2=mean(cinnamon, na.rm=T),
            cinnamon_tc1=names(which.max(table(cinnamon2))),
            cinnamon_tc2=mean(cinnamon2, na.rm=T),
            
            rubberop_prod1=names(which.max(table(rubberop))),
            rubberop_prod2=mean(rubberop, na.rm=T),
            rubberop_tc1=names(which.max(table(rubberop2))),
            rubberop_tc2=mean(rubberop2, na.rm=T),
            
            CASH_CROP_tc1=names(which.max(table(CASH_CROP))),
            CASH_CROP_tc2=mean(CASH_CROP, na.rm=T),
            CASH_CROP_prod1=names(which.max(table(CASH_CROP2))),
            CASH_CROP_prod2=mean(CASH_CROP2, na.rm=T),
            
            LandOwn_Ag_cnt=names(which.max(table(LandOwn_Ag))),
            LandOwn_Ag_mn=mean(LandOwn_Ag, na.rm=T),
            
            LandOwn_Ag2_cnt=names(which.max(table(LandOwn_Ag2))),
            LandOwn_Ag2_mn=mean(LandOwn_Ag2, na.rm=T),
            
            LandOwn_All_cnt=names(which.max(table(LandOwn_All))),
            LandOwn_All_mn=mean(LandOwn_All, na.rm=T),
            
            LandGain=mean(lnd_gain, na.rm=T),
            LandLoss=mean(lnd_loss, na.rm=T),
            
            totalLA_mn=mean(totalLA, na.rm=T),
            prox_ksnp=mean(NEAR_DIST_KSNP, na.rm=T),
            POVLINE1=mean(POVLINE1, na.rm=T),
            mdpi=mean(mdpi,na.rm=T),
            pov_hc=mean(pov_hc,na.rm=T),
            ID=mean(ID.y, na.rm=T),
            freq=n())

table(survey_vil$NM.REVISED, survey_vil$freq)

#View(survey_vil)
table(survey_vil$VCA, survey_vil$ID)
table(survey_vil$topcrop, survey_vil$HVCODE3)
table(survey_vil$VCA, survey_vil$pov_hc)
summary(survey_vil$mdpi[survey_vil$VCA==1])
summary(survey_vil$mdpi[survey_vil$VCA==0])

##Merging survey_vil dataset with ksnp_wide1 from "KerinciAnalysis-Analysis1.R"
survey_vil_ksnplos<-read.csv("E:/Indonesian_Spatial_Data/Sampling_Maps/srv_vils_KSNPlos0316.txt")
survey_vil_ksnplos<-survey_vil_ksnplos[,c("VALUE", "COUNT")]
survey_vil_ksnplos$COUNT.ksnplos<-survey_vil_ksnplos$COUNT
survey_vil_ksnplos$COUNT<-NULL

survey_vil_prmlos<-read.csv("E:/Indonesian_Spatial_Data/Sampling_Maps/srv_vils_prmlos0316.txt")
survey_vil_prmlos<-survey_vil_prmlos[,c("VALUE", "COUNT")]
survey_vil_prmlos$COUNT.prmlos<-survey_vil_prmlos$COUNT
survey_vil_prmlos$COUNT<-NULL

survey_vil_seclos<-read.csv("E:/Indonesian_Spatial_Data/Sampling_Maps/srv_vils_seclos0316.txt")
survey_vil_seclos<-survey_vil_seclos[,c("VALUE", "COUNT")]
survey_vil_seclos$COUNT.seclos<-survey_vil_seclos$COUNT
survey_vil_seclos$COUNT<-NULL

survey_vilelev<-read.csv("E:/Indonesian_Spatial_Data/Sampling_Maps/srv_vils_elev.txt")
survey_vilelev<-survey_vilelev[,c("VALUE", "MEAN")]
survey_vil_vilelev<-MEAN.vilelev<-survey_vil_seclos$MEAN
survey_vil_vilelev$MEAN<-NULL

survey_vil2<-merge(survey_vil, survey_vil_ksnplos, by.x="ID", by.y="VALUE", all.x=T)
survey_vil3<-merge(survey_vil2, survey_vil_prmlos, by.x="ID", by.y="VALUE", all.x=T)
survey_vil4<-merge(survey_vil3, survey_vil_seclos, by.x="ID", by.y="VALUE", all.x=T)
survey_vil5<-merge(survey_vil4, survey_vilelev, by.x="ID", by.y="VALUE", all.x=T)

##Dropping pilot observations##

#View(survey_vil5)

##Testing a few correlations##
cor(survey_vil$totalincome, survey_vil$GINI, use="pairwise.complete.obs")
cor(survey_vil$GINI, survey_vil$prox_ksnp, use="pairwise.complete.obs")
cor(survey_vil$GINI, survey_vil$elev1, use="pairwise.complete.obs")
cor(survey_vil$totalincome, survey_vil$mdpi, use="pairwise.complete.obs")
cor(survey_vil$GINI, survey_vil$mdpi, use="pairwise.complete.obs")

########################################################
###Merging village-level forest loss with survey data###
########################################################

survey6<-merge(survey5, survey_vil_ksnplos, by.x="ID.y", by.y="VALUE", all.x=T)
survey7<-merge(survey6, survey_vil_prmlos, by.x="ID.y", by.y="VALUE", all.x=T)
survey8<-merge(survey7, survey_vil_seclos, by.x="ID.y", by.y="VALUE", all.x=T)
survey9<-merge(survey8, survey_vilelev, by.x="ID.y", by.y="VALUE", all.x=T)
###Survey 9 is the largest dataset with all the values###

###DROPPING PILOT OBSERVATIONS FOR ANALYSIS###

survey9<-survey9[survey9$HVCODE2!=0,]

##Checking values of villages and names##
table(survey9$ID.y)
#Looks good: village-level samples from 20 to 29

survey9$COUNT.ksnplos[is.na(survey9$COUNT.ksnplos)]<-0
survey9$COUNT.prmlos[is.na(survey9$COUNT.prmlos)]<-0
survey9$COUNT.secloss[is.na(survey9$COUNT.seclos)]<-0

survey_vil5$COUNT.ksnplos[is.na(survey_vil5$COUNT.ksnplos)]<-0
survey_vil5$COUNT.prmlos[is.na(survey_vil5$COUNT.prmlos)]<-0
survey_vil5$COUNT.seclos[is.na(survey_vil5$COUNT.seclos)]<-0

###Fixing treatment 1 variables in survey9 dataset###

survey9$trt1[survey9$ID.y==1502052001 | survey9$ID.y==1572020001 | 
               survey9$ID.y==1502010017| survey9$ID.y==1501092014 | 
               survey9$ID.y==1501080034 | survey9$ID.y==1501071007 | 
               survey9$ID.y==1501020001]<-1

survey9$trt1[survey9$ID.y!=1502052001 & survey9$ID.y!=1572020001 & 
               survey9$ID.y!=1502010017& survey9$ID.y!=1501092014 & 
               survey9$ID.y!=1501080034 & survey9$ID.y!=1501071007 & 
               survey9$ID.y!=1501020001]<-0

table(survey9$trt1, survey9$ID.y)
#################################################################################################################

##Preliminary Analysis##

#################################################################################################################

#########################################
###Clustered t-tests on household data###
#########################################
##Environmental factors##
clusWilcox.test(survey9$NEAR_DIST_KSNP, cluster=survey9$ID.y, group=survey9$trt1, conf.int=0.95)
clusWilcox.test(survey9$elev, cluster=survey9$ID.y, group=survey9$trt1, conf.int=0.95)
svyttest(survey9$NEAR_DIST_KSNP~survey9$trt1, design)

##Income##

#Total income
clusWilcox.test(log(survey9$totalincome), cluster=survey9$ID.y, group=survey9$trt1, conf.int=0.95)
##Nonsignificant (p-value = 0.1076). So, the same income levels across treatment and non-treatment villages
t.test.cluster(log(survey9$totalincome), survey9$ID.y, factor(survey9$trt1), conf.int=0.95)
##Very nonsignificant (p-value = 0.8076954). So, the same income levels across treatment and non-treatment villages

#Difference between total income and median income within the village
clusWilcox.test(survey9$diffincome, cluster=survey9$HVCODE2, group=survey9$trt1, conf.int=0.95)
##Nonsignificant (p-value = 0.6262) difference in means. So, the same difference in median income levels between villages

t.test.cluster(survey9$diffincome, survey9$ID.y, factor(survey9$trt1), conf.int=0.95)
##Nonsignificant (p-value = 0.6127522 ) difference in means. So, the same difference in median income levels between villages

##Poverty line##

#HH below Indonesian poverty line
clusWilcox.test(survey9$POVLINE1, cluster=survey9$ID.y, group=survey9$trt1, conf.int=0.95)
#Highly nonsignificant (p-value = 0.7224)
t.test.cluster(survey9$POVLINE1, survey9$ID.y, factor(survey9$trt1), conf.int=0.95)
#Nonsignificant (p-value = 0.2675702).

##HH below World Bank poverty line
clusWilcox.test(survey9$POVLINE2, group=survey9$trt1,  cluster=survey9$ID.y,  conf.int=0.95)
#Nonsignificant (p-value = 0.4229)
t.test.cluster(survey9$POVLINE2, survey9$ID.y, factor(survey9$trt1), conf.int=0.95)
#Highly nonsignificant (p-value = 0.660522)

##MDPI tests##

clusWilcox.test(survey9$mdpi, group=survey9$trt1,  cluster=survey9$ID.y,  conf.int=0.95)
#Nonsignificant (p-value = 0.8567)
t.test.cluster(survey9$mdpi, survey9$ID.y, survey9$trt1, conf.int=0.95)
#Nonsignificant (p-value = 0.3592588)

##Livelihood strategies##

##Total land owned
clusWilcox.test(log(survey9$totalLA+1), group=survey9$trt1,  cluster=survey9$ID.y, conf.int=0.95)
clusWilcox.test(survey9$totalLA, group=survey9$trt1,  cluster=survey9$ID.y, conf.int=0.95)
##Signifcant difference: VCA HHs have more land
##Significant for logged (p-value = 0.02537) and not logged values (p-value = 0.02536)

#Land Ownership
clusWilcox.test(survey9$LandOwn_Ag, group=survey9$trt1,  cluster=survey9$ID.y, conf.int=0.95)
#Nonsignificant at (p-value = 0.509)
clusWilcox.test(survey9$LandOwn_Ag2, group=survey9$trt1,  cluster=survey9$ID.y, conf.int=0.95)
#Nonsignificant at (p-value = 0.6182)
clusWilcox.test(survey9$LandOwn_All, group=survey9$trt1,  cluster=survey9$ID.y, conf.int=0.95)
t.test.cluster(survey9$LandOwn_All, survey9$ID.y, survey9$trt1, conf.int=0.95)
#Nonsignificant at (p-value = 0.3999)
table(survey9$LandOwn_Ag2, survey9$trt1)

##Land loss vs gain##
clusWilcox.test(survey9$lnd_gain, group=survey9$trt1,  cluster=survey9$ID.y, conf.int=0.95)
t.test.cluster(survey9$lnd_gain, survey9$ID.y, factor(survey9$trt1), conf.int=0.95)
#Nonsignificant at (p-value = 0.21)
clusWilcox.test(survey9$lnd_loss, group=survey9$trt1,  cluster=survey9$ID.y, conf.int=0.95)
#Nonsignificant at (p-value = 0.9564)

###As an observation, of the HHs that were in the treatment and gained land, 1 had a formal title for ag land, 24 did not

#Growing coffee--VCAs and non-VCAs
###Need to investigate this finding, as it may or may not be related to the non-normal distribution of binary variable##
clusWilcox.test(survey9$coffee, group=survey9$trt1,  cluster=survey9$ID.y, conf.int=0.95)
t.test.cluster(survey9$coffee, survey9$ID.y, factor(survey9$trt1), conf.int=0.95)
t.test.cluster(survey9$coffee2, survey9$ID.y, factor(survey9$trt1), conf.int=0.95)
##Nonsignificant(p-value = 0.1176)
clusWilcox.test(survey9$coffee2, group=survey9$trt1,  cluster=survey9$ID.y, conf.int=0.95)
##Nonsignificant(p-value = 0.1449)

glmer(coffee~trt1 + (1|ID.y), family="binomial", data=survey9)

#Growing cinnamon--VCAs and non-VCAs
###Need to investigate this finding, as it may or may not be related to the non-normal distribution of binary variable##
clusWilcox.test(survey9$cinnamon, group=survey9$trt1,  cluster=survey9$ID.y, conf.int=0.95)
t.test.cluster(survey9$cinnamon, survey9$ID.y, factor(survey9$trt1), conf.int=0.95)
t.test.cluster(survey9$cinnamon2, survey9$ID.y, factor(survey9$trt1), conf.int=0.95)

#Nonsignificant(p-value = 0.1275)
clusWilcox.test(survey9$cinnamon, group=survey9$trt1,  cluster=survey9$ID.y, conf.int=0.95)
#Nonsignificant (p-value = 0.689)

#Growing Rubber/Oil Palm
clusWilcox.test(survey9$rubberop, group=survey9$trt1,  cluster=survey9$ID.y, conf.int=0.95)
t.test.cluster(survey9$rubberop2, survey9$ID.y, factor(survey9$trt1), conf.int=0.95)

#Nonsignificant(p-value = 0.2769)
clusWilcox.test(survey9$rubberop2, group=survey9$trt1,  cluster=survey9$ID.y, conf.int=0.95)
#Nonsignificant(p-value = 0.2815)

#Growing coffee, cinnamon, or oil palm
clusWilcox.test(survey9$CASH_CROP , group=survey9$trt1,  cluster=survey9$ID.y, conf.int=0.95)
#Significant (p-value = 0.03579)

clusWilcox.test(survey9$CASH_CROP2 , group=survey9$trt1,  cluster=survey9$ID.y, conf.int=0.95)
#Significant (p-value = 0.0165)

##CROPS and forest loss for "village perspective"##

##Treatment and forest loss##
wilcox.test(survey_vil5$COUNT.prmlos~survey_vil5$VCA, conf.int=0.95)
#Non-significant at (p-value = 0.02189)
wilcox.test(survey_vil5$COUNT.ksnplos~survey_vil5$VCA, conf.int=0.95)
#Significant at (p-value = 0.01121)
wilcox.test(survey_vil5$COUNT.seclos~survey_vil5$VCA, conf.int=0.95)
#Non-significant at (p-value = 0.01818)

##Crops and primary forest loss##
wilcox.test(survey_vil5$COUNT.prmlos~factor(survey_vil5$CASH_CROP_tc1), conf.int=0.95)
#Significant at (p-value = 0.0002582)
cor.test(survey_vil5$CASH_CROP_tc2, survey_vil5$COUNT.prmlos, method="kendall", alternative="two.sided")
#Significant at (p-value = 0.006368, tau = 0.2776759 )
wilcox.test(survey_vil5$COUNT.prmlos~factor(survey_vil5$CASH_CROP_prod1), conf.int=0.95)
#Significant at (p-value = 0.01352)
cor.test(survey_vil5$CASH_CROP_prod2, survey_vil5$COUNT.prmlos, method="kendall", alternative="two.sided")
#Significant at (p-value = 0.009648, cor = 0.2599336  )
wilcox.test(survey_vil5$COUNT.prmlos~factor(survey_vil5$coffee_prod1), conf.int=0.95)
#Significant at (p-value = 0.002918)
cor.test(survey_vil5$coffee_prod2, survey_vil5$COUNT.prmlos, method="kendall", alternative="two.sided")
#Significant at (p-value = 0.000972, cor = 0.3345944)
wilcox.test(survey_vil5$COUNT.prmlos~factor(survey_vil5$coffee_tc1), conf.int=0.95)
#Significant at (p-value = 0.0005323)
cor.test(survey_vil5$coffee_tc2, survey_vil5$COUNT.prmlos, method="kendall", alternative="two.sided")
#Significant at (p-value = 0.0002903, cor = 0.3801822 )
wilcox.test(survey_vil5$COUNT.prmlos~factor(survey_vil5$cinnamon_prod1), conf.int=0.95)
#Nonsignificant at (p-value = 0.9028)
#No villages with majority cinnamon as topcrop
cor.test(survey_vil5$cinnamon_prod2, survey_vil5$COUNT.prmlos, method="kendall", alternative="two.sided")
#Nonsignificant at (p-value = 0.9171, cor = 0.01066201 )
wilcox.test(survey_vil5$COUNT.prmlos~factor(survey_vil5$rubberop_prod1), conf.int=0.95)
#Nonsignificant at (p-value = 0.5916)
##All villages with majority of people growing rubber/karet also have majority with rubber/karet as top crop
cor.test(survey_vil5$rubberop_prod2, survey_vil5$COUNT.prmlos, method="kendall", alternative="two.sided")
#Nonsignificant at (p-value = 0.7263, cor = -0.03951038 )

###Land holdings and ownership and primary forest loss###

cor.test(survey_vil5$totalLA, survey_vil5$COUNT.prmlos, method="kendall", alternative="two.sided")
#Significant at (p-value = 0.004749, cor = 0.2808511 )

wilcox.test(survey_vil5$COUNT.prmlos~factor(survey_vil5$LandOwn_Ag_cnt), conf.int=0.95)
cor.test(survey_vil5$LandOwn_Ag_mn, survey_vil5$COUNT.prmlos, method="kendall", alternative="two.sided")
##Nonsignificant at (p-value = 0.1868, cor = -0.1358981  )

wilcox.test(survey_vil5$COUNT.prmlos~factor(survey_vil5$LandOwn_Ag2_cnt), conf.int=0.95)
cor.test(survey_vil5$LandOwn_Ag2_mn, survey_vil5$COUNT.prmlos, method="kendall", alternative="two.sided")
##Nnonsignificant at (p-value = 0.3967, cor = 0.09431217 )

wilcox.test(survey_vil5$COUNT.prmlos~factor(survey_vil5$LandOwn_All_cnt), conf.int=0.95)
cor.test(survey_vil5$LandOwn_All_mn, survey_vil5$COUNT.prmlos, method="kendall", alternative="two.sided")
##Significant at (p-value = 0.04805, cor = -0.2004334)

cor.test(survey_vil5$LandGain, survey_vil5$COUNT.prmlos, method="kendall", alternative="two.sided")
#Significant at (p-value = 0.01363, cor = 0.2576311)
cor.test(survey_vil5$LandLoss, survey_vil5$COUNT.prmlos, method="kendall", alternative="two.sided")
#Significant at (p-value = 0.03202, cor = -0.2157628 )


##CROPS and KSNP forest loss##

wilcox.test(survey_vil5$COUNT.ksnplos~factor(survey_vil5$CASH_CROP_prod1), conf.int=0.95)
#Nonsignificant at (p-value = 0.0902)
cor.test(survey_vil5$CASH_CROP_prod2, survey_vil5$COUNT.ksnplos, method="kendall", alternative="two.sided")
#Nonsignificant at (p-value = 0.1362, cor = 0.1481043)
wilcox.test(survey_vil5$COUNT.ksnplos~factor(survey_vil5$CASH_CROP_tc1), conf.int=0.95)
#Significant at (p-value = 0.005332)
cor.test(survey_vil5$CASH_CROP_tc2, survey_vil5$COUNT.ksnplos, method="kendall", alternative="two.sided")
#Significant at (p-value = 0.06293, cor = 0.18732)
wilcox.test(survey_vil5$COUNT.ksnplos~survey_vil5$coffee_prod1, conf.int=0.95)
##Significant at (p-value = 0.01948)
cor.test(survey_vil5$coffee_prod2, survey_vil5$COUNT.ksnplos, method="kendall", alternative="two.sided")
##Significant at (p-value = 0.006623, 0.2725881)
wilcox.test(survey_vil5$COUNT.ksnplos~survey_vil5$coffee_tc1, conf.int=0.95)
##Significant at (p-value = 0.02549)
cor.test(survey_vil5$coffee_tc2, survey_vil5$COUNT.ksnplos, method="kendall", alternative="two.sided")
##Significant at (p-value = 0.005514, cor = 0.2881511 )
wilcox.test(survey_vil5$COUNT.ksnplos~factor(survey_vil5$cinnamon_prod1), conf.int=0.95)
##Nonsignificant at (p-value = 0.9893)
cor.test(survey_vil5$cinnamon_prod2, survey_vil5$COUNT.ksnplos, method="kendall", alternative="two.sided")
##Nonsignificant at (p-value = 0.8931, cor = 0.01363057   )
cor.test(survey_vil5$cinnamon_tc2, survey_vil5$COUNT.ksnplos, method="kendall", alternative="two.sided")
##Nonsignificant at (p-value = 0.2101, cor = -0.1330347    )
wilcox.test(survey_vil5$COUNT.ksnplos~survey_vil5$rubberop_tc1, conf.int=0.95)
##Nonsignificant at (p-value = 0.7834)
cor.test(survey_vil5$rubberop_tc2, survey_vil5$COUNT.ksnplos, method="kendall", alternative="two.sided")
#Nonsignificant at (p-value = 0.9196, cor = 0.01167293)
cor.test(survey_vil5$rubberop_prod2, survey_vil5$COUNT.ksnplos, method="kendall", alternative="two.sided")
#Nonsignificant at (p-value = 0.5336, cor = -0.06952694  )
table(survey_vil5$CASH_CROPV, survey_vil5$coffee2)

###Land ownership and KSNP forest loss###

cor.test(survey_vil5$totalLA, survey_vil5$COUNT.ksnplos, method="kendall", alternative="two.sided")
#Significant at (p-value = 0.03861, cor = 0.2036101)

wilcox.test(survey_vil5$COUNT.ksnplos~factor(survey_vil5$LandOwn_Ag_cnt), conf.int=0.95)
cor.test(survey_vil5$LandOwn_Ag_mn, survey_vil5$COUNT.ksnplos, method="kendall", alternative="two.sided")
##Nonsignificant at (p-value = 0.3088, cor = -0.1036929 )

wilcox.test(survey_vil5$COUNT.ksnplos~factor(survey_vil5$LandOwn_Ag2_cnt), conf.int=0.95)
cor.test(survey_vil5$LandOwn_Ag2_mn, survey_vil5$COUNT.ksnplos, method="kendall", alternative="two.sided")
##Nonsignificant at (p-value = 0.3017, cor = 0.1137149)

wilcox.test(survey_vil5$COUNT.ksnplos~factor(survey_vil5$LandOwn_All_cnt), conf.int=0.95)
cor.test(survey_vil5$LandOwn_All_mn, survey_vil5$COUNT.ksnplos, method="kendall", alternative="two.sided")
##Nonsignificant at (p-value = 0.07776, cor = -0.1769847)
clusWilcox.test(survey9$CASH_CROP , group=survey9$trt1,  cluster=survey9$ID.y, conf.int=0.95)

cor.test(survey_vil5$LandGain, survey_vil5$COUNT.ksnplos, method="kendall", alternative="two.sided")
#Significant at (p-value = 0.01211, cor = 0.259297)
cor.test(survey_vil5$LandLoss, survey_vil5$COUNT.ksnplos, method="kendall", alternative="two.sided")
#Significant at (p-value = 0.04381, cor = -0.2007776)

###############
###Narrative###
###############

###From the above analysis, it seems that there is greater forest cover loss in villages that received treatment
### and greater forest cover loss in villages that have a majority of people who grow coffee as their top crop or
### grow coffee but not as their top crop. However, there is reason to believe that there is significant forest-cover loss
### within villages that grow rubber, oil palm, and cinnamon as well. These crops are similar in that there have been
### government and private programs to increase yield. Thus, it may be the case that villages who underwent KSNP
### treatment have become better able to receive government benefits while at the same time understanding the 
### opportunity cost of conservation. To test this, I model the following:

### I.
### A. I visualize the different livelihood strategies that treatment and non-treatment HHs pursue by crop
    ### HHs in the treatment have an equal spread across elevation and distance from KSNP, but they are more heavily
    ### invested in agroforest cash crops: cinnamon, coffee, rubber/oil palm

### B. I visualize the different land holdings of treatment and non-treatment HHs
    ### HHs in the treatment report owning more land, they hold fewer formal rights to this land, and they 
    ### have gained more land in the last five years than non-treatment households

### II.
### A. I look at the propensity of a HH to be within a "treatment" as based on their livelihood strategies
### B. I look at the propensity of a HH to produce coffee as based on their livelihood strategy
    ### This indicates no significant effect of treatment on coffee production, when we control for clustering
### C. I look at the propensity of a HH to produce an Agroforestry Cash Crop based on their livelihood strategy
    ### This indicates  significant effect of treatment on ag cash crop production, when we control for clustering
### D. I look at the propensity of a HH to claim ownershi of more land based on their demographics and livelihood strategies
    ### This indicates  significant effect of treatment on land ownership

### III.
### A. I model HH income based on agro cash crops and HH data, using fixed effects to examine the affect of treatment villages
### B. I model HH MDP based on agro cash crops and HH data and use fixed effects to examine the effects of treatment villages
### C. I model HH income based on coffee production and HH data, using fixed effects to examine the affect of treatment villages
### D. I model HH MDP based on coffee production and HH data, using fixed effects to examine the affect of treatment villages

###

#################################################################################################################

##I. VISUALIZING###

#################################################################################################################
# Function for plotting colors side-by-side
pal <- function(col, border = "light gray", ...){
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
       axes = FALSE, xlab = "", ylab = "", ...)
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

# Qualitative color schemes by Paul Tol
tol1qualitative=c("#4477AA")
tol2qualitative=c("#4477AA", "#CC6677")
tol3qualitative=c("#4477AA", "#DDCC77", "#CC6677")
tol4qualitative=c("#4477AA", "#117733", "#DDCC77", "#CC6677")
tol5qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677")
tol6qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499")
tol7qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499")
tol8qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499")
tol9qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499")
tol10qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
tol11qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
tol12qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499")

###########################################################################################
###   Ia: Visualizing cropping patterns                                                 ###
###########################################################################################

#############################################################################################
###Creating scatterplot visualizations for crop production between treatment/non-treatment###
#############################################################################################

##Making NAs a category
survey9$TOPCROP_CAT[is.na(survey9$TOPCROP_CAT)]<-8
##Combining Other Vegetable and other Fruit top crops to just "Other"
survey9$TOPCROP_CAT[survey9$TOPCROP_CAT==4]<-3

vca_topcrop1<-ggplot(survey9[survey9$trt1==0 & survey9$TOPCROP_CAT<8,], 
                     aes(x=NEAR_DIST_KSNP/1000, y=elev, color=factor(TOPCROP_CAT)))+
  scale_color_manual(name=NULL, 
                    values=tol6qualitative,
                    breaks=c("1","2","5", "6", "7", "3" ), 
                    labels=c("Rice", "Tubers", "Coffee", 
                             "Cinnamon","Rubber/Oil Palm","Other"))+
  geom_point(position=position_dodge(width=.5))+
  coord_cartesian(ylim=c(0,2000), xlim=c(0,8))+
  labs(y="Elevation", x="Distance to KSNP (Km)")+
  ggtitle("Non-VCA Households\n")+
  theme_classic()+
  theme(plot.title = element_text(size=12, hjust = 0.5),
        text=element_text(size=12),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        legend.text = element_text(margin = margin(r = 20, unit = "pt")))+
  guides(color=guide_legend(nrow=1,byrow=TRUE))

  
vca_topcrop1

vca_topcrop2<-ggplot(survey9[survey9$trt1==1&survey9$TOPCROP_CAT<8,], 
                     aes(x=NEAR_DIST_KSNP/1000, y=elev, color=factor(TOPCROP_CAT)))+
  scale_color_manual(name=NULL, 
                    values=tol6qualitative,
                    breaks=c("1","2","5", "6", "7", "3" ), 
                    labels=c("Rice", "Tubers", "Coffee", 
                             "Cinnamon","Rubber/Oil Palm","Other"))+
  geom_point(position=position_dodge(width=.5))+
  coord_cartesian(ylim=c(0,2000), xlim=c(0,8))+
  labs(y="Elevation", x="Distance to KSNP (Km)")+
  ggtitle("VCA Households\n")+
  theme_classic()+
  theme(plot.title = element_text(size=12, hjust = 0.5),
        text=element_text(size=12),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        legend.text = element_text(margin = margin(r = 20, unit = "pt")))+
  guides(color=guide_legend(nrow=1,byrow=TRUE))


vca_topcrop2

ggarrange(vca_topcrop1, vca_topcrop2, common.legend = TRUE)

###########################################################################################
###Creating waffle chart visualizations for crop production between treatment/non-treatment###
###########################################################################################

install.packages("waffle")
install.packages("extrafont")
install.packages("emojifont")

library(ggplot2)
library(emojifont)
library(waffle)

###   Reordering the palette

tol6qualitative_wfl=c("#332288", "#88CCEE",  "#DDCC77", "#CC6677","#AA4499", "#117733")


### First need to get the counts for each category:

wfl_tab<-count(survey9, trt1, TOPCROP_CAT) %>% ungroup()
wfl_tab$TOPCROP_CAT[is.na(wfl_tab$TOPCROP_CAT)]<-8
wfl_tab$prop<-0
wfl_tab$prop[wfl_tab$trt1==0]<-wfl_tab$n[wfl_tab$trt1==0]/1125
wfl_tab$prop[wfl_tab$trt1==1]<-wfl_tab$n[wfl_tab$trt1==1]/179
wfl_tab

###   Non VCA Waffle Chart
parts1<-c(Rice=round(149/790*100), Tubers=round(237/790*100), Coffee=round(161/790*100), Cinnamon=round(74/790*100),
          "Rubber/Oil Palm"=round(47/790*100), "Other"=round(122/790*100) )

wfl_nvca<-waffle(parts1, rows=10, size=0, colors=tol6qualitative_wfl)
wfl_nvca<-wfl_nvca + ggtitle("Proportion of Non-VCA Households")+theme(plot.title = element_text(size=12, hjust = 0.5))


###   VCA Waffle Chart
parts2<-c(Rice=23/161*100, Tubers=27/161*100, Coffee=56/161*100, Cinnamon=21/161*100, "Rubber/Oil Palm"=24/161*100, 
          "Other"=14/161*100)
wfl_vca<-waffle(parts2, rows=10, size=0, colors=tol6qualitative_wfl)
wfl_vca<-wfl_vca + ggtitle("Proportion of VCA Households")+theme(plot.title = element_text(size=12, hjust = 0.5))


###  Combined waffle charts
ggarrange(wfl_nvca, wfl_vca, nrow=2, align="hv")

###   Combined all top crop charts
elev_waffle<-ggarrange(vca_topcrop1,  vca_topcrop2,wfl_nvca, wfl_vca, 
                       align="hv" ,common.legend = TRUE, nrow=2, ncol=2, heights=c(1,1), labels="AUTO", legend="bottom")


# ggsave(file="C:/Users/f003r0x/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/Plots/elev_prox_llhood_final.pdf",
#        plot=elev_waffle,
#        dpi=600, width=6.4, height=4.25, unit="in")  

#################################################################################################################
###   HH Data Analysis###
#################################################################################################################

##Fixing treatment variable##  
survey9$trt1<-0
survey9$trt1[survey9$HVCODE3==4 | survey9$HVCODE3==36 | survey9$HVCODE3==28 | survey9$HVCODE3==40 |
               survey9$HVCODE3==14 | survey9$HVCODE3==49 | survey9$HVCODE3==45]<-1
summary(survey9$trt1)

##Generating primary occupation is farming variable##
survey9$FARMHH1<-0
survey9$FARMHH1[!is.na(survey9$FTOPCROP_1_1)]<-1
summary(survey9$FARMHH1)

survey9$FARMHH2<-0
survey9$FARMHH2[survey9$MHOCC==1] <-1
summary(survey9$FARMHH2)

##Adding real 0s to total land area##
survey9$totalLA[is.na(survey9$totalLA)]<-0
summary(survey9$totalLA)

##Adding real 0s to Ag Land Ownership##
survey9$LandOwn_Ag[is.na(survey9$LandOwn_Ag)]<-0
summary(survey9$LandOwn_Ag)

survey9$LandOwn_Ag2[is.na(survey9$LandOwn_Ag2)]<-0
summary(survey9$LandOwn_Ag2)

survey9$LandOwn_All[is.na(survey9$LandOwn_All)]<-0
summary(survey9$LandOwn_All)


##Adding cash crop variable##
#survey9$CASH_CROP<-0
#survey9$CASH_CROP[survey9$TOPCROP_CAT>=5]<-1
#summary(survey9$CASH_CROP)
#table(survey9$CASH_CROP, survey9$trt1)
#prop.test(table(survey9$CASH_CROP, survey9$trt1))

###Subsetting dataset to only have relevant variables###

survey_glm<-survey9[,c("coffee", "coffee2", "rubberop", "cinnamon", "CASH_CROP", 
              "HHMEMB_ADULTS", "ethnic", "totalincome",
              "totalLA", "mdpi", "LandOwn_Ag" , "LandOwn_Ag2" , "LandOwn_All", "trt1", "elev", "ID.y", "HVCODE", "FARMHH1", "FARMHH2", "MHAGE",
              "MHSEX", "MHEDU", "NEAR_DIST_KSNP", "COUNT.ksnplos", "COUNT.prmlos", "COUNT.seclos")]

##Adding dichotomous ag land ownership variable##
survey_glm$totalLA_dic<-0
survey_glm$totalLA_dic[survey_glm$totalLA>0]<-1
summary(survey_glm$totalLA_dic)

##Changing units of elevation, distance to KSNP, and KSNP forest loss
survey_glm$elev<-survey_glm$elev/100
survey_glm$NEAR_DIST_KSNP<-survey_glm$NEAR_DIST_KSNP/1000
survey_glm$COUNT.ksnplos<-(survey_glm$COUNT.ksnplos*900)/10000
survey_glm$COUNT.prmlos<-(survey_glm$COUNT.prmlos*900)/10000
survey_glm$COUNT.seclos<-(survey_glm$COUNT.seclos*900)/10000
survey_glm$COUNT.seclos[is.na(survey_glm$COUNT.seclos)]<-0
summary(survey_glm$elev)
summary(survey_glm$NEAR_DIST_KSNP)
summary(survey_glm$COUNT.ksnplos)
summary(survey_glm$COUNT.prmlos)
summary(survey_glm$COUNT.seclos)

##Setting variables as factors##

survey_glm$FARMHH2<-as.factor(survey_glm$FARMHH2)
survey_glm$ethnic<-as.factor(survey_glm$ethnic)
survey_glm$trt1<-as.factor(survey_glm$trt1)
survey_glm$LandOwn_Ag<-as.factor(survey_glm$LandOwn_Ag)
survey_glm$totalLA_dic<-as.factor(survey_glm$totalLA_dic)
summary(survey_glm$ethnic)

##Subsetting further to remove Nas##
survey_glm1<-survey_glm[c("CASH_CROP", "MHSEX", "MHAGE",
                          "MHEDU", "HHMEMB_ADULTS" , "FARMHH2", "ethnic", "totalLA_dic",
                          "totalLA", "totalincome","mdpi", "LandOwn_Ag", "LandOwn_Ag2", "LandOwn_All", "trt1", "elev", 
                          "ID.y", "NEAR_DIST_KSNP","COUNT.ksnplos", "COUNT.prmlos", "COUNT.seclos", "HVCODE")]

##Removing NAs##
survey_glm1<-survey_glm1[complete.cases(survey_glm1),]
              
##################################################
###Modeling propensity of household in treatment##
##################################################

survey_glm1$frm_lnd<-(as.numeric(survey_glm1$FARMHH2)-1)*(as.numeric(survey_glm1$totalLA_dic)-1)
survey_glm1$infrm_lnd<-(as.numeric(survey_glm1$LandOwn_Ag)-2)*(-1)
summary(survey_glm1$LandOwn_Ag)
summary(survey_glm1$frm_lnd)
survey_glm1$infml_lnd<-NULL

###  Model 1 accounts for only variables mentioned in project documents for village selection criteria   ###
trt1.1<-glm(trt1~
                  elev+
                  NEAR_DIST_KSNP+
                  #mdpi+
                  log(totalincome)+
                  factor(FARMHH2),
              data=survey_glm1, family="binomial")  
summary(trt1.1)
BIC(trt1.1)
#View(summary(trt1.1)$coefficients)

sandwich_se<-diag(vcovCR(trt1.1, cluster=survey_glm1$ID.y, type="CR0", method="arellano"))^.5
sandwich_se

q.val <- qnorm(0.975)

r.estm_trt1.1 <- cbind(
  Estimate = coef(trt1.1)
  , "Robust SE" = sandwich_se
  , z = (coef(trt1.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(trt1.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(trt1.1) - q.val  * sandwich_se
  , UL = coef(trt1.1) + q.val  * sandwich_se
)
#r.estm_trt1.1<-data.frame(r.estm_LA_ha)
r.estm_trt1.1
View(r.estm_trt1.1)

##  Model two account for additional demographic variables that might demonstrate differences

trt1.2<-glm((trt1)~
              elev+
              NEAR_DIST_KSNP+
              #mdpi+
              log(totalincome)+
              factor(FARMHH2)+
              MHSEX+
              MHAGE+
              MHEDU+
              HHMEMB_ADULTS,
             #factor(ethnic),
             #  factor(CASH_CROP)+
             #  log(totalincome)+
             #    mdpi+
             #factor(totalLA_dic)+
             #totalLA +
             #  factor(LandOwn_Ag),
              data=survey_glm1, family="binomial")  
summary(trt1.2)
BIC(trt1.2)
#View(summary(trt1.2)$coefficients)

sandwich_se<-diag(vcovCR(trt1.2, cluster=survey_glm1$ID.y, type="CR0", method="arellano"))^.5
sandwich_se

q.val <- qnorm(0.975)

r.estm_trt1.2 <- cbind(
  Estimate = coef(trt1.2)
  , "Robust SE" = sandwich_se
  , z = (coef(trt1.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(trt1.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(trt1.2) - q.val  * sandwich_se
  , UL = coef(trt1.2) + q.val  * sandwich_se
)
#r.estm_trt1.2<-data.frame(r.estm_LA_ha)
r.estm_trt1.2
View(r.estm_trt1.2)

###   Model 3 Contains Report and Literature Variables that predict propensity   ###

trt1.3<-glm((trt1)~
              elev+
              NEAR_DIST_KSNP+
              #mdpi+
              log(totalincome)+
              factor(FARMHH2)+
              factor(CASH_CROP)+
              factor(totalLA_dic)+
              factor(LandOwn_Ag),
            data=survey_glm1, family="binomial")  
summary(trt1.3)
BIC(trt1.3)
#View(summary(trt1.3)$coefficients)
vif(trt1.3)

sandwich_se<-diag(vcovCR(trt1.3, cluster=survey_glm1$ID.y, type="CR0", method="arellano"))^.5
sandwich_se

q.val <- qnorm(0.975)

r.estm_trt1.3 <- cbind(
  Estimate = coef(trt1.3)
  , "Robust SE" = sandwich_se
  , z = (coef(trt1.3)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(trt1.3)/sandwich_se), lower.tail = FALSE)
  , LL = coef(trt1.3) - q.val  * sandwich_se
  , UL = coef(trt1.3) + q.val  * sandwich_se
)
#r.estm_trt1.3<-data.frame(r.estm_LA_ha)
r.estm_trt1.3
View(r.estm_trt1.3)

###   Model 4 has all the variables   ###

trt1.4<-glm((trt1)~
              elev+
              NEAR_DIST_KSNP+
              #mdpi+
              log(totalincome)+
              factor(FARMHH2)+
              MHSEX+
              MHAGE+
              MHEDU+
              HHMEMB_ADULTS+
              #factor(ethnic)+
              factor(CASH_CROP)+
              factor(totalLA_dic)+
              factor(LandOwn_Ag),
            data=survey_glm1, family="binomial")  
summary(trt1.4)
BIC(trt1.4)
View(summary(trt1.4)$coefficients)
vif(trt1.4)

sandwich_se<-diag(vcovCR(trt1.4, cluster=survey_glm1$ID.y, type="CR0", method="arellano"))^.5
sandwich_se

q.val <- qnorm(0.975)

r.estm_trt1.4 <- cbind(
  Estimate = coef(trt1.4)
  , "Robust SE" = sandwich_se
  , z = (coef(trt1.4)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(trt1.4)/sandwich_se), lower.tail = FALSE)
  , LL = coef(trt1.4) - q.val  * sandwich_se
  , UL = coef(trt1.4) + q.val  * sandwich_se
)
#r.estm_trt1.4<-data.frame(r.estm_LA_ha)
r.estm_trt1.4
View(r.estm_trt1.4)


anova_trt1<-anova(trt1.1,  trt1.2, trt1.4, test="LRT")
(anova_trt1)
View(anova_trt1)

anova_trt2<-anova(trt1.1,  trt1.3, trt1.4, test="LRT")
View(anova_trt2)

########################################
###Modeling total area of agland owned##
########################################

##Models the propensity to own any land##

LA_ha1.1<-glm((totalLA_dic)~
             CASH_CROP+
             log(totalincome)+
             mdpi+
             factor(trt1),
           data=survey_glm1, family="binomial")  
summary(LA_ha1.1)
View(summary(LA_ha1.1)$coefficients)

LA_ha1.2<-glm((totalLA_dic)~
                MHSEX+
                MHAGE+
                MHEDU+
                HHMEMB_ADULTS+
                factor(FARMHH2)+
                factor(ethnic)+
                CASH_CROP+
                log(totalincome)+
                mdpi,
              data=survey_glm1, family="binomial")  
summary(LA_ha1.2)
View(summary(LA_ha1.2)$coefficients)

LA_ha1.3<-glm((totalLA_dic)~
                MHSEX+
                MHAGE+
                MHEDU+
                HHMEMB_ADULTS+
                factor(FARMHH2)+
                factor(ethnic)+
                CASH_CROP+
                log(totalincome)+
                mdpi+
                factor(trt1)+
                elev+
                NEAR_DIST_KSNP+
                COUNT.ksnplos,
              data=survey_glm1, family="binomial")  
summary(LA_ha1.3)
View(summary(LA_ha1.3)$coefficients)
vif(LA_ha1.3)


anova_LA1<-anova(LA_ha1.1, LA_ha1.2, LA_ha1.3, test="Chisq")
View(anova_LA1)

sandwich_se<-diag(vcovCR(LA_ha1.3, cluster=survey_glm1$ID.y, type="CR0", method="arellano"))^.5
sandwich_se

q.val <- qnorm(0.975)

r.estm_LA_ha1.3 <- cbind(
  Estimate = coef(LA_ha1.3)
  , "Robust SE" = sandwich_se
  , z = (coef(LA_ha1.3)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(LA_ha1.3)/sandwich_se), lower.tail = FALSE)
  , LL = coef(LA_ha1.3) - q.val  * sandwich_se
  , UL = coef(LA_ha1.3) + q.val  * sandwich_se
)
#r.estm_LA_ha<-data.frame(r.estm_LA_ha)
r.estm_LA_ha1.3
View(r.estm_LA_ha1.3)

########################################################
###Models the amount of ag land that land owners have###
########################################################

##Removing HHs without land
survey_glm1_LA2<-survey_glm1
survey_glm1_LA2$totalLA_log<-log(survey_glm1_LA2$totalLA)
survey_glm1_LA2$totalLA_log[survey_glm1_LA2$totalLA_log=="-Inf"]<-NA
summary(survey_glm1_LA2$totalLA_log)

survey_glm1_LA2<-survey_glm1_LA2[complete.cases(survey_glm1_LA2),]

LA_ha2.1<-lm(totalLA_log~
            CASH_CROP+
            log(totalincome)+
            mdpi+
            factor(LandOwn_Ag)+
            factor(trt1),
          data=survey_glm1_LA2)  
summary(LA_ha2.1)
View(summary(LA_ha2.1)$coefficients)

LA_ha2.2<-lm(totalLA_log~MHSEX+
               MHAGE+
               MHEDU+
               HHMEMB_ADULTS+
               factor(FARMHH2)+
               factor(ethnic)+
               CASH_CROP+
               log(totalincome)+
               mdpi+
               factor(LandOwn_Ag)+
               factor(trt1),
             data=survey_glm1_LA2)  
summary(LA_ha2.2)
View(summary(LA_ha2.2)$coefficients)

sandwich_se<-diag(vcovCR(LA_ha2.2, cluster=survey_glm1_LA2$ID.y, type="CR0", method="arellano"))^.5
sandwich_se

#sandwich_se<-diag(vcovHC (LA_ha, type="HC0", method="arellano"))^.5
#sandwich_se

q.val <- qnorm(0.975)

r.estm_LA_ha2.2 <- cbind(
  Estimate = coef(LA_ha2.2)
  , "Robust SE" = sandwich_se
  , z = (coef(LA_ha2.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(LA_ha2.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(LA_ha2.2) - q.val  * sandwich_se
  , UL = coef(LA_ha2.2) + q.val  * sandwich_se
)
#r.estm_LA_ha<-data.frame(r.estm_LA_ha)
r.estm_LA_ha2.2

LA_ha2.3<-lm(totalLA_log~MHSEX+
               MHAGE+
               MHEDU+
               HHMEMB_ADULTS+
               factor(FARMHH2)+
               factor(ethnic)+
               CASH_CROP+
               log(totalincome)+
               mdpi+
               factor(LandOwn_Ag)+
               factor(trt1)+
               elev+
               NEAR_DIST_KSNP+
               COUNT.ksnplos,
             data=survey_glm1_LA2)  
summary(LA_ha2.3)
View(summary(LA_ha2.3)$coefficients)
vif(LA_ha2.3)

anova_LA2<-anova(LA_ha2.1,LA_ha2.2,LA_ha2.3,test="Chisq")
View(anova_LA2)

sandwich_se<-diag(vcovCR(LA_ha2.3, cluster=survey_glm1_LA2$ID.y, type="CR0", method="arellano"))^.5
sandwich_se

#sandwich_se<-diag(vcovHC (LA_ha, type="HC0", method="arellano"))^.5
#sandwich_se

q.val <- qnorm(0.975)

r.estm_LA_ha2.3 <- cbind(
  Estimate = coef(LA_ha2.3)
  , "Robust SE" = sandwich_se
  , z = (coef(LA_ha2.3)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(LA_ha2.3)/sandwich_se), lower.tail = FALSE)
  , LL = coef(LA_ha2.3) - q.val  * sandwich_se
  , UL = coef(LA_ha2.3) + q.val  * sandwich_se
)

r.estm_LA_ha2.3
View(r.estm_LA_ha2.3)
###No matter how you cut it, treatment 1 is a significant predictor

############################################################
###Modeling propensity to own land without a formal title###
############################################################
table(factor(survey_glm1_LA2$CASH_CROP),factor(survey_glm1_LA2$LandOwn_Ag))

LA_ttl.1<-glm((LandOwn_Ag)~
                CASH_CROP+
                log(totalincome)+
                mdpi+
                factor(trt1),
              data=survey_glm1_LA2, family="binomial")  
summary(LA_ttl.1)
View(summary(LA_ttl.1)$coefficients)

LA_ttl.2<-glm((LandOwn_Ag)~
                MHSEX+
                MHAGE+
                MHEDU+
                HHMEMB_ADULTS+
                factor(FARMHH2)+
                factor(ethnic)+
                CASH_CROP+
                log(totalincome)+
                mdpi+
                factor(trt1),
              data=survey_glm1_LA2, family="binomial")  
summary(LA_ttl.2)
View(summary(LA_ttl.2)$coefficients)

LA_ttl.3<-glm((LandOwn_Ag)~
                MHSEX+
                MHAGE+
                MHEDU+
                HHMEMB_ADULTS+
                factor(FARMHH2)+
                factor(ethnic)+
                CASH_CROP+
                log(totalincome)+
                mdpi+
                factor(trt1)+
                elev+
                NEAR_DIST_KSNP,
                #COUNT.ksnplos,
              data=survey_glm1_LA2, family="binomial")  
summary(LA_ttl.3)
View(summary(LA_ttl.3)$coefficients)
vif(LA_ttl.3)


anova_ttl1<-anova(LA_ttl.1, LA_ttl.2, LA_ttl.3, test="Chisq")
View(anova_ttl1)

sandwich_se<-diag(vcovCR(LA_ttl.3, cluster=survey_glm1_LA2$ID.y, type="CR0", method="arellano"))^.5
sandwich_se

q.val <- qnorm(0.975)

r.estm_LA_ttl.3 <- cbind(
  Estimate = coef(LA_ttl.3)
  , "Robust SE" = sandwich_se
  , z = (coef(LA_ttl.3)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(LA_ttl.3)/sandwich_se), lower.tail = FALSE)
  , LL = coef(LA_ttl.3) - q.val  * sandwich_se
  , UL = coef(LA_ttl.3) + q.val  * sandwich_se
)
#r.estm_LA_ha<-data.frame(r.estm_LA_ha)
r.estm_LA_ttl.3
View(r.estm_LA_ttl.3)
###No matter how you cut it, treatment 1 is a significant predictor

###############################################
###Modeling propensity to produce cash crops###
###############################################

cashc_prop0<-glm(CASH_CROP~
                   totalLA+
                   log(totalincome)+
                   mdpi+
                   factor(LandOwn_Ag)+
                   factor(trt1),
                 data=survey_glm1, family="binomial")  
summary(cashc_prop0)
View(summary(cashc_prop0)$coefficients)

cashc_prop1<-glm(CASH_CROP~MHSEX+
                  MHAGE+
                  MHEDU+
                  HHMEMB_ADULTS+
                  factor(FARMHH2)+
                  factor(ethnic)+
                  totalLA+
                  log(totalincome)+
                  mdpi+
                  factor(LandOwn_Ag)+
                  factor(trt1),
              data=survey_glm1, family="binomial")  
summary(cashc_prop1)
View(summary(cashc_prop1)$coefficients)


cashc_prop2<-glm(CASH_CROP~MHSEX+
                  MHAGE+
                  MHEDU+
                  HHMEMB_ADULTS+
                  factor(FARMHH2)+
                  factor(ethnic)+
                  totalLA+
                  log(totalincome)+
                  mdpi+
                  factor(LandOwn_Ag)+
                  factor(trt1)+
                  elev+
                  NEAR_DIST_KSNP+
                  COUNT.ksnplos,
                data=survey_glm1, family="binomial")  
summary(cashc_prop2)
View(summary(cashc_prop2)$coefficients)
vif(cashc_prop2)

anova1<-anova(cashc_prop0,cashc_prop1,cashc_prop2, test="Chisq")

View(anova1)

sandwich_se<-diag(vcovCR(cashc_prop2, cluster=survey_glm1$ID.y, type="CR0", method="arellano"))^.5
sandwich_se

#sandwich_se<-diag(vcovHC (cashc_prop2, type="HC0", method="arellano"))^.5
#sandwich_se

q.val <- qnorm(0.975)

r.estm_cashc_prop2 <- cbind(
  Estimate = coef(cashc_prop2)
  , "Robust SE" = sandwich_se
  , z = (coef(cashc_prop2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(cashc_prop2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(cashc_prop2) - q.val  * sandwich_se
  , UL = coef(cashc_prop2) + q.val  * sandwich_se
)
r.estm_cashc_prop2

r.estm_cashc_prop2<-data.frame(r.estm_cashc_prop2)
r.estm_cashc_prop2
View(r.estm_cashc_prop2)
View(r.estm_cashc_prop2$Pr...z...)


trt_prop<-glm(trt1~
                HMEMB_3_1_1+
                HMEMB_2_1_1+
                HHMEMB_ADULTS+
                factor(ethnic)+
                factor(FARMHH)+
                log(totalincome)+
                totalLA+
                CASH_CROP+
                elev+
                factor(ID.y),
              data=survey9, family="binomial")  
summary(trt_prop)

sandwich_se<-diag(vcovCR(trt_prop, cluster=survey9$ID.y, type="CR0", method="arellano"))^.5
sandwich_se

q.val <- qnorm(0.975)

r.estm_trt_prop <- cbind(
  Estimate = coef(trt_prop)
  , "Robust SE" = sandwich_se
  , z = (coef(trt_prop)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(trt_prop)/sandwich_se), lower.tail = FALSE)
  , LL = coef(trt_prop) - q.val  * sandwich_se
  , UL = coef(trt_prop) + q.val  * sandwich_se
)
r.estm_trt_prop

###Interpretation###
## Although being in the treatment is a significant predictor for coffee production without village clusters,
## with village clusters it is no longer significant. This means that, when taking the sample design into account
## there is no greater propensity for a HH in a treatment to produce coffee than for one outside of it. However,
## Land Holding is significantly predicted by treatment, and Land Holding significantly predicts coffee

################################################################################################################

###Modeling HH income##

################################################################################################################

survey_glm$mdpi<-survey_glm$mdpi*10

inc1.0 <- lm(log(totalincome) ~
               factor(FARMHH2)+
               mdpi+
               factor(LandOwn_Ag)+
               totalLA +
               factor(trt1)*CASH_CROP,
             data=survey_glm1)
summary(inc1.0)

inc1.1 <- lm(log(totalincome) ~
               MHSEX+
               MHAGE+
               MHEDU+
               HHMEMB_ADULTS+
               factor(FARMHH2)+
               factor(ethnic)+
               mdpi+
               factor(LandOwn_Ag)+
               totalLA +
               factor(trt1)*CASH_CROP,
             data=survey_glm1)
summary(inc1.1)

inc1.2.1 <- lm(log(totalincome) ~
                 MHSEX+
                 MHAGE+
                 MHEDU+
                 HHMEMB_ADULTS+
                 factor(FARMHH2)+
                 factor(ethnic)+
                 mdpi+
                 factor(LandOwn_Ag)+
                 totalLA +
                 elev+
                 NEAR_DIST_KSNP+
                 COUNT.ksnplos+
                 factor(trt1)*CASH_CROP,
               data=survey_glm1)
summary(inc1.2.1)

sandwich_se<-diag(vcovCR(fm1.2.1,  cluster=survey_glm1$ID.y, type="CR0", method="arellano"))^.5
sandwich_se

#sandwich_se<-diag(vcovHC (cof_prop, type="HC0", method="arellano"))^.5
#sandwich_se

q.val <- qnorm(0.975)

r.estm_fm1.2.1 <- cbind(
  Estimate = coef(fm1.2.1)
  , "Robust SE" = sandwich_se
  , z = (coef(fm1.2.1)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(fm1.2.1)/sandwich_se), lower.tail = FALSE)
  , LL = coef(fm1.2.1) - q.val  * sandwich_se
  , UL = coef(fm1.2.1) + q.val  * sandwich_se
)

r.estm_fm1.2.1
View(r.estm_fm1.2.1)

survey_glm1$mdpi<-survey_glm1$mdpi*100

inc1.2.2 <- lm(log(totalincome) ~
                MHSEX+
                MHAGE+
                MHEDU+
                HHMEMB_ADULTS+
                factor(FARMHH2)+
                factor(ethnic)+
                CASH_CROP +
                mdpi+
                factor(LandOwn_Ag)+
                totalLA*factor(trt1)+
                elev+
                NEAR_DIST_KSNP+
                COUNT.ksnplos, data=survey_glm1)
summary(inc1.2.2)
qqnorm (resid(inc1.2.2))
qqline(resid(inc1.2.2))

anova_inc<-anova(inc1.0, inc1.1, inc1.2.2, test="Chisq")
View(anova_inc)

jtools::interact_plot(inc1.2.2, pred = "totalLA", modx = "trt1", cluster="ID.y", y.label="Log of Total Income (IDR)",
                      x.label="Land Owned (Ha)", legend.main="HH Type", modx.labels=c("non-VCA","VCA"),data=survey_glm1[survey_glm1$totalLA<10,])

sandwich_se<-diag(vcovCR(inc1.2.2,  cluster=survey_glm1$ID.y, type="CR0", method="arellano"))^.5
sandwich_se

#sandwich_se<-diag(vcovHC (cof_prop, type="HC0", method="arellano"))^.5
#sandwich_se

q.val <- qnorm(0.975)

r.estm_inc1.2.2 <- cbind(
   Estimate = coef(inc1.2.2)
  , "Robust SE" = sandwich_se
  , z = (coef(inc1.2.2)/sandwich_se)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(inc1.2.2)/sandwich_se), lower.tail = FALSE)
  , LL = coef(inc1.2.2) - q.val  * sandwich_se
  , UL = coef(inc1.2.2) + q.val  * sandwich_se
)

r.estm_inc1.2.2
View(r.estm_inc1.2.2)
vif(inc1.2.2)

r.est_inc1.2.2<-as.data.frame(r.estm_inc1.2.2)

r.est_inc1.2.2$row.names<-row.names(r.est_inc1.2.2)

r.est_inc1.2.2$sig<-r.est_inc1.2.2$"Pr(>|z|)"
r.est_inc1.2.2$sig[r.est_inc1.2.2$sig>.1]<-2
r.est_inc1.2.2$sig[r.est_inc1.2.2$sig<=.1]<-1
r.est_inc1.2.2$sig[r.est_inc1.2.2$sig==2]<-.5

View(r.est_inc1.2.2)

###Creating subsetted data for plotting different coefficients###

df1.2.1<-r.est_inc1.2.2[2:18,]


###VISUALIZING MODEL COEFFICIENTS###


##Changing mdpi to percent##

##Can't call the interaction term, so manually replacing row.name##
df1.2.1[17,7]<-"interaction "


##ggplot of fixed effect variables of interest##
fm1.2_coplot1 <- ggplot(df1.2.1, aes(x=row.names, y=Estimate)) + 
  geom_pointrange(aes(ymin=LL, ymax=UL, alpha=sig), color = "black") + 
  scale_x_discrete(labels = c("CASH_CROP"="High Value\nTree Crop",
                                           "MHAGE" = "Age HH Head", 
                                           "MHSEX" = "Female HH Head" ,
                                           "MHEDU" = "Ed. HH Head",
                                           "factor(FARMHH2)1" = "Ag. Primary HH Income",
                                           "HHMEMB_ADULTS" = "No. of Adults",
                                           "factor(ethnic)2" = "HH Head Melayu Jambi",
                                           "factor(ethnic)3"= "HH Head Javanese",
                                           "factor(ethnic)4" = "HH Head Kerinci",
                                           "totalLA" = "Total Ha Owned",
                                           "factor(LandOwn_Ag)1" = "Formal Land Title",
                                           "factor(trt1)1" = "Formal Land Title",
                                           "elev" = "HH Elevation",
                                           "mdpi" = "MPI (%)",
                                           "NEAR_DIST_KSNP" = "Distance to KSNP",
                                           "COUNT.ksnplos" = "KSKNP Forest Loss",
                                           "interaction " = "VCA-Ha Interaction"),
                   limits=c("interaction ", "COUNT.ksnplos", "NEAR_DIST_KSNP", "elev", "factor(trt1)1",
                            "totalLA", "factor(LandOwn_Ag)1", "mdpi", "CASH_CROP",  "factor(ethnic)4", 
                            "factor(ethnic)3", "factor(ethnic)2",
                            "factor(FARMHH2)1", "HHMEMB_ADULTS",  "MHEDU",  "MHAGE", "MHSEX")) + 
  scale_alpha_continuous(range = c(.3, 1), guide='none')+
  geom_hline(yintercept=0, color="black") + 
  theme(text = element_text(size=10)) +
  ylab(NULL) +
  xlab(NULL) +
  theme_bw() + 
  coord_flip(ylim = c(-0.3,0.3))

# ggsave(file="C:/Users/f003r0x/Dropbox/#michigan phd/1.Dissertation/Ch5-Kerinci/inc_coef_plot.png",
#        #plot="ksnp_forloss1",
#        dpi=600, width=3, height=4, unit="in")  


#################################################################################################################

###KADES RESPONSES ###

################################################################################################################

xwalk<-read.csv("C:/Users/f003r0x/Dropbox/Library/1.Data/Dissertation_Data/Spatial_Datatables/xwalk_wide_mergedalld2_lags2.csv")

#Subsetting xwalk for only 2003 and 2014 IDs

xwalkids<-xwalk[,c("ID.2003","ID.2014")]
kades_ids<-merge(kades, xwalkids,by.x="ID2013_d", by.y="ID.2014", all.x=T)
kades_all<-merge(kades_ids,survey_vil5,by.x="ID2013_d", by.y="ID")

kades_all$VCA

##############################################################################
###   Pie Charts
##############################################################################

pie_tab<-count(survey9, trt1, TOPCROP_CAT) %>% ungroup()
pie_tab$TOPCROP_CAT[is.na(pie_tab$TOPCROP_CAT)]<-8
pie_tab$prop<-0
pie_tab$prop[pie_tab$trt1==0]<-pie_tab$n[pie_tab$trt1==0]/1125
pie_tab$prop[pie_tab$trt1==1]<-pie_tab$n[pie_tab$trt1==1]/179

##Pie chart for non-vca HHs
sum(pie_tab$nn[pie_tab$trt1==0 & pie_tab$TOPCROP_CAT==8])
#sum of hhs with top crop is 797
bp1<-ggplot(pie_tab[pie_tab$trt1==0 & pie_tab$TOPCROP_CAT<8,], 
            aes(x=0.7,y=n/797, fill=factor(TOPCROP_CAT)))+
  scale_fill_manual(name="Crop Type", 
                    values=tol6qualitative,
                    breaks=c("1","2","5", "6", "7", "3"), 
                    labels=c("Rice", "Tubers", "Coffee", 
                             "Cinnamon","Rubber/Oil Palm","Other"))+
  geom_bar(width = 1, stat="identity")+
  #geom_text(aes(x=1.65, y=nn/797, label=nn/797))+
  labs(y="Proportion of non-VCA Households", x=" ")+
  coord_polar(theta="y")+
  theme_bw()+theme(panel.border=element_blank())+
  theme(plot.margin=margin(t=1,r=15,b=1,l=15, "pt"))+
  theme(axis.text.y=element_blank(), axis.text.x=element_blank(),axis.ticks=element_blank(), panel.grid=element_blank())+
  scale_x_discrete(limits=c(0, 1))

bp1

bp2<-ggplot(pie_tab[pie_tab$trt1==1 & pie_tab$TOPCROP_CAT<8,], 
            aes(x=0.7,y=nn/164, fill=factor(TOPCROP_CAT)))+
  scale_fill_manual(name="Crop Type", 
                    values=tol7qualitative,
                    breaks=c("1","2","5", "6", "7", "3","4" ), 
                    labels=c("Rice", "Tubers", "Coffee", 
                             "Cinnamon","Rubber/Oil Palm","Other Veg.", "Other Hort."))+
  geom_bar(width = 1, stat="identity")+
  labs(y="Proportion of VCA Households", x="")+
  coord_polar(theta="y")+
  theme_bw()+theme(panel.border=element_blank())+
  theme(plot.margin=margin(t=1,r=15,b=1,l=15, "pt"))+
  theme(axis.text.y=element_blank(), axis.text.x=element_blank(),axis.ticks=element_blank(), panel.grid=element_blank())+
  scale_x_discrete(limits=c(0, 1))


bp2

ggarrange(bp1,bp2,common.legend = TRUE)

###Pie and elevation/crop charts###

elev_pie<-ggarrange(vca_topcrop1,  vca_topcrop2,bp1,bp2, align="hv",common.legend = TRUE,nrow=2,ncol=2,heights=c(1,1),labels="AUTO" ,legend="top")

# ggsave(file="C:/Users/f003r0x/Dropbox/#michigan phd/1.Dissertation/Ch5-Kerinci/elev_prox_llhood_final.png",
#        plot=elev_pie,
#        dpi=600, width=6, height=4, unit="in")  
