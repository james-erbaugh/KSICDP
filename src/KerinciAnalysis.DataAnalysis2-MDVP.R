###Data Step 2 for KSNP MDVPI Analysis###

install.packages("dplyr")
install.packages("sandwich")
install.packages("vcov")
install.packages("ggpubr")
install.packages("tidyverse")
install.packages("haven")
install.packages("car")
install.packages("MatchIt")
install.packages("plotly")
install.packages("plm")
install.packages("clubSandwich")
install.packages("HMisc")
install.packages("haven")


library(dplyr)
library(sandwich)
library(vcov)
library(ggpubr)
library(tidyverse)
library(haven)
library(car)
library(MatchIt)
library(plotly)
library(plm)
library(clubSandwich)
library(Hmisc)
library(haven)
library(lmtest)


#####################################################################################################################################
###   Data Step   ###################################################################################################################
#####################################################################################################################################

spatpod_fullset<-read.csv("C:/Users/JTErbaugh/Dropbox/Library/1.Data/Dissertation_Data/Spatial_Datatables/xwalk_wide_mergedalld2_lags2.csv")
longset<-read.csv("C:/Users/JTErbaugh/Dropbox/#michigan phd/1.Dissertation/Ch5-Kerinci/ksnp0314_long.csv")
##This dataset comes from "KerinciAnalysis.DataStep1 . . . "
ksnpvca<-read.csv("C:/Users/JTErbaugh/Dropbox/#michigan phd/1.Dissertation/Ch5-Kerinci/KSNP-VilNames-ICDP_HKD.csv")
yrforloss_KSNP_wts<-read.csv("C:/Users/JTErbaugh/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/Data/yrforloss_KSNP_wts.csv")
spatpod.ksnp_mtch_all<-read.csv("C:/Users/JTErbaugh/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/Data/spatpod.ksnp_mtch_all.csv")
  
#####################################################################################################################################
###   Subsetting  yrforloss_KSNP_wts dataset and redefining forest change values to correspond to years    ##########################
###   2000, 2003, 2006, 2008, 2011, and 2014   ######################################################################################
#####################################################################################################################################

###   KSNP Forest Loss Area
mdvd_data<-yrforloss_KSNP_wts %>%
  select(ID2003_d, year, dstrds, VCA, HKD, HKDII, KSNP_AreaForLoss_Ha2, nonKSNP_AreaForLoss_Ha, All_AreaForLoss_Ha)

mdvd_data$KSNP_AreaForLoss_Ha2_trunc
mdvd_data$KSNP_AreaForLoss_Ha2_trunc[mdvd_data$year==2003]<-mdvd_data$KSNP_AreaForLoss_Ha2[mdvd_data$year==2001]+
  mdvd_data$KSNP_AreaForLoss_Ha2[mdvd_data$year==2002]+ mdvd_data$KSNP_AreaForLoss_Ha2[mdvd_data$year==2003] 

mdvd_data$KSNP_AreaForLoss_Ha2_trunc[mdvd_data$year==2006]<-mdvd_data$KSNP_AreaForLoss_Ha2[mdvd_data$year==2004]+
  mdvd_data$KSNP_AreaForLoss_Ha2[mdvd_data$year==2005]+ mdvd_data$KSNP_AreaForLoss_Ha2[mdvd_data$year==2006] 

mdvd_data$KSNP_AreaForLoss_Ha2_trunc[mdvd_data$year==2008]<-mdvd_data$KSNP_AreaForLoss_Ha2[mdvd_data$year==2007]+
  mdvd_data$KSNP_AreaForLoss_Ha2[mdvd_data$year==2008]

mdvd_data$KSNP_AreaForLoss_Ha2_trunc[mdvd_data$year==2011]<-mdvd_data$KSNP_AreaForLoss_Ha2[mdvd_data$year==2009]+
  mdvd_data$KSNP_AreaForLoss_Ha2[mdvd_data$year==2010]+ mdvd_data$KSNP_AreaForLoss_Ha2[mdvd_data$year==2011] 

mdvd_data$KSNP_AreaForLoss_Ha2_trunc[mdvd_data$year==2014]<-mdvd_data$KSNP_AreaForLoss_Ha2[mdvd_data$year==2012]+
  mdvd_data$KSNP_AreaForLoss_Ha2[mdvd_data$year==2013]+ mdvd_data$KSNP_AreaForLoss_Ha2[mdvd_data$year==2014]

summary(mdvd_data$KSNP_AreaForLoss_Ha2_trunc)

###   Non-KSNP Forest Loss Area
mdvd_data$All_AreaForLoss_Ha_trunc
mdvd_data$All_AreaForLoss_Ha_trunc[mdvd_data$year==2003]<-mdvd_data$All_AreaForLoss_Ha[mdvd_data$year==2001]+
  mdvd_data$All_AreaForLoss_Ha[mdvd_data$year==2002]+ mdvd_data$All_AreaForLoss_Ha[mdvd_data$year==2003] 

mdvd_data$All_AreaForLoss_Ha_trunc[mdvd_data$year==2006]<-mdvd_data$All_AreaForLoss_Ha[mdvd_data$year==2004]+
  mdvd_data$All_AreaForLoss_Ha[mdvd_data$year==2005]+ mdvd_data$All_AreaForLoss_Ha[mdvd_data$year==2006] 

mdvd_data$All_AreaForLoss_Ha_trunc[mdvd_data$year==2008]<-mdvd_data$All_AreaForLoss_Ha[mdvd_data$year==2007]+
  mdvd_data$All_AreaForLoss_Ha[mdvd_data$year==2008]

mdvd_data$All_AreaForLoss_Ha_trunc[mdvd_data$year==2011]<-mdvd_data$All_AreaForLoss_Ha[mdvd_data$year==2009]+
  mdvd_data$All_AreaForLoss_Ha[mdvd_data$year==2010]+ mdvd_data$All_AreaForLoss_Ha[mdvd_data$year==2011] 

mdvd_data$All_AreaForLoss_Ha_trunc[mdvd_data$year==2014]<-mdvd_data$All_AreaForLoss_Ha[mdvd_data$year==2012]+
  mdvd_data$All_AreaForLoss_Ha[mdvd_data$year==2013]+ mdvd_data$All_AreaForLoss_Ha[mdvd_data$year==2014]

summary(mdvd_data$All_AreaForLoss_Ha_trunc)


###   All Forest Loss Area
mdvd_data$nonKSNP_AreaForLoss_Ha_trunc
mdvd_data$nonKSNP_AreaForLoss_Ha_trunc[mdvd_data$year==2003]<-mdvd_data$nonKSNP_AreaForLoss_Ha[mdvd_data$year==2001]+
  mdvd_data$nonKSNP_AreaForLoss_Ha[mdvd_data$year==2002]+ mdvd_data$nonKSNP_AreaForLoss_Ha[mdvd_data$year==2003] 

mdvd_data$nonKSNP_AreaForLoss_Ha_trunc[mdvd_data$year==2006]<-mdvd_data$nonKSNP_AreaForLoss_Ha[mdvd_data$year==2004]+
  mdvd_data$nonKSNP_AreaForLoss_Ha[mdvd_data$year==2005]+ mdvd_data$nonKSNP_AreaForLoss_Ha[mdvd_data$year==2006] 

mdvd_data$nonKSNP_AreaForLoss_Ha_trunc[mdvd_data$year==2008]<-mdvd_data$nonKSNP_AreaForLoss_Ha[mdvd_data$year==2007]+
  mdvd_data$nonKSNP_AreaForLoss_Ha[mdvd_data$year==2008]

mdvd_data$nonKSNP_AreaForLoss_Ha_trunc[mdvd_data$year==2011]<-mdvd_data$nonKSNP_AreaForLoss_Ha[mdvd_data$year==2009]+
  mdvd_data$nonKSNP_AreaForLoss_Ha[mdvd_data$year==2010]+ mdvd_data$nonKSNP_AreaForLoss_Ha[mdvd_data$year==2011] 

mdvd_data$nonKSNP_AreaForLoss_Ha_trunc[mdvd_data$year==2014]<-mdvd_data$nonKSNP_AreaForLoss_Ha[mdvd_data$year==2012]+
  mdvd_data$nonKSNP_AreaForLoss_Ha[mdvd_data$year==2013]+ mdvd_data$nonKSNP_AreaForLoss_Ha[mdvd_data$year==2014]

summary(mdvd_data$nonKSNP_AreaForLoss_Ha_trunc)

#####################################################################################################################################
###   Creating analysis dataset, with summed forest change values, PODES population data, and MDVD data   ###########################
#####################################################################################################################################

###   Creating original dataset (i.e. no imputation)   ##############################################################################

###   Limiting dataset to PODES years   

mdvd_data_analysis<-mdvd_data %>% 
  filter(year==2000 | year==2003 | year==2006| year==2008 | year==2011 | year==2014)

###   Merging forest loss data with podes data   ###

podes_forloss_KSNP_mdvd<-merge(longset, mdvd_data_analysis, by=c("ID2003_d", "year"), all.x=T)

podes_forloss_KSNP_mdvd$ID2003_d[is.na(podes_forloss_KSNP_mdvd$All_AreaForLoss_Ha_trunc)]
##The 10 observations that do not align between "longset" and "mdvd_data_analysis" are all lake (888) or forest (999) values.
##Dropping those values

podes_forloss_KSNP_mdvd2<-podes_forloss_KSNP_mdvd %>% 
  filter(!is.na(All_AreaForLoss_Ha_trunc))

###   Imputing values for missing data in population and mdvd   ######################################################################

impset1<-podes_forloss_KSNP_mdvd2 %>% 
  select(ID2003_d , year , forlag_MEAN.2003 , VCA.y , elev_MEAN.2003 , ovslp_MEAN.2003 , dstrds.y ,
           PAs_MEAN , hhpop , prmmrkt , mdvp , KSNP_AreaForLoss_Ha2_trunc ,
           All_AreaForLoss_Ha_trunc , nonKSNP_AreaForLoss_Ha_trunc)
  

impute_ksnpvils1 <- aregImpute(~ ID2003_d + year + forlag_MEAN.2003 + VCA.y + elev_MEAN.2003 + ovslp_MEAN.2003 + dstrds.y +
                                PAs_MEAN + hhpop + prmmrkt + mdvp + KSNP_AreaForLoss_Ha2_trunc +
                                All_AreaForLoss_Ha_trunc + nonKSNP_AreaForLoss_Ha_trunc, 
                              data = impset1, nk=0,  n.impute = 500)
impute_ksnpvils1

mean(impute_ksnpvils1$imputed$hhpop)

# Get the imputed values
imputed1 <-impute.transcan(impute_ksnpvils1, data=impset1, imputation=1, list.out=TRUE, pr=FALSE, check=FALSE)
# convert the list to the database
imputed.data1 <- as.data.frame(do.call(cbind,imputed1))
# arrange the columns accordingly
imputed.data1 <- imputed.data1[, colnames(impset1), drop = FALSE]

###   Subsetting imputed dataset and adding imputed columns to original dataset  

imputed.data_vars1 <- imputed.data1 %>% 
  select(hhpop, mdvp, prmmrkt) %>% 
  dplyr::rename(hhpop_imp=hhpop, mdvp_imp=mdvp, prmmrkt_imp=prmmrkt)

mdvp_analysis_imp<-cbind(podes_forloss_KSNP_mdvd2, imputed.data_vars1)

mdvp_analysis_imp$hhpop[is.na(mdvp_analysis_imp$hhpop)]<-mdvp_analysis_imp$hhpop_imp[is.na(mdvp_analysis_imp$hhpop)]
mdvp_analysis_imp$mdvp[is.na(mdvp_analysis_imp$mdvp)]<-mdvp_analysis_imp$mdvp_imp[is.na(mdvp_analysis_imp$mdvp)]
mdvp_analysis_imp$prmmrkt[is.na(mdvp_analysis_imp$prmmrkt)]<-mdvp_analysis_imp$prmmrkt_imp[is.na(mdvp_analysis_imp$prmmrkt)]

summary(mdvp_analysis_imp$hhpop)
summary(mdvp_analysis_imp$mdvp)
summary(mdvp_analysis_imp$prmmrkt)


###   Subsetting the variables for analysis (Regular and imputed datasets)   ########################################################

mdvd_analysis<-podes_forloss_KSNP_mdvd2 %>% 
  select(ID2003_d, mdvp, year, Area2003, ev_frac1, ev_frac2, PAs_MEAN, hhpop, prmmrkt, VCA.y, dstrds.y, HKD, HKDII, KSNP_AreaForLoss_Ha2_trunc, 
         All_AreaForLoss_Ha_trunc, nonKSNP_AreaForLoss_Ha_trunc)

sum(is.na(mdvd_analysis))

mdvd_analysis_imp<-mdvp_analysis_imp %>% 
  select(ID2003_d, mdvp, year, Area2003, ev_frac1, ev_frac2, PAs_MEAN, hhpop, prmmrkt, VCA.y, dstrds.y, HKD, HKDII, KSNP_AreaForLoss_Ha2_trunc, 
         All_AreaForLoss_Ha_trunc, nonKSNP_AreaForLoss_Ha_trunc)

######################################################################################################################################
###   Creating Matching Dataset for matching weights   ###############################################################################
######################################################################################################################################

###   Merging 2003 values of tree-cover change with the matching dataset   ###

tcchange_merge<-mdvd_data %>% 
  filter(year==2003) %>%
  select(-VCA, -HKDII)

mdvp_mtch_all1<-merge(spatpod.ksnp_mtch_all, tcchange_merge, by="ID2003_d", all.x=T)

str(mdvp_mtch_all1)

which(is.na(mdvp_mtch_all1), arr.ind=TRUE)

###   Note: 27 missing MDVP values and 24 missing hhpop values

###   Imputing missing values that are important for modeling   ###

###subsetting only those vars that I want to use for imputation###

impvars2<-c("ID2003_d", "pop.2003_pds" , "mdvp.2003" , "Area2003" , "elev_MEAN.2003" , "ovslp_MEAN.2003" , "dstrds00_MEAN.2003" ,
  "dstrds16_MEAN.2003" , "PAs_MEAN.2003" , "prmforlag_MEAN.2003" , "dstrds.2003" , "pop.2003" ,
  "AreaNonFor_3km" , "AreaFor_3km" , "VCA" , "AreaConc" , "KSNP_AreaForLoss_Ha2_trunc" ,
  "All_AreaForLoss_Ha_trunc" , "nonKSNP_AreaForLoss_Ha_trunc")

impset2<-mdvp_mtch_all1[impvars2]

impute_ksnpvils2 <- aregImpute(~ ID2003_d + pop.2003_pds + mdvp.2003 + Area2003 + elev_MEAN.2003 + ovslp_MEAN.2003 + dstrds00_MEAN.2003 +
                                dstrds16_MEAN.2003 + PAs_MEAN.2003 + prmforlag_MEAN.2003 + dstrds.2003 + pop.2003 +
                                AreaNonFor_3km + AreaFor_3km + VCA + AreaConc + KSNP_AreaForLoss_Ha2_trunc +
                                All_AreaForLoss_Ha_trunc + nonKSNP_AreaForLoss_Ha_trunc, 
                              data = impset2, nk=0,  n.impute = 500)
impute_ksnpvils2

mean(impute_ksnpvils2$imputed$pop.2003_pds)

# Get the imputed values
imputed2 <-impute.transcan(impute_ksnpvils2, data=impset2, imputation=1, list.out=TRUE, pr=FALSE, check=FALSE)
# convert the list to the database
imputed.data2 <- as.data.frame(do.call(cbind,imputed2))
# arrange the columns accordingly
imputed.data2 <- imputed.data2[, colnames(impset2), drop = FALSE]

###Subsetting imputed dataset and adding imputed columns to original dataset###

imputed.data_vars2 <- imputed.data2 %>% 
  select(pop.2003_pds, mdvp.2003) %>% 
  dplyr::rename(pop.2003_pds_imp=pop.2003_pds,
         mdvp.2003_imp=mdvp.2003)

mdvp_mtch_all_imp<-cbind(mdvp_mtch_all1, imputed.data_vars2)

###   Creating Matching Weights   ##################################################################################################

###Replacing NA values with imputed values###
mdvp_mtch_all_imp$mdvp.2003[is.na(mdvp_mtch_all_imp$mdvp.2003)]<-mdvp_mtch_all_imp$mdvp.2003_imp[is.na(mdvp_mtch_all_imp$mdvp.2003)]
summary(mdvp_mtch_all_imp$mdvp.2003)
summary(mdvp_mtch_all_imp$mdvp.2003_imp)

mdvp_mtch_all_imp$pop.2003_pds[is.na(mdvp_mtch_all_imp$pop.2003_pds)]<-mdvp_mtch_all_imp$pop.2003_pds_imp[is.na(mdvp_mtch_all_imp$pop.2003_pds)]
summary(mdvp_mtch_all_imp$pop.2003_pds)
summary(mdvp_mtch_all_imp$pop.2003_pds_imp)

sum(is.na(mdvp_mtch_imp))

###Scaling all variables###

mdvp_mtch_all_imp$Area2003_z <- (mdvp_mtch_all_imp$Area2003 - mean(mdvp_mtch_all_imp$Area2003)) / sd(mdvp_mtch_all_imp$Area2003)
mdvp_mtch_all_imp$elev_MEAN.2003_z <- (mdvp_mtch_all_imp$elev_MEAN.2003 - mean(mdvp_mtch_all_imp$elev_MEAN.2003)) / sd(mdvp_mtch_all_imp$elev_MEAN.2003)
mdvp_mtch_all_imp$ovslp_MEAN.2003_z <- (mdvp_mtch_all_imp$ovslp_MEAN.2003 - mean(mdvp_mtch_all_imp$ovslp_MEAN.2003)) / sd(mdvp_mtch_all_imp$ovslp_MEAN.2003)
mdvp_mtch_all_imp$dstrds00_MEAN.2003_z <- (mdvp_mtch_all_imp$dstrds00_MEAN.2003 - mean(mdvp_mtch_all_imp$dstrds00_MEAN.2003)) / sd(mdvp_mtch_all_imp$dstrds00_MEAN.2003)
mdvp_mtch_all_imp$PAs_MEAN.2003_z <- (mdvp_mtch_all_imp$PAs_MEAN.2003 - mean(mdvp_mtch_all_imp$PAs_MEAN.2003)) / sd(mdvp_mtch_all_imp$PAs_MEAN.2003)
mdvp_mtch_all_imp$prmforlag_MEAN.2003_z <- (mdvp_mtch_all_imp$prmforlag_MEAN.2003 - mean(mdvp_mtch_all_imp$prmforlag_MEAN.2003)) / sd(mdvp_mtch_all_imp$prmforlag_MEAN.2003)
mdvp_mtch_all_imp$AreaNonFor_3km_z <- (mdvp_mtch_all_imp$AreaNonFor_3km - mean(mdvp_mtch_all_imp$AreaNonFor_3km, na.rm=T)) / sd(mdvp_mtch_all_imp$AreaNonFor_3km, na.rm=T)
mdvp_mtch_all_imp$AreaFor_3km_z <- (mdvp_mtch_all_imp$AreaFor_3km - mean(mdvp_mtch_all_imp$AreaFor_3km, na.rm=T)) / sd(mdvp_mtch_all_imp$AreaFor_3km, na.rm=T)
mdvp_mtch_all_imp$AreaConc_z <- (mdvp_mtch_all_imp$AreaConc - mean(mdvp_mtch_all_imp$AreaConc, na.rm=T)) / sd(mdvp_mtch_all_imp$AreaConc, na.rm=T)
mdvp_mtch_all_imp$pop.2003_pds_z <- (mdvp_mtch_all_imp$pop.2003_pds - mean(mdvp_mtch_all_imp$pop.2003_pds, na.rm=T)) / sd(mdvp_mtch_all_imp$pop.2003_pds, na.rm=T)
mdvp_mtch_all_imp$All_AreaForLoss_Ha_trunc_z <- (mdvp_mtch_all_imp$All_AreaForLoss_Ha_trunc - mean(mdvp_mtch_all_imp$All_AreaForLoss_Ha_trunc, na.rm=T)) / sd(mdvp_mtch_all_imp$All_AreaForLoss_Ha_trunc, na.rm=T)


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
              pop.2003_pds_z+
              All_AreaForLoss_Ha_trunc_z,
            #mdvp.2003_z,
            data=mdvp_mtch_all_imp, family="binomial")
summary(psm1.1)

##   Dropping Lagged Primary Forest Cover
psm1.2<-glm(VCA~Area2003_z + 
              elev_MEAN.2003_z + 
              ovslp_MEAN.2003_z + 
              dstrds00_MEAN.2003_z + 
              PAs_MEAN.2003_z + 
              AreaNonFor_3km_z + 
              AreaFor_3km_z +
              AreaConc_z+
              pop.2003_pds_z+
              All_AreaForLoss_Ha_trunc_z,
            data=mdvp_mtch_all_imp, family="binomial")
summary(psm1.2)

##Dropping Area and buffer forest area

psm1.3<-glm(VCA~#Area2003_z + 
              elev_MEAN.2003_z + 
              ovslp_MEAN.2003_z + 
              dstrds00_MEAN.2003_z + 
              #prmforlag_MEAN.2003_z +
              PAs_MEAN.2003_z + 
              AreaNonFor_3km_z + 
              #AreaFor_3km_z +
              AreaConc_z+
              pop.2003_pds_z+
              All_AreaForLoss_Ha_trunc_z,
            data=mdvp_mtch_all_imp, family="binomial")
summary(psm1.3)

anova(psm1.3, psm1.2, psm1.1, test="Chisq")

###Models are nonsignificantly different

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
              pop.2003_pds_z+
              All_AreaForLoss_Ha_trunc_z,
            data=mdvp_mtch_all_imp, family="binomial")
summary(psm1.2)

vif(psm1.2)

mdvp_mtch_all_imp$ps03.1<- predict(psm1.2, type = "response")

rm(psm1.1, psm1.2, psm1.3)

sum(is.na(mdvp_mtch_all_imp))
which(is.na(mdvp_mtch_all_imp), arr.ind=TRUE)
View(mdvp_mtch_all_imp)


###Match 1.0: Mahalanobis within calipers##########################################################################

mahdataset<-data.frame(mdvp_mtch_all_imp)
row.names(mahdataset)
row.names(mahdataset)<- paste0('X', row.names(mahdataset))

rm(mdvp_mtch_all_imp)
sum(is.na(mahdataset))

mdvp_mtch1.2<-summary(mdvp_mtch1.1<-matchit(VCA ~
                                              elev_MEAN.2003_z + 
                                              ovslp_MEAN.2003_z + 
                                              dstrds00_MEAN.2003_z + 
                                              prmforlag_MEAN.2003_z + 
                                              AreaNonFor_3km_z + 
                                              AreaConc_z+
                                              pop.2003_pds_z+
                                              All_AreaForLoss_Ha_trunc_z,
                                            data=mahdataset, 
                                            method="nearest",
                                            distance="logit",
                                            discard="both",
                                            caliper=.25, 
                                            mahvars=c(#"elev_MEAN.2003_z",
                                              "ovslp_MEAN.2003_z",
                                              "dstrds00_MEAN.2003_z",
                                              #"prmforlag_MEAN.2003_z",
                                              "PAs_MEAN.2003_z",
                                              "AreaNonFor_3km_z",
                                              #"All_AreaForLoss_Ha_trunc_z",
                                              "pop.2003_pds_z"),
                                            ratio=3,
                                            replace=TRUE),
                      standardize=TRUE) 


#plot(mdvp_mtch1.2)
mdvp_mtch_mah2<-match.data(mdvp_mtch1.1)
mdvp_mtch1.2
#write.csv(mdvp_mtch_mah2, "C:/Users/JTErbaugh/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/Data/mdvp_mtch_mah2.csv")

### Full Matching ##############################################################################################
mdvp_mtch2.2<-summary(mdvp_mtch2.1<-matchit(VCA ~
                                              elev_MEAN.2003_z + 
                                              ovslp_MEAN.2003_z + 
                                              dstrds00_MEAN.2003_z + 
                                              prmforlag_MEAN.2003_z + 
                                              AreaNonFor_3km_z + 
                                              AreaConc_z+
                                              pop.2003_pds_z+
                                              All_AreaForLoss_Ha_trunc_z,,
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


#plot(mdvp_mtch2.2)
mdvp_mtch_full2<-match.data(mdvp_mtch2.1)
mdvp_mtch2.2
#write.csv(mdvp_mtch_full2, "C:/Users/JTErbaugh/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/Data/mdvp_mtch_full2.csv")

### Genetic Matching ###########################################################################################
mdvp_mtch3.2<-summary(mdvp_mtch3.1<-matchit(VCA ~
                                              elev_MEAN.2003_z + 
                                              ovslp_MEAN.2003_z + 
                                              dstrds00_MEAN.2003_z + 
                                              prmforlag_MEAN.2003_z + 
                                              AreaNonFor_3km_z + 
                                              AreaConc_z+
                                              pop.2003_pds_z+
                                              All_AreaForLoss_Ha_trunc_z,,
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
                                            replace=TRUE,
                                            pop.size=1000),
                      standardize=TRUE) 


#plot(mdvp_mtch3.2)
mdvp_mtch_gen2<-match.data(mdvp_mtch3.1)
mdvp_mtch3.2
#write.csv(mdvp_mtch_gen2, "C:/Users/JTErbaugh/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/Data/mdvp_mtch_gen2.csv")

###Adding weights to Analysis Dataset###

###Subsetting all weights from all matches###
mdvp_mtch_mah2_wts<-mdvp_mtch_mah2[,c("ID2003_d", "weights")]
#Renaming mahalanobis weights
mdvp_mtch_mah2_wts$weights_mah<-mdvp_mtch_mah2_wts$weights
mdvp_mtch_mah2_wts$weights<-NULL

mdvp_mtch_full2_wts<-mdvp_mtch_full2[,c("ID2003_d", "weights")]
#Renaming full weights
mdvp_mtch_full2_wts$weights_full<-mdvp_mtch_full2_wts$weights
mdvp_mtch_full2_wts$weights<-NULL

mdvp_mtch_gen2_wts<-mdvp_mtch_gen2[,c("ID2003_d", "weights")]
#Renaming genetic weights
mdvp_mtch_gen2_wts$weights_gen<-mdvp_mtch_gen2_wts$weights
mdvp_mtch_gen2_wts$weights<-NULL

###   Merging onto Analysis dataset (original and imputed)###
###   Original
mdvp_wts1<-merge(mdvd_analysis, mdvp_mtch_mah2_wts, By="ID2000_d", all.x=T)
mdvp_wts2<-merge(mdvp_wts1, mdvp_mtch_full2_wts, BY="ID2000_d", all.x=T)
mdvp_wts3<-merge(mdvp_wts2, mdvp_mtch_gen2_wts, BY="ID2000_d", all.x=T)

mdvp_wts<-mdvp_wts3
#write.csv(mdvp_wts, "C:/Users/JTErbaugh/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/Data/mdvp_wts.csv")

###   Imputed
mdvp_wts1_imp<-merge(mdvd_analysis_imp, mdvp_mtch_mah2_wts, By="ID2000_d", all.x=T)
mdvp_wts2_imp<-merge(mdvp_wts1_imp, mdvp_mtch_full2_wts, BY="ID2000_d", all.x=T)
mdvp_wts3_imp<-merge(mdvp_wts2_imp, mdvp_mtch_gen2_wts, BY="ID2000_d", all.x=T)

mdvp_wts_imp<-mdvp_wts3_imp
#write.csv(mdvp_wts_imp, "C:/Users/JTErbaugh/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/Data/mdvp_wts_imp.csv")

# rm(mahdataset, yrforloss_KSNP_wts, mdvp_mtch_mah2_wts, mdvp_mtch_mah2, 
#    mdvp_mtch_full2_wts, mdvp_mtch_full2, mdvp_mtch_gen2,
#    mdvp_mtch_gen2_wts, mdvp_wts1, mdvp_wts2, mdvp_wts3, mdvp_wts1_imp, mdvp_wts2_imp, mdvp_wts3_imp)

str(mdvp_wts)
str(mdvp_wts_imp)


###   Saving data to stata files   #####################################################################################################
###   Renaming variables to be saved as a Stata dta file (eliminating all non-letters, numbers, and underscores)

mdvp_wts_stata<-mdvp_wts %>% 
  rename(VCA_y = VCA.y,
         VCA_y_num = VCA.y_num,
         dstrds_y = dstrds.y)

mdvp_wts_imp_stata<-mdvp_wts_imp %>% 
  rename(VCA_y = VCA.y,
         dstrds_y = dstrds.y)

###   Writing dta files   
#write_dta(mdvp_wts_stata, "C:/Users/JTErbaugh/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/Data/mdvp_wts.dta")
#write_dta(mdvp_wts_imp_stata, "C:/Users/JTErbaugh/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/Data/mdvp_wts_imp.dta")

###   Analysis:   ######################################################################################################################
###   Running fixed-effects models for mdvpi   #########################################################################################

###   No Weights, Between Effects, Original dataset   ###
mdvpi_be_1<-plm(mdvp~VCA.y + nonKSNP_AreaForLoss_Ha_trunc + KSNP_AreaForLoss_Ha2_trunc+dstrds.y+hhpop+prmmrkt,
               model="between",
               effect="individual",
               index="ID2003_d",
              data=mdvp_wts)

summary(mdvpi_be_1) 

mdvpi_be_2<-plm(mdvp~HKD + nonKSNP_AreaForLoss_Ha_trunc + KSNP_AreaForLoss_Ha2_trunc+dstrds.y+hhpop+prmmrkt,
                model="between",
                effect="individual",
                index="ID2003_d",
                data=mdvp_wts)

summary(mdvpi_be_2) 

###   No Weights, Between Effects, Imputed Dataset   ###

mdvpi_be_3<-plm(mdvp~VCA.y + nonKSNP_AreaForLoss_Ha_trunc + KSNP_AreaForLoss_Ha2_trunc+dstrds.y+hhpop+prmmrkt,
                model="between",
                effect="individual",
                index="ID2003_d",
                data=mdvp_wts_imp)

summary(mdvpi_be_3) 

mdvpi_be_4<-plm(mdvp~HKD + nonKSNP_AreaForLoss_Ha_trunc + KSNP_AreaForLoss_Ha2_trunc+dstrds.y+hhpop+prmmrkt,
                model="between",
                effect="individual",
                index="ID2003_d",
                data=mdvp_wts_imp)

summary(mdvpi_be_4) 

###   No Weights, Two-Way Fixed Effects, Original dataset   ###
mdvpi_fe_1<-plm(mdvp~nonKSNP_AreaForLoss_Ha_trunc + KSNP_AreaForLoss_Ha2_trunc+dstrds.y+hhpop+prmmrkt,
                model="within",
                effect="twoway",
                index="ID2003_d",
                data=mdvp_wts)

summary(mdvpi_fe_1) 
coeftest(mdvpi_fe_1, vcovHC(mdvpi_fe_1, type = 'HC3', cluster = 'group'))


fixef_1<-as.data.frame(fixef(mdvpi_fe_1,effect="individual"))
fixef_1$ID2003_d<-as.numeric(rownames(fixef_1))
fixef_1_tbl<-as_tibble(fixef_1)
fixef_1_tbl<-fixef_1_tbl %>% rename(coeff = 1) %>% mutate(mtch = "None") %>% select(ID2003_d, coeff, mtch)
fixef_1_tbl

###   No Weights, Two-Way Fixed Effects, imputed dataset   ###
mdvpi_fe_2<-plm(mdvp~nonKSNP_AreaForLoss_Ha_trunc + KSNP_AreaForLoss_Ha2_trunc+dstrds.y+hhpop+prmmrkt,
                model="within",
                effect="twoway",
                index="ID2003_d",
                data=mdvp_wts_imp)
summary(mdvpi_fe_2)
coeftest(mdvpi_fe_2, vcovHC(mdvpi_fe_2, type = 'HC3', cluster = 'group'))


###   Mahalanobis weights, original dataset   ###
mdvpi_fe_3<-plm(mdvp~nonKSNP_AreaForLoss_Ha_trunc + KSNP_AreaForLoss_Ha2_trunc+dstrds.y+hhpop+prmmrkt,
                model="within",
                effect="twoway",
                index="ID2003_d",
                data=mdvp_wts,
                weights=weights_mah)

summary(mdvpi_fe_3)
coeftest(mdvpi_fe_3, vcov.=vcovHC(mdvpi_fe_3, type="HC1"))


fixef_3<-as.data.frame(fixef(mdvpi_fe_3,effect="individual"))
fixef_3$ID2003_d<-as.numeric(rownames(fixef_3))
fixef_3_tbl<-as_tibble(fixef_3)
fixef_3_tbl<-fixef_3_tbl %>% rename(coeff = 1) %>% mutate(mtch = "Mahal.") %>% select(ID2003_d, coeff, mtch)
fixef_3_tbl

###   Mahalanobis weights, imputed dataset   ###
mdvpi_fe_4<-plm(mdvp~nonKSNP_AreaForLoss_Ha_trunc + KSNP_AreaForLoss_Ha2_trunc+dstrds.y+hhpop+prmmrkt,
                model="within",
                effect="twoway",
                index="ID2003_d",
                data=mdvp_wts_imp,
                weights=weights_mah)

summary(mdvpi_fe_4) 


###   Genetic weights, original dataset   ###
mdvpi_fe_5<-plm(mdvp~nonKSNP_AreaForLoss_Ha_trunc + KSNP_AreaForLoss_Ha2_trunc+dstrds.y+hhpop+prmmrkt,
                model="within",
                effect="twoway",
                index="ID2003_d",
                data=mdvp_wts,
                weights=weights_gen)

summary(mdvpi_fe_5) 

fixef_5<-as.data.frame(fixef(mdvpi_fe_5,effect="individual"))
fixef_5$ID2003_d<-as.numeric(rownames(fixef_5))
fixef_5_tbl<-as_tibble(fixef_5)
fixef_5_tbl<-fixef_5_tbl %>% rename(coeff = 1) %>% mutate(mtch = "Genetic") %>% select(ID2003_d, coeff, mtch)
fixef_5_tbl

###   Genetic weights, imputed dataset   ###
mdvpi_fe_6<-plm(mdvp~nonKSNP_AreaForLoss_Ha_trunc + KSNP_AreaForLoss_Ha2_trunc+dstrds.y+hhpop+prmmrkt,
                model="within",
                effect="twoway",
                index="ID2003_d",
                data=mdvp_wts_imp,
                weights=weights_gen)

summary(mdvpi_fe_6) 

###   Full weights, original dataset   ###
mdvpi_fe_7<-plm(mdvp~nonKSNP_AreaForLoss_Ha_trunc + KSNP_AreaForLoss_Ha2_trunc+dstrds.y+hhpop+prmmrkt,
                model="within",
                effect="twoway",
                index="ID2003_d",
                data=mdvp_wts,
                weights=weights_full)

summary(mdvpi_fe_7) 

fixef_7<-as.data.frame(fixef(mdvpi_fe_7,effect="individual"))
fixef_7$ID2003_d<-as.numeric(rownames(fixef_7))
fixef_7_tbl<-as_tibble(fixef_7)
fixef_7_tbl<-fixef_7_tbl %>% rename(coeff = 1) %>% mutate(mtch = "Full") %>% select(ID2003_d, coeff, mtch)
fixef_7_tbl

###   Full weights, imputed dataset   ###
mdvpi_fe_8<-plm(mdvp~nonKSNP_AreaForLoss_Ha_trunc + KSNP_AreaForLoss_Ha2_trunc+dstrds.y+hhpop+prmmrkt,
                model="within",
                effect="twoway",
                index="ID2003_d",
                data=mdvp_wts_imp,
                weights=weights_full)

summary(mdvpi_fe_8)

###   NOTE: CHECK THE STANDARD ERRORS AGAINST STATA TO MAKE SURE THAT ROBUST SEs ARE NOT WILDLY DIFFERENT!!  ########################
###         Check the difference between robust and clustered-robust SEs in Stata   #################################################

###   Binding Fixed Effect Tables   ###

fixef_orig<-rbind(fixef_1_tbl, fixef_3_tbl, fixef_5_tbl, fixef_7_tbl)

###   Merging with VCA data and subsetting dataset   ###

fixef_orig_all<-merge(fixef_orig, ksnpvca, by="ID2003_d", all.x=T)

fixef_orig_plot<-fixef_orig_all %>% 
  select(ID2003_d, coeff, mtch, VCA, HKD, HKDII)

##############################################
###   Re-Labeling variables for plotting   ###
##############################################

fixef_orig_plot$HKDII<-fixef_orig_plot$VCA+fixef_orig_plot$HKDII

fixef_orig_plot$VCA[fixef_orig_plot$VCA==0]<-"Non-VCA"
fixef_orig_plot$VCA[fixef_orig_plot$VCA==1]<-"VCA"

fixef_orig_plot$HKDII[fixef_orig_plot$HKDII==0]<-"Non-VCA"
fixef_orig_plot$HKDII[fixef_orig_plot$HKDII==1]<-"VCA: Partial"
fixef_orig_plot$HKDII[fixef_orig_plot$HKDII==2]<-"VCA: Full"

fixef_orig_plot$HKD[fixef_orig_plot$HKD>250000000]<-fixef_orig_plot$HKD[fixef_orig_plot$HKD>250000000]/2

########################################
###   Plotting fixed effect values   ###
########################################

fixef_orig_plot1<- fixef_orig_plot %>% 
  group_by(mtch,VCA) %>%  
  summarize(coeff = mean(coeff, na.rm=T))

fe_1<-ggplot(fixef_orig_plot, 
             aes(x=factor(VCA), y=coeff)) + 
  geom_boxplot()+
  geom_hline(yintercept=.33,linetype="dashed")+
  facet_wrap(~mtch)+
  labs(x = "Village Type", y="Average Fixed Effect Coefficient (Individual)")+
    theme_bw()
fe_1

ggsave(file="C:/Users/JTErbaugh/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/FEs_VCA.png",
       plot=fe_1,
       dpi=600, width=6.5, height=4, unit="in")

fe_2<-ggplot(fixef_orig_plot, 
             aes(x=factor(HKDII), y=coeff)) + 
  geom_boxplot()+
  geom_hline(yintercept=.33, linetype="dashed")+
  facet_wrap(~mtch)+
  labs(x = "Village Type", y="Average Fixed Effect Coefficient (Individual)")+
  theme_bw()
fe_2

ggsave(file="C:/Users/JTErbaugh/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/FEs_HKD.png",
       plot=fe_2,
       dpi=600, width=6.5, height=4, unit="in")

fe_3<-ggplot(fixef_orig_plot, aes(x=HKD, y=coeff, color=mtch)) + 
  geom_smooth(method="loess", span=1.5) +
  geom_point(color="black") +
  geom_hline(yintercept=.33, linetype="dashed") +
  #facet_wrap(~mtch)+
  labs(x = "Village Type", y="Fixed Effect Coefficient (Individual)")+
  theme_bw()
fe_3

mdvp_wts_tmp<-mdvp_wts
mdvp_wts_tmp$HKD[mdvp_wts_tmp$HKD>250000000]<-mdvp_wts_tmp$HKD[mdvp_wts_tmp$HKD>250000000]/2
fe_4<-ggplot(mdvp_wts_tmp %>% group_by(ID2003_d) %>% mutate(mdvp=mean(mdvp, na.omit=T),
                                                        hkd=mean(HKD, na.omit=T)), aes(x=hkd, y=mdvp)) + 
  geom_smooth(method="loess", span=1) +
  geom_point(color="black") +
  geom_hline(yintercept=.33, linetype="dashed") +
  coord_cartesian(ylim=c(0,.6))+
  #facet_wrap(~mtch)+
  labs(x = "VCA Award Disbursed", y="Average MDVD Score (2003-2014)")+
  theme_bw()
fe_4
               
