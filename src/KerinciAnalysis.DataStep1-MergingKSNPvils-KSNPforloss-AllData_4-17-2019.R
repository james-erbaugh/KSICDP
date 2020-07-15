###Data Step 1 for KSNP Analysis###

install.packages("dplyr")
install.packages("plyr")
library(dplyr)
library(plyr)


###Spatial and PODES data for 2003 villages
#spatpod03<-read.csv("E:/Indonesian_Spatial_Data/VillageLandcoverTables/2003abc/2003dCSV/xwalk_wide_merged2003d_lags.csv")
spatpod03<-read.csv("C:/Users/JTErbaugh/Dropbox/Library/1.Data/Dissertation_Data/Spatial_Datatables/xwalk_wide_merged2003d_lags.csv")

###All Spatial and PODES data for villages (2000-2014)
xwalk<-read.csv("C:/Users/JTErbaugh/Dropbox/Library/1.Data/Dissertation_Data/Spatial_Datatables/xwalk_wide_mergedalld2_lags2.csv")

ksnpvils<-read.csv("E:/Indonesian_Spatial_Data/Sampling_Maps/KSNPVils03d.txt")
knspforloss<-read.csv("E:/Indonesian_Spatial_Data/Sampling_Maps/ksnplos0316.txt")
knspprmforloss<-read.csv("E:/Indonesian_Spatial_Data/Sampling_Maps/ksnpprmlos2.txt")
knspsecforloss<-read.csv("E:/Indonesian_Spatial_Data/Sampling_Maps/ksnpseclos2.txt")
ksnpvca<-read.csv("C:/Users/JTErbaugh/Dropbox/#michigan phd/1.Dissertation/Ch5-Kerinci/KSNP-VilNames-ICDP_HKD.csv")

##Merging KSNP Vils with Loss from inside NP##
ksnpvils_forloss1<-merge(ksnpvils,knspforloss, by.x="ID2003_d", by.y="ID2003_D", all.x=T)
str(ksnpvils_forloss1)
ksnpvils_forloss1$ksnp.loss0316.mean<-(ksnpvils_forloss1$COUNT*900)/ksnpvils_forloss1$Area2003
ksnpvils_forloss1$ksnp.loss0316.ha<-(ksnpvils_forloss1$COUNT*900)/10000
ksnpvils_forloss1$ksnp.loss0316.pxl<-ksnpvils_forloss1$COUNT

ksnpvils_forloss1$STD<-NULL
ksnpvils_forloss1$COUNT<-NULL
ksnpvils_forloss1$AREA<-NULL
ksnpvils_forloss1$MEAN<-NULL
ksnpvils_forloss1$ZONE_CODE<-NULL
ksnpvils_forloss1$Rowid_<-NULL

str(ksnpvils_forloss1)

##Merging KSNP Vils with NP Loss with Loss from all Primary Forests##

ksnpvils_forloss2<-merge(ksnpvils_forloss1,knspprmforloss, by.x="ID2003_d", by.y="ID2003_D", all.x=T)

ksnpvils_forloss2$ksnp.prmloss.mean<-(ksnpvils_forloss2$COUNT*900)/ksnpvils_forloss2$Area2003
ksnpvils_forloss2$ksnp.prmloss.ha<-(ksnpvils_forloss2$COUNT*900)/10000
ksnpvils_forloss2$ksnp.prmloss.pxl<-ksnpvils_forloss2$COUNT

ksnpvils_forloss2$STD<-NULL
ksnpvils_forloss2$COUNT<-NULL
ksnpvils_forloss2$AREA<-NULL
ksnpvils_forloss2$MEAN<-NULL
ksnpvils_forloss2$ZONE_CODE<-NULL
ksnpvils_forloss2$Rowid_<-NULL

str(ksnpvils_forloss2)

##Merging KSNP Vils with NP Loss and Loss from all Primary Forests with Loss from all Secondary Forests##

ksnpvils_forloss3<-merge(ksnpvils_forloss2,knspsecforloss, by.x="ID2003_d", by.y="ID2003_D", all.x=T)

ksnpvils_forloss3$ksnp.secloss.mean<-(ksnpvils_forloss3$COUNT*900)/ksnpvils_forloss3$Area2003
ksnpvils_forloss3$ksnp.secloss.ha<-(ksnpvils_forloss3$COUNT*900)/10000
ksnpvils_forloss3$ksnp.secloss.pxl<-ksnpvils_forloss3$COUNT

ksnpvils_forloss3$STD<-NULL
ksnpvils_forloss3$COUNT<-NULL
ksnpvils_forloss3$AREA<-NULL
ksnpvils_forloss3$MEAN<-NULL
ksnpvils_forloss3$ZONE_CODE<-NULL
ksnpvils_forloss3$Rowid_<-NULL

str(ksnpvils_forloss3)

##Adding overall forest loss (inside and outside KSNP)##

ksnpvils_forloss3$all.loss0316.mean<-ksnpvils_forloss3$ksnp.prmloss.mean+ksnpvils_forloss3$ksnp.secloss.mean
ksnpvils_forloss3$all.loss0316.ha<-ksnpvils_forloss3$ksnp.prmloss.ha+ksnpvils_forloss3$ksnp.secloss.ha
ksnpvils_forloss3$all.loss0316.pxl<-ksnpvils_forloss3$ksnp.prmloss.pxl+ksnpvils_forloss3$ksnp.secloss.pxl

##Checking numbers of total forest loss with KSNP forest loss to make sure the former is greater than the latter##
summary(ksnpvils_forloss3$all.loss0316.mean)
summary(ksnpvils_forloss3$ksnp.loss0316.mean)

summary(ksnpvils_forloss3$all.loss0316.ha)
summary(ksnpvils_forloss3$ksnp.prmloss.ha)
summary(ksnpvils_forloss3$ksnp.secloss.ha)
summary(ksnpvils_forloss3$ksnp.loss0316.ha)

##More secondary forest is being removed from KSNP than I initially realized. This generates a lower average
##total primary forest loss than KSNP forest loss (since KSNP forest loss also includes secondary forest).
##Regardless, the total forest loss is always greater than the KSNP forest loss, which is what I was checking for. 

###Merging KSNP vils and forest loss data with overall 2003 data###

ksnp_vils<-merge(ksnpvils_forloss3,spatpod03,by.x="ID2003_d", by.y="iid.2003", all.x=T)
str(ksnp_vils)

###Merging KSNP vils and forest loss data with xwalk###

ksnp_vils.xwalk<-merge(ksnpvils_forloss3,xwalk,by.x="ID2003_d", by.y="ID.2003", all.x=T)
str(ksnp_vils)
head(ksnp_vils.xwalk$NM1999)
ksnp_vils.xwalk.names<-ksnp_vils.xwalk[,c("NM.2000","NM.2003","NM.2006","NM.2008","NM.2011","NM2013_1.2014",
                                    "ID.2000","ID2003_D","ID.2006","ID.2008","ID.2011","ID.2014")]
#write.csv(ksnp_vils.xwalk.names,"C:/Users/JTErbaugh/Dropbox/#michigan phd/1.Dissertation/Ch5-Kerinci/KSNP-VilNames-ICDP2_4-17-2019.csv")

##Used the above tables to generate the 0,1 treatment variable for VCAs

vca<-read.csv("C:/Users/JTErbaugh/Dropbox/#michigan phd/1.Dissertation/Ch5-Kerinci/KSNP-VilNames-ICDP_HKD.csv")
ksnp_vils2<-merge(ksnp_vils,vca, by="ID2003_d", all.x=T)

write.csv(ksnp_vils2, "C:/Users/JTErbaugh/Dropbox/#michigan phd/1.Dissertation/Ch5-Kerinci/KSNP-Vils-2003.csv")
#This datatable will be used for the matching analysis of forest-cover loss between direct investment and non-direct
#investment treatment

###Summarizing relevant variables by 2003 data to get comparative estimates###
##Do this for MDVD, li, hi, ei, mean forest cover, mean ag plantation, mean dryland ag, mean mixed ag##

###############
###2003-2006###
###############                     

ksnpvilclst03061<-ddply(ksnp_vils.xwalk, .(ID2003_d), summarise,
                    for_MEAN.2006 = weighted.mean(for_MEAN.2006, W_AREA032.2006, na.action=na.pass, na.rm=T),
                    prmfor_MEAN.2006 = weighted.mean(prmfor_MEAN.2006, W_AREA032.2006, na.action=na.pass, na.rm=T),
                    secfor_MEAN.2006 = weighted.mean(secfor_MEAN.2006, W_AREA032.2006, na.action=na.pass, na.rm=T),
                    mngfor_MEAN.2006 = weighted.mean(mngfor_MEAN.2006, W_AREA032.2006, na.action=na.pass, na.rm=T),
                    agfor_MEAN.2006 = weighted.mean(agfor_MEAN.2006, W_AREA032.2006, na.action=na.pass, na.rm=T),
                    tmbfor_MEAN.2006=weighted.mean(tmbfor_MEAN.2006, W_AREA032.2006, na.action=na.pass, na.rm=T),
                    mixdag_MEAN.2006=weighted.mean(mixdag_MEAN.2006, W_AREA032.2006, na.action=na.pass, na.rm=T),
                    dryldag_MEAN.2006=weighted.mean(dryldag_MEAN.2006, W_AREA032.2006, na.action=na.pass, na.rm=T),
                    pdyld_MEAN.2006=weighted.mean(pdyld_MEAN.2006, W_AREA032.2006, na.action=na.pass, na.rm=T),
                    stlmnt_MEAN.2006=weighted.mean(stlmnt_MEAN.2006, W_AREA032.2006, na.action=na.pass, na.rm=T),
                    trnsld_MEAN.2006=weighted.mean(trnsld_MEAN.2006, W_AREA032.2006, na.action=na.pass, na.rm=T),
                    opnld_MEAN.2006=weighted.mean(opnld_MEAN.2006, W_AREA032.2006, na.action=na.pass, na.rm=T),
                    mngld_MEAN.2006=weighted.mean(mngld_MEAN.2006, W_AREA032.2006, na.action=na.pass, na.rm=T),
                    dstrds.2006=weighted.mean(dstrds.2006, W_AREA032.2006, na.action=na.pass, na.rm=T),
                    dststl_MEAN.2006=weighted.mean(dststl_MEAN.2006, W_AREA032.2006, na.action=na.pass, na.rm=T),
                    distnerkab.2006=weighted.mean(distnerkab.2006, W_AREA032.2006, na.action=na.pass, na.rm=T),
                    slp_MEAN.2006=weighted.mean(slp_MEAN.2006, W_AREA032.2006, na.action=na.pass, na.rm=T),
                    ovslp_MEAN.2006=weighted.mean(ovslp_MEAN.2006, W_AREA032.2006, na.action=na.pass, na.rm=T),
                    precip_MEAN.2006=weighted.mean(precip_MEAN.2006, W_AREA032.2006, na.action=na.pass, na.rm=T),
                    elev_MEAN.2006=weighted.mean(elev_MEAN.2006, W_AREA032.2006, na.action=na.pass, na.rm=T),
                    PAs_MEAN.2006=weighted.mean(PAs_MEAN.2006, W_AREA032.2006, na.action=na.pass, na.rm=T))

ksnpvilclst03062 <-ddply(ksnp_vils.xwalk, .(ID2003_d), summarise, 
                      freq.2006=length(ID2003_d), 
                      hhpop.2006=sum(hhpop.2006, na.rm=T),
                      vilcncl.2006=sum(vilcncl.2006, na.rm=T),
                      prmmrkt.2006=sum(prmmrkt.2006, na.rm=T),
                      hi.2006=weighted.mean(hi.2006, W_POP032.2006, na.action=na.pass, na.rm=T),
                      ei.2006=weighted.mean(ei.2006, W_POP032.2006, na.action=na.pass, na.rm=T),
                      li.2006=weighted.mean(li.2006, W_POP032.2006, na.action=na.pass, na.rm=T),
                      mdvp.2006=weighted.mean(mdvp.2006, W_POP032.2006, na.action=na.pass, na.rm=T))

head(ksnpvilclst03062)

ksnpvilclst0306<-merge(ksnpvilclst03061,ksnpvilclst03062, by="ID2003_d")

write.csv(ksnpvilclst0306,"C:/Users/JTErbaugh/Dropbox/#michigan phd/1.Dissertation/Ch5-Kerinci/ksnpvilclst0306_4-17-2019.csv")

###############
###2003-2008###
###############                     

ksnpvilclst03081<-ddply(ksnp_vils.xwalk, .(ID2003_d), summarise,
                        for_MEAN.2008 = weighted.mean(for_MEAN.2008, W_AREA032.2008, na.action=na.pass, na.rm=T),
                        prmfor_MEAN.2008 = weighted.mean(prmfor_MEAN.2008, W_AREA032.2008, na.action=na.pass, na.rm=T),
                        secfor_MEAN.2008 = weighted.mean(secfor_MEAN.2008, W_AREA032.2008, na.action=na.pass, na.rm=T),
                        mngfor_MEAN.2008 = weighted.mean(mngfor_MEAN.2008, W_AREA032.2008, na.action=na.pass, na.rm=T),
                        agfor_MEAN.2008 = weighted.mean(agfor_MEAN.2008, W_AREA032.2008, na.action=na.pass, na.rm=T),
                        tmbfor_MEAN.2008=weighted.mean(tmbfor_MEAN.2008, W_AREA032.2008, na.action=na.pass, na.rm=T),
                        mixdag_MEAN.2008=weighted.mean(mixdag_MEAN.2008, W_AREA032.2008, na.action=na.pass, na.rm=T),
                        dryldag_MEAN.2008=weighted.mean(dryldag_MEAN.2008, W_AREA032.2008, na.action=na.pass, na.rm=T),
                        pdyld_MEAN.2008=weighted.mean(pdyld_MEAN.2008, W_AREA032.2008, na.action=na.pass, na.rm=T),
                        stlmnt_MEAN.2008=weighted.mean(stlmnt_MEAN.2008, W_AREA032.2008, na.action=na.pass, na.rm=T),
                        trnsld_MEAN.2008=weighted.mean(trnsld_MEAN.2008, W_AREA032.2008, na.action=na.pass, na.rm=T),
                        opnld_MEAN.2008=weighted.mean(opnld_MEAN.2008, W_AREA032.2008, na.action=na.pass, na.rm=T),
                        mngld_MEAN.2008=weighted.mean(mngld_MEAN.2008, W_AREA032.2008, na.action=na.pass, na.rm=T),
                        dstrds.2008=weighted.mean(dstrds.2008, W_AREA032.2008, na.action=na.pass, na.rm=T),
                        dststl_MEAN.2008=weighted.mean(dststl_MEAN.2008, W_AREA032.2008, na.action=na.pass, na.rm=T),
                        distnerkab.2008=weighted.mean(distnerkab.2008, W_AREA032.2008, na.action=na.pass, na.rm=T),
                        slp_MEAN.2008=weighted.mean(slp_MEAN.2008, W_AREA032.2008, na.action=na.pass, na.rm=T),
                        ovslp_MEAN.2008=weighted.mean(ovslp_MEAN.2008, W_AREA032.2008, na.action=na.pass, na.rm=T),
                        precip_MEAN.2008=weighted.mean(precip_MEAN.2008, W_AREA032.2008, na.action=na.pass, na.rm=T),
                        elev_MEAN.2008=weighted.mean(elev_MEAN.2008, W_AREA032.2008, na.action=na.pass, na.rm=T),
                        PAs_MEAN.2008=weighted.mean(PAs_MEAN.2008, W_AREA032.2008, na.action=na.pass, na.rm=T))

ksnpvilclst03082 <-ddply(ksnp_vils.xwalk, .(ID2003_d), summarise, 
                         freq.2008=length(ID2003_d), 
                         hhpop.2008=sum(hhpop.2008, na.rm=T),
                         vilcncl.2008=sum(vilcncl.2008, na.rm=T),
                         prmmrkt.2008=sum(prmmrkt.2008, na.rm=T),
                         hi.2008=weighted.mean(hi.2008, W_POP032.2008, na.action=na.pass, na.rm=T),
                         ei.2008=weighted.mean(ei.2008, W_POP032.2008, na.action=na.pass, na.rm=T),
                         li.2008=weighted.mean(li.2008, W_POP032.2008, na.action=na.pass, na.rm=T),
                         mdvp.2008=weighted.mean(mdvp.2008, W_POP032.2008, na.action=na.pass, na.rm=T))

head(ksnpvilclst03082)

ksnpvilclst0308<-merge(ksnpvilclst03081,ksnpvilclst03082, by="ID2003_d")

write.csv(ksnpvilclst0308,"C:/Users/JTErbaugh/Dropbox/#michigan phd/1.Dissertation/Ch5-Kerinci/ksnpvilclst0308_4-17-2019.csv")

###############
###2003-2011###
###############                     

ksnpvilclst03111<-ddply(ksnp_vils.xwalk, .(ID2003_d), summarise,
                        for_MEAN.2011 = weighted.mean(for_MEAN.2011, W_AREA032.2011, na.action=na.pass, na.rm=T),
                        prmfor_MEAN.2011 = weighted.mean(prmfor_MEAN.2011, W_AREA032.2011, na.action=na.pass, na.rm=T),
                        secfor_MEAN.2011 = weighted.mean(secfor_MEAN.2011, W_AREA032.2011, na.action=na.pass, na.rm=T),
                        mngfor_MEAN.2011 = weighted.mean(mngfor_MEAN.2011, W_AREA032.2011, na.action=na.pass, na.rm=T),
                        agfor_MEAN.2011 = weighted.mean(agfor_MEAN.2011, W_AREA032.2011, na.action=na.pass, na.rm=T),
                        tmbfor_MEAN.2011=weighted.mean(tmbfor_MEAN.2011, W_AREA032.2011, na.action=na.pass, na.rm=T),
                        mixdag_MEAN.2011=weighted.mean(mixdag_MEAN.2011, W_AREA032.2011, na.action=na.pass, na.rm=T),
                        dryldag_MEAN.2011=weighted.mean(dryldag_MEAN.2011, W_AREA032.2011, na.action=na.pass, na.rm=T),
                        pdyld_MEAN.2011=weighted.mean(pdyld_MEAN.2011, W_AREA032.2011, na.action=na.pass, na.rm=T),
                        stlmnt_MEAN.2011=weighted.mean(stlmnt_MEAN.2011, W_AREA032.2011, na.action=na.pass, na.rm=T),
                        trnsld_MEAN.2011=weighted.mean(trnsld_MEAN.2011, W_AREA032.2011, na.action=na.pass, na.rm=T),
                        opnld_MEAN.2011=weighted.mean(opnld_MEAN.2011, W_AREA032.2011, na.action=na.pass, na.rm=T),
                        mngld_MEAN.2011=weighted.mean(mngld_MEAN.2011, W_AREA032.2011, na.action=na.pass, na.rm=T),
                        dstrds.2011=weighted.mean(dstrds.2011, W_AREA032.2011, na.action=na.pass, na.rm=T),
                        dststl_MEAN.2011=weighted.mean(dststl_MEAN.2011, W_AREA032.2011, na.action=na.pass, na.rm=T),
                        distnerkab.2011=weighted.mean(distnerkab.2011, W_AREA032.2011, na.action=na.pass, na.rm=T),
                        slp_MEAN.2011=weighted.mean(slp_MEAN.2011, W_AREA032.2011, na.action=na.pass, na.rm=T),
                        ovslp_MEAN.2011=weighted.mean(ovslp_MEAN.2011, W_AREA032.2011, na.action=na.pass, na.rm=T),
                        precip_MEAN.2011=weighted.mean(precip_MEAN.2011, W_AREA032.2011, na.action=na.pass, na.rm=T),
                        elev_MEAN.2011=weighted.mean(elev_MEAN.2011, W_AREA032.2011, na.action=na.pass, na.rm=T),
                        PAs_MEAN.2011=weighted.mean(PAs_MEAN.2011, W_AREA032.2011, na.action=na.pass, na.rm=T))

ksnpvilclst03112 <-ddply(ksnp_vils.xwalk, .(ID2003_d), summarise, 
                         freq.2011=length(ID2003_d), 
                         hhpop.2011=sum(hhpop.2011, na.rm=T),
                         vilcncl.2011=sum(vilcncl.2011, na.rm=T),
                         prmmrkt.2011=sum(prmmrkt.2011, na.rm=T),
                         hi.2011=weighted.mean(hi.2011, W_POP032.2011, na.action=na.pass, na.rm=T),
                         ei.2011=weighted.mean(ei.2011, W_POP032.2011, na.action=na.pass, na.rm=T),
                         li.2011=weighted.mean(li.2011, W_POP032.2011, na.action=na.pass, na.rm=T),
                         mdvp.2011=weighted.mean(mdvp.2011, W_POP032.2011, na.action=na.pass, na.rm=T))

head(ksnpvilclst03112)

ksnpvilclst0311<-merge(ksnpvilclst03111,ksnpvilclst03112, by="ID2003_d")

write.csv(ksnpvilclst0311,"C:/Users/JTErbaugh/Dropbox/#michigan phd/1.Dissertation/Ch5-Kerinci/ksnpvilclst0311_4-17-2019.csv")

###############
###2003-2014###
############### 

ksnpvilclst03141<-ddply(ksnp_vils.xwalk, .(ID2003_d), summarise,
                        for_MEAN.2014 = weighted.mean(for_MEAN.2014, W_AREA032.2014, na.action=na.pass, na.rm=T),
                        prmfor_MEAN.2014 = weighted.mean(prmfor_MEAN.2014, W_AREA032.2014, na.action=na.pass, na.rm=T),
                        secfor_MEAN.2014 = weighted.mean(secfor_MEAN.2014, W_AREA032.2014, na.action=na.pass, na.rm=T),
                        mngfor_MEAN.2014 = weighted.mean(mngfor_MEAN.2014, W_AREA032.2014, na.action=na.pass, na.rm=T),
                        agfor_MEAN.2014 = weighted.mean(agfor_MEAN.2014, W_AREA032.2014, na.action=na.pass, na.rm=T),
                        tmbfor_MEAN.2014=weighted.mean(tmbfor_MEAN.2014, W_AREA032.2014, na.action=na.pass, na.rm=T),
                        mixdag_MEAN.2014=weighted.mean(mixdag_MEAN.2014, W_AREA032.2014, na.action=na.pass, na.rm=T),
                        dryldag_MEAN.2014=weighted.mean(dryldag_MEAN.2014, W_AREA032.2014, na.action=na.pass, na.rm=T),
                        pdyld_MEAN.2014=weighted.mean(pdyld_MEAN.2014, W_AREA032.2014, na.action=na.pass, na.rm=T),
                        stlmnt_MEAN.2014=weighted.mean(stlmnt_MEAN.2014, W_AREA032.2014, na.action=na.pass, na.rm=T),
                        trnsld_MEAN.2014=weighted.mean(trnsld_MEAN.2014, W_AREA032.2014, na.action=na.pass, na.rm=T),
                        opnld_MEAN.2014=weighted.mean(opnld_MEAN.2014, W_AREA032.2014, na.action=na.pass, na.rm=T),
                        mngld_MEAN.2014=weighted.mean(mngld_MEAN.2014, W_AREA032.2014, na.action=na.pass, na.rm=T),
                        dstrds.2014=weighted.mean(dstrds.2014, W_AREA032.2014, na.action=na.pass, na.rm=T),
                        dststl_MEAN.2014=weighted.mean(dststl_MEAN.2014, W_AREA032.2014, na.action=na.pass, na.rm=T),
                        distnerkab.2014=weighted.mean(distnerkab.2014, W_AREA032.2014, na.action=na.pass, na.rm=T),
                        slp_MEAN.2014=weighted.mean(slp_MEAN.2014, W_AREA032.2014, na.action=na.pass, na.rm=T),
                        ovslp_MEAN.2014=weighted.mean(ovslp_MEAN.2014, W_AREA032.2014, na.action=na.pass, na.rm=T),
                        precip_MEAN.2014=weighted.mean(precip_MEAN.2014, W_AREA032.2014, na.action=na.pass, na.rm=T),
                        elev_MEAN.2014=weighted.mean(elev_MEAN.2014, W_AREA032.2014, na.action=na.pass, na.rm=T),
                        PAs_MEAN.2014=weighted.mean(PAs_MEAN.2014, W_AREA032.2014, na.action=na.pass, na.rm=T))

ksnpvilclst03142 <-ddply(ksnp_vils.xwalk, .(ID2003_d), summarise, 
                         freq.2014=length(ID2003_d), 
                         hhpop.2014=sum(hhpop.2014, na.rm=T),
                         vilcncl.2014=sum(vilcncl.2014, na.rm=T),
                         prmmrkt.2014=sum(prmmrkt.2014, na.rm=T),
                         hi.2014=weighted.mean(hi.2014, W_POP032.2014, na.action=na.pass, na.rm=T),
                         ei.2014=weighted.mean(ei.2014, W_POP032.2014, na.action=na.pass, na.rm=T),
                         li.2014=weighted.mean(li.2014, W_POP032.2014, na.action=na.pass, na.rm=T),
                         mdvp.2014=weighted.mean(mdvp.2014, W_POP032.2014, na.action=na.pass, na.rm=T))

head(ksnpvilclst03142)

ksnpvilclst0314<-merge(ksnpvilclst03141,ksnpvilclst03142, by="ID2003_d")

write.csv(ksnpvilclst0314,"C:/Users/JTErbaugh/Dropbox/#michigan phd/1.Dissertation/Ch5-Kerinci/ksnpvilclst0314_4-17-2019.csv")

###Subset 2003 values###
ksnp2003_base_dups<-subset(ksnp_vils.xwalk, select=c("ID2003_d","Area2003","X2003",               
                                                "Y2003","ksnp.loss0316.mean","ksnp.loss0316.ha","ksnp.loss0316.pxl",   
                                                "ksnp.prmloss.mean","ksnp.prmloss.ha","ksnp.prmloss.pxl","ksnp.secloss.mean", 
                                                "ksnp.secloss.ha","ksnp.secloss.pxl","all.loss0316.mean","all.loss0316.ha"  ,  
                                                "all.loss0316.pxl","ev_frac1", "ev_frac2",
                                                "provfract1.2003", "provfract2.2003",
                                                "provfract1.2006", "provfract2.2006",
                                                "provfract1.2008", "provfract2.2008",
                                                "provfract1.2011", "provfract2.2011",
                                                "provfract1.2014", "provfract2.2014",
                                                "kabfract1.2003", "kabfract2.2003", 
                                                "kabfract1.2006", "kabfract2.2006", 
                                                "kabfract1.2008", "kabfract2.2008", 
                                                "kabfract1.2011", "kabfract2.2011", 
                                                "kabfract1.2014", "kabfract2.2014",
                                                "kecfract1.2003", "kecfract2.2003",
                                                "kecfract1.2006", "kecfract2.2006",
                                                "kecfract1.2008", "kecfract2.2008",
                                                "kecfract1.2011", "kecfract2.2011",
                                                "kecfract1.2014", "kecfract2.2014",
                                                "vilfract1.2003", "vilfract2.2003",
                                                "vilfract1.2006", "vilfract2.2006",
                                                "vilfract1.2008", "vilfract2.2008",
                                                "vilfract1.2011", "vilfract2.2011",
                                                "vilfract1.2014", "vilfract2.2014",
                                                "hasfract1.2003", "hasfract2.2003", 
                                                "hasfract1.2006", "hasfract2.2006", 
                                                "hasfract1.2008", "hasfract2.2008", 
                                                "hasfract1.2011", "hasfract2.2011", 
                                                "hasfract1.2014", "hasfract2.2014", 
                                                "fract1.2003", "fract2.2003",
                                                "fract1.2006", "fract2.2006",
                                                "fract1.2008", "fract2.2008",
                                                "fract1.2011", "fract2.2011",
                                                "fract1.2014", "fract2.2014",
                                                "precip_AREA.2003",
                                                "vilcncl.2003",
                                                "prov.2003", 
                                                "linkkab.2003",
                                                "linkkec.2003", 
                                                "for_MEAN.2003",
                                                "forlag_MEAN.2003",
                                                "prmfor_MEAN.2003",
                                                "prmforlag_MEAN.2003",
                                                "secfor_MEAN.2003",
                                                "secforlag_MEAN.2003",
                                                "agfor_MEAN.2003",
                                                "agforlag_MEAN.2003",
                                                "tmbfor_MEAN.2003",
                                                "tmbforlag_MEAN.2003",
                                                "mngfor_MEAN.2003",
                                                "mngforlag_MEAN.2003",
                                                "mixdag_MEAN.2003", 
                                                "dryldag_MEAN.2003",
                                                "pdyld_MEAN.2003",
                                                "stlmnt_MEAN.2003", 
                                                "trnsld_MEAN.2003", 
                                                "opnld_MEAN.2003", 
                                                "mngld_MEAN.2003", 
                                                "dstrds.2003", 
                                                "dststl_MEAN.2003", 
                                                "distnerkab.2003",
                                                "PAs_MEAN.2003",
                                                "slp_MEAN.2003",
                                                "ovslp_MEAN.2003", 
                                                "elev_MEAN.2003", 
                                                "precip_MEAN.2003", 
                                                "hhpop.2003", 
                                                "prmmrkt.2003",
                                                "hi.2003",
                                                "ei.2003",
                                                "li.2003",
                                                "mdvp.2003"))
ksnp2003_base<-ksnp2003_base_dups[!duplicated(ksnp2003_base_dups$ID2003_d),]

ksnp0306<-merge(ksnpvilclst0306, ksnp2003_base, by=c("ID2003_d"))
ksnp0308<-merge(ksnpvilclst0308, ksnp0306, by=c("ID2003_d"))
ksnp0311<-merge(ksnpvilclst0311, ksnp0308, by=c("ID2003_d"))
ksnp0314<-merge(ksnpvilclst0314, ksnp0311, by=c("ID2003_d"))

###Merging ksnp0314 with ksncpvca to get treatment variable###

ksnpvca$X<-NULL
ksnpvca$NM.2003<-NULL

ksnp0314<-merge(ksnp0314, ksnpvca, by="ID2003_d", all.x=T)



###Here everything is in wide format. Now I need to reorder the variables and then tidy for long DT###

ksnp2003_ordered<-subset(ksnp0314, select=c("ID2003_d","Area2003","X2003",
                                            "forlag_MEAN.2003","VCA",               
                                                     "Y2003","ksnp.loss0316.mean","ksnp.loss0316.ha","ksnp.loss0316.pxl",   
                                                     "ksnp.prmloss.mean","ksnp.prmloss.ha","ksnp.prmloss.pxl","ksnp.secloss.mean", 
                                                     "ksnp.secloss.ha","ksnp.secloss.pxl","all.loss0316.mean","all.loss0316.ha"  , 
                                                     "all.loss0316.pxl","ev_frac1", "ev_frac2",
                                                     "ovslp_MEAN.2003", "elev_MEAN.2003", "precip_MEAN.2003", 
                                              "precip_AREA.2003",
                                              "prov.2003",
                                              "linkkab.2003", 
                                              "linkkec.2003",
                                            "provfract1.2003", "provfract1.2006", "provfract1.2008", "provfract1.2011", "provfract1.2014", 
                                            "provfract2.2003", "provfract2.2006", "provfract2.2008", "provfract2.2011", "provfract2.2014",
                                            "kabfract1.2003", "kabfract1.2006", "kabfract1.2008",  "kabfract1.2011", "kabfract1.2014", 
                                            "kabfract2.2003", "kabfract2.2006","kabfract2.2008", "kabfract2.2011", "kabfract2.2014", 
                                            "kecfract1.2003","kecfract1.2006", "kecfract1.2008", "kecfract1.2011", "kecfract1.2014", 
                                            "kecfract2.2003","kecfract2.2006","kecfract2.2008","kecfract2.2011","kecfract2.2014",
                                         "vilfract1.2003", "vilfract1.2006", "vilfract1.2008", "vilfract1.2011", "vilfract1.2014", 
                                         "vilfract2.2003", "vilfract2.2006", "vilfract2.2008", "vilfract2.2011", "vilfract2.2014",
                                         "hasfract1.2003", "hasfract1.2006", "hasfract1.2008", "hasfract1.2011",  "hasfract1.2014", 
                                         "hasfract2.2003","hasfract2.2006",  "hasfract2.2008", "hasfract2.2011", "hasfract2.2014",
                                         "fract1.2003", "fract1.2006",  "fract1.2008", "fract1.2011", "fract1.2014", 
                                         "fract2.2003", "fract2.2006", "fract2.2008", "fract2.2011", "fract2.2014",
                                                  "for_MEAN.2003","for_MEAN.2006", "for_MEAN.2008", "for_MEAN.2011", "for_MEAN.2014",
                                                     "prmfor_MEAN.2003", "prmfor_MEAN.2006", "prmfor_MEAN.2008", "prmfor_MEAN.2011", "prmfor_MEAN.2014",
                                                     "secfor_MEAN.2003", "secfor_MEAN.2006", "secfor_MEAN.2008", "secfor_MEAN.2011", "secfor_MEAN.2014",
                                                     "agfor_MEAN.2003", "agfor_MEAN.2006", "agfor_MEAN.2008", "agfor_MEAN.2011", "agfor_MEAN.2014",
                                                     "tmbfor_MEAN.2003", "tmbfor_MEAN.2006", "tmbfor_MEAN.2008", "tmbfor_MEAN.2011", "tmbfor_MEAN.2014",
                                                     "mngfor_MEAN.2003", "mngfor_MEAN.2006", "mngfor_MEAN.2008", "mngfor_MEAN.2011", "mngfor_MEAN.2014",
                                                     "mixdag_MEAN.2003",  "mixdag_MEAN.2006", "mixdag_MEAN.2008",  "mixdag_MEAN.2011",  "mixdag_MEAN.2014", 
                                                     "dryldag_MEAN.2003", "dryldag_MEAN.2006", "dryldag_MEAN.2008", "dryldag_MEAN.2011", "dryldag_MEAN.2014",
                                                     "pdyld_MEAN.2003",  "pdyld_MEAN.2006", "pdyld_MEAN.2008", "pdyld_MEAN.2011", "pdyld_MEAN.2014",
                                                     "stlmnt_MEAN.2003", "stlmnt_MEAN.2006", "stlmnt_MEAN.2008", "stlmnt_MEAN.2011", "stlmnt_MEAN.2014", 
                                                     "trnsld_MEAN.2003", "trnsld_MEAN.2006", "trnsld_MEAN.2008", "trnsld_MEAN.2011", "trnsld_MEAN.2014", 
                                                     "opnld_MEAN.2003", "opnld_MEAN.2006", "opnld_MEAN.2008", "opnld_MEAN.2011", "opnld_MEAN.2014", 
                                                     "mngld_MEAN.2003", "mngld_MEAN.2006", "mngld_MEAN.2008", "mngld_MEAN.2011", "mngld_MEAN.2014", 
                                                     "dstrds.2003", "dstrds.2006", "dstrds.2008", "dstrds.2011", "dstrds.2014", 
                                                     "dststl_MEAN.2003", "dststl_MEAN.2006", "dststl_MEAN.2008", "dststl_MEAN.2011", "dststl_MEAN.2014", 
                                                     "distnerkab.2003", "distnerkab.2006", "distnerkab.2008", "distnerkab.2011", "distnerkab.2014", 
                                                     "PAs_MEAN.2003","PAs_MEAN.2006", "PAs_MEAN.2008", "PAs_MEAN.2011", "PAs_MEAN.2014", 
                                                     "hhpop.2003", "hhpop.2006", "hhpop.2008", "hhpop.2011", "hhpop.2014", 
                                                     "prmmrkt.2003","prmmrkt.2006","prmmrkt.2008","prmmrkt.2011","prmmrkt.2014",
                                                     "hi.2003", "hi.2006", "hi.2008", "hi.2011", "hi.2014",
                                                     "ei.2003", "ei.2006", "ei.2008", "ei.2011", "ei.2014",
                                                     "li.2003", "li.2006", "li.2008", "li.2011", "li.2014",
                                                     "mdvp.2003", "mdvp.2006", "mdvp.2008", "mdvp.2011", "mdvp.2014"))

ksnp0314_tidy<-reshape(ksnp2003_ordered, direction="long", 
                             idvar='ID2000_d',
                             varying=c(28:202),
                             sep='.',
                             timevar="year",
                             times=c('2003','2006','2008','2011','2014'))

write.csv(ksnp2003_ordered, "C:/Users/JTErbaugh/Dropbox/#michigan phd/1.Dissertation/Ch5-Kerinci/ksnp0314_wide.csv")

write.csv(ksnp0314_tidy, "C:/Users/JTErbaugh/Dropbox/#michigan phd/1.Dissertation/Ch5-Kerinci/ksnp0314_long.csv")