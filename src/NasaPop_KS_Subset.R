### Subsetting xwalk_arcmap to create population surface   ###


xwalk_arcmap<-read.csv("C:/Users/JTErbaugh/Dropbox/Library/1.Data/Dissertation_Data/Spatial_Datatables/xwalk_arcmap.csv")
ksnpvils<-read.csv("E:/Indonesian_Spatial_Data/Sampling_Maps/KSNPVils03d.txt")
ksnpvca<-read.csv("C:/Users/JTErbaugh/Dropbox/#michigan phd/1.Dissertation/Ch5-Kerinci/KSNP-VilNames-ICDP_HKD.csv")


###  Merging these datasets   ###

ksnp_vca<-merge(ksnpvils, ksnpvca, by="ID2003_d", all.x=T)

xwalk_pop<-xwalk_arcmap %>%
  select(ID2000_D, ID2003_D, ID2006_D, ID2008_D, ID2011_D, ID.2014,
         pop.2000, pop.2003, pop.2006, pop.2008, pop.2011, pop.2014)

ksnp_pop<-merge(ksnp_vca, xwalk_pop, by.x="ID2003_d", by.y="ID2003_D", all.x=T)

write.csv(ksnp_pop, "C:/Users/JTErbaugh/Dropbox/Library/1.Data/Dissertation_Data/Spatial_Datatables/ksnp_pop.csv")

ksnp_pop03<-ksnp_pop %>%
  filter(!is.na(pop.2003), !is.na(ID2003_d))

write.csv(ksnp_pop03, "C:/Users/JTErbaugh/Dropbox/Library/1.Data/Dissertation_Data/Spatial_Datatables/ksnp_pop03.csv")
