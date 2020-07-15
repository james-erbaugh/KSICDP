###   Visualizing Model Results   ###

library(tidyverse)
library(ggpubr)

mods<-read.csv("C:/Users/f003r0x/Dropbox/_DarmouthPostDoc/Publications/2.In_Review/KerinciConsLegacy/Data/AllFEModelResults.csv")

###   Transforming log estimates

mods$Est_trans<-(exp(mods$Estimate)-1)*100
mods$LL_trans<-(exp(mods$UL)-1)*100
mods$UL_trans<-(exp(mods$LL)-1)*100

##All Coefficients for model 3, dichotomous treatment variable, log(ha)
ksnp_did_plots1<-mods %>%
  filter(model==2 & did_type=="Dich." &depvar2=="Log(Ha)") %>% mutate(term=fct_rev(term))%>%
  ggplot(aes(x=fct_rev(depvar), 
             y=Estimate, 
             ymax=UL, 
             ymin=LL,
             fill=fct_rev(mtch),
             color=fct_rev(mtch)))+
  geom_pointrange(aes(ymin=LL, ymax=UL, group=(mtch), color=fct_rev(mtch)),
                  position=position_dodge(width = .5)) + 
  #scale_alpha_continuous(range = c(.3, 1), guide='none')+
  scale_y_continuous(limits=c(-0.85,0.85))+
  geom_hline(yintercept=0, color="black") + 
  theme(text = element_text(size=10)) +
  labs(fill = "Matching", color = "Matching", y="Additional forest-cover change in VCA villages (%)", x=NULL) +
  #xlab(NULL) +
  theme_bw() +
  coord_flip()+
  facet_wrap(term~.)
ksnp_did_plots1

##All Coefficients for model 3, continuous treatment variable, log(ha)

ksnp_did_plots2<-mods %>%
  filter(model==2 & did_type=="Cont." &depvar2=="Log(Ha)") %>% mutate(term=fct_rev(term))%>%
  ggplot(aes(x=fct_rev(depvar), 
             y=Estimate*10, 
             ymax=UL*10, 
             ymin=LL*10,
             fill=fct_rev(mtch),
             color=fct_rev(mtch)))+
  geom_pointrange(aes(ymin=(LL*10), ymax=(UL*10), group=(mtch), color=fct_rev(mtch)),
                  position=position_dodge(width = .5)) + 
  #scale_alpha_continuous(range = c(.3, 1), guide='none')+
  scale_y_continuous(limits=c(-0.85,0.85))+
  geom_hline(yintercept=0, color="black") + 
  theme(text = element_text(size=10)) +
  labs(fill = "Matching", color = "Matching", y="Additional forest-cover loss per $10,000 (%)", x=NULL) +
  theme_bw() +
  coord_flip()+
  facet_grid(~term)
ksnp_did_plots2


##Combining the two plots into one##
ksnp_did_both<-ggarrange(ksnp_did_plots1, ksnp_did_plots2, nrow=2, ncol=1, common.legend = TRUE, labels="AUTO")
ksnp_did_both<-annotate_figure(ksnp_did_both,left=text_grob("Forest-Cover Type", rot=90))
ksnp_did_both

ggsave(file="C:/Users/f003r0x/Dropbox/_DarmouthPostDoc/Publications/2.In_Review/KerinciConsLegacy/ksnp_bothplots_log_mod2.png",
       plot=ksnp_did_both,
       dpi=600, width=6.5, height=4, unit="in")


##All Coefficients for model 3, dichotomous treatment variable, log(ha) transformed to %

###   NOTE: Only visualized "long-term" models

ksnp_did_plots1<-mods %>%
  filter(model==2 & did_type=="Dich." & depvar2=="Log(Ha)" & term=="Long-Term") %>% mutate(term=fct_rev(term))%>%
  ggplot(aes(x=fct_rev(depvar), 
             y=Est_trans, 
             ymax=UL_trans, 
             ymin=LL_trans,
             fill=fct_rev(mtch),
             color=fct_rev(mtch)))+
  geom_pointrange(aes(ymin=LL_trans, ymax=UL_trans, group=(mtch), color=fct_rev(mtch)),
                  position=position_dodge(width = .5)) + 
  #scale_alpha_continuous(range = c(.3, 1), guide='none')+
  scale_y_continuous(limits=c(-75,125))+
  geom_hline(yintercept=0, color="black") + 
  theme(text = element_text(size=10)) +
  labs(fill = "Matching", color = "Matching", y="Additional forest-cover change in VCA villages (%)", x=NULL) +
  #xlab(NULL) +
  theme_bw() +
  coord_flip()
 # facet_wrap(term~.)
ksnp_did_plots1

##All Coefficients for model 3, continuous treatment variable, log(ha)

ksnp_did_plots2<-mods %>%
  filter(model==2 & did_type=="Cont." &depvar2=="Log(Ha)" & term=="Long-Term") %>% mutate(term=fct_rev(term))%>%
  ggplot(aes(x=fct_rev(depvar), 
             y=Est_trans*10, 
             ymax=UL_trans*10, 
             ymin=LL_trans*10,
             fill=fct_rev(mtch),
             color=fct_rev(mtch)))+
  geom_pointrange(aes(ymin=(LL_trans*10), ymax=(UL_trans*10), group=(mtch), color=fct_rev(mtch)),
                  position=position_dodge(width = .5)) + 
  #scale_alpha_continuous(range = c(.3, 1), guide='none')+
  scale_y_continuous(limits=c(-75,125))+
  geom_hline(yintercept=0, color="black") + 
  theme(text = element_text(size=10)) +
  labs(fill = "Matching", color = "Matching", y="Additional forest-cover loss per $10,000 (%)", x=NULL) +
  theme_bw() +
  coord_flip()
  #facet_grid(~term)
ksnp_did_plots2


##Combining the two plots into one##
ksnp_did_longterm_trans<-ggarrange(ksnp_did_plots1, ksnp_did_plots2, nrow=2, ncol=1, common.legend = TRUE, labels="AUTO")
#ksnp_did_both_trans<-annotate_figure(ksnp_did_both_trans,left=text_grob("Forest-Cover Type", rot=90))
ksnp_did_longterm_trans

ggsave(file="C:/Users/f003r0x/Dropbox/_DarmouthPostDoc/Publications/2.In_Review/KerinciConsLegacy/ksnp_longterm_log_mod2_trans.png",
       plot=ksnp_did_longterm_trans,
       dpi=600, width=6.5, height=4, unit="in")

##All Coefficients for model 3, dichotomous treatment variable, ha
ksnp_did_plots3<-mods %>%
  filter(model==3 & did_type=="Dich." &depvar2=="Ha") %>% mutate(term=fct_rev(term))%>%
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
  labs(fill = "Matching", color = "Matching", y="Average additional forest-cover loss in VCA villages (Ha)", x=NULL) +
  #xlab(NULL) +
  theme_bw() +
  coord_flip()+
  facet_wrap(term~.)
ksnp_did_plots3

##All Coefficients for model 3, continuous treatment variable, log(ha)

ksnp_did_plots4<-mods %>%
  filter(model==3 & did_type=="Cont." &depvar2=="Ha") %>% mutate(term=fct_rev(term))%>%
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
  labs(fill = "Matching", color = "Matching", y="Average additional forest-cover loss per $1,000 (Ha)", x=NULL) +
  theme_bw() +
  coord_flip()+
  facet_grid(.~term)
ksnp_did_plots4