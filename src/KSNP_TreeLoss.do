import delimited 
use "C:/Users/f003r0x/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/Data/yrforloss_KSNP_wts.dta"
ssc install asdoc

#delimit ;
destring, replace
/*Need to destring certain variables: mdvp, hhpop, vilcncl
problem with Nas. Replace them with "." */

describe

list HKD
*Before creating panel dataset, need to relabel years as consecutive periods

generate period=year
replace period = 1 if (period==2001)
replace period = 2 if (period==2002) 
replace period = 3 if (period==2003) 
replace period = 4 if (period==2004) 
replace period = 5 if (period==2005)
replace period = 6 if (period==2006)
replace period = 7 if (period==2007) 
replace period = 8 if (period==2008) 
replace period = 9 if (period==2009) 
replace period = 10 if (period==2010)
replace period = 11 if (period==2011)
replace period = 12 if (period==2012) 
replace period = 13 if (period==2013) 
replace period = 14 if (period==2014) 
replace period = 15 if (period==2015)
replace period = 16 if (period==2016)

generate ksnp_log=log(KSNP_AreaForLoss_Ha2+.01)
generate nonksnp_log=log(nonKSNP_AreaForLoss_Ha+.01)
generate all_log=log(All_AreaForLoss_Ha+.01)

generate HKD_USD=HKD/8936600
generate did2_USD=did2/8936600

*** Checking OLS Results   ***

#delimit ;
reg KSNP_AreaForLoss_Ha2
     f.did1
	 dstrds
	 pop
	 f.period##f.distID2003_d, rob cluster(ID2003_d);

***Setting Time and Individual Variables***

xtset ID2003_d period

********************************************************************************

***FE Models (Long-Term)***

********************************************************************************

*** NO WEIGHTS: KSNP Forest*****************************************************
***Dichotomous Treatment***
***Ha Forest Loss***
#delimit ;
xtreg KSNP_AreaForLoss_Ha2
     f.did1, fe rob cluster(ID2003_d);
#delimit ;
xtreg KSNP_AreaForLoss_Ha2
     f.did1
	 dstrds
	 pop, fe rob cluster(ID2003_d);
#delimit ;
xtreg KSNP_AreaForLoss_Ha2
     f.did1
	 dstrds
	 pop
	 f.period##f.distID2003_d, fe rob cluster(ID2003_d);
	 
***NO WEIGHTS: KSNP Forest***
***Dichotomous Treatment***
***Logged Ha Forest Loss***
#delimit ;
xtreg ksnp_log
     f.did1, fe rob cluster(ID2003_d);
#delimit ;
xtreg ksnp_log
     f.did1
	 dstrds
	 pop, fe rob cluster(ID2003_d);
#delimit ;
xtreg ksnp_log
     f.did1
	 dstrds
	 pop
	 f.period##f.distID2003_d, fe rob cluster(ID2003_d);
	 
***NO WEIGHTS: KSNP Forest***
***Continuous Treatment***
***Ha Forest Loss***
#delimit ;
xtreg KSNP_AreaForLoss_Ha2
     f.did2_USD, fe rob cluster(ID2003_d);
#delimit ;
xtreg KSNP_AreaForLoss_Ha2
     f.did2_USD
     dstrds 
     pop, fe rob cluster(ID2003_d);
#delimit ;
xtreg KSNP_AreaForLoss_Ha2
     f.did2_USD
     dstrds 
     pop
	 f.period##f.distID2003_d, fe rob cluster(ID2003_d);
	 
***NO WEIGHTS: KSNP Forest***
***Continuous Treatment***
***Logged Ha Forest Loss***
#delimit ;
xtreg ksnp_log
     f.did2_USD, fe rob cluster(ID2003_d);
#delimit ;
xtreg ksnp_log
     f.did2_USD
     dstrds 
     pop, fe rob cluster(ID2003_d);
#delimit ;
xtreg ksnp_log
     f.did2_USD
     dstrds 
     pop
	 f.period##f.distID2003_d, fe rob cluster(ID2003_d);

*** NO WEIGHTS: Non-KSNP Forest*************************************************
***Dichotomous Treatment***
***Ha Forest Loss***
#delimit ;
xtreg nonKSNP_AreaForLoss_Ha
     f.did1, fe rob cluster(ID2003_d);
#delimit ;
xtreg nonKSNP_AreaForLoss_Ha
     f.did1
	 dstrds
	 pop, fe rob cluster(ID2003_d);
#delimit ;
xtreg nonKSNP_AreaForLoss_Ha
     f.did1
	 dstrds
	 pop
	 f.period##f.distID2003_d, fe rob cluster(ID2003_d);
*** NO WEIGHTS: Non-KSNP Forest   ***
***Dichotomous Treatment***
***Logged Ha Forest Loss***
#delimit ;
xtreg nonksnp_log
     f.did1, fe rob cluster(ID2003_d);
#delimit ;
xtreg nonksnp_log
     f.did1
     dstrds 
     pop, fe rob cluster(ID2003_d);
#delimit ;
xtreg nonksnp_log
     f.did1
     dstrds 
     pop
	 f.period##f.distID2003_d, fe rob cluster(ID2003_d);
*** NO WEIGHTS: Non-KSNP Forest   ***
***Continuous Treatment***
***Ha Forest Loss***
#delimit ;
xtreg nonKSNP_AreaForLoss_Ha
     f.did2_USD, fe rob cluster(ID2003_d);
#delimit ;
xtreg nonKSNP_AreaForLoss_Ha
     f.did2_USD
     dstrds 
     pop, fe rob cluster(ID2003_d);
#delimit ;
xtreg nonKSNP_AreaForLoss_Ha
     f.did2_USD
     dstrds 
     pop
	 f.period##f.distID2003_d, fe rob cluster(ID2003_d);
	 
*** NO WEIGHTS: Non-KSNP Forest   ***
***Continuous Treatment***
***Logged Ha Forest Loss***
#delimit ;
xtreg nonksnp_log
     f.did2_USD, fe rob cluster(ID2003_d);
#delimit ;
xtreg nonksnp_log
     f.did2_USD
     dstrds 
     pop, fe rob cluster(ID2003_d);
#delimit ;
xtreg nonksnp_log
     f.did2_USD
     dstrds 
     pop
	 f.period##f.distID2003_d, fe rob cluster(ID2003_d);

************************************************************************************************************************************************************************************************************************************
*** MAHALANOBIS MATCHING: KSNP Forest*******************************************
***Dichotomous Treatment***
***Ha Forest Loss***
#delimit ;
xtreg KSNP_AreaForLoss_Ha2
     f.did1
	 [weight=weights_mah], fe rob cluster(ID2003_d);
#delimit ;
xtreg KSNP_AreaForLoss_Ha2
     f.did1
	 dstrds
	 pop
	 [weight=weights_mah], fe rob cluster(ID2003_d);
#delimit ;
xtreg KSNP_AreaForLoss_Ha2
     f.did1
	 dstrds
	 pop
	 f.period##f.distID2003_d
	 [weight=weights_mah], fe rob cluster(ID2003_d);
	 
***MAHALANOBIS MATXCHING: KSNP Forest***
***Dichotomous Treatment***
***Logged Ha Forest Loss***
#delimit ;
xtreg ksnp_log
     f.did1
	 [weight=weights_mah], fe rob cluster(ID2003_d);
#delimit ;
xtreg ksnp_log
     f.did1
	 dstrds
	 pop
	 [weight=weights_mah], fe rob cluster(ID2003_d);
#delimit ;
xtreg ksnp_log
     f.did1
	 dstrds
	 pop
	 f.period##f.distID2003_d
	 [weight=weights_mah], fe rob cluster(ID2003_d);
	 
***MAHALANOBIS MATXCHING: KSNP Forest***
***Continuous Treatment***
***Ha Forest Loss***
#delimit ;
xtreg KSNP_AreaForLoss_Ha2
     f.did2_USD
	 [weight=weights_mah], fe rob cluster(ID2003_d);
#delimit ;
xtreg KSNP_AreaForLoss_Ha2
     f.did2_USD
     dstrds 
     pop
	 [weight=weights_mah], fe rob cluster(ID2003_d);
#delimit ;
xtreg KSNP_AreaForLoss_Ha2
     f.did2_USD
     dstrds 
     pop
	 f.period##f.distID2003_d
	 [weight=weights_mah], fe rob cluster(ID2003_d);
	 
***MAHALANOBIS MATXCHING: KSNP Forest***
***Continuous Treatment***
***Logged Ha Forest Loss***
#delimit ;
xtreg ksnp_log
     f.did2_USD
	 [weight=weights_mah], fe rob cluster(ID2003_d);
#delimit ;
xtreg ksnp_log
     f.did2_USD
     dstrds 
     pop
	 [weight=weights_mah], fe rob cluster(ID2003_d);
#delimit ;
xtreg ksnp_log
     f.did2_USD
     dstrds 
     pop
	 f.period##f.distID2003_d
	 [weight=weights_mah], fe rob cluster(ID2003_d);

*** MAHALANOBIS MATXCHING: Non-KSNP Forest**************************************
***Dichotomous Treatment***
***Ha Forest Loss***
#delimit ;
xtreg nonKSNP_AreaForLoss_Ha
     f.did1
	 [weight=weights_mah], fe rob cluster(ID2003_d);
#delimit ;
xtreg nonKSNP_AreaForLoss_Ha
     f.did1
	 dstrds
	 pop
	 [weight=weights_mah], fe rob cluster(ID2003_d);
#delimit ;
xtreg nonKSNP_AreaForLoss_Ha
     f.did1
	 dstrds
	 pop
	 f.period##f.distID2003_d
	 [weight=weights_mah], fe rob cluster(ID2003_d);
	 
*** MAHALANOBIS MATXCHING: Non-KSNP Forest   ***
***Dichotomous Treatment***
***Logged Ha Forest Loss***
#delimit ;
xtreg nonksnp_log
     f.did1
	 [weight=weights_mah], fe rob cluster(ID2003_d);
#delimit ;
xtreg nonksnp_log
     f.did1
     dstrds 
     pop
	 [weight=weights_mah], fe rob cluster(ID2003_d);
#delimit ;
xtreg nonksnp_log
     f.did1
     dstrds 
     pop
	 f.period##f.distID2003_d
	 [weight=weights_mah], fe rob cluster(ID2003_d);
	 
*** MAHALANOBIS MATXCHING: Non-KSNP Forest   ***
***Continuous Treatment***
***Ha Forest Loss***
#delimit ;
xtreg nonKSNP_AreaForLoss_Ha
     f.did2_USD
	 [weight=weights_mah], fe rob cluster(ID2003_d);
#delimit ;
xtreg nonKSNP_AreaForLoss_Ha
     f.did2_USD
     dstrds 
     pop
	 [weight=weights_mah], fe rob cluster(ID2003_d);
#delimit ;
xtreg nonKSNP_AreaForLoss_Ha
     f.did2_USD
     dstrds 
     pop
	 f.period##f.distID2003_d
	 [weight=weights_mah], fe rob cluster(ID2003_d);
	 
*** MAHALANOBIS MATXCHING: Non-KSNP Forest   ***
***Continuous Treatment***
***Logged Ha Forest Loss***
#delimit ;
xtreg nonksnp_log
     f.did2_USD
	 [weight=weights_mah], fe rob cluster(ID2003_d);
#delimit ;
xtreg nonksnp_log
     f.did2_USD
     dstrds 
     pop
	 [weight=weights_mah], fe rob cluster(ID2003_d);
#delimit ;
xtreg nonksnp_log
     f.did2_USD
     dstrds 
     pop
	 f.period##f.distID2003_d
	 [weight=weights_mah], fe rob cluster(ID2003_d);

	 
***********************************************************************************************************************************************************************************************************************************
*** FULL MATCHING: KSNP Forest**************************************************
***Dichotomous Treatment***
***Ha Forest Loss***
#delimit ;
xtreg KSNP_AreaForLoss_Ha2
     f.did1
	 [weight=weights_full], fe rob cluster(ID2003_d);
#delimit ;
xtreg KSNP_AreaForLoss_Ha2
     f.did1
	 dstrds
	 pop
	 [weight=weights_full], fe rob cluster(ID2003_d);
#delimit ;
xtreg KSNP_AreaForLoss_Ha2
     f.did1
	 dstrds
	 pop
	 f.period##f.distID2003_d
	 [weight=weights_full], fe rob cluster(ID2003_d);
	 
***FULL MATXCHING: KSNP Forest***
***Dichotomous Treatment***
***Logged Ha Forest Loss***
#delimit ;
xtreg ksnp_log
     f.did1
	 [weight=weights_full], fe rob cluster(ID2003_d);
#delimit ;
xtreg ksnp_log
     f.did1
	 dstrds
	 pop
	 [weight=weights_full], fe rob cluster(ID2003_d);
#delimit ;
xtreg ksnp_log
     f.did1
	 dstrds
	 pop
	 f.period##f.distID2003_d
	 [weight=weights_full], fe rob cluster(ID2003_d);
	 
***FULL MATXCHING: KSNP Forest***
***Continuous Treatment***
***Ha Forest Loss***
#delimit ;
xtreg KSNP_AreaForLoss_Ha2
     f.did2_USD
	 [weight=weights_full], fe rob cluster(ID2003_d);
#delimit ;
xtreg KSNP_AreaForLoss_Ha2
     f.did2_USD
     dstrds 
     pop
	 [weight=weights_full], fe rob cluster(ID2003_d);
#delimit ;
xtreg KSNP_AreaForLoss_Ha2
     f.did2_USD
     dstrds 
     pop
	 f.period##f.distID2003_d
	 [weight=weights_full], fe rob cluster(ID2003_d);
	 
***FULL MATXCHING: KSNP Forest***
***Continuous Treatment***
***Logged Ha Forest Loss***
#delimit ;
xtreg ksnp_log
     f.did2_USD
	 [weight=weights_full], fe rob cluster(ID2003_d);
#delimit ;
xtreg ksnp_log
     f.did2_USD
     dstrds 
     pop
	 [weight=weights_full], fe rob cluster(ID2003_d);
#delimit ;
xtreg ksnp_log
     f.did2_USD
     dstrds 
     pop
	 f.period##f.distID2003_d
	 [weight=weights_full], fe rob cluster(ID2003_d);

*** FULL MATXCHING: Non-KSNP Forest*********************************************
***Dichotomous Treatment***
***Ha Forest Loss***
#delimit ;
xtreg nonKSNP_AreaForLoss_Ha
     f.did1
	 [weight=weights_full], fe rob cluster(ID2003_d);
#delimit ;
xtreg nonKSNP_AreaForLoss_Ha
     f.did1
	 dstrds
	 pop
	 [weight=weights_full], fe rob cluster(ID2003_d);
#delimit ;
xtreg nonKSNP_AreaForLoss_Ha
     f.did1
	 dstrds
	 pop
	 f.period##f.distID2003_d
	 [weight=weights_full], fe rob cluster(ID2003_d);
	 
*** FULL MATXCHING: Non-KSNP Forest   ***
***Dichotomous Treatment***
***Logged Ha Forest Loss***
#delimit ;
xtreg nonksnp_log
     f.did1
	 [weight=weights_full], fe rob cluster(ID2003_d);
#delimit ;
xtreg nonksnp_log
     f.did1
     dstrds 
     pop
	 [weight=weights_full], fe rob cluster(ID2003_d);
#delimit ;
xtreg nonksnp_log
     f.did1
     dstrds 
     pop
	 f.period##f.distID2003_d
	 [weight=weights_full], fe rob cluster(ID2003_d);
	 
*** FULL MATXCHING: Non-KSNP Forest   ***
***Continuous Treatment***
***Ha Forest Loss***
#delimit ;
xtreg nonKSNP_AreaForLoss_Ha
     f.did2_USD
	 [weight=weights_full], fe rob cluster(ID2003_d);
#delimit ;
xtreg nonKSNP_AreaForLoss_Ha
     f.did2_USD
     dstrds 
     pop
	 [weight=weights_full], fe rob cluster(ID2003_d);
#delimit ;
xtreg nonKSNP_AreaForLoss_Ha
     f.did2_USD
     dstrds 
     pop
	 f.period##f.distID2003_d
	 [weight=weights_full], fe rob cluster(ID2003_d);
	 
*** FULL MATXCHING: Non-KSNP Forest   ***
***Continuous Treatment***
***Logged Ha Forest Loss***
#delimit ;
xtreg nonksnp_log
     f.did2_USD
	 [weight=weights_full], fe rob cluster(ID2003_d);
#delimit ;
xtreg nonksnp_log
     f.did2_USD
     dstrds 
     pop
	 [weight=weights_full], fe rob cluster(ID2003_d);
#delimit ;
xtreg nonksnp_log
     f.did2_USD
     dstrds 
     pop
	 f.period##f.distID2003_d
	 [weight=weights_full], fe rob cluster(ID2003_d);

***********************************************************************************************************************************************************************************************************************************
*** GENETIC MATCHING: KSNP Forest*******************************************
***Dichotomous Treatment***
***Ha Forest Loss***
#delimit ;
xtreg KSNP_AreaForLoss_Ha2
     f.did1
	 [weight=weights_gen], fe rob cluster(ID2003_d);
#delimit ;
xtreg KSNP_AreaForLoss_Ha2
     f.did1
	 dstrds
	 pop
	 [weight=weights_gen], fe rob cluster(ID2003_d);
#delimit ;
xtreg KSNP_AreaForLoss_Ha2
     f.did1
	 dstrds
	 pop
	 f.period##f.distID2003_d
	 [weight=weights_gen], fe rob cluster(ID2003_d);
	 
***GENETIC MATXCHING: KSNP Forest***
***Dichotomous Treatment***
***Logged Ha Forest Loss***
#delimit ;
xtreg ksnp_log
     f.did1
	 [weight=weights_gen], fe rob cluster(ID2003_d);
#delimit ;
xtreg ksnp_log
     f.did1
	 dstrds
	 pop
	 [weight=weights_gen], fe rob cluster(ID2003_d);
#delimit ;
xtreg ksnp_log
     f.did1
	 dstrds
	 pop
	 f.period##f.distID2003_d
	 [weight=weights_gen], fe rob cluster(ID2003_d);
	 
***GENETIC MATXCHING: KSNP Forest***
***Continuous Treatment***
***Ha Forest Loss***
#delimit ;
xtreg KSNP_AreaForLoss_Ha2
     f.did2_USD
	 [weight=weights_gen], fe rob cluster(ID2003_d);
#delimit ;
xtreg KSNP_AreaForLoss_Ha2
     f.did2_USD
     dstrds 
     pop
	 [weight=weights_gen], fe rob cluster(ID2003_d);
#delimit ;
xtreg KSNP_AreaForLoss_Ha2
     f.did2_USD
     dstrds 
     pop
	 f.period##f.distID2003_d
	 [weight=weights_gen], fe rob cluster(ID2003_d);
	 
***GENETIC MATXCHING: KSNP Forest***
***Continuous Treatment***
***Logged Ha Forest Loss***
#delimit ;
xtreg ksnp_log
     f.did2_USD
	 [weight=weights_gen], fe rob cluster(ID2003_d);
#delimit ;
xtreg ksnp_log
     f.did2_USD
     dstrds 
     pop
	 [weight=weights_gen], fe rob cluster(ID2003_d);
#delimit ;
xtreg ksnp_log
     f.did2_USD
     dstrds 
     pop
	 f.period##f.distID2003_d
	 [weight=weights_gen], fe rob cluster(ID2003_d);

*** GENETIC MATXCHING: Non-KSNP Forest**************************************
***Dichotomous Treatment***
***Ha Forest Loss***
#delimit ;
xtreg nonKSNP_AreaForLoss_Ha
     f.did1
	 [weight=weights_gen], fe rob cluster(ID2003_d);
#delimit ;
xtreg nonKSNP_AreaForLoss_Ha
     f.did1
	 dstrds
	 pop
	 [weight=weights_gen], fe rob cluster(ID2003_d);
#delimit ;
xtreg nonKSNP_AreaForLoss_Ha
     f.did1
	 dstrds
	 pop
	 f.period##f.distID2003_d
	 [weight=weights_gen], fe rob cluster(ID2003_d);
	 
*** GENETIC MATXCHING: Non-KSNP Forest   ***
***Dichotomous Treatment***
***Logged Ha Forest Loss***
#delimit ;
xtreg nonksnp_log
     f.did1
	 [weight=weights_gen], fe rob cluster(ID2003_d);
#delimit ;
xtreg nonksnp_log
     f.did1
     dstrds 
     pop
	 [weight=weights_gen], fe rob cluster(ID2003_d);
#delimit ;
xtreg nonksnp_log
     f.did1
     dstrds 
     pop
	 f.period##f.distID2003_d
	 [weight=weights_gen], fe rob cluster(ID2003_d);
	 
*** GENETIC MATXCHING: Non-KSNP Forest   ***
***Continuous Treatment***
***Ha Forest Loss***
#delimit ;
xtreg nonKSNP_AreaForLoss_Ha
     f.did2_USD
	 [weight=weights_gen], fe rob cluster(ID2003_d);
#delimit ;
xtreg nonKSNP_AreaForLoss_Ha
     f.did2_USD
     dstrds 
     pop
	 [weight=weights_gen], fe rob cluster(ID2003_d);
#delimit ;
xtreg nonKSNP_AreaForLoss_Ha
     f.did2_USD
     dstrds 
     pop
	 f.period##f.distID2003_d
	 [weight=weights_gen], fe rob cluster(ID2003_d);
	 
*** GENETIC MATXCHING: Non-KSNP Forest   ***
***Continuous Treatment***
***Logged Ha Forest Loss***
#delimit ;
xtreg nonksnp_log
     f.did2_USD
	 [weight=weights_gen], fe rob cluster(ID2003_d);
#delimit ;
xtreg nonksnp_log
     f.did2_USD
     dstrds 
     pop
	 [weight=weights_gen], fe rob cluster(ID2003_d);
#delimit ;
xtreg nonksnp_log
     f.did2_USD
     dstrds 
     pop
	 f.period##f.distID2003_d
	 [weight=weights_gen], fe rob cluster(ID2003_d);
