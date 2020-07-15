import delimited 
use "C:/Users/JTErbaugh/Dropbox/_DarmouthPostDoc/Publications/KerinciConsLegacy/Data/mdvp_wts.dta"

ssc install asdoc

#delimit ;
/*Need to destring certain variables: mdvp, hhpop, vilcncl
problem with Nas. Replace them with "." */

describe

list HKD
*Before creating panel dataset, need to relabel years as consecutive periods

generate period=year
replace period = 1 if (period==2003)
replace period = 2 if (period==2006) 
replace period = 3 if (period==2008) 
replace period = 4 if (period==2011) 
replace period = 5 if (period==2014)

***Setting Time and Individual Variables***

xtset ID2003_d period

********************************************************************************

***BE Models for dichotomous VS monetary treatment***

********************************************************************************

***Dichotomous***
#delimit ;
xtreg mdvp
     f.VCA_y##c.nonKSNP_AreaForLoss_Ha_trunc 
     KSNP_AreaForLoss_Ha2_trunc 
     dstrds_y 
     hhpop
	 prmmrkt, be wls;
	 
***Monetary***
#delimit ;
xtreg mdvp
     c.HKD##c.nonKSNP_AreaForLoss_Ha_trunc 
     KSNP_AreaForLoss_Ha2_trunc 
     dstrds_y 
     hhpop
	 prmmrkt, be wls;

***No weights***
#delimit ;
xtreg mdvp
     VCA_y
	 nonKSNP_AreaForLoss_Ha_trunc  
	 KSNP_AreaForLoss_Ha2_trunc
     dstrds_y 
     hhpop
	 prmmrkt, fe rob;
	 
***No weights with VCA and nonKSNP_AreaForLoss interaction***
#delimit ;
xtreg mdvp
     f.VCA_y##c.nonKSNP_AreaForLoss_Ha_trunc  
	 KSNP_AreaForLoss_Ha2_trunc
     dstrds_y 
     hhpop
	 prmmrkt, fe rob;
	 
***No weights with VCA and KSNP_AreaForLoss interaction***
#delimit ;
xtreg mdvp
     f.VCA_y##c.KSNP_AreaForLoss_Ha2_trunc
	 nonKSNP_AreaForLoss_Ha_trunc  
     dstrds_y 
     hhpop
	 prmmrkt, fe rob;

estimates store fixed_one

***Mahalanobis Weights***
#delimit ;
xtreg mdvp
     VCA_y
	 nonKSNP_AreaForLoss_Ha_trunc 
     KSNP_AreaForLoss_Ha2_trunc 
     dstrds_y 
     hhpop
	 prmmrkt
	 [weight=weights_mah], fe rob;

hausman fixed_one .

***Mahalanobis Weights and nonKSNP_AreaForLoss interaction***
#delimit ;
xtreg mdvp
     f.VCA_y##c.nonKSNP_AreaForLoss_Ha_trunc 
     KSNP_AreaForLoss_Ha2_trunc 
     dstrds_y 
     hhpop
	 prmmrkt
	 [weight=weights_mah], fe rob;
	 
***Mahalanobis Weights and KSNP_AreaForLoss interaction***
#delimit ;
xtreg mdvp
     f.VCA_y##c.KSNP_AreaForLoss_Ha2_trunc 
     nonKSNP_AreaForLoss_Ha_trunc 
     dstrds_y 
     hhpop
	 prmmrkt
	 [weight=weights_mah], fe rob;


***Genetic Weights***
#delimit ;
xtreg mdvp
     VCA_y
	 nonKSNP_AreaForLoss_Ha_trunc 
     KSNP_AreaForLoss_Ha2_trunc 
     dstrds_y 
     hhpop
	 prmmrkt
	 [weight=weights_gen], fe rob;

***Genetic Weights and nonKSNP_AreaForLoss interaction***
#delimit ;
xtreg mdvp
     f.VCA_y##c.nonKSNP_AreaForLoss_Ha_trunc 
     KSNP_AreaForLoss_Ha2_trunc 
     dstrds_y 
     hhpop
	 prmmrkt
	 [weight=weights_gen], fe rob;	 

***Genetic Weights and KSNP_AreaForLoss interaction***
#delimit ;
xtreg mdvp
     f.VCA_y##c.KSNP_AreaForLoss_Ha2_trunc 
     nonKSNP_AreaForLoss_Ha_trunc 
     dstrds_y 
     hhpop
	 prmmrkt
	 [weight=weights_gen], fe rob;	 
	 
***Full Weights***
#delimit ;
xtreg mdvp
     VCA_y
	 nonKSNP_AreaForLoss_Ha_trunc 
     KSNP_AreaForLoss_Ha2_trunc 
     dstrds_y 
     hhpop
	 prmmrkt
	 [weight=weights_full], fe rob ;


estimates store fixed_two

***Full Weights and nonKSNP_AreaForLoss interaction***
#delimit ;
xtreg mdvp
     f.VCA_y##c.nonKSNP_AreaForLoss_Ha_trunc 
     KSNP_AreaForLoss_Ha2_trunc 
     dstrds_y 
     hhpop
	 prmmrkt
	 [weight=weights_full], fe rob ;

***Full Weights and nonKSNP_AreaForLoss interaction***
#delimit ;
xtreg mdvp
     f.VCA_y##c.KSNP_AreaForLoss_Ha2_trunc 
     nonKSNP_AreaForLoss_Ha_trunc 
     dstrds_y 
     hhpop
	 prmmrkt
	 [weight=weights_full], fe rob ;
	 
#delimit ;
xtreg for_AREA3
vilfract1hrc kecfract1hrc kabfract1hrc provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, re;

hausman fixed_two .

***Two Lags***

#delimit ;
xtreg for_AREA3
L2.vilfract1hrc L2.kecfract1hrc L2.kabfract1hrc L2.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe;


estimates store fixed_three


#delimit ;
xtreg for_AREA3
L2.vilfract1hrc L2.kecfract1hrc L2.kabfract1hrc L2.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, re;

hausman fixed_three .

***One Lead***

#delimit ;
xtreg for_AREA3
F.vilfract1hrc F.kecfract1hrc F.kabfract1hrc F.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe;


estimates store fixed_four


#delimit ;
xtreg for_AREA3
F.vilfract1hrc F.kecfract1hrc F.kabfract1hrc F.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, re;

hausman fixed_four .

********************************************************************************

#delimit ;
xtreg carforchng_MEAN
L.vilfract1hrc L.kecfract1hrc L.kabfract1hrc L.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe;


estimates store fixed_five


#delimit ;
xtreg carforchng_MEAN
L.vilfract1hrc L.kecfract1hrc L.kabfract1hrc L.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, re;

hausman fixed_five .

***SAME PERIOD***

#delimit ;
xtreg carforchng_MEAN
vilfract1hrc kecfract1hrc kabfract1hrc provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe;


estimates store fixed_six


#delimit ;
xtreg carforchng_MEAN
vilfract1hrc kecfract1hrc kabfract1hrc provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, re;

hausman fixed_six .

***Two Lags***

#delimit ;
xtreg carforchng_MEAN
L2.vilfract1hrc L2.kecfract1hrc L2.kabfract1hrc L2.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe;


estimates store fixed_seven


#delimit ;
xtreg carforchng_MEAN
L2.vilfract1hrc L2.kecfract1hrc L2.kabfract1hrc L2.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, re;

hausman fixed_seven .

***One Lead***

#delimit ;
xtreg carforchng_MEAN
F.vilfract1hrc F.kecfract1hrc F.kabfract1hrc F.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe;


estimates store fixed_eight


#delimit ;
xtreg carforchng_MEAN
F.vilfract1hrc F.kecfract1hrc F.kabfract1hrc F.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, re;

hausman fixed_eight .



********************************************************************************

***Same period***

********************************************************************************


*Model 1.1: Two-Way Fixed-Effect for CAR forest change: Robust SE
#delimit ;
asdoc xtreg carforchng_MEAN
vilfract1hrc kecfract1hrc kabfract1hrc provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(robust), save(Ch3_DispModels2.doc);
/*Want to predict margins that "turn off" all village-level changes (L.vilfract1hrc) and compare the
outcome to predicted values with actual villag-level changes*/

*margins, predict(L.vilfract1hrc) ;
*margins, dydx(L.kabfract1hrc) ;



*Model 1.2: Two-Way Fixed-Effect for CAR forest change: Clustered Robust SE at Kabupaten

#delimit ;
asdoc xtreg carforchng_MEAN
vilfract1hrc kecfract1hrc kabfract1hrc provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(cluster kec), save(Ch3_DispModels2.doc);

*Model 1.3: Two-Way Fixed-Effect for Forest Area: Robust SE

#delimit ;
asdoc xtreg for_AREA3
vilfract1hrc kecfract1hrc kabfract1hrc provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(robust), save(Ch3_DispModels2.doc);


*Model 1.4: Two-Way Fixed-Effect for Forest Area: Clustered Robust SE at Kabupaten

#delimit ;
asdoc xtreg for_AREA3
vilfract1hrc kecfract1hrc kabfract1hrc provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(cluster kec), save(Ch3_DispModels2.doc);
			
*Model 1.5: Two-Way Fixed-Effect for Forest Area: Robust SE

#delimit ;
asdoc xtreg for_AREA4
vilfract1hrc kecfract1hrc kabfract1hrc provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(robust), save(Ch3_DispModels2.doc);


*Model 1.6: Two-Way Fixed-Effect for Forest Area: Clustered Robust SE at Kabupaten

#delimit ;
asdoc xtreg for_AREA4
vilfract1hrc kecfract1hrc kabfract1hrc provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(cluster kec), save(Ch3_DispModels2.doc);
			
*Model 1.6: Two-Way Fixed-Effect for Forest Area: Clustered Robust SE at Kabupaten

#delimit ;
asdoc xtreg for_AREA4
vilfract1hrc kabkecfract1hrc provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(cluster kec), save(Ch3_DispModels2.doc);
			
			
********************************************************************************

***One lag period***

********************************************************************************

*Model 2.1: Two-Way Fixed-Effect for CAR forest change: Robust SE
#delimit ;
asdoc xtreg carforchng_MEAN
L.vilfract1hrc L.kecfract1hrc L.kabfract1hrc L.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(robust), save(Ch3_DispModels2.doc);
/*Want to predict margins that "turn off" all village-level changes (L.vilfract1hrc) and compare the
outcome to predicted values with actual villag-level changes*/

*margins, predict(L.vilfract1hrc) ;
*margins, dydx(L.kabfract1hrc) ;



*Model 2.2: Two-Way Fixed-Effect for CAR forest change: Clustered Robust SE at Kabupaten
#delimit ;
asdoc xtreg carforchng_MEAN
L.vilfract1hrc L.kecfract1hrc L.kabfract1hrc L.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(cluster kab), save(Ch3_DispModels2.doc);

*Model 2.3: Two-Way Fixed-Effect for Forest Area: Robust SE

#delimit ;
asdoc xtreg for_AREA3
L.vilfract1hrc L.kecfract1hrc L.kabfract1hrc L.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(robust), save(Ch3_DispModels2.doc);


*Model 2.4: Two-Way Fixed-Effect for Forest Area: Clustered Robust SE at Kabupaten

#delimit ;
asdoc xtreg for_AREA3
L.vilfract1hrc L.kecfract1hrc L.kabfract1hrc L.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(cluster kab), save(Ch3_DispModels2.doc);

*Model 2.5: Two-Way Fixed-Effect for Forest Area: Robust SE

clonevar vilfract1hrc2 = vilfract1hrc
clonevar kabfract1hrc2 = kabfract1hrc

#delimit ;
xtreg for_AREA4
L.vilfract1hrc L.kecfract1hrc L.kabfract1hrc L.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(robust);
			
margins
margins, at(L.vilfract1hrc2=0)
margins, at(L1.kabfract1hrc2=0)

*predict p1
*#replace vilfract1hrc2 = 0
*predict p2
*gen vil_counter1=sinh(p1)
*mean vil_counter1
*gen vil_counter2 = sinh(p2)
*mean vil_counter2

*Model 2.6: Two-Way Fixed-Effect for Forest Area: Clustered Robust SE at Kabupaten

#delimit ;
asdoc xtreg for_AREA4
L.vilfract1hrc L.kecfract1hrc L.kabfract1hrc L.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(cluster kec), save(Ch3_DispModels2.doc);
			
*Model 2.6: Two-Way Fixed-Effect for Forest Area: Clustered Robust SE at Kabupaten

#delimit ;
asdoc xtreg for_AREA4
L.vilfract1hrc L.kabkecfract1hrc L.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(cluster kec), save(Ch3_DispModels2.doc);
			
********************************************************************************

***Two lag period***

********************************************************************************

*Model 3.1: Two-Way Fixed-Effect for CAR forest change: Robust SE
#delimit ;
asdoc xtreg carforchng_MEAN
L2.vilfract1hrc L2.kecfract1hrc L2.kabfract1hrc L2.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(robust), save(Ch3_DispModels2.doc);
/*Want to predict margins that "turn off" all village-level changes (L.vilfract1hrc) and compare the
outcome to predicted values with actual villag-level changes*/

*margins, predict(L.vilfract1hrc) ;
*margins, dydx(L.kabfract1hrc) ;



*Model 3.2: Two-Way Fixed-Effect for CAR forest change: Clustered Robust SE at Kabupaten

#delimit ;
asdoc xtreg carforchng_MEAN
L2.vilfract1hrc L2.kecfract1hrc L2.kabfract1hrc L2.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(cluster kab), save(Ch3_DispModels2.doc);

*Model 3.3: Two-Way Fixed-Effect for Forest Area: Robust SE

#delimit ;
asdoc xtreg for_AREA3
L2.vilfract1hrc L2.kecfract1hrc L2.kabfract1hrc L2.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(robust), save(Ch3_DispModels2.doc);


*Model 3.4: Two-Way Fixed-Effect for Forest Area: Clustered Robust SE at Kabupaten

#delimit ;
asdoc xtreg for_AREA3
L2.vilfract1hrc L2.kecfract1hrc L2.kabfract1hrc L2.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(cluster kab), save(Ch3_DispModels2.doc);
	
*Model 3.5: Two-Way Fixed-Effect for Forest Area: Robust SE

#delimit ;
asdoc xtreg for_AREA4
L2.vilfract1hrc L2.kecfract1hrc L2.kabfract1hrc L2.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(robust), save(Ch3_DispModels2.doc);


*Model 3.6: Two-Way Fixed-Effect for Forest Area: Clustered Robust SE at Kabupaten

#delimit ;
asdoc xtreg for_AREA4
L2.vilfract1hrc L2.kecfract1hrc L2.kabfract1hrc L2.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(cluster kec), save(Ch3_DispModels2.doc);
			
*Model 3.6: Two-Way Fixed-Effect for Forest Area: Clustered Robust SE at Kabupaten

#delimit ;
asdoc xtreg for_AREA4
L2.vilfract1hrc L2.kabkecfract1hrc L2.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(cluster kec), save(Ch3_DispModels2.doc);
			
********************************************************************************

***One Lead Period***

********************************************************************************

*Model 4.1: Two-Way Fixed-Effect for CAR forest change: Robust SE
#delimit ;
asdoc xtreg carforchng_MEAN
F.vilfract1hrc F.kecfract1hrc F.kabfract1hrc F.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(robust), save(Ch3_DispModels2.doc);
/*Want to predict margins that "turn off" all village-level changes (L.vilfract1hrc) and compare the
outcome to predicted values with actual villag-level changes*/

*margins, predict(L.vilfract1hrc) ;
*margins, dydx(L.kabfract1hrc) ;



*Model 4.2: Two-Way Fixed-Effect for CAR forest change: Clustered Robust SE at Kabupaten

#delimit ;
asdoc xtreg carforchng_MEAN
F.vilfract1hrc F.kecfract1hrc F.kabfract1hrc F.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(cluster kab), save(Ch3_DispModels2.doc);

*Model 4.3: Two-Way Fixed-Effect for Forest Area: Robust SE

#delimit ;
asdoc xtreg for_AREA3
F.vilfract1hrc F.kecfract1hrc F.kabfract1hrc F.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(robust), save(Ch3_DispModels2.doc);


*Model 4.4: Two-Way Fixed-Effect for Forest Area: Clustered Robust SE at Kabupaten

#delimit ;
asdoc xtreg for_AREA3
F.vilfract1hrc F.kecfract1hrc F.kabfract1hrc F.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(cluster kab), save(Ch3_DispModels2.doc);
	
*Model 4.5: Two-Way Fixed-Effect for Forest Area: Robust SE

#delimit ;
asdoc xtreg for_AREA4
F.vilfract1hrc F.kecfract1hrc F.kabfract1hrc F.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(robust), save(Ch3_DispModels2.doc);


*Model 4.6: Two-Way Fixed-Effect for Forest Area: Clustered Robust SE at Kabupaten

#delimit ;
asdoc xtreg for_AREA4
F.vilfract1hrc F.kecfract1hrc F.kabfract1hrc F.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(cluster kec), save(Ch3_DispModels2.doc);


********************************************************************************
***Running FE model on just dispersed villages***
********************************************************************************

keep if evvilfract_hrc==1 | evkecfract_hrc==1 | evkabfract_hrc==1 | evprovfract_hrc==1  

*Model 5.1: Two-Way Fixed-Effect for CAR forest change: Robust SE
#delimit ;
asdoc xtreg carforchng_MEAN
L.vilfract1hrc L.kecfract1hrc L.kabfract1hrc L.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(robust), save(Ch3_DispModels3.doc);
/*Want to predict margins that "turn off" all village-level changes (L.vilfract1hrc) and compare the
outcome to predicted values with actual villag-level changes*/

*margins, predict(L.vilfract1hrc) ;
*margins, dydx(L.kabfract1hrc) ;



*Model 5.2: Two-Way Fixed-Effect for CAR forest change: Clustered Robust SE at Kabupaten
#delimit ;
asdoc xtreg carforchng_MEAN
L.vilfract1hrc L.kecfract1hrc L.kabfract1hrc L.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(cluster kec), save(Ch3_DispModels3.doc);

*Model 5.3: Two-Way Fixed-Effect for Forest Area: Robust SE

#delimit ;
asdoc xtreg for_AREA3
L.vilfract1hrc L.kecfract1hrc L.kabfract1hrc L.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(robust), save(Ch3_DispModels3.doc);


*Model 5.4: Two-Way Fixed-Effect for Forest Area: Clustered Robust SE at Kabupaten

#delimit ;
asdoc xtreg for_AREA3
L.vilfract1hrc L.kecfract1hrc L.kabfract1hrc L.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(cluster kec), save(Ch3_DispModels3.doc);
	
*Model 5.5: Two-Way Fixed-Effect for Forest Area: Robust SE

#delimit ;
asdoc xtreg for_AREA4
L.vilfract1hrc L.kecfract1hrc L.kabfract1hrc L.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(robust), save(Ch3_DispModels3.doc);


*Model 5.6: Two-Way Fixed-Effect for Forest Area: Clustered Robust SE at Kabupaten

#delimit ;
asdoc xtreg for_AREA4
L.vilfract1hrc L.kecfract1hrc L.kabfract1hrc L.provfract1
            L.for_MEAN
            L.pdyld_MEAN
            L.dryldag_MEAN
            L.mixdag_MEAN 
            L.tmbfor_MEAN
            L.agfor_MEAN
            L.stlmnt_MEAN
            L.PAs_MEAN
			L.vilcncl 
			L.dstrds
            L.hhpop
            L.mdvp, fe vce(cluster kec), save(Ch3_DispModels3.doc);
