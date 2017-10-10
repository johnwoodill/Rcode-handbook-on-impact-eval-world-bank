capture log close
log using ..\log\iv.log,replace

drop _all
set more 1
set mem 50m

#delimit ;
****IV using ivreg implementation;
use ..\data\hh_98,clear;
gen lexptot=ln(1+exptot);
gen lnland=ln(1+hhland/100);
gen vill=thanaid*10+villid;
egen villmmf=max(dmmfd), by(vill);
gen mchoice=villmmf==1 & hhland<50;
for var agehead-educhead lnland vaccess pcirr rice-oil: gen mchX=mchoice*X;
egen villfmf=max(dfmfd), by(vill);
gen fchoice=villfmf==1 & hhland<50;
for var agehead-educhead lnland vaccess pcirr rice-oil: gen fchX=fchoice*X;

****Male participation;
ivreg lexptot agehead-educhead lnland vaccess pcirr rice-oil (dmmfd= agehead-educhead 
      lnland vaccess pcirr rice-oil mch*);
****Test for endogeneity;
ivendog;

****Female participation;
ivreg lexptot agehead-educhead lnland vaccess pcirr rice-oil (dfmfd= agehead-educhead 
      lnland vaccess pcirr rice-oil fch*), first;
****Test for endogeneity;
ivendog;

****IV using treatreg implementation;
treatreg lexptot agehead-educhead lnland vaccess pcirr rice-oil, treat (dmmfd= agehead-educhead 
         lnland vaccess pcirr rice-oil mch*);
treatreg lexptot agehead-educhead lnland vaccess pcirr rice-oil, treat (dfmfd= agehead-educhead 
         lnland vaccess pcirr rice-oil fch*);

****IV with FE implementation in cross-sectional data;
use ..\data\hh_98,clear;
gen lexptot=ln(1+exptot);
gen lnland=ln(1+hhland/100);
gen vill=thanaid*10+villid;
egen villmmf=max(dmmfd), by(vill year);
gen mchoice=villmmf==1 & hhland<50;
for var agehead-educhead lnland vaccess pcirr rice-oil: gen mchX=mchoice*X;
egen villfmf=max(dfmfd), by(vill year);
gen fchoice=villfmf==1 & hhland<50;
for var agehead-educhead lnland vaccess pcirr rice-oil: gen fchX=fchoice*X;
xtivreg lexptot year agehead-educhead lnland vaccess pcirr rice-oil (dmmfd= agehead-educhead 
        lnland vaccess pcirr rice-oil mch*), fe i(vill);
****Test for endogeneity;
dmexogxt;
xtivreg lexptot year agehead-educhead lnland vaccess pcirr rice-oil (dfmfd= agehead-educhead 
        lnland vaccess pcirr rice-oil mch*), fe i(vill);
****Test for endogeneity;
dmexogxt;

****IV with FE implementation in panel data;
use ..\data\hh_9198,clear;
gen lexptot=ln(1+exptot);
gen lnland=ln(1+hhland/100);
gen vill=thanaid*10+villid;
egen villmmf=max(dmmfd), by(vill year);
gen mchoice=villmmf==1 & hhland<50;
for var agehead-educhead lnland vaccess pcirr rice-oil: gen mchX=mchoice*X;
egen villfmf=max(dfmfd), by(vill year);
gen fchoice=villfmf==1 & hhland<50;
for var agehead-educhead lnland vaccess pcirr rice-oil: gen fchX=fchoice*X;

xtivreg lexptot year agehead-educhead lnland vaccess pcirr rice-oil (dmmfd= agehead-educhead 
        lnland vaccess pcirr rice-oil mch*), fe i(nh);

****Test for endogeneity;
dmexogxt;
xtivreg lexptot year agehead-educhead lnland vaccess pcirr rice-oil (dfmfd= agehead-educhead 
        lnland vaccess pcirr rice-oil fch*), fe i(nh);
****Test for endogeneity;
dmexogxt;
log close;

