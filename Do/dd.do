capture log close
log using ..\log\dd.log,replace
****DD IMPLEMENTATION;

drop _all
set more 1
set mem 50m

#delimit ;
***Simplest implementation;
use ..\data\hh_9198;
gen exptot0=exptot if year==0;
egen exptot91=max(exptot0), by(nh);
keep if year==1;
gen lexptot91=ln(1+exptot91) if year==1;
gen lexptot98=ln(1+exptot) if year==1;
gen lexptot9891=lexptot98-lexptot91;

ttest lexptot9891 if year==1, by(dmmfd);
ttest lexptot9891 if year==1, by(dfmfd);

***Regression implementation;
use ..\data\hh_9198,clear;
gen lexptot=ln(1+exptot);
gen lnland=ln(1+hhland/100);
gen dmmfd1=dmmfd==1 & year==1;
egen dmmfd98=max(dmmfd1), by(nh);
gen dfmfd1=dfmfd==1 & year==1;
egen dfmfd98=max(dfmfd1), by(nh);
gen dmmfdyr=dmmfd98*year;
gen dfmfdyr=dfmfd98*year;

***Basic model;
reg lexptot year dmmfd98 dmmfdyr;
reg lexptot year dfmfd98 dfmfdyr;

****Full model;
reg lexptot year dmmfd98 dmmfdyr sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight];
reg lexptot year dfmfd98 dfmfdyr sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight];

****Fixed effects: Basic;
xtreg lexptot year dmmfd98 dmmfdyr, fe i(nh);
xtreg lexptot year dfmfd98 dfmfdyr, fe i(nh);

****Fixed effects: Full Model;
xtreg lexptot year dmmfd98 dmmfdyr sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg, fe i(nh);
xtreg lexptot year dfmfd98 dfmfdyr sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg, fe i(nh);

***DD in cross-sectional data;
use ..\data\hh_91,clear;
gen vill=thanaid*10+villid;
gen lexptot=ln(1+exptot);
gen lnland=ln(1+hhland/100);
gen target=hhland<50;
gen progvill=thanaid<25;
gen progtarget=progvill*target;

sum target if progvill==1;

reg lexptot progvill target progtarget;
reg lexptot progvill target progtarget sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight];
xtreg lexptot progvill target progtarget, fe i(vill);
xtreg lexptot progvill target progtarget sexhead agehead educhead lnland, fe i(vill);

****Taking into account initial conditions;
use ..\data\hh_9198,clear;

gen lexptot=ln(1+exptot);
gen lnland=ln(1+hhland/100);
gen dmmfd1=dmmfd==1 & year==1;
egen dmmfd98=max(dmmfd1), by(nh);
gen dfmfd1=dfmfd==1 & year==1;
egen dfmfd98=max(dfmfd1), by(nh);
gen dmmfdyr=dmmfd98*year;
gen dfmfdyr=dfmfd98*year;
drop dmmfd1 dfmfd1;

sort nh year;
by nh: gen dlexptot=lexptot[2]-lexptot[1];
by nh: gen ddmmfd98= dmmfd98[2]- dmmfd98[1];
by nh: gen ddfmfd98= dfmfd98[2]- dfmfd98[1];
by nh: gen ddmmfdyr= dmmfdyr[2]- dmmfdyr[1];
by nh: gen ddfmfdyr= dfmfdyr[2]- dfmfdyr[1];
by nh: gen dsexhead= sexhead[2]- sexhead[1];
by nh: gen dagehead= agehead[2]- agehead[1];
by nh: gen deduchead= educhead[2]- educhead[1];
by nh: gen dlnland= lnland[2]- lnland[1];
by nh: gen dvaccess= vaccess[2]- vaccess[1];
by nh: gen dpcirr= pcirr[2]- pcirr[1];
by nh: gen drice= rice[2]- rice[1];
by nh: gen dwheat= wheat[2]- wheat[1];
by nh: gen dmilk= milk[2]- milk[1];
by nh: gen doil= oil[2]- oil[1];
by nh: gen degg= egg[2]- egg[1];

reg dlexptot ddmmfd98 ddmmfdyr dsexhead dagehead deduchead dlnland dvaccess dpcirr drice dwheat dmilk doil degg
       sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg if year==0 [pw=weight];
reg dlexptot ddfmfd98 ddfmfdyr dsexhead dagehead deduchead dlnland dvaccess dpcirr drice dwheat dmilk doil degg
       sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg if year==0 [pw=weight];

****DD with PSM;
****Male paticipants;
use ..\data\hh_9198,clear;
gen lnland=ln(1+hhland/100);
gen dmmfd1=dmmfd==1 & year==1;
egen dmmfd98=max(dmmfd1), by(nh);
keep if year==0;
pscore dmmfd98 sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight], 
        pscore(ps98) blockid(blockf1) comsup level(0.001);
keep if blockf1!=.;
keep nh;
sort nh;
merge nh using ..\data\hh_9198;
keep if _merge==3;
gen lexptot=ln(1+exptot);
gen lnland=ln(1+hhland/100);
gen dmmfd1=dmmfd==1 & year==1;
egen dmmfd98=max(dmmfd1), by(nh);
gen dmmfdyr=dmmfd98*year;

xtreg lexptot year dmmfd98 dmmfdyr sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg, fe i(nh);

****Female paticipants;
use ..\data\hh_9198,clear;
gen lnland=ln(1+hhland/100);
gen dfmfd1=dfmfd==1 & year==1;
egen dfmfd98=max(dfmfd1), by(nh);
keep if year==0;

pscore dfmfd98 sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight], 
        pscore(ps98) blockid(blockf1) comsup level(0.001);
keep if blockf1!=.;
keep nh;
sort nh;
merge nh using ..\data\hh_9198;
keep if _merge==3;
gen lexptot=ln(1+exptot);
gen lnland=ln(1+hhland/100);
gen dfmfd1=dfmfd==1 & year==1;
egen dfmfd98=max(dfmfd1), by(nh);
gen dfmfdyr=dfmfd98*year;

xtreg lexptot year dfmfd98 dfmfdyr sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg, fe i(nh);

log close;
