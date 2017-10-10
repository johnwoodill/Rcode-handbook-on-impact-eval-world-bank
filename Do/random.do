capture log close
log using ..\log\random.log,replace

drop _all
set more 1
set mem 50m

#delimit ;
use ..\data\hh_98;
gen lexptot=ln(1+exptot);
gen lnland=ln(1+hhland/100);
gen vill=thanaid*10+villid;
egen progvillm=max(dmmfd), by(vill);
egen progvillf=max(dfmfd), by(vill);

***Impacts of program placement;  
****t-test;
ttest lexptot, by(progvillm);
ttest lexptot, by(progvillf);

****Regression implementation;
reg lexptot progvillm;
reg lexptot progvillf;

****Expanded regression
reg lexptot progvillm sexhead agehead educhead lnland vaccess pcirr rice wheat
     milk oil egg [pw=weight];
reg lexptot progvillf sexhead agehead educhead lnland vaccess pcirr rice wheat 
     milk oil egg [pw=weight];

***Impacts of program participation;  
****t-test;
ttest lexptot, by(dmmfd);
ttest lexptot, by(dfmfd);

****Regression implementation;
reg lexptot dmmfd;
reg lexptot dfmfd;

****Expanded regression;
reg lexptot dmmfd sexhead agehead educhead lnland vaccess pcirr rice wheat
     milk oil egg [pw=weight];
reg lexptot dfmfd sexhead agehead educhead lnland vaccess pcirr rice wheat 
     milk oil egg [pw=weight];

****Expanded regression: capturing both program placement and participation; 
reg lexptot dmmfd progvillm sexhead agehead educhead lnland vaccess pcirr rice 
     wheat milk oil egg [pw=weight];
reg lexptot dfmfd progvillf sexhead agehead educhead lnland vaccess pcirr rice 
     wheat milk oil egg [pw=weight];

***Impacts of program participation in program villages;
reg lexptot dmmfd if progvillm==1 [pw=weight];
reg lexptot dfmfd if progvillf==1 [pw=weight];
reg lexptot dmmfd sexhead agehead educhead lnland vaccess pcirr rice wheat milk 
     oil egg if progvillm==1 [pw=weight];
reg lexptot dfmfd sexhead agehead educhead lnland vaccess pcirr rice wheat milk 
     oil egg if progvillf==1 [pw=weight];

***Spliiover effects of program placement;
reg lexptot progvillm if dmmfd==0 [pw=weight];
reg lexptot progvillf if dfmfd==0 [pw=weight];

reg lexptot progvillm sexhead agehead educhead lnland vaccess pcirr rice wheat 
     milk oil egg if dmmfd==0 [pw=weight];
reg lexptot progvillf sexhead agehead educhead lnland vaccess pcirr rice wheat 
    milk oil egg if dfmfd==0 [pw=weight];

log close
