capture log close
log using ..\log\psm.log,replace

drop _all
set more 1
set mem 50m

use ..\data\hh_98
gen lexptot=ln(1+exptot)
gen lnland=ln(1+hhland/100)

#delimit ;
****Impacts of program participation;

***Male participants; 
****pscore equation;
pscore dmmfd sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight], 
       pscore(ps98) blockid(blockf1) comsup level(0.001);
drop ps98 blockf1;
pscore dmmfd sexhead agehead educhead vaccess pcirr rice wheat milk oil [pw=weight], 
       pscore(ps98) blockid(blockf1) comsup level(0.001);

****Nearest Neighbor Matching;
attnd lexptot dmmfd [pweight=weight], pscore(ps98) comsup;

****Stratification Matching;
atts lexptot dmmfd, pscore(ps98) blockid(blockf1) comsup;


****Radius Matching;
attr lexptot dmmfd, pscore(ps98) radius(0.001) comsup;


****Kernel Matching;
attk lexptot dmmfd, pscore(ps98) comsup bootstrap reps(50);

drop ps98 blockf1;

***Female participants; 
****pscore equation;
pscore dfmfd sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight], 
       pscore(ps98) blockid(blockf1) comsup level(0.001);

****Nearest Neighbor Matching;
attnd lexptot dfmfd [pweight=weight], pscore(ps98) comsup;

****Stratification Matching;
atts lexptot dfmfd, pscore(ps98) blockid(blockf1) comsup;


****Radius Matching;
attr lexptot dfmfd, pscore(ps98) radius(0.001) comsup;

****Kernel Matching;
attk lexptot dfmfd, pscore(ps98) comsup bootstrap reps(50);

****Direct Matching using Nearest neighbor;
nnmatch lexptot dmmfd sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight], tc(att) m(1);
nnmatch lexptot dfmfd sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight], tc(att) m(1);
log close;
