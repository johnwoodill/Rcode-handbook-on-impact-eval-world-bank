capture log close
log using ..\log\rd.log,replace

****IMPLEMENTATION OF REGRESSION DISCONTINUITY; 
drop _all
set more 1
set mem 50m

#delimit ;
use ..\data\hh_98,clear;
gen lexptot=ln(1+exptot);
gen lnland=ln(1+hhland/100);

*****Program for Sharp Discontinuity;
drop if (hhland<50 & (dmmfd==0|dfmfd==0))|(hhland>=50 & (dmmfd==1|dfmfd==1));
capture prog drop rd_sharp;
prog rd_sharp, rclass;
  version 8.2;
  args outcome;
  confirm var `outcome';
  tempname outrd1 outrd0 outcome1 outcome0;
  locpoly `outcome' lnland if hhland<50, gen(`outrd1') at(lnland) nogr tri w(3) d(1);  
  locpoly `outcome' lnland if hhland>=50, gen(`outrd0') at(lnland) nogr tri w(3) d(1);
  sum `outrd1' if hhland>=45 & hhland<50, meanonly;
  scalar `outcome1'=r(mean); 
  sum `outrd0' if hhland>=50 & hhland<55, meanonly;
  scalar `outcome0'=r(mean); 
  return scalar diff_outcome=`outcome1'-`outcome0';
end;

****Participation;
set seed 12345;
bootstrap "rd_sharp lexptot" impact_sharp=r(diff_outcome), reps(100) nowarn;
gen t_impact_sharp=_b[impact_sharp]/_se[impact_sharp];
sum t_impact_sharp;


use ..\data\hh_98,clear;
gen lexptot=ln(1+exptot);
gen lnland=ln(1+hhland/100);
*****Program for Fuzzy Discontinuity;
capture prog drop rd_fuzzy;
prog rd_fuzzy, rclass;
  version 8.2;
  args treatment outcome;
  confirm var `treatment';
  confirm var `outcome';
  tempname treatrd1 treatrd0 outrd1 outrd0 treat1 treat0 outcome1 outcome0;
  locpoly `treatment' lnland if hhland<50, gen(`treatrd1') at(lnland) nogr tri w(3) d(1);  
  locpoly `treatment' lnland if hhland>=50, gen(`treatrd0') at(lnland) nogr tri w(3) d(1); 
  locpoly `outcome' lnland if hhland<50, gen(`outrd1') at(lnland) nogr tri w(3) d(1);  
  locpoly `outcome' lnland if hhland>=50, gen(`outrd0') at(lnland) nogr tri w(3) d(1); 
  sum `treatrd1' if hhland>=45 & hhland<=55, meanonly;
  scalar `treat1'=r(mean); 
  sum `treatrd0' if hhland>=45 & hhland<=55, meanonly;
  scalar `treat0'=r(mean); 
  sum `outrd1' if hhland>=45 & hhland<=55, meanonly;
  scalar `outcome1'=r(mean); 
  sum `outrd0' if hhland>=45 & hhland<=55, meanonly;
  scalar `outcome0'=r(mean); 

  return scalar impact=(`outcome1'-`outcome0')/(`treat1'-`treat0');
end;
***Male participation;
set seed 12345;
bootstrap "rd_fuzzy dmmfd lexptot" impact_fuzzy_m=r(impact), reps(100) nowarn;
gen t_impact_fuzzy_m=_b[impact_fuzzy_m]/_se[impact_fuzzy_m];
sum t_impact_fuzzy_m;

***Female participation;
set seed 123;
bootstrap "rd_fuzzy dfmfd lexptot" impact_fuzzy_f=r(impact), reps(100) nowarn;
gen t_impact_fuzzy_f=_b[impact_fuzzy_f]/_se[impact_fuzzy_f];
sum t_impact_fuzzy_f;

log close;
