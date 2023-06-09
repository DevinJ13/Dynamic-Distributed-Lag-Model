gen month=month(time)
tab month 

*look for command to rename months 
label define month 1 "Jan" 2 "Feb" 3 "March" 
label values month month
tab month 
graph bar (sum) sales adv, over (month)


tsline sales adv,ytitle("Unit Sales") graphregion(color(white))




****OLS************** 
reg sales adv 
predict hat 

tsline sales adv hat, lpattern(. dash) graphregion(color(white)) ///
ytitle(Sales) xtitle(Time) xlab(0(10)90, labsize(small)) ///
legend(label(1 "Actual") label(2 "Predicted"))

****RESIDUALS**
predict residual, residual
tsline residual, yline(0) name(graph1,replace)  
//*have to describe if it has a correlation positive, negative no correlation********


*****AUTO CORRELATION** **AND PARTIAL CORRELATION**

ac residual, lags(20) note("") name(graph1, replace) graphregion(color(white)) 
pac residual,lags(20) note("") name(graph2,replace) graphregion(color(white)) 
graph combine graph1 graph2, ycommon t1(Autocorrelation) note("95% Confidence Bands") ///
graphregion(color(white))

 
________________________________________________________________________________
*adv is the y variable dependent***
*sales is the x variable independent***
***************HILDRETH-LU*********
prais adv sales,sse 
estimates store HL 
estimates table OLS HAC COI PW HL,se t 

display _b[_cons]/(1-e(rho))
outreg using "C:\Users\devin\OneDrive\decomposition\simulation", replace 


******LAG MODEL ********

gen lag1_adv=adv[_n-1]
gen lag2_adv=adv[_n-2]

reg sales adv lag1_adv 
reg sales adv lag1_adv lag2_adv 
 ///**two period line 



prais sales adv lag1_adv,corc

******NEW SCHOOL METHOD*********

gen lag1_advb=L1.adv
gen lag2_advb=L2.adv

reg sales adv lag1_advb lag2_advb 



***Newer School Method-The Lag Operator-Data must be time set 
*don't need to generate new variables*******

reg sales adv L1.adv 

reg sales adv L1.adv L2.adv

reg sales adv L1.adv L2.adv L3.adv

reg sales adv L1.adv L2.adv L3.adv L4.adv

reg sales adv L1.adv L2.adv L3.adv L4.adv L5.adv 

reg sales adv L1.adv L2.adv L3.adv L4.adv L5.adv L6.adv

reg sales adv L1.adv L2.adv L3.adv L4.adv L5.adv L6.adv L7.adv

reg sales adv L1.adv L2.adv L3.adv L4.adv L5.adv L6.adv L7.adv L8.adv

reg sales adv L1.adv L2.adv L3.adv L4.adv L5.adv L6.adv L7.adv L8.adv L9.adv










 


reg sales adv L(0/20).adv 
outreg using "C:\Users\devin\OneDrive\decomposition\BAYERSMODEL", replace 


estimates store DLM 

****lag selection the bayes (aka schwarz) information criteion (BIC) the Akaike information criteion (AIC)
reg sales adv

*to retrive the IC use estat a post regression command
estat ic 

*to save the IC*
estimates store zero 

reg sales L(0/1).adv 
estimates store one 

reg sales L(0/2). adv

estimates store two

reg sales L(0/3).adv 

estimates store three

reg sales L(0/4).adv 

estimates store four

reg sales L(0/5).adv 

estimates store five

reg sales L(0/6).adv  

estimates store six

reg sales L(0/7).adv  

estimates store seven

reg sales L(0/8).adv  

estimates store eight

reg sales L(0/9).adv 

estimates store nine

reg sales L(0/10).adv  

estimates store ten

*NOTE SET THE SAMPLE SIZE TO THE VALUE WITHTHE MAX LAGS*
estimates stats zero one two three four five six seven eight nine ten,n(20)

*AIC keeps getting smaller (more negative) BIC gets larger (less negative) after the first lag, but gets smaller after the eigth lag
*should go with one-lag, but going to use six for pedagogical purposes.

*Using Loops to calculate the IC's* 193 ***
 ***20 regressions**
forvalue i=0/20{        
reg sales L(0/`i').adv 
estat ic
estimates store ic`i'
}

estimates stats ic0 ic1 ic2 ic3 ic4 ic5 ic6 ic7 ic8,n(20)

estimates stats ic*,n(20)

**Graphing the Lag Coefficients**

reg sales L(0/20).adv 
outreg using "C:\Users\devin\OneDrive\decomposition\LAGADV", replace 


gen lag=_n-1

gen lbeta1=_b[adv] if lag==0

replace lbeta1=_b[L1.adv] if lag==1

replace lbeta1=_b[L2.adv] if lag==2

replace lbeta1=_b[L3.adv] if lag==3

replace lbeta1=_b[L4.adv] if lag==4

replace lbeta1=_b[L5.adv] if lag==5

replace lbeta1=_b[L6.adv] if lag==6

replace lbeta1=_b[L7.adv] if lag==7

replace lbeta1=_b[L8.adv] if lag==8

line lbeta1 lag if lag<9, sort ytitle("Coefficient on Lagged Advertising") graphregion(color(white))

**95% Confidence Interval on Lagged Coefficients** 
gen lci=_b[adv]-_se[adv]*invttail(34,.05/2) if lag==0
replace lci=_b[L1.adv]-_se[L1.adv]*invttail(34,.05/2) if lag==1
replace lci=_b[L2.adv]-_se[L2.adv]*invttail(34,.05/2) if lag==2
replace lci=_b[L3.adv]-_se[L3.adv]*invttail(34,.05/2) if lag==3 
replace lci=_b[L4.adv]-_se[L4.adv]*invttail(34,.05/2) if lag==4
replace lci=_b[L5.adv]-_se[L5.adv]*invttail(34,.05/2) if lag==5 
replace lci=_b[L6.adv]-_se[L6.adv]*invttail(34,.05/2) if lag==6
replace lci=_b[L7.adv]-_se[L7.adv]*invttail(34,.05/2) if lag==7
replace lci=_b[L8.adv]-_se[L8.adv]*invttail(34,.05/2) if lag==8


gen uci=_b[adv]+_se[adv]*invttail(34,.05/2) if lag==0
replace uci=_b[L1.adv]+_se[L1.adv]*invttail(34,.05/2) if lag==1
replace uci=_b[L2.adv]+_se[L2.adv]*invttail(34,.05/2) if lag==2 
replace uci=_b[L3.adv]+_se[L3.adv]*invttail(34,.05/2) if lag==3
replace uci=_b[L4.adv]+_se[L4.adv]*invttail(34,.05/2) if lag==4
replace uci=_b[L5.adv]+_se[L5.adv]*invttail(34,.05/2) if lag==5 
replace uci=_b[L6.adv]+_se[L6.adv]*invttail(34,.05/2) if lag==6
replace uci=_b[L7.adv]+_se[L7.adv]*invttail(34,.05/2) if lag==7
replace uci=_b[L8.adv]+_se[L8.adv]*invttail(34,.05/2) if lag==8
display invttail(34,.05/2)


line lbeta lci uci lag if lag<9, sort lpattern(. dash dash) lcolor(. gray gray) xlab(0(1)8) xtitle("Lag") ytitle("Dollar Change in Sales") yline(0) graphregion(color(white)) legend(label(1 "Lagged Coefficient") label(2 "95% CI") order (1 2))










*Using a loop to generate lagged coefficients*

reg sales L(0/20).adv 

drop lbeta1 lci uci

gen lbeta1=_b[adv] if lag==0 
forvalue i=1/20{
replace lbeta1=_b[L`i'.adv] if lag==`i'
}

scatter lbeta1 lag if lag<20, sort c(l) s(i) ytitle("Coefficient on Lagged Advertising") graphregion(color(white))

*Using a loop to generate the confidence interval*

gen lci1=_b[adv]-_se[adv]*invttail(34,.05/2) if lag==0
forvalue i=1/20{
replace lci1=_b[L`i'.adv]-_se[L`i'.adv]*invttail(34,.05/2) if lag==`i'
}

gen uci1=_b[adv]+_se[adv]*invttail(34,.05/2) if lag==0

forvalue i=1/20{
replace uci1=_b[L`i'.adv]+_se[L`i'.adv]*invttail(34,.05/2) if lag==`i'
}

scatter lbeta lci uci lag if lag<20,sort c(l l l) s(i i i) lpattern(. dash dash) lcolor(. gray gray) xlab(0(1)20) xtitle("Lag") ytitle("Dollar Change in Sales") yline(0) graphregion(color(white)) legend(label(1 "Lagged Coefficient") label(2 "95% CI") order(1 2))


__________________________________________________________
second data set 
use "C:\Users\devin\Downloads\lingering_effects.dta"

gen month=month(time)
tab month 

*look for command to rename months 
label define month 1 "Jan" 2 "Feb"  
label values month month
tab month 
graph bar (sum) sales adv, over (month)


tsset time 

tsline sales adv,ytitle("Unit Sales") graphregion(color(white))


reg sales adv L(0/8).adv 
outreg using "C:\Users\devin\OneDrive\decomposition\BAYERSMODEL", replace 


estimates store DLM 

****lag selection the bayes (aka schwarz) information criteion (BIC) the Akaike information criteion (AIC)
reg sales adv

*to retrive the IC use estat a post regression command
estat ic 

*to save the IC*
estimates store zero 

reg sales L(0/1).adv 
estimates store one 

reg sales L(0/2). adv

estimates store two

reg sales L(0/3).adv 

estimates store three

reg sales L(0/4).adv 

estimates store four

reg sales L(0/5).adv 

estimates store five

reg sales L(0/6).adv  

estimates store six

reg sales L(0/7).adv  

estimates store seven

reg sales L(0/8).adv  

estimates store eight

reg sales L(0/9).adv 

estimates store nine

reg sales L(0/10).adv  

estimates store ten

*NOTE NEED TO SET THE SAMPLE SIZE TO THE VALUE WITH THE MAX LAGS*
estimates stats zero one two three four five six seven eight nine ten,n(78)

*AIC keeps getting smaller (more negative) BIC gets larger (less negative) after the first lag, but gets smaller after the eigth lag
*should go with one-lag, but let's use six for pedagogical purposes.

*Using Loops to calculate the IC's* 193 ***
 ***20 regressions**
forvalue i=0/20{        
reg sales L(0/`i').adv 
estat ic
estimates store ic`i'
}

estimates stats ic0 ic1 ic2 ic3 ic4 ic5 ic6 ic7 ic8,n(20)

estimates stats ic*,n(20)

**Graphing the Lag Coefficients**

reg sales L(0/20).adv 
outreg using "C:\Users\devin\OneDrive\decomposition\LAGADV", replace 


gen lag=_n-1

gen lbeta1=_b[adv] if lag==0

replace lbeta1=_b[L1.adv] if lag==1

replace lbeta1=_b[L2.adv] if lag==2

replace lbeta1=_b[L3.adv] if lag==3

replace lbeta1=_b[L4.adv] if lag==4

replace lbeta1=_b[L5.adv] if lag==5

replace lbeta1=_b[L6.adv] if lag==6

replace lbeta1=_b[L7.adv] if lag==7

replace lbeta1=_b[L8.adv] if lag==8

line lbeta1 lag if lag<21, sort ytitle("Coefficient on Lagged Advertising") graphregion(color(white))

**95% Confidence Interval on Lagged Coefficients** 
gen lci=_b[adv]-_se[adv]*invttail(34,.05/2) if lag==0
replace lci=_b[L1.adv]-_se[L1.adv]*invttail(34,.05/2) if lag==1
replace lci=_b[L2.adv]-_se[L2.adv]*invttail(34,.05/2) if lag==2
replace lci=_b[L3.adv]-_se[L3.adv]*invttail(34,.05/2) if lag==3 
replace lci=_b[L4.adv]-_se[L4.adv]*invttail(34,.05/2) if lag==4
replace lci=_b[L5.adv]-_se[L5.adv]*invttail(34,.05/2) if lag==5 
replace lci=_b[L6.adv]-_se[L6.adv]*invttail(34,.05/2) if lag==6
replace lci=_b[L7.adv]-_se[L7.adv]*invttail(34,.05/2) if lag==7
replace lci=_b[L8.adv]-_se[L8.adv]*invttail(34,.05/2) if lag==8


gen uci=_b[adv]+_se[adv]*invttail(34,.05/2) if lag==0
replace uci=_b[L1.adv]+_se[L1.adv]*invttail(34,.05/2) if lag==1
replace uci=_b[L2.adv]+_se[L2.adv]*invttail(34,.05/2) if lag==2 
replace uci=_b[L3.adv]+_se[L3.adv]*invttail(34,.05/2) if lag==3
replace uci=_b[L4.adv]+_se[L4.adv]*invttail(34,.05/2) if lag==4
replace uci=_b[L5.adv]+_se[L5.adv]*invttail(34,.05/2) if lag==5 
replace uci=_b[L6.adv]+_se[L6.adv]*invttail(34,.05/2) if lag==6
replace uci=_b[L7.adv]+_se[L7.adv]*invttail(34,.05/2) if lag==7
replace uci=_b[L8.adv]+_se[L8.adv]*invttail(34,.05/2) if lag==8
display invttail(34,.05/2)


line lbeta lci uci lag if lag<21, sort lpattern(. dash dash) lcolor(. gray gray) xlab(0(1)20) xtitle("Lag") ytitle("Dollar Change in Sales") yline(0) graphregion(color(white)) legend(label(1 "Lagged Coefficient") label(2 "95% CI") order (1 2))


*Using a loop to generate lagged coefficients*

reg sales L(0/20).adv 

drop lbeta1 lci uci

gen lbeta1=_b[adv] if lag==0 
forvalue i=1/20{
replace lbeta1=_b[L`i'.adv] if lag==`i'
}

scatter lbeta1 lag if lag<21, sort c(l) s(i) ytitle("Coefficient on Lagged Advertising") graphregion(color(white))

*Using a loop to generate the confidence interval*

gen lci1=_b[adv]-_se[adv]*invttail(34,.05/2) if lag==0
forvalue i=1/20{
replace lci1=_b[L`i'.adv]-_se[L`i'.adv]*invttail(34,.05/2) if lag==`i'
}

gen uci1=_b[adv]+_se[adv]*invttail(34,.05/2) if lag==0

forvalue i=1/20{
replace uci1=_b[L`i'.adv]+_se[L`i'.adv]*invttail(34,.05/2) if lag==`i'
}

scatter lbeta lci uci lag if lag<21,sort c(l l l) s(i i i) lpattern(. dash dash) lcolor(. gray gray) xlab(0(1)20) xtitle("Lag") ytitle("Dollar Change in Sales") yline(0) graphregion(color(white)) legend(label(1 "Lagged Coefficient") label(2 "95% CI") order(1 2))

