***Barcelona Graduate School of Economics
***Quantitative and Statistics Methods II.
***Quantitative Analysis of Market and Firm Conduct
***************************************************
***** Bendeguz Istvan Biksi & Adnan Arif *****
***************************************************


use "Biksi_Adnan_3_assigment_econometrics_ii.dta"

*set the data set as time series
tsset time, m

*plot the cement price index against time with vertical lines at key periods
tsline price_cement, tline(2001m11 2002m2 2002m7 2004m9)

*produce a table with cross correlations between the diferent input price idices and the price index of cement
*in order to get a first peg of the intensity of the relationship between inputs and the output
corr price_cement inpprice_limestone inpprice_heatingoil inpprice_electricity inpprice_browncoal inpprice_blackcoal

*plot the cement price index together with the brown coal, electricity and limestone price indices
line price_cement inpprice_browncoal inpprice_electricity inpprice_limestone time

*generate quarterly dummy variables
forvalues i=1/4 {
gen Q`i'=0
}
*
replace Q1=1 if month<=3
replace Q2=1 if 4==month | month==5 | month==6
replace Q3=1 if 7==month | month==8 | month==9
replace Q4=1 if 10<=month

*generate the trend and the cyclical component of the quantity of construction variable
tsfilter hp quanconseascyc = quant_construction, trend(quanconseastrend)

*generate wage cost per employee
gen wagecostperem=totalwages_cement/employees_cement




*generate the logarithm of the variables
gen logprice_cement=log(price_cement)

gen loginpprice_limestone=log(inpprice_limestone)

gen loginpprice_electricity=log(inpprice_electricity)

gen loginpprice_blackcoal=log(inpprice_blackcoal)

gen loginpprice_browncoal=log(inpprice_browncoal)

gen loginpprice_ppi=log(inpprice_ppi)

gen logemployees_cement=log(employees_cement)

gen logtotalwages_cement=log(totalwages_cement)

gen logwagecostperem=log(wagecostperem)

gen logprod_cement=log(prod_cement)

gen logprod_readymixconcrete=log(prod_readymixconcrete)

gen logvalue_construction=log(value_construction)

gen logprod_concreteparts=log(prod_concreteparts)

**generate cartel dummy variables

gen cartel91to97=0

gen cartel98to01=0

replace cartel91to97=1 if year>=1991 & year<=1997

replace cartel98to01=1 if year>=1998 & year<=2001

replace cartel98to01=0 if year==2001 & month==12

*generate an overall cartel dummy
gen cartel=0
replace cartel=1 if year>=1991  & year<=2001
replace cartel=0 if year==2001 & month==12

*generate a price war dummy
gen pricewar=0

replace pricewar=1 if year==2003

*generate a post-merger dummy
gen pmerger=0
replace pmerger=1 if year>=2004 & month>=9 | year>=2005



***LIN-LIN


*2sls with two cartel dummies without black coal without inpprice_ppi
ivregress 2sls price_cement inpprice_limestone inpprice_electricity inpprice_browncoal  wagecostperem cartel91to97 cartel98to01 time (prod_cement=value_construction prod_concreteparts), first



***LOG-LOG


*2sls with the log-log specification with two cartel dummies without black coal without loginpprice_ppi
ivregress 2sls logprice_cement loginpprice_limestone loginpprice_electricity loginpprice_browncoal  logwagecostperem cartel91to97 cartel98to01 time (logprod_cement=logvalue_construction logprod_concreteparts), first



***Merger Dummy

*Post-merger 2sls lin lin without black coal and inpprice ppi
ivregress 2sls price_cement inpprice_limestone inpprice_electricity inpprice_browncoal wagecostperem cartel91to97 cartel98to01 pmerger time (prod_cement=value_construction prod_concreteparts), first

*Merger Log-log without black coal and inpprice ppi
ivregress 2sls logprice_cement loginpprice_limestone loginpprice_electricity loginpprice_browncoal logwagecostperem pmerger cartel91to97 cartel98to01 time (logprod_cement=logvalue_construction logprod_concreteparts), first



***Using the first LAG of the different indenpendent variables (the input price indices)

*2sls lin-lin
ivregress 2sls price_cement inpprice_limestone inpprice_electricity inpprice_browncoal  wagecostperem L.inpprice_limestone L.inpprice_electricity L.inpprice_browncoal  L.wagecostperem cartel91to97 cartel98to01 time (prod_cement=value_construction prod_concreteparts), first

*using the second and the third lags of brown coal (because the last is significant)
ivregress 2sls price_cement inpprice_limestone inpprice_electricity inpprice_browncoal  wagecostperem L.inpprice_limestone L.inpprice_electricity L.inpprice_browncoal L2.inpprice_browncoal L3.inpprice_browncoal  L.wagecostperem cartel91to97 cartel98to01 time (prod_cement=value_construction prod_concreteparts), first


*2sls log-log
ivregress 2sls logprice_cement loginpprice_limestone loginpprice_electricity loginpprice_browncoal  logwagecostperem L.loginpprice_limestone L.loginpprice_electricity L.loginpprice_browncoal  L.logwagecostperem cartel91to97 cartel98to01 time (logprod_cement=logvalue_construction logprod_concreteparts), first


*using the second and the third lags of brown coal (because the last is significant)
ivregress 2sls logprice_cement loginpprice_limestone loginpprice_electricity loginpprice_browncoal  logwagecostperem L.loginpprice_limestone L.loginpprice_electricity L.loginpprice_browncoal L2.loginpprice_browncoal L3.loginpprice_browncoal  L.logwagecostperem cartel91to97 cartel98to01 time (logprod_cement=logvalue_construction logprod_concreteparts), first
