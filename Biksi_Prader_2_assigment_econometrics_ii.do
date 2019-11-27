***Barcelona Graduate School of Economics
***Quantitative and Statistics Methods II.
***Assignment on Merger Simulation
***Quantitative Analysis of Market and Firm Conduct
***************************************************
***** Bendeguz Istvan Biksi & Benjamin Prader *****
*********************02/2016***********************
clear all

set more off

use "Biksi_Prader_2_assigment_econometrics_ii.dta"


* keep data only for Germany

keep if country3==1


* generate the pontential number of consumers

gen L=pop/4


* generate a firm identification variable

egen firmid=group(firm)


* make a table of firms and segments

tab firm segment


* generate the total quantity sold in a given year

forvalues i = 1970/1999 {

 
  egen Q`i'=sum(qu) if year==`i'
  
 }
*

* sum up across rows

egen SUMTOTALQ=rowtotal(Q*)


* drop the matrix

drop Q*


* generate market share per type for every year

gen mssharetype=qu/L


* generate the outside good i.e. the difference b/w the potential number of consumers and the actual sales in a given year

gen q0=L-SUMTOTALQ


* generate the market share of the outside good

gen s0=q0/L


* define the dataset as a panel one

xtset co year


* define the dependent variable i.e. Sjt/S0t and then take the log of it

gen mshareratios=mssharetype/s0

gen logmshareratios=log(mshareratios)


*5.
* run pooled OLS with price without princ, with princ without price and with both

regress logmshareratios pr horsepower fuel width height domestic

est store first


regress logmshareratios princ horsepower fuel width height domestic

est store second


regress logmshareratios pr princ horsepower fuel width height domestic

est store third


* for latex output

outreg2 [first second third] using Ols3.tex, replace

* generate average horsepower per firm (across the years)

forvalues i=1/42 {
egen avghppwrfirm`i'=mean(horsepower) if firm==`i'
}
*

* sum up across rows

egen firmavghppwr=rowtotal(avghppwrfirm*)


* drop the matrix

drop avghppwrfirm*


** generate ivs, i.e. the average of the horsepower of other firms than the retailer of car j

* take the average of the horsepower of firms that is not the car in question

forvalues i=1/28 {

egen aiv`i'=mean(firmavghppwr) if firmid!=`i'

egen iv`i'=mean(aiv`i')

drop aiv`i'
}
*
* and then put it in to the right row

forvalues i=1/28 {

egen realiv`i'=mean(iv`i') if firmid==`i'

drop iv`i'
}
*

* sum up across rows

egen Instrument=rowtotal(realiv*)


* drop the matrix

drop realiv*


* generate segment dummies

tab segment, gen(segment)



*6.


* First: run the fixed-effects model, then save it.

xtivreg logmshareratios horsepower fuel width height domestic year (pr=Instrument), fe first


* Save the estimates

estimates store fixed


* Run random-effects estimation.

xtivreg logmshareratios horsepower fuel width height segment1 segment2 segment3 segment5 domestic year (pr=Instrument), re first


* Finally do the Hausman test.

hausman fixed

* Since the p-value is below the .05 treshold the problem of endogeneity occurs, namely the unobserved time-invariant error term is likely to be correlated with explanatory variable(s).
* Hence we use fixed-effects with the horsepower instrumental variable obtained from above.

xtivreg logmshareratios horsepower fuel width height domestic year (pr=Instrument), fe first


* For checking the result of the 2SLS we run

ivregress 2sls logmshareratios horsepower fuel width height segment1 segment2 segment3 segment5 domestic year (pr=Instrument), first

* Note: the price coefficient is -.0001096 and significant in 1% significance level. 





*7.
* obtaining the price coefficient (from 5) is -.0002813, therefore create a variable for it

gen alpha= -.0002813 


* Calculate the own price elascticities for every type/year we define it as follows:

gen logitownpriceelasticity=alpha*pr*(1-mssharetype)


* Calculate the cross price elasticities

gen logitcrossprice=-alpha*mssharetype*pr




*** Two-level nested logit***


* generate segment codes

egen segmentcode=group(segment)


** define the share of product j within it's subnest
* generate the total quantity in a given year (t) in a given segment (s) given that wether the car is domestic (d) or not i.ei in the given subnest:

forvalues t=1970/1999 {
forvalues s=1/5 {
forvalues d=0/1 {

egen summaqu`t'`s'`d'=sum(qu) if year==`t' & segmentcode==`s' & domestic==`d'
}
}
}
*

* sum up across rows

egen Summaqu=rowtotal(summaqu*)


* drop the matrix

drop summaqu*


* generate the share of car j within it's subnest (domestic/foreign) in the given year in the given segment(nest/group) and in the given subnest(domestic/foreign)

gen sharetypewtinsubnest=qu/Summaqu


** define the share of of the subnest within it's nest in the given year
* generate the total quantity sold in the nest/segment(s) in the given year(t):

forvalues t=1970/1999 {
forvalues s=1/5 {

egen summaaqu`t'`s'=sum(qu) if year==`t' & segmentcode==`s'
}
}
*

* sum up across rows

egen SUMMAnest=rowtotal(summaaqu*)


* drop the matrix

drop summaaqu*


* generate the share of a subnest within it's nest in a given year

gen sharesbnestwtinnest=Summaqu/SUMMAnest


* generate the logarithm of car j's share within it's subnest and generate the logarithm of the share of a subnest within it's nest

gen logsharetypewtinsubnest=log(sharetypewtinsubnest)

gen logsharesbnestwtinnest=log(sharesbnestwtinnest)


*10.
* Run a pooled OLS on the logarithm of the market share ratios of car j and the outside good on characteristics price car j's share within it's subnest and on the share of a subnest within it's nest

reg logmshareratios pr year domestic segment1 segment2 segment3 segment5 horsepower fuel width height logsharetypewtinsubnest logsharesbnestwtinnest


***Extra exercise: run a 2sls model with instrument of the average horsepower of other firms' then the producer of car j

ivregress 2sls logmshareratios year domestic segment1 segment2 segment3 segment5 horsepower fuel width height logsharetypewtinsubnest logsharesbnestwtinnest (pr=Instrument)


* obtaining from the OLS regression the price coefficient (alpha), sigma_h and sigma_g we recover the own price elasticity

gen alphanested=-.0000155


* generate sigma_h that is the coefficient of the logarithm of car j's share within it's subnest

gen sigma_h=.9601487


* generate sigma_g that is the coefficient of the logarithm of car j's subnest's share within car j's 

gen sigma_g=.9441263


* generate car j's share within it's nest

gen sharequinnest=qu/SUMMAnest


* generate a variable that is common in elasticity expressions

gen commonterm=(sigma_g/(1-sigma_g))*sharequinnest+mssharetype


* generate the own price elasticities according to the slides (Chapter_3(III.))

gen nestedownelast=-(alphanested*pr*(-(1/(1-sigma_h))+((1/(1-sigma_h))-(1/(1-sigma_g)))*sharetypewtinsubnest+commonterm))


* Final note: In terms of the cross price elasticites we used excel because the calculations are more tractable there so if you are interested we can send you the excel files.

