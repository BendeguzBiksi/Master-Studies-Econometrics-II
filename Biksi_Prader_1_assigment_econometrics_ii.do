* BGSE - Competition and Market Regulation, winter 2016
* Quantitative and Statistics Methods II.
* Bendegúz István Biksi, Benjamin Prader

* First assignment

clear all

use "Biksi_Prader_1_assigment_econometrics_ii_rail.dta"

set more off

*4.

* Summarizing the variables

summarize PRICE QUANTITY LAKES COLLUSION, detail

*5.

* Generating the logarithm of price and quantity

generate LOGPRICE=log(PRICE)

generate LOGQUANTITY=log(QUANTITY)

* generating month dummies

tab (MONTH), gen(month)

* OLS for the demand

regress LOGQUANTITY LOGPRICE LAKES month*

outreg2 using 5_demand_2, tex

* OLS for the supply

regress LOGPRICE LOGQUANTITY DM1 DM2 DM3 DM4 COLLUSION month*

outreg2 using 5_supply_2, tex

*6.

** manual 2sls for the demand

* first stage regression

reg LOGPRICE DM1 DM2 DM3 DM4 LAKES month*

outreg2 using 6_demand_2, tex

* predict prices

predict P1

* second stage regression

reg LOGQUANTITY P1 LAKES month*

** manual 2sls for the supply

* first stage regression

reg LOGQUANTITY DM1 DM2 DM3 DM4 COLLUSION LAKES month*

outreg2 using 6_supply_3, tex 

* predict quantities

predict Q1

* second stage regression

reg LOGPRICE Q1 DM1 DM2 DM3 DM4 COLLUSION


** built in 2sls

* Demand

ivregress 2sls LOGQUANTITY LAKES month* (LOGPRICE=DM1 DM2 DM3 DM4), first

outreg2 using 6_demand_iv_2, tex

* Supply

ivregress 2sls LOGPRICE DM1 DM2 DM3 DM4 COLLUSION month* (LOGQUANTITY=LAKES), first

outreg2 using 6_supply_iv_2, tex
