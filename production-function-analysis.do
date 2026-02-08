
****************************************************
* Project: Production Function Analysis
* Author: Issah Msodoka
* Date: 2025
* Description:
* This script estimates a production function to
* analyze factors affecting agricultural output.
****************************************************

* Set working directory
cd "."

* Start log file
log using "production_function.log", replace

* Load dataset
use "production_data.dta", clear


* Calculating gross margins
* Calculating revenue
	mata 
		mata stata matrix prices = J(15,5,.)
	end
	matrix colnames prices = no_obs mean p25th median p75th
	matrix rownames prices = mzlo mzhy rice ocer pota bean gnut sunf toma onio cabb oveg toba cott sugc
	matrix list prices  

gen output_pricee = .

	foreach crop of numlist 1/15 {
		quietly sum output_pricee if cropid == `crop' , det
		matrix prices[`crop',1] = r(N)
		matrix prices[`crop',2] = round(r(mean))
		matrix prices[`crop',3] = round(r(p25))
		matrix prices[`crop',4] = round(r(p50))
		matrix prices[`crop',5] = round(r(p75))
		/*multiply by median price; could also do mean*/
		replace output_pricee = prices[`crop',2] if cropid == `crop' & output_pricee == 0   
		}

gen revenuee = output_pricee*prodkg
gen npk_costt = npk*qfertilizer
*gen urea_cost = urea*qfertilizer
*gen dap_cost = dap*qfertilizer
*gen can_cost = can*qfertilizer
*gen lab_cost = wage_men*hired_labour
gen lab_costt = wage_men*aes
gen seed_costt = seed_price*seedkg
gen gross_marginn = revenue - npk_cost - seed_cost
gen cost_prodd = npk_cost + seed_cost + lab_cost 

* 1. The profit function: The case of groundnuts
preserve
tab cropid
tab cropid, nolab
keep if cropid==6 & prodkg>0
* 1a. A Cobb-Douglas profit function
gen ln_pi_ = ln(gross_margin)
gen ln_w1_ = ln(wage_men) 
gen ln_w2_ = ln(seed_price)
gen ln_w3_ = ln(npk)
gen ln_z_  = ln(landAcres)
gen ln_c_  = ln(cost_prod)
* OLS version 
reg ln_pi ln_w1 ln_w2 ln_w3 ln_z [pweight = hh_wgt] /* Profit function*/
reg ln_c ln_w1 ln_w2 ln_w3 ln_z [pweight = hh_wgt] /* Cost function */
*Predicted profit
predict ln_pi1,xb
* The labour demand equation x_i = dpi/dw_i = -1*alpha*pi/w_i
gen x_1 = -1*_b[ln_w1]*exp(ln_pi1)/wage_men
* The output supply function q=pi+sum_i wi*x = (1-sumalpha_i)*pi
gen q_ = (1-_b[ln_w1]-_b[ln_w2]-_b[ln_w3])*exp(ln_pi1)
order cropid prodkg output_price revenue npk_cost lab_cost seed_cost gross_margin ln_* x_1 
ci means ln_* x_1 
*1b. A translog profit function
gen ln_w1_ln_w2_ = .5*ln_w1_*ln_w2_
gen ln_w1_ln_w3_ = .5*ln_w1_*ln_w3_
gen ln_w2_ln_w3_ = .5*ln_w1_*ln_w3_
reg ln_pi_ ln_w1_ ln_w2_ ln_w3_ ln_z_ ln_w1_ln_w2_ ln_w1_ln_w3_ ln_w2_ln_w3_ [pweight = hh_wgt] /* Profit function */
reg ln_c_ ln_w1_ ln_w2_ ln_w3_ ln_z_ ln_w1_ln_w2_ ln_w1_ln_w3_ ln_w2_ln_w3_ [pweight = hh_wgt], level(90) /* Cost function*/
restore
log close