cd "C:\Users\Zearo\OneDrive - University of Edinburgh\University of Edinburgh\Courses\PhD_Asset Pricing\PhD_1_SEM2_Emprical Asset Pricing_Prof Sergei Sarkissian\Assignment2"

//Suppose we have 372 month 1990jan-2020dec(31years*12months), 30 industry
//get data
use [return.dta],clear 
//calssify industry var.
egen ind_id = group(industry)
//get year & month var.
gen year = year()
gen month = month()

//merging return data with market data
merge m:1 year month using [market.dta]
keep if _merge == 3 
drop _merge 
//merging return data with FFriskfree data
merge m:1 year month using [risk_free.dta]
keep if _merge == 3 
drop _merge 

use merge part A.dta
save ass2.dta,replace 

*1.best test of the CAPM 
//generate excess return
gen exc_ri = ri - rf 
//gen exc_rm = rm - RF
egen ind_id = group(industry)
save ass2_1.dta, replace

keep exc_ri exc_rm year month ind_id industry 

save ass2_1.dta,replace 

use ass2_1.dta,clear 
//generate unique time sequence 1-240 (20yrs)
sort ind_id year month 
by ind_id: gen time = _n

//generate initial β by time series test
gen b1 = .
forvalues i = 1/30{
	forvalues j = 1/312{
		qui reg exc_ri exc_rm if ind_id == `i' /// 
		& time >= `j' & time <= `j'+ 59
		replace b1 = _b[exc_rm] if ind_id == `i' /// 
		& time == `j'+ 60		
	}
}

save ass2_b1.dta, replace

drop if b1 == .
use ass2_b1.dta, clear
//use the initial beta to regress for generating λ0 λm
gen lam_0 = . 
gen lam_1 = . 
gen residual = .
forvalues k = 1/312{
	qui reg exc_ri b1 if time == `k' + 60
	replace lam_0 = _b[_cons] if time == `k' + 60 
	replace lam_1 = _b[b1] if time == `k' + 60
	predict residuals_`k', residuals
	gen residuals_squared_`k' = residuals_`k'^2
	sum residuals_squared_`k' if time == `k' + 60
	replace residual = r(sum) if time == `k' + 60
	drop residuals_`k' residuals_squared_`k'
}
save ass2_PA_residuals.dta, replace

//C2: average pricing error is zero
use ass2_PA_residuals.dta, clear
reg exc_ri b1
predict residuals, residuals
egen total_residual = mean(residuals)
display total_residual
//very small
drop residual residuals total_residual

//C3: Fama-MacBeth t-statistics average premium associated with the market risk is positive
save ass2_l0&m.dta,replace
*average lambda 0&1
use ass2_l0&m.dta, clear

collapse lam_0 lam_1, by(time) 
//generate mean
sum lam_0 lam_1

//generate t

forvalues p = 0/1{
	gen mean_lam`p' = .
	gen std_lam`p' = .
	gen t_lam`p' = .
	sum lam_`p'
	replace mean_lam`p' = r(mean)
	replace std_lam`p' = r(sd)
	replace t_lam`p' = mean_lam`p' / (std_lam`p'/sqrt(312))
}

dis mean_lam0 t_lam0 mean_lam1 t_lam1
save ass2_1_2.dta,replace

*2. Testing the CAPM with industry-specific risk
use ass2_PA_residuals.dta, clear

drop lam_0 lam_1
gen lresidual = log(residual)
//generate λ
gen lam_0 = . 
gen lam_1 = . 
gen lam_2 = .

xtset ind_id time 
forvalues k = 1/312{
	qui reg exc_ri b1 lresidual if time == `k' + 60
	replace lam_0 = _b[_cons] if time == `k' + 60 
	replace lam_1 = _b[b1] if time == `k' + 60
	replace lam_2 = _b[lresidual] if time == `k' + 60
}

save ass2_2_1.dta,replace
*average lambda 
use ass2_2_1.dta,clear
collapse lam_0 lam_1 lam_2, by(time) 

forvalues p = 0/2{
	gen mean_lam`p' = .
	gen std_lam`p' = .
	gen t_lam`p' = .
	sum lam_`p'
	replace mean_lam`p' = r(mean)
	replace std_lam`p' = r(sd)
	replace t_lam`p' = mean_lam`p' / (std_lam`p'/sqrt(312))
}

dis mean_lam0 t_lam0 mean_lam1 t_lam1 mean_lam2 t_lam2

save ass2_2_2.dta,replace

*3. test of the extra CAPM 
use ass2.dta, clear 
gen exc_ri = ret_rw - risk_free 
gen exc_rm = rm - risk_free
keep ind_id year month CAPEI_Median exc_rm exc_ri 

save ass2_3.dta,replace 


sort ind_id year month 
by ind_id: gen time = _n  

//generate λ0 λm λA
gen b1 = .
forvalues i = 1/30{
	forvalues j = 1/312{
		qui reg exc_ri exc_rm if ind_id == `i' /// 
		& time >= `j' & time <= `j'+ 59
		replace b1 = _b[exc_rm] if ind_id == `i' /// 
		& time == `j'+ 60		
	}
}

gen b2 = . 
forvalues i = 1/30{
	forvalues j = 1/312{
		qui reg exc_ri BM if ind_id == `i' /// 
		& time >= `j' & time <= `j'+ 59
		replace b2 = _b[BM] if ind_id == `i' /// 
		& time == `j'+ 60		
	}
}

//generate λ
gen lam_0 = . 
gen lam_1 = . 
gen lam_2 = .

xtset ind_id time 
forvalues k = 1/312{
	qui reg exc_ri b1 b2 if time == `k' + 60
	replace lam_0 = _b[_cons] if time == `k' + 60 
	replace lam_1 = _b[b1] if time == `k' + 60
	replace lam_2 = _b[b2] if time == `k' + 60
}

drop if b1 == .

save ass2_3_1.dta,replace
*average lambda 
use ass2_3_1.dta,clear

collapse lam_0 lam_1 lam_2, by(time) 

forvalues p = 0/2{
	gen mean_lam`p' = .
	gen std_lam`p' = .
	gen t_lam`p' = .
	sum lam_`p'
	replace mean_lam`p' = r(mean)
	replace std_lam`p' = r(sd)
	replace t_lam`p' = mean_lam`p' / (std_lam`p'/sqrt(312))
}

dis mean_lam0 t_lam0 mean_lam1 t_lam1 mean_lam2 t_lam2

save ssa2_3_result.dta, replace











