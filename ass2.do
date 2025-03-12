cd M:

use return.dta,clear 

encode FFI30_desc, gen(industry)

egen ind_id = group(industry)

drop FFI30_desc

gen year = year(public_date)
gen month = month(public_date)
drop public_date

rename indret_vw ret_rw 

merge m:1 year month using [market.dta]
keep if _merge == 3 
drop _merge 

merge m:1 year month using [risk_free.dta]
keep if _merge == 3 
drop _merge 

save ass2.dta,replace 

*1.best test of the CAPM 
gen exc_ri = ret_rw - risk_free 
gen exc_rm = rm - risk_free

keep exc_ri exc_rm year month ind_id industry 

save ass2_1.dta,replace 

use ass2_1.dta,clear 
sort ind_id year month 
by ind_id: gen time = _n


gen b1 = .
forvalues i = 1/30{
	forvalues j = 1/192{
		qui reg exc_ri exc_rm if ind_id == `i' /// 
		& time >= `j' & time <= `j'+ 59
		replace b1 = _b[exc_rm] if ind_id == `i' /// 
		& time == `j'+ 60		
	}
}

gen lam_0 = . 
gen lam_1 = . 

forvalues k = 1/192{
	qui reg exc_ri b1 if time == `k' + 60
	replace lam_0 = _b[_cons] if time == `k' + 60 
	replace lam_1 = _b[b1] if time == `k' + 60
}

save ass2_1_1.dta,replace
*average lambda 
use ass2_1_1.dta,clear
drop if lam_1 == . 
collapse lam_0 lam_1, by(time) 

forvalues p = 0/1{
	gen mean_lam`p' = .
	gen std_lam`p' = .
	gen t_lam`p' = .
	sum lam_`p'
	replace mean_lam`p' = r(mean)
	replace std_lam`p' = r(sd)
	replace t_lam`p' = mean_lam`p' / (std_lam`p'/sqrt(192))
}

dis mean_lam0 t_lam0 mean_lam1 t_lam1


*2. Testing the CAPM with industry-specific risk
use ass2_1.dta,clear 

sort ind_id year month 
by ind_id: gen time = _n  

gen b1 = .
forvalues i = 1/30{
	forvalues j = 1/192{
		qui reg exc_ri exc_rm if ind_id == `i' /// 
		& time >= `j' & time <= `j'+ 59
		replace b1 = _b[exc_rm] if ind_id == `i' /// 
		& time == `j'+ 60		
	}
}

gen lam_0 = . 
gen lam_1 = . 
gen lam_2 = . 

forvalues k = 1/192{
	qui reg exc_ri b1 if time == `k' + 60
	replace lam_0 = _b[_cons] if time == `k' + 60 
	replace lam_1 = _b[b1] if time == `k' + 60
}




*3. test of the extra CAPM 
use ass2.dta, clear 
gen exc_ri = ret_rw - risk_free 
gen exc_rm = rm - risk_free
keep ind_id year month CAPEI_Median exc_rm exc_ri 

save ass2_3.dta,replace 

sort ind_id year month 
by ind_id: gen time = _n  

gen b1 = .
gen b2 = . 
forvalues i = 1/30{
	forvalues j = 1/192{
		qui reg exc_ri exc_rm if ind_id == `i' /// 
		& time >= `j' & time <= `j'+ 59
		replace b1 = _b[exc_rm] if ind_id == `i' /// 
		& time == `j'+ 60		
	}
}


gen lam_0 = . 
gen lam_1 = . 
gen lam_2 = .
 
xtset ind_id time 
forvalues k = 1/192{
	qui reg exc_ri b1 l.CAPEI_Median if time == `k' + 60
	replace lam_0 = _b[_cons] if time == `k' + 60 
	replace lam_1 = _b[b1] if time == `k' + 60
	replace lam_2 = _b[l.CAPEI_Median] if time == `k' + 60
}

save ass2_3_1.dta,replace
*average lambda 
use ass2_3_1.dta,clear
drop if lam_2 == . 
collapse lam_0 lam_1 lam_2, by(time) 

forvalues p = 0/2{
	gen mean_lam`p' = .
	gen std_lam`p' = .
	gen t_lam`p' = .
	sum lam_`p'
	replace mean_lam`p' = r(mean)
	replace std_lam`p' = r(sd)
	replace t_lam`p' = mean_lam`p' / (std_lam`p'/sqrt(192))
}

dis mean_lam0 t_lam0 mean_lam1 t_lam1 mean_lam2 t_lam2













