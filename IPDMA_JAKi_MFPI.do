clear
import excel "/Users/amstutzal/Documents/GitHub/JAKi-IPDMA/df_tot.xlsx", sheet("Sheet1") firstrow

ssc install mfpi
* get metacurve_i via help function
* help metacurve_i

* recode trial variable
replace trial= "8" if trial == "TACTIC-R" 
replace trial= "7" if trial == "RECOVERY" 
replace trial= "6" if trial == "COV-BARRIER" 
replace trial= "5" if trial == "COVINIB" 
replace trial= "4" if trial == "TOFACOV" 
replace trial= "3" if trial == "Ghazaeian" 
replace trial= "2" if trial == "ACTT2" 
replace trial= "1" if trial == "Bari-Solidact" 

destring trial, replace
destring clinstatus_baseline, replace

* Death is 1
* destring mort_28, replace
* trt must be 0, 1; intervention == 1

* ventilated at baseline is 1
* gen vbaseline_b = vbaseline == "Ventilation at baseline"


* truncate all to be investigated cont. covariates at 1 and 99 centile (except days symptoms..)
centile age, centile(1 5 95 99)
gen age_trunc = age
replace age_trunc = 90 if age_trunc > 90 & age_trunc != .
replace age_trunc = 24 if age_trunc < 24 & age_trunc != .

centile sympdur, centile(1 5 95 99)
gen sympdur_trunc = sympdur
replace sympdur_trunc = 29 if sympdur_trunc > 29 & sympdur_trunc != .
replace sympdur_trunc = 1 if sympdur_trunc < 1 & sympdur_trunc != .

centile crp, centile(1 5 95 99)
gen crp_trunc = crp
replace crp_trunc = 367.92 if crp_trunc > 367.92 & crp_trunc != .
replace crp_trunc = 3 if crp_trunc < 3 & crp_trunc != .


******************************************************************************************
* MFPI for age, outcome mortality day 28 (binary)

* 5th and 95th percentile // Take out Spinner since for those we only have age as a categorical variable, does not converge otherwise
centile age, centile(1 5 95 99)

* truncated
mfpi, with(trt) linear(age_trunc) flex(2) select(0.05) gendiff(age_truncdiff): logistic mort_28 age clinstatus_baseline
mfpi_plot age_trunc

mfpi, with(trt) fp1(age_trunc) flex(2) select(0.05) gendiff(age_truncdiff): logistic mort_28 age clinstatus_baseline
mfpi_plot age_trunc

mfpi, with(trt) fp2(age_trunc) fp1(age_trunc) linear(age_trunc) flex(2) select(0.05) gendiff(age_truncdiff): logistic mort_28 age clinstatus_baseline
mfpi_plot age_trunc

mfpi, with(trt) fp2(age_trunc) fp1(age_trunc) linear(age_trunc) flex(2) select(0.05) gendiff(age_truncdiff): logistic mort_28 age clinstatus_baseline
metacurve_i, by(trial) with(trt) fixpowers(3) generate(age_truncmean) function(test_age_trunc) genwt(wtage_trunc_m28) adjust(clinstatus_baseline) random: logistic mort_28 age_trunc

* transform to OR
gen age_truncmean_or = exp(age_truncmean)
gen age_truncmean_ll_or = exp(age_truncmean_ll)
gen age_truncmean_ul_or = exp(age_truncmean_ul)

*** replace age_truncmean_ul_or=. if age_truncmean_ul_or > 5
line age_truncmean_or age_truncmean_ll_or age_truncmean_ul_or age_trunc, legend(order(1 "Average Effect" 2 "95% CI Limits")) xtitle("Age at baseline") ///
	ytitle("Treatment effect (Odds ratio, log scale)", size(medium)) title("Outcome Mortality at Day 28", size(large)) ///
	graphregion(color(white)) plotregion(fcolor(white)) ///
	yscale(log titlegap(1.75) range(0.3 1.5)) ylabel(0.3 0.5 0.75 1.0 1.5) ///
	///yscale(log titlegap(1.75) range(0.25 6)) ylabel(0.25 0.5 1.0 2.0 4.0 6.0) ///
	yline(1, lwidth(vthin) lpattern(solid) lcolor(black)) ///
	xlabel(30 40 50 60 70 80 90) ///
	xline(32, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) /// add the 5 percentile
	xline(82, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) /// add the 95 percentile
	lpatter("__" "-.-" "-.-") lcolor(black black black) lwidth(medthick thin thin) sort
graph export age_trunc_m28_or.pdf, replace


******************************************************************************************
* MFPI for Days from symptom onset until randomization, outcome mortality day 28 (binary)
* Add age70 as adjustment variable

centile sympdur_trunc, centile(1 5 95 99)

* truncated
mfpi, with(trt) fp2(sympdur_trunc) linear(sympdur_trunc) select(0.05) flex(3) gendiff(sympdur_truncdiff): logistic mort_28 sympdur clinstatus_baseline
metacurve_i, by(trial) fixpowers(1) with(trt) generate(sympdur_truncmean) function(test_sympdur_trunc) genwt(wtsympdur_trunc_m28) adjust(clinstatus_baseline) random: logistic mort_28 sympdur_trunc

* transform to OR
gen sympdur_truncmean_or = exp(sympdur_truncmean)
gen sympdur_truncmean_ll_or = exp(sympdur_truncmean_ll)
gen sympdur_truncmean_ul_or = exp(sympdur_truncmean_ul)

line sympdur_truncmean_or sympdur_truncmean_ll_or sympdur_truncmean_ul_or sympdur_trunc, legend(order(1 "Average Effect" 2 "95% CI Limits")) xtitle("Days with symptoms pre-randomization") ///
	ytitle("Treatment effect (Odds ratio, log scale)") title("Outcome Mortality at Day 28") ///
	graphregion(color(white)) plotregion(fcolor(white)) ///
	yscale(log titlegap(1.75) range(0.3 1.5)) ylabel(0.3 0.5 0.75 1.0 1.5) ///
	///yscale(log titlegap(1.75) range(0.5 1.5)) ylabel(0.5 0.70 1.0 1.5) ///
	yline(1, lwidth(vthin) lpattern(solid) lcolor(black)) ///
	xlabel(0 3 6 9 12 15 18 21 24 27 30) ///
	xline(3, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	xline(17, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	lpatter("__" "-.-" "-.-") lcolor(black black black) lwidth(medthick thin thin) sort 
graph export sympdur_trunc_m28_or.pdf, replace

******************************************************************************************
* MPFI for crp, outcome mortality day 28 (binary)

centile crp, centile(1 5 95 99)

* truncated
mfpi, with(trt) fp2(crp_trunc) linear(crp_trunc) select(0.05) flex(3) gendiff(crp_truncdiff): logistic mort_28 crp clinstatus_baseline
metacurve_i, by(trial) fixpowers(1) with(trt) generate(crp_truncmean) function(test_crp_trunc) genwt(wtcrp_trunc_m28) adjust(clinstatus_baseline) random: logistic mort_28 crp_trunc

* transform to OR
gen crp_truncmean_or = exp(crp_truncmean)
gen crp_truncmean_ll_or = exp(crp_truncmean_ll)
gen crp_truncmean_ul_or = exp(crp_truncmean_ul)

line crp_truncmean_or crp_truncmean_ll_or crp_truncmean_ul_or crp_trunc, legend(order(1 "Average Effect" 2 "95% CI Limits")) xtitle("C-reactive protein level in mg/L") ///
	ytitle("Treatment effect (Odds ratio, log scale)") title("Outcome Mortality at Day 28") ///
	graphregion(color(white)) plotregion(fcolor(white)) ///
	yscale(log titlegap(1.75) range(0.3 1.5)) ylabel(0.3 0.5 0.75 1.0 1.5) ///
	///yscale(log titlegap(1.75) range(0.25 6)) ylabel(0.25 0.5 1.0 2.0 4.0 6.0) ///
	yline(1, lwidth(vthin) lpattern(solid) lcolor(black)) ///
	xlabel(0 50 100 150 200 250 300 350) ///
	xline(10, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	xline(266, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	lpatter("__" "-.-" "-.-") lcolor(black black black) lwidth(medthick thin thin) sort 
graph export crp_trunc_m28_or.pdf, replace












****************************************
******************** OLD 
****************************************





******************************************************************************************
* MPFI for Date of enrolment (days since Jan 1, 2020, in increments of 30), outcome mortality day 28 (binary)
* take out enrollmentperiod as adjustment variable

centile months_enroll_trunc, centile(1 5 95 99)

* truncated, in months
mfpi logistic mortality28, with(trt) fp1(months_enroll_trunc) select(0.05) flex(3) gendiff(months_enroll_truncdiff)
metacurve_i, by(trial) fixpowers(1) with(trt) generate(months_enroll_truncmean) function(test_months_enroll_trunc) genwt(wtmonths_enroll_trunc_m28) adjust(vbaseline_b age70_b) random: logistic mortality28 months_enroll_trunc

* transform to OR
gen months_enroll_truncmean_or = exp(months_enroll_truncmean)
gen months_enroll_truncmean_ll_or = exp(months_enroll_truncmean_ll)
gen months_enroll_truncmean_ul_or = exp(months_enroll_truncmean_ul)

line months_enroll_truncmean_or months_enroll_truncmean_ll_or months_enroll_truncmean_ul_or months_enroll_trunc, legend(order(1 "Average Effect" 2 "95% CI Limits")) xtitle("Month of enrollment since 01.01.2020") ///
	ytitle("Treatment effect (Odds ratio, log scale)") title("Outcome Mortality at Day 28") ///
	graphregion(color(white)) plotregion(fcolor(white)) ///
	yscale(log titlegap(1.75) range(0.25 6)) ylabel(0.25 0.5 1.0 2.0 4.0 6.0) ///
	yline(1, lwidth(vthin) lpattern(solid) lcolor(black)) ///
	xlabel(0 2 4 6 8 10 12 14) ///
	xline(1.5, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	xline(14, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	lpatter("__" "-.-" "-.-") lcolor(black black black) lwidth(medthick thin thin) sort 
graph export months_enroll_trunc_m28_or.pdf, replace


******************************************************************************************
* MFPI for age, outcome Days until discharge within 28 days (time-to-event): dischargedimp / dischargedcens

* truncate days until discharge to max. 28 days
gen dischargedimp_trunc = dischargedimp
replace dischargedimp_trunc = 28 if dischargedimp_trunc > 28 & dischargedimp_trunc != .

* main model (without random intercept)
stset dischargedimp_trunc, failure(dischargedcens==1)
stcox trt vbaseline_b enrollmentperiod_b

* truncated
mfpi stcox, with(trt) fp1(age_trunc) select(0.05) flex(3) gendiff(age_truncdiff_dtd)
metacurve_i, by(trial) fixpowers(1) with(trt) generate(age_truncmean_dtd) function(test_age_trunc_dtd) genwt(wtage_trunc_dtd) adjust(vbaseline_b enrollmentperiod_b) random: stcox age_trunc

* transform to HR
gen age_truncmean_dtd_hr = exp(age_truncmean_dtd)
gen age_truncmean_dtd_ll_hr = exp(age_truncmean_dtd_ll)
gen age_truncmean_dtd_ul_hr = exp(age_truncmean_dtd_ul)

line age_truncmean_dtd_hr age_truncmean_dtd_ll_hr age_truncmean_dtd_ul_hr age_trunc, legend(order(1 "Average Effect" 2 "95% CI Limits")) xtitle("Age at baseline") ///
	ytitle("Treatment effect (Hazard ratio, log scale)") title("Outcome Days until Hospital Discharge") ///
	graphregion(color(white)) plotregion(fcolor(white)) ///
	yscale(log titlegap(1.75) range(0.25 6)) ylabel(0.25 0.5 1.0 2.0 4.0 6.0) ///
	xlabel(30 40 50 60 70 80 90) ///
	yline(1, lwidth(vthin) lpattern(solid) lcolor(black)) ///
	xline(31, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	xline(82, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	lpatter("__" "-.-" "-.-") lcolor(black black black) lwidth(medthick thin thin) sort 
graph export age_trunc_dtd_hr.pdf, replace

******************************************************************************************
* MFPI for days since symptom onset, outcome Days until discharge within 28 days (time-to-event)
* Add age70 as adjustment variable

* main model (without random intercept)
stset dischargedimp_trunc, failure(dischargedcens==1)
stcox trt vbaseline_b enrollmentperiod_b age70_b

*truncated
mfpi stcox, with(trt) fp1(dayssymptoms_trunc) select(0.05) flex(3) gendiff(dayssymptoms_truncdiff_dtd)
metacurve_i, by(trial) fixpowers(1) with(trt) generate(dayssymptoms_truncmean_dtd) function(test_dayssymptoms_trunc_dtd) genwt(wtdayssymptoms_trunc_dtd) adjust(vbaseline_b enrollmentperiod_b age70_b) random: stcox dayssymptoms_trunc

* transform to HR
gen dayssymptoms_truncmean_dtd_hr = exp(dayssymptoms_truncmean_dtd)
gen dayssymptoms_truncmean_dtd_ll_hr = exp(dayssymptoms_truncmean_dtd_ll)
gen dayssymptoms_truncmean_dtd_ul_hr = exp(dayssymptoms_truncmean_dtd_ul)

line dayssymptoms_truncmean_dtd_hr dayssymptoms_truncmean_dtd_ll_hr dayssymptoms_truncmean_dtd_ul_hr dayssymptoms_trunc, legend(order(1 "Average Effect" 2 "95% CI Limits")) xtitle("Number of days with symptoms pre-randomization") ///
	ytitle("Treatment effect (Hazard ratio, log scale)") title("Outcome Days until Hospital Discharge") ///
	graphregion(color(white)) plotregion(fcolor(white)) ///
	yscale(log titlegap(1.75) range(0.25 6)) ylabel(0.25 0.5 1.0 2.0 4.0 6.0) ///
	yline(1, lwidth(vthin) lpattern(solid) lcolor(black)) ///
	xlabel(0 2 4 6 8 10 12 14 16 18 20) ///
	xline(3, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	xline(18, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	lpatter("__" "-.-" "-.-") lcolor(black black black) lwidth(medthick thin thin) sort 
graph export dayssymptoms_trunc_dtd_hr.pdf, replace

******************************************************************************************
* MFPI for age, outcome Days until cessation of oxygen within 28 days among those on oxy at baseline (time-to-event): oxcessationdimp / oxcessationdcens

* truncate days until cessation to max. 28 days
gen oxcessationdimp_trunc = oxcessationdimp
replace oxcessationdimp_trunc = 28 if oxcessationdimp_trunc > 28 & oxcessationdimp_trunc != .
* drop all not on oxy at baseline
drop if oxcessationdcens == .

* main model (without random intercept)
stset oxcessationdimp_trunc, failure(oxcessationdcens==1)
stcox trt vbaseline_b enrollmentperiod_b

* truncated
mfpi stcox, with(trt) fp1(age_trunc) select(0.05) flex(3) gendiff(age_truncdiff_dtc)
metacurve_i, by(trial) fixpowers(1) with(trt) generate(age_truncmean_dtc) function(test_age_trunc_dtc) genwt(wtage_trunc_dtc) adjust(vbaseline_b enrollmentperiod_b) random: stcox age_trunc

* transform to HR
gen age_truncmean_dtc_hr = exp(age_truncmean_dtc)
gen age_truncmean_dtc_ll_hr = exp(age_truncmean_dtc_ll)
gen age_truncmean_dtc_ul_hr = exp(age_truncmean_dtc_ul)

line age_truncmean_dtc_hr age_truncmean_dtc_ll_hr age_truncmean_dtc_ul_hr age_trunc, legend(order(1 "Average Effect" 2 "95% CI Limits")) xtitle("Age at baseline") ///
	ytitle("Treatment effect (Hazard ratio, log scale)") title("Outcome Days until Cessation of Oxygen") ///
	graphregion(color(white)) plotregion(fcolor(white)) ///
	yscale(log titlegap(1.75) range(0.25 6)) ylabel(0.25 0.5 1.0 2.0 4.0 6.0) ///
	yline(1, lwidth(vthin) lpattern(solid) lcolor(black)) ///
	xlabel(30 40 50 60 70 80 90) ///
	xline(36, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	xline(86, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	lpatter("__" "-.-" "-.-") lcolor(black black black) lwidth(medthick thin thin) sort 
graph export age_trunc_dtc_hr.pdf, replace

******************************************************************************************
* MFPI for days since symptom onset, outcome Days until cessation of oxygen within 28 days among those on oxy at baseline (time-to-event): oxcessationdimp / oxcessationdcens
* Add age70 as adjustment variable

* main model (without random intercept)
stset oxcessationdimp_trunc, failure(oxcessationdcens==1)
stcox trt vbaseline_b enrollmentperiod_b

*truncated
mfpi stcox, with(trt) fp1(dayssymptoms_trunc) select(0.05) flex(3) gendiff(dayssymptoms_truncdiff_dtc)
metacurve_i, by(trial) fixpowers(1) with(trt) generate(dayssymptoms_truncmean_dtc) function(test_dayssymptoms_trunc_dtc) genwt(wtdayssymptoms_trunc_dtc) adjust(vbaseline_b enrollmentperiod_b age70_b) random: stcox dayssymptoms_trunc

* transform to HR
gen dayssymptoms_truncmean_dtc_hr = exp(dayssymptoms_truncmean_dtc)
gen dayssymptoms_truncmean_dtc_ll_hr = exp(dayssymptoms_truncmean_dtc_ll)
gen dayssymptoms_truncmean_dtc_ul_hr = exp(dayssymptoms_truncmean_dtc_ul)

line dayssymptoms_truncmean_dtc_hr dayssymptoms_truncmean_dtc_ll_hr dayssymptoms_truncmean_dtc_ul_hr dayssymptoms_trunc, legend(order(1 "Average Effect" 2 "95% CI Limits")) xtitle("Number of days with symptoms pre-randomization") ///
	ytitle("Treatment effect (Hazard ratio, log scale)") title("Outcome Days until Cessation of Oxygen") ///
	graphregion(color(white)) plotregion(fcolor(white)) ///
	yscale(log titlegap(1.75) range(0.25 6)) ylabel(0.25 0.5 1.0 2.0 4.0 6.0) ///
	yline(1, lwidth(vthin) lpattern(solid) lcolor(black)) ///
	xlabel(0 2 4 6 8 10 12 14 16 18 20) ///
	xline(3, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	xline(18, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	lpatter("__" "-.-" "-.-") lcolor(black black black) lwidth(medthick thin thin) sort 
graph export dayssymptoms_trunc_dtc_hr.pdf, replace

******************************************************************************************
* MFPI for age, outcome mortality 60 days (binary)

* truncated
mfpi logistic mortality60, with(trt) fp1(age_trunc) select(0.05) flex(3) gendiff(age_truncdiff_m60)
metacurve_i, by(trial) fixpowers(1) with(trt) generate(age_truncmean_m60) function(test_age_trunc_m60) genwt(wtage_trunc_m60) adjust(vbaseline_b enrollmentperiod_b) random: logistic mortality60 age_trunc

* transform to OR
gen age_truncmean_m60_or = exp(age_truncmean_m60)
gen age_truncmean_m60_ll_or = exp(age_truncmean_m60_ll)
gen age_truncmean_m60_ul_or = exp(age_truncmean_m60_ul)

line age_truncmean_m60_or age_truncmean_m60_ll_or age_truncmean_m60_ul_or age_trunc, legend(order(1 "Average Effect" 2 "95% CI Limits")) xtitle("Age at baseline") ///
	ytitle("Treatment effect (Odds ratio, log scale)", size(medium)) title("Outcome Mortality at Day 60", size(large)) ///
	graphregion(color(white)) plotregion(fcolor(white)) ///
	yscale(log titlegap(1.75) range(0.25 6)) ylabel(0.25 0.5 1.0 2.0 4.0 6.0) ///
	yline(1, lwidth(vthin) lpattern(solid) lcolor(black)) ///
	xlabel(30 40 50 60 70 80 90) ///
	xline(31, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	xline(82, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	lpatter("__" "-.-" "-.-") lcolor(black black black) lwidth(medthick thin thin) sort
graph export age_trunc_m60_or.pdf, replace

******************************************************************************************
* MFPI for days since symptom onset, outcome mortality 60 days (binary)

* truncated
mfpi logistic mortality60, with(trt) fp1(dayssymptoms_trunc) select(0.05) flex(3) gendiff(dayssymptoms_truncdiff_m60)
metacurve_i, by(trial) fixpowers(1) with(trt) generate(dayssymptoms_truncmean_m60) function(test_dayssymptoms_trunc_m60) genwt(wtdayssymptoms_trunc_m60) adjust(vbaseline_b enrollmentperiod_b age70_b) random: logistic mortality60 dayssymptoms_trunc

* transform to OR
gen dayssymptoms_truncmean_m60_or = exp(dayssymptoms_truncmean_m60)
gen dayssymptoms_truncmean_m60_ll_or = exp(dayssymptoms_truncmean_m60_ll)
gen dayssymptoms_truncmean_m60_ul_or = exp(dayssymptoms_truncmean_m60_ul)

line dayssymptoms_truncmean_m60_or dayssymptoms_truncmean_m60_ll_or dayssymptoms_truncmean_m60_ul_or dayssymptoms_trunc, legend(order(1 "Average Effect" 2 "95% CI Limits")) xtitle("Number of days with symptoms pre-randomization") ///
	ytitle("Treatment effect (Odds ratio, log scale)") title("Outcome Mortality at Day 60") ///
	graphregion(color(white)) plotregion(fcolor(white)) ///
	yscale(log titlegap(1.75) range(0.25 6)) ylabel(0.25 0.5 1.0 2.0 4.0 6.0) ///
	yline(1, lwidth(vthin) lpattern(solid) lcolor(black)) ///
	xlabel(0 2 4 6 8 10 12 14 16 18 20) ///
	xline(3, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	xline(18, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	lpatter("__" "-.-" "-.-") lcolor(black black black) lwidth(medthick thin thin) sort 
graph export dayssymptoms_trunc_m60_or.pdf, replace


******************************************************************************************
* MFPI for age, outcome new mechanical ventilation/ECMO/death within 28 days (binary)

* truncated
mfpi logistic newmvdeath28imp, with(trt) fp1(age_trunc) select(0.05) flex(3) gendiff(age_truncdiff_nmvd)
metacurve_i, by(trial) fixpowers(1) with(trt) generate(age_truncmean_nmvd) function(test_age_trunc_nmvd) genwt(wtage_trunc_nmvd) adjust(vbaseline_b enrollmentperiod_b) random: logistic newmvdeath28imp age_trunc

* transform to OR
gen age_truncmean_nmvd_or = exp(age_truncmean_nmvd)
gen age_truncmean_nmvd_ll_or = exp(age_truncmean_nmvd_ll)
gen age_truncmean_nmvd_ul_or = exp(age_truncmean_nmvd_ul)

line age_truncmean_nmvd_or age_truncmean_nmvd_ll_or age_truncmean_nmvd_ul_or age_trunc, legend(order(1 "Average Effect" 2 "95% CI Limits")) xtitle("Age at baseline") ///
	ytitle("Treatment effect (Odds ratio, log scale)", size(medium)) title("Outcome New Mechanical Ventilation or Death", size(large)) ///
	graphregion(color(white)) plotregion(fcolor(white)) ///
	yscale(log titlegap(1.75) range(0.25 6)) ylabel(0.25 0.5 1.0 2.0 4.0 6.0) ///
	yline(1, lwidth(vthin) lpattern(solid) lcolor(black)) ///
	xlabel(30 40 50 60 70 80 90) ///
	xline(31, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	xline(82, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	lpatter("__" "-.-" "-.-") lcolor(black black black) lwidth(medthick thin thin) sort
graph export age_trunc_nmvd_or.pdf, replace

******************************************************************************************
* MFPI for days since symptom onset, outcome new mechanical ventilation/ECMO/death within 28 days (binary)

* truncated
mfpi logistic newmvdeath28imp, with(trt) fp1(dayssymptoms_trunc) select(0.05) flex(3) gendiff(dayssymptoms_truncdiff_nmvd)
metacurve_i, by(trial) fixpowers(1) with(trt) generate(dayssymptoms_truncmean_nmvd) function(test_dayssymptoms_trunc_nmvd) genwt(wtdayssymptoms_trunc_nmvd) adjust(vbaseline_b enrollmentperiod_b age70_b) random: logistic newmvdeath28imp dayssymptoms_trunc

* transform to OR
gen dayssymptoms_truncmean_nmvd_or = exp(dayssymptoms_truncmean_nmvd)
gen ds_truncmean_nmvd_ll_or = exp(dayssymptoms_truncmean_nmvd_ll)
gen ds_truncmean_nmvd_ul_or = exp(dayssymptoms_truncmean_nmvd_ul)

line dayssymptoms_truncmean_nmvd_or ds_truncmean_nmvd_ll_or ds_truncmean_nmvd_ul_or dayssymptoms_trunc, legend(order(1 "Average Effect" 2 "95% CI Limits")) xtitle("Number of days with symptoms pre-randomization") ///
	ytitle("Treatment effect (Odds ratio, log scale)") title("Outcome New Mechanical Ventilation or Death", size(large)) ///
	graphregion(color(white)) plotregion(fcolor(white)) ///
	yscale(log titlegap(1.75) range(0.25 6)) ylabel(0.25 0.5 1.0 2.0 4.0 6.0) ///
	yline(1, lwidth(vthin) lpattern(solid) lcolor(black)) ///
	xlabel(0 2 4 6 8 10 12 14 16 18 20) ///
	xline(3, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	xline(18, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	lpatter("__" "-.-" "-.-") lcolor(black black black) lwidth(medthick thin thin) sort 
graph export dayssymptoms_trunc_nmvd_or.pdf, replace

******************************************************************************************
* MFPI for age, outcome mechanical ventilator free days within 28 days (count)

* truncated
mfpi poisson nomvdaysimp, with(trt) fp1(age_trunc) select(0.05) flex(3) gendiff(age_truncdiff_nomvfd)
metacurve_i, by(trial) fixpowers(1) with(trt) generate(age_truncmean_nomvfd) function(test_age_trunc_nomvfd) genwt(wtage_trunc_nomvfd) adjust(vbaseline_b enrollmentperiod_b) random: poisson nomvdaysimp age_trunc

* transform to OR
gen age_truncmean_nomvfd_irr = exp(age_truncmean_nomvfd)
gen age_truncmean_nomvfd_ll_irr = exp(age_truncmean_nomvfd_ll)
gen age_truncmean_nomvfd_ul_irr = exp(age_truncmean_nomvfd_ul)

line age_truncmean_nomvfd_irr age_truncmean_nomvfd_ll_irr age_truncmean_nomvfd_ul_irr age_trunc, legend(order(1 "Average Effect" 2 "95% CI Limits")) xtitle("Age at baseline") ///
	ytitle("Treatment effect (Incidence rate ratio, log scale)", size(medium)) title("Outcome Mechanical Ventilation-free Days", size(large)) ///
	graphregion(color(white)) plotregion(fcolor(white)) ///
	yscale(log titlegap(1.75) range(0.25 4)) ylabel(0.25 0.5 1.0 2.0 4.0 6.0) ///
	yline(1, lwidth(vthin) lpattern(solid) lcolor(black)) ///
	xlabel(30 40 50 60 70 80 90) ///
	xline(36, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	xline(86, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	lpatter("__" "-.-" "-.-") lcolor(black black black) lwidth(medthick thin thin) sort
graph export age_trunc_nomvfd_irr.pdf, replace

******************************************************************************************
* MFPI for days since symptom onset, outcome mechanical ventilator free days within 28 days (count)

poisson nomvdaysimp trt vbaseline_b enrollmentperiod_b age70_b, irr

* truncated
mfpi poisson nomvdaysimp, with(trt) fp1(dayssymptoms_trunc) select(0.05) flex(3) gendiff(ds_truncdiff_nomvfd)
metacurve_i, by(trial) fixpowers(1) with(trt) generate(dayssymptoms_truncmean_nomvfd) function(test_ds_trunc_nomvfd) genwt(wtds_trunc_nomvfd) adjust(vbaseline_b enrollmentperiod_b age70_b) random: poisson nomvdaysimp dayssymptoms_trunc

* transform to OR
gen ds_truncmean_nomvfd_or = exp(dayssymptoms_truncmean_nomvfd)
gen ds_truncmean_nomvfd_ll_or = exp(dayssymptoms_truncmean_nomvfd_ll)
gen ds_truncmean_nomvfd_ul_or = exp(dayssymptoms_truncmean_nomvfd_ul)

line ds_truncmean_nomvfd_or ds_truncmean_nomvfd_ll_or ds_truncmean_nomvfd_ul_or dayssymptoms_trunc, legend(order(1 "Average Effect" 2 "95% CI Limits")) xtitle("Number of days with symptoms pre-randomization") ///
	ytitle("Treatment effect (Incidence rate ratio, log scale)") title("Outcome Mechanical Ventilation-free Days", size(large)) ///
	graphregion(color(white)) plotregion(fcolor(white)) ///
	yscale(log titlegap(1.75) range(0.25 4)) ylabel(0.25 0.5 1.0 2.0 4.0 6.0) ///
	yline(1, lwidth(vthin) lpattern(solid) lcolor(black)) ///
	xlabel(0 2 4 6 8 10 12 14 16 18 20) ///
	xline(3, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	xline(18, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	lpatter("__" "-.-" "-.-") lcolor(black black black) lwidth(medthick thin thin) sort 
graph export dayssymptoms_trunc_nomvfd_irr.pdf, replace

******************************************************************************************
* MFPI for age, outcome clinical score at 28 days (ordinal score)
* take out vbaseline_b

ologit clinstatus28imp trt enrollmentperiod_b age70_b, or

* truncated
mfpi ologit clinstatus28imp, with(trt) fp1(age_trunc) select(0.05) flex(3) gendiff(age_truncdiff_c28)
metacurve_i, by(trial) fixpowers(1) with(trt) generate(age_truncmean_c28) function(test_age_trunc_c28) genwt(wtage_trunc_c28) adjust(vbaseline_b enrollmentperiod_b) random: ologit clinstatus28imp age_trunc

* transform to OR
gen age_truncmean_c28_or = exp(age_truncmean_c28)
gen age_truncmean_c28_ll_or = exp(age_truncmean_c28_ll)
gen age_truncmean_c28_ul_or = exp(age_truncmean_c28_ul)

line age_truncmean_c28_or age_truncmean_c28_ll_or age_truncmean_c28_ul_or age_trunc, legend(order(1 "Average Effect" 2 "95% CI Limits")) xtitle("Age at baseline") ///
	ytitle("Treatment effect (Odds ratio, log scale)", size(medium)) title("Outcome Clinical Status at Day 28", size(large)) ///
	graphregion(color(white)) plotregion(fcolor(white)) ///
	yscale(log titlegap(1.75) range(0.25 4)) ylabel(0.25 0.5 1.0 2.0 4.0 6.0) ///
	yline(1, lwidth(vthin) lpattern(solid) lcolor(black)) ///
	xlabel(30 40 50 60 70 80 90) ///
	xline(36, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	xline(86, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	lpatter("__" "-.-" "-.-") lcolor(black black black) lwidth(medthick thin thin) sort
graph export age_trunc_c28_or.pdf, replace

******************************************************************************************
* MFPI for days since symptom onset, outcome clinical score at 28 days (ordinal score)

* truncated
mfpi ologit clinstatus28imp, with(trt) fp1(dayssymptoms_trunc) select(0.05) flex(3) gendiff(ds_truncdiff_c28)
metacurve_i, by(trial) fixpowers(1) with(trt) generate(dayssymptoms_truncmean_c28) function(test_ds_trunc_c28) genwt(wtds_trunc_c28) adjust(vbaseline_b enrollmentperiod_b age70_b) random: ologit clinstatus28imp dayssymptoms_trunc

* transform to OR
gen ds_truncmean_c28_or = exp(dayssymptoms_truncmean_c28)
gen ds_truncmean_c28_ll_or = exp(dayssymptoms_truncmean_c28_ll)
gen ds_truncmean_c28_ul_or = exp(dayssymptoms_truncmean_c28_ul)

line ds_truncmean_c28_or ds_truncmean_c28_ll_or ds_truncmean_c28_ul_or dayssymptoms_trunc, legend(order(1 "Average Effect" 2 "95% CI Limits")) xtitle("Number of days with symptoms pre-randomization") ///
	ytitle("Treatment effect (Incidence rate ratio, log scale)") title("Outcome Clinical Status at Day 28", size(large)) ///
	graphregion(color(white)) plotregion(fcolor(white)) ///
	yscale(log titlegap(1.75) range(0.25 4)) ylabel(0.25 0.5 1.0 2.0 4.0 6.0) ///
	yline(1, lwidth(vthin) lpattern(solid) lcolor(black)) ///
	xlabel(0 2 4 6 8 10 12 14 16 18 20) ///
	xline(3, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	xline(18, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	lpatter("__" "-.-" "-.-") lcolor(black black black) lwidth(medthick thin thin) sort 
graph export dayssymptoms_trunc_c28_or.pdf, replace

