clear
import excel "/Users/amstutzal/Documents/GitHub/JAKi-IPDMA/df_tot.xlsx", sheet("Sheet1") firstrow

ssc install mfpi
* get metacurve_i via help function
* help metacurve_i

* recode trial variable // add new trials, if more are added to the dataset!
replace trial= "9" if trial == "PANCOVID"
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


* truncate all to be investigated cont. covariates at 1 and 99 centile
centile age, centile(1 5 95 99)
gen age_trunc = age
replace age_trunc = 89 if age_trunc > 89 & age_trunc != .
replace age_trunc = 24 if age_trunc < 24 & age_trunc != .

centile sympdur, centile(1 5 95 99)
gen sympdur_trunc = sympdur
replace sympdur_trunc = 29 if sympdur_trunc > 29 & sympdur_trunc != .
replace sympdur_trunc = 1 if sympdur_trunc < 1 & sympdur_trunc != .

centile crp, centile(1 5 95 99)
gen crp_trunc = crp
replace crp_trunc = 367.62 if crp_trunc > 367.62 & crp_trunc != .
replace crp_trunc = 3 if crp_trunc < 3 & crp_trunc != .


******************************************************************************************
* MFPI for age, outcome mortality day 28 (binary)

* 5th and 95th percentile
centile age, centile(1 5 95 99)

* truncated

mfpi, with(trt) fp1(age_trunc) flex(3) select(0.05) gendiff(age_truncdiff): logistic mort_28 age clinstatus_baseline
// fp1(age_trunc): Instead of assuming a linear effect, this command specifies using a first-degree fractional polynomial to model age_trunc. This allows for more flexibility than a linear model and can capture non-linear relationships between age and the outcome.
// flex(2): This specifies that the model can use two degrees of freedom for flexibility when selecting the best-fitting fractional polynomial model.
// select(0.05): This indicates that the significance level for model selection is 0.05, which is a standard threshold for determining statistical significance.
// gendiff(age_truncdiff): This option generates a variable representing the difference in effects across levels of age_trunc.
// : logistic mort_28 age clinstatus_baseline: This part specifies that the logistic regression model predicts mortality at day 28 (mort_28), adjusting for baseline clinical status (clinstatus_baseline).

metacurve_i, by(trial) with(trt) fixpowers(3) generate(age_truncmean) function(test_age_trunc) genwt(wtage_trunc_m28) adjust(clinstatus_baseline) random: logistic mort_28 age_trunc /// meta-analysis of interaction effects, stratified by trial (separate analyses within each trial); 
// fixpowers(3): Implies a cubic transformation, allowing for more complex curves that could model relationships with multiple inflection points (i.e., the effect of age could increase and decrease over the range of age values)
// generate(age_truncmean): Creates a variable age_truncmean to store the average value of age_trunc
// function(test_age_trunc): Specifies the function used to evaluate the effect of age_trunc
// genwt(wtage_trunc_m28): Generates weights for the analysis, reflecting sample sizes or variance within trials
// adjust(clinstatus_baseline): Adjusts the model for the baseline clinical status
// random: logistic mort_28 age_trunc: Specifies that a random-effects logistic regression model should be used for analyzing the interaction effect across trials.

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
mfpi, with(trt) fp1(sympdur_trunc) flex(3) select(0.05) gendiff(sympdur_truncdiff): logistic mort_28 sympdur clinstatus_baseline age
metacurve_i, by(trial) fixpowers(3) with(trt) generate(sympdur_truncmean) function(test_sympdur_trunc) genwt(wtsympdur_trunc_m28) adjust(clinstatus_baseline age) random: logistic mort_28 sympdur_trunc

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
mfpi, with(trt) fp1(crp_trunc) flex(3) select(0.05) gendiff(crp_truncdiff): logistic mort_28 crp clinstatus_baseline age
metacurve_i, by(trial) fixpowers(3) with(trt) generate(crp_truncmean) function(test_crp_trunc) genwt(wtcrp_trunc_m28) adjust(clinstatus_baseline age) random: logistic mort_28 crp_trunc

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
	xline(265, lwidth(thin) lpattern(longdash_dot) lcolor(gs10)) ///
	lpatter("__" "-.-" "-.-") lcolor(black black black) lwidth(medthick thin thin) sort 
graph export crp_trunc_m28_or.pdf, replace
