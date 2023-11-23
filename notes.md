---
title: "Notebook"
author: "A.Amstutz"
date: "2023-10-16"
output: 
  html_document:
    keep_md: yes
    toc: yes
    toc_float: yes
    code_folding: hide
  pdf_document:
    toc: yes
---

# The Guidance
Richard Riley et al. Individual Participant Data Meta-Analysis: A Handbook for Healthcare Research
https://www.ipdma.co.uk/description 


# General
1. Use F&G also for sens-analysis (sustained discharge)?
2. Re QoL: Wait for more trials first. Find out more about the QoL measure used, besides bari-solidact.
3. ACTT2:
* For Country only Region -> Non-US Site n=148 and US Site n=885: "There were 67 trial sites in 8 countries: the United States (55 sites), Singapore (4), South Korea (2), Mexico (2), Japan (1), Spain (1), the United Kingdom (1), and Denmark (1)."


## Adjustments across all endpoints and across all trials
1. "When including prognostic factors that are recorded on a continuous scale, they should be analysed on their continuous scale, as dichotomisation or categorisation is arbitrary and loses power. Thus, adjustment for continuous prognostic factors should at least assume a linear trend." (p. 103)
2. "Where conditional treatment effects are of interest for meta-analysis and the pre-defined prognostic (adjustment) factors are systematically missing in some trials, then it is sensible to use the subset of factors that are consistently recorded. In particular, for treatment effects defined by odds ratios or hazard ratios, adjusting for different sets of prognostic factors is problematic due to non-collapsibility (Section 5.2.4); it will make the subsequent summary meta-analysis result hard to interpret and impact upon (often increase) the between-trial heterogeneity in treatment effect. Even for collapsible measures (such as mean differences and risk ratios), the weighting of a particular trial in the meta-analysis may be influenced by whether a full or reduced set of prognostic factors is used, and so it is better to be consistent across trials." (p. 106) 
3. "Partially missing prognostic factor values in a trial can be handled (in each trial separately) by using mean imputation or the missing indicator method, which – although rightly criticised for use in other medical research applications – is actually appropriate for randomised trials aiming to estimate a treatment effect. Again, multiple imputation is a possible alternative (Chapter 18), for example to utilise additional prognostic factors and outcomes available in the IPD, or even to consider missing not at random assumptions." (p. 106) 

a) age

b) clinical status 2-5:
* Not all trials enrolled all stages
* Numeric or factor?

* *__Decision 01.11.23__*: Don't assume linearity (numeric), use it as a factor (incl. when centering in one-stage?)
c) COVID comedication: toci, dexa, rdv
* BariSolidact: Only 1 with toci, 8 with rdv, nearly all with dexa
* ACTT2: Noone toci, Noone dexa, all in intervention also had rdv
* Ghazaeian: Noone toci, all rdv, all dexa
* Tofacov: Noone toci, nearly all rdv, nearly all dexa
* Covinib: Noone toci, noone rdv, a few dexa

* *__Decision 01.11.23__*: Drop this adjustment variable. Justification: consistency across trials/models; adjustment would not be by co-medication, but by trial; and collinearity in models. Unadjusted analyses as sens-analysis (if needed).


## Subgroup analyses
1. Respiratory support & comedication & comorbidity: Numeric or factor?
2. CRP: truncated or not?
3. Rare event strategy? See e.g. firth regression in Ghazaeian, respiratory support, all 7 events only in clinstatus_baseline == 3

*__Chapter7__* Using IPD Meta-Analysis to Examine Interactions between Treatment Effect and Participant-level Covariates


## How to deal with rare events and small trials
1. binary outcomes (page 99): "A problem occurs in trials that have no outcome events (or no non-events) in either the treatment group or the control group, as then the treatment effect estimate (e.g. log odds ratio or log risk ratio) and its variance cannot be calculated because the regression model cannot be fitted.32–34 Then, the meta-analyst is faced with either throwing such trials away (which might be viewed as research waste) or adding extra information to their data to allow estimates to be derived. Traditionally, the latter is addressed in a particular trial by using a continuity correction, where a small number is added to each cell of the trial’s two-by-two table (i.e. that which tabulates the number of events and non-events for the treatment and control groups). Traditionally 0.5 is the value added,35 but Sweeting et al. suggest that a ‘treatment arm’ continuity correction is more appropriate,33 which adds 1/(sample size of the opposite treatment group) to the number of event and non-events. In the IPD context, a similar approach is to add two extra participants to each group in a trial if it has zero events in either of the groups; one of the added participants has the event and the other does not have the event in each group. Then, a weighted regression analysis can be performed to analyse the extended IPD, with all participants weighted equally except the four added participants, who are given a weight according to Sweeting correction (i.e. 1/(sample size of the opposite treat- ment group)). However, this approach becomes problematic when adjusting for prognostic factors or extending to non-binary variables. *__For this reason, a more general approach is to adopt Firth regression__*,36 which is a penalisation method that reduces small sample bias for non-linear models such as logistic regression, and resolves problems related to separation. Alternatively, researchers may revert to a one-stage IPD meta-analysis approach (Chapter 6),14,34 and placing random effects on parameters (rather than stratifying parameters by trial) so that estimation of trial-specific terms are avoided."
2. Time-to-event outcomes (page 102): "are few events in some trials adaptions of Firth’s correction are important to reduce small sample bias in the estimated treatment effect.45"
3. Alternative A: *__Add 0.5 correction__* to crosstab, calculate ORs and then inverse variance pooling in second stage
4. Alternative B: 2x2 directly into Mantel-Haenszel across several trials (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5297998/)? "The meta-analysis models described in Sections 5.3.1 and 5.3.2 are known as inverse variance meta-analysis methods. They combine trial estimates of treatment effect (i.e. θi), with weights accounting for the inverse of their estimated variances. "Both the common-effect and random-effects models of (5.10) and (5.14) assume that the treatment effect estimates in each trial are normally distributed and that their variances are know. Though these assumptions are appropriate when trials are reasonably large (in terms of number of participants and, if applicable, the number of events), they are more tenuous when trials are small or outcome events are rare;6,32,33 indeed, trial estimates of treatment effects and their variances may even be biased in such situations. To address this, other two-stage approaches have been proposed that use a different weighting scheme, such as the Mantel-Haenszel and Peto methods.99,100 For time-to-event outcomes, an extension to the Peto method is to calculate log hazard ratio estimates and variances based on the log-rank statistics in each trial, and then apply model (5.11). Simulation results suggest that, compared to the inverse variance method, the Peto method works well when treatment effects are small, event risks are <1%, and when treatment and control group sizes within trials are balanced (i.e. similar numbers allocated to each).6,32 A key advantage is that it does not require continuity corrections in trials with a zero event in either treatment or control groups." (p. 113)
5. Alternative C: *__However, this issue can also be addressed by applying a one-stage IPD meta-analysis model__* (Chapter 6), which is our preference as *__it can be more easily extended to more complex models, such as including prognostic factors and treatment-covariate interactions__*" (p. 113)

* BariSolidact: mort_28: 36 deaths (15 vs 21). 289 total. 12%
* ACTT2: mort_28: 61 deaths (24 vs 37). 1033 total. 6%
* Ghazaeian: mort_28: 7 deaths (3 vs 4). 97 total. 7% (but ae_28: only 1 AE grade 3,4)
* Tofacov: mort_28: 1 death. 116 total. 0.8%
* Covinib: mort_28: 2 deaths (0 vs 2). 110 total. 1.8%


## Missing data
1. For covariates of adjustment, see notes under first subtitle. For missing outcome data: "Some trials may have missing outcome values for some participants, with little or no missing data in covariates included in the analysis. Then, if missing outcomes are considered ‘missing at random’ (that is, that the probability of being missing depends only on the observed data for included covariates such as treatment and prognostic factors), Sullivan et al. recommend generally using complete-case analysis for a single outcome or a likelihood-based approach for multiple outcomes (e.g. linear mixed model for multiple continuous outcomes).78 *__They note that for randomised trials aiming to estimate a treatment effect, “multiple imputation can be inferior to complete-case analysis and likelihood-based approaches, adding in unnecessary simulation error” and that complete-case analysis “is optimal when missing data are restricted to a univariate outcome and variables associated with missingness are included as covariates in the analysis model__*”.78 This agrees with Groenwold et al., who state that for randomised trials “complete-case analysis with covariate adjustment and multiple imputation yield similar estimates in the event of missing outcome data, as long as the same predictors of missingness are included. Hence, complete case analysis with covariate adjustment can and should be used as the analysis of choice more often”.79 The missing at random assumption is more plausible when prognostic factors are included as adjustment covariates in the analysis model (or the imputation model80), which gives further weight to our recommendation to include a pre-defined set of prognostic factors (Section 5.2.4)." (p. 105)
2. "Where multiple imputation is used, it is important to impute separately for each randomised group as this offers greater robustness.78"  (p. 105)

a) BariSolidact:
* vacc: 5 missing -> MICE for subgroup analysis?
* mort_28: 12 missing
b) ACTT2: 
* sympdur & comorbities -> MICE for subgroup analysis?
* mort_28 (& mort_60): 47 missing
* new_mv_28 & new_mvd_28: 26 missing
c) Covinib: 3-5 missings in mort_28/mort_60/new_mv_28/new_mvd_28
d) Ghazaeian: No missing (besides 2 in CRP -> MICE for subgroup analysis?)
e) Tofacov: No missing (besides 1 in CRP -> MICE for subgroup analysis?)

* *__Decision 01.11.23__*: Do not use Complete Case, to keep ITT. Alternative imputation strategy (based on ACTT2 clinicalstatus day 15):
* Participant was randomized but had no clinical score data available for any timepoint (pre- or post-randomization): worse outcome but not death: 5 = ECMO/MV
* Participant was discharged from the hospital not due to death or transfer to hospice/another hospital and was not readmitted again: 1 = out of hospital
* Participant was discharged from the hospital due to transfer to hospice: 6 = death
* Participant was discharged from the hospital due to transfer to another hospital: last observed value carried forward
* Participant was withdrawn from the study while hospitalized (not due to death): last observed value carried forward

*__Chapter18__*


# TWO-STAGE APPROACH
"As the chosen regression model is estimated in each trial separately, a separate estimate of each parameter (i.e. αi , θi , β1i , β2i , ... is obtained for each trial. This process is also known as *__stratifying__* the estimation by trial, and implies that no information is shared across trials" (p. 93)

## Second stage
1. "Usually *__the assumption of a common treatment effect will be inappropriate__*, as the true effect of a treatment is likely to vary across trials. This is known as between-trial heterogeneity in treatment effect, and it occurs when the distribution of any treatment effect modifiers (i.e. methodological, clinical or participant-level characteristics that influence the magnitude of a treatment’s effect) varies across trials included in the IPD meta-analysis. Examples of potential effect modifiers include follow-up length, the dose of the treatment, the type of intervention in the control group (e.g. usual care), trial quality (risk of bias), and participant-level covariates that interact with treatment response. To allow for between-trial heterogeneity in treatment effect, the θi can be made random,89,90 such that *__the true treatment effects are allowed to be different but assumed to be drawn from a particular distribution, such as a normal distribution__*."
2. "Model (5.14) is often loosely termed a random-effects meta-analysis; however, it is more explicit to refer to it as a *__random treatment effects meta-analysis__* (indeed, in subsequent chapters we introduce IPD meta-analysis models that place random effects on multiple parameters, not just the treatment effect)." (p. 108/109)
3. "In this situation, the random weights (equation (5.17)) for each trial will be more similar to each other than the common weights (equation (5.13)), such that larger and smaller trials will be more equally weighted when assuming random rather than common treatment effects. Some readers may view this as controversial, but it is a consequence of allowing for the heterogeneity in treatment effects, and estimating the average of a distribution of true treatment effects, rather than a single common treatment effect. A more equal weighting is of most concern when smaller trials are at a higher risk of bias, as then the distribution of true treatment effects is potentially distorted by the smaller trials. *__Sensitivity analyses excluding trials at high risk of bias are important in this situation (Chapter 9)__*." (p. 109)
4. "*__I2 describes the percentage of variability in treatment effect estimates that is due to between-trial heterogeneity rather than chance.__* The value of I2 should not be used to decide between a common-effect or random-effects meta-analysis. Another common mistake is to interpret I2 as a measure of the (absolute) amount of heterogeneity (i.e. to consider I2 as an estimate of τ2). This is dangerous, as I2 depends on the size of the within-trial variances; for example, if all trials are small and thus s2i values are large, I2 can be small (i.e. close to 0%) even when the magnitude of τ2 is large and important.97 Rather, I2 should be viewed as a measure of the impact of heterogeneity (τ2) on the summary treatment effect estimate, with the impact small if I2 is close to 0% and large if I2 is close to 100%. The heterogeneity of treatment effects can also be quantified by using a 95% prediction interval for the treatment effect in a new trial" (p. 112)
5. "Both the common-effect and random-effects models of (5.10) and (5.14) assume that the treatment effect estimates in each trial are normally distributed and that their variances are know. Though these assumptions are appropriate when trials are reasonably large (in terms of number of participants and, if applicable, the number of events), they are more tenuous when trials are small or outcome events are rare;6,32,33 indeed, trial estimates of treatment effects and their variances may even be biased in such situations. To address this, other two-stage approaches have been proposed that use a different weighting scheme, such as the Mantel-Haenszel and Peto methods.99,100 For time-to-event outcomes, an extension to the Peto method is to calculate log hazard ratio estimates and variances based on the log-rank statistics in each trial, and then apply model (5.11). Simulation results suggest that, compared to the inverse variance method, the Peto method works well when treatment effects are small, event risks are <1%, and when treatment and control group sizes within trials are balanced (i.e. similar numbers allocated to each).6,32 A key advantage is that it does not require continuity corrections in trials with a zero event in either treatment or control groups. *__However, this issue can also be addressed by applying a one-stage IPD meta-analysis model__* (Chapter 6), which is our preference as *__it can be more easily extended to more complex models, such as including prognostic factors and treatment-covariate interactions__*" (p. 113)
6. "Therefore, generally we *__recommend REML for fitting meta-analysis models assuming random treatment effects__*, and this is used when applying model (5.14) in Box 5.4 and Box 5.5. However, it is not perfect; as for other estimation methods, *__it has poor properties when trial sizes are small or the event of interest is rare. In situations where REML does not converge, the Paule Mandel__* approach is a viable alternative.95,109" (p. 113)
7. "For example, following estimation of either models (5.10) or (5.14), a standard (‘Wald-based’) confidence interval for the summary treatment effect can be calculated. However, this approach does not account for the uncertainty in variance estimates, and so is likely to give confidence intervals that are too narrow. To address this, Hartung and Knapp111,112 (and also independently Sidik and Jonkman113) suggest using a modified confidence interval. When τ2 > 0, the HKSJ confidence interval will usually be wider than the standard confidence interval because it is based on the t-distribution, which has larger critical values (tS − 1,1 − α2 ) than the standard normal distribution. *__In summary, we generally recommend taking the HKSJ approach for deriving confidence intervals for the summary treatment effect following estimation of the random-effects meta-analysis model__* (5.14); however, if the standard method for deriving confidence intervals gives a wider interval than the HKSJ approach, then the standard confidence interval should (also) be presented.117" (p. 115)

## Prediction interval
1. "Predictive distributions are potentially the most relevant and complete statistical inferences to be drawn from random-effects meta-analyses.90" (p. 119)
2. Section 5.3.10

## Meta-regressions
1. page 120
* By risk of bias
* By different JAK inhibitors

## Combining IPD Trials with Partially Reconstructed IPD from Non-IPD Trials
1. "For binary outcome data, IPD can be partly reconstructed from the aggregated information
in published two-by-two tables.8 For example, if the number of participants who were dead and
alive for each of the treatment and control groups is known, IPD can be re-created in a binary
data format, where control/treatment group and alive/dead are represented by a series of
zeros and ones. This is illustrated in Box 6.5." (p. 160)
2. Add CTI biopharma trial 
3. Also, see p.127


# ONE-STAGE APPROACH
"__A potential concern, however, is that the second stage [of the two-stage approach] assumes treatment effect estimates in each trial are normally distributed and that their variances are known; this may be problematic when most trials in the meta-analysis are small (in terms of number of participants and/or events), and motivates rather using the one-stage approach described in the next chapter.__"

## Setting the right parameters/assumptions and general comments
1. "One-stage IPD meta-analysis models usually include multiple parameters and these are estimated simultaneously. *__For each parameter (such as the intercept, treatment effect, residual variances) the analyst must specify whether they are common (the same in each trial), stratified (different in each trial) or random__* (different in each trial and assumed drawn from a particular distribution)." (p.127)
2. "A *__stratified trial intercept__* is generally preferred, unless there are computational concerns. The use of random trial intercepts allows information about baseline risk to be shared across trials, which may compromise randomisation within each trial." (p.127)
3. "For binary outcomes, unless most included trials have sparse numbers of events, REML estimation of a pseudo-likelihood is recommended." (p.127)
4. "The one-stage modelling framework can be extended to accommodate non-proportional hazards, trials with different designs and inclusion of aggregate data from non-IPD trials." (p.127)
5. "Researchers must pre-specify and report their one-stage model specification and assumptions" (p.127)
6. "A one-stage IPD meta-analysis utilises a more exact statistical likelihood than a two-stage meta-analysis approach, which is advantageous when included trials have few participants or outcome events." (p.127)
7. "Compared to a two-stage approach to IPD meta-analysis (Chapter 5), *__a major advantage of one-stage models for binary (and multinomial, ordinal or count) outcomes is the ability to handle trials that contain a group with no participants who have the outcome event (or conversely no participants without the outcome event).__*14,147,157,166 This is due to modelling the binomial likelihood directly, and avoids the need for continuity corrections, unlike the two-stage approach. Thus, one-stage models are especially important when trials contain small numbers of participants or events. Including trials with double zeros (i.e. both groups have no participants with the outcome event, or conversely all participants have the outcome event) is not recommended, as they provide no information about the treatment effect unless additional assumptions are made. Where data are sparse, one-stage models for binary outcomes are prone to potential bias in summary odds ratios,98 although any bias should be less than occurs for the two-stage approach, where data are even more sparse due to deriving trial-specific estimates separately in the first stage." (p.127)
8. "The meta-analysis literature traditionally uses the words common and fixed interchangeably; in particular, the phrase ‘fixed-effect meta-analysis’ is usually short-hand for a meta- analysis that assumes the treatment effect is common to all trials. This is unhelpful in the context of a one-stage IPD meta-analysis for various reasons. Firstly, there are multiple parameters within a one-stage model, and so loosely stating that a ‘fixed-effect IPD meta-analysis was used’ does not reveal which parameter (or parameters) the word ‘fixed’ actually refers to. Secondly, the word ‘fixed’ is ambiguous; it could refer to either a common or stratified parameter, even though they imply different model specifications and assumptions. *__Therefore, we recommend that the word ‘fixed’ be avoided in one-stage IPD models__*, and encourage researchers to use common or stratified instead. As the one-stage model produces multiple parameter estimates, is also helpful to replace vague wording such as ‘common-effect meta-analysis’ or ‘random-effects meta-analysis’ *__with more explicit wording about which parameters in the model are being assumed common or random (or stratified)__*. This is especially important when the one-stage model equation is not actually provided." (p. 138)
9. "*__Assuming a parameter is stratified requires a separate value of the parameter to be estimated for each trial_*; it allows the *__true value of each parameter to be trial-specific__*, without making any assumption about the distribution of the parameter values across trials. For example, allowing for a stratified intercept when there are 10 trials is equivalent to specifying 10 intercepts, one for each trial. This may be acceptable for so-called nuisance parameters (i.e. those parameters not of direct interest, such as the trial-specific intercepts, prognostic factor effects, residual variances, etc). However, stratification is unhelpful for those parameters requiring summary inferences, such as the treatment effect, and rather these must be assumed common or random. Homogeneity of treatment effect is a strong assumption, and often will be inappropriate due to unexplained between-trial heterogeneity (Section 5.3.2). To address this, the treatment effect parameter can be made random, such that the true treatment effect in each trial is assumed drawn from a particular distribution, typically a normal distribution." (p. 138)
10. "*__Importantly, even when allowing the treatment effects to be random, the nuisance parameters within the GLMM (i.e. those parameters other than the treatment effect) are still stratified by trial__*. This is needed to account for clustering of participants within trials, and to allow for potential between-trial heterogeneity in baseline risk and prognostic effects. An alternative option is to assume nuisance parameters are random. Section 6.2.4.2 discusses the choice between random and stratified parameters in more detail. *__Many researchers assume nuisance parameters are common (often because this is the default in software packages), but this is not recommended as it may lead to inappropriate conclusions.__*" (p. 138)
11. "The *__advantage of the stratified intercept approach is that it makes no assumptions about the distribution of intercepts across trials__* - *__and mirrors exactly the two-stage approach__*. The advantage of the random intercepts approach is that it requires fewer parameters to be estimated and so may reduce model convergence issues. But usually they give very similar results." (p. 141 & 143)
12. "Therefore, when fitting a one-stage model with random intercepts and random treatment effects, Turner et al. suggest the correlation should be accounted for and a 1/0 treatment variable coding used.157 If the correlation is not estimable, then assuming it is zero and using a +0.5/–0.5 treatment variable coding is sensible." (p. 144)
13. "Our *__default recommendation is to use stratified prognostic effects (i.e. estimate a separate effect of each included prognostic factor for each trial), with trial-specific centering of each prognostic factor to improve ML estimation__* (for the reasons explained in Section 6.2.8.3). However, if outcome data or prognostic factor categories are sparse, the stratification approach may lead to estimation difficulties, and then allowing prognostic factor effects to be random is a sensible alternative." (p. 145)
14."Unfortunately, there is no natural extension from ML to REML estimation for the exact likelihood defined by a GLMM of a binary, ordinal or count outcome, as the model residuals cannot be estimated separately from the main parameters. Thus ML estimation is generally the default estimation choice for GLMMs of non-continuous outcomes, for which downward bias in between-trial variance estimates and low coverage of confidence intervals is a strong concern, especially with a small number of trials in the IPD meta-analysis. To address this issue, Wolfinger and O’Connell suggest using a pseudo-likelihood approximation of the exact likelihood,199 where the outcome response variable is transformed to an approximately linear scale. This allows REML to be used for GLMMs of non-continuous outcomes, but at the expense of an approximate likelihood. This may be an acceptable trade-off in some situations, to improve between-trial variance estimates and confidence interval coverage. For example, when there are, say, 15 or fewer trials in the IPD meta-analysis, and most have reasonable numbers of participants and events, REML estimation of the pseudo-likelihood may be a good approximation and perform much better than ML estimation of the exact likelihood.200 REML also allows the Kenward-Roger or Satterthwaite corrections to the confidence interval. *__However, this REML approach may not be appropriate when data are sparse (i.e. most trials are small and have few or even zero events)__*, as other work indicates the pseudo-likelihood is not accurate in sparse data situations.153 Problems with REML estimation of the pseudo-likelihood are revealed when parameter estimates are unstable. Instability is evident when first-order and second-order linearisation of the likelihood lead to very different parameter estimates, or when different parameterisations that should not affect REML (such as centering of covariates) change parameter estimates importantly. *__In such situations, ML estimation of the exact likelihood is preferred.__* " (p. 148)
15. "*__Trial-specific Centering of Variables to Improve ML Estimation of One-stage__*: Models with a Stratified Intercept. As previously discussed (Section 6.2.4.1), Jackson et al. and Riley et al. show that for one-stage models with a stratified intercept, *__ML estimation is improved when using trial-specific centering of treatment and other included variables.__*181,185 *__Centering disentangles (i.e. makes uncorrelated) the estimation of main parameters of interest from other nuisance parameters, which leads to less downward bias in estimates of variance parameters (Figure 6.2) and thus improves coverage of 95% confidence intervals.__*" (p. 147)


## The main recommendations for one-stage IPD meta-analysis models using GLMMs summarized (Box 6.4)
1. *__Use a random treatment effect.__*
Justification: Typically the included trials are conducted in different settings, populations and time periods. Therefore, some heterogeneity of treatment effect is expected. Heterogeneity might be reduced by inclusion of prognostic factors (Section 6.2.6) or trial-level covariates (6.2.7), but usually unexplained heterogeneity remains and so should be acknowledged.
2. *__Stratify by trial the intercept and parameters for other non-treatment variables (such as prognostic factors and residual variances)__*. If convergence issues arise, then consider making the intercept (and other non-treatment variables) random.
Justification: Although a random intercept will usually give similar results to a stratified inter- cept, in some situations it may compromise randomisation (as it allows baseline risk informa- tion to be shared across trials). If a stratified intercept model fails to converge (e.g. with rare events or many parameters), assuming random intercepts is a sensible compromise, in which case accounting for the correlation between the random effects on intercept and treatment effect may be important. Another option is to entirely condition out the trial intercepts (Section 6.2.4.2).
3. *__Use trial-specific centering of the treatment variable (and any other included variables, such as prognostic factors) when using ML estimation of a one-stage model with a stratified intercept.__* Justification: Simulation studies and mathematical reasoning show that this improves ML estimation of between-trial variances and the coverage of confidence intervals for the summary treatment effect (Section 6.2.8.3).
4. For frequentist estimation of one-stage models for binary, ordinal or count outcomes, use REML estimation of the pseudo-likelihood approach unless most trials in the meta-analysis are small (in terms of participants or outcome events), which then warrants ML estimation of the exact likelihood.
Justification: Although estimation of the exact likelihood is preferred, ML estimation is known to produce downwardly biased estimates of between-trial variances. Therefore, unless most included trials are small, REML estimation of an approximate pseudo-likelihood specification may improve estimation of between-trial variances and coverage of confidence intervals (Section 6.2.8.4). When either REML or ML estimation is used, coverage is improved using confidence intervals based on the t-distribution (Section 6.2.8.5). A Bayesian approach is an appealing alternative.

Also, check out: Adapted from Simmonds MC, Higgins JP. A general framework for the use of logistic regression models in meta-analysis. Stat Methods Med Res 2016;25(6):2858–77.

And, for time to event outcomes: https://eprints.keele.ac.uk/id/eprint/7367/1/Manuscript%20-%20Individual%20participant%20data%20meta-analysis%20of%20intervention%20studies%20with%20time-to-event%20outcomes%20A%20review%20of%20the%20methodology%20and%20an%20applied%20example.pdf 



