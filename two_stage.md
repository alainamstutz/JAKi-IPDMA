---
title: "two_stage"
author: "A.Amstutz"
date: "2023-10-15"
output:
  html_document:
    keep_md: yes
    toc: yes
    toc_float: yes
    code_folding: hide
  pdf_document:
    toc: yes
---

# Load packages

```r
library(tidyverse)
library(readxl)
library(writexl)
library(tableone)
library(here)
library(kableExtra)

library(jtools) # for summ() and plot_summs
library(sjPlot) # for tab_model
library(ggplot2) # survival/TTE analyses and other graphs
library(ggfortify) # autoplot
library(meta)
library(forestplot)
```

# Load entire dataset (from one-stage.Rmd) for descriptive purpose - make sure to run one-stage before

```r
df_tot <- readRDS("df_tot.RData") # without Murugesan
df_tot_Muru <- readRDS("df_tot_Muru.RData") # with Murugesan
```

# Load treatment effect estimates from all trials

```r
df_barisolidact <- readRDS("trt_effects_barisolidact.RData")
df_actt2 <- readRDS("trt_effects_actt2.RData")
df_ghazaeian <- readRDS("trt_effects_ghazaeian.RData")
df_tofacov <- readRDS("trt_effects_tofacov.RData")
df_covinib <- readRDS("trt_effects_covinib.RData")
df_covbarrier <- readRDS("trt_effects_cov-barrier.RData")
# df_murugesan <- readRDS("trt_effects_murugesan.RData")
```

# Reshape dataframes for all treatment effect estimates

```r
### Create a list of all data frames / trials
list_df <- list(df_barisolidact, df_actt2, df_ghazaeian, df_tofacov, df_covinib, df_covbarrier) # add all trials

## Mortality at day 28
outcomes <- "death at day 28"
outcomes.firth <- "death at day 28_firth" # depends on which estimates to include
# Initialize an empty data frame to store the selected rows
df_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_mort28 <- rbind(df_mort28, selected_rows)
}

## Mortality at day 60
outcomes <- "death at day 60"
outcomes.firth <- "death at day 60_firth"
df_mort60 <- data.frame()
for (df in list_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_mort60 <- rbind(df_mort60, selected_rows)
}

## Death within fup
outcomes <- "death within fup"
outcomes.firth <- "death within fup_firth"
df_ttdeath <- data.frame()
for (df in list_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_ttdeath <- rbind(df_ttdeath, selected_rows)
}

## new MV within 28d
outcomes <- "new MV within 28d"
outcomes.firth <- "new MV within 28d_firth"
df_new_mv28 <- data.frame()
for (df in list_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_new_mv28 <- rbind(df_new_mv28, selected_rows)
}

## new MV or death within 28d
outcomes <- "new MV or death within 28d"
outcomes.firth <- "new MV or death within 28d_firth"
df_new_mvd28 <- data.frame()
for (df in list_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_new_mvd28 <- rbind(df_new_mvd28, selected_rows)
}

## new MV or death within 28d
outcomes <- "new MV or death within 28d"
outcomes.firth <- "new MV or death within 28d_firth"
df_new_mvd28 <- data.frame()
for (df in list_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_new_mvd28 <- rbind(df_new_mvd28, selected_rows)
}

## clinical status at day 28
outcomes <- "clinical status at day 28"
outcomes.firth <- "clinical status at day 28_firth"
df_clin28 <- data.frame()
for (df in list_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_clin28 <- rbind(df_clin28, selected_rows)
}

## discharge within 28 days
outcomes <- "discharge within 28 days"
outcomes.firth <- "discharge within 28 days_firth"
df_ttdischarge <- data.frame()
for (df in list_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_ttdischarge <- rbind(df_ttdischarge, selected_rows)
}

## discharge within 28 days, death=comp.event
outcomes <- "discharge within 28 days, death=comp.event"
outcomes.firth <- "discharge within 28 days, death=comp.event_firth"
df_ttdischarge_comp <- data.frame()
for (df in list_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_ttdischarge_comp <- rbind(df_ttdischarge_comp, selected_rows)
}

## discharge within 28 days, death=hypo.event
outcomes <- "discharge within 28 days, death=hypo.event"
outcomes.firth <- "discharge within 28 days, death=hypo.event_firth"
df_ttdischarge_hypo <- data.frame()
for (df in list_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_ttdischarge_hypo <- rbind(df_ttdischarge_hypo, selected_rows)
}

## sustained discharge within 28 days
outcomes <- "sustained discharge within 28 days"
outcomes.firth <- "sustained discharge within 28 days_firth"
df_ttdischarge_sus <- data.frame()
for (df in list_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_ttdischarge_sus <- rbind(df_ttdischarge_sus, selected_rows)
}

## viral clearance until day 5
outcomes <- "viral clearance until day 5"
outcomes.firth <- "viral clearance until day 5_firth"
df_vir_clear_5 <- data.frame()
for (df in list_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_vir_clear_5 <- rbind(df_vir_clear_5, selected_rows)
}

## viral clearance until day 10
outcomes <- "viral clearance until day 10"
outcomes.firth <- "viral clearance until day 10_firth"
df_vir_clear_10 <- data.frame()
for (df in list_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_vir_clear_10 <- rbind(df_vir_clear_10, selected_rows)
}

## viral clearance until day 15
outcomes <- "viral clearance until day 15"
outcomes.firth <- "viral clearance until day 15_firth"
df_vir_clear_15 <- data.frame()
for (df in list_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_vir_clear_15 <- rbind(df_vir_clear_15, selected_rows)
}

## Any AE grade 3,4 within 28 days
outcomes <- "Any AE grade 3,4 within 28 days"
outcomes2 <- "any AE grade 3,4 within 28 days"
outcomes.firth <- "Any AE grade 3,4 within 28 days_firth"
outcomes.firth2 <- "any AE grade 3,4 within 28 days_firth"
df_ae28 <- data.frame()
for (df in list_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes2 | variable == outcomes.firth | variable == outcomes.firth2)
  df_ae28 <- rbind(df_ae28, selected_rows)
}

## AEs grade 3,4 within 28 days
outcomes <- "AEs grade 3,4 within 28 days"
outcomes.firth <- "AEs grade 3,4 within 28 days_firth"
df_ae28sev <- data.frame()
for (df in list_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_ae28sev <- rbind(df_ae28sev, selected_rows)
}
```

# (i) Primary outcome: Mortality at day 28

```r
str(df_mort28)
```

```
## 'data.frame':	6 obs. of  10 variables:
##  $ variable         : chr  "death at day 28" "death at day 28" "death at day 28" "death at day 28_firth" ...
##  $ hazard_odds_ratio: num  0.64 0.737 0.791 2.537 0.182 ...
##  $ ci_lower         : num  0.29477 0.41582 0.14728 0.12715 0.00131 ...
##  $ ci_upper         : num  1.36 1.29 3.83 380.13 2.29 ...
##  $ standard_error   : num  0.387 0.287 0.798 1.315 1.358 ...
##  $ p_value          : num  0.25 0.287 0.769 0.552 0.203 ...
##  $ n_intervention   : num  145 515 46 58 55 815
##  $ n_control        : num  144 518 51 58 55 811
##  $ trial            : chr  "Bari-SolidAct" "ACTT-2" "Ghazaeian" "TOFACOV" ...
##  $ JAKi             : chr  "Baricitinib" "Baricitinib" "Tofacitinib" "Tofacitinib" ...
```

```r
mort28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_mort28,
                      n.e = n_intervention + n_control,
                      # n.c = n_control,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = F,
                      method.tau = "REML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "se", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Average treatment effect - mortality 28 days",
                      # subset = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # exclude entirely
                      # exclude = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # include in forestplot but exclude from analysis
                      )
summary(mort28)
```

```
## Review:     Average treatment effect - mortality 28 days
## 
##                   OR            95%-CI %W(random)
## Bari-SolidAct 0.6404 [0.2998;  1.3677]       11.9
## ACTT-2        0.7368 [0.4197;  1.2933]       21.6
## Ghazaeian     0.7909 [0.1654;  3.7807]        2.8
## TOFACOV       2.5366 [0.1928; 33.3748]        1.0
## COVINIB       0.1822 [0.0127;  2.6082]        1.0
## COV-BARRIER   0.5122 [0.3674;  0.7141]       61.8
## 
## Number of studies: k = 6
## Number of observations: o = 3271
## 
##                                  OR           95%-CI     t p-value
## Random effects model (HK-SE) 0.5795 [0.4114; 0.8164] -4.09  0.0094
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 2.5985]; tau = 0 [0.0000; 1.6120]
##  I^2 = 0.0% [0.0%; 74.6%]; H = 1.00 [1.00; 1.99]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  3.43    5  0.6333
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 5)
```

```r
forest.meta(mort28,
            # hetstat = T,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE", "n.e"),
            leftlabs = c("Trial", "log(OR)", "Standard Error", "Sample Size"),
            text.random = "Average treatment effect (random effects model)",
            title = "Average treatment effect - mortality 28 days", # get the title into the figure
            # xlim = c(0.15,5),
            # xlab = "Average treatment effect (95% CI)"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
# Open a pdf file
# pdf("./fp_aggregated.pdf", width=9, height=4)
# forest.meta(i.mort28_adhoc_se,
#             xlim = c(0.1,5),
#             xlab = "                  Favours Remdesivir <-> Favours No Remdesivir",
#             fs.xlab = 9)
# dev.off()
```
Discussion points:
1. REML or ML ? -> Give the exact same result, but the one-stage uses ML (including centering) due to rare events. REML is preferred (see notes), but to correspond with one-stage, a sens-analysis with ML is probably worth it. The choice of estimator might have the biggest influence on the 95%CI, larger than other model parameter choices.

# (ii) Mortality at day 60

```r
str(df_mort60)
```

```
## 'data.frame':	6 obs. of  10 variables:
##  $ variable         : chr  "death at day 60" "death at day 60" "death at day 60" "death at day 60_firth" ...
##  $ hazard_odds_ratio: num  0.89 0.737 0.791 2.537 0.182 ...
##  $ ci_lower         : num  0.44951 0.41582 0.14728 0.12715 0.00131 ...
##  $ ci_upper         : num  1.75 1.29 3.83 380.13 2.29 ...
##  $ standard_error   : num  0.346 0.287 0.798 1.315 1.358 ...
##  $ p_value          : num  0.736 0.287 0.769 0.552 0.203 ...
##  $ n_intervention   : num  145 515 46 58 55 815
##  $ n_control        : num  144 518 51 58 55 811
##  $ trial            : chr  "Bari-SolidAct" "ACTT-2" "Ghazaeian" "TOFACOV" ...
##  $ JAKi             : chr  "Baricitinib" "Baricitinib" "Tofacitinib" "Tofacitinib" ...
```

```r
mort60 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_mort60,
                      n.e = n_intervention + n_control,
                      # n.c = n_control,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = F,
                      method.tau = "REML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "se", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Average treatment effect - mortality 60 days",
                      # subset = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # exclude entirely
                      # exclude = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # include in forestplot but exclude from analysis
                      )
summary(mort60)
```

```
## Review:     Average treatment effect - mortality 60 days
## 
##                   OR            95%-CI %W(random)
## Bari-SolidAct 0.8900 [0.4520;  1.7522]       13.3
## ACTT-2        0.7368 [0.4197;  1.2933]       19.3
## Ghazaeian     0.7909 [0.1654;  3.7807]        2.5
## TOFACOV       2.5366 [0.1928; 33.3748]        0.9
## COVINIB       0.1822 [0.0127;  2.6082]        0.9
## COV-BARRIER   0.5696 [0.4181;  0.7758]       63.1
## 
## Number of studies: k = 6
## Number of observations: o = 3271
## 
##                                  OR           95%-CI     t p-value
## Random effects model (HK-SE) 0.6430 [0.4646; 0.8898] -3.49  0.0174
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0004 [0.0000; 2.6022]; tau = 0.0211 [0.0000; 1.6131]
##  I^2 = 0.0% [0.0%; 74.6%]; H = 1.00 [1.00; 1.99]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  3.72    5  0.5904
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 5)
```

```r
forest.meta(mort60,
            # hetstat = T,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE", "n.e"),
            leftlabs = c("Trial", "log(OR)", "Standard Error", "Sample Size"),
            text.random = "Average treatment effect (random effects model)",
            title = "Average treatment effect - mortality 60 days", # get the title into the figure
            # xlim = c(0.15,5),
            # xlab = "Average treatment effect (95% CI)"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
Discussion points

# (iii) Time to death within max. follow-up time

```r
str(df_ttdeath)
```

```
## 'data.frame':	4 obs. of  10 variables:
##  $ variable         : chr  "death within fup" "death within fup" "death within fup" "death within fup"
##  $ hazard_odds_ratio: num  0.773 0.741 0.838 0.614
##  $ ci_lower         : num  0.432 0.442 0.187 0.476
##  $ ci_upper         : num  1.38 1.24 3.75 0.79
##  $ standard_error   : num  0.296 0.264 0.764 0.129
##  $ p_value          : num  0.384572 0.256266 0.817035 0.000154
##  $ n_intervention   : num  145 515 46 815
##  $ n_control        : num  144 518 51 811
##  $ trial            : chr  "Bari-SolidAct" "ACTT-2" "Ghazaeian" "COV-BARRIER"
##  $ JAKi             : chr  "Baricitinib" "Baricitinib" "Tofacitinib" "Baricitinib"
```

```r
ttdeath <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_ttdeath,
                      n.e = n_intervention + n_control,
                      # n.c = n_control,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = F,
                      method.tau = "REML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "se", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Average treatment effect - time to death",
                      # subset = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # exclude entirely
                      # exclude = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # include in forestplot but exclude from analysis
                      )
summary(ttdeath)
```

```
## Review:     Average treatment effect - time to death
## 
##                   OR           95%-CI %W(random)
## Bari-SolidAct 0.7727 [0.4322; 1.3817]       13.0
## ACTT-2        0.7409 [0.4415; 1.2434]       16.4
## Ghazaeian     0.8380 [0.1874; 3.7469]        2.0
## COV-BARRIER   0.6136 [0.4764; 0.7902]       68.6
## 
## Number of studies: k = 4
## Number of observations: o = 3045
## 
##                                  OR           95%-CI     t p-value
## Random effects model (HK-SE) 0.6561 [0.4668; 0.9221] -3.94  0.0291
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.1376]; tau = 0 [0.0000; 0.3709]
##  I^2 = 0.0% [0.0%; 84.7%]; H = 1.00 [1.00; 2.56]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  0.89    3  0.8281
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 3)
```

```r
forest.meta(ttdeath,
            # hetstat = T,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE", "n.e"),
            leftlabs = c("Trial", "log(OR)", "Standard Error", "Sample Size"),
            text.random = "Average treatment effect (random effects model)",
            title = "Average treatment effect - time to death", # get the title into the figure
            # xlim = c(0.15,5),
            # xlab = "Average treatment effect (95% CI)"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
Discussion points

# (iv) New mechanical ventilation or death within 28 days

```r
str(df_new_mvd28)
```

```
## 'data.frame':	6 obs. of  10 variables:
##  $ variable         : chr  "new MV or death within 28d" "new MV or death within 28d" "new MV or death within 28d" "new MV or death within 28d" ...
##  $ hazard_odds_ratio: num  1.016 0.71 0.791 0.503 0.199 ...
##  $ ci_lower         : num  0.5916 0.4833 0.1473 0.0222 0.0287 ...
##  $ ci_upper         : num  1.747 1.038 3.826 5.795 0.862 ...
##  $ standard_error   : num  0.276 0.195 0.798 1.271 0.826 ...
##  $ p_value          : num  0.9528 0.078 0.7688 0.5891 0.0509 ...
##  $ n_intervention   : num  145 515 46 58 55 815
##  $ n_control        : num  144 518 51 58 55 811
##  $ trial            : chr  "Bari-SolidAct" "ACTT-2" "Ghazaeian" "TOFACOV" ...
##  $ JAKi             : chr  "Baricitinib" "Baricitinib" "Tofacitinib" "Tofacitinib" ...
```

```r
new.mvd28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_new_mvd28,
                      n.e = n_intervention + n_control,
                      # n.c = n_control,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = F,
                      method.tau = "REML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "se", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Average treatment effect - New MV or death within 28 days",
                      # subset = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # exclude entirely
                      # exclude = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # include in forestplot but exclude from analysis
                      )
summary(new.mvd28)
```

```
## Review:     Average treatment effect - New MV or death within 28 days
## 
##                   OR           95%-CI %W(random)
## Bari-SolidAct 1.0165 [0.5923; 1.7443]       13.4
## ACTT-2        0.7097 [0.4846; 1.0392]       26.8
## Ghazaeian     0.7909 [0.1654; 3.7807]        1.6
## TOFACOV       0.5034 [0.0417; 6.0761]        0.6
## COVINIB       0.1994 [0.0395; 1.0066]        1.5
## COV-BARRIER   0.7651 [0.5879; 0.9958]       56.1
## 
## Number of studies: k = 6
## Number of observations: o = 3271
## 
##                                  OR           95%-CI     t p-value
## Random effects model (HK-SE) 0.7618 [0.5880; 0.9870] -2.70  0.0428
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 1.4870]; tau = 0 [0.0000; 1.2194]
##  I^2 = 0.0% [0.0%; 74.6%]; H = 1.00 [1.00; 1.99]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  3.97    5  0.5537
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 5)
```

```r
forest.meta(new.mvd28,
            # hetstat = T,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE", "n.e"),
            leftlabs = c("Trial", "log(OR)", "Standard Error", "Sample Size"),
            text.random = "Average treatment effect (random effects model)",
            title = "Average treatment effect - New MV or death within 28 days", # get the title into the figure
            # xlim = c(0.15,5),
            # xlab = "Average treatment effect (95% CI)"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
Discussion points

# (iv.i) New mechanical ventilation among survivors within 28 days

```r
str(df_new_mv28)
```

```
## 'data.frame':	5 obs. of  10 variables:
##  $ variable         : chr  "new MV within 28d" "new MV within 28d" "new MV within 28d_firth" "new MV within 28d" ...
##  $ hazard_odds_ratio: num  1.483 0.705 0.217 0.27 1.214
##  $ ci_lower         : num  0.72932 0.4374 0.00154 0.03782 0.83795
##  $ ci_upper         : num  3.09 1.13 2.88 1.25 1.77
##  $ standard_error   : num  0.366 0.242 1.385 0.848 0.19
##  $ p_value          : num  0.281 0.149 0.269 0.122 0.307
##  $ n_intervention   : num  114 449 57 55 702
##  $ n_control        : num  108 436 58 53 661
##  $ trial            : chr  "Bari-SolidAct" "ACTT-2" "TOFACOV" "COVINIB" ...
##  $ JAKi             : chr  "Baricitinib" "Baricitinib" "Tofacitinib" "Baricitinib" ...
```

```r
new.mv28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_new_mv28,
                      n.e = n_intervention + n_control,
                      # n.c = n_control,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = F,
                      method.tau = "REML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "se", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Average treatment effect - New MV among survivors within 28 days",
                      # subset = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # exclude entirely
                      # exclude = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # include in forestplot but exclude from analysis
                      )
summary(new.mv28)
```

```
## Review:     Average treatment effect - New MV among survivors within 28 days
## 
##                   OR           95%-CI %W(random)
## Bari-SolidAct 1.4835 [0.7243; 3.0383]       21.7
## ACTT-2        0.7055 [0.4394; 1.1328]       32.2
## TOFACOV       0.2175 [0.0144; 3.2855]        2.5
## COVINIB       0.2698 [0.0512; 1.4223]        6.1
## COV-BARRIER   1.2142 [0.8368; 1.7617]       37.6
## 
## Number of studies: k = 5
## Number of observations: o = 2693
## 
##                                  OR           95%-CI     t p-value
## Random effects model (HK-SE) 0.9306 [0.4685; 1.8488] -0.29  0.7857
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0968 [0.0000; 5.4203]; tau = 0.3112 [0.0000; 2.3281]
##  I^2 = 49.3% [0.0%; 81.4%]; H = 1.40 [1.00; 2.32]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  7.89    4  0.0957
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 4)
```

```r
forest.meta(new.mv28,
            # hetstat = T,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE", "n.e"),
            leftlabs = c("Trial", "log(OR)", "Standard Error", "Sample Size"),
            text.random = "Average treatment effect (random effects model)",
            title = "Average treatment effect - New MV among survivors within 28 days", # get the title into the figure
            # xlim = c(0.15,5),
            # xlab = "Average treatment effect (95% CI)"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
Discussion points

# (v) Clinical status at day 28

```r
str(df_clin28)
```

```
## 'data.frame':	6 obs. of  10 variables:
##  $ variable         : chr  "clinical status at day 28" "clinical status at day 28" "clinical status at day 28" "clinical status at day 28" ...
##  $ hazard_odds_ratio: num  0.945 0.703 0.826 0.509 0.321 ...
##  $ ci_lower         : num  0.5773 0.4833 0.1539 0.0667 0.0428 ...
##  $ ci_upper         : num  1.55 1.02 3.99 2.84 1.65 ...
##  $ standard_error   : num  0.251 0.19 0.798 0.907 0.887 ...
##  $ p_value          : num  0.8219 0.0638 0.8102 0.4563 0.2003 ...
##  $ n_intervention   : num  145 515 46 58 55 815
##  $ n_control        : num  144 518 51 58 55 811
##  $ trial            : chr  "Bari-SolidAct" "ACTT-2" "Ghazaeian" "TOFACOV" ...
##  $ JAKi             : chr  "Baricitinib" "Baricitinib" "Tofacitinib" "Tofacitinib" ...
```

```r
clin28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_clin28,
                      n.e = n_intervention + n_control,
                      # n.c = n_control,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = F,
                      method.tau = "REML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "se", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Average treatment effect - Clinical status at day 28",
                      # subset = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # exclude entirely
                      # exclude = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # include in forestplot but exclude from analysis
                      )
summary(clin28)
```

```
## Review:     Average treatment effect - Clinical status at day 28
## 
##                   OR           95%-CI %W(random)
## Bari-SolidAct 0.9451 [0.5778; 1.5456]       14.1
## ACTT-2        0.7031 [0.4845; 1.0204]       24.6
## Ghazaeian     0.8256 [0.1729; 3.9418]        1.4
## TOFACOV       0.5090 [0.0861; 3.0089]        1.1
## COVINIB       0.3212 [0.0565; 1.8266]        1.1
## COV-BARRIER   0.8487 [0.6655; 1.0823]       57.7
## 
## Number of studies: k = 6
## Number of observations: o = 3271
## 
##                                  OR           95%-CI     t p-value
## Random effects model (HK-SE) 0.8089 [0.6349; 1.0306] -2.25  0.0742
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.4620]; tau = 0 [0.0000; 0.6797]
##  I^2 = 0.0% [0.0%; 74.6%]; H = 1.00 [1.00; 1.99]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  2.42    5  0.7878
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 5)
```

```r
forest.meta(clin28,
            # hetstat = T,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE", "n.e"),
            leftlabs = c("Trial", "log(OR)", "Standard Error", "Sample Size"),
            text.random = "Average treatment effect (random effects model)",
            title = "Average treatment effect - Clinical status at day 28", # get the title into the figure
            # xlim = c(0.15,5),
            # xlab = "Average treatment effect (95% CI)"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
Discussion points

# (vi) Time to discharge or reaching discharge criteria up to day 28. Death = Competing event

```r
str(df_ttdischarge_comp)
```

```
## 'data.frame':	6 obs. of  10 variables:
##  $ variable         : chr  "discharge within 28 days, death=comp.event" "discharge within 28 days, death=comp.event" "discharge within 28 days, death=comp.event" "discharge within 28 days, death=comp.event" ...
##  $ hazard_odds_ratio: num  1.113 1.16 0.738 1.256 1.53 ...
##  $ ci_lower         : num  0.847 1.02 0.48 0.876 1.059 ...
##  $ ci_upper         : num  1.46 1.32 1.14 1.8 2.21 ...
##  $ standard_error   : num  0.1396 0.0656 0.2198 0.1839 0.1875 ...
##  $ p_value          : num  0.44 0.024 0.167 0.21 0.023 ...
##  $ n_intervention   : num  145 515 46 58 55 815
##  $ n_control        : num  144 518 51 58 55 811
##  $ trial            : chr  "Bari-SolidAct" "ACTT-2" "Ghazaeian" "TOFACOV" ...
##  $ JAKi             : chr  "Baricitinib" "Baricitinib" "Tofacitinib" "Tofacitinib" ...
```

```r
ttdischarge.comp <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_ttdischarge_comp,
                      n.e = n_intervention + n_control,
                      # n.c = n_control,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = F,
                      method.tau = "REML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "se", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Average treatment effect - Time to discharge within 28 days",
                      # subset = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # exclude entirely
                      # exclude = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # include in forestplot but exclude from analysis
                      )
summary(ttdischarge.comp)
```

```
## Review:     Average treatment effect - Time to discharge within 28 days
## 
##                   OR           95%-CI %W(random)
## Bari-SolidAct 1.1133 [0.8468; 1.4636]        7.4
## ACTT-2        1.1601 [1.0202; 1.3192]       33.5
## Ghazaeian     0.7381 [0.4798; 1.1355]        3.0
## TOFACOV       1.2562 [0.8760; 1.8014]        4.3
## COVINIB       1.5296 [1.0591; 2.2092]        4.1
## COV-BARRIER   1.1099 [0.9966; 1.2360]       47.7
## 
## Number of studies: k = 6
## Number of observations: o = 3271
## 
##                                  OR           95%-CI    t p-value
## Random effects model (HK-SE) 1.1338 [1.0104; 1.2722] 2.80  0.0379
## 
## Quantifying heterogeneity:
##  tau^2 < 0.0001 [0.0000; 0.3003]; tau = 0.0018 [0.0000; 0.5480]
##  I^2 = 28.2% [0.0%; 70.4%]; H = 1.18 [1.00; 1.84]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  6.97    5  0.2232
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 5)
```

```r
forest.meta(ttdischarge.comp,
            # hetstat = T,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE", "n.e"),
            leftlabs = c("Trial", "log(OR)", "Standard Error", "Sample Size"),
            text.random = "Average treatment effect (random effects model)",
            title = "Average treatment effect - Time to discharge within 28 days", # get the title into the figure
            # xlim = c(0.15,5),
            # xlab = "Average treatment effect (95% CI)"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
Discussion points

# (vi.i) Time to discharge or reaching discharge criteria up to day 28. Death = Hypothetical

```r
str(df_ttdischarge_hypo)
```

```
## 'data.frame':	6 obs. of  10 variables:
##  $ variable         : chr  "discharge within 28 days, death=hypo.event" "discharge within 28 days, death=hypo.event" "discharge within 28 days, death=hypo.event" "discharge within 28 days, death=hypo.event" ...
##  $ hazard_odds_ratio: num  1.083 1.211 0.823 1.284 1.59 ...
##  $ ci_lower         : num  0.816 1.057 0.54 0.88 1.069 ...
##  $ ci_upper         : num  1.44 1.39 1.25 1.87 2.36 ...
##  $ standard_error   : num  0.1442 0.0694 0.215 0.1924 0.2026 ...
##  $ p_value          : num  0.5816 0.00583 0.36545 0.19432 0.02219 ...
##  $ n_intervention   : num  145 515 46 58 55 815
##  $ n_control        : num  144 518 51 58 55 811
##  $ trial            : chr  "Bari-SolidAct" "ACTT-2" "Ghazaeian" "TOFACOV" ...
##  $ JAKi             : chr  "Baricitinib" "Baricitinib" "Tofacitinib" "Tofacitinib" ...
```

```r
ttdischarge.hypo <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_ttdischarge_hypo,
                      n.e = n_intervention + n_control,
                      # n.c = n_control,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = F,
                      method.tau = "REML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "se", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Average treatment effect - Time to discharge within 28 days",
                      # subset = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # exclude entirely
                      # exclude = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # include in forestplot but exclude from analysis
                      )
summary(ttdischarge.hypo)
```

```
## Review:     Average treatment effect - Time to discharge within 28 days
## 
##                   OR           95%-CI %W(random)
## Bari-SolidAct 1.0827 [0.8162; 1.4361]        7.6
## ACTT-2        1.2108 [1.0569; 1.3872]       32.8
## Ghazaeian     0.8232 [0.5401; 1.2546]        3.4
## TOFACOV       1.2836 [0.8804; 1.8714]        4.3
## COVINIB       1.5895 [1.0685; 2.3646]        3.8
## COV-BARRIER   1.1214 [1.0023; 1.2548]       48.1
## 
## Number of studies: k = 6
## Number of observations: o = 3271
## 
##                                  OR           95%-CI    t p-value
## Random effects model (HK-SE) 1.1569 [1.0325; 1.2962] 3.29  0.0216
## 
## Quantifying heterogeneity:
##  tau^2 < 0.0001 [0.0000; 0.2401]; tau = 0.0015 [0.0000; 0.4900]
##  I^2 = 19.3% [0.0%; 64.0%]; H = 1.11 [1.00; 1.67]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  6.19    5  0.2879
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 5)
```

```r
forest.meta(ttdischarge.hypo,
            # hetstat = T,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE", "n.e"),
            leftlabs = c("Trial", "log(OR)", "Standard Error", "Sample Size"),
            text.random = "Average treatment effect (random effects model)",
            title = "Average treatment effect - Time to discharge within 28 days", # get the title into the figure
            # xlim = c(0.15,5),
            # xlab = "Average treatment effect (95% CI)"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
Discussion points

# (vi.ii) Time to discharge or reaching discharge criteria up to day 28. Death = Censored

```r
str(df_ttdischarge)
```

```
## 'data.frame':	6 obs. of  10 variables:
##  $ variable         : chr  "discharge within 28 days" "discharge within 28 days" "discharge within 28 days" "discharge within 28 days" ...
##  $ hazard_odds_ratio: num  1.072 1.214 0.738 1.284 1.586 ...
##  $ ci_lower         : num  0.808 1.06 0.48 0.88 1.066 ...
##  $ ci_upper         : num  1.42 1.39 1.14 1.87 2.36 ...
##  $ standard_error   : num  0.1441 0.0694 0.2198 0.1924 0.2026 ...
##  $ p_value          : num  0.62921 0.00519 0.16705 0.19432 0.02278 ...
##  $ n_intervention   : num  145 515 46 58 55 815
##  $ n_control        : num  144 518 51 58 55 811
##  $ trial            : chr  "Bari-SolidAct" "ACTT-2" "Ghazaeian" "TOFACOV" ...
##  $ JAKi             : chr  "Baricitinib" "Baricitinib" "Tofacitinib" "Tofacitinib" ...
```

```r
ttdischarge.cens <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_ttdischarge,
                      n.e = n_intervention + n_control,
                      # n.c = n_control,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = F,
                      method.tau = "REML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "se", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Average treatment effect - Time to discharge within 28 days",
                      # subset = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # exclude entirely
                      # exclude = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # include in forestplot but exclude from analysis
                      )
summary(ttdischarge.cens)
```

```
## Review:     Average treatment effect - Time to discharge within 28 days
## 
##                   OR           95%-CI %W(random)
## Bari-SolidAct 1.0721 [0.8083; 1.4219]       11.5
## ACTT-2        1.2141 [1.0596; 1.3910]       31.6
## Ghazaeian     0.7381 [0.4798; 1.1355]        5.5
## TOFACOV       1.2836 [0.8804; 1.8714]        7.0
## COVINIB       1.5863 [1.0664; 2.3597]        6.4
## COV-BARRIER   1.0679 [0.9544; 1.1949]       38.0
## 
## Number of studies: k = 6
## Number of observations: o = 3271
## 
##                                  OR           95%-CI    t p-value
## Random effects model (HK-SE) 1.1326 [0.9565; 1.3411] 1.89  0.1167
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0043 [0.0000; 0.3459]; tau = 0.0658 [0.0000; 0.5881]
##  I^2 = 45.4% [0.0%; 78.4%]; H = 1.35 [1.00; 2.15]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  9.16    5  0.1027
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 5)
```

```r
forest.meta(ttdischarge.cens,
            # hetstat = T,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE", "n.e"),
            leftlabs = c("Trial", "log(OR)", "Standard Error", "Sample Size"),
            text.random = "Average treatment effect (random effects model)",
            title = "Average treatment effect - Time to discharge within 28 days", # get the title into the figure
            # xlim = c(0.15,5),
            # xlab = "Average treatment effect (95% CI)"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
Discussion points

# (vi.iii) Time to sustained discharge or reaching discharge criteria up to day 28. Death = Censored

```r
str(df_ttdischarge_sus)
```

```
## 'data.frame':	6 obs. of  10 variables:
##  $ variable         : chr  "sustained discharge within 28 days" "sustained discharge within 28 days" "sustained discharge within 28 days" "sustained discharge within 28 days" ...
##  $ hazard_odds_ratio: num  1.055 1.214 0.738 1.284 1.586 ...
##  $ ci_lower         : num  0.795 1.06 0.48 0.88 1.066 ...
##  $ ci_upper         : num  1.4 1.39 1.14 1.87 2.36 ...
##  $ standard_error   : num  0.1448 0.0694 0.2198 0.1924 0.2026 ...
##  $ p_value          : num  0.70965 0.00519 0.16705 0.19432 0.02278 ...
##  $ n_intervention   : num  145 515 46 58 55 815
##  $ n_control        : num  144 518 51 58 55 811
##  $ trial            : chr  "Bari-SolidAct" "ACTT-2" "Ghazaeian" "TOFACOV" ...
##  $ JAKi             : chr  "Baricitinib" "Baricitinib" "Tofacitinib" "Tofacitinib" ...
```

```r
ttdischarge.sus <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_ttdischarge_sus,
                      n.e = n_intervention + n_control,
                      # n.c = n_control,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = F,
                      method.tau = "REML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "se", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Average treatment effect - Time to sustained discharge within 28 days",
                      # subset = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # exclude entirely
                      # exclude = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # include in forestplot but exclude from analysis
                      )
summary(ttdischarge.sus)
```

```
## Review:     Average treatment effect - Time to sustained discharge within 28 ...
## 
##                   OR           95%-CI %W(random)
## Bari-SolidAct 1.0554 [0.7946; 1.4017]       12.3
## ACTT-2        1.2141 [1.0596; 1.3910]       30.9
## Ghazaeian     0.7381 [0.4798; 1.1355]        6.1
## TOFACOV       1.2836 [0.8804; 1.8714]        7.7
## COVINIB       1.5863 [1.0664; 2.3597]        7.0
## COV-BARRIER   1.0582 [0.9454; 1.1845]       36.1
## 
## Number of studies: k = 6
## Number of observations: o = 3271
## 
##                                  OR           95%-CI    t p-value
## Random effects model (HK-SE) 1.1276 [0.9435; 1.3475] 1.73  0.1438
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0058 [0.0000; 0.3498]; tau = 0.0761 [0.0000; 0.5914]
##  I^2 = 47.6% [0.0%; 79.2%]; H = 1.38 [1.00; 2.19]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  9.54    5  0.0892
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 5)
```

```r
forest.meta(ttdischarge.sus,
            # hetstat = T,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE", "n.e"),
            leftlabs = c("Trial", "log(OR)", "Standard Error", "Sample Size"),
            text.random = "Average treatment effect (random effects model)",
            title = "Average treatment effect - Time to sustained discharge within 28 days", # get the title into the figure
            # xlim = c(0.15,5),
            # xlab = "Average treatment effect (95% CI)"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
Discussion points

# (vii) Viral clearance up to day 5

```r
str(df_vir_clear_5)
```

```
## 'data.frame':	2 obs. of  10 variables:
##  $ variable         : chr  "viral clearance until day 5" "viral clearance until day 5"
##  $ hazard_odds_ratio: num  1.495 0.898
##  $ ci_lower         : num  0.639 0.67
##  $ ci_upper         : num  3.58 1.2
##  $ standard_error   : num  0.437 0.149
##  $ p_value          : num  0.358 0.471
##  $ n_intervention   : num  62 492
##  $ n_control        : num  59 486
##  $ trial            : chr  "Bari-SolidAct" "COV-BARRIER"
##  $ JAKi             : chr  "Baricitinib" "Baricitinib"
```

```r
vir.clear5 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_vir_clear_5,
                      n.e = n_intervention + n_control,
                      # n.c = n_control,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = F,
                      method.tau = "REML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "se", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Average treatment effect - Viral clearance up to day 5",
                      # subset = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # exclude entirely
                      # exclude = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # include in forestplot but exclude from analysis
                      )
summary(vir.clear5)
```

```
## Review:     Average treatment effect - Viral clearance up to day 5
## 
##                   OR           95%-CI %W(random)
## Bari-SolidAct 1.4947 [0.6349; 3.5190]       17.5
## COV-BARRIER   0.8980 [0.6700; 1.2034]       82.5
## 
## Number of studies: k = 2
## Number of observations: o = 1099
## 
##                                  OR            95%-CI     t p-value
## Random effects model (HK-SE) 0.9820 [0.0837; 11.5267] -0.09  0.9404
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0233; tau = 0.1525; I^2 = 17.9%; H = 1.10
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  1.22    1  0.2697
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Hartung-Knapp adjustment for random effects model (df = 1)
```

```r
forest.meta(vir.clear5,
            # hetstat = T,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE", "n.e"),
            leftlabs = c("Trial", "log(OR)", "Standard Error", "Sample Size"),
            text.random = "Average treatment effect (random effects model)",
            title = "Average treatment effect - Viral clearance up to day 5", # get the title into the figure
            # xlim = c(0.15,5),
            # xlab = "Average treatment effect (95% CI)"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
Discussion points

# (viii) Viral clearance up to day 10

```r
str(df_vir_clear_10)
```

```
## 'data.frame':	2 obs. of  10 variables:
##  $ variable         : chr  "viral clearance until day 10" "viral clearance until day 10"
##  $ hazard_odds_ratio: num  1.09 0.99
##  $ ci_lower         : num  0.536 0.77
##  $ ci_upper         : num  2.21 1.27
##  $ standard_error   : num  0.361 0.128
##  $ p_value          : num  0.815 0.94
##  $ n_intervention   : num  66 538
##  $ n_control        : num  61 522
##  $ trial            : chr  "Bari-SolidAct" "COV-BARRIER"
##  $ JAKi             : chr  "Baricitinib" "Baricitinib"
```

```r
vir.clear10 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_vir_clear_10,
                      n.e = n_intervention + n_control,
                      # n.c = n_control,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = F,
                      method.tau = "REML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "se", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Average treatment effect - Viral clearance up to day 10",
                      # subset = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # exclude entirely
                      # exclude = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # include in forestplot but exclude from analysis
                      )
summary(vir.clear10)
```

```
## Review:     Average treatment effect - Viral clearance up to day 10
## 
##                   OR           95%-CI %W(random)
## Bari-SolidAct 1.0882 [0.5367; 2.2064]       11.2
## COV-BARRIER   0.9903 [0.7701; 1.2736]       88.8
## 
## Number of studies: k = 2
## Number of observations: o = 1187
## 
##                                  OR           95%-CI    t p-value
## Random effects model (HK-SE) 1.0009 [0.2154; 4.6513] 0.01  0.9954
## 
## Quantifying heterogeneity:
##  tau^2 = 0; tau = 0; I^2 = 0.0%; H = 1.00
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  0.06    1  0.8054
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Hartung-Knapp adjustment for random effects model (df = 1)
```

```r
forest.meta(vir.clear10,
            # hetstat = T,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE", "n.e"),
            leftlabs = c("Trial", "log(OR)", "Standard Error", "Sample Size"),
            text.random = "Average treatment effect (random effects model)",
            title = "Average treatment effect - Viral clearance up to day 10", # get the title into the figure
            # xlim = c(0.15,5),
            # xlab = "Average treatment effect (95% CI)"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
Discussion points

# (ix) Viral clearance up to day 15

```r
str(df_vir_clear_15)
```

```
## 'data.frame':	2 obs. of  10 variables:
##  $ variable         : chr  "viral clearance until day 15" "viral clearance until day 15"
##  $ hazard_odds_ratio: num  1.004 0.972
##  $ ci_lower         : num  0.496 0.763
##  $ ci_upper         : num  2.03 1.24
##  $ standard_error   : num  0.359 0.123
##  $ p_value          : num  0.991 0.816
##  $ n_intervention   : num  67 551
##  $ n_control        : num  61 545
##  $ trial            : chr  "Bari-SolidAct" "COV-BARRIER"
##  $ JAKi             : chr  "Baricitinib" "Baricitinib"
```

```r
vir.clear15 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_vir_clear_15,
                      n.e = n_intervention + n_control,
                      # n.c = n_control,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = F,
                      method.tau = "REML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "se", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Average treatment effect - Viral clearance up to day 15",
                      # subset = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # exclude entirely
                      # exclude = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # include in forestplot but exclude from analysis
                      )
summary(vir.clear15)
```

```
## Review:     Average treatment effect - Viral clearance up to day 15
## 
##                   OR           95%-CI %W(random)
## Bari-SolidAct 1.0042 [0.4970; 2.0290]       10.6
## COV-BARRIER   0.9717 [0.7630; 1.2375]       89.4
## 
## Number of studies: k = 2
## Number of observations: o = 1224
## 
##                                  OR           95%-CI     t p-value
## Random effects model (HK-SE) 0.9751 [0.2215; 4.2931] -0.22  0.8646
## 
## Quantifying heterogeneity:
##  tau^2 = 0; tau = 0; I^2 = 0.0%; H = 1.00
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  0.01    1  0.9309
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Hartung-Knapp adjustment for random effects model (df = 1)
```

```r
forest.meta(vir.clear15,
            # hetstat = T,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE", "n.e"),
            leftlabs = c("Trial", "log(OR)", "Standard Error", "Sample Size"),
            text.random = "Average treatment effect (random effects model)",
            title = "Average treatment effect - Viral clearance up to day 15", # get the title into the figure
            # xlim = c(0.15,5),
            # xlab = "Average treatment effect (95% CI)"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-16-1.png)<!-- -->
Discussion points

# (x) Adverse event(s) grade 3 or 4, or a serious adverse event(s), excluding death, by day 28. ANY

```r
str(df_ae28)
```

```
## 'data.frame':	4 obs. of  10 variables:
##  $ variable         : chr  "any AE grade 3,4 within 28 days_firth" "any AE grade 3,4 within 28 days" "any AE grade 3,4 within 28 days" "Any AE grade 3,4 within 28 days"
##  $ hazard_odds_ratio: num  3.225 0.694 0.797 1.24
##  $ ci_lower         : num  0.17 0.241 0.308 0.843
##  $ ci_upper         : num  472.45 1.96 2.04 1.83
##  $ standard_error   : num  1.476 0.529 0.477 0.198
##  $ p_value          : num  0.441 0.489 0.634 0.277
##  $ n_intervention   : num  46 58 55 734
##  $ n_control        : num  51 58 53 684
##  $ trial            : chr  "Ghazaeian" "TOFACOV" "COVINIB" "COV-BARRIER"
##  $ JAKi             : chr  "Tofacitinib" "Tofacitinib" "Baricitinib" "Baricitinib"
```

```r
ae28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_ae28,
                      n.e = n_intervention + n_control,
                      # n.c = n_control,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = F,
                      method.tau = "REML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "se", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Average treatment effect - dverse event(s) grade 3 or 4, or a serious adverse event(s), excluding death, by day 28. ANY",
                      # subset = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # exclude entirely
                      # exclude = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # include in forestplot but exclude from analysis
                      )
summary(ae28)
```

```
## Review:     Average treatment effect - dverse event(s) grade 3 or 4, or a se ...
## 
##                 OR            95%-CI %W(random)
## Ghazaeian   3.2247 [0.1787; 58.1925]        1.4
## TOFACOV     0.6936 [0.2461;  1.9552]       10.5
## COVINIB     0.7969 [0.3127;  2.0306]       12.9
## COV-BARRIER 1.2399 [0.8413;  1.8275]       75.2
## 
## Number of studies: k = 4
## Number of observations: o = 1739
## 
##                                  OR           95%-CI    t p-value
## Random effects model (HK-SE) 1.1158 [0.6463; 1.9266] 0.64  0.5684
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 5.2235]; tau = 0 [0.0000; 2.2855]
##  I^2 = 0.0% [0.0%; 84.7%]; H = 1.00 [1.00; 2.56]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  2.11    3  0.5505
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 3)
```

```r
forest.meta(ae28,
            # hetstat = T,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE", "n.e"),
            leftlabs = c("Trial", "log(OR)", "Standard Error", "Sample Size"),
            text.random = "Average treatment effect (random effects model)",
            title = "Average treatment effect - dverse event(s) grade 3 or 4, or a serious adverse event(s), excluding death, by day 28. ANY", # get the title into the figure
            # xlim = c(0.15,5),
            # xlab = "Average treatment effect (95% CI)"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
Discussion points

# (x.i) Adverse event(s) grade 3 or 4, or a serious adverse event(s), excluding death, by day 28. SEVERAL

```r
str(df_ae28sev)
```

```
## 'data.frame':	4 obs. of  10 variables:
##  $ variable         : chr  "AEs grade 3,4 within 28 days_firth" "AEs grade 3,4 within 28 days" "AEs grade 3,4 within 28 days" "AEs grade 3,4 within 28 days"
##  $ hazard_odds_ratio: num  3.225 0.694 0.588 1.287
##  $ ci_lower         : num  0.17 0.241 0.324 0.969
##  $ ci_upper         : num  472.45 1.96 1.04 1.72
##  $ standard_error   : num  1.476 0.529 0.296 0.145
##  $ p_value          : num  0.4412 0.489 0.0724 0.0834
##  $ n_intervention   : num  46 58 55 734
##  $ n_control        : num  51 58 53 684
##  $ trial            : chr  "Ghazaeian" "TOFACOV" "COVINIB" "COV-BARRIER"
##  $ JAKi             : chr  "Tofacitinib" "Tofacitinib" "Baricitinib" "Baricitinib"
```

```r
ae28sev <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_ae28sev,
                      n.e = n_intervention + n_control,
                      # n.c = n_control,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = F,
                      method.tau = "REML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "se", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Average treatment effect - Adverse event(s) grade 3 or 4, or a serious adverse event(s), excluding death, by day 28. SEVERAL",
                      # subset = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # exclude entirely
                      # exclude = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # include in forestplot but exclude from analysis
                      )
summary(ae28sev)
```

```
## Review:     Average treatment effect - Adverse event(s) grade 3 or 4, or a s ...
## 
##                 OR            95%-CI %W(random)
## Ghazaeian   3.2247 [0.1787; 58.1925]        3.3
## TOFACOV     0.6936 [0.2461;  1.9552]       18.0
## COVINIB     0.5880 [0.3295;  1.0494]       32.8
## COV-BARRIER 1.2865 [0.9673;  1.7110]       45.9
## 
## Number of studies: k = 4
## Number of observations: o = 1739
## 
##                                  OR           95%-CI     t p-value
## Random effects model (HK-SE) 0.9175 [0.3803; 2.2132] -0.31  0.7760
## 
## Quantifying heterogeneity:
##  tau^2 = 0.1458 [0.0000; 6.9235]; tau = 0.3818 [0.0000; 2.6313]
##  I^2 = 56.7% [0.0%; 85.6%]; H = 1.52 [1.00; 2.64]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  6.93    3  0.0742
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 3)
```

```r
forest.meta(ae28sev,
            # hetstat = T,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE", "n.e"),
            leftlabs = c("Trial", "log(OR)", "Standard Error", "Sample Size"),
            text.random = "Average treatment effect (random effects model)",
            title = "Average treatment effect - Adverse event(s) grade 3 or 4, or a serious adverse event(s), excluding death, by day 28. SEVERAL", # get the title into the figure
            # xlim = c(0.15,5),
            # xlab = "Average treatment effect (95% CI)"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-18-1.png)<!-- -->
Discussion points

# Collect all treatment effect estimates across endpoints

```r
# Empty data frame to store the results
result_df <- data.frame(
  variable = character(),
  hazard_odds_ratio = numeric(),
  ci_lower = numeric(),
  ci_upper = numeric(),
  standard_error = numeric(),
  p_value = numeric(),
  n_intervention = numeric(),
  n_intervention_tot = numeric(),
  n_control = numeric(),
  n_control_tot = numeric()
)

# Function to extract treatment results from different model types
extract_trt_results <- function(model, variable_name, n_int, n_int_tot, n_cont, n_cont_tot) {
  if (inherits(model, "metagen")) {
    hazard_odds_ratio <- exp(summary(model)$TE.random)
    ci.lower <- exp(summary(model)$lower.random)
    ci.upper <- exp(summary(model)$upper.random)
    se <- summary(model)$seTE.random
    p_value <- summary(model)$pval.random
  } else {
    stop("Unsupported model class")
  }
  # capture the results
  result <- data.frame(
    variable = variable_name,
    hazard_odds_ratio = hazard_odds_ratio,
    ci_lower = ci.lower,
    ci_upper = ci.upper,
    standard_error = se,
    p_value = p_value,
    n_intervention = n_int,
    n_intervention_tot = n_int_tot,
    n_control = n_cont,
    n_control_tot = n_cont_tot
  )
  return(result)
}

# Loop through
result_list <- list()

result_list[[1]] <- extract_trt_results(mort28, "death at day 28",
                                        addmargins(table(df_tot_Muru$mort_28, df_tot_Muru$trt))[2,2], 
                                        addmargins(table(df_tot_Muru$mort_28, df_tot_Muru$trt))[3,2],
                                        addmargins(table(df_tot_Muru$mort_28, df_tot_Muru$trt))[2,1],
                                        addmargins(table(df_tot_Muru$mort_28, df_tot_Muru$trt))[3,1])
result_list[[2]] <- extract_trt_results(mort60, "death at day 60",
                                        addmargins(table(df_tot_Muru$mort_60, df_tot_Muru$trt))[2,2], 
                                        addmargins(table(df_tot_Muru$mort_60, df_tot_Muru$trt))[3,2],
                                        addmargins(table(df_tot_Muru$mort_60, df_tot_Muru$trt))[2,1],
                                        addmargins(table(df_tot_Muru$mort_60, df_tot_Muru$trt))[3,1])
result_list[[3]] <- extract_trt_results(ttdeath, "death within fup",
                                        addmargins(table(df_tot_Muru$death_reached, df_tot_Muru$trt))[2,2], 
                                        addmargins(table(df_tot_Muru$death_reached, df_tot_Muru$trt))[3,2],
                                        addmargins(table(df_tot_Muru$death_reached, df_tot_Muru$trt))[2,1],
                                        addmargins(table(df_tot_Muru$death_reached, df_tot_Muru$trt))[3,1])
result_list[[4]] <- extract_trt_results(new.mvd28, "new MV or death within 28d",
                                        addmargins(table(df_tot_Muru$new_mvd_28, df_tot_Muru$trt))[2,2], 
                                        addmargins(table(df_tot_Muru$new_mvd_28, df_tot_Muru$trt))[3,2],
                                        addmargins(table(df_tot_Muru$new_mvd_28, df_tot_Muru$trt))[2,1],
                                        addmargins(table(df_tot_Muru$new_mvd_28, df_tot_Muru$trt))[3,1])
result_list[[5]] <- extract_trt_results(new.mv28, "new MV within 28d",
                                        addmargins(table(df_tot_Muru$new_mv_28, df_tot_Muru$trt))[2,2], 
                                        addmargins(table(df_tot_Muru$new_mv_28, df_tot_Muru$trt))[3,2],
                                        addmargins(table(df_tot_Muru$new_mv_28, df_tot_Muru$trt))[2,1],
                                        addmargins(table(df_tot_Muru$new_mv_28, df_tot_Muru$trt))[3,1])
result_list[[6]] <- extract_trt_results(clin28, "clinical status at day 28",
                                        addmargins(table(df_tot_Muru$clinstatus_28_imp, df_tot_Muru$trt))[7,2], 
                                        addmargins(table(df_tot_Muru$clinstatus_28_imp, df_tot_Muru$trt))[7,2],
                                        addmargins(table(df_tot_Muru$clinstatus_28_imp, df_tot_Muru$trt))[7,1],
                                        addmargins(table(df_tot_Muru$clinstatus_28_imp, df_tot_Muru$trt))[7,1])
result_list[[7]] <- extract_trt_results(ttdischarge.comp, "discharge within 28 days, death=comp.event",
                                        addmargins(table(df_tot_Muru$discharge_reached, df_tot_Muru$trt))[2,2], 
                                        addmargins(table(df_tot_Muru$discharge_reached, df_tot_Muru$trt))[3,2],
                                        addmargins(table(df_tot_Muru$discharge_reached, df_tot_Muru$trt))[2,1],
                                        addmargins(table(df_tot_Muru$discharge_reached, df_tot_Muru$trt))[3,1])
result_list[[8]] <- extract_trt_results(ttdischarge.hypo, "discharge within 28 days, death=hypo.event",
                                        addmargins(table(df_tot_Muru$discharge_reached, df_tot_Muru$trt))[2,2], 
                                        addmargins(table(df_tot_Muru$discharge_reached, df_tot_Muru$trt))[3,2],
                                        addmargins(table(df_tot_Muru$discharge_reached, df_tot_Muru$trt))[2,1],
                                        addmargins(table(df_tot_Muru$discharge_reached, df_tot_Muru$trt))[3,1])
result_list[[9]] <- extract_trt_results(ttdischarge.cens, "discharge within 28 days, death=censored",
                                        addmargins(table(df_tot_Muru$discharge_reached, df_tot_Muru$trt))[2,2], 
                                        addmargins(table(df_tot_Muru$discharge_reached, df_tot_Muru$trt))[3,2],
                                        addmargins(table(df_tot_Muru$discharge_reached, df_tot_Muru$trt))[2,1],
                                        addmargins(table(df_tot_Muru$discharge_reached, df_tot_Muru$trt))[3,1])
result_list[[10]] <- extract_trt_results(ttdischarge.sus, "sustained discharge within 28 days",
                                        addmargins(table(df_tot_Muru$discharge_reached_sus, df_tot_Muru$trt))[2,2], 
                                        addmargins(table(df_tot_Muru$discharge_reached_sus, df_tot_Muru$trt))[3,2],
                                        addmargins(table(df_tot_Muru$discharge_reached_sus, df_tot_Muru$trt))[2,1],
                                        addmargins(table(df_tot_Muru$discharge_reached_sus, df_tot_Muru$trt))[3,1])
result_list[[11]] <- extract_trt_results(vir.clear5, "viral clearance until day 5",
                                        addmargins(table(df_tot_Muru$vir_clear_5, df_tot_Muru$trt))[2,2], 
                                        addmargins(table(df_tot_Muru$vir_clear_5, df_tot_Muru$trt))[3,2],
                                        addmargins(table(df_tot_Muru$vir_clear_5, df_tot_Muru$trt))[2,1],
                                        addmargins(table(df_tot_Muru$vir_clear_5, df_tot_Muru$trt))[3,1])
result_list[[12]] <- extract_trt_results(vir.clear10, "viral clearance until day 10",
                                        addmargins(table(df_tot_Muru$vir_clear_10, df_tot_Muru$trt))[2,2], 
                                        addmargins(table(df_tot_Muru$vir_clear_10, df_tot_Muru$trt))[3,2],
                                        addmargins(table(df_tot_Muru$vir_clear_10, df_tot_Muru$trt))[2,1],
                                        addmargins(table(df_tot_Muru$vir_clear_10, df_tot_Muru$trt))[3,1])
result_list[[13]] <- extract_trt_results(vir.clear15, "viral clearance until day 15",
                                        addmargins(table(df_tot_Muru$vir_clear_15, df_tot_Muru$trt))[2,2], 
                                        addmargins(table(df_tot_Muru$vir_clear_15, df_tot_Muru$trt))[3,2],
                                        addmargins(table(df_tot_Muru$vir_clear_15, df_tot_Muru$trt))[2,1],
                                        addmargins(table(df_tot_Muru$vir_clear_15, df_tot_Muru$trt))[3,1])
result_list[[14]] <- extract_trt_results(ae28, "Any AE grade 3,4 within 28 days",
                                        addmargins(table(df_tot_Muru$ae_28, df_tot_Muru$trt))[2,2], 
                                        addmargins(table(df_tot_Muru$ae_28, df_tot_Muru$trt))[3,2],
                                        addmargins(table(df_tot_Muru$ae_28, df_tot_Muru$trt))[2,1],
                                        addmargins(table(df_tot_Muru$ae_28, df_tot_Muru$trt))[3,1])
result_list[[15]] <- extract_trt_results(ae28sev, "AEs grade 3,4 within 28 days",
                                        addmargins(table(df_tot_Muru$ae_28_sev, df_tot_Muru$trt))[9,2], 
                                        addmargins(table(df_tot_Muru$ae_28_sev, df_tot_Muru$trt))[9,2],
                                        addmargins(table(df_tot_Muru$ae_28_sev, df_tot_Muru$trt))[9,1],
                                        addmargins(table(df_tot_Muru$ae_28_sev, df_tot_Muru$trt))[9,1])

# Filter out NULL results and bind the results into a single data frame
result_df <- do.call(rbind, Filter(function(x) !is.null(x), result_list))

# Add the analysis approach
result_df$approach <- "two-stage"

# Nicely formatted table
kable(result_df, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|variable                                   | hazard_odds_ratio|  ci_lower|   ci_upper| standard_error|   p_value| n_intervention| n_intervention_tot| n_control| n_control_tot|approach  |
|:------------------------------------------|-----------------:|---------:|----------:|--------------:|---------:|--------------:|------------------:|---------:|-------------:|:---------|
|death at day 28                            |         0.5795133| 0.4113851|  0.8163535|      0.1333001| 0.0094212|            125|               1684|       193|          1687|two-stage |
|death at day 60                            |         0.6429778| 0.4646233|  0.8897972|      0.1263851| 0.0173876|            152|               1684|       214|          1687|two-stage |
|death within fup                           |         0.6560949| 0.4668262|  0.9221001|      0.1069455| 0.0291212|            153|               1684|       214|          1687|two-stage |
|new MV or death within 28d                 |         0.7618224| 0.5880279|  0.9869827|      0.1007317| 0.0427504|            268|               1684|       333|          1687|two-stage |
|new MV within 28d                          |         0.9306486| 0.4684692|  1.8488021|      0.2472267| 0.7857166|            143|               1470|       140|          1413|two-stage |
|clinical status at day 28                  |         0.8089203| 0.6349042|  1.0306312|      0.0942301| 0.0742403|           1684|               1684|      1687|          1687|two-stage |
|discharge within 28 days, death=comp.event |         1.1337880| 1.0104104|  1.2722308|      0.0448177| 0.0379191|           1350|               1684|      1305|          1687|two-stage |
|discharge within 28 days, death=hypo.event |         1.1568680| 1.0325273|  1.2961823|      0.0442339| 0.0216126|           1350|               1684|      1305|          1687|two-stage |
|discharge within 28 days, death=censored   |         1.1325994| 0.9565386|  1.3410660|      0.0657242| 0.1166822|           1350|               1684|      1305|          1687|two-stage |
|sustained discharge within 28 days         |         1.1275720| 0.9435417|  1.3474959|      0.0693156| 0.1437881|           1342|               1684|      1303|          1687|two-stage |
|viral clearance until day 5                |         0.9819506| 0.0836518| 11.5266782|      0.1938327| 0.9403527|            136|                554|       138|           545|two-stage |
|viral clearance until day 10               |         1.0008751| 0.2153709|  4.6512825|      0.1209069| 0.9953944|            236|                604|       228|           583|two-stage |
|viral clearance until day 15               |         0.9751177| 0.2214836|  4.2931147|      0.1166524| 0.8645700|            292|                618|       291|           606|two-stage |
|Any AE grade 3,4 within 28 days            |         1.1158432| 0.6462771|  1.9265822|      0.1716092| 0.5684263|             89|                943|        75|           896|two-stage |
|AEs grade 3,4 within 28 days               |         0.9174720| 0.3803375|  2.2131788|      0.2766938| 0.7759547|            943|                943|       896|           896|two-stage |

```r
# Save
saveRDS(result_df, file = "overall_results_two-stage.RData")
```
Discussion points

# Plot all treatment effect estimates across endpoints

```r
# Order
result_df$variable <- factor(result_df$variable, 
                             levels = c("AEs grade 3,4 within 28 days",
                                        "Any AE grade 3,4 within 28 days",
                                        "viral clearance until day 15",
                                        "viral clearance until day 10",
                                        "viral clearance until day 5",
                                        "sustained discharge within 28 days",
                                        "discharge within 28 days, death=censored",
                                        "discharge within 28 days, death=hypo.event",
                                        "discharge within 28 days, death=comp.event",
                                        "clinical status at day 28",
                                        "new MV within 28d",
                                        "new MV or death within 28d",
                                        "death within fup",
                                        "death at day 60",
                                        "death at day 28"))

# Plotting
result_df$truncated <- ifelse(result_df$ci_upper > 2.0, TRUE, FALSE)  # Truncate at upper CI 2.0, and add arrow for those
ggplot(result_df, aes(x = variable, y = hazard_odds_ratio)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = pmin(ci_upper, 2.0)), width = 0.5) +
  geom_segment(data = subset(result_df, truncated), 
               aes(x = variable, xend = variable, y = pmin(ci_upper, 2.0), yend = pmin(ci_upper, 2.0) + 0.1),
               arrow = arrow(length = unit(0.3, "cm")), color = "black") +
  geom_hline(yintercept = 1, linetype = "dotted", color = "red", size = 0.5) +
  labs(title = "All endpoint results - two-stage",
       x = "Endpoints",
       y = "aOR/aHR/aIRR") +
  theme_minimal() +
  coord_flip()
```

```
## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
## ℹ Please use `linewidth` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

![](two_stage_files/figure-html/unnamed-chunk-20-1.png)<!-- -->


# TREATMENT-COVARIATE INTERACTIONS
# Load treatment-covariate interaction estimates from all trials (on primary endpoint - and vacc.ae)

```r
df_int_barisolidact <- readRDS("int_effects_barisolidact.RData")
df_int_actt2 <- readRDS("int_effects_actt2.RData")
df_int_ghazaeian <- readRDS("int_effects_ghazaeian.RData")
df_int_tofacov <- readRDS("int_effects_tofacov.RData")
df_int_covinib <- readRDS("int_effects_covinib.RData")
df_int_covbarrier <- readRDS("int_effects_cov-barrier.RData")
# df_int_murugesan <- readRDS("int_effects_murugesan.RData")
```

# Load subgroup effects from all trials (on primary endpoint - and vacc.ae)

```r
df_subgroup_actt2 <- readRDS("subgroup_effects_ACTT2.RData")
df_subgroup_covbarrier <- readRDS("subgroup_effects_cov-barrier.RData")
```

# Reshape dataframes for all treatment-covariate interaction estimates (on primary endpoint - and vacc.ae)

```r
### Create a list of all data frames / trials
list_int_df <- list(df_int_barisolidact, df_int_actt2, df_int_ghazaeian, df_int_tofacov, df_int_covinib, df_int_covbarrier) # add all trials

## Respiratory support on Mortality at day 28
outcomes <- "respiratory support"
outcomes.firth <- "respiratory support_firth" # depends on which estimates to include
# Initialize an empty data frame to store the selected rows
df_rs_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_int_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_rs_mort28 <- rbind(df_rs_mort28, selected_rows)
}

## Ventilation on Mortality at day 28
outcomes <- "ventilation"
outcomes.firth <- "ventilation_firth" # depends on which estimates to include
# Initialize an empty data frame to store the selected rows
df_vb_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_int_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_vb_mort28 <- rbind(df_vb_mort28, selected_rows)
}

## Age on Mortality at day 28
outcomes <- "age"
outcomes.firth <- "age_firth" # depends on which estimates to include
# Initialize an empty data frame to store the selected rows
df_age_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_int_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_age_mort28 <- rbind(df_age_mort28, selected_rows)
}

## Comorbidity on Mortality at day 28
outcomes <- "comorbidity"
outcomes.firth <- "comorbidity_firth" # depends on which estimates to include
# Initialize an empty data frame to store the selected rows
df_comorb_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_int_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_comorb_mort28 <- rbind(df_comorb_mort28, selected_rows)
}

## Comedication on Mortality at day 28
outcomes <- "comedication"
outcomes.firth <- "comedication_firth" # depends on which estimates to include
# Initialize an empty data frame to store the selected rows
df_comed_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_int_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_comed_mort28 <- rbind(df_comed_mort28, selected_rows)
}

## Vacc on AEs
outcomes <- "vaccination on AEs"
outcomes.firth <- "vaccination on AEs_firth" # depends on which estimates to include
# Initialize an empty data frame to store the selected rows
df_vacc_ae28 <- data.frame()
# Loop through the list of data frames
for (df in list_int_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_vacc_ae28 <- rbind(df_vacc_ae28, selected_rows)
}

## Symptom duration on Mortality at day 28
outcomes <- "symptom duration"
outcomes.firth <- "symptom duration_firth" # depends on which estimates to include
# Initialize an empty data frame to store the selected rows
df_symp_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_int_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_symp_mort28 <- rbind(df_symp_mort28, selected_rows)
}

## CRP on Mortality at day 28
outcomes <- "crp"
outcomes.firth <- "crp_firth" # depends on which estimates to include
# Initialize an empty data frame to store the selected rows
df_crp_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_int_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_crp_mort28 <- rbind(df_crp_mort28, selected_rows)
}
```

# Reshape dataframes for all subgroup estimates (on primary endpoint) // if needed

```r
# start a data frame to save interaction estimates
df_subgroup_actt2_title <- data.frame(variable = c("ACTT2"))
df_subgroup_actt2_title$hazard_odds_ratio <- NA
df_subgroup_actt2_title$ci_lower <- NA
df_subgroup_actt2_title$ci_upper <- NA
df_subgroup_actt2_title$standard_error <- NA
df_subgroup_actt2_title$p_value <- NA
df_subgroup_actt2_title$n_intervention <- NA
df_subgroup_actt2_title$n_intervention_tot <- NA
df_subgroup_actt2_title$n_control <- NA
df_subgroup_actt2_title$n_control_tot <- NA
df_subgroup_actt2_title$trial <- NA
df_subgroup_actt2_title$JAKi <- NA
df_subgroup_covbarrier_title <- data.frame(variable = c("COV-BARRIER"))
df_subgroup_covbarrier_title$hazard_odds_ratio <- NA
df_subgroup_covbarrier_title$ci_lower <- NA
df_subgroup_covbarrier_title$ci_upper <- NA
df_subgroup_covbarrier_title$standard_error <- NA
df_subgroup_covbarrier_title$p_value <- NA
df_subgroup_covbarrier_title$n_intervention <- NA
df_subgroup_covbarrier_title$n_intervention_tot <- NA
df_subgroup_covbarrier_title$n_control <- NA
df_subgroup_covbarrier_title$n_control_tot <- NA
df_subgroup_covbarrier_title$trial <- NA
df_subgroup_covbarrier_title$JAKi <- NA

df_subgroup_tot <- rbind(df_subgroup_actt2_title, df_subgroup_actt2, df_subgroup_covbarrier_title, df_subgroup_covbarrier)

# ## Ventilation on Mortality at day 28
# outcomes <- "ventilation"
# outcomes.firth <- "ventilation_firth" # depends on which estimates to include
# # Initialize an empty data frame to store the selected rows
# df_vb_mort28 <- data.frame()
# # Loop through the list of data frames
# for (df in list_subgroup_df) {
#   selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
#   df_vb_mort28 <- rbind(df_vb_mort28, selected_rows)
# }
```

# Interaction: Respiratory support (proxy for disease severity) on primary endpoint

```r
str(df_rs_mort28)
```

```
## 'data.frame':	6 obs. of  8 variables:
##  $ variable      : chr  "respiratory support" "respiratory support" "respiratory support_firth" "respiratory support_firth" ...
##  $ log_odds_ratio: num  0.22 1.859 1 1.188 0.254 ...
##  $ ci_lower      : num  0.022524 0.956064 0.001618 0.003153 0.000502 ...
##  $ ci_upper      : num  1.54 3.73 617.98 689.75 80.69 ...
##  $ standard_error: num  1.0498 0.3453 0.0328 2.4621 2.4394 ...
##  $ p_value       : num  0.149 0.0726 1 0.9488 0.5913 ...
##  $ trial         : chr  "Bari-SolidAct" "ACTT-2" "Ghazaeian" "TOFACOV" ...
##  $ JAKi          : chr  "Baricitinib" "Baricitinib" "Tofacitinib" "Tofacitinib" ...
```

```r
## "TOFACOV", "COVINIB", "Ghazaeian" do only have events in 1 subgroup -> see deft: do not include! "(insufficient data)"
rs.mort28 <- metagen(TE = log(log_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_rs_mort28,
                      # n.e = n_intervention + n_control,
                      # n.c = n_control,
                      sm = "log(Ratio of OR)",
                      fixed = T, # the true interaction is assumed the same in all trials 
                      random = T, # the true interactions are assumed random across trials
                      method.tau = "REML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "se", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Treatment-covariate interaction on primary endpoint: Respiratory support",
                      subset = trial %in% c("COV-BARRIER", "ACTT-2", "Bari-SolidAct"),
                      # exclude = trial %in% c("TOFACOV", "COVINIB", "Ghazaeian") # incl in plot but exclude from analysis
                      )
summary(rs.mort28)
```

```
## Review:     Treatment-covariate interaction on primary endpoint: Respiratory ...
## 
##               log(Ratio of OR)            95%-CI %W(common) %W(random)
## Bari-SolidAct          -1.5148 [-3.5724; 0.5427]        3.1       11.2
## ACTT-2                  0.6198 [-0.0569; 1.2965]       28.6       40.0
## COV-BARRIER            -0.1065 [-0.5443; 0.3312]       68.3       48.8
## 
## Number of studies: k = 3
## 
##                              log(Ratio of OR)            95%-CI  z|t p-value
## Common effect model                    0.0576 [-0.3042; 0.4194] 0.31  0.7552
## Random effects model (HK-SE)           0.0256 [-1.9374; 1.9886] 0.06  0.9603
## 
## Quantifying heterogeneity:
##  tau^2 = 0.2650 [0.0000; 45.8270]; tau = 0.5147 [0.0000; 6.7696]
##  I^2 = 63.2% [0.0%; 89.5%]; H = 1.65 [1.00; 3.08]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  5.44    2  0.0660
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 2)
```

```r
forest.meta(rs.mort28,
            # hetstat = F,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE"),
            leftlabs = c("Trial", "log(Ratio of OR)", "Standard Error"),
            text.common = "Average interaction effect (common effect model)*",
            text.random = "Average interaction effect (random effect model)*",
            title = "Treatment-covariate interaction on primary endpoint: Respiratory support",
            # xlim = c(0.15,5),
            xlab = "95% CI for interaction effect"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-25-1.png)<!-- -->
Discussion points

# Interaction: Ventilation requirement (proxy for disease severity) on primary endpoint

```r
str(df_vb_mort28)
```

```
## 'data.frame':	3 obs. of  8 variables:
##  $ variable      : chr  "ventilation_firth" "ventilation" "ventilation"
##  $ log_odds_ratio: num  0.778 2.437 0.809
##  $ ci_lower      : num  8.81e-13 7.21e-01 4.22e-01
##  $ ci_upper      : num  1.65 9.16 1.56
##  $ standard_error: num  0.0712 0.6395 0.3322
##  $ p_value       : num  0.249 0.164 0.524
##  $ trial         : chr  "Bari-SolidAct" "ACTT-2" "COV-BARRIER"
##  $ JAKi          : chr  "Baricitinib" "Baricitinib" "Baricitinib"
```

```r
## "TOFACOV", "COVINIB", "Ghazaeian" and "Bari-SolidAct" do only have events in 1 subgroup -> see deft: do not include! "(insufficient data)"
vb.mort28 <- metagen(TE = log(log_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_vb_mort28,
                      # n.e = n_intervention + n_control,
                      # n.c = n_control,
                      sm = "log(Ratio of OR)",
                      fixed = T, # the true interaction is assumed the same in all trials 
                      random = T, # the true interactions are assumed random across trials
                      method.tau = "REML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "se", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Treatment-covariate interaction on primary endpoint: Ventilation requirement",
                      subset = trial %in% c("COV-BARRIER", "ACTT-2"),
                      # exclude = trial %in% c("TOFACOV", "COVINIB", "Ghazaeian") # incl in plot but exclude from analysis
                      )
summary(vb.mort28)
```

```
## Review:     Treatment-covariate interaction on primary endpoint: Ventilation ...
## 
##             log(Ratio of OR)            95%-CI %W(common) %W(random)
## ACTT-2                0.8907 [-0.3627; 2.1442]       21.3       37.7
## COV-BARRIER          -0.2119 [-0.8630; 0.4392]       78.7       62.3
## 
## Number of studies: k = 2
## 
##                              log(Ratio of OR)            95%-CI  z|t p-value
## Common effect model                    0.0224 [-0.5554; 0.6002] 0.08  0.9393
## Random effects model (HK-SE)           0.2040 [-6.5866; 6.9947] 0.38  0.7678
## 
## Quantifying heterogeneity:
##  tau^2 = 0.3482; tau = 0.5901; I^2 = 57.3% [0.0%; 89.8%]; H = 1.53 [1.00; 3.13]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  2.34    1  0.1260
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Hartung-Knapp adjustment for random effects model (df = 1)
```

```r
# Open a pdf file
# pdf("vb_mort28.pdf", width=11, height=4)
forest.meta(vb.mort28,
            hetstat = F,
            leftcols = c("studlab"),
            leftlabs = c("Trial"),
            text.common = "Average interaction effect (common effect model)*",
            text.random = "Average interaction effect (random effect model)*",
            title = "Treatment-covariate interaction on primary endpoint: Ventilation requirement",
            xlab = "more effect: ventilated <-> more effect: not ventilated",
            xlab.pos = 0.7,
            fs.xlab = 11
            )
```

![](two_stage_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

```r
# dev.off()
```
Discussion points

# Subgroups: Ventilation requirement (proxy for disease severity) on primary endpoint

```r
base_data <- tibble(mean = df_subgroup_tot$hazard_odds_ratio, 
                    lower = df_subgroup_tot$ci_lower,
                    upper = df_subgroup_tot$ci_upper,
                    subgroup = df_subgroup_tot$variable)
header <- tibble(subgroup = c("Treatment effects by trial and subgroup (ventilation)"),
                 summary = TRUE)
vb_mort28_fp <- bind_rows(header,base_data)
font <- "sans"

# Open a pdf file
# pdf("vb_mort28_fp.pdf", width=10, height=4)
vb_mort28_fp %>% 
  forestplot(labeltext = c(subgroup),
             txt_gp = fpTxtGp(label = gpar(fontfamily = font, cex=1),
                              ticks = gpar(cex=1),
                              xlab = gpar(cex=1)),
             is.summary = summary,
             graph.pos = 2,
             clip = c(0.1, 2), 
             hrzl_lines = list("2" = gpar(lty = 2), 
                               "3" = gpar(lty = 2),
                               "5" = gpar(lty = 2), 
                               "6" = gpar(lty = 2)),
             xlog = FALSE,
             xticks = c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2),
             boxsize = 0.22,
             lty.ci = c(1),
             col = fpColors(box = "maroon4",
                            line = "maroon1",
                            hrz_lines = "gray63"),
             vertices = TRUE,
             xlab = "     Favours JAKi <-> Favours No JAKi",
             zero = 1,
             graphwidth = unit(100, "mm"), colgap = unit(2.5, "mm")
             )
```

![](two_stage_files/figure-html/unnamed-chunk-27-1.png)<!-- -->

```r
# dev.off()
```
Discussion points

# Interaction: Age on primary endpoint

```r
str(df_age_mort28)
```

```
## 'data.frame':	6 obs. of  8 variables:
##  $ variable      : chr  "age" "age" "age" "age_firth" ...
##  $ log_odds_ratio: num  1.026 0.993 1.05 1.047 0.869 ...
##  $ ci_lower      : num  0.954 0.952 0.951 0.661 0.551 ...
##  $ ci_upper      : num  1.11 1.04 1.18 1.67 1.54 ...
##  $ standard_error: num  0.0377 0.0212 0.0531 0.0973 0.1064 ...
##  $ p_value       : num  0.498 0.732 0.354 0.775 0.422 ...
##  $ trial         : chr  "Bari-SolidAct" "ACTT-2" "Ghazaeian" "TOFACOV" ...
##  $ JAKi          : chr  "Baricitinib" "Baricitinib" "Tofacitinib" "Tofacitinib" ...
```

```r
age.mort28 <- metagen(TE = log(log_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_age_mort28,
                      # n.e = n_intervention + n_control,
                      # n.c = n_control,
                      sm = "log(Ratio of OR)",
                      fixed = T, # the true interaction is assumed the same in all trials 
                      random = T, # the true interactions are assumed random across trials
                      method.tau = "REML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "se", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Treatment-covariate interaction on primary endpoint: Age",
                      # subset = trial %in% c("COV-BARRIER", "ACTT-2"),
                      # exclude = trial %in% c("TOFACOV", "COVINIB", "Ghazaeian") # incl in plot but exclude from analysis
                      )
summary(age.mort28)
```

```
## Review:     Treatment-covariate interaction on primary endpoint: Age
## 
##               log(Ratio of OR)            95%-CI %W(common) %W(random)
## Bari-SolidAct           0.0255 [-0.0483; 0.0993]        8.3        8.4
## ACTT-2                 -0.0073 [-0.0488; 0.0343]       26.2       26.3
## Ghazaeian               0.0492 [-0.0549; 0.1534]        4.2        4.2
## TOFACOV                 0.0460 [-0.1448; 0.2368]        1.2        1.3
## COVINIB                -0.1400 [-0.3486; 0.0686]        1.0        1.1
## COV-BARRIER             0.0228 [-0.0049; 0.0505]       59.0       58.7
## 
## Number of studies: k = 6
## 
##                              log(Ratio of OR)            95%-CI  z|t p-value
## Common effect model                    0.0149 [-0.0064; 0.0361] 1.37  0.1713
## Random effects model (HK-SE)           0.0148 [-0.0133; 0.0429] 1.36  0.2331
## 
## Quantifying heterogeneity:
##  tau^2 < 0.0001 [0.0000; 0.0209]; tau = 0.0019 [0.0000; 0.1445]
##  I^2 = 0.0% [0.0%; 74.6%]; H = 1.00 [1.00; 1.99]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  4.12    5  0.5321
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 5)
```

```r
forest.meta(age.mort28,
            # hetstat = F,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE"),
            leftlabs = c("Trial", "log(Ratio of OR)", "Standard Error"),
            text.common = "Average interaction effect (common effect model)*",
            text.random = "Average interaction effect (random effect model)*",
            title = "Treatment-covariate interaction on primary endpoint: Age",
            # xlim = c(0.15,5),
            xlab = "95% CI for interaction effect"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-28-1.png)<!-- -->
Discussion points

# Interaction: Comorbidity on primary endpoint

```r
str(df_comorb_mort28)
```

```
## 'data.frame':	6 obs. of  8 variables:
##  $ variable      : chr  "comorbidity" "comorbidity" "comorbidity" "comorbidity_firth" ...
##  $ log_odds_ratio: num  1.257 0.818 1.914 0.606 1.499 ...
##  $ ci_lower      : num  0.50767 0.34036 0.32782 0.00546 0.02422 ...
##  $ ci_upper      : num  3.26 1.96 16.49 88.27 6641.84 ...
##  $ standard_error: num  0.468 0.444 0.95 1.381 1.261 ...
##  $ p_value       : num  0.625 0.65 0.494 0.777 0.789 ...
##  $ trial         : chr  "Bari-SolidAct" "ACTT-2" "Ghazaeian" "TOFACOV" ...
##  $ JAKi          : chr  "Baricitinib" "Baricitinib" "Tofacitinib" "Tofacitinib" ...
```

```r
comorb.mort28 <- metagen(TE = log(log_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_comorb_mort28,
                      # n.e = n_intervention + n_control,
                      # n.c = n_control,
                      sm = "log(Ratio of OR)",
                      fixed = T, # the true interaction is assumed the same in all trials 
                      random = T, # the true interactions are assumed random across trials
                      method.tau = "REML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "se", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Treatment-covariate interaction on primary endpoint: Comorbidity",
                      # subset = trial %in% c("COV-BARRIER", "ACTT-2"),
                      # exclude = trial %in% c("TOFACOV", "COVINIB", "Ghazaeian") # incl in plot but exclude from analysis
                      )
summary(comorb.mort28)
```

```
## Review:     Treatment-covariate interaction on primary endpoint: Comorbidity
## 
##               log(Ratio of OR)            95%-CI %W(common) %W(random)
## Bari-SolidAct           0.2290 [-0.6881; 1.1460]       15.3       15.3
## ACTT-2                 -0.2010 [-1.0703; 0.6683]       17.0       17.0
## Ghazaeian               0.6492 [-1.2128; 2.5111]        3.7        3.7
## TOFACOV                -0.5007 [-3.2076; 2.2062]        1.8        1.8
## COVINIB                 0.4050 [-2.0656; 2.8757]        2.1        2.1
## COV-BARRIER            -0.1323 [-0.5946; 0.3300]       60.1       60.1
## 
## Number of studies: k = 6
## 
##                              log(Ratio of OR)            95%-CI   z|t p-value
## Common effect model                   -0.0550 [-0.4135; 0.3035] -0.30  0.7638
## Random effects model (HK-SE)          -0.0550 [-0.5251; 0.4152] -0.30  0.7759
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.2470]; tau = 0 [0.0000; 0.4970]
##  I^2 = 0.0% [0.0%; 74.6%]; H = 1.00 [1.00; 1.99]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  1.37    5  0.9275
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 5)
```

```r
forest.meta(comorb.mort28,
            # hetstat = F,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE"),
            leftlabs = c("Trial", "log(Ratio of OR)", "Standard Error"),
            text.common = "Average interaction effect (common effect model)*",
            text.random = "Average interaction effect (random effect model)*",
            title = "Treatment-covariate interaction on primary endpoint: Comorbidity",
            # xlim = c(0.15,5),
            xlab = "95% CI for interaction effect"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-29-1.png)<!-- -->
Discussion points

# Interaction: Comedication on primary endpoint

```r
str(df_comed_mort28)
```

```
## 'data.frame':	5 obs. of  8 variables:
##  $ variable      : chr  "comedication_firth" "comedication_firth" "comedication_firth" "comedication_firth" ...
##  $ log_odds_ratio: num  1.715 0.994 1.901 1.361 1.216
##  $ ci_lower      : num  0.337293 0.000715 0.09537 0.073967 0.762656
##  $ ci_upper      : num  22.15 7.84 48.5 31.44 1.99
##  $ standard_error: num  0.8879 0.0342 1.2141 1.2146 0.2431
##  $ p_value       : num  0.526 0.951 0.636 0.813 0.421
##  $ trial         : chr  "Bari-SolidAct" "Ghazaeian" "TOFACOV" "COVINIB" ...
##  $ JAKi          : chr  "Baricitinib" "Tofacitinib" "Tofacitinib" "Baricitinib" ...
```

```r
## "Ghazaeian" do only have events in 1 subgroup -> see deft: do not include! "(insufficient data)"
comed.mort28 <- metagen(TE = log(log_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_comed_mort28,
                      # n.e = n_intervention + n_control,
                      # n.c = n_control,
                      sm = "log(Ratio of OR)",
                      fixed = T, # the true interaction is assumed the same in all trials 
                      random = T, # the true interactions are assumed random across trials
                      method.tau = "REML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "se", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Treatment-covariate interaction on primary endpoint: Comedication",
                      subset = trial %in% c("COV-BARRIER", "Bari-SolidAct", "ACTT-2", "TOFACOV", "COVINIB"),
                      # exclude = trial %in% c("TOFACOV", "COVINIB", "Ghazaeian") # incl in plot but exclude from analysis
                      )
summary(comed.mort28)
```

```
## Review:     Treatment-covariate interaction on primary endpoint: Comedication
## 
##               log(Ratio of OR)            95%-CI %W(common) %W(random)
## Bari-SolidAct           0.5395 [-1.2009; 2.2798]        6.5        6.5
## TOFACOV                 0.6425 [-1.7371; 3.0221]        3.5        3.5
## COVINIB                 0.3084 [-2.0723; 2.6890]        3.5        3.5
## COV-BARRIER             0.1954 [-0.2810; 0.6719]       86.6       86.6
## 
## Number of studies: k = 4
## 
##                              log(Ratio of OR)            95%-CI  z|t p-value
## Common effect model                    0.2372 [-0.2061; 0.6806] 1.05  0.2943
## Random effects model (HK-SE)           0.2372 [-0.4827; 0.9571] 1.05  0.3714
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.0755]; tau = 0 [0.0000; 0.2747]
##  I^2 = 0.0% [0.0%; 84.7%]; H = 1.00 [1.00; 2.56]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  0.26    3  0.9673
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 3)
```

```r
forest.meta(comed.mort28,
            # hetstat = F,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE"),
            leftlabs = c("Trial", "log(Ratio of OR)", "Standard Error"),
            text.common = "Average interaction effect (common effect model)*",
            text.random = "Average interaction effect (random effect model)*",
            title = "Treatment-covariate interaction on primary endpoint: Comedication",
            # xlim = c(0.15,5),
            xlab = "95% CI for interaction effect"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-30-1.png)<!-- -->
Discussion points

# Interaction: Vaccination on AEs

```r
str(df_vacc_ae28)
```

```
## 'data.frame':	2 obs. of  8 variables:
##  $ variable      : chr  "vaccination on AEs_firth" "vaccination on AEs_firth"
##  $ log_odds_ratio: num  2.65 1
##  $ ci_lower      : num  9.71e-03 5.63e-205
##  $ ci_upper      : num  7.65e+02 1.77e+204
##  $ standard_error: num  2.36 2.4
##  $ p_value       : num  0.684 1
##  $ trial         : chr  "TOFACOV" "COVINIB"
##  $ JAKi          : chr  "Tofacitinib" "Baricitinib"
```

```r
vacc.ae28 <- metagen(TE = log(log_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_vacc_ae28,
                      # n.e = n_intervention + n_control,
                      # n.c = n_control,
                      sm = "log(Ratio of OR)",
                      fixed = T, # the true interaction is assumed the same in all trials 
                      random = T, # the true interactions are assumed random across trials
                      method.tau = "REML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "se", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Treatment-covariate interaction on AEs: vaccination",
                      # subset = trial %in% c("COV-BARRIER", "Bari-SolidAct", "ACTT-2", "TOFACOV", "COVINIB"),
                      # exclude = trial %in% c("TOFACOV", "COVINIB", "Ghazaeian") # incl in plot but exclude from analysis
                      )
summary(vacc.ae28)
```

```
## Review:     Treatment-covariate interaction on AEs: vaccination
## 
##         log(Ratio of OR)            95%-CI %W(common) %W(random)
## TOFACOV           0.9745 [-3.6477; 5.5967]       50.9       50.9
## COVINIB           0.0000 [-4.7030; 4.7030]       49.1       49.1
## 
## Number of studies: k = 2
## 
##                              log(Ratio of OR)              95%-CI  z|t p-value
## Common effect model                    0.4957 [ -2.8009;  3.7923] 0.29  0.7682
## Random effects model (HK-SE)           0.4957 [-20.8757; 21.8671] 0.29  0.8175
## 
## Quantifying heterogeneity:
##  tau^2 = 0; tau = 0; I^2 = 0.0%; H = 1.00
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  0.08    1  0.7721
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Hartung-Knapp adjustment for random effects model (df = 1)
```

```r
forest.meta(vacc.ae28,
            # hetstat = F,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE"),
            leftlabs = c("Trial", "log(Ratio of OR)", "Standard Error"),
            text.common = "Average interaction effect (common effect model)*",
            text.random = "Average interaction effect (random effect model)*",
            title = "Treatment-covariate interaction on AEs: vaccination",
            # xlim = c(0.15,5),
            xlab = "95% CI for interaction effect"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-31-1.png)<!-- -->
Discussion points

# Interaction: Symptom onset on primary endpoint

```r
str(df_symp_mort28)
```

```
## 'data.frame':	6 obs. of  8 variables:
##  $ variable      : chr  "symptom duration" "symptom duration" "symptom duration" "symptom duration_firth" ...
##  $ log_odds_ratio: num  0.796 0.949 1.196 0.824 1.286 ...
##  $ ci_lower      : num  0.6279 0.8225 0.6407 0.3883 0.0609 ...
##  $ ci_upper      : num  1 1.08 2.36 3.74 41.95 ...
##  $ standard_error: num  0.1177 0.0703 0.3233 0.259 0.5147 ...
##  $ p_value       : num  0.0531 0.4544 0.5804 0.6358 0.7423 ...
##  $ trial         : chr  "Bari-SolidAct" "ACTT-2" "Ghazaeian" "TOFACOV" ...
##  $ JAKi          : chr  "Baricitinib" "Baricitinib" "Tofacitinib" "Tofacitinib" ...
```

```r
## "Ghazaeian" do only have events in 1 subgroup -> see deft: do not include! "(insufficient data)"
symp.mort28 <- metagen(TE = log(log_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_symp_mort28,
                      # n.e = n_intervention + n_control,
                      # n.c = n_control,
                      sm = "log(Ratio of OR)",
                      fixed = T, # the true interaction is assumed the same in all trials 
                      random = T, # the true interactions are assumed random across trials
                      method.tau = "REML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "se", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Treatment-covariate interaction on primary endpoint: Symptom duration",
                      # subset = trial %in% c("COV-BARRIER", "Bari-SolidAct", "ACTT-2", "TOFACOV", "COVINIB"),
                      # exclude = trial %in% c("TOFACOV", "COVINIB", "Ghazaeian") # incl in plot but exclude from analysis
                      )
summary(symp.mort28)
```

```
## Review:     Treatment-covariate interaction on primary endpoint: Symptom dur ...
## 
##               log(Ratio of OR)            95%-CI %W(common) %W(random)
## Bari-SolidAct          -0.2277 [-0.4584; 0.0030]        6.1       17.2
## ACTT-2                 -0.0526 [-0.1903; 0.0852]       17.1       29.9
## Ghazaeian               0.1787 [-0.4549; 0.8123]        0.8        3.2
## TOFACOV                -0.1941 [-0.7017; 0.3135]        1.3        4.9
## COVINIB                 0.2516 [-0.7571; 1.2604]        0.3        1.3
## COV-BARRIER             0.0457 [-0.0205; 0.1118]       74.4       43.5
## 
## Number of studies: k = 6
## 
##                              log(Ratio of OR)            95%-CI   z|t p-value
## Common effect model                    0.0108 [-0.0462; 0.0679]  0.37  0.7092
## Random effects model (HK-SE)          -0.0353 [-0.1897; 0.1192] -0.59  0.5828
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0071 [0.0000; 0.1325]; tau = 0.0846 [0.0000; 0.3639]
##  I^2 = 29.6% [0.0%; 71.2%]; H = 1.19 [1.00; 1.86]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  7.10    5  0.2132
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 5)
```

```r
forest.meta(symp.mort28,
            # hetstat = F,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE"),
            leftlabs = c("Trial", "log(Ratio of OR)", "Standard Error"),
            text.common = "Average interaction effect (common effect model)*",
            text.random = "Average interaction effect (random effect model)*",
            title = "Treatment-covariate interaction on primary endpoint: Symptom duration",
            # xlim = c(0.15,5),
            xlab = "95% CI for interaction effect"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-32-1.png)<!-- -->
Discussion points

# Interaction: CRP on primary endpoint

```r
str(df_crp_mort28)
```

```
## 'data.frame':	5 obs. of  8 variables:
##  $ variable      : chr  "crp" "crp" "crp_firth" "crp_firth" ...
##  $ log_odds_ratio: num  1 0.997 0.989 1.02 1.001
##  $ ci_lower      : num  0.995 0.955 0.483 0.907 0.998
##  $ ci_upper      : num  1 1.03 1.05 1.11 1
##  $ standard_error: num  0.00162 0.01962 0.02457 0.01563 0.00157
##  $ p_value       : num  0.974 0.864 0.695 0.394 0.604
##  $ trial         : chr  "Bari-SolidAct" "Ghazaeian" "TOFACOV" "COVINIB" ...
##  $ JAKi          : chr  "Baricitinib" "Tofacitinib" "Tofacitinib" "Baricitinib" ...
```

```r
## "Ghazaeian" do only have events in 1 subgroup -> see deft: do not include! "(insufficient data)"
crp.mort28 <- metagen(TE = log(log_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_crp_mort28,
                      # n.e = n_intervention + n_control,
                      # n.c = n_control,
                      sm = "log(Ratio of OR)",
                      fixed = T, # the true interaction is assumed the same in all trials 
                      random = T, # the true interactions are assumed random across trials
                      method.tau = "REML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "se", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Treatment-covariate interaction on primary endpoint: CRP",
                      # subset = trial %in% c("COV-BARRIER", "Bari-SolidAct", "ACTT-2", "TOFACOV", "COVINIB"),
                      # exclude = trial %in% c("TOFACOV", "COVINIB", "Ghazaeian") # incl in plot but exclude from analysis
                      )
summary(crp.mort28)
```

```
## Review:     Treatment-covariate interaction on primary endpoint: CRP
## 
##               log(Ratio of OR)            95%-CI %W(common) %W(random)
## Bari-SolidAct           0.0001 [-0.0031; 0.0032]       47.9       47.9
## Ghazaeian              -0.0034 [-0.0418; 0.0351]        0.3        0.3
## TOFACOV                -0.0108 [-0.0589; 0.0374]        0.2        0.2
## COVINIB                 0.0202 [-0.0104; 0.0508]        0.5        0.5
## COV-BARRIER             0.0008 [-0.0023; 0.0039]       51.0       51.0
## 
## Number of studies: k = 5
## 
##                              log(Ratio of OR)            95%-CI  z|t p-value
## Common effect model                    0.0005 [-0.0017; 0.0027] 0.46  0.6487
## Random effects model (HK-SE)           0.0005 [-0.0026; 0.0036] 0.46  0.6723
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.0008]; tau = 0 [0.0000; 0.0276]
##  I^2 = 0.0% [0.0%; 79.2%]; H = 1.00 [1.00; 2.19]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  1.96    4  0.7438
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 4)
```

```r
forest.meta(crp.mort28,
            # hetstat = F,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE"),
            leftlabs = c("Trial", "log(Ratio of OR)", "Standard Error"),
            text.common = "Average interaction effect (common effect model)*",
            text.random = "Average interaction effect (random effect model)*",
            title = "Treatment-covariate interaction on primary endpoint: CRP",
            # xlim = c(0.15,5),
            xlab = "95% CI for interaction effect"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-33-1.png)<!-- -->
Discussion points

# Collect all interaction effect estimates

```r
# Empty data frame to store the results
interaction_df <- data.frame(
  variable = character(),
  log_odds_ratio = numeric(),
  ci_lower = numeric(),
  ci_upper = numeric(),
  standard_error = numeric(),
  p_value = numeric()
)

# Function to extract treatment results from different model types
extract_interaction <- function(model, variable_name) {
  if (inherits(model, "metagen")) {
    log_odds_ratio <- exp(summary(model)$TE.random)
    ci.lower <- exp(summary(model)$lower.random)
    ci.upper <- exp(summary(model)$upper.random)
    se <- summary(model)$seTE.random
    p_value <- summary(model)$pval.random
  } else {
    stop("Unsupported model class")
  }
  # capture the results
  result <- data.frame(
    variable = variable_name,
    log_odds_ratio = round(log_odds_ratio,3),
    ci_lower = round(ci.lower,3),
    ci_upper = round(ci.upper,3),
    standard_error = round(se,3),
    p_value = round(p_value,3)
  )
  return(result)
}

# Loop through
result_list <- list()

result_list[[1]] <- extract_interaction(rs.mort28, "respiratory support") 
result_list[[2]] <- extract_interaction(vb.mort28, "ventilation") 
result_list[[3]] <- extract_interaction(age.mort28, "age")
result_list[[4]] <- extract_interaction(comorb.mort28, "comorbidity") 
result_list[[5]] <- extract_interaction(comed.mort28, "comedication")
result_list[[6]] <- extract_interaction(vacc.ae28, "vaccination on AEs") 
result_list[[7]] <- extract_interaction(symp.mort28, "symptom duration") 
result_list[[8]] <- extract_interaction(crp.mort28, "crp") 

# Filter out NULL results and bind the results into a single data frame
interaction_df <- do.call(rbind, Filter(function(x) !is.null(x), result_list))

# Add the analysis approach
interaction_df$approach <- "two-stage"

# Nicely formatted table
kable(interaction_df, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|variable            | log_odds_ratio| ci_lower|     ci_upper| standard_error| p_value|approach  |
|:-------------------|--------------:|--------:|------------:|--------------:|-------:|:---------|
|respiratory support |          1.026|    0.144| 7.306000e+00|          0.456|   0.960|two-stage |
|ventilation         |          1.226|    0.001| 1.090810e+03|          0.534|   0.768|two-stage |
|age                 |          1.015|    0.987| 1.044000e+00|          0.011|   0.233|two-stage |
|comorbidity         |          0.947|    0.591| 1.515000e+00|          0.183|   0.776|two-stage |
|comedication        |          1.268|    0.617| 2.604000e+00|          0.226|   0.371|two-stage |
|vaccination on AEs  |          1.642|    0.000| 3.138722e+09|          1.682|   0.818|two-stage |
|symptom duration    |          0.965|    0.827| 1.127000e+00|          0.060|   0.583|two-stage |
|crp                 |          1.001|    0.997| 1.004000e+00|          0.001|   0.672|two-stage |

```r
# Save
saveRDS(interaction_df, file = "int_effects_two-stage.RData")
```

# Interactions: Multivariate IPD Meta-Analysis for Summarising Non-linear Interactions
