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
```

# Load treatment effect estimates from all trials


# Reshape dataframes for each outcome

```r
### Create a list of all data frames / trials
list_df <- list(df_barisolidact, df_actt2, df_ghazaeian, df_tofacov, df_covinib) # add all trials


## Mortality at day 28
outcome <- "death at day 28"
outcome2 <- "death at day 28_firth" # depends on which estimates to include
# Initialize an empty data frame to store the selected rows
df_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_df) {
  selected_rows <- df %>% filter(variable == outcome | variable == outcome2)
  df_mort28 <- rbind(df_mort28, selected_rows)
}
# Reset row names
rownames(df_mort28) <- NULL

## Mortality at day 60
outcome <- "death at day 60"
outcome2 <- "death at day 60_firth"
df_mort60 <- data.frame()
for (df in list_df) {
  selected_rows <- df %>% filter(variable == outcome | variable == outcome2)
  df_mort60 <- rbind(df_mort60, selected_rows)
}
rownames(df_mort60) <- NULL
```

# (i) Primary outcome: Mortality at day 28

```r
str(df_mort28)
```

```
## 'data.frame':	5 obs. of  10 variables:
##  $ variable         : chr  "death at day 28" "death at day 28" "death at day 28" "death at day 28_firth" ...
##  $ hazard_odds_ratio: num  0.656 0.704 0.791 2.537 0.182
##  $ ci_lower         : num  0.30018 0.39577 0.14728 0.12715 0.00131
##  $ ci_upper         : num  1.4 1.24 3.83 380.13 2.29
##  $ standard_error   : num  0.391 0.289 0.798 1.315 1.361
##  $ p_value          : num  0.281 0.225 0.769 NA NA
##  $ n_intervention   : num  137 494 46 58 53
##  $ n_control        : num  140 492 51 58 54
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
                      method.tau = "REML", # no difference with Paule-Mandel
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "se", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Average treatment effect - mortality 28 days",
                      # subset = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # exclude entirely
                      # exclude = trial %in% c("Bari-SolidAct", "ACTT-2", "COVINIB") # include in forestplot but exclude from analysis
                      )
summary(mort28)
```

```
## Review:     Average treatment effect - mortality 28 days
## 
##                   OR            95%-CI %W(random)
## Bari-SolidAct 0.6565 [0.3052;  1.4120]       30.9
## ACTT-2        0.7041 [0.3996;  1.2406]       56.4
## Ghazaeian     0.7909 [0.1654;  3.7807]        7.4
## TOFACOV       2.5366 [0.1928; 33.3748]        2.7
## COVINIB       0.1816 [0.0126;  2.6139]        2.5
## 
## Number of studies: k = 5
## Number of observations: o = 1583
## 
##                                  OR           95%-CI     t p-value
## Random effects model (HK-SE) 0.6953 [0.3805; 1.2705] -1.67  0.1695
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 5.4355]; tau = 0 [0.0000; 2.3314]
##  I^2 = 0.0% [0.0%; 79.2%]; H = 1.00 [1.00; 2.19]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  1.99    4  0.7372
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 4)
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

![](two_stage_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# Open a pdf file
# pdf("./fp_aggregated.pdf", width=9, height=4)
# forest.meta(i.mort28_adhoc_se,
#             xlim = c(0.1,5),
#             xlab = "                  Favours Remdesivir <-> Favours No Remdesivir",
#             fs.xlab = 9)
# dev.off()
```

# (ii) Mortality at day 60


