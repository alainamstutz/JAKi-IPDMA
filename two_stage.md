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
library(metafor) #forest()
library(logistf) # Firth regression in case of rare events
```

# Load tot dataset for descriptive purpose (from descriptive.Rmd -> run before!)

```r
df_tot <- readRDS("df_tot_rux.RData") # without Murugesan, but with RUXCOVID for descriptive purpose
```

# Load treatment effect estimates from all trials

```r
df_barisolidact <- readRDS("trt_effects_barisolidact.RData")
df_actt2 <- readRDS("trt_effects_actt2.RData")
df_ghazaeian <- readRDS("trt_effects_ghazaeian.RData")
df_tofacov <- readRDS("trt_effects_tofacov.RData")
df_covinib <- readRDS("trt_effects_covinib.RData")
df_covbarrier <- readRDS("trt_effects_cov-barrier.RData")
df_recovery <- readRDS("trt_effects_recovery.RData")
df_tactic_r <- readRDS("trt_effects_tactic-r.RData")
df_ruxcovid <- readRDS("trt_effects_ruxcovid_05072024.RData")
df_pancovid <- readRDS("trt_effects_pancovid.RData")
```

# Reshape dataframes for all treatment effect estimates

```r
### Create a list of all data frames / trials
list_df <- list(df_barisolidact, df_actt2, df_ghazaeian, df_tofacov, df_covinib, df_covbarrier, df_recovery, df_tactic_r, 
                df_ruxcovid, 
                df_pancovid) # add all trials

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

## Mortality at day 28: average marginal effect
outcomes <- "death at day 28_marginal"
outcomes.firth <- "death at day 28_marginal_firth"
df_mort28_ame <- data.frame()
for (df in list_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_mort28_ame <- rbind(df_mort28_ame, selected_rows)
}

## Mortality at day 28: deterministic imputation
outcomes <- "death at day 28_dimp"
outcomes.firth <- "death at day 28_dimp_firth"
df_mort28_dimp <- data.frame()
for (df in list_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_mort28_dimp <- rbind(df_mort28_dimp, selected_rows)
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

## Show treatment estimates across trials

```r
# Nicely formatted table
kable(df_mort28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|     |variable              | hazard_odds_ratio|  ci_lower|    ci_upper| standard_error|   p_value| n_int| n_cont| e_int| e_cont|trial         |JAKi        |
|:----|:---------------------|-----------------:|---------:|-----------:|--------------:|---------:|-----:|------:|-----:|------:|:-------------|:-----------|
|trt  |death at day 28       |         0.6573165| 0.3016319|   1.3988895|      0.3888042| 0.2805077|   137|    140|    15|     21|Bari-SolidAct |Baricitinib |
|trt1 |death at day 28       |         0.7040732| 0.3957723|   1.2350463|      0.2890288| 0.2247583|   494|    492|    24|     37|ACTT-2        |Baricitinib |
|trt2 |death at day 28       |         0.7908805| 0.1472786|   3.8263828|      0.7982351| 0.7688275|    46|     51|     3|      4|Ghazaeian     |Tofacitinib |
|trt3 |death at day 28_firth |         2.5365726| 0.1271530| 380.1340920|      1.3148138| 0.5524355|    58|     58|     1|      0|TOFACOV       |Tofacitinib |
|trt4 |death at day 28_firth |         0.1815850| 0.0013083|   2.2871313|      1.3606659| 0.2027397|    53|     54|     0|      2|COVINIB       |Baricitinib |
|trt5 |death at day 28       |         0.5130861| 0.3652288|   0.7161199|      0.1715528| 0.0001003|   758|    765|    82|    129|COV-BARRIER   |Baricitinib |
|trt6 |death at day 28       |         0.8109044| 0.7032412|   0.9347835|      0.0725913| 0.0038835|  4061|   3940|   513|    545|RECOVERY      |Baricitinib |
|trt7 |death at day 28       |         0.8118731| 0.3531119|   1.8464022|      0.4190378| 0.6189376|   130|    138|    18|     17|TACTIC-R      |Baricitinib |
|trt8 |death at day 28       |         1.4673679| 0.4153822|   6.8220613|      0.6886735| 0.5776474|   282|    142|     9|      3|RUXCOVID      |Ruxolitinib |
|trt9 |death at day 28       |         0.2919981| 0.0385728|   1.4729645|      0.8856491| 0.1645440|   140|    136|     2|      6|PANCOVID      |Baricitinib |

```r
kable(df_mort60, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|      |variable              | hazard_odds_ratio|  ci_lower|    ci_upper| standard_error|   p_value| n_int| n_cont| e_int| e_cont|trial         |JAKi        |
|:-----|:---------------------|-----------------:|---------:|-----------:|--------------:|---------:|-----:|------:|-----:|------:|:-------------|:-----------|
|trt3  |death at day 60       |         0.9170453| 0.4614448|   1.8145475|      0.3476107| 0.8032644|   137|    140|    22|     24|Bari-SolidAct |Baricitinib |
|trt31 |death at day 60       |         0.7040732| 0.3957723|   1.2350463|      0.2890288| 0.2247583|   494|    492|    24|     37|ACTT-2        |Baricitinib |
|trt32 |death at day 60       |         0.7908805| 0.1472786|   3.8263828|      0.7982351| 0.7688275|    46|     51|     3|      4|Ghazaeian     |Tofacitinib |
|trt2  |death at day 60_firth |         2.5365726| 0.1271530| 380.1340920|      1.3148138| 0.5524355|    58|     58|     1|      0|TOFACOV       |Tofacitinib |
|trt21 |death at day 60_firth |         0.1815850| 0.0013083|   2.2871313|      1.3606659| 0.2027397|    53|     54|     0|      2|COVINIB       |Baricitinib |
|trt33 |death at day 60       |         0.5655834| 0.4121980|   0.7724540|      0.1600564| 0.0003700|   748|    750|   102|    147|COV-BARRIER   |Baricitinib |
|trt34 |death at day 60       |         0.8109044| 0.7032412|   0.9347835|      0.0725913| 0.0038835|  4061|   3940|   513|    545|RECOVERY      |Baricitinib |
|trt35 |death at day 60       |         0.9558738| 0.4457384|   2.0451423|      0.3863856| 0.9070195|   129|    135|    23|     19|TACTIC-R      |Baricitinib |
|trt22 |death at day 60       |         1.4673679| 0.4153822|   6.8220613|      0.6886735| 0.5776474|   282|    142|     9|      3|RUXCOVID      |Ruxolitinib |
|trt36 |death at day 60       |         0.3737420| 0.0742696|   1.4936543|      0.7400535| 0.1835546|   140|    136|     3|      7|PANCOVID      |Baricitinib |

```r
kable(df_ttdeath, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|      |variable               | hazard_odds_ratio|  ci_lower|      ci_upper| standard_error|   p_value| n_int| n_cont| e_int| e_cont|trial         |JAKi        |
|:-----|:----------------------|-----------------:|---------:|-------------:|--------------:|---------:|-----:|------:|-----:|------:|:-------------|:-----------|
|trt4  |death within fup       |         0.8356348| 0.4670056|  1.495240e+00|      0.2968678| 0.5452716|   145|    144|    22|     24|Bari-SolidAct |Baricitinib |
|trt41 |death within fup       |         0.7409249| 0.4415164|  1.243373e+00|      0.2641294| 0.2562656|   515|    518|    24|     37|ACTT-2        |Baricitinib |
|trt42 |death within fup       |         0.8132523| 0.1819296|  3.635359e+00|      0.7640047| 0.7867247|    46|     51|     3|      4|Ghazaeian     |Tofacitinib |
|trt3  |death within fup_firth |         3.0000000| 1.1735588| 1.309377e+190|      2.3094011| 0.4694594|    58|     58|     1|      0|TOFACOV       |Tofacitinib |
|trt31 |death within fup_firth |         0.1747878| 1.0012703|  8.430047e+00|      1.9410119| 0.1859877|    55|     55|     0|      2|COVINIB       |Baricitinib |
|trt43 |death within fup       |         0.6130530| 0.4760130|  7.895456e-01|      0.1290872| 0.0001503|   815|    811|   102|    147|COV-BARRIER   |Baricitinib |
|trt44 |death within fup       |         0.8442946| 0.7483075|  9.525941e-01|      0.0615763| 0.0059835|  4136|   3994|   513|    545|RECOVERY      |Baricitinib |
|trt45 |death within fup       |         1.0670214| 0.5777646|  1.970586e+00|      0.3129954| 0.8358078|   137|    145|    23|     19|TACTIC-R      |Baricitinib |
|trt32 |death within fup       |         1.4335664| 0.3854712|  5.331430e+00|      0.6701420| 0.5909594|   287|    145|     9|      3|RUXCOVID      |Ruxolitinib |
|trt46 |death within fup       |         0.3776779| 0.0929692|  1.534278e+00|      0.7152037| 0.1733718|   145|    142|     3|      7|PANCOVID      |Baricitinib |

```r
kable(df_mort28_ame, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|      |variable                 | hazard_odds_ratio|   ci_lower|   ci_upper| standard_error|   p_value| n_int| n_cont| e_int| e_cont|trial         |JAKi        |
|:-----|:------------------------|-----------------:|----------:|----------:|--------------:|---------:|-----:|------:|-----:|------:|:-------------|:-----------|
|trt2  |death at day 28_marginal |        -0.0411176| -0.1169964|  0.0347612|      0.0387144| 0.2882015|   137|    140|    15|     21|Bari-SolidAct |Baricitinib |
|trt21 |death at day 28_marginal |        -0.0176890| -0.0465268|  0.0111488|      0.0147134| 0.2292728|   494|    492|    24|     37|ACTT-2        |Baricitinib |
|trt22 |death at day 28_marginal |        -0.0154566| -0.1229478|  0.0920345|      0.0548434| 0.7780722|    46|     51|     3|      4|Ghazaeian     |Tofacitinib |
|1     |death at day 28_marginal |         0.0150326| -0.0185850|  0.0486501|      0.0171521|        NA|    58|     58|     1|      0|TOFACOV       |Tofacitinib |
|11    |death at day 28_marginal |        -0.0376995| -0.0879332|  0.0125343|      0.0256299|        NA|    53|     54|     0|      2|COVINIB       |Baricitinib |
|trt23 |death at day 28_marginal |        -0.0621199| -0.0934182| -0.0308217|      0.0159688| 0.0001002|   758|    765|    82|    129|COV-BARRIER   |Baricitinib |
|trt24 |death at day 28_marginal |        -0.0199695| -0.0335891| -0.0063500|      0.0069489| 0.0040560|  4061|   3940|   513|    545|RECOVERY      |Baricitinib |
|trt25 |death at day 28_marginal |        -0.0182545| -0.0965741|  0.0600651|      0.0399597| 0.6477979|   130|    138|    18|     17|TACTIC-R      |Baricitinib |
|trt26 |death at day 28_marginal |        -0.0310762| -0.0754655|  0.0133132|      0.0226480| 0.1700214|   140|    136|     2|      6|PANCOVID      |Baricitinib |

```r
kable(df_mort28_dimp, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|      |variable                   | hazard_odds_ratio|  ci_lower|    ci_upper| standard_error|   p_value| n_int| n_cont| e_int| e_cont|trial         |JAKi        |
|:-----|:--------------------------|-----------------:|---------:|-----------:|--------------:|---------:|-----:|------:|-----:|------:|:-------------|:-----------|
|trt1  |death at day 28_dimp       |         0.6403796| 0.2947749|   1.3584352|      0.3871741| 0.2496719|   145|    144|    15|     21|Bari-SolidAct |Baricitinib |
|trt11 |death at day 28_dimp       |         0.7367848| 0.4158210|   1.2878504|      0.2870900| 0.2873355|   515|    518|    24|     37|ACTT-2        |Baricitinib |
|trt12 |death at day 28_dimp       |         0.7908805| 0.1472786|   3.8263828|      0.7982351| 0.7688275|    46|     51|     3|      4|Ghazaeian     |Tofacitinib |
|trt13 |death at day 28_dimp_firth |         2.5365726| 0.1271530| 380.1340920|      1.3148138| 0.5524355|    58|     58|     1|      0|TOFACOV       |Tofacitinib |
|trt14 |death at day 28_dimp_firth |         0.1821907| 0.0013138|   2.2907423|      1.3578533| 0.2033326|    55|     55|     0|      2|COVINIB       |Baricitinib |
|trt15 |death at day 28_dimp       |         0.5122147| 0.3660614|   0.7121437|      0.1695550| 0.0000796|   815|    811|    82|    129|COV-BARRIER   |Baricitinib |
|trt16 |death at day 28_dimp       |         0.8030429| 0.6968020|   0.9252111|      0.0723122| 0.0024187|  4136|   3994|   513|    545|RECOVERY      |Baricitinib |
|trt17 |death at day 28_dimp       |         0.8966336| 0.3978406|   2.0105465|      0.4104700| 0.7903837|   137|    145|    18|     17|TACTIC-R      |Baricitinib |
|trt18 |death at day 28_dimp       |         1.4777138| 0.4183067|   6.8695011|      0.6886330| 0.5706735|   287|    145|     9|      3|RUXCOVID      |Ruxolitinib |
|trt19 |death at day 28_dimp       |         0.2835650| 0.0373289|   1.4327528|      0.8873556| 0.1555195|   145|    142|     2|      6|PANCOVID      |Baricitinib |

```r
kable(df_new_mv28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|      |variable                | hazard_odds_ratio|  ci_lower| ci_upper| standard_error|   p_value| n_int| n_cont| e_int| e_cont|trial         |JAKi        |
|:-----|:-----------------------|-----------------:|---------:|--------:|--------------:|---------:|-----:|------:|-----:|------:|:-------------|:-----------|
|trt5  |new MV within 28d       |         1.5355491| 0.7522041| 3.207729|      0.3676045| 0.2433273|   107|    104|    22|     15|Bari-SolidAct |Baricitinib |
|trt51 |new MV within 28d       |         0.6873481| 0.4248321| 1.104252|      0.2430476| 0.1229387|   437|    422|    39|     52|ACTT-2        |Baricitinib |
|trt4  |new MV within 28d_firth |         0.2174501| 0.0015447| 2.877308|      1.3853829| 0.2687398|    57|     58|     0|      2|TOFACOV       |Tofacitinib |
|trt41 |new MV within 28d       |         0.2704553| 0.0378052| 1.260632|      0.8500686| 0.1239788|    53|     52|     2|      6|COVINIB       |Baricitinib |
|trt52 |new MV within 28d       |         1.2208474| 0.8397427| 1.781919|      0.1915855| 0.2976219|   655|    624|    78|     60|COV-BARRIER   |Baricitinib |
|trt53 |new MV within 28d       |         0.8228684| 0.6570718| 1.029693|      0.1145031| 0.0886331|  3481|   3338|   158|    174|RECOVERY      |Baricitinib |
|trt54 |new MV within 28d       |         0.8582744| 0.2657998| 2.691659|      0.5779994| 0.7914605|   119|    126|     6|      7|TACTIC-R      |Baricitinib |
|trt42 |new MV within 28d       |         1.0889041| 0.3713229| 3.622376|      0.5685645| 0.8809213|   273|    139|    12|      5|RUXCOVID      |Ruxolitinib |
|trt55 |new MV within 28d       |         0.8630304| 0.2668136| 2.734215|      0.5804555| 0.7996690|   138|    130|     6|      7|PANCOVID      |Baricitinib |

```r
kable(df_new_mvd28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|      |variable                   | hazard_odds_ratio|  ci_lower|  ci_upper| standard_error|   p_value| n_int| n_cont| e_int| e_cont|trial         |JAKi        |
|:-----|:--------------------------|-----------------:|---------:|---------:|--------------:|---------:|-----:|------:|-----:|------:|:-------------|:-----------|
|trt6  |new MV or death within 28d |         1.0501251| 0.6083616| 1.8136644|      0.2779433| 0.8603186|   138|    140|    37|     36|Bari-SolidAct |Baricitinib |
|trt61 |new MV or death within 28d |         0.6874157| 0.4667200| 1.0080115|      0.1961283| 0.0559949|   500|    499|    63|     89|ACTT-2        |Baricitinib |
|trt5  |new MV or death within 28d |         0.7908805| 0.1472786| 3.8263828|      0.7982351| 0.7688275|    46|     51|     3|      4|Ghazaeian     |Tofacitinib |
|trt51 |new MV or death within 28d |         0.5033582| 0.0222146| 5.7949089|      1.2708458| 0.5890904|    58|     58|     1|      2|TOFACOV       |Tofacitinib |
|trt52 |new MV or death within 28d |         0.1994253| 0.0285841| 0.8653293|      0.8280712| 0.0515259|    53|     54|     2|      8|COVINIB       |Baricitinib |
|trt62 |new MV or death within 28d |         0.8234639| 0.6203522| 1.0917877|      0.1440924| 0.1776605|   768|    775|   191|    210|COV-BARRIER   |Baricitinib |
|trt63 |new MV or death within 28d |         0.8153498| 0.7199695| 0.9231650|      0.0634090| 0.0012847|  4077|   3952|   671|    719|RECOVERY      |Baricitinib |
|trt64 |new MV or death within 28d |         0.8713662| 0.4379743| 1.7223634|      0.3476931| 0.6920911|   137|    145|    24|     24|TACTIC-R      |Baricitinib |
|trt53 |new MV or death within 28d |         1.2640033| 0.5493098| 3.1711248|      0.4414242| 0.5955952|   282|    142|    21|      8|RUXCOVID      |Ruxolitinib |
|trt65 |new MV or death within 28d |         0.6132898| 0.2290263| 1.5627647|      0.4830565| 0.3114742|   140|    136|     8|     13|PANCOVID      |Baricitinib |

```r
kable(df_clin28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|      |variable                  | hazard_odds_ratio|  ci_lower|  ci_upper| standard_error|   p_value| n_int| n_cont| e_int| e_cont|trial         |JAKi        |
|:-----|:-------------------------|-----------------:|---------:|---------:|--------------:|---------:|-----:|------:|-----:|------:|:-------------|:-----------|
|trt7  |clinical status at day 28 |         0.9450541| 0.5773149| 1.5470316|      0.2510000| 0.8218613|   145|    144|    NA|     NA|Bari-SolidAct |Baricitinib |
|trt71 |clinical status at day 28 |         0.6928205| 0.4961089| 0.9648921|      0.1695147| 0.0303944|   515|    518|    NA|     NA|ACTT-2        |Baricitinib |
|trt6  |clinical status at day 28 |         0.8256311| 0.1539462| 3.9888785|      0.7975917| 0.8101502|    46|     51|    NA|     NA|Ghazaeian     |Tofacitinib |
|trt61 |clinical status at day 28 |         0.5089991| 0.0666734| 2.8430057|      0.9065891| 0.4563383|    58|     58|    NA|     NA|TOFACOV       |Tofacitinib |
|trt62 |clinical status at day 28 |         0.3212049| 0.0428055| 1.6548681|      0.8868231| 0.2003301|    55|     55|    NA|     NA|COVINIB       |Baricitinib |
|trt72 |clinical status at day 28 |         0.8486995| 0.6652989| 1.0821731|      0.1240389| 0.1859787|   815|    811|    NA|     NA|COV-BARRIER   |Baricitinib |
|trt73 |clinical status at day 28 |         0.7987406| 0.7117768| 0.8961529|      0.0587555| 0.0001310|  4136|   3994|    NA|     NA|RECOVERY      |Baricitinib |
|trt74 |clinical status at day 28 |         0.8436558| 0.4665473| 1.5175791|      0.2999809| 0.5708920|   137|    145|    NA|     NA|TACTIC-R      |Baricitinib |
|trt63 |clinical status at day 28 |         1.1528359| 0.5359331| 2.6404373|      0.4025796| 0.7238755|   287|    145|    NA|     NA|RUXCOVID      |Ruxolitinib |
|trt75 |clinical status at day 28 |         0.5708269| 0.3594016| 0.9012863|      0.2342265| 0.0166791|   145|    142|    NA|     NA|PANCOVID      |Baricitinib |

```r
kable(df_ttdischarge, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|      |variable                 | hazard_odds_ratio|  ci_lower| ci_upper| standard_error|   p_value| n_int| n_cont| e_int| e_cont|trial         |JAKi        |
|:-----|:------------------------|-----------------:|---------:|--------:|--------------:|---------:|-----:|------:|-----:|------:|:-------------|:-----------|
|trt8  |discharge within 28 days |         1.0720566| 0.8082711| 1.421930|      0.1441029| 0.6292086|   145|    144|    99|     94|Bari-SolidAct |Baricitinib |
|trt81 |discharge within 28 days |         1.2140636| 1.0596485| 1.390981|      0.0694073| 0.0051947|   515|    518|   433|    406|ACTT-2        |Baricitinib |
|trt7  |discharge within 28 days |         0.7381029| 0.4797810| 1.135510|      0.2197763| 0.1670538|    46|     51|    43|     47|Ghazaeian     |Tofacitinib |
|trt71 |discharge within 28 days |         1.2836003| 0.8804226| 1.871408|      0.1923618| 0.1943172|    58|     58|    56|     54|TOFACOV       |Tofacitinib |
|trt72 |discharge within 28 days |         1.5863095| 1.0663853| 2.359727|      0.2026239| 0.0227757|    55|     55|    53|     50|COVINIB       |Baricitinib |
|trt82 |discharge within 28 days |         1.0679179| 0.9544072| 1.194929|      0.0573356| 0.2517645|   815|    811|   616|    604|COV-BARRIER   |Baricitinib |
|trt83 |discharge within 28 days |         1.1216980| 1.0682008| 1.177874|      0.0249330| 0.0000041|  4136|   3994|  3328|   3124|RECOVERY      |Baricitinib |
|trt84 |discharge within 28 days |         1.0079653| 0.7709468| 1.317852|      0.1367727| 0.9537433|   137|    145|   106|    113|TACTIC-R      |Baricitinib |
|trt73 |discharge within 28 days |         1.0303537| 0.8364309| 1.269237|      0.1063864| 0.7786559|   283|    142|   260|    135|RUXCOVID      |Ruxolitinib |
|trt85 |discharge within 28 days |         1.3210084| 0.9681911| 1.802395|      0.1585341| 0.0790782|   145|    142|    93|     72|PANCOVID      |Baricitinib |

```r
kable(df_ttdischarge_comp, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|      |variable                                   | hazard_odds_ratio|  ci_lower| ci_upper| standard_error|   p_value| n_int| n_cont| e_int| e_cont|trial         |JAKi        |
|:-----|:------------------------------------------|-----------------:|---------:|--------:|--------------:|---------:|-----:|------:|-----:|------:|:-------------|:-----------|
|trt9  |discharge within 28 days, death=comp.event |         1.1132720| 0.8468207| 1.463562|      0.1395789| 0.4400000|   145|    144|    99|     94|Bari-SolidAct |Baricitinib |
|trt91 |discharge within 28 days, death=comp.event |         1.1601147| 1.0201821| 1.319241|      0.0655817| 0.0240000|   515|    518|   433|    406|ACTT-2        |Baricitinib |
|trt8  |discharge within 28 days, death=comp.event |         0.7381029| 0.4797810| 1.135510|      0.2197763| 0.1670538|    46|     51|    43|     47|Ghazaeian     |Tofacitinib |
|trt81 |discharge within 28 days, death=comp.event |         1.2562193| 0.8760345| 1.801398|      0.1839098| 0.2100000|    58|     58|    56|     54|TOFACOV       |Tofacitinib |
|trt82 |discharge within 28 days, death=comp.event |         1.5296461| 1.0591368| 2.209174|      0.1875454| 0.0230000|    55|     55|    53|     50|COVINIB       |Baricitinib |
|trt92 |discharge within 28 days, death=comp.event |         1.1098862| 0.9966098| 1.236038|      0.0549262| 0.0580000|   815|    811|   616|    604|COV-BARRIER   |Baricitinib |
|trt93 |discharge within 28 days, death=comp.event |         1.0929595| 1.0428902| 1.145433|      0.0239256| 0.0002000|  4136|   3994|  3328|   3124|RECOVERY      |Baricitinib |
|trt94 |discharge within 28 days, death=comp.event |         0.9762508| 0.7555294| 1.261454|      0.1307681| 0.8500000|   137|    145|   106|    113|TACTIC-R      |Baricitinib |
|trt83 |discharge within 28 days, death=comp.event |         1.0211830| 0.8212574| 1.269778|      0.1061990| 0.8504351|   283|    142|   260|    135|RUXCOVID      |Ruxolitinib |
|trt95 |discharge within 28 days, death=comp.event |         1.2612342| 0.9754959| 1.630670|      0.1310739| 0.0770000|   145|    142|    93|     72|PANCOVID      |Baricitinib |

```r
kable(df_ttdischarge_hypo, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|       |variable                                   | hazard_odds_ratio|  ci_lower| ci_upper| standard_error|   p_value| n_int| n_cont| e_int| e_cont|trial         |JAKi        |
|:------|:------------------------------------------|-----------------:|---------:|--------:|--------------:|---------:|-----:|------:|-----:|------:|:-------------|:-----------|
|trt10  |discharge within 28 days, death=hypo.event |         1.0826737| 0.8161998| 1.436146|      0.1441504| 0.5816017|   145|    144|    99|     94|Bari-SolidAct |Baricitinib |
|trt101 |discharge within 28 days, death=hypo.event |         1.2108219| 1.0568667| 1.387204|      0.0693844| 0.0058317|   515|    518|   433|    406|ACTT-2        |Baricitinib |
|trt9   |discharge within 28 days, death=hypo.event |         0.8231640| 0.5400812| 1.254624|      0.2150223| 0.3654539|    46|     51|    43|     47|Ghazaeian     |Tofacitinib |
|trt91  |discharge within 28 days, death=hypo.event |         1.2836003| 0.8804226| 1.871408|      0.1923618| 0.1943172|    58|     58|    56|     54|TOFACOV       |Tofacitinib |
|trt92  |discharge within 28 days, death=hypo.event |         1.5895397| 1.0685224| 2.364608|      0.2026403| 0.0221941|    55|     55|    53|     50|COVINIB       |Baricitinib |
|trt102 |discharge within 28 days, death=hypo.event |         1.1214423| 1.0022795| 1.254773|      0.0573167| 0.0455339|   815|    811|   616|    604|COV-BARRIER   |Baricitinib |
|trt103 |discharge within 28 days, death=hypo.event |         1.1327442| 1.0787118| 1.189483|      0.0249370| 0.0000006|  4136|   3994|  3328|   3124|RECOVERY      |Baricitinib |
|trt104 |discharge within 28 days, death=hypo.event |         0.9919843| 0.7588691| 1.296709|      0.1366749| 0.9530442|   137|    145|   106|    113|TACTIC-R      |Baricitinib |
|trt93  |discharge within 28 days, death=hypo.event |         1.0181278| 0.8265765| 1.254069|      0.1063429| 0.8658444|   283|    142|   260|    135|RUXCOVID      |Ruxolitinib |
|trt105 |discharge within 28 days, death=hypo.event |         1.3901249| 1.0185580| 1.897238|      0.1586793| 0.0379083|   145|    142|    93|     72|PANCOVID      |Baricitinib |

```r
kable(df_ttdischarge_sus, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|       |variable                           | hazard_odds_ratio|  ci_lower| ci_upper| standard_error|   p_value| n_int| n_cont| e_int| e_cont|trial         |JAKi        |
|:------|:----------------------------------|-----------------:|---------:|--------:|--------------:|---------:|-----:|------:|-----:|------:|:-------------|:-----------|
|trt11  |sustained discharge within 28 days |         1.0553905| 0.7946311| 1.401719|      0.1447925| 0.7096461|   145|    144|    97|     94|Bari-SolidAct |Baricitinib |
|trt111 |sustained discharge within 28 days |         1.2140636| 1.0596485| 1.390981|      0.0694073| 0.0051947|   515|    518|   433|    406|ACTT-2        |Baricitinib |
|trt10  |sustained discharge within 28 days |         0.7381029| 0.4797810| 1.135510|      0.2197763| 0.1670538|    46|     51|    43|     47|Ghazaeian     |Tofacitinib |
|trt101 |sustained discharge within 28 days |         1.2836003| 0.8804226| 1.871408|      0.1923618| 0.1943172|    58|     58|    56|     54|TOFACOV       |Tofacitinib |
|trt102 |sustained discharge within 28 days |         1.5863095| 1.0663853| 2.359727|      0.2026239| 0.0227757|    55|     55|    53|     50|COVINIB       |Baricitinib |
|trt112 |sustained discharge within 28 days |         1.0582193| 0.9453869| 1.184518|      0.0575259| 0.3252680|   815|    811|   610|    602|COV-BARRIER   |Baricitinib |
|trt113 |sustained discharge within 28 days |         1.1216980| 1.0682008| 1.177874|      0.0249330| 0.0000041|  4136|   3994|  3328|   3124|RECOVERY      |Baricitinib |
|trt114 |sustained discharge within 28 days |         1.0079653| 0.7709468| 1.317852|      0.1367727| 0.9537433|   137|    145|   106|    113|TACTIC-R      |Baricitinib |
|trt103 |sustained discharge within 28 days |         1.0303537| 0.8364309| 1.269237|      0.1063864| 0.7786559|   283|    142|   260|    135|RUXCOVID      |Ruxolitinib |
|trt115 |sustained discharge within 28 days |         1.3210084| 0.9681911| 1.802395|      0.1585341| 0.0790782|   145|    142|    93|     72|PANCOVID      |Baricitinib |

```r
kable(df_vir_clear_5, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|       |variable                    | hazard_odds_ratio|  ci_lower| ci_upper| standard_error|   p_value| n_int| n_cont| e_int| e_cont|trial         |JAKi        |
|:------|:---------------------------|-----------------:|---------:|--------:|--------------:|---------:|-----:|------:|-----:|------:|:-------------|:-----------|
|trt12  |viral clearance until day 5 |         1.4947189| 0.6387866| 3.583365|      0.4368603| 0.3575409|    62|     59|    18|     12|Bari-SolidAct |Baricitinib |
|trt121 |viral clearance until day 5 |         0.9730685| 0.7147263| 1.324544|      0.1572772| 0.8621926|   375|    372|   126|    125|ACTT-2        |Baricitinib |
|trt122 |viral clearance until day 5 |         0.8979536| 0.6696927| 1.203326|      0.1493832| 0.4711914|   492|    486|   118|    126|COV-BARRIER   |Baricitinib |
|trt123 |viral clearance until day 5 |         0.9712782| 0.6519952| 1.446190|      0.2025879| 0.8856187|  3810|   3707|    49|     50|RECOVERY      |Baricitinib |
|trt124 |viral clearance until day 5 |         0.3944873| 0.0928222| 1.486722|      0.6957503| 0.1812460|    26|     24|     6|      9|TACTIC-R      |Baricitinib |

```r
kable(df_vir_clear_10, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|       |variable                     | hazard_odds_ratio|  ci_lower| ci_upper| standard_error|   p_value| n_int| n_cont| e_int| e_cont|trial         |JAKi        |
|:------|:----------------------------|-----------------:|---------:|--------:|--------------:|---------:|-----:|------:|-----:|------:|:-------------|:-----------|
|trt13  |viral clearance until day 10 |         1.0882308| 0.5360542| 2.212393|      0.3606135| 0.8146196|    66|     61|    31|     27|Bari-SolidAct |Baricitinib |
|trt131 |viral clearance until day 10 |         0.8881068| 0.6617212| 1.191279|      0.1499087| 0.4286111|   378|    377|   145|    154|ACTT-2        |Baricitinib |
|trt132 |viral clearance until day 10 |         0.9903239| 0.7700322| 1.273725|      0.1283352| 0.9396065|   538|    522|   205|    201|COV-BARRIER   |Baricitinib |
|trt133 |viral clearance until day 10 |         0.9141180| 0.6490415| 1.285688|      0.1739792| 0.6057646|  3912|   3798|    65|     70|RECOVERY      |Baricitinib |
|trt134 |viral clearance until day 10 |         0.8137138| 0.2763189| 2.384134|      0.5455649| 0.7055353|    34|     30|    13|     13|TACTIC-R      |Baricitinib |

```r
kable(df_vir_clear_15, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|       |variable                     | hazard_odds_ratio|  ci_lower| ci_upper| standard_error|   p_value| n_int| n_cont| e_int| e_cont|trial         |JAKi        |
|:------|:----------------------------|-----------------:|---------:|--------:|--------------:|---------:|-----:|------:|-----:|------:|:-------------|:-----------|
|trt14  |viral clearance until day 15 |         1.0042249| 0.4961136| 2.033213|      0.3588361| 0.9906258|    67|     61|    37|     34|Bari-SolidAct |Baricitinib |
|trt141 |viral clearance until day 15 |         1.0197142| 0.7660006| 1.357492|      0.1459096| 0.8935626|   380|    379|   182|    179|ACTT-2        |Baricitinib |
|trt142 |viral clearance until day 15 |         0.9717344| 0.7629677| 1.237579|      0.1233524| 0.8161915|   551|    545|   255|    257|COV-BARRIER   |Baricitinib |
|trt143 |viral clearance until day 15 |         0.8550711| 0.6171853| 1.181996|      0.1654235| 0.3439017|  3951|   3832|    70|     80|RECOVERY      |Baricitinib |
|trt144 |viral clearance until day 15 |         0.9679081| 0.3419839| 2.743368|      0.5277283| 0.9507152|    34|     31|    15|     14|TACTIC-R      |Baricitinib |

```r
kable(df_ae28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|       |variable                              | hazard_odds_ratio|  ci_lower|   ci_upper| standard_error|   p_value| n_int| n_cont| e_int| e_cont|trial         |JAKi        |
|:------|:-------------------------------------|-----------------:|---------:|----------:|--------------:|---------:|-----:|------:|-----:|------:|:-------------|:-----------|
|trt15  |Any AE grade 3,4 within 28 days       |         0.9097563| 0.5310592|   1.557063|      0.2738090| 0.7297801|   130|    123|    44|     44|Bari-SolidAct |Baricitinib |
|trt151 |Any AE grade 3,4 within 28 days       |         0.9214240| 0.7032692|   1.207079|      0.1377423| 0.5524340|   491|    481|   196|    204|ACTT-2        |Baricitinib |
|trt11  |any AE grade 3,4 within 28 days_firth |         3.2247381| 0.1701822| 472.450898|      1.4759989| 0.4412199|    46|     51|     1|      0|Ghazaeian     |Tofacitinib |
|trt111 |any AE grade 3,4 within 28 days       |         0.6936245| 0.2408177|   1.956966|      0.5287298| 0.4890045|    58|     58|     9|     13|TOFACOV       |Tofacitinib |
|trt112 |any AE grade 3,4 within 28 days       |         0.7968701| 0.3083398|   2.035379|      0.4772424| 0.6342294|    55|     53|    11|     12|COVINIB       |Baricitinib |
|trt152 |Any AE grade 3,4 within 28 days       |         1.1048134| 0.8459092|   1.444017|      0.1363248| 0.4646760|   683|    642|   171|    143|COV-BARRIER   |Baricitinib |
|trt153 |Any AE grade 3,4 within 28 days       |         0.9093754| 0.7511848|   1.100785|      0.0974199| 0.3294946|  3559|   3399|   229|    230|RECOVERY      |Baricitinib |
|trt154 |Any AE grade 3,4 within 28 days       |         1.3388686| 0.6236369|   2.925078|      0.3912535| 0.4557449|   119|    128|    18|     14|TACTIC-R      |Baricitinib |
|trt113 |Any AE grade 3,4 within 28 days       |         0.5830045| 0.3117147|   1.098558|      0.3196187| 0.0913849|   273|    139|    27|     21|RUXCOVID      |Ruxolitinib |
|trt12  |Any AE grade 3,4 within 28 days       |         1.1944162| 0.5826642|   2.479301|      0.3668133| 0.6281539|   143|    136|    19|     16|PANCOVID      |Baricitinib |

```r
kable(df_ae28sev, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|       |variable                           | hazard_odds_ratio|  ci_lower|    ci_upper| standard_error|   p_value| n_int| n_cont| e_int| e_cont|trial         |JAKi        |
|:------|:----------------------------------|-----------------:|---------:|-----------:|--------------:|---------:|-----:|------:|-----:|------:|:-------------|:-----------|
|trt16  |AEs grade 3,4 within 28 days       |         1.2666683| 0.9515693|   1.6929896|      0.1467211| 0.1071465|     1|      0|    NA|     NA|Bari-SolidAct |Baricitinib |
|trt161 |AEs grade 3,4 within 28 days       |         0.8106790| 0.7179717|   0.9148002|      0.0617874| 0.0006816|   491|    481|    NA|     NA|ACTT-2        |Baricitinib |
|trt12  |AEs grade 3,4 within 28 days_firth |         3.2247381| 0.1701822| 472.4508978|      1.4759989| 0.4412199|    46|     51|    NA|     NA|Ghazaeian     |Tofacitinib |
|trt121 |AEs grade 3,4 within 28 days       |         0.6936245| 0.2408177|   1.9569664|      0.5287298| 0.4890045|    58|     58|    NA|     NA|TOFACOV       |Tofacitinib |
|trt122 |AEs grade 3,4 within 28 days       |         0.5880219| 0.3243155|   1.0415364|      0.2955431| 0.0723893|    55|     53|    NA|     NA|COVINIB       |Baricitinib |
|trt162 |AEs grade 3,4 within 28 days       |         1.2980088| 1.1162311|   1.5115350|      0.0773026| 0.0007404|   683|    642|    NA|     NA|COV-BARRIER   |Baricitinib |
|trt163 |AEs grade 3,4 within 28 days       |         0.8576196| 0.7201617|   1.0209676|      0.0889837| 0.0843297|  3559|   3399|    NA|     NA|RECOVERY      |Baricitinib |
|trt164 |AEs grade 3,4 within 28 days_firth |         1.4541954| 0.7410439|   2.8944172|      0.3428692| 0.2768425|   119|    128|    NA|     NA|TACTIC-R      |Baricitinib |
|trt123 |AEs grade 3,4 within 28 days       |         0.5844046| 0.3873412|   0.8859314|      0.2103249| 0.0106505|   273|    139|    NA|     NA|RUXCOVID      |Ruxolitinib |
|trt13  |AEs grade 3,4 within 28 days       |         1.1317900| 0.6369407|   2.0215022|      0.2927380| 0.6723643|   143|    136|    NA|     NA|PANCOVID      |Baricitinib |


# (i) Primary outcome: Mortality at day 28. Only IPD trials

```r
# str(df_mort28)
mort28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_mort28,
                      n.e = n_int,
                      n.c = n_cont,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = T,
                      method.tau = "ML", # same results with REML (-> see one-stage!)
                      # hakn = T, 
                      # adhoc.hakn.ci = "", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      method.random.ci = "HK", # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      # subset = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # exclude entirely
                      # exclude = trial %in% c("RUXCOVID") # include in forestplot but exclude from analysis
                      )
summary(mort28)
```

```
##                   OR            95%-CI %W(random)
## Bari-SolidAct 0.6573 [0.3068;  1.4084]        6.2
## ACTT-2        0.7041 [0.3996;  1.2406]       10.4
## Ghazaeian     0.7909 [0.1654;  3.7807]        1.6
## TOFACOV       2.5366 [0.1928; 33.3748]        0.6
## COVINIB       0.1816 [0.0126;  2.6139]        0.6
## COV-BARRIER   0.5131 [0.3666;  0.7182]       22.9
## RECOVERY      0.8109 [0.7034;  0.9349]       48.9
## TACTIC-R      0.8119 [0.3571;  1.8458]        5.4
## RUXCOVID      1.4674 [0.3805;  5.6590]        2.1
## PANCOVID      0.2920 [0.0515;  1.6567]        1.3
## 
## Number of studies: k = 10
## Number of observations: o = 12075
## 
##                               OR           95%-CI     t p-value
## Random effects model (HK) 0.7085 [0.5758; 0.8718] -3.76  0.0045
## Prediction interval              [0.4875; 1.0298]              
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0159 [0.0000; 0.6284]; tau = 0.1262 [0.0000; 0.7927]
##  I^2 = 12.5% [0.0%; 53.6%]; H = 1.07 [1.00; 1.47]
## 
## Test of heterogeneity:
##      Q d.f. p-value
##  10.28    9  0.3281
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 9)
## - Prediction interval based on t-distribution (df = 8)
```

```r
forest(mort28, 
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aOR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "e_int", "n.c", "e_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "no JAKi", "Events\nno JAKi"),
            text.random = "Average treatment effect",
            # xlim = c(0.15,5),
            sortvar = +TE,
            label.left = "Favours JAKi",
            label.right = "Favours No JAKi",
            )
```

![](two_stage_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
# Open a pdf file
pdf("./fp_mort_28.pdf", width=13, height=5)

forestplot <- forest(mort28,
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aOR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "e_int", "n.c", "e_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "no JAKi", "Events\nno JAKi"),
            text.random = "Average treatment effect   ",
            # xlim = c(0.15,5),
            sortvar = +TE,
            label.left = "Favours JAKi",
            label.right = "Favours No JAKi",
            pooled.total = F
)

summary_row_y <- unit(0.26, "npc")
total_row <- c(sum(df_mort28$n_int, na.rm = TRUE), 
               sum(df_mort28$e_int, na.rm = TRUE), 
               sum(df_mort28$n_cont, na.rm = TRUE),
               sum(df_mort28$e_cont, na.rm = TRUE))

pushViewport(viewport())

# Add text annotations aligned with the summary row
grid.text(label = total_row[1], x = unit(0.318, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[2], x = unit(0.37, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[3], x = unit(0.42 , "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[4], x = unit(0.475 , "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))

popViewport()

dev.off()
```

```
## quartz_off_screen 
##                 2
```
Discussion points:
1. REML or ML -> Give the exact same result, but the one-stage uses ML (including centering) due to rare events. The choice of estimator might have the biggest influence on the 95%CI, larger than other model parameter choices.

### Funnel plot

```r
## funnel plot (contour enhanced)
funnel(mort28)
```

![](two_stage_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
funnel(mort28, common = TRUE,
  level = 0.95, contour = c(0.9, 0.95, 0.99),
  col.contour = c("darkgreen", "green", "lightgreen"),
  lwd = 2, cex = 1.5, pch = 16, studlab = TRUE, cex.studlab = 1.1)
```

![](two_stage_files/figure-html/unnamed-chunk-6-2.png)<!-- -->

```r
# legend(0.05, 0.05,
#   c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"),
#   fill = c("darkgreen", "green", "lightgreen"))
# par(oldpar)
```

# (i.i) Primary outcome: Including the non-IPD RCTs // data preparation

```r
# first, add more columns to df_mort28
df_mort28 <- df_mort28 %>% 
  mutate(recruitment_period = case_when(trial == "Bari-SolidAct" ~ "06.2021-03.2022",
                                        trial == "ACTT-2" ~ "05.2020-07.2020",
                                        trial == "Ghazaeian" ~ "08.2021-11.2021",
                                        trial == "TOFACOV" ~ "09.2020-12.2021",
                                        trial == "COVINIB" ~ "09.2020-06.2021",
                                        trial == "COV-BARRIER" ~ "06.2020-01.2021",
                                        trial == "RECOVERY" ~ "02.2021-12.2021", 
                                        trial == "TACTIC-R" ~ "05.2020-05.2021",
                                        trial == "RUXCOVID" ~ "04.2020-09.2020",
                                        trial == "PANCOVID" ~ "10.2020-09.2021",
                                        ))
df_mort28 <- df_mort28 %>% 
  mutate(period = case_when(trial == "ACTT-2" ~ "pre-delta",
                                            trial == "COV-BARRIER" ~ "pre-delta",
                                            trial == "RECOVERY" ~ "pre-delta/delta",
                                            trial == "TOFACOV" ~ "pre-delta/delta",
                                            trial == "COVINIB" ~ "pre-delta/delta",
                                            trial == "Ghazaeian" ~ "pre-delta/delta",
                                            trial == "Bari-SolidAct" ~ "delta/omicron",
                                            trial == "TACTIC-R" ~ "pre-delta/delta",
                                            trial == "RUXCOVID" ~ "pre-delta",
                                            trial == "PANCOVID" ~ "pre-delta/delta",
                                        ))

df_mort28 <- df_mort28 %>% 
  mutate(rob_mort28 = case_when(trial == "ACTT-2" ~ "low risk",
                                trial == "COV-BARRIER" ~ "low risk",
                                trial == "RECOVERY" ~ "low risk",
                                trial == "TOFACOV" ~ "low risk",
                                trial == "COVINIB" ~ "low risk",
                                trial == "Ghazaeian" ~ "low risk",
                                trial == "Bari-SolidAct" ~ "low risk",
                                trial == "TACTIC-R" ~ "low risk",
                                trial == "RUXCOVID" ~ "low risk",
                                trial == "PANCOVID" ~ "low risk",
                                        ))
df_mort28 <- df_mort28 %>% 
  mutate('Data provided' = case_when(trial == "ACTT-2" ~ "IPD trials",
                                trial == "COV-BARRIER" ~ "IPD trials",
                                trial == "RECOVERY" ~ "IPD trials",
                                trial == "TOFACOV" ~ "IPD trials",
                                trial == "COVINIB" ~ "IPD trials",
                                trial == "Ghazaeian" ~ "IPD trials",
                                trial == "Bari-SolidAct" ~ "IPD trials",
                                trial == "TACTIC-R" ~ "IPD trials",
                                trial == "RUXCOVID" ~ "IPD trials",
                                trial == "PANCOVID" ~ "IPD trials",
                                        ))


#### read in aggregate data
## PRE-VENT
df_prevent <- read_excel("/Users/amstutzal/Library/CloudStorage/OneDrive-usb.ch/Dokumente - JAKi IPDMA data source management/General/non-IPD/JAKi_IPDMA_aggr_data.xlsx", sheet = "PRE-VENT")
# analyse with same model
addmargins(table(df_prevent$mort_28, df_prevent$trt, useNA = "always"))
```

```
##       
##          0   1 <NA> Sum
##   0     93  89    0 182
##   1      8  10    0  18
##   <NA>   0   0    0   0
##   Sum  101  99    0 200
```

```r
mort.28.prevent <- df_prevent %>% 
  glm(mort_28 ~ trt
      , family = "binomial", data=.)
summ(mort.28.prevent, exp = T, confint = T, model.info = T, model.fit = F, digits = 2)
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Observations </td>
   <td style="text-align:right;"> 200 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Dependent variable </td>
   <td style="text-align:right;"> mort_28 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Type </td>
   <td style="text-align:right;"> Generalized linear model </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Family </td>
   <td style="text-align:right;"> binomial </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Link </td>
   <td style="text-align:right;"> logit </td>
  </tr>
</tbody>
</table>  <table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> exp(Est.) </th>
   <th style="text-align:right;"> 2.5% </th>
   <th style="text-align:right;"> 97.5% </th>
   <th style="text-align:right;"> z val. </th>
   <th style="text-align:right;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> (Intercept) </td>
   <td style="text-align:right;"> 0.09 </td>
   <td style="text-align:right;"> 0.04 </td>
   <td style="text-align:right;"> 0.18 </td>
   <td style="text-align:right;"> -6.66 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> trt </td>
   <td style="text-align:right;"> 1.31 </td>
   <td style="text-align:right;"> 0.49 </td>
   <td style="text-align:right;"> 3.46 </td>
   <td style="text-align:right;"> 0.54 </td>
   <td style="text-align:right;"> 0.59 </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup></sup> Standard errors: MLE</td></tr></tfoot>
</table>

```r
# add effect estimates and other parameters to df_mort28
row_prevent <- tibble(
  variable = "death at day 28",
  hazard_odds_ratio = exp(coef(mort.28.prevent)["trt"]),
  ci_lower = exp(confint(mort.28.prevent)["trt", ])[1],
  ci_upper = exp(confint(mort.28.prevent)["trt", ])[2],
  standard_error = summary(mort.28.prevent)$coefficients["trt", "Std. Error"],
  p_value = summary(mort.28.prevent)$coefficients["trt", "Pr(>|z|)"],
  n_int = addmargins(table(df_prevent$mort_28, df_prevent$trt))[3,2],
  n_cont = addmargins(table(df_prevent$mort_28, df_prevent$trt))[3,1],
  e_int = addmargins(table(df_prevent$mort_28, df_prevent$trt))[2,2],
  e_cont = addmargins(table(df_prevent$mort_28, df_prevent$trt))[2,1],
  trial = "PRE-VENT*",
  JAKi = "Pacritinib",
  recruitment_period = "06.2020-02.2021",
  period = "pre-delta",
  rob_mort28 = "low risk",
  'Data provided' = "non-IPD trials")

## CAO
df_cao <- read_excel("/Users/amstutzal/Library/CloudStorage/OneDrive-usb.ch/Dokumente - JAKi IPDMA data source management/General/non-IPD/JAKi_IPDMA_aggr_data.xlsx", sheet = "CAO")
# analyse with same model
addmargins(table(df_cao$mort_28, df_cao$trt, useNA = "always"))
```

```
##       
##         0  1 <NA> Sum
##   0    18 20    0  38
##   1     3  0    0   3
##   <NA>  0  0    0   0
##   Sum  21 20    0  41
```

```r
mort.28.cao <- df_cao %>% 
  logistf(mort_28 ~ trt
      , family = "binomial", data=.)
summary(mort.28.cao)
```

```
## logistf(formula = mort_28 ~ trt, data = ., family = "binomial")
## 
## Model fitted by Penalized ML
## Coefficients:
##                  coef  se(coef) lower 0.95 upper 0.95     Chisq            p
## (Intercept) -1.665008 0.5828965  -2.996461 -0.6391919 11.219466 0.0008094379
## trt         -2.048564 1.5454930  -6.975291  0.3788351  2.610367 0.1061671667
##             method
## (Intercept)      2
## trt              2
## 
## Method: 1-Wald, 2-Profile penalized log-likelihood, 3-None
## 
## Likelihood ratio test=2.610367 on 1 df, p=0.1061672, n=41
## Wald test = 14.89037 on 1 df, p = 0.0001139429
```

```r
# add effect estimates and other parameters to df_mort28
row_cao <- tibble(
  variable = "death at day 28",
  hazard_odds_ratio = exp(coef(mort.28.cao)["trt"]),
  ci_lower = exp(mort.28.cao$ci.lower["trt"]),
  ci_upper = exp(mort.28.cao$ci.upper["trt"]),
  standard_error = sqrt(diag(vcov(mort.28.cao)))["trt"],
  p_value = mort.28.cao$prob["trt"],
  n_int = addmargins(table(df_cao$mort_28, df_cao$trt))[3,2],
  n_cont = addmargins(table(df_cao$mort_28, df_cao$trt))[3,1],
  e_int = addmargins(table(df_cao$mort_28, df_cao$trt))[2,2],
  e_cont = addmargins(table(df_cao$mort_28, df_cao$trt))[2,1],
  trial = "CAO*",
  JAKi = "Ruxolitinib",
  recruitment_period = "02.2020",
  period = "pre-delta",
  rob_mort28 = "low risk",
  'Data provided' = "non-IPD trials")

## STOP-COVID
df_stopcovid <- read_excel("/Users/amstutzal/Library/CloudStorage/OneDrive-usb.ch/Dokumente - JAKi IPDMA data source management/General/non-IPD/JAKi_IPDMA_aggr_data.xlsx", sheet = "STOP-COVID")
# analyse with same model
addmargins(table(df_stopcovid$mort_28, df_stopcovid$trt, useNA = "always"))
```

```
##       
##          0   1 <NA> Sum
##   0    137 140    0 277
##   1      8   4    0  12
##   <NA>   0   0    0   0
##   Sum  145 144    0 289
```

```r
mort.28.stopcovid <- df_stopcovid %>% 
  glm(mort_28 ~ trt
      , family = "binomial", data=.)
summ(mort.28.stopcovid, exp = T, confint = T, model.info = T, model.fit = F, digits = 2)
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Observations </td>
   <td style="text-align:right;"> 289 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Dependent variable </td>
   <td style="text-align:right;"> mort_28 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Type </td>
   <td style="text-align:right;"> Generalized linear model </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Family </td>
   <td style="text-align:right;"> binomial </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Link </td>
   <td style="text-align:right;"> logit </td>
  </tr>
</tbody>
</table>  <table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> exp(Est.) </th>
   <th style="text-align:right;"> 2.5% </th>
   <th style="text-align:right;"> 97.5% </th>
   <th style="text-align:right;"> z val. </th>
   <th style="text-align:right;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> (Intercept) </td>
   <td style="text-align:right;"> 0.06 </td>
   <td style="text-align:right;"> 0.03 </td>
   <td style="text-align:right;"> 0.12 </td>
   <td style="text-align:right;"> -7.81 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> trt </td>
   <td style="text-align:right;"> 0.49 </td>
   <td style="text-align:right;"> 0.14 </td>
   <td style="text-align:right;"> 1.66 </td>
   <td style="text-align:right;"> -1.15 </td>
   <td style="text-align:right;"> 0.25 </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup></sup> Standard errors: MLE</td></tr></tfoot>
</table>

```r
# add effect estimates and other parameters to df_mort28
row_stopcovid <- tibble(
  variable = "death at day 28",
  hazard_odds_ratio = exp(coef(mort.28.stopcovid)["trt"]),
  ci_lower = exp(confint(mort.28.stopcovid)["trt", ])[1],
  ci_upper = exp(confint(mort.28.stopcovid)["trt", ])[2],
  standard_error = summary(mort.28.stopcovid)$coefficients["trt", "Std. Error"],
  p_value = summary(mort.28.stopcovid)$coefficients["trt", "Pr(>|z|)"],
  n_int = addmargins(table(df_stopcovid$mort_28, df_stopcovid$trt))[3,2],
  n_cont = addmargins(table(df_stopcovid$mort_28, df_stopcovid$trt))[3,1],
  e_int = addmargins(table(df_stopcovid$mort_28, df_stopcovid$trt))[2,2],
  e_cont = addmargins(table(df_stopcovid$mort_28, df_stopcovid$trt))[2,1],
  trial = "STOP-COVID*",
  JAKi = "Tofacitinib",
  recruitment_period = "09.2020-12.2020",
  period = "pre-delta",
  rob_mort28 = "low risk",
  'Data provided' = "non-IPD trials")

## RUXCOVID-DEVENT
df_ruxcoviddevent <- read_excel("/Users/amstutzal/Library/CloudStorage/OneDrive-usb.ch/Dokumente - JAKi IPDMA data source management/General/non-IPD/JAKi_IPDMA_aggr_data.xlsx", sheet = "RUXCOVID-DEVENT")
# analyse with same model
addmargins(table(df_ruxcoviddevent$mort_28, df_ruxcoviddevent$trt, useNA = "always"))
```

```
##       
##          0   1 <NA> Sum
##   0     14  80    0  94
##   1     33  84    0 117
##   <NA>   0   0    0   0
##   Sum   47 164    0 211
```

```r
mort.28.ruxcoviddevent <- df_ruxcoviddevent %>% 
  glm(mort_28 ~ trt
      , family = "binomial", data=.)
summ(mort.28.ruxcoviddevent, exp = T, confint = T, model.info = T, model.fit = F, digits = 2)
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Observations </td>
   <td style="text-align:right;"> 211 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Dependent variable </td>
   <td style="text-align:right;"> mort_28 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Type </td>
   <td style="text-align:right;"> Generalized linear model </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Family </td>
   <td style="text-align:right;"> binomial </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Link </td>
   <td style="text-align:right;"> logit </td>
  </tr>
</tbody>
</table>  <table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> exp(Est.) </th>
   <th style="text-align:right;"> 2.5% </th>
   <th style="text-align:right;"> 97.5% </th>
   <th style="text-align:right;"> z val. </th>
   <th style="text-align:right;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> (Intercept) </td>
   <td style="text-align:right;"> 2.36 </td>
   <td style="text-align:right;"> 1.26 </td>
   <td style="text-align:right;"> 4.40 </td>
   <td style="text-align:right;"> 2.69 </td>
   <td style="text-align:right;"> 0.01 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> trt </td>
   <td style="text-align:right;"> 0.45 </td>
   <td style="text-align:right;"> 0.22 </td>
   <td style="text-align:right;"> 0.89 </td>
   <td style="text-align:right;"> -2.28 </td>
   <td style="text-align:right;"> 0.02 </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup></sup> Standard errors: MLE</td></tr></tfoot>
</table>

```r
# add effect estimates and other parameters to df_mort28
row_ruxcoviddevent <- tibble(
  variable = "death at day 28",
  hazard_odds_ratio = exp(coef(mort.28.ruxcoviddevent)["trt"]),
  ci_lower = exp(confint(mort.28.ruxcoviddevent)["trt", ])[1],
  ci_upper = exp(confint(mort.28.ruxcoviddevent)["trt", ])[2],
  standard_error = summary(mort.28.ruxcoviddevent)$coefficients["trt", "Std. Error"],
  p_value = summary(mort.28.ruxcoviddevent)$coefficients["trt", "Pr(>|z|)"],
  n_int = addmargins(table(df_ruxcoviddevent$mort_28, df_ruxcoviddevent$trt))[3,2],
  n_cont = addmargins(table(df_ruxcoviddevent$mort_28, df_ruxcoviddevent$trt))[3,1],
  e_int = addmargins(table(df_ruxcoviddevent$mort_28, df_ruxcoviddevent$trt))[2,2],
  e_cont = addmargins(table(df_ruxcoviddevent$mort_28, df_ruxcoviddevent$trt))[2,1],
  trial = "RUXCOVID-DEVENT*",
  JAKi = "Ruxolitinib",
  recruitment_period = "05.2020-12.2020",
  period = "pre-delta",
  rob_mort28 = "low risk",
  'Data provided' = "non-IPD trials")

## Dastan
df_dastan <- read_excel("/Users/amstutzal/Library/CloudStorage/OneDrive-usb.ch/Dokumente - JAKi IPDMA data source management/General/non-IPD/JAKi_IPDMA_aggr_data.xlsx", sheet = "Dastan")
# analyse with same model
addmargins(table(df_dastan$mort_28, df_dastan$trt, useNA = "always"))
```

```
##       
##         0  1 <NA> Sum
##   0    32 34    0  66
##   1     2  0    0   2
##   <NA>  0  0    0   0
##   Sum  34 34    0  68
```

```r
mort.28.dastan <- df_dastan %>% 
  logistf(mort_28 ~ trt
      , family = "binomial", data=.)
summary(mort.28.dastan)
```

```
## logistf(formula = mort_28 ~ trt, data = ., family = "binomial")
## 
## Model fitted by Penalized ML
## Coefficients:
##                  coef  se(coef) lower 0.95 upper 0.95     Chisq            p
## (Intercept) -2.564949 0.6563301  -4.152264 -1.4678989 30.507998 3.324930e-08
## trt         -1.669157 1.5683605  -6.610664  0.8879405  1.515226 2.183433e-01
##             method
## (Intercept)      2
## trt              2
## 
## Method: 1-Wald, 2-Profile penalized log-likelihood, 3-None
## 
## Likelihood ratio test=1.515226 on 1 df, p=0.2183433, n=68
## Wald test = 24.10837 on 1 df, p = 9.106353e-07
```

```r
# add effect estimates and other parameters to df_mort28
row_dastan <- tibble(
  variable = "death at day 28",
  hazard_odds_ratio = exp(coef(mort.28.dastan)["trt"]),
  ci_lower = exp(mort.28.dastan$ci.lower["trt"]),
  ci_upper = exp(mort.28.dastan$ci.upper["trt"]),
  standard_error = sqrt(diag(vcov(mort.28.dastan)))["trt"],
  p_value = mort.28.dastan$prob["trt"],
  n_int = addmargins(table(df_dastan$mort_28, df_dastan$trt))[3,2],
  n_cont = addmargins(table(df_dastan$mort_28, df_dastan$trt))[3,1],
  e_int = addmargins(table(df_dastan$mort_28, df_dastan$trt))[2,2],
  e_cont = addmargins(table(df_dastan$mort_28, df_dastan$trt))[2,1],
  trial = "Dastan*",
  JAKi = "Baricitinib",
  recruitment_period = "03.2022-08.2022",
  period = "delta/omicron",
  rob_mort28 = "Some concerns",
  'Data provided' = "non-IPD trials")

## Singh
df_singh <- read_excel("/Users/amstutzal/Library/CloudStorage/OneDrive-usb.ch/Dokumente - JAKi IPDMA data source management/General/non-IPD/JAKi_IPDMA_aggr_data.xlsx", sheet = "Singh")
# analyse with same model
addmargins(table(df_singh$mort_28, df_singh$trt, useNA = "always"))
```

```
##       
##          0   1 <NA> Sum
##   0     89  97    0 186
##   1     13   6    0  19
##   <NA>   0   0    0   0
##   Sum  102 103    0 205
```

```r
mort.28.singh <- df_singh %>% 
  glm(mort_28 ~ trt
      , family = "binomial", data=.)
summ(mort.28.singh, exp = T, confint = T, model.info = T, model.fit = F, digits = 2)
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Observations </td>
   <td style="text-align:right;"> 205 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Dependent variable </td>
   <td style="text-align:right;"> mort_28 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Type </td>
   <td style="text-align:right;"> Generalized linear model </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Family </td>
   <td style="text-align:right;"> binomial </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Link </td>
   <td style="text-align:right;"> logit </td>
  </tr>
</tbody>
</table>  <table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> exp(Est.) </th>
   <th style="text-align:right;"> 2.5% </th>
   <th style="text-align:right;"> 97.5% </th>
   <th style="text-align:right;"> z val. </th>
   <th style="text-align:right;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> (Intercept) </td>
   <td style="text-align:right;"> 0.15 </td>
   <td style="text-align:right;"> 0.08 </td>
   <td style="text-align:right;"> 0.26 </td>
   <td style="text-align:right;"> -6.48 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> trt </td>
   <td style="text-align:right;"> 0.42 </td>
   <td style="text-align:right;"> 0.15 </td>
   <td style="text-align:right;"> 1.16 </td>
   <td style="text-align:right;"> -1.67 </td>
   <td style="text-align:right;"> 0.10 </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup></sup> Standard errors: MLE</td></tr></tfoot>
</table>

```r
# add effect estimates and other parameters to df_mort28
row_singh <- tibble(
  variable = "death at day 28",
  hazard_odds_ratio = exp(coef(mort.28.singh)["trt"]),
  ci_lower = exp(confint(mort.28.singh)["trt", ])[1],
  ci_upper = exp(confint(mort.28.singh)["trt", ])[2],
  standard_error = summary(mort.28.singh)$coefficients["trt", "Std. Error"],
  p_value = summary(mort.28.singh)$coefficients["trt", "Pr(>|z|)"],
  n_int = addmargins(table(df_singh$mort_28, df_singh$trt))[3,2],
  n_cont = addmargins(table(df_singh$mort_28, df_singh$trt))[3,1],
  e_int = addmargins(table(df_singh$mort_28, df_singh$trt))[2,2],
  e_cont = addmargins(table(df_singh$mort_28, df_singh$trt))[2,1],
  trial = "Singh*",
  JAKi = "Nezulcitinib",
  recruitment_period = "06.2020-04.2021",
  period = "pre-delta/delta",
  rob_mort28 = "low risk",
  'Data provided' = "non-IPD trials")


# Add the new rows to your existing dataframe
df_mort28_agg <- bind_rows(df_mort28, row_prevent, row_cao, row_stopcovid, row_ruxcoviddevent, row_dastan, row_singh)
```
Discussion points:
* How much do the true effects vary, and over what specific interval?
a. The confidence interval tells us that the mean effect size in the universe of comparable studies probably falls in the interval -xxx to -xxx.
b. The prediction interval tells us that in any single study (selected at random from the universe of comparable studies) the true effect size will usually fall between -xxx and +xxx.
The confidence interval is based on the standard error of the mean and speaks to the precision of the mean. The prediction interval is based on the standard deviation of true effects and speaks to the dispersion of those effects.
Researchers often assume that if the effect is beneficial and statistically significant, it must be helpful in all populations. However, this is a mistake. The fact that an effect is statisti- cally significant tells us (for example) that the treatment is associated with a benefit on average. It may still be associ- ated with harm in some populations.
a. The results are statistically significant because the confidence interval excludes zero. However, this speaks only to the mean effect size.
b. The dispersion of effects is an entirely separate matter.

### Forestplot

```r
mort28.agg <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_mort28_agg,
                      n.e = n_int,
                      n.c = n_cont,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = T,
                      subgroup = df_mort28_agg$`Data provided`,
                      method.tau = "ML",
                      method.random.ci = "HK",
                      )
summary(mort28.agg)
```

```
##                      OR            95%-CI %W(random) `Data provided`
## Bari-SolidAct    0.6573 [0.3068;  1.4084]        5.8      IPD trials
## ACTT-2           0.7041 [0.3996;  1.2406]        9.5      IPD trials
## Ghazaeian        0.7909 [0.1654;  3.7807]        1.5      IPD trials
## TOFACOV          2.5366 [0.1928; 33.3748]        0.6      IPD trials
## COVINIB          0.1816 [0.0126;  2.6139]        0.5      IPD trials
## COV-BARRIER      0.5131 [0.3666;  0.7182]       19.5      IPD trials
## RECOVERY         0.8109 [0.7034;  0.9349]       37.0      IPD trials
## TACTIC-R         0.8119 [0.3571;  1.8458]        5.1      IPD trials
## RUXCOVID         1.4674 [0.3805;  5.6590]        2.0      IPD trials
## PANCOVID         0.2920 [0.0515;  1.6567]        1.2      IPD trials
## PRE-VENT*        1.3062 [0.4931;  3.4597]        3.7  non-IPD trials
## CAO*             0.1289 [0.0062;  2.6659]        0.4  non-IPD trials
## STOP-COVID*      0.4893 [0.1440;  1.6625]        2.4  non-IPD trials
## RUXCOVID-DEVENT* 0.4455 [0.2221;  0.8935]        6.8  non-IPD trials
## Dastan*          0.1884 [0.0087;  4.0746]        0.4  non-IPD trials
## Singh*           0.4235 [0.1544;  1.1618]        3.5  non-IPD trials
## 
## Number of studies: k = 16
## Number of observations: o = 13089
## 
##                               OR           95%-CI     t p-value
## Random effects model (HK) 0.6715 [0.5525; 0.8160] -4.36  0.0006
## Prediction interval              [0.4582; 0.9839]              
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0217 [0.0000; 0.3793]; tau = 0.1474 [0.0000; 0.6158]
##  I^2 = 13.5% [0.0%; 50.8%]; H = 1.08 [1.00; 1.43]
## 
## Test of heterogeneity:
##      Q d.f. p-value
##  17.35   15  0.2984
## 
## Results for subgroups (random effects model (HK)):
##                                    k     OR           95%-CI  tau^2    tau
## `Data provided` = IPD trials      10 0.7085 [0.5758; 0.8718] 0.0159 0.1262
## `Data provided` = non-IPD trials   6 0.5359 [0.2976; 0.9648]      0      0
##                                      Q   I^2
## `Data provided` = IPD trials     10.28 12.5%
## `Data provided` = non-IPD trials  5.01  0.2%
## 
## Test for subgroup differences (random effects model (HK)):
##                   Q d.f. p-value
## Between groups 1.28    1  0.2571
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 15)
## - Prediction interval based on t-distribution (df = 14)
```

```r
forest.meta(mort28.agg,
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aOR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "e_int", "n.c", "e_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "no JAKi", "Events\nno JAKi"),
            sortvar = +TE,
            text.random = "Average treatment effect   ",
            # xlim = c(0.10,5),
            label.left = "Favours JAKi",  
            label.right = "Favours No JAKi",
            overall.hetstat = T,
            test.subgroup = F
            )
```

![](two_stage_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
# Open a pdf file
pdf("./fp_mort_28_agg.pdf", width=13, height=7)

forestplot <- forest(mort28.agg,
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aOR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "e_int", "n.c", "e_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "no JAKi", "Events\nno JAKi"),
            sortvar = +TE,
            text.random = "Average treatment effect   ",
            # xlim = c(0.15,5),
            label.left = "Favours JAKi",
            label.right = "Favours No JAKi",
            overall.hetstat = T,
            test.subgroup = F,
            pooled.total = F
)

summary_row_y <- unit(0.142, "npc")
total_row <- c(sum(df_mort28_agg$n_int, na.rm = TRUE), 
               sum(df_mort28_agg$e_int, na.rm = TRUE), 
               sum(df_mort28_agg$n_cont, na.rm = TRUE),
               sum(df_mort28_agg$e_cont, na.rm = TRUE))

pushViewport(viewport())

# Add text annotations aligned with the summary row
grid.text(label = total_row[1], x = unit(0.318, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[2], x = unit(0.365, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[3], x = unit(0.42, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[4], x = unit(0.47, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))

popViewport()

dev.off()
```

```
## quartz_off_screen 
##                 2
```

### Funnel plot

```r
## funnel plot (contour enhanced)
funnel(mort28.agg)
```

![](two_stage_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
funnel(mort28.agg, common = TRUE,
  level = 0.95, contour = c(0.9, 0.95, 0.99),
  col.contour = c("grey", "lightgrey", "lightyellow"),
  lwd = 2, cex = 1.5, pch = 16, studlab = TRUE, cex.studlab = 1.0)
```

![](two_stage_files/figure-html/unnamed-chunk-9-2.png)<!-- -->

```r
# legend(0.05, 0.05,
#   c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"),
#   fill = c("darkgreen", "green", "lightgreen"))
# par(oldpar)
```

# (i.ii) Primary outcome: Subgroup by JAKi, including the non-IPD RCTs

```r
# meta-regression by JAKi / or as subgroup?
# forest(mort28.agg.jaki, layout = "subgroup", calcwidth.hetstat = TRUE) // use custom design instead

mort28.agg.jaki <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_mort28_agg,
                      n.e = n_int,
                      n.c = n_cont,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = T,
                      subgroup = JAKi,
                      method.tau = "ML",
                      method.random.ci = "HK",
                      )
summary(mort28.agg.jaki)
```

```
##                      OR            95%-CI %W(random)         JAKi
## Bari-SolidAct    0.6573 [0.3068;  1.4084]        5.8  Baricitinib
## ACTT-2           0.7041 [0.3996;  1.2406]        9.5  Baricitinib
## Ghazaeian        0.7909 [0.1654;  3.7807]        1.5  Tofacitinib
## TOFACOV          2.5366 [0.1928; 33.3748]        0.6  Tofacitinib
## COVINIB          0.1816 [0.0126;  2.6139]        0.5  Baricitinib
## COV-BARRIER      0.5131 [0.3666;  0.7182]       19.5  Baricitinib
## RECOVERY         0.8109 [0.7034;  0.9349]       37.0  Baricitinib
## TACTIC-R         0.8119 [0.3571;  1.8458]        5.1  Baricitinib
## RUXCOVID         1.4674 [0.3805;  5.6590]        2.0  Ruxolitinib
## PANCOVID         0.2920 [0.0515;  1.6567]        1.2  Baricitinib
## PRE-VENT*        1.3062 [0.4931;  3.4597]        3.7   Pacritinib
## CAO*             0.1289 [0.0062;  2.6659]        0.4  Ruxolitinib
## STOP-COVID*      0.4893 [0.1440;  1.6625]        2.4  Tofacitinib
## RUXCOVID-DEVENT* 0.4455 [0.2221;  0.8935]        6.8  Ruxolitinib
## Dastan*          0.1884 [0.0087;  4.0746]        0.4  Baricitinib
## Singh*           0.4235 [0.1544;  1.1618]        3.5 Nezulcitinib
## 
## Number of studies: k = 16
## Number of observations: o = 13089
## 
##                               OR           95%-CI     t p-value
## Random effects model (HK) 0.6715 [0.5525; 0.8160] -4.36  0.0006
## Prediction interval              [0.4582; 0.9839]              
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0217 [0.0000; 0.3793]; tau = 0.1474 [0.0000; 0.6158]
##  I^2 = 13.5% [0.0%; 50.8%]; H = 1.08 [1.00; 1.43]
## 
## Test of heterogeneity:
##      Q d.f. p-value
##  17.35   15  0.2984
## 
## Results for subgroups (random effects model (HK)):
##                       k     OR           95%-CI   tau^2    tau    Q   I^2
## JAKi = Baricitinib    8 0.6797 [0.5394; 0.8564]  0.0205 0.1431 9.24 24.2%
## JAKi = Tofacitinib    3 0.7025 [0.1412; 3.4951]       0      0 1.31  0.0%
## JAKi = Ruxolitinib    3 0.5391 [0.0986; 2.9474] <0.0001 0.0004 3.26 38.6%
## JAKi = Pacritinib     1 1.3062 [0.4931; 3.4597]      --     -- 0.00    --
## JAKi = Nezulcitinib   1 0.4235 [0.1544; 1.1618]      --     -- 0.00    --
## 
## Test for subgroup differences (random effects model (HK)):
##                   Q d.f. p-value
## Between groups 2.92    4  0.5708
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 15)
## - Prediction interval based on t-distribution (df = 14)
```

```r
forest.meta(mort28.agg.jaki,
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aOR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "e_int", "n.c", "e_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "no JAKi", "Events\nno JAKi"),
            sortvar = +TE,
            text.random = "Average treatment effect   ",
            # xlim = c(0.10,5),
            label.left = "Favours JAKi",  
            label.right = "Favours No JAKi",
            overall.hetstat = F,
            test.subgroup = T
            )
```

![](two_stage_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
# Open a pdf file
pdf("./fp_mort_28_JAKi.pdf", width=13, height=9)

forestplot <- forest(mort28.agg.jaki,
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aOR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "e_int", "n.c", "e_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "no JAKi", "Events\nno JAKi"),
            sortvar = +TE,
            text.random = "Average treatment effect   ",
            # xlim = c(0.15,5),
            label.left = "Favours JAKi",
            label.right = "Favours No JAKi",
            test.subgroup = T,
            pooled.total = F
)

summary_row_y <- unit(0.143, "npc")
total_row <- c(sum(df_mort28_agg$n_int, na.rm = TRUE), 
               sum(df_mort28_agg$e_int, na.rm = TRUE), 
               sum(df_mort28_agg$n_cont, na.rm = TRUE),
               sum(df_mort28_agg$e_cont, na.rm = TRUE))
pushViewport(viewport())
grid.text(label = total_row[1], x = unit(0.317, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[2], x = unit(0.365, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[3], x = unit(0.42, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[4], x = unit(0.47, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
popViewport()

dev.off()
```

```
## quartz_off_screen 
##                 2
```

# (i.ii) Primary outcome: Meta-regression by JAKi, including the non-IPD RCTs

```r
mort28.agg.jaki <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_mort28_agg,
                      n.e = n_int,
                      n.c = n_cont,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = T,
                      # subgroup = JAKi,
                      method.tau = "ML",
                      method.random.ci = "HK",
                      )
summary(mort28.agg.jaki)
```

```
##                      OR            95%-CI %W(random)
## Bari-SolidAct    0.6573 [0.3068;  1.4084]        5.8
## ACTT-2           0.7041 [0.3996;  1.2406]        9.5
## Ghazaeian        0.7909 [0.1654;  3.7807]        1.5
## TOFACOV          2.5366 [0.1928; 33.3748]        0.6
## COVINIB          0.1816 [0.0126;  2.6139]        0.5
## COV-BARRIER      0.5131 [0.3666;  0.7182]       19.5
## RECOVERY         0.8109 [0.7034;  0.9349]       37.0
## TACTIC-R         0.8119 [0.3571;  1.8458]        5.1
## RUXCOVID         1.4674 [0.3805;  5.6590]        2.0
## PANCOVID         0.2920 [0.0515;  1.6567]        1.2
## PRE-VENT*        1.3062 [0.4931;  3.4597]        3.7
## CAO*             0.1289 [0.0062;  2.6659]        0.4
## STOP-COVID*      0.4893 [0.1440;  1.6625]        2.4
## RUXCOVID-DEVENT* 0.4455 [0.2221;  0.8935]        6.8
## Dastan*          0.1884 [0.0087;  4.0746]        0.4
## Singh*           0.4235 [0.1544;  1.1618]        3.5
## 
## Number of studies: k = 16
## Number of observations: o = 13089
## 
##                               OR           95%-CI     t p-value
## Random effects model (HK) 0.6715 [0.5525; 0.8160] -4.36  0.0006
## Prediction interval              [0.4582; 0.9839]              
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0217 [0.0000; 0.3793]; tau = 0.1474 [0.0000; 0.6158]
##  I^2 = 13.5% [0.0%; 50.8%]; H = 1.08 [1.00; 1.43]
## 
## Test of heterogeneity:
##      Q d.f. p-value
##  17.35   15  0.2984
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 15)
## - Prediction interval based on t-distribution (df = 14)
```

```r
mort28.agg.jaki.mreg <- metareg(mort28.agg.jaki, ~JAKi)
mort28.agg.jaki.mreg
```

```
## 
## Mixed-Effects Model (k = 16; tau^2 estimator: ML)
## 
## tau^2 (estimated amount of residual heterogeneity):     0.0158 (SE = 0.0258)
## tau (square root of estimated tau^2 value):             0.1256
## I^2 (residual heterogeneity / unaccounted variability): 13.37%
## H^2 (unaccounted variability / sampling variability):   1.15
## R^2 (amount of heterogeneity accounted for):            27.41%
## 
## Test for Residual Heterogeneity:
## QE(df = 11) = 13.8078, p-val = 0.2438
## 
## Test of Moderators (coefficients 2:5):
## F(df1 = 4, df2 = 11) = 0.7718, p-val = 0.5657
## 
## Model Results:
## 
##                   estimate      se     tval  df    pval    ci.lb    ci.ub     
## intrcpt            -0.3757  0.1005  -3.7400  11  0.0033  -0.5968  -0.1546  ** 
## JAKiNezulcitinib   -0.4835  0.5235  -0.9237  11  0.3755  -1.6357   0.6686     
## JAKiPacritinib      0.6428  0.5069   1.2681  11  0.2310  -0.4729   1.7586     
## JAKiRuxolitinib    -0.2312  0.3301  -0.7004  11  0.4982  -0.9578   0.4953     
## JAKiTofacitinib     0.0281  0.4643   0.0604  11  0.9529  -0.9939   1.0500     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

# (i.iii) Primary outcome: Subgroup by period, including the non-IPD RCTs

```r
mort28.agg.period <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_mort28_agg,
                      n.e = n_int,
                      n.c = n_cont,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = T,
                      subgroup = period,
                      method.tau = "ML",
                      method.random.ci = "HK",
                      )
summary(mort28.agg.period)
```

```
##                      OR            95%-CI %W(random)          period
## Bari-SolidAct    0.6573 [0.3068;  1.4084]        5.8   delta/omicron
## ACTT-2           0.7041 [0.3996;  1.2406]        9.5       pre-delta
## Ghazaeian        0.7909 [0.1654;  3.7807]        1.5 pre-delta/delta
## TOFACOV          2.5366 [0.1928; 33.3748]        0.6 pre-delta/delta
## COVINIB          0.1816 [0.0126;  2.6139]        0.5 pre-delta/delta
## COV-BARRIER      0.5131 [0.3666;  0.7182]       19.5       pre-delta
## RECOVERY         0.8109 [0.7034;  0.9349]       37.0 pre-delta/delta
## TACTIC-R         0.8119 [0.3571;  1.8458]        5.1 pre-delta/delta
## RUXCOVID         1.4674 [0.3805;  5.6590]        2.0       pre-delta
## PANCOVID         0.2920 [0.0515;  1.6567]        1.2 pre-delta/delta
## PRE-VENT*        1.3062 [0.4931;  3.4597]        3.7       pre-delta
## CAO*             0.1289 [0.0062;  2.6659]        0.4       pre-delta
## STOP-COVID*      0.4893 [0.1440;  1.6625]        2.4       pre-delta
## RUXCOVID-DEVENT* 0.4455 [0.2221;  0.8935]        6.8       pre-delta
## Dastan*          0.1884 [0.0087;  4.0746]        0.4   delta/omicron
## Singh*           0.4235 [0.1544;  1.1618]        3.5 pre-delta/delta
## 
## Number of studies: k = 16
## Number of observations: o = 13089
## 
##                               OR           95%-CI     t p-value
## Random effects model (HK) 0.6715 [0.5525; 0.8160] -4.36  0.0006
## Prediction interval              [0.4582; 0.9839]              
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0217 [0.0000; 0.3793]; tau = 0.1474 [0.0000; 0.6158]
##  I^2 = 13.5% [0.0%; 50.8%]; H = 1.08 [1.00; 1.43]
## 
## Test of heterogeneity:
##      Q d.f. p-value
##  17.35   15  0.2984
## 
## Results for subgroups (random effects model (HK)):
##                            k     OR            95%-CI   tau^2    tau    Q   I^2
## period = delta/omicron     2 0.6114 [0.0150; 24.9337]       0      0 0.60  0.0%
## period = pre-delta         7 0.5822 [0.4172;  0.8126] <0.0001 0.0026 7.02 14.5%
## period = pre-delta/delta   7 0.7954 [0.6821;  0.9275]       0      0 4.81  0.0%
## 
## Test for subgroup differences (random effects model (HK)):
##                   Q d.f. p-value
## Between groups 4.81    2  0.0901
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 15)
## - Prediction interval based on t-distribution (df = 14)
```

```r
forest.meta(mort28.agg.period,
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aOR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "e_int", "n.c", "e_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "no JAKi", "Events\nno JAKi"),
            sortvar = +TE,
            text.random = "Average treatment effect   ",
            # xlim = c(0.10,5),
            label.left = "Favours JAKi",  
            label.right = "Favours No JAKi",
            overall.hetstat = F,
            test.subgroup = T
            )
```

![](two_stage_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
# Open a pdf file
pdf("./fp_mort_28_period.pdf", width=13, height=9)

forestplot <- forest(mort28.agg.period,
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aOR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "e_int", "n.c", "e_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "no JAKi", "Events\nno JAKi"),
            sortvar = +TE,
            text.random = "Average treatment effect   ",
            # xlim = c(0.15,5),
            label.left = "Favours JAKi",
            label.right = "Favours No JAKi",
            test.subgroup = T,
            pooled.total = T
)

summary_row_y <- unit(0.189, "npc")
total_row <- c(sum(df_mort28_agg$n_int, na.rm = TRUE), 
               sum(df_mort28_agg$e_int, na.rm = TRUE), 
               sum(df_mort28_agg$n_cont, na.rm = TRUE),
               sum(df_mort28_agg$e_cont, na.rm = TRUE))
pushViewport(viewport())
grid.text(label = total_row[1], x = unit(0.316, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[2], x = unit(0.365, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[3], x = unit(0.4189, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[4], x = unit(0.47, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
popViewport()

dev.off()
```

```
## quartz_off_screen 
##                 2
```

# (i.iv) Primary outcome: Meta-regression by RoB, including the non-IPD RCTs // all low risk!

```r
# # meta-regression by RoB
# mort28.agg.rob <- update.meta(mort28.agg,
#                                subgroup = rob_mort28)
# forest.meta(mort28.agg.rob,
#             leftcols = c("studlab",
#                          # "TE",
#                          # "seTE",
#                          "n.e"),
#             leftlabs = c("Trial",
#                          # "log(OR)",
#                          # "Standard Error",
#                          "Sample Size"),
#             sortvar = +TE,
#             test.subgroup.random = TRUE,
#             text.random = "Average treatment effect (RE model)",
#             title = "Average treatment effect - mortality 28 days",
#             xlim = c(0.03,30),
#             # xlab = "Average treatment effect (95% CI)"
#             )
```

# (i.v) Primary outcome: Covariate-adjusted Average marginal effects (RDs)

```r
mort28.ame <- metagen(TE = hazard_odds_ratio,
                      seTE = standard_error,
                      studlab = trial,
                      data = df_mort28_ame,
                      n.e = n_int + n_cont,
                      # n.c = n_control,
                      # sm = "SMD",
                      fixed = F,
                      random = T,
                      prediction = F,
                      method.tau = "ML", 
                      hakn = T,
                      adhoc.hakn.ci = "", 
                      title = "Covariate-adjusted average marginal effect - mortality 28 days",
                      # exclude = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # include in forestplot but exclude from analysis
                      )
summary(mort28.ame)
```

```
## Review:     Covariate-adjusted average marginal effect - mortality 28 days
## 
##                                   95%-CI %W(random)
## Bari-SolidAct -0.0411 [-0.1170;  0.0348]        3.9
## ACTT-2        -0.0177 [-0.0465;  0.0111]       16.4
## Ghazaeian     -0.0155 [-0.1229;  0.0920]        2.0
## TOFACOV        0.0150 [-0.0186;  0.0487]       13.7
## COVINIB       -0.0377 [-0.0879;  0.0125]        7.8
## COV-BARRIER   -0.0621 [-0.0934; -0.0308]       14.9
## RECOVERY      -0.0200 [-0.0336; -0.0063]       28.3
## TACTIC-R      -0.0183 [-0.0966;  0.0601]        3.7
## PANCOVID      -0.0311 [-0.0755;  0.0133]        9.4
## 
## Number of studies: k = 9
## Number of observations: o = 11651
## 
##                                               95%-CI     t p-value
## Random effects model (HK) -0.0242 [-0.0418; -0.0065] -3.16  0.0135
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0002 [0.0000; 0.0013]; tau = 0.0135 [0.0000; 0.0366]
##  I^2 = 33.0% [0.0%; 69.1%]; H = 1.22 [1.00; 1.80]
## 
## Test of heterogeneity:
##      Q d.f. p-value
##  11.94    8  0.1540
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 8)
```

```r
forest.meta(mort28.ame,
            hetstat = T,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE", "n.e"),
            leftlabs = c("Trial", "Risk Difference", "Standard Error", "Sample Size"),
            # text.common = "Average marginal effect (common effects model)*",
            text.random = "Average marginal effect (random effects model)*",
            title = "Covariate-adjusted average marginal effect - mortality 28 days",
            # xlim = c(-0.01,0.1),
            sortvar = +TE,
            # xlab = "Covariate-adjusted average marginal effect (95% CI)"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
*Covariate-Adjusted Analysis for Marginal Estimands based on https://arxiv.org/abs/2306.05823 & FDA guidance https://www.fda.gov/media/148910/download

# (i) Primary outcome: Deterministic imputation

```r
mort28.dimp <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_mort28_dimp,
                      n.e = n_int + n_cont,
                      # n.c = n_control,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = T,
                      method.tau = "ML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Average treatment effect, deterministic imputation - mortality 28 days",
                      # subset = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # exclude entirely
                      # exclude = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # include in forestplot but exclude from analysis
                      )
summary(mort28.dimp)
```

```
## Review:     Average treatment effect, deterministic imputation - mortality 2 ...
## 
##                   OR            95%-CI %W(random)
## Bari-SolidAct 0.6404 [0.2998;  1.3677]        6.0
## ACTT-2        0.7368 [0.4197;  1.2933]       10.2
## Ghazaeian     0.7909 [0.1654;  3.7807]        1.5
## TOFACOV       2.5366 [0.1928; 33.3748]        0.6
## COVINIB       0.1822 [0.0127;  2.6082]        0.5
## COV-BARRIER   0.5122 [0.3674;  0.7141]       22.8
## RECOVERY      0.8030 [0.6969;  0.9253]       49.7
## TACTIC-R      0.8966 [0.4011;  2.0045]        5.4
## RUXCOVID      1.4777 [0.3832;  5.6984]        2.0
## PANCOVID      0.2836 [0.0498;  1.6143]        1.2
## 
## Number of studies: k = 10
## Number of observations: o = 12402
## 
##                               OR           95%-CI     t p-value
## Random effects model (HK) 0.7115 [0.5779; 0.8760] -3.70  0.0049
## Prediction interval              [0.4957; 1.0213]              
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0147 [0.0000; 0.6833]; tau = 0.1211 [0.0000; 0.8266]
##  I^2 = 13.7% [0.0%; 55.0%]; H = 1.08 [1.00; 1.49]
## 
## Test of heterogeneity:
##      Q d.f. p-value
##  10.43    9  0.3167
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 9)
## - Prediction interval based on t-distribution (df = 8)
```

```r
forest.meta(mort28.dimp,
            # hetstat = T,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE", "n.e"),
            leftlabs = c("Trial", "log(OR)", "Standard Error", "Sample Size"),
            text.random = "Average treatment effect (random effects model)",
            title = "Average treatment effect, deterministic imputation - mortality 28 days", # get the title into the figure
            xlim = c(0.15,5),
            sortvar = +TE,
            # xlab = "Average treatment effect (95% CI)"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
Discussion points:

# (i.vii) Primary outcome: Multiple imputation

```r
## Mortality at day 28: multiple imputation

##### ONLY WORKS IF NEW MI ESTIMATES
# outcomes <- "death at day 28_mi"
# outcomes.firth <- "death at day 28_mi_firth"
# df_mort28_mi <- data.frame()
# for (df in list_df) {
#   selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
#   df_mort28_mi <- rbind(df_mort28_mi, selected_rows)
# }
# # Save
# saveRDS(df_mort28_mi, file = "trt_effects_mi.RData") # collected on 7.5.24: Bari-Solidact, ACTT2, RECOVERY, COV-BARRIER, TACTIC-R
##### ONLY WORKS IF NEW MI ESTIMATES

##### ONLY RUN IF NEW MI ESTIMATES
# ## UPDATE on 14.5.24
# df_mort28_mi <- readRDS("trt_effects_mi.RData") # use the one above
# df_pancovid_mi <- readRDS("trt_effects_pancovid.RData")
# df_pancovid_mi <- df_pancovid_mi %>% # add PANCOVID
#   filter(variable == "death at day 28_mi")
# df_ruxcovid_mi <- readRDS("trt_effects_ruxcovid.RData")
# df_ruxcovid_mi <- df_ruxcovid_mi %>% # add RUXCOVID
#   filter(variable == "death at day 28_mi")
# # %>%
# #   mutate(JAKi = case_when(JAKi == "Baricitinib" ~ "Ruxolitinib", ## shoud be fine now, double-check
# #                           TRUE ~ JAKi))
# df_mort28_mi <- rbind(df_mort28_mi, df_ruxcovid_mi, df_pancovid_mi)
# Save
# saveRDS(df_mort28_mi, file = "trt_effects_mi_14052024.RData") # updated on 14.5.24: incl. PANCOVID and RUXCOVID
##### ONLY RUN IF NEW MI ESTIMATES

# LOAD
df_mort28_mi <- readRDS("trt_effects_mi_14052024.RData")
## adapt dataframe to include the event columns
df_mort28_add_events <- df_mort28_dimp %>%
  filter(trial %in% c("Bari-SolidAct", "ACTT-2", "COV-BARRIER", "RECOVERY", "TACTIC-R", 
                      "RUXCOVID", 
                      "PANCOVID")) %>% 
  dplyr::select(trial, n_int, n_cont, e_int, e_cont)
df_mort28_mi <- left_join(df_mort28_mi, df_mort28_add_events, by = join_by(trial == trial)) 
df_mort28_mi <- df_mort28_mi %>% 
  dplyr::select(!n_intervention) %>% 
  dplyr::select(!n_control)

# no MI from ghazaeian, tofacov -> add their df_mort28 estimates and covinib
df_mort28_mi_add <- df_mort28_dimp %>%
  filter(trial == "Ghazaeian" | trial == "TOFACOV" | trial == "COVINIB")
df_mort28_mi_ext <- rbind(df_mort28_mi, df_mort28_mi_add)

mort28.mi <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_mort28_mi_ext,
                      n.e = n_int,
                      n.c = n_cont,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = T,
                      method.tau = "ML",
                      method.random.ci = "HK"
                      )
summary(mort28.mi)
```

```
##                   OR            95%-CI %W(random)
## Bari-SolidAct 0.6486 [0.3031;  1.3877]        6.3
## ACTT-2        0.7344 [0.4147;  1.3007]       10.4
## COV-BARRIER   0.5106 [0.3645;  0.7152]       22.9
## RECOVERY      0.8118 [0.7040;  0.9361]       48.3
## TACTIC-R      1.0507 [0.4769;  2.3152]        5.9
## RUXCOVID      1.3425 [0.3504;  5.1438]        2.2
## PANCOVID      0.2840 [0.0500;  1.6133]        1.3
## Ghazaeian     0.7909 [0.1654;  3.7807]        1.6
## TOFACOV       2.5366 [0.1928; 33.3748]        0.6
## COVINIB       0.1822 [0.0127;  2.6082]        0.6
## 
## Number of studies: k = 10
## Number of observations: o = 12402
## 
##                               OR           95%-CI     t p-value
## Random effects model (HK) 0.7198 [0.5791; 0.8948] -3.42  0.0077
## Prediction interval              [0.4926; 1.0518]              
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0165 [0.0000; 0.6869]; tau = 0.1285 [0.0000; 0.8288]
##  I^2 = 17.2% [0.0%; 58.3%]; H = 1.10 [1.00; 1.55]
## 
## Test of heterogeneity:
##      Q d.f. p-value
##  10.87    9  0.2846
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 9)
## - Prediction interval based on t-distribution (df = 8)
```

```r
forest(mort28.mi, 
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aOR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "e_int", "n.c", "e_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "no JAKi", "Events\nno JAKi"),
            text.random = "Average treatment effect",
            sortvar = +TE,
            label.left = "Favours JAKi",
            label.right = "Favours No JAKi",
            )
```

![](two_stage_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

# (ii) Mortality at day 60

```r
mort60 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_mort60,
                      n.e = n_int,
                      n.c = n_cont,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = T,
                      method.tau = "ML",
                      method.random.ci = "HK"
                      )
summary(mort60)
```

```
##                   OR            95%-CI %W(random)
## Bari-SolidAct 0.9170 [0.4640;  1.8125]        3.2
## ACTT-2        0.7041 [0.3996;  1.2406]        4.6
## Ghazaeian     0.7909 [0.1654;  3.7807]        0.6
## TOFACOV       2.5366 [0.1928; 33.3748]        0.2
## COVINIB       0.1816 [0.0126;  2.6139]        0.2
## COV-BARRIER   0.5656 [0.4133;  0.7740]       14.9
## RECOVERY      0.8109 [0.7034;  0.9349]       72.3
## TACTIC-R      0.9559 [0.4482;  2.0384]        2.6
## RUXCOVID      1.4674 [0.3805;  5.6590]        0.8
## PANCOVID      0.3737 [0.0876;  1.5941]        0.7
## 
## Number of studies: k = 10
## Number of observations: o = 12046
## 
##                               OR           95%-CI     t p-value
## Random effects model (HK) 0.7688 [0.6703; 0.8817] -4.34  0.0019
## Prediction interval              [0.6668; 0.8864]              
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.5345]; tau = 0 [0.0000; 0.7311]
##  I^2 = 0.0% [0.0%; 62.4%]; H = 1.00 [1.00; 1.63]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  8.67    9  0.4686
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 9)
## - Prediction interval based on t-distribution (df = 8)
```

```r
forest(mort60, 
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aOR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "e_int", "n.c", "e_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "no JAKi", "Events\nno JAKi"),
            text.random = "Average treatment effect",
            sortvar = +TE,
            label.left = "Favours JAKi",
            label.right = "Favours No JAKi",
            )
```

![](two_stage_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

```r
# Open a pdf file
pdf("./fp_mort_60.pdf", width=13, height=5)

forestplot <- forest(mort60, 
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aOR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "e_int", "n.c", "e_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "no JAKi", "Events\nno JAKi"),
            text.random = "Average treatment effect",
            sortvar = +TE,
            label.left = "Favours JAKi",
            label.right = "Favours No JAKi",
            pooled.total = F
            )
summary_row_y <- unit(0.26, "npc")
total_row <- c(sum(df_mort60$n_int, na.rm = TRUE), 
               sum(df_mort60$e_int, na.rm = TRUE), 
               sum(df_mort60$n_cont, na.rm = TRUE),
               sum(df_mort60$e_cont, na.rm = TRUE))
pushViewport(viewport())
grid.text(label = total_row[1], x = unit(0.313, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[2], x = unit(0.358, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[3], x = unit(0.415, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[4], x = unit(0.461, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
popViewport()

dev.off()
```

```
## quartz_off_screen 
##                 2
```
Discussion points

# (iii) Time to death within max. follow-up time

```r
ttdeath <- metagen(TE = log(hazard_odds_ratio), # check COVINIB and TOFACOV
                      seTE = standard_error,
                      studlab = trial,
                      data = df_ttdeath,
                      n.e = n_int,
                      n.c = n_cont,
                      sm = "HR",
                      fixed = F,
                      random = T,
                      prediction = T,
                      method.tau = "ML",
                      method.random.ci = "HK"
                      )
summary(ttdeath)
```

```
##                   HR             95%-CI %W(random)
## Bari-SolidAct 0.8356 [0.4670;   1.4952]        5.9
## ACTT-2        0.7409 [0.4415;   1.2434]        7.3
## Ghazaeian     0.8133 [0.1819;   3.6354]        0.9
## TOFACOV       3.0000 [0.0325; 277.2599]        0.1
## COVINIB       0.1748 [0.0039;   7.8470]        0.1
## COV-BARRIER   0.6131 [0.4760;   0.7895]       24.0
## RECOVERY      0.8443 [0.7483;   0.9526]       53.9
## TACTIC-R      1.0670 [0.5778;   1.9706]        5.3
## RUXCOVID      1.4336 [0.3855;   5.3314]        1.2
## PANCOVID      0.3777 [0.0930;   1.5343]        1.1
## 
## Number of studies: k = 10
## Number of observations: o = 12402
## 
##                               HR           95%-CI     t p-value
## Random effects model (HK) 0.7808 [0.6734; 0.9053] -3.78  0.0043
## Prediction interval              [0.6054; 1.0070]              
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0066 [0.0000; 0.2365]; tau = 0.0811 [0.0000; 0.4863]
##  I^2 = 0.0% [0.0%; 62.4%]; H = 1.00 [1.00; 1.63]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  8.77    9  0.4587
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 9)
## - Prediction interval based on t-distribution (df = 8)
```

```r
forest(ttdeath, 
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aHR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "e_int", "n.c", "e_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "no JAKi", "Events\nno JAKi"),
            text.random = "Average treatment effect",
            sortvar = +TE,
            label.left = "Favours JAKi",
            label.right = "Favours No JAKi",
            )
```

![](two_stage_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

```r
# Open a pdf file
pdf("./fp_ttdeath.pdf", width=13, height=5)

forestplot <- forest(ttdeath, 
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aHR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "e_int", "n.c", "e_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "no JAKi", "Events\nno JAKi"),
            text.random = "Average treatment effect",
            sortvar = +TE,
            label.left = "Favours JAKi",
            label.right = "Favours No JAKi",
            pooled.total = F
            )

summary_row_y <- unit(0.26, "npc")
total_row <- c(sum(df_ttdeath$n_int, na.rm = TRUE), 
               sum(df_ttdeath$e_int, na.rm = TRUE), 
               sum(df_ttdeath$n_cont, na.rm = TRUE),
               sum(df_ttdeath$e_cont, na.rm = TRUE))
pushViewport(viewport())
grid.text(label = total_row[1], x = unit(0.31, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[2], x = unit(0.355, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[3], x = unit(0.414, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[4], x = unit(0.46, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))

popViewport()

dev.off()
```

```
## quartz_off_screen 
##                 2
```
Discussion points

# (iv) New mechanical ventilation or death within 28 days

```r
new.mvd28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_new_mvd28,
                      n.e = n_int,
                      n.c = n_cont,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = T,
                      method.tau = "ML",
                      method.random.ci = "HK"
                      )
summary(new.mvd28)
```

```
##                   OR           95%-CI %W(random)
## Bari-SolidAct 1.0501 [0.6091; 1.8106]        3.6
## ACTT-2        0.6874 [0.4680; 1.0096]        7.3
## Ghazaeian     0.7909 [0.1654; 3.7807]        0.4
## TOFACOV       0.5034 [0.0417; 6.0761]        0.2
## COVINIB       0.1994 [0.0393; 1.0107]        0.4
## COV-BARRIER   0.8235 [0.6209; 1.0922]       13.5
## RECOVERY      0.8153 [0.7201; 0.9232]       69.6
## TACTIC-R      0.8714 [0.4408; 1.7225]        2.3
## RUXCOVID      1.2640 [0.5321; 3.0025]        1.4
## PANCOVID      0.6133 [0.2380; 1.5807]        1.2
## 
## Number of studies: k = 10
## Number of observations: o = 12151
## 
##                               OR           95%-CI     t p-value
## Random effects model (HK) 0.8119 [0.7364; 0.8952] -4.83  0.0009
## Prediction interval              [0.7187; 0.9173]              
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.3518]; tau = 0 [0.0000; 0.5931]
##  I^2 = 0.0% [0.0%; 62.4%]; H = 1.00 [1.00; 1.63]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  5.99    9  0.7407
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 9)
## - Prediction interval based on t-distribution (df = 8)
```

```r
forest(new.mvd28, 
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aOR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "e_int", "n.c", "e_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "no JAKi", "Events\nno JAKi"),
            text.random = "Average treatment effect",
            sortvar = +TE,
            label.left = "Favours JAKi",
            label.right = "Favours No JAKi",
            )
```

![](two_stage_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

```r
# Open a pdf file
pdf("./fp_newmvd28.pdf", width=13, height=5)

forestplot <- forest(new.mvd28, 
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aOR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "e_int", "n.c", "e_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "no JAKi", "Events\nno JAKi"),
            text.random = "Average treatment effect",
            sortvar = +TE,
            label.left = "Favours JAKi",
            label.right = "Favours No JAKi",
            pooled.total = F
            )

summary_row_y <- unit(0.26, "npc")
total_row <- c(sum(df_new_mvd28$n_int, na.rm = TRUE), 
               sum(df_new_mvd28$e_int, na.rm = TRUE), 
               sum(df_new_mvd28$n_cont, na.rm = TRUE),
               sum(df_new_mvd28$e_cont, na.rm = TRUE))
pushViewport(viewport())
grid.text(label = total_row[1], x = unit(0.315, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[2], x = unit(0.357, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[3], x = unit(0.418, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[4], x = unit(0.46, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))

popViewport()

dev.off()
```

```
## quartz_off_screen 
##                 2
```
Discussion points

# (iv.i) New mechanical ventilation among survivors within 28 days

```r
new.mv28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_new_mv28,
                      n.e = n_int + n_cont,
                      # n.c = n_control,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = T,
                      method.tau = "ML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
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
## Bari-SolidAct 1.5355 [0.7471; 3.1562]        6.4
## ACTT-2        0.6873 [0.4269; 1.1068]       14.0
## TOFACOV       0.2175 [0.0144; 3.2855]        0.5
## COVINIB       0.2705 [0.0511; 1.4311]        1.2
## COV-BARRIER   1.2208 [0.8387; 1.7772]       21.4
## RECOVERY      0.8229 [0.6575; 1.0299]       48.5
## TACTIC-R      0.8583 [0.2765; 2.6645]        2.7
## RUXCOVID      1.0889 [0.3573; 3.3186]        2.7
## PANCOVID      0.8630 [0.2767; 2.6922]        2.6
## 
## Number of studies: k = 9
## Number of observations: o = 10313
## 
##                               OR           95%-CI     t p-value
## Random effects model (HK) 0.8997 [0.7137; 1.1341] -1.05  0.3232
## Prediction interval              [0.6767; 1.1961]              
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0055 [0.0000; 0.7986]; tau = 0.0741 [0.0000; 0.8936]
##  I^2 = 17.1% [0.0%; 59.1%]; H = 1.10 [1.00; 1.56]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  9.65    8  0.2903
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 8)
## - Prediction interval based on t-distribution (df = 7)
```

```r
forest.meta(new.mv28,
            # hetstat = T,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE", "n.e"),
            leftlabs = c("Trial", "log(OR)", "Standard Error", "Sample Size"),
            text.random = "Average treatment effect (random effects model)",
            title = "Average treatment effect - New MV among survivors within 28 days", # get the title into the figure
            xlim = c(0.15,5),
            sortvar = +TE,
            # xlab = "Average treatment effect (95% CI)"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-20-1.png)<!-- -->
Discussion points:
1. Ghazaeian: new_mv_28: Besides the deaths no-one was intubated, and the deaths are excluded => no further events than death => not a single event in either arm!

# (v) Clinical status at day 28

```r
clin28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_clin28,
                      n.e = n_int,
                      n.c = n_cont,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = T,
                      method.tau = "ML",
                      method.random.ci = "HK"
                      )
summary(clin28)
```

```
##                   OR           95%-CI %W(random)
## Bari-SolidAct 0.9451 [0.5778; 1.5456]        3.6
## ACTT-2        0.6928 [0.4970; 0.9659]        7.8
## Ghazaeian     0.8256 [0.1729; 3.9418]        0.4
## TOFACOV       0.5090 [0.0861; 3.0089]        0.3
## COVINIB       0.3212 [0.0565; 1.8266]        0.3
## COV-BARRIER   0.8487 [0.6655; 1.0823]       14.6
## RECOVERY      0.7987 [0.7119; 0.8962]       65.1
## TACTIC-R      0.8437 [0.4686; 1.5188]        2.5
## RUXCOVID      1.1528 [0.5237; 2.5377]        1.4
## PANCOVID      0.5708 [0.3607; 0.9034]        4.1
## 
## Number of studies: k = 10
## Number of observations: o = 12402
## 
##                               OR           95%-CI     t p-value
## Random effects model (HK) 0.7929 [0.7286; 0.8629] -6.21  0.0002
## Prediction interval              [0.7108; 0.8845]              
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.1034]; tau = 0 [0.0000; 0.3215]
##  I^2 = 0.0% [0.0%; 62.4%]; H = 1.00 [1.00; 1.63]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  5.59    9  0.7797
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 9)
## - Prediction interval based on t-distribution (df = 8)
```

```r
forest(clin28, 
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aOR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "e_int", "n.c", "e_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "no JAKi", "Events\nno JAKi"),
            text.random = "Average treatment effect",
            sortvar = +TE,
            label.left = "Favours JAKi",
            label.right = "Favours No JAKi",
            )
```

![](two_stage_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

```r
# Open a pdf file
pdf("./fp_clin28.pdf", width=13, height=5)

forestplot <- forest(clin28, 
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aOR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "n.c"),
            leftlabs = c("Trial", "JAKi", "no JAKi"),
            text.random = "Average treatment effect",
            sortvar = +TE,
            label.left = "Favours JAKi",
            label.right = "Favours No JAKi",
            pooled.total = F
            )
summary_row_y <- unit(0.26, "npc")
total_row <- c(sum(df_clin28$n_int, na.rm = TRUE), 
               sum(df_clin28$n_cont, na.rm = TRUE))
pushViewport(viewport())
grid.text(label = total_row[1], x = unit(0.3668, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[2], x = unit(0.422, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
popViewport()

dev.off()
```

```
## quartz_off_screen 
##                 2
```
Discussion points

# (vi) Time to discharge or reaching discharge criteria up to day 28. Death = Competing event

```r
ttdischarge.comp <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_ttdischarge_comp,
                      n.e = n_int,
                      n.c = n_cont,
                      sm = "HR",
                      fixed = F,
                      random = T,
                      prediction = T,
                      method.tau = "ML",
                      method.random.ci = "HK"
                      )
summary(ttdischarge.comp)
```

```
##                   HR           95%-CI %W(random)
## Bari-SolidAct 1.1133 [0.8468; 1.4636]        1.9
## ACTT-2        1.1601 [1.0202; 1.3192]        8.8
## Ghazaeian     0.7381 [0.4798; 1.1355]        0.8
## TOFACOV       1.2562 [0.8760; 1.8014]        1.1
## COVINIB       1.5296 [1.0591; 2.2092]        1.1
## COV-BARRIER   1.1099 [0.9966; 1.2360]       12.6
## RECOVERY      1.0930 [1.0429; 1.1454]       65.9
## TACTIC-R      0.9763 [0.7555; 1.2615]        2.2
## RUXCOVID      1.0212 [0.8293; 1.2575]        3.4
## PANCOVID      1.2612 [0.9755; 1.6307]        2.2
## 
## Number of studies: k = 10
## Number of observations: o = 12395
## 
##                               HR           95%-CI    t p-value
## Random effects model (HK) 1.1018 [1.0516; 1.1543] 4.70  0.0011
## Prediction interval              [1.0532; 1.1525]             
## 
## Quantifying heterogeneity:
##  tau^2 < 0.0001 [0.0000; 0.0828]; tau = 0.0017 [0.0000; 0.2877]
##  I^2 = 10.7% [0.0%; 51.2%]; H = 1.06 [1.00; 1.43]
## 
## Test of heterogeneity:
##      Q d.f. p-value
##  10.08    9  0.3442
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 9)
## - Prediction interval based on t-distribution (df = 8)
```

```r
forest(ttdischarge.comp, 
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aHR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "e_int", "n.c", "e_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "no JAKi", "Events\nno JAKi"),
            text.random = "Average treatment effect",
            sortvar = +TE,
            label.left = "Favours JAKi",
            label.right = "Favours No JAKi",
            )
```

![](two_stage_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

```r
# Open a pdf file
pdf("./fp_ttdischarge_comp.pdf", width=13, height=5)

forestplot <- forest(ttdischarge.comp, 
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aHR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "e_int", "n.c", "e_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "no JAKi", "Events\nno JAKi"),
            text.random = "Average treatment effect",
            sortvar = +TE,
            label.left = "Favours JAKi",
            label.right = "Favours No JAKi",
            pooled.total = F
            )
summary_row_y <- unit(0.26, "npc")
total_row <- c(sum(df_ttdischarge_comp$n_int, na.rm = TRUE), 
               sum(df_ttdischarge_comp$e_int, na.rm = TRUE), 
               sum(df_ttdischarge_comp$n_cont, na.rm = TRUE),
               sum(df_ttdischarge_comp$e_cont, na.rm = TRUE))
pushViewport(viewport())
grid.text(label = total_row[1], x = unit(0.316, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[2], x = unit(0.358, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[3], x = unit(0.419, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[4], x = unit(0.463, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
popViewport()

dev.off()
```

```
## quartz_off_screen 
##                 2
```
Discussion points

# (vi.i) Time to discharge or reaching discharge criteria up to day 28. Death = Hypothetical

```r
ttdischarge.hypo <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_ttdischarge_hypo,
                      n.e = n_int + n_cont,
                      # n.c = n_control,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = T,
                      method.tau = "ML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
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
## Bari-SolidAct 1.0827 [0.8162; 1.4361]        2.0
## ACTT-2        1.2108 [1.0569; 1.3872]        8.6
## Ghazaeian     0.8232 [0.5401; 1.2546]        0.9
## TOFACOV       1.2836 [0.8804; 1.8714]        1.1
## COVINIB       1.5895 [1.0685; 2.3646]        1.0
## COV-BARRIER   1.1214 [1.0023; 1.2548]       12.6
## RECOVERY      1.1327 [1.0787; 1.1895]       66.3
## TACTIC-R      0.9920 [0.7589; 1.2967]        2.2
## RUXCOVID      1.0181 [0.8266; 1.2541]        3.7
## PANCOVID      1.3901 [1.0186; 1.8972]        1.6
## 
## Number of studies: k = 10
## Number of observations: o = 12395
## 
##                               OR           95%-CI    t p-value
## Random effects model (HK) 1.1351 [1.0811; 1.1917] 5.89  0.0002
## Prediction interval              [1.0829; 1.1897]             
## 
## Quantifying heterogeneity:
##  tau^2 < 0.0001 [0.0000; 0.0781]; tau = 0.0015 [0.0000; 0.2794]
##  I^2 = 10.7% [0.0%; 51.2%]; H = 1.06 [1.00; 1.43]
## 
## Test of heterogeneity:
##      Q d.f. p-value
##  10.08    9  0.3442
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 9)
## - Prediction interval based on t-distribution (df = 8)
```

```r
forest.meta(ttdischarge.hypo,
            # hetstat = T,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE", "n.e"),
            leftlabs = c("Trial", "log(OR)", "Standard Error", "Sample Size"),
            text.random = "Average treatment effect (random effects model)",
            title = "Average treatment effect - Time to discharge within 28 days", # get the title into the figure
            xlim = c(0.15,5),
            sortvar = +TE,
            # xlab = "Average treatment effect (95% CI)"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-23-1.png)<!-- -->
Discussion points

# (vi.ii) Time to discharge or reaching discharge criteria up to day 28. Death = Censored

```r
ttdischarge.cens <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_ttdischarge,
                      n.e = n_int + n_cont,
                      # n.c = n_control,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = T,
                      method.tau = "ML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
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
## Bari-SolidAct 1.0721 [0.8083; 1.4219]        2.0
## ACTT-2        1.2141 [1.0596; 1.3910]        8.6
## Ghazaeian     0.7381 [0.4798; 1.1355]        0.9
## TOFACOV       1.2836 [0.8804; 1.8714]        1.1
## COVINIB       1.5863 [1.0664; 2.3597]        1.0
## COV-BARRIER   1.0679 [0.9544; 1.1949]       12.6
## RECOVERY      1.1217 [1.0682; 1.1779]       66.4
## TACTIC-R      1.0080 [0.7709; 1.3179]        2.2
## RUXCOVID      1.0304 [0.8364; 1.2692]        3.7
## PANCOVID      1.3210 [0.9682; 1.8024]        1.6
## 
## Number of studies: k = 10
## Number of observations: o = 12395
## 
##                               OR           95%-CI    t p-value
## Random effects model (HK) 1.1198 [1.0632; 1.1796] 4.93  0.0008
## Prediction interval              [1.0685; 1.1737]             
## 
## Quantifying heterogeneity:
##  tau^2 < 0.0001 [0.0000; 0.0969]; tau = 0.0012 [0.0000; 0.3112]
##  I^2 = 21.6% [0.0%; 61.6%]; H = 1.13 [1.00; 1.61]
## 
## Test of heterogeneity:
##      Q d.f. p-value
##  11.48    9  0.2441
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 9)
## - Prediction interval based on t-distribution (df = 8)
```

```r
forest.meta(ttdischarge.cens,
            # hetstat = T,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE", "n.e"),
            leftlabs = c("Trial", "log(OR)", "Standard Error", "Sample Size"),
            text.random = "Average treatment effect (random effects model)",
            title = "Average treatment effect - Time to discharge within 28 days", # get the title into the figure
            xlim = c(0.15,5),
            sortvar = +TE,
            # xlab = "Average treatment effect (95% CI)"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-24-1.png)<!-- -->
Discussion points

# (vi.iii) Time to sustained discharge or reaching discharge criteria up to day 28. Death = Censored

```r
ttdischarge.sus <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_ttdischarge_sus,
                      n.e = n_int + n_cont,
                      # n.c = n_control,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = T,
                      method.tau = "ML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
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
## Bari-SolidAct 1.0554 [0.7946; 1.4017]        2.0
## ACTT-2        1.2141 [1.0596; 1.3910]        8.6
## Ghazaeian     0.7381 [0.4798; 1.1355]        0.9
## TOFACOV       1.2836 [0.8804; 1.8714]        1.1
## COVINIB       1.5863 [1.0664; 2.3597]        1.0
## COV-BARRIER   1.0582 [0.9454; 1.1845]       12.5
## RECOVERY      1.1217 [1.0682; 1.1779]       66.5
## TACTIC-R      1.0080 [0.7709; 1.3179]        2.2
## RUXCOVID      1.0304 [0.8364; 1.2692]        3.7
## PANCOVID      1.3210 [0.9682; 1.8024]        1.6
## 
## Number of studies: k = 10
## Number of observations: o = 12395
## 
##                               OR           95%-CI    t p-value
## Random effects model (HK) 1.1183 [1.0608; 1.1789] 4.79  0.0010
## Prediction interval              [1.0670; 1.1720]             
## 
## Quantifying heterogeneity:
##  tau^2 < 0.0001 [0.0000; 0.0981]; tau = 0.0009 [0.0000; 0.3132]
##  I^2 = 24.0% [0.0%; 63.1%]; H = 1.15 [1.00; 1.65]
## 
## Test of heterogeneity:
##      Q d.f. p-value
##  11.84    9  0.2227
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 9)
## - Prediction interval based on t-distribution (df = 8)
```

```r
forest.meta(ttdischarge.sus,
            # hetstat = T,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE", "n.e"),
            leftlabs = c("Trial", "log(OR)", "Standard Error", "Sample Size"),
            text.random = "Average treatment effect (random effects model)",
            title = "Average treatment effect - Time to sustained discharge within 28 days", # get the title into the figure
            xlim = c(0.15,5),
            sortvar = +TE,
            # xlab = "Average treatment effect (95% CI)"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

# (vii) Viral clearance up to day 5

```r
vir.clear5 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_vir_clear_5,
                      n.e = n_int,
                      n.c = n_cont,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = T,
                      method.tau = "ML",
                      method.random.ci = "HK"
                      )
summary(vir.clear5)
```

```
##                   OR           95%-CI %W(random)
## Bari-SolidAct 1.4947 [0.6349; 3.5190]        4.5
## ACTT-2        0.9731 [0.7149; 1.3244]       34.6
## COV-BARRIER   0.8980 [0.6700; 1.2034]       38.3
## RECOVERY      0.9713 [0.6530; 1.4447]       20.8
## TACTIC-R      0.3945 [0.1009; 1.5426]        1.8
## 
## Number of studies: k = 5
## Number of observations: o = 9413
## 
##                               OR           95%-CI     t p-value
## Random effects model (HK) 0.9463 [0.7620; 1.1752] -0.71  0.5184
## Prediction interval              [0.7050; 1.2702]              
## 
## Quantifying heterogeneity:
##  tau^2 < 0.0001 [0.0000; 1.5710]; tau = 0.0014 [0.0000; 1.2534]
##  I^2 = 0.0% [0.0%; 79.2%]; H = 1.00 [1.00; 2.19]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  2.85    4  0.5836
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 4)
## - Prediction interval based on t-distribution (df = 3)
```

```r
forest(vir.clear5, 
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aOR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "e_int", "n.c", "e_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "no JAKi", "Events\nno JAKi"),
            text.random = "Average treatment effect",
            sortvar = +TE,
            label.left = "Favours JAKi",
            label.right = "Favours No JAKi",
            )
```

![](two_stage_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

```r
# Open a pdf file
pdf("./fp_vir_clear5.pdf", width=13, height=5)

forestplot <- forest(vir.clear5, 
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aOR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "e_int", "n.c", "e_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "no JAKi", "Events\nno JAKi"),
            text.random = "Average treatment effect",
            sortvar = +TE,
            label.left = "Favours JAKi",
            label.right = "Favours No JAKi",
            pooled.total = F
            )

summary_row_y <- unit(0.36, "npc")

total_row <- c(sum(df_vir_clear_5$n_int, na.rm = TRUE), 
               sum(df_vir_clear_5$e_int, na.rm = TRUE), 
               sum(df_vir_clear_5$n_cont, na.rm = TRUE),
               sum(df_vir_clear_5$e_cont, na.rm = TRUE))

pushViewport(viewport())

grid.text(label = total_row[1], x = unit(0.315, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[2], x = unit(0.36, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[3], x = unit(0.418, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[4], x = unit(0.466, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))

popViewport()

dev.off()
```

```
## quartz_off_screen 
##                 2
```

# (viii) Viral clearance up to day 10

```r
vir.clear10 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_vir_clear_10,
                      n.e = n_int,
                      n.c = n_cont,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = T,
                      method.tau = "ML",
                      method.random.ci = "HK"
                      )
summary(vir.clear10)
```

```
##                   OR           95%-CI %W(random)
## Bari-SolidAct 1.0882 [0.5367; 2.2064]        5.2
## ACTT-2        0.8881 [0.6620; 1.1914]       29.8
## COV-BARRIER   0.9903 [0.7701; 1.2736]       40.7
## RECOVERY      0.9141 [0.6500; 1.2856]       22.1
## TACTIC-R      0.8137 [0.2793; 2.3706]        2.3
## 
## Number of studies: k = 5
## Number of observations: o = 9716
## 
##                               OR           95%-CI     t p-value
## Random effects model (HK) 0.9423 [0.8649; 1.0265] -1.93  0.1261
## Prediction interval              [0.7262; 1.2226]              
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.0058]; tau = 0 [0.0000; 0.0759]
##  I^2 = 0.0% [0.0%; 79.2%]; H = 1.00 [1.00; 2.19]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  0.57    4  0.9665
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 4)
## - Prediction interval based on t-distribution (df = 3)
```

```r
forest(vir.clear10, 
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aOR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "e_int", "n.c", "e_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "no JAKi", "Events\nno JAKi"),
            text.random = "Average treatment effect",
            sortvar = +TE,
            label.left = "Favours JAKi",
            label.right = "Favours No JAKi",
            )
```

![](two_stage_files/figure-html/unnamed-chunk-27-1.png)<!-- -->

```r
# Open a pdf file
pdf("./fp_vir_clear10.pdf", width=13, height=5)

forestplot <- forest(vir.clear10, 
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aOR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "e_int", "n.c", "e_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "no JAKi", "Events\nno JAKi"),
            text.random = "Average treatment effect",
            sortvar = +TE,
            label.left = "Favours JAKi",
            label.right = "Favours No JAKi",
            pooled.total = F
            )

summary_row_y <- unit(0.36, "npc")

total_row <- c(sum(df_vir_clear_10$n_int, na.rm = TRUE), 
               sum(df_vir_clear_10$e_int, na.rm = TRUE), 
               sum(df_vir_clear_10$n_cont, na.rm = TRUE),
               sum(df_vir_clear_10$e_cont, na.rm = TRUE))

pushViewport(viewport())

grid.text(label = total_row[1], x = unit(0.315, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[2], x = unit(0.36, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[3], x = unit(0.418, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[4], x = unit(0.466, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))

popViewport()

dev.off()
```

```
## quartz_off_screen 
##                 2
```

# (ix) Viral clearance up to day 15

```r
vir.clear15 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_vir_clear_15,
                      n.e = n_int,
                      n.c = n_cont,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = T,
                      method.tau = "ML",
                      method.random.ci = "HK"
                      )
summary(vir.clear15)
```

```
##                   OR           95%-CI %W(random)
## Bari-SolidAct 1.0042 [0.4970; 2.0290]        4.8
## ACTT-2        1.0197 [0.7661; 1.3573]       29.2
## COV-BARRIER   0.9717 [0.7630; 1.2375]       40.9
## RECOVERY      0.8551 [0.6183; 1.1825]       22.8
## TACTIC-R      0.9679 [0.3441; 2.7229]        2.2
## 
## Number of studies: k = 5
## Number of observations: o = 9831
## 
##                               OR           95%-CI     t p-value
## Random effects model (HK) 0.9587 [0.8755; 1.0498] -1.29  0.2664
## Prediction interval              [0.7458; 1.2324]              
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.0108]; tau = 0 [0.0000; 0.1038]
##  I^2 = 0.0% [0.0%; 79.2%]; H = 1.00 [1.00; 2.19]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  0.69    4  0.9530
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 4)
## - Prediction interval based on t-distribution (df = 3)
```

```r
forest(vir.clear15, 
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aOR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "e_int", "n.c", "e_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "no JAKi", "Events\nno JAKi"),
            text.random = "Average treatment effect",
            sortvar = +TE,
            label.left = "Favours JAKi",
            label.right = "Favours No JAKi",
            )
```

![](two_stage_files/figure-html/unnamed-chunk-28-1.png)<!-- -->

```r
# Open a pdf file
pdf("./fp_vir_clear15.pdf", width=13, height=5)

forestplot <- forest(vir.clear15, 
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aOR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "e_int", "n.c", "e_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "no JAKi", "Events\nno JAKi"),
            text.random = "Average treatment effect",
            sortvar = +TE,
            label.left = "Favours JAKi",
            label.right = "Favours No JAKi",
            pooled.total = F
            )

summary_row_y <- unit(0.36, "npc")

total_row <- c(sum(df_vir_clear_15$n_int, na.rm = TRUE), 
               sum(df_vir_clear_15$e_int, na.rm = TRUE), 
               sum(df_vir_clear_15$n_cont, na.rm = TRUE),
               sum(df_vir_clear_15$e_cont, na.rm = TRUE))

pushViewport(viewport())

grid.text(label = total_row[1], x = unit(0.315, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[2], x = unit(0.36, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[3], x = unit(0.418, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[4], x = unit(0.466, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))

popViewport()

dev.off()
```

```
## quartz_off_screen 
##                 2
```

# (x) Adverse event(s) grade 3 or 4, or a serious adverse event(s), excluding death, by day 28. ANY

```r
ae28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_ae28,
                      n.e = n_int,
                      n.c = n_cont,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = T,
                      method.tau = "ML",
                      method.random.ci = "HK"
                      )
summary(ae28)
```

```
##                   OR            95%-CI %W(random)
## Bari-SolidAct 0.9098 [0.5319;  1.5559]        5.2
## ACTT-2        0.9214 [0.7034;  1.2070]       20.5
## Ghazaeian     3.2247 [0.1787; 58.1925]        0.2
## TOFACOV       0.6936 [0.2461;  1.9552]        1.4
## COVINIB       0.7969 [0.3127;  2.0306]        1.7
## COV-BARRIER   1.1048 [0.8458;  1.4432]       20.9
## RECOVERY      0.9094 [0.7513;  1.1007]       40.9
## TACTIC-R      1.3389 [0.6219;  2.8825]        2.5
## RUXCOVID      0.5830 [0.3116;  1.0908]        3.8
## PANCOVID      1.1944 [0.5820;  2.4513]        2.9
## 
## Number of studies: k = 10
## Number of observations: o = 10767
## 
##                               OR           95%-CI     t p-value
## Random effects model (HK) 0.9469 [0.8426; 1.0641] -1.06  0.3178
## Prediction interval              [0.8201; 1.0933]              
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.1408]; tau = 0 [0.0000; 0.3753]
##  I^2 = 0.0% [0.0%; 62.4%]; H = 1.00 [1.00; 1.63]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  6.17    9  0.7231
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 9)
## - Prediction interval based on t-distribution (df = 8)
```

```r
forest(ae28, 
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aOR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "e_int", "n.c", "e_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "no JAKi", "Events\nno JAKi"),
            text.random = "Average treatment effect",
            sortvar = +TE,
            label.left = "Favours JAKi",
            label.right = "Favours No JAKi",
            )
```

![](two_stage_files/figure-html/unnamed-chunk-29-1.png)<!-- -->

```r
# Open a pdf file
pdf("./fp_ae28.pdf", width=13, height=5)

forestplot <- forest(ae28, 
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aOR", "95%-CI", "Weight"),
            leftcols = c("studlab", "n.e", "e_int", "n.c", "e_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "no JAKi", "Events\nno JAKi"),
            text.random = "Average treatment effect",
            sortvar = +TE,
            label.left = "Favours JAKi",
            label.right = "Favours No JAKi",
            pooled.total = F
            )

summary_row_y <- unit(0.26, "npc")

total_row <- c(sum(df_ae28$n_int, na.rm = TRUE), 
               sum(df_ae28$e_int, na.rm = TRUE), 
               sum(df_ae28$n_cont, na.rm = TRUE),
               sum(df_ae28$e_cont, na.rm = TRUE))

pushViewport(viewport())

grid.text(label = total_row[1], x = unit(0.314, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[2], x = unit(0.36, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[3], x = unit(0.417, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label = total_row[4], x = unit(0.466, "npc"), y = summary_row_y, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))

popViewport()

dev.off()
```

```
## quartz_off_screen 
##                 2
```

# (x) Meta-regression by RoB: Adverse event(s) grade 3 or 4, or a serious adverse event(s), excluding death, by day 28. ANY

```r
df_ae28 <- df_ae28 %>% 
  mutate("Risk of Bias" = case_when(trial == "ACTT-2" ~ "low risk",
                                            trial == "COV-BARRIER" ~ "low risk",
                                            trial == "RECOVERY" ~ "some concerns",
                                            trial == "TOFACOV" ~ "some concerns",
                                            trial == "COVINIB" ~ "some concerns",
                                            trial == "Ghazaeian" ~ "low risk",
                                            trial == "Bari-SolidAct" ~ "low risk",
                                trial == "TACTIC-R" ~ "some concerns",
                                trial == "RUXCOVID" ~ "low risk",
                                trial == "PANCOVID" ~ "some concerns",
                                        ))

# meta-regression by RoB
ae28.rob <- update.meta(ae28,
                        subgroup = df_ae28$`Risk of Bias`)
forest.meta(ae28.rob,
            hetstat = F,
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("aOR", "95%-CI", "Weight"),
            leftcols = c("studlab", 
                         "n.e", "n.c"),
            leftlabs = c("Trial",
                         "Total Intervention", "Total Control"),
            sortvar = +TE,
            test.subgroup.random = T,
            text.random = "Average treatment effect (RE model)",
            xlim = c(0.2,5),
            label.left = "Favours JAKi",  
            label.right = "Favours No JAKi"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-30-1.png)<!-- -->

# (x.i) Adverse event(s) grade 3 or 4, or a serious adverse event(s), excluding death, by day 28. SEVERAL

```r
# str(df_ae28sev)
ae28sev <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_ae28sev,
                      n.e = n_int + n_cont,
                      # n.c = n_control,
                      sm = "OR",
                      fixed = F,
                      random = T,
                      prediction = T,
                      method.tau = "ML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Average treatment effect - Adverse event(s) grade 3 or 4, or a serious adverse event(s), excluding death, by day 28. SEVERAL",
                      # subset = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # exclude entirely
                      # exclude = trial %in% c("Bari-SolidAct", "ACTT-2", "Ghazaeian") # include in forestplot but exclude from analysis
                      )
summary(ae28sev)
```

```
## Review:     Average treatment effect - Adverse event(s) grade 3 or 4, or a s ...
## 
##                   OR            95%-CI %W(random)
## Bari-SolidAct 1.2667 [0.9501;  1.6887]       13.6
## ACTT-2        0.8107 [0.7182;  0.9150]       17.8
## Ghazaeian     3.2247 [0.1787; 58.1925]        0.5
## TOFACOV       0.6936 [0.2461;  1.9552]        3.1
## COVINIB       0.5880 [0.3295;  1.0494]        7.3
## COV-BARRIER   1.2980 [1.1155;  1.5104]       17.2
## RECOVERY      0.8576 [0.7204;  1.0210]       16.7
## TACTIC-R      1.4542 [0.7426;  2.8476]        6.0
## RUXCOVID      0.5844 [0.3870;  0.8826]       10.5
## PANCOVID      1.1318 [0.6377;  2.0088]        7.4
## 
## Number of studies: k = 10
## Number of observations: o = 10515
## 
##                               OR           95%-CI     t p-value
## Random effects model (HK) 0.9463 [0.7466; 1.1994] -0.53  0.6111
## Prediction interval              [0.5278; 1.6966]              
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0538 [0.0124; 0.3927]; tau = 0.2320 [0.1112; 0.6267]
##  I^2 = 76.9% [57.5%; 87.5%]; H = 2.08 [1.53; 2.82]
## 
## Test of heterogeneity:
##      Q d.f.  p-value
##  38.97    9 < 0.0001
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 9)
## - Prediction interval based on t-distribution (df = 8)
```

```r
forest.meta(ae28sev,
            # hetstat = T,
            # rightcols = c("w.random"),
            leftcols = c("studlab", "TE", "seTE", "n.e"),
            leftlabs = c("Trial", "log(OR)", "Standard Error", "Sample Size"),
            text.random = "Average treatment effect (random effects model)",
            title = "Average treatment effect - Adverse event(s) grade 3 or 4, or a serious adverse event(s), excluding death, by day 28. SEVERAL", # get the title into the figure
            xlim = c(0.15,5),
            sortvar = +TE,
            # xlab = "Average treatment effect (95% CI)"
            )
```

![](two_stage_files/figure-html/unnamed-chunk-31-1.png)<!-- -->
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
                                        addmargins(table(df_tot$mort_28, df_tot$trt))[2,2], 
                                        addmargins(table(df_tot$mort_28, df_tot$trt))[3,2],
                                        addmargins(table(df_tot$mort_28, df_tot$trt))[2,1],
                                        addmargins(table(df_tot$mort_28, df_tot$trt))[3,1])
result_list[[2]] <- extract_trt_results(mort28.dimp, "death at day 28_dimp",
                                        addmargins(table(df_tot$mort_28_dimp, df_tot$trt))[2,2], 
                                        addmargins(table(df_tot$mort_28_dimp, df_tot$trt))[3,2],
                                        addmargins(table(df_tot$mort_28_dimp, df_tot$trt))[2,1],
                                        addmargins(table(df_tot$mort_28_dimp, df_tot$trt))[3,1])
result_list[[3]] <- extract_trt_results(mort28.mi, "death at day 28_mi",
                                        addmargins(table(df_tot$mort_28, df_tot$trt))[2,2],
                                        addmargins(table(df_tot$mort_28, df_tot$trt))[3,2],
                                        addmargins(table(df_tot$mort_28, df_tot$trt))[2,1],
                                        addmargins(table(df_tot$mort_28, df_tot$trt))[3,1])
result_list[[4]] <- extract_trt_results(mort28.agg, "death at day 28_agg",
                                        addmargins(table(df_tot$mort_28, df_tot$trt))[2,2], # CAVE: counts are without non-IPD!
                                        addmargins(table(df_tot$mort_28, df_tot$trt))[3,2],
                                        addmargins(table(df_tot$mort_28, df_tot$trt))[2,1],
                                        addmargins(table(df_tot$mort_28, df_tot$trt))[3,1])
result_list[[5]] <- extract_trt_results(mort60, "death at day 60",
                                        addmargins(table(df_tot$mort_60, df_tot$trt))[2,2], 
                                        addmargins(table(df_tot$mort_60, df_tot$trt))[3,2],
                                        addmargins(table(df_tot$mort_60, df_tot$trt))[2,1],
                                        addmargins(table(df_tot$mort_60, df_tot$trt))[3,1])
result_list[[6]] <- extract_trt_results(ttdeath, "death within fup",
                                        addmargins(table(df_tot$death_reached, df_tot$trt))[2,2], 
                                        addmargins(table(df_tot$death_reached, df_tot$trt))[3,2],
                                        addmargins(table(df_tot$death_reached, df_tot$trt))[2,1],
                                        addmargins(table(df_tot$death_reached, df_tot$trt))[3,1])
result_list[[7]] <- extract_trt_results(new.mvd28, "new MV or death within 28d",
                                        addmargins(table(df_tot$new_mvd_28, df_tot$trt))[2,2], 
                                        addmargins(table(df_tot$new_mvd_28, df_tot$trt))[3,2],
                                        addmargins(table(df_tot$new_mvd_28, df_tot$trt))[2,1],
                                        addmargins(table(df_tot$new_mvd_28, df_tot$trt))[3,1])
result_list[[8]] <- extract_trt_results(new.mv28, "new MV within 28d",
                                        addmargins(table(df_tot$new_mv_28, df_tot$trt))[2,2], 
                                        addmargins(table(df_tot$new_mv_28, df_tot$trt))[3,2],
                                        addmargins(table(df_tot$new_mv_28, df_tot$trt))[2,1],
                                        addmargins(table(df_tot$new_mv_28, df_tot$trt))[3,1])
result_list[[9]] <- extract_trt_results(clin28, "clinical status at day 28",
                                        addmargins(table(df_tot$clinstatus_28_imp, df_tot$trt))[7,2], 
                                        addmargins(table(df_tot$clinstatus_28_imp, df_tot$trt))[7,2],
                                        addmargins(table(df_tot$clinstatus_28_imp, df_tot$trt))[7,1],
                                        addmargins(table(df_tot$clinstatus_28_imp, df_tot$trt))[7,1])
result_list[[10]] <- extract_trt_results(ttdischarge.comp, "discharge within 28 days, death=comp.event",
                                        addmargins(table(df_tot$discharge_reached, df_tot$trt))[2,2], 
                                        addmargins(table(df_tot$discharge_reached, df_tot$trt))[3,2],
                                        addmargins(table(df_tot$discharge_reached, df_tot$trt))[2,1],
                                        addmargins(table(df_tot$discharge_reached, df_tot$trt))[3,1])
result_list[[11]] <- extract_trt_results(ttdischarge.hypo, "discharge within 28 days, death=hypo.event",
                                        addmargins(table(df_tot$discharge_reached, df_tot$trt))[2,2], 
                                        addmargins(table(df_tot$discharge_reached, df_tot$trt))[3,2],
                                        addmargins(table(df_tot$discharge_reached, df_tot$trt))[2,1],
                                        addmargins(table(df_tot$discharge_reached, df_tot$trt))[3,1])
result_list[[12]] <- extract_trt_results(ttdischarge.cens, "discharge within 28 days, death=censored",
                                        addmargins(table(df_tot$discharge_reached, df_tot$trt))[2,2], 
                                        addmargins(table(df_tot$discharge_reached, df_tot$trt))[3,2],
                                        addmargins(table(df_tot$discharge_reached, df_tot$trt))[2,1],
                                        addmargins(table(df_tot$discharge_reached, df_tot$trt))[3,1])
result_list[[13]] <- extract_trt_results(ttdischarge.sus, "sustained discharge within 28 days",
                                        addmargins(table(df_tot$discharge_reached_sus, df_tot$trt))[2,2], 
                                        addmargins(table(df_tot$discharge_reached_sus, df_tot$trt))[3,2],
                                        addmargins(table(df_tot$discharge_reached_sus, df_tot$trt))[2,1],
                                        addmargins(table(df_tot$discharge_reached_sus, df_tot$trt))[3,1])
result_list[[14]] <- extract_trt_results(vir.clear5, "viral clearance until day 5",
                                        addmargins(table(df_tot$vir_clear_5, df_tot$trt))[2,2], 
                                        addmargins(table(df_tot$vir_clear_5, df_tot$trt))[3,2],
                                        addmargins(table(df_tot$vir_clear_5, df_tot$trt))[2,1],
                                        addmargins(table(df_tot$vir_clear_5, df_tot$trt))[3,1])
result_list[[15]] <- extract_trt_results(vir.clear10, "viral clearance until day 10",
                                        addmargins(table(df_tot$vir_clear_10, df_tot$trt))[2,2], 
                                        addmargins(table(df_tot$vir_clear_10, df_tot$trt))[3,2],
                                        addmargins(table(df_tot$vir_clear_10, df_tot$trt))[2,1],
                                        addmargins(table(df_tot$vir_clear_10, df_tot$trt))[3,1])
result_list[[16]] <- extract_trt_results(vir.clear15, "viral clearance until day 15",
                                        addmargins(table(df_tot$vir_clear_15, df_tot$trt))[2,2], 
                                        addmargins(table(df_tot$vir_clear_15, df_tot$trt))[3,2],
                                        addmargins(table(df_tot$vir_clear_15, df_tot$trt))[2,1],
                                        addmargins(table(df_tot$vir_clear_15, df_tot$trt))[3,1])
result_list[[17]] <- extract_trt_results(ae28, "Any AE grade 3,4 within 28 days",
                                        addmargins(table(df_tot$ae_28, df_tot$trt))[2,2], 
                                        addmargins(table(df_tot$ae_28, df_tot$trt))[3,2],
                                        addmargins(table(df_tot$ae_28, df_tot$trt))[2,1],
                                        addmargins(table(df_tot$ae_28, df_tot$trt))[3,1])
result_list[[18]] <- extract_trt_results(ae28sev, "AEs grade 3,4 within 28 days",
                                        addmargins(table(df_tot$ae_28_sev, df_tot$trt))[9,2], 
                                        addmargins(table(df_tot$ae_28_sev, df_tot$trt))[9,2],
                                        addmargins(table(df_tot$ae_28_sev, df_tot$trt))[9,1],
                                        addmargins(table(df_tot$ae_28_sev, df_tot$trt))[9,1])

# Filter out NULL results and bind the results into a single data frame
result_df <- do.call(rbind, Filter(function(x) !is.null(x), result_list))

# Add the analysis approach
result_df$approach <- "two-stage"

# Nicely formatted table
kable(result_df, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|variable                                   | hazard_odds_ratio|  ci_lower|  ci_upper| standard_error|   p_value| n_intervention| n_intervention_tot| n_control| n_control_tot|approach  |
|:------------------------------------------|-----------------:|---------:|---------:|--------------:|---------:|--------------:|------------------:|---------:|-------------:|:---------|
|death at day 28                            |         0.7085112| 0.5758061| 0.8718006|      0.0916801| 0.0044947|            667|               6159|       764|          5916|two-stage |
|death at day 28_dimp                       |         0.7115181| 0.5779297| 0.8759855|      0.0919249| 0.0049014|            667|               6339|       764|          6063|two-stage |
|death at day 28_mi                         |         0.7198414| 0.5790670| 0.8948388|      0.0961970| 0.0076618|            667|               6159|       764|          5916|two-stage |
|death at day 28_agg                        |         0.6714559| 0.5525446| 0.8159576|      0.0914467| 0.0005650|            667|               6159|       764|          5916|two-stage |
|death at day 60                            |         0.7688075| 0.6703457| 0.8817317|      0.0605825| 0.0018781|            700|               6148|       788|          5898|two-stage |
|death within fup                           |         0.7808233| 0.6734426| 0.9053258|      0.0654004| 0.0043293|            700|               6264|       788|          6009|two-stage |
|new MV or death within 28d                 |         0.8119202| 0.7363671| 0.8952253|      0.0431771| 0.0009397|           1021|               6199|      1113|          5952|two-stage |
|new MV within 28d                          |         0.8996797| 0.7136961| 1.1341291|      0.1004255| 0.3232391|            323|               5363|       328|          5040|two-stage |
|clinical status at day 28                  |         0.7929382| 0.7286483| 0.8629006|      0.0373776| 0.0001575|           6339|               6339|      6063|          6063|two-stage |
|discharge within 28 days, death=comp.event |         1.1017572| 1.0515857| 1.1543224|      0.0206030| 0.0011146|           5087|               6335|      4699|          6060|two-stage |
|discharge within 28 days, death=hypo.event |         1.1350594| 1.0811205| 1.1916894|      0.0215224| 0.0002330|           5087|               6335|      4699|          6060|two-stage |
|discharge within 28 days, death=censored   |         1.1198491| 1.0631547| 1.1795669|      0.0229663| 0.0008148|           5087|               6335|      4699|          6060|two-stage |
|sustained discharge within 28 days         |         1.1182814| 1.0608116| 1.1788647|      0.0233224| 0.0009827|           5079|               6335|      4697|          6060|two-stage |
|viral clearance until day 5                |         0.9463025| 0.7619593| 1.1752444|      0.0780383| 0.5184358|            317|               4765|       322|          4648|two-stage |
|viral clearance until day 10               |         0.9422592| 0.8649134| 1.0265218|      0.0308492| 0.1261162|            459|               4928|       465|          4788|two-stage |
|viral clearance until day 15               |         0.9586996| 0.8755424| 1.0497550|      0.0326800| 0.2663823|            559|               4983|       564|          4848|two-stage |
|Any AE grade 3,4 within 28 days            |         0.9469009| 0.8425909| 1.0641240|      0.0515936| 0.3178353|            725|               5557|       697|          5210|two-stage |
|AEs grade 3,4 within 28 days               |         0.9462924| 0.7465857| 1.1994194|      0.1047855| 0.6110515|              6|                  6|         4|             4|two-stage |

```r
# Save
saveRDS(result_df, file = "overall_results_two-stage.RData")
```
Discussion points

# Plot all treatment effect estimates across endpoints

```r
# str(result_df)

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
                                        "death at day 28_agg", # CAVE: counts are without non-IPD!
                                        "death at day 28_mi",
                                        "death at day 28_dimp",
                                        "death at day 28"))

# Plotting
result_df$truncated <- ifelse(result_df$ci_upper > 2.0, TRUE, FALSE)  # Truncate at upper CI 2.0, and add arrow for those
ggplot(result_df, aes(x = variable, y = hazard_odds_ratio)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = pmin(ci_upper, 2.0)), width = 0.5) +
  geom_segment(data = subset(result_df, truncated),
               aes(x = variable, xend = variable, y = pmin(ci_upper, 2.0), yend = pmin(ci_upper, 2.0) + 0.1),
               arrow = arrow(length = unit(0.3, "cm")), color = "black")+
  geom_hline(yintercept = 1, linetype = "dotted", color = "red", size = 0.5) +
  labs(title = "All endpoints, two-stage approach",
       x = "Endpoints",
       y = "aOR / aHR") +
  theme_minimal() +
  scale_y_log10() +
  coord_flip()
```

```
## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
## ℹ Please use `linewidth` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

![](two_stage_files/figure-html/unnamed-chunk-33-1.png)<!-- -->

# Plot treatment effect estimates across endpoints, without sensitivity endpoints

```r
# Filter
main_result_df <- result_df %>% 
  filter(variable %in% c("Any AE grade 3,4 within 28 days",
                         "viral clearance until day 15",
                         "viral clearance until day 10",
                         "viral clearance until day 5",
                         "discharge within 28 days, death=comp.event",
                         "clinical status at day 28",
                         "new MV or death within 28d",
                         "death within fup",
                         "death at day 60",
                         "death at day 28"))
# Rename
main_result_df <- main_result_df %>% 
  mutate(variable = case_when(variable == "Any AE grade 3,4 within 28 days" ~ "Any AE grade 3,4 or SAE within 28 days",
                              variable == "viral clearance until day 15" ~ "Viral clearance at day 15",
                              variable == "viral clearance until day 10" ~ "Viral clearance at day 10",
                              variable == "viral clearance until day 5" ~ "Viral clearance at day 5",
                              variable == "discharge within 28 days, death=comp.event" ~ "Days until discharge within 28 days",
                              variable == "clinical status at day 28" ~ "Clinical status at day 28",
                              variable == "new MV or death within 28d" ~ "New mechanical ventilation or death at day 28",
                              variable == "death within fup" ~ "Days until death within 60 days",
                              variable == "death at day 60" ~ "All-cause mortality at day 60",
                              variable == "death at day 28" ~ "All-cause mortality at day 28",))
# Order
main_result_df$variable <- factor(main_result_df$variable, 
                             levels = c("Any AE grade 3,4 or SAE within 28 days",
                                        "Viral clearance at day 15",
                                        "Viral clearance at day 10",
                                        "Viral clearance at day 5",
                                        "Days until discharge within 28 days",
                                        "Clinical status at day 28",
                                        "New mechanical ventilation or death at day 28",
                                        "Days until death within 60 days",
                                        "All-cause mortality at day 60",
                                        "All-cause mortality at day 28"))
# Plotting
main_result_df$truncated <- ifelse(main_result_df$ci_upper > 2.0, TRUE, FALSE)
ggplot(main_result_df, aes(x = variable, y = hazard_odds_ratio)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = pmin(ci_upper, 2.0)), width = 0.5) +
  geom_segment(data = subset(main_result_df, truncated),
               aes(x = variable, xend = variable, y = pmin(ci_upper, 2.0), yend = pmin(ci_upper, 2.0) + 0.1),
               arrow = arrow(length = unit(0.3, "cm")), color = "black")+
  geom_hline(yintercept = 1, linetype = "dotted", color = "red", size = 0.5) +
  labs(title = "All endpoints, two-stage approach",
       x = "Endpoints",
       y = "aOR / aHR") +
  theme_minimal() +
  scale_y_log10() +
  coord_flip()
```

![](two_stage_files/figure-html/unnamed-chunk-34-1.png)<!-- -->

# Plot sensitivity treatment effect estimates across endpoints

```r
# Filter
sens_result_df <- result_df %>% 
  filter(variable %in% c("Any AE grade 3,4 within 28 days",
                         "AEs grade 3,4 within 28 days",
                         "discharge within 28 days, death=comp.event",
                         "sustained discharge within 28 days",
                         "new MV or death within 28d",
                         "new MV within 28d",
                         "death at day 28",
                         "death at day 28_mi"
                         ))
# Rename
sens_result_df <- sens_result_df %>% 
  mutate(variable = case_when(variable == "Any AE grade 3,4 within 28 days" ~ "Any AE grade 3,4 or SAE within 28 days*",
                              variable == "AEs grade 3,4 within 28 days" ~ "AEs grade 3,4 or SAE within 28 days$",
                              variable == "discharge within 28 days, death=comp.event" ~ "Days until discharge within 28 days*",
                              variable == "sustained discharge within 28 days" ~ "Days until sustained discharge within 28 days",
                              variable == "new MV or death within 28d" ~ "New mechanical ventilation or death at day 28*",
                              variable == "new MV within 28d" ~ "New mechanical ventilation, among survivors, at day 28",
                              variable == "death at day 28" ~ "All-cause mortality at day 28*",
                              variable == "death at day 28_mi" ~ "All-cause mortality at day 28, incl. multiple imputation"
                              ))
# Order
sens_result_df$variable <- factor(sens_result_df$variable, 
                             levels = c("Any AE grade 3,4 or SAE within 28 days*",
                                        "AEs grade 3,4 or SAE within 28 days$",
                                        "Days until discharge within 28 days*",
                                        "Days until sustained discharge within 28 days",
                                        "New mechanical ventilation or death at day 28*",
                                        "New mechanical ventilation, among survivors, at day 28",
                                        "All-cause mortality at day 28*",
                                        "All-cause mortality at day 28, incl. multiple imputation"
                                        ))

# Plotting
sens_result_df$truncated <- ifelse(sens_result_df$ci_upper > 2.0, TRUE, FALSE)
ggplot(sens_result_df, aes(x = variable, y = hazard_odds_ratio)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = pmin(ci_upper, 2.0)), width = 0.5) +
  geom_segment(data = subset(sens_result_df, truncated),
               aes(x = variable, xend = variable, y = pmin(ci_upper, 2.0), yend = pmin(ci_upper, 2.0) + 0.1),
               arrow = arrow(length = unit(0.3, "cm")), color = "black")+
  geom_hline(yintercept = 1, linetype = "dotted", color = "red", size = 0.5) +
  labs(title = "Sens endpoints, two-stage approach",
       x = "Endpoints",
       y = "aOR / aHR / aIRR") +
  theme_minimal() +
  scale_y_log10() +
  coord_flip()
```

![](two_stage_files/figure-html/unnamed-chunk-35-1.png)<!-- -->



# TREATMENT-COVARIATE INTERACTIONS
# Load treatment-covariate interaction estimates from all trials (on primary endpoint - and vacc.ae)

```r
df_int_barisolidact <- readRDS("int_effects_barisolidact.RData")
df_int_actt2 <- readRDS("int_effects_actt2.RData")
df_int_ghazaeian <- readRDS("int_effects_ghazaeian.RData")
df_int_tofacov <- readRDS("int_effects_tofacov.RData")
df_int_covinib <- readRDS("int_effects_covinib.RData")
df_int_covbarrier <- readRDS("int_effects_cov-barrier.RData")
df_int_recovery <- readRDS("int_effects_recovery.RData")
df_int_tactic_r <- readRDS("int_effects_tactic-r.RData")
df_int_ruxcovid <- readRDS("int_effects_ruxcovid_19072024.RData")
df_int_pancovid <- readRDS("int_effects_pancovid.RData")
```

# Reshape dataframes for all treatment-covariate interaction estimates (on primary endpoint - and vacc.ae)

```r
### Create a list of all data frames / trials
list_int_df <- list(df_int_barisolidact, df_int_actt2, df_int_ghazaeian, df_int_tofacov, df_int_covinib, df_int_covbarrier, df_int_recovery, df_int_tactic_r, df_int_ruxcovid, df_int_pancovid) # add all trials

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

## Comorbidity Count on Mortality at day 28
outcomes <- "comorbidity_count"
outcomes.firth <- "comorbidity_count_firth" # depends on which estimates to include
# Initialize an empty data frame to store the selected rows
df_comorb_count_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_int_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_comorb_count_mort28 <- rbind(df_comorb_count_mort28, selected_rows)
}

## Any Comorbidity on Mortality at day 28
outcomes <- "comorbidity_any"
outcomes.firth <- "comorbidity_any_firth" # depends on which estimates to include
# Initialize an empty data frame to store the selected rows
df_comorb_any_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_int_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_comorb_any_mort28 <- rbind(df_comorb_any_mort28, selected_rows)
}

## Comorbidity without immunosuppressed on Mortality at day 28
outcomes <- "comorbidity_noimmuno"
outcomes.firth <- "comorbidity_noimmuno_firth" # depends on which estimates to include
# Initialize an empty data frame to store the selected rows
df_comorb_noimmuno_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_int_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_comorb_noimmuno_mort28 <- rbind(df_comorb_noimmuno_mort28, selected_rows)
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

## At risk on AEs
outcomes <- "at risk on AEs"
outcomes.firth <- "at risk on AEs_firth" # depends on which estimates to include
# Initialize an empty data frame to store the selected rows
df_atrisk_ae28 <- data.frame()
# Loop through the list of data frames
for (df in list_int_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_atrisk_ae28 <- rbind(df_atrisk_ae28, selected_rows)
}

## Comedication on AEs
outcomes <- "comedication on AEs"
outcomes.firth <- "comedication on AEs_firth" # depends on which estimates to include
# Initialize an empty data frame to store the selected rows
df_comed_ae28 <- data.frame()
# Loop through the list of data frames
for (df in list_int_df) {
  selected_rows <- df %>% filter(variable == outcomes | variable == outcomes.firth)
  df_comed_ae28 <- rbind(df_comed_ae28, selected_rows)
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

## Show interaction estimates across trials

```r
# Nicely formatted table
kable(df_rs_mort28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|                           |variable                  | log_odds_ratio|  ci_lower|   ci_upper| standard_error|   p_value|trial         |JAKi        |
|:--------------------------|:-------------------------|--------------:|---------:|----------:|--------------:|---------:|:-------------|:-----------|
|trt:clinstatus_baseline_n  |respiratory support       |      0.2120952| 0.0216867|   1.492091|      1.0509098| 0.1400520|Bari-SolidAct |Baricitinib |
|trt:clinstatus_baseline_n1 |respiratory support       |      1.7465135| 0.8949053|   3.517276|      0.3473074| 0.1083715|ACTT-2        |Baricitinib |
|trt:clinstatus_baseline_n2 |respiratory support_firth |      1.0000000| 0.0016182| 617.982662|      0.0327887| 0.9999994|Ghazaeian     |Tofacitinib |
|trt:clinstatus_baseline_n3 |respiratory support_firth |      1.1883509| 0.0031529| 689.752137|      2.4621311| 0.9487548|TOFACOV       |Tofacitinib |
|trt:clinstatus_baseline_n4 |respiratory support_firth |      0.2524142| 0.0004994|  80.306181|      2.4418058| 0.5899644|COVINIB       |Baricitinib |
|trt:clinstatus_baseline_n5 |respiratory support       |      0.8770690| 0.5632959|   1.365549|      0.2255726| 0.5609060|COV-BARRIER   |Baricitinib |
|trt:clinstatus_baseline_n6 |respiratory support       |      0.8420453| 0.6754221|   1.049383|      0.1123806| 0.1260627|RECOVERY      |Baricitinib |
|trt:clinstatus_baseline_n7 |respiratory support       |      0.8715173| 0.1955359|   3.951934|      0.7556393| 0.8555898|TACTIC-R      |Baricitinib |
|trt:clinstatus_baseline_n8 |respiratory support       |      1.2192547| 0.0775021|  17.152121|      1.3374900| 0.8821709|RUXCOVID      |Ruxolitinib |
|trt:clinstatus_baseline_n9 |respiratory support       |      0.0372326| 0.0006964|   1.901509|      1.9012026| 0.0834903|PANCOVID      |Baricitinib |

```r
kable(df_vb_mort28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|               |variable          | log_odds_ratio|  ci_lower|    ci_upper| standard_error|   p_value|trial         |JAKi        |
|:--------------|:-----------------|--------------:|---------:|-----------:|--------------:|---------:|:-------------|:-----------|
|trt:vbaseline  |ventilation_firth |      0.7731991| 0.0000000|   1.6339739|      0.0717185| 0.2422306|Bari-SolidAct |Baricitinib |
|trt:vbaseline1 |ventilation       |      2.1962233| 0.6476978|   8.2809444|      0.6412119| 0.2198389|ACTT-2        |Baricitinib |
|trt:vbaseline2 |ventilation       |      0.7885977| 0.4089575|   1.5269399|      0.3355537| 0.4790806|COV-BARRIER   |Baricitinib |
|trt:vbaseline3 |ventilation       |      0.7462126| 0.5605528|   0.9930332|      0.1458505| 0.0447330|RECOVERY      |Baricitinib |
|trt:vbaseline4 |ventilation       |      0.6567971| 0.1060567|   4.0059285|      0.9110306| 0.6444875|TACTIC-R      |Baricitinib |
|trt:vbaseline5 |ventilation_firth |      0.1648545| 0.0007272|  36.9297493|      2.1698850| 0.4277741|RUXCOVID      |Ruxolitinib |
|trt:vbaseline6 |ventilation_firth |      9.8019038| 0.0403478| 702.0425957|      2.1662596| 0.3352295|PANCOVID      |Baricitinib |

```r
kable(df_age_mort28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|         |variable  | log_odds_ratio|  ci_lower| ci_upper| standard_error|   p_value|trial         |JAKi        |
|:--------|:---------|--------------:|---------:|--------:|--------------:|---------:|:-------------|:-----------|
|trt:age  |age       |      1.0266420| 0.9543445| 1.108300|      0.0376993| 0.4855233|Bari-SolidAct |Baricitinib |
|trt:age1 |age       |      0.9924607| 0.9514065| 1.035299|      0.0214898| 0.7247154|ACTT-2        |Baricitinib |
|trt:age2 |age       |      1.0504721| 0.9508741| 1.180266|      0.0531186| 0.3539392|Ghazaeian     |Tofacitinib |
|trt:age3 |age_firth |      1.0470785| 0.6614695| 1.665865|      0.0973369| 0.7745284|TOFACOV       |Tofacitinib |
|trt:age4 |age_firth |      0.8660242| 0.5581487| 1.539408|      0.1063739| 0.4253310|COVINIB       |Baricitinib |
|trt:age5 |age       |      1.0240520| 0.9961887| 1.053274|      0.0141903| 0.0939543|COV-BARRIER   |Baricitinib |
|trt:age6 |age       |      1.0080396| 0.9967378| 1.019495|      0.0057570| 0.1642554|RECOVERY      |Baricitinib |
|trt:age7 |age       |      1.0544741| 0.9719703| 1.150721|      0.0426149| 0.2132469|TACTIC-R      |Baricitinib |
|trt:age8 |age       |      1.0129174| 0.8844238| 1.140805|      0.0635585| 0.8399680|RUXCOVID      |Ruxolitinib |
|trt:age9 |age       |      1.0307682| 0.8131631| 1.359848|      0.1264829| 0.8106459|PANCOVID      |Baricitinib |

```r
kable(df_comorb_mort28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|                |variable          | log_odds_ratio|  ci_lower|    ci_upper| standard_error|   p_value|trial         |JAKi        |
|:---------------|:-----------------|--------------:|---------:|-----------:|--------------:|---------:|:-------------|:-----------|
|trt:comorb_cat  |comorbidity       |      1.0985344| 0.4298968|    2.902199|      0.4805639| 0.8449580|Bari-SolidAct |Baricitinib |
|trt:comorb_cat1 |comorbidity       |      0.8088102| 0.3373946|    1.939348|      0.4427415| 0.6317493|ACTT-2        |Baricitinib |
|trt:comorb_cat2 |comorbidity       |      1.9139158| 0.3278192|   16.486883|      0.9500040| 0.4944083|Ghazaeian     |Tofacitinib |
|trt:comorb_cat3 |comorbidity_firth |      0.6061082| 0.0054610|   88.272626|      1.3811009| 0.7773835|TOFACOV       |Tofacitinib |
|trt:comorb_cat4 |comorbidity_firth |      1.5083366| 0.0234199| 5786.072176|      1.2663027| 0.7880265|COVINIB       |Baricitinib |
|trt:comorb_cat5 |comorbidity       |      0.8491513| 0.5344257|    1.359975|      0.2376823| 0.4914727|COV-BARRIER   |Baricitinib |
|trt:comorb_cat6 |comorbidity       |      1.3073433| 1.0885552|    1.570974|      0.0935633| 0.0041788|RECOVERY      |Baricitinib |
|trt:comorb_cat7 |comorbidity       |      1.2511032| 0.4672406|    3.534220|      0.5099396| 0.6604311|TACTIC-R      |Baricitinib |
|trt:comorb_cat8 |comorbidity       |      0.5421417| 0.0286079|    3.573009|      1.0989479| 0.5774567|RUXCOVID      |Ruxolitinib |
|trt:comorb_cat9 |comorbidity       |      0.3797651| 0.0174044|    3.400573|      1.2185020| 0.4268554|PANCOVID      |Baricitinib |

```r
kable(df_comorb_count_mort28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|                  |variable                | log_odds_ratio|  ci_lower|  ci_upper| standard_error|   p_value|trial         |JAKi        |
|:-----------------|:-----------------------|--------------:|---------:|---------:|--------------:|---------:|:-------------|:-----------|
|trt:comorb_count  |comorbidity_count       |      1.2400296| 0.7033591|  2.235382|      0.2921383| 0.4614777|Bari-SolidAct |Baricitinib |
|trt:comorb_count1 |comorbidity_count       |      0.8221079| 0.5480154|  1.226120|      0.2047512| 0.3387233|ACTT-2        |Baricitinib |
|trt:comorb_count2 |comorbidity_count       |      1.6527667| 0.4561498|  7.127619|      0.6797149| 0.4597807|Ghazaeian     |Tofacitinib |
|trt:comorb_count3 |comorbidity_count_firth |      0.5792559| 0.0111203| 71.683097|      1.1610445| 0.7259072|TOFACOV       |Tofacitinib |
|trt:comorb_count4 |comorbidity_count_firth |      1.4734268| 0.0715237| 44.559658|      0.6844395| 0.6646862|COVINIB       |Baricitinib |
|trt:comorb_count5 |comorbidity_count       |      1.0128857| 0.7834545|  1.310115|      0.1309990| 0.9221418|COV-BARRIER   |Baricitinib |
|trt:comorb_count6 |comorbidity_count       |      1.1785783| 1.0084527|  1.377930|      0.0796135| 0.0390336|RECOVERY      |Baricitinib |
|trt:comorb_count7 |comorbidity_count       |      1.0288121| 0.5968718|  1.776240|      0.2761055| 0.9180608|TACTIC-R      |Baricitinib |
|trt:comorb_count8 |comorbidity_count       |      0.7865137| 0.3212603|  2.010084|      0.4543178| 0.5970940|RUXCOVID      |Ruxolitinib |
|trt:comorb_count9 |comorbidity_count       |      0.4323923| 0.0197156|  2.636989|      1.1444389| 0.4637992|PANCOVID      |Baricitinib |

```r
kable(df_comorb_any_mort28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|                |variable              | log_odds_ratio|  ci_lower|    ci_upper| standard_error|   p_value|trial         |JAKi        |
|:---------------|:---------------------|--------------:|---------:|-----------:|--------------:|---------:|:-------------|:-----------|
|trt:comorb_any  |comorbidity_any       |      0.8065627| 0.0781916|    8.310637|      1.1302683| 0.8491548|Bari-SolidAct |Baricitinib |
|trt:comorb_any1 |comorbidity_any       |      0.2910316| 0.0122596|    3.553572|      1.3081572| 0.3453950|ACTT-2        |Baricitinib |
|trt:comorb_any2 |comorbidity_any       |      1.2376379| 0.0481126|   47.600700|      1.6602489| 0.8978188|Ghazaeian     |Tofacitinib |
|trt:comorb_any3 |comorbidity_any_firth |      2.0316540| 0.0046406| 1792.745926|      2.3389317| 0.8051105|TOFACOV       |Tofacitinib |
|trt:comorb_any4 |comorbidity_any_firth |      2.6114916| 0.0071795| 1109.685140|      2.2845963| 0.7167929|COVINIB       |Baricitinib |
|trt:comorb_any5 |comorbidity_any       |      0.8300117| 0.2988331|    2.420735|      0.5288732| 0.7246225|COV-BARRIER   |Baricitinib |
|trt:comorb_any6 |comorbidity_any       |      1.5902636| 1.1766232|    2.153221|      0.1541064| 0.0026103|RECOVERY      |Baricitinib |
|trt:comorb_any7 |comorbidity_any       |      1.6654141| 0.1106514|   45.120711|      1.4142407| 0.7183464|TACTIC-R      |Baricitinib |
|trt:comorb_any8 |comorbidity_any_firth |      1.1338499| 0.0067774|   29.856153|      1.7351052| 0.9446033|RUXCOVID      |Ruxolitinib |
|trt:comorb_any9 |comorbidity_any       |      0.3750313| 0.0062031|   16.919231|      1.8634990| 0.5986849|PANCOVID      |Baricitinib |

```r
kable(df_comorb_noimmuno_mort28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|                     |variable                   | log_odds_ratio|  ci_lower|  ci_upper| standard_error|   p_value|trial         |JAKi        |
|:--------------------|:--------------------------|--------------:|---------:|---------:|--------------:|---------:|:-------------|:-----------|
|trt:comorb_noimmuno  |comorbidity_noimmuno       |      1.1602232| 0.4010770|  3.556440|      0.5469590| 0.7858476|Bari-SolidAct |Baricitinib |
|trt:comorb_noimmuno1 |comorbidity_noimmuno       |      0.5510679| 0.2105313|  1.425191|      0.4816795| 0.2160410|ACTT-2        |Baricitinib |
|trt:comorb_noimmuno2 |comorbidity_noimmuno       |      0.7627025| 0.0914175|  6.377876|      1.0144732| 0.7894518|Ghazaeian     |Tofacitinib |
|trt:comorb_noimmuno3 |comorbidity_noimmuno_firth |      0.6061082| 0.0054610| 88.272626|      1.3811009| 0.7773835|TOFACOV       |Tofacitinib |
|trt:comorb_noimmuno4 |comorbidity_noimmuno_firth |      1.4490143| 0.0221204| 77.884746|      1.2880327| 0.8069154|COVINIB       |Baricitinib |
|trt:comorb_noimmuno5 |comorbidity_noimmuno       |      0.8069903| 0.5011118|  1.309559|      0.2443724| 0.3801998|COV-BARRIER   |Baricitinib |
|trt:comorb_count1    |comorbidity_noimmuno       |      1.1785783| 1.0084527|  1.377930|      0.0796135| 0.0390336|RECOVERY      |Baricitinib |
|trt:comorb_noimmuno6 |comorbidity_noimmuno       |      0.8725508| 0.2526574|  3.156544|      0.6303995| 0.8287799|TACTIC-R      |Baricitinib |
|trt:comorb_cat1      |comorbidity_noimmuno       |      0.5421417| 0.0286079|  3.573009|      1.0989479| 0.5774567|RUXCOVID      |Ruxolitinib |
|trt:comorb_cat11     |comorbidity_noimmuno       |      0.3797651| 0.0174044|  3.400573|      1.2185020| 0.4268554|PANCOVID      |Baricitinib |

```r
kable(df_comed_mort28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|               |variable           | log_odds_ratio|  ci_lower|    ci_upper| standard_error|   p_value|trial         |JAKi        |
|:--------------|:------------------|--------------:|---------:|-----------:|--------------:|---------:|:-------------|:-----------|
|trt:comed_cat  |comedication_firth |      2.7732555| 0.0990539|  478.163891|      1.7967362| 0.5593131|Bari-SolidAct |Baricitinib |
|trt:comed_cat1 |comedication       |      2.1775073| 0.2043023|   23.309170|      1.1514131| 0.4991369|ACTT-2        |Baricitinib |
|trt:comed_cat2 |comedication_firth |      0.9812005| 0.0000001|   22.809022|      0.0768805| 0.9268050|Ghazaeian     |Tofacitinib |
|trt:comed_cat3 |comedication_firth |      3.6145387| 0.0090955| 2352.731675|      2.4282252| 0.6361043|TOFACOV       |Tofacitinib |
|trt:comed_cat4 |comedication_firth |      1.8595017| 0.0054508|  997.162606|      2.4370110| 0.8119091|COVINIB       |Baricitinib |
|trt:comed_cat5 |comedication       |      1.4282252| 0.5549093|    3.877636|      0.4923985| 0.4691455|COV-BARRIER   |Baricitinib |
|trt:comed_cat6 |comedication       |      0.9439961| 0.7294600|    1.221383|      0.1314592| 0.6610880|RECOVERY      |Baricitinib |
|trt:comed_cat7 |comedication       |      0.3978042| 0.0546930|    3.009177|      1.0151769| 0.3638705|TACTIC-R      |Baricitinib |
|trt:comed_cat8 |comedication       |      1.1749551| 0.0734291|   32.844519|      1.4398826| 0.9108437|RUXCOVID      |Ruxolitinib |
|trt:comed_cat9 |comedication_firth |      0.6602075| 0.0034688|   21.917325|      1.8491390| 0.8269920|PANCOVID      |Baricitinib |

```r
kable(df_vacc_ae28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|          |variable                 | log_odds_ratio|  ci_lower|      ci_upper| standard_error|   p_value|trial         |JAKi        |
|:---------|:------------------------|--------------:|---------:|-------------:|--------------:|---------:|:-------------|:-----------|
|trt:vacc  |vaccination on AEs       |      1.4025601| 0.4397817|  4.499607e+00|      0.5914840| 0.5673559|Bari-SolidAct |Baricitinib |
|trt:vacc1 |vaccination on AEs_firth |      2.6499354| 0.0097053|  7.652130e+02|      2.3583074| 0.6837461|TOFACOV       |Tofacitinib |
|trt:vacc2 |vaccination on AEs_firth |      1.0000000| 0.0000000| 1.774721e+204|      2.3995390| 0.9999999|COVINIB       |Baricitinib |
|trt:vacc3 |vaccination on AEs       |      0.9483684| 0.6418187|  1.401235e+00|      0.1990351| 0.7899724|RECOVERY      |Baricitinib |
|trt:vacc4 |vaccination on AEs       |      1.3276689| 0.1663357|  1.267403e+01|      1.0618899| 0.7895416|PANCOVID      |Baricitinib |

```r
kable(df_symp_mort28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|             |variable               | log_odds_ratio|  ci_lower|    ci_upper| standard_error|   p_value|trial         |JAKi        |
|:------------|:----------------------|--------------:|---------:|-----------:|--------------:|---------:|:-------------|:-----------|
|trt:sympdur  |symptom duration       |      0.8094195| 0.6375130|   1.0188508|      0.1184351| 0.0742183|Bari-SolidAct |Baricitinib |
|trt:sympdur1 |symptom duration       |      0.9527670| 0.8245538|   1.0896952|      0.0710958| 0.4961508|ACTT-2        |Baricitinib |
|trt:sympdur2 |symptom duration       |      1.1956574| 0.6406956|   2.3620705|      0.3232509| 0.5803940|Ghazaeian     |Tofacitinib |
|trt:sympdur3 |symptom duration_firth |      0.8235629| 0.3882541|   3.7407409|      0.2589834| 0.6358318|TOFACOV       |Tofacitinib |
|trt:sympdur4 |symptom duration_firth |      1.2662547| 0.0574558| 106.9157996|      0.5252723| 0.7623855|COVINIB       |Baricitinib |
|trt:sympdur5 |symptom duration       |      1.0402961| 0.9725413|   1.1110205|      0.0338918| 0.2437633|COV-BARRIER   |Baricitinib |
|trt:sympdur6 |symptom duration       |      0.9969161| 0.9682696|   1.0263879|      0.0148674| 0.8354261|RECOVERY      |Baricitinib |
|trt:sympdur7 |symptom duration       |      0.9948033| 0.8653390|   1.1538854|      0.0721968| 0.9424687|TACTIC-R      |Baricitinib |
|trt:sympdur8 |symptom duration       |      1.0252332| 0.8101625|   1.3652073|      0.1314394| 0.8496274|RUXCOVID      |Ruxolitinib |
|trt:sympdur9 |symptom duration       |      0.5936892| 0.2696346|   0.9818156|      0.3133931| 0.0961678|PANCOVID      |Baricitinib |

```r
kable(df_crp_mort28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|         |variable  | log_odds_ratio|  ci_lower| ci_upper| standard_error|   p_value|trial         |JAKi        |
|:--------|:---------|--------------:|---------:|--------:|--------------:|---------:|:-------------|:-----------|
|trt:crp  |crp       |      1.0001293| 0.9949001| 1.003014|      0.0015382| 0.9330246|Bari-SolidAct |Baricitinib |
|trt:crp1 |crp       |      0.9983016| 0.9932335| 1.003022|      0.0024671| 0.4908312|ACTT-2        |Baricitinib |
|trt:crp2 |crp       |      0.9966468| 0.9552354| 1.034729|      0.0196176| 0.8640553|Ghazaeian     |Tofacitinib |
|trt:crp3 |crp_firth |      0.9892815| 0.4830348| 1.046442|      0.0245673| 0.6953879|TOFACOV       |Tofacitinib |
|trt:crp4 |crp_firth |      1.0215022| 0.9090829| 1.111718|      0.0156007| 0.3665624|COVINIB       |Baricitinib |
|trt:crp5 |crp       |      1.0005119| 0.9972352| 1.003570|      0.0015972| 0.7486330|COV-BARRIER   |Baricitinib |
|trt:crp6 |crp       |      1.0001251| 0.9983955| 1.001859|      0.0008830| 0.8873777|RECOVERY      |Baricitinib |
|trt:crp7 |crp       |      1.0009004| 0.9896404| 1.012042|      0.0056165| 0.8726872|TACTIC-R      |Baricitinib |
|trt:crp8 |crp       |      0.9940109| 0.9731708| 1.015258|      0.0101573| 0.5542488|RUXCOVID      |Ruxolitinib |
|trt:crp9 |crp       |      0.9743573| 0.9146557| 1.016907|      0.0254621| 0.3076190|PANCOVID      |Baricitinib |

```r
kable(df_atrisk_ae28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|             |variable             | log_odds_ratio|  ci_lower|  ci_upper| standard_error|   p_value|trial         |JAKi        |
|:------------|:--------------------|--------------:|---------:|---------:|--------------:|---------:|:-------------|:-----------|
|trt:at_risk  |at risk on AEs       |      1.2294818| 0.4142531|  3.670119|      0.5552097| 0.7098194|Bari-SolidAct |Baricitinib |
|trt:at_risk1 |at risk on AEs       |      0.9655355| 0.5540986|  1.681546|      0.2830432| 0.9013852|ACTT-2        |Baricitinib |
|trt:at_risk2 |at risk on AEs_firth |      0.2005732| 0.0003771| 68.103813|      2.4674534| 0.5373824|Ghazaeian     |Tofacitinib |
|trt:at_risk3 |at risk on AEs       |      1.0998251| 0.0348928| 34.581506|      1.5739799| 0.9517952|TOFACOV       |Tofacitinib |
|trt:at_risk4 |at risk on AEs       |      0.5620926| 0.0745475|  4.178634|      1.0180360| 0.5714737|COVINIB       |Baricitinib |
|trt:at_risk5 |at risk on AEs       |      1.2095172| 0.7037829|  2.082667|      0.2765626| 0.4915753|COV-BARRIER   |Baricitinib |
|trt:at_risk6 |at risk on AEs       |      0.8407115| 0.5706143|  1.238500|      0.1975548| 0.3797967|RECOVERY      |Baricitinib |
|trt:at_risk7 |at risk on AEs       |      0.3137477| 0.0590740|  1.515225|      0.8170721| 0.1559916|TACTIC-R      |Baricitinib |
|trt:at_risk8 |at risk on AEs       |      2.2435276| 0.5829516|  9.394466|      0.7013907| 0.2492933|RUXCOVID      |Ruxolitinib |
|trt:at_risk9 |at risk on AEs       |      0.9430390| 0.1967419|  4.416148|      0.7841062| 0.9403774|PANCOVID      |Baricitinib |

```r
kable(df_comed_ae28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|                |variable                  | log_odds_ratio|  ci_lower|     ci_upper| standard_error|   p_value|trial         |JAKi        |
|:---------------|:-------------------------|--------------:|---------:|------------:|--------------:|---------:|:-------------|:-----------|
|trt:comed_cat1  |comedication on AEs       |      1.0025515| 0.1168221| 1.055681e+01|      1.1154956| 0.9981773|Bari-SolidAct |Baricitinib |
|trt:comed_cat11 |comedication on AEs       |      1.2089287| 0.2762678| 5.403130e+00|      0.7522954| 0.8008806|ACTT-2        |Baricitinib |
|trt:comed_cat12 |comedication on AEs_firth |      1.0038232| 0.0000000| 4.658784e+11|      0.1703626| 1.0000000|Ghazaeian     |Tofacitinib |
|trt:comed_cat13 |comedication on AEs_firth |      2.9293027| 0.0941528| 5.348705e+02|      1.8579432| 0.5519923|TOFACOV       |Tofacitinib |
|trt:comed_cat14 |comedication on AEs       |      0.8775139| 0.0501322| 2.584768e+01|      1.4824285| 0.9297648|COVINIB       |Baricitinib |
|trt:comed_cat15 |comedication on AEs       |      0.7852309| 0.3945380| 1.548850e+00|      0.3479803| 0.4871794|COV-BARRIER   |Baricitinib |
|trt:comed_cat16 |comedication on AEs       |      1.1014949| 0.7804889| 1.555381e+00|      0.1758209| 0.5824492|RECOVERY      |Baricitinib |
|trt:comed_cat17 |comedication on AEs       |      3.8768734| 0.4808470| 3.168773e+01|      1.0631619| 0.2024766|TACTIC-R      |Baricitinib |
|trt:comed_cat18 |comedication on AEs       |      2.6439096| 0.7320758| 1.004275e+01|      0.6638690| 0.1430481|RUXCOVID      |Ruxolitinib |
|trt:comed_cat19 |comedication on AEs       |      2.7963619| 0.0854931| 9.325435e+01|      1.6068324| 0.5221942|PANCOVID      |Baricitinib |


# Load subgroup effects from all trials (on primary endpoint - and vacc.ae)

```r
df_subgroup_actt2 <- readRDS("subgroup_effects_ACTT2.RData")
df_subgroup_covbarrier <- readRDS("subgroup_effects_cov-barrier.RData")
df_subgroup_barisolidact <- readRDS("subgroup_effects_barisolidact.RData")
df_subgroup_covinib <- readRDS("subgroup_effects_covinib.RData")
df_subgroup_tofacov <- readRDS("subgroup_effects_tofacov.RData")
df_subgroup_ghazaeian <- readRDS("subgroup_effects_ghazaeian.RData")
df_subgroup_recovery <- readRDS("subgroup_effects_recovery.RData")
df_subgroup_tactic_r <- readRDS("subgroup_effects_tactic-r.RData")
df_subgroup_ruxcovid <- readRDS("subgroup_effects_ruxcovid_19072024.RData")
df_subgroup_pancovid <- readRDS("subgroup_effects_pancovid.RData")
```

# Reshape dataframes for subgroup estimates by trial

```r
### Create a list of all data frames / trials
list_subgroup_df <- list(df_subgroup_actt2, df_subgroup_covbarrier, df_subgroup_barisolidact, df_subgroup_covinib, df_subgroup_tofacov, df_subgroup_ghazaeian, df_subgroup_recovery, df_subgroup_tactic_r, df_subgroup_ruxcovid, df_subgroup_pancovid) # add all trials

## Respiratory support
outcomes1 <- "No oxygen"
outcomes1.firth <- "No oxygen_firth"
outcomes2 <- "low-flow oxygen"
outcomes2.firth <- "low-flow oxygen_firth"
outcomes3 <- "high-flow oxygen / NIV"
outcomes3.firth <- "high-flow oxygen / NIV_firth"
outcomes4 <- "Mechanical ventilation / ECMO"
outcomes4.firth <- "Mechanical ventilation / ECMO_firth"
# Initialize an empty data frame to store the selected rows
df_sg_rs_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth
                                 | variable == outcomes2 | variable == outcomes2.firth
                                 | variable == outcomes3 | variable == outcomes3.firth
                                 | variable == outcomes4 | variable == outcomes4.firth)
  df_sg_rs_mort28 <- rbind(df_sg_rs_mort28, selected_rows)
}

## Ventilation
outcomes1 <- "None or low-flow oxygen"
outcomes1.firth <- "None or low-flow oxygen_firth"
outcomes2 <- "High-flow or non-invasive, mechanical, or ECMO"
outcomes2.firth <- "High-flow or non-invasive, mechanical, or ECMO_firth"
# Initialize an empty data frame to store the selected rows
df_sg_vb_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth
                                 | variable == outcomes2 | variable == outcomes2.firth)
  df_sg_vb_mort28 <- rbind(df_sg_vb_mort28, selected_rows)
}

## Age
outcomes1 <- "70 years and above"
outcomes1.firth <- "70 years and above_firth"
outcomes2 <- "below 70 years"
outcomes2.firth <- "below 70 years_firth"
# Initialize an empty data frame to store the selected rows
df_sg_age_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth
                                 | variable == outcomes2 | variable == outcomes2.firth)
  df_sg_age_mort28 <- rbind(df_sg_age_mort28, selected_rows)
}

## Comorbidity
outcomes1 <- "No comorbidity"
outcomes1.firth <- "No comorbidity_firth"
outcomes2 <- "One comorbidity"
outcomes2.firth <- "One comorbidity_firth"
outcomes3 <- "Multiple comorbidities"
outcomes3.firth <- "Multiple comorbidities_firth"
outcomes4 <- "Immunocompromised"
outcomes4.firth <- "Immunocompromised_firth"
# Initialize an empty data frame to store the selected rows
df_sg_comorb_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth
                                 | variable == outcomes2 | variable == outcomes2.firth
                                 | variable == outcomes3 | variable == outcomes3.firth
                                 | variable == outcomes4 | variable == outcomes4.firth)
  df_sg_comorb_mort28 <- rbind(df_sg_comorb_mort28, selected_rows)
}

## Comedication
outcomes1 <- "No Dexa, no Tocilizumab"
outcomes1.firth <- "No Dexa, no Tocilizumab_firth"
outcomes2 <- "Dexa, but no Tocilizumab"
outcomes2.firth <- "Dexa, but no Tocilizumab_firth"
outcomes3 <- "Dexa and Tocilizumab"
outcomes3.firth <- "Dexa and Tocilizumab_firth"
outcomes4 <- "Tocilizumab, but no Dexa"
outcomes4.firth <- "Tocilizumab, but no Dexa_firth"
# Initialize an empty data frame to store the selected rows
df_sg_comed_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth
                                  | variable == outcomes2 | variable == outcomes2.firth
                                 | variable == outcomes3 | variable == outcomes3.firth
                                | variable == outcomes4 | variable == outcomes4.firth
                                 )
  df_sg_comed_mort28 <- rbind(df_sg_comed_mort28, selected_rows)
}

## Vaccination on AEs
outcomes1 <- "vaccinated"
outcomes1.firth <- "vaccinated_firth"
outcomes2 <- "not vaccinated"
outcomes2.firth <- "not vaccinated_firth"
# Initialize an empty data frame to store the selected rows
df_sg_vacc_ae28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth
                                 | variable == outcomes2 | variable == outcomes2.firth)
  df_sg_vacc_ae28 <- rbind(df_sg_vacc_ae28, selected_rows)
}

## At risk on AEs
outcomes1 <- "Not at risk"
outcomes1.firth <- "Not at risk_firth"
outcomes2 <- "At risk"
outcomes2.firth <- "At risk_firth"
# Initialize an empty data frame to store the selected rows
df_sg_atrisk_ae28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth
                                 | variable == outcomes2 | variable == outcomes2.firth)
  df_sg_atrisk_ae28 <- rbind(df_sg_atrisk_ae28, selected_rows)
}

## Comedication on AEs
outcomes1 <- "No Dexa, no Tocilizumab_AE"
outcomes1.firth <- "No Dexa, no Tocilizumab_AE_firth"
outcomes2 <- "Dexa, but no Tocilizumab_AE"
outcomes2.firth <- "Dexa, but no Tocilizumab_AE_firth"
outcomes3 <- "Dexa and Tocilizumab_AE"
outcomes3.firth <- "Dexa and Tocilizumab_AE_firth"
outcomes4 <- "Tocilizumab, but no Dexa_AE"
outcomes4.firth <- "Tocilizumab, but no Dexa_AE_firth"
# Initialize an empty data frame to store the selected rows
df_sg_comed_ae28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth
                                 | variable == outcomes2 | variable == outcomes2.firth)
  df_sg_comed_ae28 <- rbind(df_sg_comed_ae28, selected_rows)
}

## Symptom onset
outcomes1 <- "More than 10 days"
outcomes1.firth <- "More than 10 days_firth"
outcomes2 <- "Between 5-10 days"
outcomes2.firth <- "Between 5-10 days_firth"
outcomes3 <- "5 days and less"
outcomes3.firth <- "5 days and less_firth"
# Initialize an empty data frame to store the selected rows
df_sg_symp_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth
                                 | variable == outcomes2 | variable == outcomes2.firth
                                 | variable == outcomes3 | variable == outcomes3.firth)
  df_sg_symp_mort28 <- rbind(df_sg_symp_mort28, selected_rows)
}

## CRP
outcomes1 <- "CRP 75 and higher"
outcomes1.firth <- "CRP 75 and higher_firth"
outcomes2 <- "CRP below 75"
outcomes2.firth <- "CRP below 75_firth"
# Initialize an empty data frame to store the selected rows
df_sg_crp_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth
                                 | variable == outcomes2 | variable == outcomes2.firth)
  df_sg_crp_mort28 <- rbind(df_sg_crp_mort28, selected_rows)
}
```

## Show subgroup estimates across trials

```r
# Nicely formatted table
kable(df_sg_rs_mort28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|      |variable                      | hazard_odds_ratio|  ci_lower|     ci_upper| standard_error|   p_value| n_intervention| n_intervention_tot| n_control| n_control_tot|trial         |JAKi        |
|:-----|:-----------------------------|-----------------:|---------:|------------:|--------------:|---------:|--------------:|------------------:|---------:|-------------:|:-------------|:-----------|
|trt2  |No oxygen_firth               |         1.0919885| 0.0059969| 2.007611e+02|      1.6675670| 0.9648055|              0|                 69|         0|            69|ACTT-2        |Baricitinib |
|trt3  |low-flow oxygen               |         0.4213865| 0.1311519| 1.170453e+00|      0.5460303| 0.1134887|              5|                274|        12|           268|ACTT-2        |Baricitinib |
|trt4  |high-flow oxygen / NIV        |         0.5655007| 0.1999432| 1.488924e+00|      0.5046464| 0.2586488|              7|                100|        13|           103|ACTT-2        |Baricitinib |
|trt5  |Mechanical ventilation / ECMO |         1.3801539| 0.5192725| 3.763665e+00|      0.5010200| 0.5201734|             12|                 51|        12|            52|ACTT-2        |Baricitinib |
|trt21 |No oxygen                     |         0.2603719| 0.0130648| 1.826796e+00|      1.1348746| 0.2357326|              1|                 83|         4|            95|COV-BARRIER   |Baricitinib |
|trt31 |low-flow oxygen               |         0.6283049| 0.3722225| 1.047315e+00|      0.2629286| 0.0771423|             29|                458|        41|           452|COV-BARRIER   |Baricitinib |
|trt41 |high-flow oxygen / NIV        |         0.4574430| 0.2646483| 7.778078e-01|      0.2742737| 0.0043508|             32|                168|        55|           170|COV-BARRIER   |Baricitinib |
|trt51 |Mechanical ventilation / ECMO |         0.4437773| 0.1896494| 1.012665e+00|      0.4255403| 0.0562390|             20|                 49|        29|            48|COV-BARRIER   |Baricitinib |
|trt1  |high-flow oxygen / NIV        |         0.8859899| 0.3692133| 2.110649e+00|      0.4409887| 0.7837033|             13|                119|        14|           118|Bari-SolidAct |Baricitinib |
|trt22 |Mechanical ventilation / ECMO |         0.2363617| 0.0292844| 1.231437e+00|      0.9156439| 0.1151927|              2|                 18|         7|            22|Bari-SolidAct |Baricitinib |
|trt11 |No oxygen_firth               |         1.4516881| 0.0050949| 3.369693e+02|      1.7562814| 0.8724571|              0|                 16|         0|            19|COVINIB       |Baricitinib |
|trt23 |low-flow oxygen_firth         |         0.1826966| 0.0013043| 2.354803e+00|      1.4673121| 0.2091351|              0|                 37|         2|            35|COVINIB       |Baricitinib |
|trt12 |No oxygen_firth               |         1.3918259| 0.0072404| 2.921471e+02|      1.7165860| 0.8722372|              0|                  9|         0|            16|TOFACOV       |Tofacitinib |
|trt24 |low-flow oxygen_firth         |         2.1912808| 0.1141366| 3.226519e+02|      1.4564436| 0.6142125|              1|                 49|         0|            42|TOFACOV       |Tofacitinib |
|trt13 |low-flow oxygen               |         0.7908805| 0.1472786| 3.826382e+00|      0.7982111| 0.7688207|              3|                 46|         4|            49|Ghazaeian     |Tofacitinib |
|trt25 |No oxygen                     |         0.7830900| 0.3626356| 1.666827e+00|      0.3863765| 0.5268497|             15|                225|        19|           233|RECOVERY      |Baricitinib |
|trt32 |low-flow oxygen               |         0.9178995| 0.7540459| 1.117162e+00|      0.1002336| 0.3927301|            256|               2723|       253|          2696|RECOVERY      |Baricitinib |
|trt42 |high-flow oxygen / NIV        |         0.6961781| 0.5530496| 8.754001e-01|      0.1170969| 0.0019832|            204|                992|       230|           899|RECOVERY      |Baricitinib |
|trt52 |Mechanical ventilation / ECMO |         0.7466881| 0.4176651| 1.329774e+00|      0.2947054| 0.3215950|             38|                121|        43|           112|RECOVERY      |Baricitinib |
|trt26 |No oxygen_firth               |        19.3071486| 0.6167050| 1.790272e+04|      1.8523368| 0.0917713|              1|                  2|         0|             8|TACTIC-R      |Baricitinib |
|trt33 |low-flow oxygen               |         0.7614310| 0.1231444| 4.467120e+00|      0.8831130| 0.7576025|              3|                 60|         4|            63|TACTIC-R      |Baricitinib |
|trt43 |high-flow oxygen / NIV        |         0.7354511| 0.2799213| 1.892107e+00|      0.4832333| 0.5248642|             14|                 68|        13|            65|TACTIC-R      |Baricitinib |
|trt27 |No oxygen                     |         1.0477132| 0.0938198| 2.350193e+01|      1.2599112| 0.9704893|              2|                 93|         1|            46|RUXCOVID      |Ruxolitinib |
|trt34 |low-flow oxygen               |         1.6975119| 0.3830702| 1.179016e+01|      0.8287241| 0.5231300|              7|                173|         2|            91|RUXCOVID      |Ruxolitinib |
|trt44 |high-flow oxygen / NIV_firth  |         0.4863857| 0.0035026| 8.579832e+01|      1.7363739| 0.7129026|              0|                 16|         0|             5|RUXCOVID      |Ruxolitinib |
|trt28 |No oxygen_firth               |         1.5003070| 0.0417655| 2.438915e+02|      1.5823848| 0.8177485|              1|                 40|         0|            30|PANCOVID      |Baricitinib |
|trt35 |low-flow oxygen               |         0.1731803| 0.0087166| 1.154727e+00|      1.1274769| 0.1199048|              1|                 99|         5|           102|PANCOVID      |Baricitinib |
|trt45 |high-flow oxygen / NIV_firth  |         1.9100247| 0.0062494| 1.197791e+03|      2.4951044| 0.7951504|              0|                  1|         1|             4|PANCOVID      |Baricitinib |

```r
kable(df_sg_vb_mort28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|      |variable                                             | hazard_odds_ratio|  ci_lower|     ci_upper| standard_error|   p_value| n_intervention| n_intervention_tot| n_control| n_control_tot|trial         |JAKi        |
|:-----|:----------------------------------------------------|-----------------:|---------:|------------:|--------------:|---------:|--------------:|------------------:|---------:|-------------:|:-------------|:-----------|
|trt   |High-flow or non-invasive, mechanical, or ECMO       |         0.8593021| 0.4376303|    1.6717306|      0.3400416| 0.6556478|             19|                151|        25|           155|ACTT-2        |Baricitinib |
|trt1  |None or low-flow oxygen                              |         0.3915676| 0.1222188|    1.0811603|      0.5438087| 0.0846843|              5|                343|        12|           337|ACTT-2        |Baricitinib |
|trt2  |High-flow or non-invasive, mechanical, or ECMO       |         0.4891922| 0.3155362|    0.7522581|      0.2212980| 0.0012339|             52|                217|        84|           218|COV-BARRIER   |Baricitinib |
|trt11 |None or low-flow oxygen                              |         0.5931543| 0.3582739|    0.9683809|      0.2527486| 0.0387829|             30|                541|        45|           547|COV-BARRIER   |Baricitinib |
|trt3  |High-flow or non-invasive, mechanical, or ECMO       |         0.6429029| 0.2971201|    1.3578034|      0.3851009| 0.2513271|             15|                137|        21|           140|Bari-SolidAct |Baricitinib |
|trt4  |None or low-flow oxygen_firth                        |         0.1706455| 0.0012210|    2.1822568|      1.4796585| 0.1882367|              0|                 53|         2|            54|COVINIB       |Baricitinib |
|trt5  |None or low-flow oxygen_firth                        |         2.6388102| 0.1384454|  386.7412922|      1.4601179| 0.5277118|              1|                 58|         0|            58|TOFACOV       |Tofacitinib |
|trt6  |None or low-flow oxygen                              |         0.8256311| 0.1539036|    3.9893830|      0.7975640| 0.8101437|              3|                 46|         4|            51|Ghazaeian     |Tofacitinib |
|trt7  |High-flow or non-invasive, mechanical, or ECMO       |         0.7051714| 0.5704914|    0.8708986|      0.1078700| 0.0012025|            242|               1113|       273|          1011|RECOVERY      |Baricitinib |
|trt12 |None or low-flow oxygen                              |         0.9130572| 0.7551250|    1.1038115|      0.0968063| 0.3474358|            271|               2948|       272|          2929|RECOVERY      |Baricitinib |
|trt8  |High-flow or non-invasive, mechanical, or ECMO       |         0.8531787| 0.3338317|    2.1587604|      0.4721503| 0.7366413|             14|                 68|        13|            67|TACTIC-R      |Baricitinib |
|trt13 |None or low-flow oxygen                              |         1.4550266| 0.2801228|    8.1156596|      0.8331843| 0.6526315|              4|                 62|         4|            71|TACTIC-R      |Baricitinib |
|trt9  |High-flow or non-invasive, mechanical, or ECMO_firth |         0.4863857| 0.0035026|   85.7983170|      1.7363739| 0.7129026|              0|                 16|         0|             5|RUXCOVID      |Ruxolitinib |
|trt14 |None or low-flow oxygen                              |         1.4770681| 0.4192599|    6.8591547|      0.6877409| 0.5706055|              9|                266|         3|           137|RUXCOVID      |Ruxolitinib |
|trt10 |High-flow or non-invasive, mechanical, or ECMO_firth |         1.9100247| 0.0062494| 1197.7908134|      2.4951044| 0.7951504|              0|                  1|         1|             4|PANCOVID      |Baricitinib |
|trt15 |None or low-flow oxygen                              |         0.2852505| 0.0384313|    1.4242911|      0.8757422| 0.1520378|              2|                139|         5|           132|PANCOVID      |Baricitinib |

```r
kable(df_sg_age_mort28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|      |variable                 | hazard_odds_ratio|  ci_lower|    ci_upper| standard_error|   p_value| n_intervention| n_intervention_tot| n_control| n_control_tot|trial         |JAKi        |
|:-----|:------------------------|-----------------:|---------:|-----------:|--------------:|---------:|--------------:|------------------:|---------:|-------------:|:-------------|:-----------|
|trt6  |70 years and above       |         0.6903606| 0.2538418|   1.7742777|      0.4906427| 0.4501194|              9|                 79|        17|           103|ACTT-2        |Baricitinib |
|trt7  |below 70 years           |         0.6761784| 0.3293397|   1.3605339|      0.3590061| 0.2757356|             15|                415|        20|           389|ACTT-2        |Baricitinib |
|trt61 |70 years and above       |         0.6487276| 0.3808324|   1.0986424|      0.2696579| 0.1085416|             39|                160|        53|           149|COV-BARRIER   |Baricitinib |
|trt71 |below 70 years           |         0.4610049| 0.2980637|   0.7029898|      0.2183732| 0.0003912|             43|                598|        76|           616|COV-BARRIER   |Baricitinib |
|trt3  |70 years and above       |         0.9118120| 0.3128627|   2.6071685|      0.5359718| 0.8632406|              9|                 31|        12|            38|Bari-SolidAct |Baricitinib |
|trt4  |below 70 years           |         0.6336362| 0.2045538|   1.8358261|      0.5489705| 0.4058853|              6|                106|         9|           102|Bari-SolidAct |Baricitinib |
|trt31 |70 years and above_firth |         0.2401700| 0.0016108|   4.6025170|      1.5279557| 0.3478313|              0|                 11|         1|             8|COVINIB       |Baricitinib |
|trt41 |below 70 years_firth     |         0.3451466| 0.0023392|   6.6124982|      1.4833067| 0.4883942|              0|                 42|         1|            46|COVINIB       |Baricitinib |
|trt32 |70 years and above_firth |         2.9152473| 0.1384519| 459.3753293|      1.4917610| 0.5008116|              1|                 17|         0|            17|TOFACOV       |Tofacitinib |
|trt42 |below 70 years_firth     |         1.1152983| 0.0057878| 218.9597870|      1.6659710| 0.9575470|              0|                 41|         0|            41|TOFACOV       |Tofacitinib |
|trt2  |70 years and above_firth |         3.5454545| 0.1547877| 562.4644435|      1.7327236| 0.4360413|              1|                  6|         0|             6|Ghazaeian     |Tofacitinib |
|trt33 |below 70 years_firth     |         0.5699856| 0.0947452|   2.7368400|      0.8206782| 0.4855442|              2|                 40|         4|            45|Ghazaeian     |Tofacitinib |
|trt62 |70 years and above       |         0.9413563| 0.7691387|   1.1522282|      0.1030750| 0.5576692|            289|                985|       277|           904|RECOVERY      |Baricitinib |
|trt72 |below 70 years           |         0.7613837| 0.6285147|   0.9214499|      0.0975500| 0.0051956|            224|               3076|       268|          3036|RECOVERY      |Baricitinib |
|trt5  |70 years and above       |         1.1694762| 0.3810221|   3.6471657|      0.5706403| 0.7838146|             12|                 34|         9|            31|TACTIC-R      |Baricitinib |
|trt63 |below 70 years           |         0.7340426| 0.2258070|   2.2735076|      0.5785174| 0.5930306|              6|                 96|         8|           107|TACTIC-R      |Baricitinib |
|trt51 |70 years and above       |         1.7549809| 0.3967762|  12.2263448|      0.8301572| 0.4980686|              7|                 82|         2|            39|RUXCOVID      |Ruxolitinib |
|trt64 |below 70 years           |         1.0435959| 0.0985408|  22.6540452|      1.2323670| 0.9723777|              2|                200|         1|           103|RUXCOVID      |Ruxolitinib |
|trt52 |70 years and above       |         0.7412002| 0.0803986|   6.7470924|      1.0577503| 0.7770745|              2|                 34|         2|            27|PANCOVID      |Baricitinib |
|trt65 |below 70 years_firth     |         0.1336728| 0.0010069|   1.2931050|      1.3699564| 0.0888291|              0|                106|         4|           109|PANCOVID      |Baricitinib |

```r
kable(df_sg_comorb_mort28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|       |variable                     | hazard_odds_ratio|  ci_lower|    ci_upper| standard_error|   p_value| n_intervention| n_intervention_tot| n_control| n_control_tot|trial         |JAKi        |
|:------|:----------------------------|-----------------:|---------:|-----------:|--------------:|---------:|--------------:|------------------:|---------:|-------------:|:-------------|:-----------|
|trt8   |No comorbidity               |         1.2952150| 0.0855367|  33.5700990|      1.3865145| 0.8520008|              2|                 67|         1|            86|ACTT-2        |Baricitinib |
|trt9   |One comorbidity              |         0.7248784| 0.2236579|   2.2478605|      0.5775547| 0.5774643|              6|                150|         8|           130|ACTT-2        |Baricitinib |
|trt10  |Multiple comorbidities       |         0.4953443| 0.2316928|   1.0135053|      0.3736364| 0.0600841|             13|                261|        28|           263|ACTT-2        |Baricitinib |
|trt11  |Immunocompromised_firth      |         1.9814786| 0.0347474| 291.0219751|      1.5122930| 0.6767261|              3|                 16|         0|            13|ACTT-2        |Baricitinib |
|trt81  |No comorbidity               |         0.5582887| 0.1968226|   1.4856867|      0.5091167| 0.2522577|              8|                173|        14|           163|COV-BARRIER   |Baricitinib |
|trt91  |One comorbidity              |         0.6398101| 0.3223249|   1.2423907|      0.3424055| 0.1921468|             20|                207|        29|           241|COV-BARRIER   |Baricitinib |
|trt101 |Multiple comorbidities       |         0.4352340| 0.2810257|   0.6662287|      0.2197616| 0.0001535|             54|                370|        86|           353|COV-BARRIER   |Baricitinib |
|trt111 |Immunocompromised_firth      |         1.2578568| 0.0018082| 321.9030187|      1.7021842| 0.9129185|              0|                  8|         0|             8|COV-BARRIER   |Baricitinib |
|trt5   |No comorbidity               |         0.7929289| 0.0700449|   8.0798326|      1.1396212| 0.8386696|              2|                 50|         2|            50|Bari-SolidAct |Baricitinib |
|trt6   |One comorbidity              |         0.7583215| 0.0915641|   5.1732692|      0.9735989| 0.7762957|              2|                 32|         7|            40|Bari-SolidAct |Baricitinib |
|trt7   |Multiple comorbidities       |         0.7853308| 0.2645997|   2.2796711|      0.5431823| 0.6564074|              9|                 50|        10|            46|Bari-SolidAct |Baricitinib |
|trt82  |Immunocompromised            |         0.7264759| 0.0401836|  12.2615910|      1.3971732| 0.8190929|              2|                  5|         2|             4|Bari-SolidAct |Baricitinib |
|trt51  |No comorbidity_firth         |         0.0726380| 0.0000024|   3.7073075|      1.8702986| 0.2226213|              0|                 17|         1|            19|COVINIB       |Baricitinib |
|trt61  |One comorbidity_firth        |         0.8351792| 0.0035622| 686.3844738|      1.4982055| 0.9239355|              0|                 21|         0|            19|COVINIB       |Baricitinib |
|trt71  |Multiple comorbidities_firth |         0.3560667| 0.0022367|   7.3735105|      1.4211978| 0.5163665|              0|                 14|         1|            15|COVINIB       |Baricitinib |
|trt83  |Immunocompromised_firth      |         1.0000000| 0.0054376|  72.9457681|      2.3094100| 1.0000000|              0|                  1|         0|             1|COVINIB       |Baricitinib |
|trt52  |No comorbidity_firth         |         1.1018332| 0.0053609| 201.0166882|      1.4976235| 0.9621649|              0|                 19|         0|            25|TOFACOV       |Tofacitinib |
|trt62  |One comorbidity_firth        |         2.6425798| 0.1102389| 502.7508467|      1.4077006| 0.5733840|              1|                 22|         0|            24|TOFACOV       |Tofacitinib |
|trt72  |Multiple comorbidities_firth |         0.3895007| 0.0021761|  62.9694758|      1.6968530| 0.6320255|              0|                 17|         0|             9|TOFACOV       |Tofacitinib |
|trt4   |No comorbidity               |         0.6584886| 0.0290147|   7.5164490|      1.2710281| 0.7423699|              1|                 17|         2|            24|Ghazaeian     |Tofacitinib |
|trt53  |One comorbidity_firth        |         1.2592497| 0.0078305| 225.3160083|      1.6982200| 0.9056900|              0|                 11|         0|            11|Ghazaeian     |Tofacitinib |
|trt63  |Multiple comorbidities       |         0.3297898| 0.0109202|   4.2837044|      1.3879102| 0.4241402|              1|                 17|         2|            15|Ghazaeian     |Tofacitinib |
|trt73  |Immunocompromised_firth      |         9.0000000| 0.1388609|  53.0508399|      2.3094109| 0.9999995|              1|                  1|         0|             1|Ghazaeian     |Tofacitinib |
|trt84  |No comorbidity               |         0.5900827| 0.4597569|   0.7550507|      0.1264554| 0.0000303|            133|               2154|       196|          2147|RECOVERY      |Baricitinib |
|trt92  |One comorbidity              |         0.9026893| 0.7099810|   1.1474058|      0.1223753| 0.4028283|            192|               1276|       184|          1208|RECOVERY      |Baricitinib |
|trt102 |Multiple comorbidities       |         1.0040793| 0.7728712|   1.3046520|      0.1334930| 0.9756714|            188|                631|       165|           585|RECOVERY      |Baricitinib |
|trt74  |No comorbidity               |         0.4900922| 0.0184000|   6.7493869|      1.3812104| 0.6056233|              1|                 20|         2|            24|TACTIC-R      |Baricitinib |
|trt85  |One comorbidity              |         1.4595901| 0.1952204|  10.4786421|      0.9737953| 0.6977705|              3|                 27|         3|            34|TACTIC-R      |Baricitinib |
|trt93  |Multiple comorbidities       |         0.5950590| 0.1835300|   1.8743635|      0.5848668| 0.3747864|             10|                 66|         8|            62|TACTIC-R      |Baricitinib |
|trt103 |Immunocompromised            |         4.1181357| 0.3422433| 124.6625022|      1.4124359| 0.3162958|              4|                 17|         4|            18|TACTIC-R      |Baricitinib |
|trt75  |No comorbidity_firth         |         1.0973275| 0.0578292| 161.1420251|      1.5034095| 0.9545145|              1|                 78|         0|            27|RUXCOVID      |Ruxolitinib |
|trt86  |One comorbidity              |         2.4121297| 0.2931049|  51.1062611|      1.1928393| 0.4604153|              4|                 85|         1|            49|RUXCOVID      |Ruxolitinib |
|trt94  |Multiple comorbidities       |         0.9901053| 0.1679785|   7.6965819|      0.9226840| 0.9914012|              4|                119|         2|            66|RUXCOVID      |Ruxolitinib |
|trt76  |No comorbidity               |         0.1568725| 0.0018455|   6.5063845|      1.8924981| 0.3276933|              1|                 37|         1|            25|PANCOVID      |Baricitinib |
|trt87  |One comorbidity_firth        |         0.3019481| 0.0230217|   2.6596564|      1.0374992| 0.2779638|              1|                 45|         2|            36|PANCOVID      |Baricitinib |
|trt95  |Multiple comorbidities_firth |         0.1874210| 0.0011875|   2.9390338|      1.3998641| 0.2578515|              0|                 58|         3|            75|PANCOVID      |Baricitinib |

```r
kable(df_sg_comed_mort28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|       |variable                       | hazard_odds_ratio|  ci_lower|    ci_upper| standard_error|   p_value| n_intervention| n_intervention_tot| n_control| n_control_tot|trial         |JAKi        |
|:------|:------------------------------|-----------------:|---------:|-----------:|--------------:|---------:|--------------:|------------------:|---------:|-------------:|:-------------|:-----------|
|trt12  |No Dexa, no Tocilizumab        |         0.6654049| 0.3654969|   1.1915474|      0.3000191| 0.1745339|             22|                479|        35|           466|ACTT-2        |Baricitinib |
|trt13  |Dexa, but no Tocilizumab       |         2.9174518| 0.2213323|  51.4447749|      1.3066912| 0.4125548|              2|                 15|         2|            26|ACTT-2        |Baricitinib |
|trt121 |No Dexa, no Tocilizumab        |         0.3812643| 0.1450479|   0.9209251|      0.4659481| 0.0385026|              9|                145|        21|           159|COV-BARRIER   |Baricitinib |
|trt131 |Dexa, but no Tocilizumab       |         0.5379299| 0.3721265|   0.7721814|      0.1859546| 0.0008552|             73|                613|       108|           606|COV-BARRIER   |Baricitinib |
|trt9   |No Dexa, no Tocilizumab_firth  |         0.3455477| 0.0018718|  21.3158124|      1.8492116| 0.5749819|              0|                  5|         1|             8|Bari-SolidAct |Baricitinib |
|trt10  |Dexa, but no Tocilizumab       |         0.6922707| 0.3127963|   1.5013292|      0.3974459| 0.3547818|             15|                132|        20|           131|Bari-SolidAct |Baricitinib |
|trt91  |No Dexa, no Tocilizumab_firth  |         0.2037385| 0.0014640|   2.6000348|      1.3605340| 0.2406854|              0|                 43|         2|            49|COVINIB       |Baricitinib |
|trt101 |Dexa, but no Tocilizumab_firth |         0.6005072| 0.0002599| 105.8983987|      1.8568148| 0.7916402|              0|                 10|         0|             5|COVINIB       |Baricitinib |
|trt8   |No Dexa, no Tocilizumab_firth  |         0.7355841| 0.0027926| 295.8054120|      1.9546827| 0.8874282|              0|                  4|         0|             3|TOFACOV       |Tofacitinib |
|trt92  |Dexa, but no Tocilizumab_firth |         2.5607746| 0.1263794| 389.6015294|      1.3173202| 0.5518554|              1|                 54|         0|            55|TOFACOV       |Tofacitinib |
|trt81  |Dexa, but no Tocilizumab       |         0.7908805| 0.1472786|   3.8263828|      0.7982351| 0.7688275|              3|                 46|         4|            51|Ghazaeian     |Tofacitinib |
|trt11  |No Dexa, no Tocilizumab        |         0.7069704| 0.3239083|   1.5125246|      0.3905753| 0.3746294|             17|                163|        19|           176|RECOVERY      |Baricitinib |
|trt122 |Dexa, but no Tocilizumab       |         0.8683616| 0.7231699|   1.0424523|      0.0932543| 0.1301347|            314|               2584|       313|          2491|RECOVERY      |Baricitinib |
|trt132 |Dexa and Tocilizumab           |         0.6961076| 0.5456052|   0.8867254|      0.1238151| 0.0034364|            174|               1296|       209|          1257|RECOVERY      |Baricitinib |
|trt14  |Tocilizumab, but no Dexa       |         2.4876095| 0.5116709|  14.1863269|      0.8307323| 0.2726367|              8|                 18|         4|            16|RECOVERY      |Baricitinib |
|trt111 |No Dexa, no Tocilizumab        |         1.4949575| 0.2191866|  10.8322478|      0.9622551| 0.6760420|              4|                 14|         3|            19|TACTIC-R      |Baricitinib |
|trt123 |Dexa, but no Tocilizumab       |         0.8410536| 0.3110521|   2.2637460|      0.5011918| 0.7298113|             14|                113|        12|           112|TACTIC-R      |Baricitinib |
|trt133 |Dexa and Tocilizumab_firth     |         0.2648583| 0.0002575|  17.2561182|      1.9399011| 0.5439963|              0|                  3|         2|             7|TACTIC-R      |Baricitinib |
|trt102 |No Dexa, no Tocilizumab        |         1.4643774| 0.2973444|  10.6241413|      0.8642946| 0.6589816|              5|                115|         2|            64|RUXCOVID      |Ruxolitinib |
|trt112 |Dexa, but no Tocilizumab       |         1.7041865| 0.2228257|  34.8053599|      1.1613405| 0.6462140|              4|                167|         1|            78|RUXCOVID      |Ruxolitinib |
|trt103 |Dexa, but no Tocilizumab       |         0.5974062| 0.0638119|   4.9334371|      1.0461094| 0.6224003|              2|                135|         3|           123|PANCOVID      |Baricitinib |
|trt113 |Dexa and Tocilizumab_firth     |         0.5405387| 0.0034865|  10.5496740|      1.6719732| 0.7060920|              0|                  5|         3|            13|PANCOVID      |Baricitinib |

```r
kable(df_sg_vacc_ae28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|       |variable         | hazard_odds_ratio|  ci_lower|      ci_upper| standard_error|   p_value| n_intervention| n_intervention_tot| n_control| n_control_tot|trial         |JAKi        |
|:------|:----------------|-----------------:|---------:|-------------:|--------------:|---------:|--------------:|------------------:|---------:|-------------:|:-------------|:-----------|
|trt11  |vaccinated       |         1.0153459| 0.3847503|  2.689931e+00|   4.927174e-01| 0.9753421|             14|                 40|        16|            44|Bari-SolidAct |Baricitinib |
|trt12  |not vaccinated   |         0.7785301| 0.3932854|  1.529344e+00|   3.451862e-01| 0.4682963|             29|                 87|        28|            77|Bari-SolidAct |Baricitinib |
|trt111 |vaccinated_firth |         1.0000980| 0.0000000| 2.005975e+204|   2.399885e+00| 1.0000000|              0|                  2|         0|             0|COVINIB       |Baricitinib |
|trt121 |not vaccinated   |         0.8436332| 0.3258987|  2.160550e+00|   4.783483e-01| 0.7222391|             11|                 53|        12|            53|COVINIB       |Baricitinib |
|trt10  |vaccinated_firth |         1.0074623| 0.0000000|  7.364779e+09|   3.839147e+06| 1.0000000|              0|                  1|         0|             2|TOFACOV       |Tofacitinib |
|trt112 |not vaccinated   |         0.6777994| 0.2341347|  1.922727e+00|   5.314252e-01| 0.4642826|              9|                 57|        13|            56|TOFACOV       |Tofacitinib |
|trt15  |vaccinated       |         0.8843193| 0.6526309|  1.197942e+00|   1.546928e-01| 0.4267778|             90|               1468|        91|          1362|RECOVERY      |Baricitinib |
|trt16  |not vaccinated   |         0.9244622| 0.7224957|  1.182727e+00|   1.256030e-01| 0.5317556|            139|               2091|       139|          2037|RECOVERY      |Baricitinib |
|trt122 |vaccinated       |         1.4750109| 0.2100704|  1.268436e+01|   9.943302e-01| 0.6958847|              3|                 19|         2|            17|PANCOVID      |Baricitinib |
|trt13  |not vaccinated   |         1.1430250| 0.5257026|  2.514096e+00|   3.959354e-01| 0.7356446|             16|                124|        14|           119|PANCOVID      |Baricitinib |

```r
kable(df_sg_symp_mort28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|       |variable                | hazard_odds_ratio|  ci_lower|     ci_upper| standard_error|   p_value| n_intervention| n_intervention_tot| n_control| n_control_tot|trial         |JAKi        |
|:------|:-----------------------|-----------------:|---------:|------------:|--------------:|---------:|--------------:|------------------:|---------:|-------------:|:-------------|:-----------|
|trt14  |More than 10 days       |         0.5058205| 0.0943159| 2.193279e+00|      0.7765180| 0.3800900|              3|                118|        11|           129|ACTT-2        |Baricitinib |
|trt15  |Between 5-10 days       |         0.7310760| 0.3315374| 1.588828e+00|      0.3967103| 0.4297682|             15|                247|        17|           238|ACTT-2        |Baricitinib |
|trt16  |5 days and less         |         0.7104569| 0.2209617| 2.166117e+00|      0.5717647| 0.5499197|              6|                129|         9|           124|ACTT-2        |Baricitinib |
|trt141 |More than 10 days       |         0.4369590| 0.2648428| 7.093433e-01|      0.2506618| 0.0009568|             37|                363|        66|           358|COV-BARRIER   |Baricitinib |
|trt151 |Between 5-10 days       |         0.6139536| 0.3609758| 1.030274e+00|      0.2667360| 0.0674133|             40|                312|        50|           329|COV-BARRIER   |Baricitinib |
|trt161 |5 days and less         |         0.3162069| 0.0844070| 1.036694e+00|      0.6289219| 0.0671475|              5|                 82|        12|            75|COV-BARRIER   |Baricitinib |
|trt13  |More than 10 days       |         0.0739507| 0.0033760| 5.180141e-01|      1.1898877| 0.0286156|              1|                 41|        10|            56|Bari-SolidAct |Baricitinib |
|trt142 |Between 5-10 days       |         1.0123236| 0.3080375| 3.428702e+00|      0.6038935| 0.9838182|              8|                 79|         6|            69|Bari-SolidAct |Baricitinib |
|trt152 |5 days and less         |         1.6344455| 0.2916523| 1.057614e+01|      0.8920304| 0.5817913|              6|                 17|         5|            15|Bari-SolidAct |Baricitinib |
|trt131 |Between 5-10 days_firth |         0.3211533| 0.0022370| 5.873839e+00|      1.3674548| 0.4494179|              0|                 15|         1|            15|COVINIB       |Baricitinib |
|trt143 |5 days and less_firth   |         0.2792402| 0.0018768| 5.345121e+00|      1.4061063| 0.4040744|              0|                 53|         2|            54|COVINIB       |Baricitinib |
|trt12  |More than 10 days_firth |         0.7396999| 0.0039160| 1.386971e+02|      1.8179073| 0.8816021|              0|                 14|         0|            14|TOFACOV       |Tofacitinib |
|trt132 |Between 5-10 days_firth |         2.2140054| 0.1104893| 3.199455e+02|      1.3096022| 0.6123815|              1|                 36|         0|            34|TOFACOV       |Tofacitinib |
|trt144 |5 days and less_firth   |         0.9633832| 0.0056439| 1.605331e+02|      1.5390619| 0.9844307|              0|                  8|         0|            10|TOFACOV       |Tofacitinib |
|trt9   |More than 10 days       |         2.9024345| 0.0835128| 1.198579e+02|      1.6591221| 0.5207192|              1|                  4|         1|            10|Ghazaeian     |Tofacitinib |
|trt10  |Between 5-10 days       |         0.4834340| 0.0216008| 5.406545e+00|      1.2594224| 0.5638570|              1|                 31|         2|            31|Ghazaeian     |Tofacitinib |
|trt11  |5 days and less         |         0.5674658| 0.0083068| 1.867271e+01|      1.6959239| 0.7383190|              1|                 11|         1|            10|Ghazaeian     |Tofacitinib |
|trt17  |More than 10 days       |         0.7990657| 0.6269355| 1.017568e+00|      0.1234705| 0.0692590|            171|               1734|       184|          1662|RECOVERY      |Baricitinib |
|trt18  |Between 5-10 days       |         0.7563059| 0.6083257| 9.393367e-01|      0.1107783| 0.0116910|            221|               1779|       244|          1761|RECOVERY      |Baricitinib |
|trt19  |5 days and less         |         0.9395257| 0.6879729| 1.282821e+00|      0.1588092| 0.6944680|            121|                548|       117|           517|RECOVERY      |Baricitinib |
|trt145 |More than 10 days       |         0.5045655| 0.1437220| 1.616668e+00|      0.6080721| 0.2606055|              6|                 60|        10|            68|TACTIC-R      |Baricitinib |
|trt153 |Between 5-10 days       |         1.2964431| 0.2556725| 7.048851e+00|      0.8240088| 0.7527049|              6|                 44|         4|            51|TACTIC-R      |Baricitinib |
|trt162 |5 days and less         |         0.7874688| 0.0836207| 6.826361e+00|      1.0859700| 0.8258581|              6|                 24|         3|            19|TACTIC-R      |Baricitinib |
|trt121 |More than 10 days       |         1.2046913| 0.2269692| 8.922388e+00|      0.8842326| 0.8331959|              4|                144|         2|            79|RUXCOVID      |Ruxolitinib |
|trt133 |Between 5-10 days_firth |         1.6877981| 0.1187169| 2.400328e+02|      1.3658585| 0.7299147|              2|                114|         0|            52|RUXCOVID      |Ruxolitinib |
|trt146 |5 days and less         |         0.1098062| 0.0002275| 6.801107e+00|      2.3000080| 0.3368297|              3|                 24|         1|            10|RUXCOVID      |Ruxolitinib |
|trt147 |More than 10 days_firth |         0.1681943| 0.0000939| 4.203424e+00|      1.4704909| 0.3018142|              0|                 38|         1|            22|PANCOVID      |Baricitinib |
|trt154 |Between 5-10 days_firth |         0.4977165| 0.0000000| 1.392540e+04|      1.4684665| 0.7472348|              0|                 67|         1|            69|PANCOVID      |Baricitinib |
|trt163 |5 days and less         |         0.5685283| 0.0587075| 4.177559e+00|      1.0372987| 0.5861671|              2|                 35|         4|            45|PANCOVID      |Baricitinib |

```r
kable(df_sg_crp_mort28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|       |variable                | hazard_odds_ratio|  ci_lower|     ci_upper| standard_error|   p_value| n_intervention| n_intervention_tot| n_control| n_control_tot|trial         |JAKi        |
|:------|:-----------------------|-----------------:|---------:|------------:|--------------:|---------:|--------------:|------------------:|---------:|-------------:|:-------------|:-----------|
|trt17  |CRP 75 and higher       |         0.5399877| 0.2800221| 1.011633e+00|      0.3256949| 0.0584934|             17|                332|        32|           321|ACTT-2        |Baricitinib |
|trt18  |CRP below 75            |         2.6842316| 0.5455134| 1.593476e+01|      0.8357233| 0.2374101|              5|                142|         4|           155|ACTT-2        |Baricitinib |
|trt171 |CRP 75 and higher       |         0.5538630| 0.3339109| 9.076951e-01|      0.2544456| 0.0202297|             40|                279|        53|           274|COV-BARRIER   |Baricitinib |
|trt181 |CRP below 75            |         0.6042953| 0.3450617| 1.043177e+00|      0.2812340| 0.0732920|             30|                328|        53|           362|COV-BARRIER   |Baricitinib |
|trt16  |CRP 75 and higher       |         0.8154793| 0.3097797| 2.074896e+00|      0.4805930| 0.6712505|             10|                 68|        16|            87|Bari-SolidAct |Baricitinib |
|trt172 |CRP below 75            |         0.6505667| 0.1545927| 2.664087e+00|      0.7090284| 0.5442898|              5|                 64|         5|            51|Bari-SolidAct |Baricitinib |
|trt15  |CRP 75 and higher_firth |         0.2566485| 0.0011791| 5.486143e+00|      1.4168444| 0.3927276|              0|                 30|         1|            25|COVINIB       |Baricitinib |
|trt161 |CRP below 75_firth      |         0.2436989| 0.0000120| 5.526709e+00|      1.4582197| 0.3923310|              0|                 23|         1|            29|COVINIB       |Baricitinib |
|trt151 |CRP 75 and higher_firth |         1.0000000| 0.0000000| 2.685274e+38|      3.3984400| 1.0000000|              0|                  1|         0|             2|TOFACOV       |Tofacitinib |
|trt162 |CRP below 75_firth      |         2.5399995| 0.1307656| 3.735116e+02|      1.3151530| 0.5470383|              1|                 57|         0|            55|TOFACOV       |Tofacitinib |
|trt12  |CRP 75 and higher       |         0.5965489| 0.0247845| 7.899474e+00|      1.3282549| 0.6973302|              1|                 16|         2|            16|Ghazaeian     |Tofacitinib |
|trt13  |CRP below 75            |         1.0842493| 0.1216951| 9.604896e+00|      1.0395168| 0.9379768|              2|                 29|         2|            34|Ghazaeian     |Tofacitinib |
|trt20  |CRP 75 and higher       |         0.8244370| 0.6810753| 9.974766e-01|      0.0972990| 0.0472405|            283|               2238|       308|          2220|RECOVERY      |Baricitinib |
|trt21  |CRP below 75            |         0.8182184| 0.6585040| 1.016098e+00|      0.1105972| 0.0696742|            225|               1766|       229|          1676|RECOVERY      |Baricitinib |
|trt173 |CRP 75 and higher       |         0.8408724| 0.3381275| 2.057402e+00|      0.4570872| 0.7045591|             16|                 88|        15|           103|TACTIC-R      |Baricitinib |
|trt182 |CRP below 75            |         0.5298220| 0.0260199| 9.129443e+00|      1.4005096| 0.6501460|              2|                 41|         2|            35|TACTIC-R      |Baricitinib |
|trt152 |CRP 75 and higher       |         1.1965174| 0.2315528| 8.996186e+00|      0.8881625| 0.8399111|              5|                 90|         2|            37|RUXCOVID      |Ruxolitinib |
|trt163 |CRP below 75_firth      |         4.4774319| 0.4533582| 5.998628e+02|      1.3565592| 0.2329516|              4|                187|         0|           101|RUXCOVID      |Ruxolitinib |
|trt174 |CRP 75 and higher_firth |         0.0852210| 0.0005786| 1.070148e+00|      1.4063062| 0.0573738|              0|                 50|         3|            45|PANCOVID      |Baricitinib |
|trt183 |CRP below 75            |         0.9213102| 0.0894348| 9.279883e+00|      1.1102131| 0.9411518|              2|                 61|         3|            64|PANCOVID      |Baricitinib |

```r
kable(df_sg_atrisk_ae28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|       |variable          | hazard_odds_ratio|  ci_lower|    ci_upper| standard_error|   p_value| n_intervention| n_intervention_tot| n_control| n_control_tot|trial         |JAKi        |
|:------|:-----------------|-----------------:|---------:|-----------:|--------------:|---------:|--------------:|------------------:|---------:|-------------:|:-------------|:-----------|
|trt19  |Not at risk       |         0.9313914| 0.6578292|   1.3187055|      0.1772541| 0.6884336|            118|                317|       115|           294|ACTT-2        |Baricitinib |
|trt20  |At risk           |         0.9003117| 0.5833286|   1.3883032|      0.2209306| 0.6345539|             78|                174|        89|           187|ACTT-2        |Baricitinib |
|trt191 |Not at risk       |         1.0216675| 0.7190005|   1.4522761|      0.1791096| 0.9047355|             94|                420|        83|           398|COV-BARRIER   |Baricitinib |
|trt201 |At risk           |         1.2457939| 0.8239951|   1.8900601|      0.2114457| 0.2986267|             77|                263|        60|           244|COV-BARRIER   |Baricitinib |
|trt18  |Not at risk       |         0.7806200| 0.3623424|   1.6647714|      0.3870626| 0.5222621|             25|                 75|        24|            68|Bari-SolidAct |Baricitinib |
|trt192 |At risk           |         0.9478624| 0.4266395|   2.1030638|      0.4051799| 0.8948628|             19|                 55|        20|            55|Bari-SolidAct |Baricitinib |
|trt17  |Not at risk       |         0.9038391| 0.2632798|   3.0179659|      0.6113948| 0.8686559|              6|                 38|         7|            42|COVINIB       |Baricitinib |
|trt181 |At risk           |         0.4843481| 0.0871070|   2.5330444|      0.8438136| 0.3902650|              5|                 17|         5|            11|COVINIB       |Baricitinib |
|trt171 |Not at risk       |         0.6561957| 0.2128490|   1.9673509|      0.5612381| 0.4528602|              8|                 37|        12|            39|TOFACOV       |Tofacitinib |
|trt182 |At risk           |         0.5175578| 0.0131880|  16.6610804|      1.6164984| 0.6836812|              1|                 21|         1|            19|TOFACOV       |Tofacitinib |
|trt14  |Not at risk_firth |         3.6521739| 0.1842096| 554.2079690|      1.4736504| 0.4013868|              1|                 32|         0|            41|Ghazaeian     |Tofacitinib |
|trt22  |Not at risk       |         0.9777556| 0.7614186|   1.2555060|      0.1274477| 0.8598950|            133|               2295|       133|          2287|RECOVERY      |Baricitinib |
|trt23  |At risk           |         0.8288704| 0.6159894|   1.1149735|      0.1511587| 0.2143528|             96|               1264|        97|          1112|RECOVERY      |Baricitinib |
|trt193 |Not at risk       |         2.5760892| 0.7687116|  10.0689264|      0.6387149| 0.1384666|              8|                 65|         4|            79|TACTIC-R      |Baricitinib |
|trt202 |At risk           |         0.8247433| 0.2989248|   2.2853379|      0.5135367| 0.7075058|             10|                 54|        10|            49|TACTIC-R      |Baricitinib |
|trt172 |Not at risk       |         0.4240682| 0.1915337|   0.9284717|      0.3993909| 0.0317198|             16|                189|        16|            97|RUXCOVID      |Ruxolitinib |
|trt183 |At risk           |         1.1562778| 0.3843312|   3.9284333|      0.5813974| 0.8027780|             11|                 84|         5|            42|RUXCOVID      |Ruxolitinib |
|trt194 |Not at risk       |         1.3360574| 0.3548438|   5.2708749|      0.6735382| 0.6670859|              6|                 50|         5|            49|PANCOVID      |Baricitinib |
|trt203 |At risk           |         1.0600390| 0.4404118|   2.5841715|      0.4468885| 0.8961943|             13|                 93|        11|            87|PANCOVID      |Baricitinib |

```r
kable(df_sg_comed_ae28, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|       |variable                          | hazard_odds_ratio|  ci_lower|   ci_upper| standard_error|   p_value| n_intervention| n_intervention_tot| n_control| n_control_tot|trial         |JAKi        |
|:------|:---------------------------------|-----------------:|---------:|----------:|--------------:|---------:|--------------:|------------------:|---------:|-------------:|:-------------|:-----------|
|trt21  |No Dexa, no Tocilizumab_AE        |      9.164330e-01| 0.6965700|  1.2055453|      0.1398572| 0.5326493|            188|                477|       192|           457|ACTT-2        |Baricitinib |
|trt22  |Dexa, but no Tocilizumab_AE       |      4.928888e-01| 0.0368487|  4.9694556|      1.2040117| 0.5568039|              8|                 14|        12|            24|ACTT-2        |Baricitinib |
|trt211 |No Dexa, no Tocilizumab_AE        |      1.359701e+00| 0.7401130|  2.5229187|      0.3115837| 0.3240646|             32|                139|        24|           138|COV-BARRIER   |Baricitinib |
|trt221 |Dexa, but no Tocilizumab_AE       |      1.056360e+00| 0.7834353|  1.4253375|      0.1525382| 0.7192612|            139|                544|       119|           504|COV-BARRIER   |Baricitinib |
|trt20  |No Dexa, no Tocilizumab_AE        |      5.490797e-01| 0.0386955|  6.9382508|      1.2644230| 0.6354011|              2|                  8|         4|             9|Bari-SolidAct |Baricitinib |
|trt212 |Dexa, but no Tocilizumab_AE       |      9.557239e-01| 0.5456751|  1.6729367|      0.2851094| 0.8737966|             42|                122|        39|           113|Bari-SolidAct |Baricitinib |
|trt19  |No Dexa, no Tocilizumab_AE        |      8.244015e-01| 0.2924886|  2.2707095|      0.5175401| 0.7090692|              9|                 44|        11|            48|COVINIB       |Baricitinib |
|trt201 |Dexa, but no Tocilizumab_AE       |      4.622442e-01| 0.0237634| 13.1726047|      1.4759221| 0.6010899|              2|                 11|         1|             5|COVINIB       |Baricitinib |
|trt191 |No Dexa, no Tocilizumab_AE_firth  |      5.850331e-01| 0.0035637| 60.0965918|      1.7769302| 0.7766083|              0|                  4|         1|             3|TOFACOV       |Tofacitinib |
|trt202 |Dexa, but no Tocilizumab_AE       |      8.061029e-01| 0.2724618|  2.3617200|      0.5448572| 0.6924024|              9|                 54|        12|            55|TOFACOV       |Tofacitinib |
|trt15  |Dexa, but no Tocilizumab_AE_firth |      5.043375e+07| 0.0000000|         NA|   4148.1471061| 0.9965885|              1|                 46|         0|            51|Ghazaeian     |Tofacitinib |
|trt24  |No Dexa, no Tocilizumab_AE        |      6.107571e-01| 0.2356888|  1.4831933|      0.4628046| 0.2867107|              8|                146|        14|           157|RECOVERY      |Baricitinib |
|trt25  |Dexa, but no Tocilizumab_AE       |      9.193613e-01| 0.7136172|  1.1843802|      0.1291051| 0.5149035|            130|               2279|       129|          2182|RECOVERY      |Baricitinib |
|trt213 |No Dexa, no Tocilizumab_AE        |      1.589858e-01| 0.0006961|  3.9368587|      1.9042628| 0.3341959|              1|                 11|         2|            18|TACTIC-R      |Baricitinib |
|trt222 |Dexa, but no Tocilizumab_AE       |      1.276201e+00| 0.5573211|  2.9814719|      0.4238777| 0.5650391|             15|                103|        12|           105|TACTIC-R      |Baricitinib |
|trt192 |No Dexa, no Tocilizumab_AE        |      3.641774e-01| 0.1325555|  0.9609916|      0.4985872| 0.0427696|              8|                110|        12|            62|RUXCOVID      |Ruxolitinib |
|trt203 |Dexa, but no Tocilizumab_AE       |      9.238495e-01| 0.3959037|  2.2848053|      0.4421620| 0.8578328|             19|                163|         9|            77|RUXCOVID      |Ruxolitinib |
|trt214 |Dexa, but no Tocilizumab_AE       |      1.141285e+00| 0.5457068|  2.4204059|      0.3771301| 0.7260226|             18|                138|        15|           126|PANCOVID      |Baricitinib |

# Reshape dataframes for each subgroup estimate (for overall forestplot only; descriptive purpose)

```r
## Respiratory support: No oxygen
outcomes1 <- "No oxygen"
outcomes1.firth <- "No oxygen_firth"
# Initialize an empty data frame to store the selected rows
df_sg_no_ox_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_no_ox_mort28 <- rbind(df_sg_no_ox_mort28, selected_rows)
}

## Respiratory support: low oxygen
outcomes1 <- "low-flow oxygen"
outcomes1.firth <- "low-flow oxygen_firth"
# Initialize an empty data frame to store the selected rows
df_sg_low_ox_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_low_ox_mort28 <- rbind(df_sg_low_ox_mort28, selected_rows)
}

## Respiratory support: niv
outcomes1 <- "high-flow oxygen / NIV"
outcomes1.firth <- "high-flow oxygen / NIV_firth"
# Initialize an empty data frame to store the selected rows
df_sg_niv_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_niv_mort28 <- rbind(df_sg_niv_mort28, selected_rows)
}

## Respiratory support: ecmo
outcomes1 <- "Mechanical ventilation / ECMO"
outcomes1.firth <- "Mechanical ventilation / ECMO_firth"
# Initialize an empty data frame to store the selected rows
df_sg_ecmo_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_ecmo_mort28 <- rbind(df_sg_ecmo_mort28, selected_rows)
}


## Ventilation: No ventilation
outcomes1 <- "None or low-flow oxygen"
outcomes1.firth <- "None or low-flow oxygen_firth"
# Initialize an empty data frame to store the selected rows
df_sg_no_vent_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_no_vent_mort28 <- rbind(df_sg_no_vent_mort28, selected_rows)
}

## Ventilation: Ventilation
outcomes1 <- "High-flow or non-invasive, mechanical, or ECMO"
outcomes1.firth <- "High-flow or non-invasive, mechanical, or ECMO_firth"
# Initialize an empty data frame to store the selected rows
df_sg_vent_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_vent_mort28 <- rbind(df_sg_vent_mort28, selected_rows)
}


## Age: 70 and above
outcomes1 <- "70 years and above"
outcomes1.firth <- "70 years and above_firth"
# Initialize an empty data frame to store the selected rows
df_sg_old_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_old_mort28 <- rbind(df_sg_old_mort28, selected_rows)
}

## Age: below 70
outcomes1 <- "below 70 years"
outcomes1.firth <- "below 70 years_firth"
# Initialize an empty data frame to store the selected rows
df_sg_young_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_young_mort28 <- rbind(df_sg_young_mort28, selected_rows)
}


## Comorbidity: No comorbidity
outcomes1 <- "No comorbidity"
outcomes1.firth <- "No comorbidity_firth"
# Initialize an empty data frame to store the selected rows
df_sg_no_comorb_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_no_comorb_mort28 <- rbind(df_sg_no_comorb_mort28, selected_rows)
}

## Comorbidity: One comorbidity
outcomes1 <- "One comorbidity"
outcomes1.firth <- "One comorbidity_firth"
# Initialize an empty data frame to store the selected rows
df_sg_one_comorb_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_one_comorb_mort28 <- rbind(df_sg_one_comorb_mort28, selected_rows)
}

## Comorbidity: Multiple comorbidities
outcomes1 <- "Multiple comorbidities"
outcomes1.firth <- "Multiple comorbidities_firth"
# Initialize an empty data frame to store the selected rows
df_sg_mult_comorb_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_mult_comorb_mort28 <- rbind(df_sg_mult_comorb_mort28, selected_rows)
}

## Comorbidity: Immunosuppressed
outcomes1 <- "Immunocompromised"
outcomes1.firth <- "Immunocompromised_firth"
# Initialize an empty data frame to store the selected rows
df_sg_immun_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_immun_mort28 <- rbind(df_sg_immun_mort28, selected_rows)
}


## Comedication: No Dexa, no Tocilizumab
outcomes1 <- "No Dexa, no Tocilizumab"
outcomes1.firth <- "No Dexa, no Tocilizumab_firth"
# Initialize an empty data frame to store the selected rows
df_sg_no_comed_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_no_comed_mort28 <- rbind(df_sg_no_comed_mort28, selected_rows)
}

## Comedication: Dexa, but no Tocilizumab
outcomes1 <- "Dexa, but no Tocilizumab"
outcomes1.firth <- "Dexa, but no Tocilizumab_firth"
# Initialize an empty data frame to store the selected rows
df_sg_dexa_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_dexa_mort28 <- rbind(df_sg_dexa_mort28, selected_rows)
}

## Comedication: Dexa and Tocilizumab
outcomes1 <- "Dexa and Tocilizumab"
outcomes1.firth <- "Dexa and Tocilizumab_firth"
# Initialize an empty data frame to store the selected rows
df_sg_dexa_toci_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_dexa_toci_mort28 <- rbind(df_sg_dexa_toci_mort28, selected_rows)
}

## Comedication: Tocilizumab but no Dexa
outcomes1 <- "Tocilizumab, but no Dexa"
outcomes1.firth <- "Tocilizumab, but no Dexa_firth"
# Initialize an empty data frame to store the selected rows
df_sg_toci_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_toci_mort28 <- rbind(df_sg_toci_mort28, selected_rows)
}


## Symptom onset: More than 10 days
outcomes1 <- "More than 10 days"
outcomes1.firth <- "More than 10 days_firth"
# Initialize an empty data frame to store the selected rows
df_sg_m10_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_m10_mort28 <- rbind(df_sg_m10_mort28, selected_rows)
}

## Symptom onset: Between 5-10 days
outcomes1 <- "Between 5-10 days"
outcomes1.firth <- "Between 5-10 days_firth"
# Initialize an empty data frame to store the selected rows
df_sg_510_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_510_mort28 <- rbind(df_sg_510_mort28, selected_rows)
}

## Symptom onset: 5 days and less
outcomes1 <- "5 days and less"
outcomes1.firth <- "5 days and less_firth"
# Initialize an empty data frame to store the selected rows
df_sg_5_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_5_mort28 <- rbind(df_sg_5_mort28, selected_rows)
}


## CRP: Above 75
outcomes1 <- "CRP 75 and higher"
outcomes1.firth <- "CRP 75 and higher_firth"
# Initialize an empty data frame to store the selected rows
df_sg_crpabove75_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_crpabove75_mort28 <- rbind(df_sg_crpabove75_mort28, selected_rows)
}

## CRP: Below 75
outcomes1 <- "CRP below 75"
outcomes1.firth <- "CRP below 75_firth"
# Initialize an empty data frame to store the selected rows
df_sg_crpbelow75_mort28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_crpbelow75_mort28 <- rbind(df_sg_crpbelow75_mort28, selected_rows)
}


## Vaccination on AEs: Vaccinated
outcomes1 <- "vaccinated"
outcomes1.firth <- "vaccinated_firth"
# Initialize an empty data frame to store the selected rows
df_sg_vaccyes_ae28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_vaccyes_ae28 <- rbind(df_sg_vaccyes_ae28, selected_rows)
}

## Vaccination on AEs: Not Vaccinated
outcomes1 <- "not vaccinated"
outcomes1.firth <- "not vaccinated_firth"
# Initialize an empty data frame to store the selected rows
df_sg_vaccno_ae28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_vaccno_ae28 <- rbind(df_sg_vaccno_ae28, selected_rows)
}


## At risk on AEs: Not at risk
outcomes1 <- "Not at risk"
outcomes1.firth <- "Not at risk_firth"
# Initialize an empty data frame to store the selected rows
df_sg_atriskno_ae28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_atriskno_ae28 <- rbind(df_sg_atriskno_ae28, selected_rows)
}

## At risk on AEs: At risk
outcomes1 <- "At risk"
outcomes1.firth <- "At risk_firth"
# Initialize an empty data frame to store the selected rows
df_sg_atriskyes_ae28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_atriskyes_ae28 <- rbind(df_sg_atriskyes_ae28, selected_rows)
}


## Comedication on AEs: No Dexa, no Tocilizumab
outcomes1 <- "No Dexa, no Tocilizumab_AE"
outcomes1.firth <- "No Dexa, no Tocilizumab_AE_firth"
# Initialize an empty data frame to store the selected rows
df_sg_no_comed_ae28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_no_comed_ae28 <- rbind(df_sg_no_comed_ae28, selected_rows)
}

## Comedication on AEs: Dexa, but no Tocilizumab
outcomes1 <- "Dexa, but no Tocilizumab_AE"
outcomes1.firth <- "Dexa, but no Tocilizumab_AE_firth"
# Initialize an empty data frame to store the selected rows
df_sg_dexa_ae28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_dexa_ae28 <- rbind(df_sg_dexa_ae28, selected_rows)
}

## Comedication on AEs: Dexa and Tocilizumab
outcomes1 <- "Dexa and Tocilizumab_AE"
outcomes1.firth <- "Dexa and Tocilizumab_AE_firth"
# Initialize an empty data frame to store the selected rows
df_sg_dexa_toci_ae28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_dexa_toci_ae28 <- rbind(df_sg_dexa_toci_ae28, selected_rows)
}

## Comedication on AEs: Tocilizumab but no Dexa
outcomes1 <- "Tocilizumab, but no Dexa_AE"
outcomes1.firth <- "Tocilizumab, but no Dexa_AE_firth"
# Initialize an empty data frame to store the selected rows
df_sg_toci_ae28 <- data.frame()
# Loop through the list of data frames
for (df in list_subgroup_df) {
  selected_rows <- df %>% filter(variable == outcomes1 | variable == outcomes1.firth)
  df_sg_toci_ae28 <- rbind(df_sg_toci_ae28, selected_rows)
}
```

# Interaction: Respiratory support (proxy for disease severity) on primary endpoint

```r
# str(df_rs_mort28)
## "Ghazaeian" do only have events in 1 subgroup ? -> see deft: do not include! "(insufficient data)"
df_rs_mort28 <- df_rs_mort28[order(df_rs_mort28$trial), ]
rs.mort28 <- metagen(TE = log(log_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_rs_mort28,
                      sm = "OR",
                      fixed = F, 
                      random = T, 
                      method.tau = "ML", 
                      method.random.ci = "HK",
                      # subset = trial %in% c("COV-BARRIER", "ACTT-2", "RECOVERY", "TACTIC-R"),
                      exclude = trial %in% c("Ghazaeian") # incl in plot but exclude from analysis
                      )
summary(rs.mort28)
```

```
##                   OR             95%-CI %W(random) exclude
## ACTT-2        1.7465 [0.8842;   3.4499]        7.5        
## Bari-SolidAct 0.2121 [0.0270;   1.6637]        0.8        
## COV-BARRIER   0.8771 [0.5637;   1.3647]       17.7        
## COVINIB       0.2524 [0.0021;  30.2401]        0.2        
## Ghazaeian     1.0000 [0.9378;   1.0664]        0.0       *
## PANCOVID      0.0372 [0.0009;   1.5461]        0.2        
## RECOVERY      0.8420 [0.6756;   1.0495]       71.4        
## RUXCOVID      1.2193 [0.0886;  16.7713]        0.5        
## TACTIC-R      0.8715 [0.1982;   3.8325]        1.6        
## TOFACOV       1.1884 [0.0095; 148.1544]        0.1        
## 
## Number of studies: k = 9
## 
##                               OR           95%-CI     t p-value
## Random effects model (HK) 0.8797 [0.6976; 1.1095] -1.27  0.2387
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 2.9373]; tau = 0 [0.0000; 1.7139]
##  I^2 = 11.0% [0.0%; 68.7%]; H = 1.06 [1.00; 1.79]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  8.99    8  0.3435
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 8)
```

```r
forest.meta(rs.mort28,
            hetstat = F,
            leftcols = c("studlab"),
            leftlabs = c("Trial"),
            # text.common = "Average interaction effect (common effect model)*",
            # text.random = "Average interaction effect (random effect model)*",
            text.random = "",
            xlab = "more resp support: greater effect < > less resp support: greater effect",
            xlab.pos = 0.0,
            fs.xlab = 11
            )
```

![](two_stage_files/figure-html/unnamed-chunk-43-1.png)<!-- -->

# Subgroups: Respiratory support (proxy for disease severity) on primary endpoint: Descriptive by trial

```r
df_sg_rs_mort28$inverse_variance <- 1 / df_sg_rs_mort28$standard_error^2 # Calculate the inverse variance
df_sg_rs_mort28$variable <- gsub("_firth$", "", df_sg_rs_mort28$variable) # remove firth
df_sg_rs_mort28 <- df_sg_rs_mort28[order(df_sg_rs_mort28$trial), ] # order to have the two forestplots for the deft plot in the same order

# add the proportions in each treatment group
df_sg_rs_mort28 <- df_sg_rs_mort28 %>%
  mutate(prop_int = paste0(round(n_intervention / n_intervention_tot * 100), "%")) %>% 
  mutate(prop_cont = paste0(round(n_control / n_control_tot * 100), "%"))

# Open a pdf file
# pdf("./fp_sg_rs.pdf", width=10, height=11)

mort28.rs <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = variable,
                      data = df_sg_rs_mort28,
                      n.e = n_intervention_tot,
                      n.c = n_control_tot,
                      sm = "OR",
                      fixed = F,
                      random = F,
                      prediction = F,
                      subgroup = trial,
                      method.tau = "ML",
                      method.random.ci = "HK",
                      )
summary(mort28.rs)
```

```
##                                    OR             95%-CI         trial
## No oxygen                      1.0920 [0.0416;  28.6849]        ACTT-2
## low-flow oxygen                0.4214 [0.1445;   1.2287]        ACTT-2
## high-flow oxygen / NIV         0.5655 [0.2103;   1.5205]        ACTT-2
## Mechanical ventilation / ECMO  1.3802 [0.5170;   3.6847]        ACTT-2
## high-flow oxygen / NIV         0.8860 [0.3733;   2.1028] Bari-SolidAct
## Mechanical ventilation / ECMO  0.2364 [0.0393;   1.4222] Bari-SolidAct
## No oxygen                      0.2604 [0.0282;   2.4077]   COV-BARRIER
## low-flow oxygen                0.6283 [0.3753;   1.0519]   COV-BARRIER
## high-flow oxygen / NIV         0.4574 [0.2672;   0.7831]   COV-BARRIER
## Mechanical ventilation / ECMO  0.4438 [0.1927;   1.0218]   COV-BARRIER
## No oxygen                      1.4517 [0.0464;  45.3756]       COVINIB
## low-flow oxygen                0.1827 [0.0103;   3.2412]       COVINIB
## low-flow oxygen                0.7909 [0.1655;   3.7805]     Ghazaeian
## No oxygen                      1.5003 [0.0675;  33.3510]      PANCOVID
## low-flow oxygen                0.1732 [0.0190;   1.5784]      PANCOVID
## high-flow oxygen / NIV         1.9100 [0.0144; 254.0245]      PANCOVID
## No oxygen                      0.7831 [0.3672;   1.6699]      RECOVERY
## low-flow oxygen                0.9179 [0.7542;   1.1172]      RECOVERY
## high-flow oxygen / NIV         0.6962 [0.5534;   0.8758]      RECOVERY
## Mechanical ventilation / ECMO  0.7467 [0.4191;   1.3304]      RECOVERY
## No oxygen                      1.0477 [0.0887;  12.3789]      RUXCOVID
## low-flow oxygen                1.6975 [0.3345;   8.6144]      RUXCOVID
## high-flow oxygen / NIV         0.4864 [0.0162;  14.6213]      RUXCOVID
## No oxygen                     19.3071 [0.5117; 728.5009]      TACTIC-R
## low-flow oxygen                0.7614 [0.1349;   4.2987]      TACTIC-R
## high-flow oxygen / NIV         0.7355 [0.2852;   1.8962]      TACTIC-R
## No oxygen                      1.3918 [0.0481;  40.2481]       TOFACOV
## low-flow oxygen                2.1913 [0.1262;  38.0562]       TOFACOV
## 
## Number of studies: k = 28
## Number of observations: o = 12071
## 
## Details on meta-analytical method:
## - Inverse variance method
```

```r
forest.meta(mort28.rs,
            rightcols = c("effect"),
            rightlabs = c("aOR"),
            leftcols = c("studlab", "n_intervention_tot", "n_intervention", "prop_int", "n_control_tot", "n_control", "prop_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "Prop.\nJAKi", "no JAKi", "Events\nno JAKi", "Prop.\nno JAKi"),
            text.random = "",
            # xlim = c(0.10,5),
            # label.left = "Favours JAKi",  
            # label.right = "Favours No JAKi",
            overall.hetstat = F,
            test.subgroup = F,
            overall = F,
            hetstat = F
            )
```

![](two_stage_files/figure-html/unnamed-chunk-44-1.png)<!-- -->

```r
# dev.off()
```
Discussion: Which trials label as "insufficient data"?



# Subgroups: Respiratory support (proxy for disease severity) on primary endpoint: Pooled across trials // Descriptive

```r
# No oxygen
rs.no.ox.mort28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_no_ox_mort28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, # the true subgroup effect is assumed the same in all trials
                      random = F)
summary(rs.no.ox.mort28)
```

```
##                  OR             95%-CI %W(common)
## ACTT-2       1.0920 [0.0416;  28.6849]        3.7
## COV-BARRIER  0.2604 [0.0282;   2.4077]        7.9
## COVINIB      1.4517 [0.0464;  45.3756]        3.3
## TOFACOV      1.3918 [0.0481;  40.2481]        3.5
## RECOVERY     0.7831 [0.3672;   1.6699]       68.2
## TACTIC-R    19.3071 [0.5117; 728.5009]        3.0
## RUXCOVID     1.0477 [0.0887;  12.3789]        6.4
## PANCOVID     1.5003 [0.0675;  33.3510]        4.1
## 
## Number of studies: k = 8
## Number of observations: o = 44
## 
##                         OR           95%-CI     z p-value
## Common effect model 0.8703 [0.4656; 1.6268] -0.44  0.6634
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 3.4413]; tau = 0 [0.0000; 1.8551]
##  I^2 = 0.0% [0.0%; 67.6%]; H = 1.00 [1.00; 1.76]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  4.32    7  0.7419
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
```

```r
forest.meta(rs.no.ox.mort28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-45-1.png)<!-- -->

```r
# low-flow oxygen
rs.low.ox.mort28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_low_ox_mort28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, 
                      random = F)
summary(rs.low.ox.mort28)
```

```
##                 OR            95%-CI %W(common)
## ACTT-2      0.4214 [0.1445;  1.2287]        2.7
## COV-BARRIER 0.6283 [0.3753;  1.0519]       11.7
## COVINIB     0.1827 [0.0103;  3.2412]        0.4
## TOFACOV     2.1913 [0.1262; 38.0562]        0.4
## Ghazaeian   0.7909 [0.1655;  3.7805]        1.3
## RECOVERY    0.9179 [0.7542;  1.1172]       80.7
## TACTIC-R    0.7614 [0.1349;  4.2987]        1.0
## RUXCOVID    1.6975 [0.3345;  8.6144]        1.2
## PANCOVID    0.1732 [0.0190;  1.5784]        0.6
## 
## Number of studies: k = 9
## Number of observations: o = 628
## 
##                         OR           95%-CI     z p-value
## Common effect model 0.8511 [0.7134; 1.0153] -1.79  0.0733
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0346 [0.0000; 1.4168]; tau = 0.1860 [0.0000; 1.1903]
##  I^2 = 0.0% [0.0%; 64.8%]; H = 1.00 [1.00; 1.69]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  7.79    8  0.4540
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
```

```r
forest.meta(rs.low.ox.mort28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-45-2.png)<!-- -->

```r
# NIV
rs.niv.mort28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_niv_mort28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, 
                      random = F)
summary(rs.niv.mort28)
```

```
##                   OR             95%-CI %W(common)
## ACTT-2        0.5655 [0.2103;   1.5205]        3.9
## COV-BARRIER   0.4574 [0.2672;   0.7831]       13.3
## Bari-SolidAct 0.8860 [0.3733;   2.1028]        5.1
## RECOVERY      0.6962 [0.5534;   0.8758]       72.9
## TACTIC-R      0.7355 [0.2852;   1.8962]        4.3
## RUXCOVID      0.4864 [0.0162;  14.6213]        0.3
## PANCOVID      1.9100 [0.0144; 254.0245]        0.2
## 
## Number of studies: k = 7
## Number of observations: o = 596
## 
##                         OR           95%-CI     z  p-value
## Common effect model 0.6630 [0.5451; 0.8065] -4.11 < 0.0001
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.1282]; tau = 0 [0.0000; 0.3580]
##  I^2 = 0.0% [0.0%; 70.8%]; H = 1.00 [1.00; 1.85]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  2.79    6  0.8342
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
```

```r
forest.meta(rs.niv.mort28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-45-3.png)<!-- -->

```r
# ECMO
rs.ecmo.mort28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_ecmo_mort28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, 
                      random = F)
summary(rs.ecmo.mort28)
```

```
##                   OR           95%-CI %W(common)
## ACTT-2        1.3802 [0.5170; 3.6847]       17.9
## COV-BARRIER   0.4438 [0.1927; 1.0218]       24.9
## Bari-SolidAct 0.2364 [0.0393; 1.4222]        5.4
## RECOVERY      0.7467 [0.4191; 1.3304]       51.8
## 
## Number of studies: k = 4
## Number of observations: o = 163
## 
##                         OR           95%-CI     z p-value
## Common effect model 0.6886 [0.4543; 1.0437] -1.76  0.0787
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0496 [0.0000; 7.3343]; tau = 0.2228 [0.0000; 2.7082]
##  I^2 = 32.3% [0.0%; 75.9%]; H = 1.22 [1.00; 2.04]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  4.43    3  0.2185
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
```

```r
forest.meta(rs.ecmo.mort28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-45-4.png)<!-- -->

# Interaction: Ventilation requirement (proxy for disease severity) on primary endpoint

```r
# str(df_vb_mort28)
## "TOFACOV", "COVINIB", "Ghazaeian" and "Bari-SolidAct" do only have 1 of 2 subgroups -> see deft: do not include! "(insufficient data)"
df_vb_mort28 <- df_vb_mort28[order(df_vb_mort28$trial), ]
vb.mort28 <- metagen(TE = log(log_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_vb_mort28,
                      sm = "OR",
                      fixed = F, 
                      random = T, 
                      method.tau = "ML", 
                      method.random.ci = "HK",
                      # subset = trial %in% c("COV-BARRIER", "ACTT-2", "Bari-SolidAct", "TOFACOV", "COVINIB", "RECOVERY", "TACTIC-R", "RUXCOVID", "PANCOVID"), #### ADD NEW TRIALS!
                      exclude = trial %in% c("Ghazaeian", "COVINIB", "TOFACOV", "Bari-SolidAct") # incl in plot but exclude from analysis
                      )
summary(vb.mort28)
```

```
##                   OR             95%-CI %W(random) exclude
## ACTT-2        2.1962 [0.6250;   7.7175]        4.1        
## Bari-SolidAct 0.7732 [0.6718;   0.8899]        0.0       *
## COV-BARRIER   0.7886 [0.4085;   1.5222]       14.8        
## PANCOVID      9.8019 [0.1404; 684.2802]        0.4        
## RECOVERY      0.7462 [0.5607;   0.9931]       78.4        
## RUXCOVID      0.1649 [0.0023;  11.5907]        0.4        
## TACTIC-R      0.6568 [0.1101;   3.9165]        2.0        
## 
## Number of studies: k = 6
## 
##                               OR           95%-CI     t p-value
## Random effects model (HK) 0.7870 [0.5722; 1.0824] -1.93  0.1112
## 
## Quantifying heterogeneity:
##  tau^2 < 0.0001 [0.0000; 7.2290]; tau = 0.0010 [0.0000; 2.6887]
##  I^2 = 0.0% [0.0%; 74.6%]; H = 1.00 [1.00; 1.99]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  4.61    5  0.4655
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 5)
```

```r
forest.meta(vb.mort28,
            hetstat = F,
            leftcols = c("studlab"),
            leftlabs = c("Trial"),
            # text.common = "Average interaction effect (common effect model)*",
            # text.random = "Average interaction effect (random effect model)*",
            text.random = "",
            xlab = "more resp support: greater effect < > less resp support: greater effect",
            xlab.pos = 0.0,
            fs.xlab = 11
            )
```

![](two_stage_files/figure-html/unnamed-chunk-46-1.png)<!-- -->

# Subgroups: Ventilation requirement (proxy for disease severity) on primary endpoint: Descriptive

```r
# Calculate the inverse variance
df_sg_vb_mort28$inverse_variance <- 1 / df_sg_vb_mort28$standard_error^2
```

# Subgroups: Ventilation requirement (proxy for disease severity) on primary endpoint: Pooled across trials // Descriptive

```r
# No ventilation
no.vent.mort28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_no_vent_mort28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, # the true subgroup effect is assumed the same in all trials
                      random = F)
summary(no.vent.mort28)
```

```
##                 OR            95%-CI %W(common)
## ACTT-2      0.3916 [0.1349;  1.1368]        2.5
## COV-BARRIER 0.5932 [0.3614;  0.9734]       11.8
## COVINIB     0.1706 [0.0094;  3.1016]        0.3
## TOFACOV     2.6388 [0.1509; 46.1597]        0.4
## Ghazaeian   0.8256 [0.1729;  3.9416]        1.2
## RECOVERY    0.9131 [0.7553;  1.1038]       80.2
## TACTIC-R    1.4550 [0.2842;  7.4487]        1.1
## RUXCOVID    1.4771 [0.3837;  5.6860]        1.6
## PANCOVID    0.2853 [0.0513;  1.5873]        1.0
## 
## Number of studies: k = 9
## Number of observations: o = 672
## 
##                         OR           95%-CI     z p-value
## Common effect model 0.8478 [0.7153; 1.0048] -1.90  0.0568
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0420 [0.0000; 1.4867]; tau = 0.2049 [0.0000; 1.2193]
##  I^2 = 11.1% [0.0%; 52.4%]; H = 1.06 [1.00; 1.45]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  9.00    8  0.3423
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
```

```r
forest.meta(no.vent.mort28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-48-1.png)<!-- -->

```r
# Ventilation
vent.mort28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_vent_mort28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, # the true subgroup effect is assumed the same in all trials
                      random = F)
summary(vent.mort28)
```

```
##                   OR             95%-CI %W(common)
## ACTT-2        0.8593 [0.4413;   1.6734]        6.8
## COV-BARRIER   0.4892 [0.3170;   0.7548]       16.1
## Bari-SolidAct 0.6429 [0.3022;   1.3676]        5.3
## RECOVERY      0.7052 [0.5708;   0.8712]       67.8
## TACTIC-R      0.8532 [0.3382;   2.1525]        3.5
## RUXCOVID      0.4864 [0.0162;  14.6213]        0.3
## PANCOVID      1.9100 [0.0144; 254.0245]        0.1
## 
## Number of studies: k = 7
## Number of observations: o = 759
## 
##                         OR           95%-CI     z  p-value
## Common effect model 0.6753 [0.5674; 0.8037] -4.42 < 0.0001
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.1222]; tau = 0 [0.0000; 0.3496]
##  I^2 = 0.0% [0.0%; 70.8%]; H = 1.00 [1.00; 1.85]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  3.26    6  0.7760
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
```

```r
forest.meta(vent.mort28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-48-2.png)<!-- -->

# Interaction: Age on primary endpoint

```r
# str(df_age_mort28)
df_age_mort28 <- df_age_mort28[order(df_age_mort28$trial), ]
age.mort28 <- metagen(TE = log(log_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_age_mort28,
                      sm = "OR",
                      fixed = F, 
                      random = T, 
                      method.tau = "ML", 
                      method.random.ci = "HK",
                      # subset = trial %in% c("COV-BARRIER", "ACTT-2"),
                      # exclude = trial %in% c("TOFACOV", "COVINIB", "Ghazaeian") # incl in plot but exclude from analysis
                      )
summary(age.mort28)
```

```
##                   OR           95%-CI %W(random)
## ACTT-2        0.9925 [0.9515; 1.0352]        5.5
## Bari-SolidAct 1.0266 [0.9535; 1.1054]        1.8
## COV-BARRIER   1.0241 [0.9960; 1.0529]       12.6
## COVINIB       0.8660 [0.7030; 1.0668]        0.2
## Ghazaeian     1.0505 [0.9466; 1.1657]        0.9
## PANCOVID      1.0308 [0.8044; 1.3208]        0.2
## RECOVERY      1.0080 [0.9967; 1.0195]       76.5
## RUXCOVID      1.0129 [0.8943; 1.1473]        0.6
## TACTIC-R      1.0545 [0.9700; 1.1463]        1.4
## TOFACOV       1.0471 [0.8652; 1.2672]        0.3
## 
## Number of studies: k = 10
## 
##                               OR           95%-CI    t p-value
## Random effects model (HK) 1.0103 [1.0012; 1.0196] 2.56  0.0307
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.0026]; tau = 0 [0.0000; 0.0510]
##  I^2 = 0.0% [0.0%; 62.4%]; H = 1.00 [1.00; 1.63]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  5.73    9  0.7661
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 9)
```

```r
forest.meta(age.mort28,
            hetstat = F,
            leftcols = c("studlab"),
            leftlabs = c("Trial"),
            # text.common = "Average interaction effect (common effect model)*",
            # text.random = "Average interaction effect (random effect model)*",
            text.random = "",
            xlab = "    older: greater effect < > younger: greater effect",
            xlab.pos = 0.0,
            fs.xlab = 11
            )
```

![](two_stage_files/figure-html/unnamed-chunk-49-1.png)<!-- -->

# Subgroups: Age on primary endpoint: Descriptive

```r
df_sg_age_mort28$inverse_variance <- 1 / df_sg_age_mort28$standard_error^2
df_sg_age_mort28$variable <- gsub("_firth$", "", df_sg_age_mort28$variable)
df_sg_age_mort28 <- df_sg_age_mort28[order(df_sg_age_mort28$trial), ]

# modify labels because for 4 trials (COVINIB, PANCOVID, RUXCOVID, TOFACOV) we used a different threshold.
df_sg_age_mort28 <- df_sg_age_mort28 %>% 
  mutate(trial = case_when(trial == "COVINIB" ~ "COVINIB*",
                           trial == "PANCOVID" ~ "PANCOVID*",
                           trial == "RUXCOVID" ~ "RUXCOVID*",
                           trial == "TOFACOV" ~ "TOFACOV*",
                           TRUE ~ trial),
         variable = case_when(trial == "COVINIB*" & variable == "70 years and above" ~ "65 years and above",
                              trial == "COVINIB*" & variable == "below 70 years" ~ "below 65 years",
                              trial == "PANCOVID*" & variable == "70 years and above" ~ "75 years and above",
                              trial == "PANCOVID*" & variable == "below 70 years" ~ "below 75 years",
                              trial == "RUXCOVID*" & variable == "70 years and above" ~ "65 years and above",
                              trial == "RUXCOVID*" & variable == "below 70 years" ~ "below 65 years",
                              trial == "TOFACOV*" & variable == "70 years and above" ~ "65 years and above",
                              trial == "TOFACOV*" & variable == "below 70 years" ~ "below 65 years",
                              TRUE ~ variable)
         )

# add the proportions in each treatment group
df_sg_age_mort28 <- df_sg_age_mort28 %>%
  mutate(prop_int = paste0(round(n_intervention / n_intervention_tot * 100), "%")) %>% 
  mutate(prop_cont = paste0(round(n_control / n_control_tot * 100), "%"))

# Open a pdf file
pdf("./fp_sg_age.pdf", width=10, height=10)

mort28.age <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = variable,
                      data = df_sg_age_mort28,
                      n.e = n_intervention_tot,
                      n.c = n_control_tot,
                      sm = "OR",
                      fixed = F,
                      random = F,
                      prediction = F,
                      subgroup = trial,
                      method.tau = "ML",
                      method.random.ci = "HK",
                      )
summary(mort28.age)
```

```
##                        OR             95%-CI         trial
## 70 years and above 0.6904 [0.2639;   1.8060]        ACTT-2
## below 70 years     0.6762 [0.3346;   1.3666]        ACTT-2
## 70 years and above 0.9118 [0.3189;   2.6069] Bari-SolidAct
## below 70 years     0.6336 [0.2161;   1.8583] Bari-SolidAct
## 70 years and above 0.6487 [0.3824;   1.1005]   COV-BARRIER
## below 70 years     0.4610 [0.3005;   0.7073]   COV-BARRIER
## 65 years and above 0.2402 [0.0120;   4.7986]      COVINIB*
## below 65 years     0.3451 [0.0189;   6.3182]      COVINIB*
## 70 years and above 3.5455 [0.1188; 105.8203]     Ghazaeian
## below 70 years     0.5700 [0.1141;   2.8473]     Ghazaeian
## 75 years and above 0.7412 [0.0932;   5.8924]     PANCOVID*
## below 75 years     0.1337 [0.0091;   1.9595]     PANCOVID*
## 70 years and above 0.9414 [0.7692;   1.1521]      RECOVERY
## below 70 years     0.7614 [0.6289;   0.9218]      RECOVERY
## 65 years and above 1.7550 [0.3449;   8.9311]     RUXCOVID*
## below 65 years     1.0436 [0.0932;  11.6822]     RUXCOVID*
## 70 years and above 1.1695 [0.3822;   3.5787]      TACTIC-R
## below 70 years     0.7340 [0.2362;   2.2812]      TACTIC-R
## 65 years and above 2.9152 [0.1566;  54.2581]      TOFACOV*
## below 65 years     1.1153 [0.0426;  29.2057]      TOFACOV*
## 
## Number of studies: k = 20
## Number of observations: o = 12075
## 
## Details on meta-analytical method:
## - Inverse variance method
```

```r
forest.meta(mort28.age,
            rightcols = c("effect"),
            rightlabs = c("aOR"),
            leftcols = c("studlab", "n_intervention_tot", "n_intervention", "prop_int", "n_control_tot", "n_control", "prop_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "Prop.\nJAKi", "no JAKi", "Events\nno JAKi", "Prop.\nno JAKi"),
            text.random = "",
            # xlim = c(0.10,5),
            # label.left = "Favours JAKi",  
            # label.right = "Favours No JAKi",
            overall.hetstat = F,
            test.subgroup = F,
            overall = F,
            hetstat = F
            )

dev.off()
```

```
## quartz_off_screen 
##                 2
```

# Subgroups: Age on primary endpoint: Pooled across trials // only descriptive purpose for overall forestplot!

```r
# Age 70 years or older
age.above70.mort28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_old_mort28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, # the true subgroup effect is assumed the same in all trials
                      random = F)
summary(age.above70.mort28)
```

```
##                   OR             95%-CI %W(common)
## ACTT-2        0.6904 [0.2639;   1.8060]        3.4
## COV-BARRIER   0.6487 [0.3824;   1.1005]       11.3
## Bari-SolidAct 0.9118 [0.3189;   2.6069]        2.9
## COVINIB       0.2402 [0.0120;   4.7986]        0.4
## TOFACOV       2.9152 [0.1566;  54.2581]        0.4
## Ghazaeian     3.5455 [0.1188; 105.8203]        0.3
## RECOVERY      0.9414 [0.7692;   1.1521]       77.1
## TACTIC-R      1.1695 [0.3822;   3.5787]        2.5
## RUXCOVID      1.7550 [0.3449;   8.9311]        1.2
## PANCOVID      0.7412 [0.0932;   5.8924]        0.7
## 
## Number of studies: k = 10
## Number of observations: o = 742
## 
##                         OR           95%-CI     z p-value
## Common effect model 0.9051 [0.7580; 1.0807] -1.10  0.2704
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.3397]; tau = 0 [0.0000; 0.5829]
##  I^2 = 0.0% [0.0%; 62.4%]; H = 1.00 [1.00; 1.63]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  4.84    9  0.8481
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
```

```r
forest.meta(age.above70.mort28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-51-1.png)<!-- -->

```r
# Age below 70 years
age.below70.mort28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_young_mort28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, # the true subgroup effect is assumed the same in all trials
                      random = F,
                      exclude = trial %in% c("RUXCOVID", "COVINIB")) # incl in plot but exclude from analysis)
summary(age.below70.mort28)
```

```
##                   OR            95%-CI %W(common) exclude
## ACTT-2        0.6762 [0.3346;  1.3666]        5.4        
## COV-BARRIER   0.4610 [0.3005;  0.7073]       14.7        
## Bari-SolidAct 0.6336 [0.2161;  1.8583]        2.3        
## COVINIB       0.3451 [0.0189;  6.3182]        0.0       *
## TOFACOV       1.1153 [0.0426; 29.2057]        0.3        
## Ghazaeian     0.5700 [0.1141;  2.8473]        1.0        
## RECOVERY      0.7614 [0.6289;  0.9218]       73.7        
## TACTIC-R      0.7340 [0.2362;  2.2812]        2.1        
## RUXCOVID      1.0436 [0.0932; 11.6822]        0.0       *
## PANCOVID      0.1337 [0.0091;  1.9595]        0.4        
## 
## Number of studies: k = 8
## Number of observations: o = 689
## 
##                         OR           95%-CI     z  p-value
## Common effect model 0.6931 [0.5882; 0.8168] -4.38 < 0.0001
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0323 [0.0000; 0.2779]; tau = 0.1799 [0.0000; 0.5272]
##  I^2 = 0.0% [0.0%; 67.6%]; H = 1.00 [1.00; 1.76]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  6.04    7  0.5354
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
```

```r
forest.meta(age.below70.mort28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-51-2.png)<!-- -->

# Interaction: Comorbidity on primary endpoint

```r
# str(df_comorb_mort28)
df_comorb_mort28 <- df_comorb_mort28[order(df_comorb_mort28$trial), ]
comorb.mort28 <- metagen(TE = log(log_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_comorb_mort28,
                      sm = "OR",
                      fixed = F, 
                      random = T, 
                      method.tau = "ML", 
                      method.random.ci = "HK",
                      # subset = trial %in% c("COV-BARRIER", "ACTT-2"),
                      # exclude = trial %in% c("TOFACOV", "COVINIB", "Ghazaeian") # incl in plot but exclude from analysis
                      )
summary(comorb.mort28)
```

```
##                   OR            95%-CI %W(random)
## ACTT-2        0.8088 [0.3396;  1.9262]        3.4
## Bari-SolidAct 1.0985 [0.4283;  2.8175]        2.9
## COV-BARRIER   0.8492 [0.5329;  1.3530]       11.9
## COVINIB       1.5083 [0.1261; 18.0458]        0.4
## Ghazaeian     1.9139 [0.2974; 12.3188]        0.7
## PANCOVID      0.3798 [0.0349;  4.1372]        0.5
## RECOVERY      1.3073 [1.0883;  1.5705]       76.7
## RUXCOVID      0.5421 [0.0629;  4.6724]        0.6
## TACTIC-R      1.2511 [0.4605;  3.3990]        2.6
## TOFACOV       0.6061 [0.0405;  9.0812]        0.4
## 
## Number of studies: k = 10
## 
##                               OR           95%-CI    t p-value
## Random effects model (HK) 1.2024 [1.0372; 1.3939] 2.82  0.0200
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.0912]; tau = 0 [0.0000; 0.3020]
##  I^2 = 0.0% [0.0%; 62.4%]; H = 1.00 [1.00; 1.63]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  5.72    9  0.7673
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 9)
```

```r
forest.meta(comorb.mort28,
            hetstat = F,
            leftcols = c("studlab"),
            leftlabs = c("Trial"),
            # text.common = "Average interaction effect (common effect model)*",
            # text.random = "Average interaction effect (random effect model)*",
            text.random = "",
            title = "Treatment-covariate interaction on primary endpoint: Comorbidity",
            xlab = "more comorbidity: greater effect <-> less comorbidity: greater effect",
            xlab.pos = 0.0,
            fs.xlab = 11
)
```

![](two_stage_files/figure-html/unnamed-chunk-52-1.png)<!-- -->

```r
### SENS
# str(df_comorb_count_mort28)
comorb.count.mort28 <- metagen(TE = log(log_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_comorb_count_mort28,
                      sm = "OR",
                      fixed = F, # the true interaction is assumed the same in all trials 
                      random = T, # the true interactions are assumed random across trials
                      # prediction = T,
                      method.tau = "ML", # same results with REML (-> see one-stage!)
                      method.random.ci = "HK", # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      # subset = trial %in% c("COV-BARRIER", "ACTT-2"),
                      # exclude = trial %in% c("TOFACOV", "COVINIB", "Ghazaeian") # incl in plot but exclude from analysis                               
                      )
summary(comorb.count.mort28)
```

```
##                   OR           95%-CI %W(random)
## Bari-SolidAct 1.2400 [0.6995; 2.1984]        4.3
## ACTT-2        0.8221 [0.5504; 1.2280]        8.7
## Ghazaeian     1.6528 [0.4362; 6.2631]        0.8
## TOFACOV       0.5793 [0.0595; 5.6384]        0.3
## COVINIB       1.4734 [0.3852; 5.6354]        0.8
## COV-BARRIER   1.0129 [0.7835; 1.3094]       21.2
## RECOVERY      1.1786 [1.0083; 1.3776]       57.3
## TACTIC-R      1.0288 [0.5988; 1.7675]        4.8
## RUXCOVID      0.7865 [0.3228; 1.9161]        1.8
## PANCOVID      0.4324 [0.0459; 4.0740]        0.3
## 
## Number of studies: k = 10
## 
##                               OR           95%-CI    t p-value
## Random effects model (HK) 1.0934 [0.9834; 1.2157] 1.91  0.0891
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.0630]; tau = 0 [0.0000; 0.2510]
##  I^2 = 0.0% [0.0%; 62.4%]; H = 1.00 [1.00; 1.63]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  5.44    9  0.7940
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 9)
```

```r
forest.meta(comorb.count.mort28,
            hetstat = F,
            leftcols = c("studlab"),
            leftlabs = c("Trial"),
            # text.common = "Average interaction effect (common effect model)*",
            # text.random = "Average interaction effect (random effect model)*",
            text.random = "",
            title = "Treatment-covariate interaction on primary endpoint: Comorbidity",
            xlab = "more comorbidity: greater effect <-> less comorbidity: greater effect",
            xlab.pos = 0.0,
            fs.xlab = 11
)
```

![](two_stage_files/figure-html/unnamed-chunk-52-2.png)<!-- -->

```r
# str(df_comorb_any_mort28)
comorb.any.mort28 <- metagen(TE = log(log_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_comorb_any_mort28,
                      sm = "OR",
                      fixed = F, # the true interaction is assumed the same in all trials 
                      random = T, # the true interactions are assumed random across trials
                      method.tau = "ML", # same results with ML (-> see one-stage!)
                      method.random.ci = "HK",
                      )
summary(comorb.any.mort28)
```

```
##                   OR             95%-CI %W(random)
## Bari-SolidAct 0.8066 [0.0880;   7.3914]        1.6
## ACTT-2        0.2910 [0.0224;   3.7796]        1.2
## Ghazaeian     1.2376 [0.0478;  32.0479]        0.7
## TOFACOV       2.0317 [0.0207; 198.9537]        0.4
## COVINIB       2.6115 [0.0297; 229.9008]        0.4
## COV-BARRIER   0.8300 [0.2944;   2.3402]        7.3
## RECOVERY      1.5903 [1.1757;   2.1510]       86.1
## TACTIC-R      1.6654 [0.1042;  26.6272]        1.0
## RUXCOVID      1.1338 [0.0378;  34.0001]        0.7
## PANCOVID      0.3750 [0.0097;  14.4637]        0.6
## 
## Number of studies: k = 10
## 
##                               OR           95%-CI    t p-value
## Random effects model (HK) 1.4563 [1.1770; 1.8018] 3.99  0.0031
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.1184]; tau = 0 [0.0000; 0.3441]
##  I^2 = 0.0% [0.0%; 62.4%]; H = 1.00 [1.00; 1.63]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  3.90    9  0.9179
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 9)
```

```r
forest.meta(comorb.any.mort28,
            hetstat = F,
            leftcols = c("studlab"),
            leftlabs = c("Trial"),
            # text.common = "Average interaction effect (common effect model)*",
            # text.random = "Average interaction effect (random effect model)*",
            text.random = "",
            title = "Treatment-covariate interaction on primary endpoint: Comorbidity",
            xlab = "any comorbidity: greater effect <-> no comorbidity: greater effect",
            xlab.pos = 0.0,
            fs.xlab = 11
)
```

![](two_stage_files/figure-html/unnamed-chunk-52-3.png)<!-- -->

```r
# str(df_comorb_noimmuno_mort28)
comorb.noimmuno.mort28 <- metagen(TE = log(log_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_comorb_noimmuno_mort28,
                      sm = "OR",
                      fixed = F, # the true interaction is assumed the same in all trials 
                      random = T, # the true interactions are assumed random across trials
                      method.tau = "ML", # same results with ML (-> see one-stage!)
                      method.random.ci = "HK",
                      )
summary(comorb.noimmuno.mort28)
```

```
##                   OR            95%-CI %W(random)
## Bari-SolidAct 1.1602 [0.3972;  3.3893]        1.8
## ACTT-2        0.5511 [0.2144;  1.4165]        2.3
## Ghazaeian     0.7627 [0.1044;  5.5703]        0.5
## TOFACOV       0.6061 [0.0405;  9.0812]        0.3
## COVINIB       1.4490 [0.1161; 18.0904]        0.3
## COV-BARRIER   0.8070 [0.4999;  1.3028]        8.9
## RECOVERY      1.1786 [1.0083;  1.3776]       83.8
## TACTIC-R      0.8726 [0.2536;  3.0018]        1.3
## RUXCOVID      0.5421 [0.0629;  4.6724]        0.4
## PANCOVID      0.3798 [0.0349;  4.1372]        0.4
## 
## Number of studies: k = 10
## 
##                               OR           95%-CI    t p-value
## Random effects model (HK) 1.1030 [0.9630; 1.2633] 1.63  0.1367
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.0859]; tau = 0 [0.0000; 0.2931]
##  I^2 = 0.0% [0.0%; 62.4%]; H = 1.00 [1.00; 1.63]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  6.10    9  0.7300
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 9)
```

```r
forest.meta(comorb.noimmuno.mort28,
            hetstat = F,
            leftcols = c("studlab"),
            leftlabs = c("Trial"),
            # text.common = "Average interaction effect (common effect model)*",
            # text.random = "Average interaction effect (random effect model)*",
            text.random = "",
            title = "Treatment-covariate interaction on primary endpoint: Comorbidity",
            xlab = "any comorbidity: greater effect <-> no comorbidity: greater effect",
            xlab.pos = 0.0,
            fs.xlab = 11
)
```

![](two_stage_files/figure-html/unnamed-chunk-52-4.png)<!-- -->

# Subgroups: Comorbidity on primary endpoint: Descriptive

```r
df_sg_comorb_mort28$inverse_variance <- 1 / df_sg_comorb_mort28$standard_error^2
df_sg_comorb_mort28$variable <- gsub("_firth$", "", df_sg_comorb_mort28$variable)
df_sg_comorb_mort28 <- df_sg_comorb_mort28[order(df_sg_comorb_mort28$trial), ]

# add the proportions in each treatment group
df_sg_comorb_mort28 <- df_sg_comorb_mort28 %>%
  mutate(prop_int = paste0(round(n_intervention / n_intervention_tot * 100), "%")) %>% 
  mutate(prop_cont = paste0(round(n_control / n_control_tot * 100), "%"))

# Open a pdf file
pdf("./fp_sg_comorb.pdf", width=10, height=13)

mort28.comorb <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = variable,
                      data = df_sg_comorb_mort28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = F,
                      random = F,
                      prediction = F,
                      subgroup = trial,
                      method.tau = "ML",
                      method.random.ci = "HK",
                      )
summary(mort28.comorb)
```

```
##                            OR             95%-CI         trial
## No comorbidity         1.2952 [0.0855;  19.6131]        ACTT-2
## One comorbidity        0.7249 [0.2337;   2.2484]        ACTT-2
## Multiple comorbidities 0.4953 [0.2382;   1.0303]        ACTT-2
## Immunocompromised      1.9815 [0.1023;  38.3933]        ACTT-2
## No comorbidity         0.7929 [0.0850;   7.4008] Bari-SolidAct
## One comorbidity        0.7583 [0.1125;   5.1119] Bari-SolidAct
## Multiple comorbidities 0.7853 [0.2708;   2.2772] Bari-SolidAct
## Immunocompromised      0.7265 [0.0470;  11.2330] Bari-SolidAct
## No comorbidity         0.5583 [0.2058;   1.5143]   COV-BARRIER
## One comorbidity        0.6398 [0.3270;   1.2517]   COV-BARRIER
## Multiple comorbidities 0.4352 [0.2829;   0.6696]   COV-BARRIER
## Immunocompromised      1.2579 [0.0447;  35.3617]   COV-BARRIER
## No comorbidity         0.0726 [0.0019;   2.8390]       COVINIB
## One comorbidity        0.8352 [0.0443;  15.7418]       COVINIB
## Multiple comorbidities 0.3561 [0.0220;   5.7711]       COVINIB
## Immunocompromised      1.0000 [0.0108;  92.4216]       COVINIB
## No comorbidity         0.6585 [0.0545;   7.9515]     Ghazaeian
## One comorbidity        1.2592 [0.0451;  35.1269]     Ghazaeian
## Multiple comorbidities 0.3298 [0.0217;   5.0076]     Ghazaeian
## Immunocompromised      9.0000 [0.0974; 831.7956]     Ghazaeian
## No comorbidity         0.1569 [0.0038;   6.4039]      PANCOVID
## One comorbidity        0.3019 [0.0395;   2.3070]      PANCOVID
## Multiple comorbidities 0.1874 [0.0121;   2.9133]      PANCOVID
## No comorbidity         0.5901 [0.4605;   0.7561]      RECOVERY
## One comorbidity        0.9027 [0.7102;   1.1474]      RECOVERY
## Multiple comorbidities 1.0041 [0.7729;   1.3044]      RECOVERY
## No comorbidity         1.0973 [0.0576;  20.8949]      RUXCOVID
## One comorbidity        2.4121 [0.2328;  24.9889]      RUXCOVID
## Multiple comorbidities 0.9901 [0.1623;   6.0405]      RUXCOVID
## No comorbidity         0.4901 [0.0327;   7.3446]      TACTIC-R
## One comorbidity        1.4596 [0.2164;   9.8430]      TACTIC-R
## Multiple comorbidities 0.5951 [0.1891;   1.8724]      TACTIC-R
## Immunocompromised      4.1181 [0.2585;  65.6097]      TACTIC-R
## No comorbidity         1.1018 [0.0585;  20.7441]       TOFACOV
## One comorbidity        2.6426 [0.1674;  41.7124]       TOFACOV
## Multiple comorbidities 0.3895 [0.0140;  10.8361]       TOFACOV
## 
## Number of studies: k = 36
## Number of observations: o = 1431
## 
## Details on meta-analytical method:
## - Inverse variance method
```

```r
forest.meta(mort28.comorb,
            rightcols = c("effect"),
            rightlabs = c("aOR"),
            leftcols = c("studlab", "n_intervention_tot", "n_intervention", "prop_int", "n_control_tot", "n_control", "prop_cont"),
            leftlabs = c("Trial", "JAKi", "Events\nJAKi", "Prop.\nJAKi", "no JAKi", "Events\nno JAKi", "Prop.\nno JAKi"),
            text.random = "",
            # xlim = c(0.10,5),
            # label.left = "Favours JAKi",  
            # label.right = "Favours No JAKi",
            overall.hetstat = F,
            test.subgroup = F,
            overall = F,
            hetstat = F
            )

dev.off()
```

```
## quartz_off_screen 
##                 2
```

# Subgroups: Comorbidity on primary endpoint: Pooled across trials (only for overall forestplot; descriptive)

```r
# No comorbidity
no.comorb.mort28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_no_comorb_mort28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, # the true subgroup effect is assumed the same in all trials
                      random = F)
summary(no.comorb.mort28)
```

```
##                   OR            95%-CI %W(common)
## ACTT-2        1.2952 [0.0855; 19.6131]        0.7
## COV-BARRIER   0.5583 [0.2058;  1.5143]        5.5
## Bari-SolidAct 0.7929 [0.0850;  7.4008]        1.1
## COVINIB       0.0726 [0.0019;  2.8390]        0.4
## TOFACOV       1.1018 [0.0585; 20.7441]        0.6
## Ghazaeian     0.6585 [0.0545;  7.9515]        0.9
## RECOVERY      0.5901 [0.4605;  0.7561]       89.0
## TACTIC-R      0.4901 [0.0327;  7.3446]        0.7
## RUXCOVID      1.0973 [0.0576; 20.8949]        0.6
## PANCOVID      0.1569 [0.0038;  6.4039]        0.4
## 
## Number of studies: k = 10
## Number of observations: o = 368
## 
##                         OR           95%-CI     z  p-value
## Common effect model 0.5899 [0.4669; 0.7453] -4.42 < 0.0001
## 
## Quantifying heterogeneity:
##  tau^2 = 0; tau = 0; I^2 = 0.0% [0.0%; 62.4%]; H = 1.00 [1.00; 1.63]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  2.51    9  0.9805
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
```

```r
forest.meta(no.comorb.mort28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-54-1.png)<!-- -->

```r
# One comorbidity
one.comorb.mort28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_one_comorb_mort28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, 
                      random = F)
summary(one.comorb.mort28)
```

```
##                   OR            95%-CI %W(common)
## ACTT-2        0.7249 [0.2337;  2.2484]        3.6
## COV-BARRIER   0.6398 [0.3270;  1.2517]       10.2
## Bari-SolidAct 0.7583 [0.1125;  5.1119]        1.3
## COVINIB       0.8352 [0.0443; 15.7418]        0.5
## TOFACOV       2.6426 [0.1674; 41.7124]        0.6
## Ghazaeian     1.2592 [0.0451; 35.1269]        0.4
## RECOVERY      0.9027 [0.7102;  1.1474]       80.1
## TACTIC-R      1.4596 [0.2164;  9.8430]        1.3
## RUXCOVID      2.4121 [0.2328; 24.9889]        0.8
## PANCOVID      0.3019 [0.0395;  2.3070]        1.1
## 
## Number of studies: k = 10
## Number of observations: o = 463
## 
##                         OR           95%-CI     z p-value
## Common effect model 0.8710 [0.7027; 1.0796] -1.26  0.2075
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.2134]; tau = 0 [0.0000; 0.4620]
##  I^2 = 0.0% [0.0%; 62.4%]; H = 1.00 [1.00; 1.63]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  3.74    9  0.9276
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
```

```r
forest.meta(one.comorb.mort28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-54-2.png)<!-- -->

```r
# Multiple comorbidity
mult.comorb.mort28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_mult_comorb_mort28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, 
                      random = F,
                      exclude = trial %in% c("TOFACOV", "COVINIB")
                      )
summary(mult.comorb.mort28)
```

```
##                   OR            95%-CI %W(common) exclude
## ACTT-2        0.4953 [0.2382;  1.0303]        7.7        
## COV-BARRIER   0.4352 [0.2829;  0.6696]       22.4        
## Bari-SolidAct 0.7853 [0.2708;  2.2772]        3.7        
## COVINIB       0.3561 [0.0220;  5.7711]        0.0       *
## TOFACOV       0.3895 [0.0140; 10.8361]        0.0       *
## Ghazaeian     0.3298 [0.0217;  5.0076]        0.6        
## RECOVERY      1.0041 [0.7729;  1.3044]       60.7        
## TACTIC-R      0.5951 [0.1891;  1.8724]        3.2        
## RUXCOVID      0.9901 [0.1623;  6.0405]        1.3        
## PANCOVID      0.1874 [0.0121;  2.9133]        0.6        
## 
## Number of studies: k = 8
## Number of observations: o = 584
## 
##                         OR           95%-CI     z p-value
## Common effect model 0.7565 [0.6171; 0.9276] -2.68  0.0073
## 
## Quantifying heterogeneity:
##  tau^2 = 0.1144 [0.0000; 0.5221]; tau = 0.3383 [0.0000; 0.7226]
##  I^2 = 49.0% [0.0%; 77.3%]; H = 1.40 [1.00; 2.10]
## 
## Test of heterogeneity:
##      Q d.f. p-value
##  13.72    7  0.0564
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
```

```r
forest.meta(mult.comorb.mort28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-54-3.png)<!-- -->

```r
# Immunosuppressed
immun.comorb.mort28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_immun_mort28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, 
                      random = F,
                      )
summary(immun.comorb.mort28)
```

```
##                   OR             95%-CI %W(common)
## ACTT-2        1.9815 [0.1023;  38.3933]       20.1
## COV-BARRIER   1.2579 [0.0447;  35.3617]       15.9
## Bari-SolidAct 0.7265 [0.0470;  11.2330]       23.6
## COVINIB       1.0000 [0.0108;  92.4216]        8.6
## Ghazaeian     9.0000 [0.0974; 831.7956]        8.6
## TACTIC-R      4.1181 [0.2585;  65.6097]       23.1
## 
## Number of studies: k = 6
## Number of observations: o = 16
## 
##                         OR           95%-CI    z p-value
## Common effect model 1.8504 [0.4893; 6.9980] 0.91  0.3646
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 1.8374]; tau = 0 [0.0000; 1.3555]
##  I^2 = 0.0% [0.0%; 74.6%]; H = 1.00 [1.00; 1.99]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  1.36    5  0.9284
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
```

```r
forest.meta(immun.comorb.mort28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-54-4.png)<!-- -->

# Interaction: Comedication on primary endpoint

```r
# str(df_comed_mort28)
## "Ghazaeian" do only have 1 subgroup -> see deft: do not include! "(insufficient data)"
comed.mort28 <- metagen(TE = log(log_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_comed_mort28,
                      # n.e = n_int + n_cont,
                      # n.c = n_control,
                      sm = "OR",
                      fixed = F, # the true interaction is assumed the same in all trials 
                      random = T, # the true interactions are assumed random across trials
                      method.tau = "ML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "ci", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Treatment-covariate interaction on primary endpoint: Comedication",
                      subset = trial %in% c("COV-BARRIER", "Bari-SolidAct", "ACTT-2", "TOFACOV", "COVINIB", "RECOVERY", "TACTIC-R", "RUXCOVID", "PANCOVID"), #### ADD NEW TRIALS!
                      # exclude = trial %in% c("Ghazaeian") # incl in plot but exclude from analysis
                      )
summary(comed.mort28)
```

```
## Review:     Treatment-covariate interaction on primary endpoint: Comedication
## 
##                   OR             95%-CI %W(random)
## Bari-SolidAct 2.7733 [0.0820;  93.8371]        0.5
## ACTT-2        2.1775 [0.2280;  20.7991]        1.2
## TOFACOV       3.6145 [0.0310; 421.6596]        0.3
## COVINIB       1.8595 [0.0157; 220.6907]        0.3
## COV-BARRIER   1.4282 [0.5441;   3.7491]        6.3
## RECOVERY      0.9440 [0.7296;   1.2214]       88.8
## TACTIC-R      0.3978 [0.0544;   2.9093]        1.5
## RUXCOVID      1.1750 [0.0699;  19.7538]        0.7
## PANCOVID      0.6602 [0.0176;  24.7554]        0.4
## 
## Number of studies: k = 9
## 
##                                  OR           95%-CI     t p-value
## Random effects model (HK-CI) 0.9761 [0.7656; 1.2444] -0.20  0.8450
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.1209]; tau = 0 [0.0000; 0.3477]
##  I^2 = 0.0% [0.0%; 64.8%]; H = 1.00 [1.00; 1.69]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  2.69    8  0.9523
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = )
```

```r
forest.meta(comed.mort28,
            hetstat = F,
            leftcols = c("studlab"),
            leftlabs = c("Trial"),
            # text.common = "Average interaction effect (common effect model)*",
            # text.random = "Average interaction effect (random effect model)*",
            text.random = "",
            title = "Treatment-covariate interaction on primary endpoint: Comorbidity",
            xlab = "more comedication: greater effect <-> less comedication: greater effect",
            xlab.pos = 0.0,
            fs.xlab = 11
)
```

![](two_stage_files/figure-html/unnamed-chunk-55-1.png)<!-- -->

# Subgroups: Comedication on primary endpoint: Descriptive

```r
# Calculate the inverse variance
df_sg_comed_mort28$inverse_variance <- 1 / df_sg_comed_mort28$standard_error^2
```

# Subgroups: Comedication on primary endpoint: Pooled across trials // Descriptive

```r
# No comedication
no.comed.mort28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_no_comed_mort28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, # the true subgroup effect is assumed the same in all trials
                      random = F)
summary(no.comed.mort28)
```

```
##                   OR            95%-CI %W(common)
## ACTT-2        0.6654 [0.3696;  1.1980]       43.1
## COV-BARRIER   0.3813 [0.1530;  0.9503]       17.9
## Bari-SolidAct 0.3455 [0.0092; 12.9587]        1.1
## COVINIB       0.2037 [0.0142;  2.9320]        2.1
## TOFACOV       0.7356 [0.0160; 33.9204]        1.0
## RECOVERY      0.7070 [0.3288;  1.5201]       25.4
## TACTIC-R      1.4950 [0.2268;  9.8560]        4.2
## RUXCOVID      1.4644 [0.2691;  7.9679]        5.2
## 
## Number of studies: k = 8
## Number of observations: o = 140
## 
##                         OR           95%-CI     z p-value
## Common effect model 0.6390 [0.4344; 0.9401] -2.27  0.0230
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.8321]; tau = 0 [0.0000; 0.9122]
##  I^2 = 0.0% [0.0%; 67.6%]; H = 1.00 [1.00; 1.76]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  3.84    7  0.7985
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
```

```r
forest.meta(no.comed.mort28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-57-1.png)<!-- -->

```r
# Dexa, but no Toci
dexa.comed.mort28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_dexa_mort28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, 
                      random = F)
summary(dexa.comed.mort28)
```

```
##                   OR            95%-CI %W(common)
## ACTT-2        2.9175 [0.2253; 37.7799]        0.4
## COV-BARRIER   0.5379 [0.3736;  0.7745]       18.2
## Bari-SolidAct 0.6923 [0.3177;  1.5086]        4.0
## COVINIB       0.6005 [0.0158; 22.8582]        0.2
## TOFACOV       2.5608 [0.1937; 33.8592]        0.4
## Ghazaeian     0.7909 [0.1654;  3.7807]        1.0
## RECOVERY      0.8684 [0.7233;  1.0425]       72.4
## TACTIC-R      0.8411 [0.3149;  2.2462]        2.5
## RUXCOVID      1.7042 [0.1750; 16.5979]        0.5
## PANCOVID      0.5974 [0.0769;  4.6422]        0.6
## 
## Number of studies: k = 10
## Number of observations: o = 891
## 
##                         OR           95%-CI     z p-value
## Common effect model 0.7943 [0.6799; 0.9279] -2.90  0.0037
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0359 [0.0000; 0.2256]; tau = 0.1896 [0.0000; 0.4750]
##  I^2 = 0.0% [0.0%; 62.4%]; H = 1.00 [1.00; 1.63]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  7.75    9  0.5596
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
```

```r
forest.meta(dexa.comed.mort28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-57-2.png)<!-- -->

```r
# Dexa and Toci
dexa.toci.comed.mort28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_dexa_toci_mort28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, 
                      random = F)
summary(dexa.toci.comed.mort28)
```

```
##              OR            95%-CI %W(common)
## RECOVERY 0.6961 [0.5461;  0.8873]       99.1
## TACTIC-R 0.2649 [0.0059; 11.8648]        0.4
## PANCOVID 0.5405 [0.0204; 14.3223]        0.5
## 
## Number of studies: k = 3
## Number of observations: o = 388
## 
##                         OR           95%-CI     z p-value
## Common effect model 0.6924 [0.5439; 0.8816] -2.98  0.0029
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 7.5573]; tau = 0 [0.0000; 2.7491]
##  I^2 = 0.0% [0.0%; 89.6%]; H = 1.00 [1.00; 3.10]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  0.27    2  0.8741
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
```

```r
forest.meta(dexa.toci.comed.mort28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-57-3.png)<!-- -->

```r
# No dexa, but toci
toci.comed.mort28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_toci_mort28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, 
                      random = F)
summary(toci.comed.mort28)
```

```
## Number of observations: o = 12
## 
##              OR            95%-CI    z p-value
## RECOVERY 2.4876 [0.4883; 12.6737] 1.10  0.2726
```

```r
forest.meta(toci.comed.mort28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-57-4.png)<!-- -->

# Interaction: Vaccination on AEs

```r
# str(df_vacc_ae28)
vacc.ae28 <- metagen(TE = log(log_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_vacc_ae28,
                      # n.e = n_int + n_cont,
                      # n.c = n_control,
                      sm = "OR",
                      fixed = F, # the true interaction is assumed the same in all trials 
                      random = T, # the true interactions are assumed random across trials
                      method.tau = "ML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "ci", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Treatment-covariate interaction on AEs: vaccination",
                      # subset = trial %in% c("COV-BARRIER", "Bari-SolidAct", "ACTT-2", "TOFACOV", "COVINIB"),
                      exclude = trial %in% c("Ghazaeian"))
summary(vacc.ae28)
```

```
## Review:     Treatment-covariate interaction on AEs: vaccination
## 
##                   OR             95%-CI %W(random) exclude
## Bari-SolidAct 1.4026 [0.4400;   4.4709]        9.7        
## TOFACOV       2.6499 [0.0261; 269.5443]        0.6        
## COVINIB       1.0000 [0.0091; 110.2786]        0.6        
## RECOVERY      0.9484 [0.6420;   1.4009]       86.0        
## PANCOVID      1.3277 [0.1657;  10.6407]        3.0        
## 
## Number of studies: k = 5
## 
##                                  OR           95%-CI    t p-value
## Random effects model (HK-CI) 1.0019 [0.6977; 1.4387] 0.01  0.9918
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.0749]; tau = 0 [0.0000; 0.2737]
##  I^2 = 0.0% [0.0%; 79.2%]; H = 1.00 [1.00; 2.19]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  0.64    4  0.9585
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = )
```

```r
forest.meta(vacc.ae28,
            hetstat = F,
            leftcols = c("studlab"),
            leftlabs = c("Trial"),
            # text.common = "Average interaction effect (common effect model)*",
            # text.random = "Average interaction effect (random effect model)*",
            text.random = "",
            title = "Treatment-covariate interaction on primary endpoint: Comorbidity",
            xlab = "vaccination: less AEs <-> no vaccination: less AEs",
            xlab.pos = 0.0,
            fs.xlab = 11
)
```

![](two_stage_files/figure-html/unnamed-chunk-58-1.png)<!-- -->

# Subgroups: Vaccination on AEs: Descriptive

```r
# Calculate the inverse variance
df_sg_vacc_ae28$inverse_variance <- 1 / df_sg_vacc_ae28$standard_error^2
```

# Subgroups: Vaccination on AEs: Pooled across trials // Descriptive

```r
# Vaccinated
vacc.yes.ae28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_vaccyes_ae28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, 
                      random = F,
                      exclude = trial %in% c("Ghazaeian"))
summary(vacc.yes.ae28)
```

```
##                   OR             95%-CI %W(common) exclude
## Bari-SolidAct 1.0153 [0.3866;   2.6670]        8.7        
## COVINIB       1.0001 [0.0091; 110.3643]        0.4        
## TOFACOV       1.0075 [0.0000;      Inf]        0.0        
## RECOVERY      0.8843 [0.6530;   1.1975]       88.7        
## PANCOVID      1.4750 [0.2101;  10.3555]        2.1        
## 
## Number of studies: k = 5
## Number of observations: o = 216
## 
##                         OR           95%-CI     z p-value
## Common effect model 0.9054 [0.6804; 1.2047] -0.68  0.4951
## 
## Quantifying heterogeneity:
##  tau^2 = 0; tau = 0; I^2 = 0.0% [0.0%; 79.2%]; H = 1.00 [1.00; 2.19]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  0.32    4  0.9885
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
```

```r
forest.meta(vacc.yes.ae28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-60-1.png)<!-- -->

```r
# Unvaccinated
vacc.no.ae28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_vaccno_ae28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, 
                      random = F,
                      exclude = trial %in% c("Ghazaeian"))
summary(vacc.no.ae28)
```

```
##                   OR           95%-CI %W(common) exclude
## Bari-SolidAct 0.7785 [0.3958; 1.5314]        9.8        
## COVINIB       0.8436 [0.3304; 2.1544]        5.1        
## TOFACOV       0.6778 [0.2392; 1.9207]        4.1        
## RECOVERY      0.9245 [0.7227; 1.1825]       73.6        
## PANCOVID      1.1430 [0.5261; 2.4836]        7.4        
## 
## Number of studies: k = 5
## Number of observations: o = 410
## 
##                         OR           95%-CI     z p-value
## Common effect model 0.9076 [0.7347; 1.1211] -0.90  0.3683
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.1307]; tau = 0 [0.0000; 0.3616]
##  I^2 = 0.0% [0.0%; 79.2%]; H = 1.00 [1.00; 2.19]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  0.88    4  0.9269
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
```

```r
forest.meta(vacc.no.ae28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-60-2.png)<!-- -->

# Interaction: At risk on AEs

```r
# str(df_atrisk_ae28)
df_atrisk_ae28 <- df_atrisk_ae28[order(df_atrisk_ae28$trial), ]
atrisk.ae28 <- metagen(TE = log(log_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_atrisk_ae28,
                      sm = "OR",
                      fixed = F, 
                      random = T, 
                      method.tau = "ML", 
                      method.random.ci = "HK",
                      exclude = trial %in% c("Ghazaeian"))
summary(atrisk.ae28)
```

```
##                   OR            95%-CI %W(random) exclude
## ACTT-2        0.9655 [0.5544;  1.6815]       20.5        
## Bari-SolidAct 1.2295 [0.4141;  3.6502]        5.3        
## COV-BARRIER   1.2095 [0.7034;  2.0798]       21.5        
## COVINIB       0.5621 [0.0764;  4.1339]        1.6        
## Ghazaeian     0.2006 [0.0016; 25.2681]        0.0       *
## PANCOVID      0.9430 [0.2028;  4.3849]        2.7        
## RECOVERY      0.8407 [0.5708;  1.2382]       42.0        
## RUXCOVID      2.2435 [0.5674;  8.8707]        3.3        
## TACTIC-R      0.3137 [0.0633;  1.5562]        2.5        
## TOFACOV       1.0998 [0.0503; 24.0490]        0.7        
## 
## Number of studies: k = 9
## 
##                               OR           95%-CI     t p-value
## Random effects model (HK) 0.9609 [0.7613; 1.2128] -0.40  0.7029
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.5056]; tau = 0 [0.0000; 0.7111]
##  I^2 = 0.0% [0.0%; 64.8%]; H = 1.00 [1.00; 1.69]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  4.97    8  0.7607
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 8)
```

```r
forest.meta(atrisk.ae28,
            hetstat = F,
            leftcols = c("studlab"),
            leftlabs = c("Trial"),
            # text.common = "Average interaction effect (common effect model)*",
            # text.random = "Average interaction effect (random effect model)*",
            text.random = "",
            xlab = "at risk: less AEs <-> not at risk: less AEs",
            xlab.pos = 0.0,
            fs.xlab = 11
)
```

![](two_stage_files/figure-html/unnamed-chunk-61-1.png)<!-- -->

# Subgroups: At risk on AEs: Descriptive

```r
# Calculate the inverse variance
df_sg_atrisk_ae28$inverse_variance <- 1 / df_sg_atrisk_ae28$standard_error^2
```

# Subgroups: At risk on AEs: Pooled across trials // Descriptive

```r
# Not at risk
atrisk.no.ae28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_atriskno_ae28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, 
                      random = F,
                      exclude = trial %in% c("Ghazaeian"))
summary(atrisk.no.ae28)
```

```
##                   OR            95%-CI %W(common) exclude
## ACTT-2        0.9314 [0.6580;  1.3183]       21.5        
## COV-BARRIER   1.0217 [0.7192;  1.4513]       21.1        
## Bari-SolidAct 0.7806 [0.3656;  1.6669]        4.5        
## COVINIB       0.9038 [0.2727;  2.9958]        1.8        
## TOFACOV       0.6562 [0.2184;  1.9713]        2.1        
## Ghazaeian     3.6522 [0.2033; 65.6032]        0.0       *
## RECOVERY      0.9778 [0.7616;  1.2552]       41.6        
## TACTIC-R      2.5761 [0.7367;  9.0082]        1.7        
## RUXCOVID      0.4241 [0.1939;  0.9277]        4.2        
## PANCOVID      1.3361 [0.3569;  5.0020]        1.5        
## 
## Number of studies: k = 9
## Number of observations: o = 814
## 
##                         OR           95%-CI     z p-value
## Common effect model 0.9432 [0.8028; 1.1080] -0.71  0.4766
## 
## Quantifying heterogeneity:
##  tau^2 < 0.0001 [0.0000; 0.5913]; tau = 0.0025 [0.0000; 0.7690]
##  I^2 = 0.0% [0.0%; 64.8%]; H = 1.00 [1.00; 1.69]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  7.69    8  0.4640
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
```

```r
forest.meta(atrisk.no.ae28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-63-1.png)<!-- -->

```r
# At risk
atrisk.yes.ae28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_atriskyes_ae28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, 
                      random = F,
                      exclude = trial %in% c("Ghazaeian"))
summary(atrisk.yes.ae28)
```

```
##                   OR            95%-CI %W(common) exclude
## ACTT-2        0.9003 [0.5839;  1.3882]       19.3        
## COV-BARRIER   1.2458 [0.8231;  1.8855]       21.0        
## Bari-SolidAct 0.9479 [0.4284;  2.0972]        5.7        
## COVINIB       0.4843 [0.0927;  2.5317]        1.3        
## TOFACOV       0.5176 [0.0218; 12.3006]        0.4        
## RECOVERY      0.8289 [0.6163;  1.1147]       41.2        
## TACTIC-R      0.8247 [0.3014;  2.2565]        3.6        
## RUXCOVID      1.1563 [0.3700;  3.6137]        2.8        
## PANCOVID      1.0600 [0.4415;  2.5452]        4.7        
## 
## Number of studies: k = 9
## Number of observations: o = 608
## 
##                         OR           95%-CI     z p-value
## Common effect model 0.9358 [0.7737; 1.1317] -0.68  0.4937
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.0386]; tau = 0 [0.0000; 0.1965]
##  I^2 = 0.0% [0.0%; 64.8%]; H = 1.00 [1.00; 1.69]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  3.52    8  0.8975
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
```

```r
forest.meta(atrisk.yes.ae28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-63-2.png)<!-- -->

# Interaction: Comed on AEs

```r
# str(df_comed_ae28)
df_comed_ae28 <- df_comed_ae28[order(df_comed_ae28$trial), ]
comed.ae28 <- metagen(TE = log(log_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_comed_ae28,
                      sm = "OR",
                      fixed = F, 
                      random = T, 
                      method.tau = "ML", 
                      method.random.ci = "HK",
                      # subset = trial %in% c("COV-BARRIER", "ACTT-2"),
                      exclude = trial %in% c("Ghazaeian")) # incl in plot but exclude from analysis
summary(comed.ae28)
```

```
##                   OR             95%-CI %W(random) exclude
## ACTT-2        1.2089 [0.2767;   5.2815]        3.7        
## Bari-SolidAct 1.0026 [0.1126;   8.9252]        1.7        
## COV-BARRIER   0.7852 [0.3970;   1.5531]       17.4        
## COVINIB       0.8775 [0.0480;  16.0361]        1.0        
## Ghazaeian     1.0038 [0.7189;   1.4017]        0.0       *
## PANCOVID      2.7964 [0.1199;  65.2126]        0.8        
## RECOVERY      1.1015 [0.7804;   1.5547]       68.2        
## RUXCOVID      2.6439 [0.7197;   9.7125]        4.8        
## TACTIC-R      3.8769 [0.4825;  31.1492]        1.9        
## TOFACOV       2.9293 [0.0768; 111.7502]        0.6        
## 
## Number of studies: k = 9
## 
##                               OR           95%-CI    t p-value
## Random effects model (HK) 1.1234 [0.8685; 1.4530] 1.04  0.3275
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.4938]; tau = 0 [0.0000; 0.7027]
##  I^2 = 0.0% [0.0%; 64.8%]; H = 1.00 [1.00; 1.69]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  4.73    8  0.7863
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 8)
```

```r
forest.meta(comed.ae28,
            hetstat = F,
            leftcols = c("studlab"),
            leftlabs = c("Trial"),
            # text.common = "Average interaction effect (common effect model)*",
            # text.random = "Average interaction effect (random effect model)*",
            text.random = "",
            xlab = "More comed: less AEs <-> Less comed: less AEs",
            xlab.pos = 0.0,
            fs.xlab = 11
)
```

![](two_stage_files/figure-html/unnamed-chunk-64-1.png)<!-- -->

# Subgroups: Comed on AEs: Descriptive

```r
# Calculate the inverse variance
df_sg_comed_ae28$inverse_variance <- 1 / df_sg_comed_ae28$standard_error^2
```

# Subgroups: Comed on AEs: Pooled across trials // Descriptive

```r
# No comedication
no.comed.ae28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_no_comed_mort28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, # the true subgroup effect is assumed the same in all trials
                      random = F,
                      exclude = trial %in% c("Ghazaeian"))
summary(no.comed.ae28)
```

```
##                   OR            95%-CI %W(common) exclude
## ACTT-2        0.6654 [0.3696;  1.1980]       43.1        
## COV-BARRIER   0.3813 [0.1530;  0.9503]       17.9        
## Bari-SolidAct 0.3455 [0.0092; 12.9587]        1.1        
## COVINIB       0.2037 [0.0142;  2.9320]        2.1        
## TOFACOV       0.7356 [0.0160; 33.9204]        1.0        
## RECOVERY      0.7070 [0.3288;  1.5201]       25.4        
## TACTIC-R      1.4950 [0.2268;  9.8560]        4.2        
## RUXCOVID      1.4644 [0.2691;  7.9679]        5.2        
## 
## Number of studies: k = 8
## Number of observations: o = 140
## 
##                         OR           95%-CI     z p-value
## Common effect model 0.6390 [0.4344; 0.9401] -2.27  0.0230
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.8321]; tau = 0 [0.0000; 0.9122]
##  I^2 = 0.0% [0.0%; 67.6%]; H = 1.00 [1.00; 1.76]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  3.84    7  0.7985
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
```

```r
forest.meta(no.comed.ae28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-66-1.png)<!-- -->

```r
# Dexa, but no Toci
dexa.comed.ae28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_dexa_ae28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, 
                      random = F,
                      exclude = trial %in% c("Ghazaeian")) # incl in plot but exclude from analysis
summary(dexa.comed.ae28)
```

```
##                          OR           95%-CI %W(common) exclude
## ACTT-2               0.4929 [0.0465; 5.2192]        0.5        
## COV-BARRIER          1.0564 [0.7834; 1.4245]       31.3        
## Bari-SolidAct        0.9557 [0.5466; 1.6712]        8.9        
## COVINIB              0.4622 [0.0256; 8.3402]        0.3        
## TOFACOV              0.8061 [0.2771; 2.3452]        2.4        
## Ghazaeian     50433754.1850 [0.0000;    Inf]        0.0       *
## RECOVERY             0.9194 [0.7138; 1.1841]       43.6        
## TACTIC-R             1.2762 [0.5560; 2.9290]        4.0        
## RUXCOVID             0.9238 [0.3884; 2.1977]        3.7        
## PANCOVID             1.1413 [0.5450; 2.3901]        5.1        
## 
## Number of studies: k = 9
## Number of observations: o = 731
## 
##                         OR           95%-CI     z p-value
## Common effect model 0.9789 [0.8282; 1.1570] -0.25  0.8026
## 
## Quantifying heterogeneity:
##  tau^2 = 0; tau = 0; I^2 = 0.0% [0.0%; 64.8%]; H = 1.00 [1.00; 1.69]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  1.78    8  0.9871
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
```

```r
forest.meta(dexa.comed.ae28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-66-2.png)<!-- -->

```r
# Dexa and Toci
dexa.toci.comed.ae28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_dexa_toci_ae28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, 
                      random = F,
                      exclude = trial %in% c("Ghazaeian"))
summary(dexa.toci.comed.ae28)
```

```
##              OR             95%-CI %W(common) exclude
## RECOVERY 0.9759 [0.7141;   1.3336]       98.6        
## TACTIC-R 1.3018 [0.0219;  77.2590]        0.6        
## PANCOVID 7.7134 [0.2340; 254.2128]        0.8        
## 
## Number of studies: k = 3
## Number of observations: o = 179
## 
##                         OR           95%-CI     z p-value
## Common effect model 0.9936 [0.7286; 1.3549] -0.04  0.9675
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 47.0348]; tau = 0 [0.0000; 6.8582]
##  I^2 = 0.0% [0.0%; 89.6%]; H = 1.00 [1.00; 3.10]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  1.35    2  0.5091
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
```

```r
forest.meta(dexa.toci.comed.ae28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-66-3.png)<!-- -->

```r
# No dexa, but toci
toci.comed.ae28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_toci_ae28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, 
                      random = F,
                      exclude = trial %in% c("Ghazaeian"))
summary(toci.comed.ae28)
```

```
## Number of observations: o = 3
## 
##              OR           95%-CI     z p-value
## RECOVERY 0.4240 [0.0287; 6.2566] -0.62  0.5321
```

```r
forest.meta(toci.comed.ae28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-66-4.png)<!-- -->

# Interaction: Symptom onset on primary endpoint

```r
# str(df_symp_mort28)
symp.mort28 <- metagen(TE = log(log_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_symp_mort28,
                      # n.e = n_int + n_cont,
                      # n.c = n_control,
                      sm = "OR",
                      fixed = F, # the true interaction is assumed the same in all trials 
                      random = T, # the true interactions are assumed random across trials
                      method.tau = "ML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "ci", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Treatment-covariate interaction on primary endpoint: Symptom duration",
                      # subset = trial %in% c("COV-BARRIER", "Bari-SolidAct", "ACTT-2", "TOFACOV", "COVINIB"),
                      # exclude = trial %in% c("TOFACOV", "COVINIB", "Ghazaeian") # incl in plot but exclude from analysis
                      )
summary(symp.mort28)
```

```
## Review:     Treatment-covariate interaction on primary endpoint: Symptom dur ...
## 
##                   OR           95%-CI %W(random)
## Bari-SolidAct 0.8094 [0.6417; 1.0209]        1.2
## ACTT-2        0.9528 [0.8288; 1.0952]        3.3
## Ghazaeian     1.1957 [0.6345; 2.2530]        0.2
## TOFACOV       0.8236 [0.4957; 1.3682]        0.3
## COVINIB       1.2663 [0.4523; 3.5451]        0.1
## COV-BARRIER   1.0403 [0.9734; 1.1117]       14.6
## RECOVERY      0.9969 [0.9683; 1.0264]       76.0
## TACTIC-R      0.9948 [0.8635; 1.1460]        3.2
## RUXCOVID      1.0252 [0.7924; 1.3265]        1.0
## PANCOVID      0.5937 [0.3212; 1.0973]        0.2
## 
## Number of studies: k = 10
## 
##                                  OR           95%-CI     t p-value
## Random effects model (HK-CI) 0.9984 [0.9697; 1.0280] -0.12  0.9050
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.0624]; tau = 0 [0.0000; 0.2499]
##  I^2 = 0.0% [0.0%; 62.4%]; H = 1.00 [1.00; 1.63]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  8.92    9  0.4451
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = 9)
```

```r
forest.meta(symp.mort28,
            hetstat = F,
            leftcols = c("studlab"),
            leftlabs = c("Trial"),
            # text.common = "Average interaction effect (common effect model)*",
            # text.random = "Average interaction effect (random effect model)*",
            text.random = "",
            title = "Treatment-covariate interaction on primary endpoint:Symptom duration",
            xlab = "shorter duration: greater effect <-> longer duration: greater effect",
            xlab.pos = 0.0,
            fs.xlab = 11
)
```

![](two_stage_files/figure-html/unnamed-chunk-67-1.png)<!-- -->

# Subgroups: Symptom onset on primary endpoint: Descriptive

```r
# Calculate the inverse variance
df_sg_symp_mort28$inverse_variance <- 1 / df_sg_symp_mort28$standard_error^2
```

# Subgroups: Symptom onset on primary endpoint: Pooled across trials // Descriptive

```r
# Enrolment more than 10 days after symptom onset
sympdur.m10.mort28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_m10_mort28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, 
                      random = F)
summary(sympdur.m10.mort28)
```

```
##                   OR            95%-CI %W(common)
## ACTT-2        0.5058 [0.1104;  2.3172]        1.9
## COV-BARRIER   0.4370 [0.2673;  0.7142]       17.9
## Bari-SolidAct 0.0740 [0.0072;  0.7617]        0.8
## TOFACOV       0.7397 [0.0210; 26.0892]        0.3
## Ghazaeian     2.9024 [0.1123; 74.9911]        0.4
## RECOVERY      0.7991 [0.6273;  1.0178]       73.7
## TACTIC-R      0.5046 [0.1532;  1.6615]        3.0
## RUXCOVID      1.2047 [0.2129;  6.8161]        1.4
## PANCOVID      0.1682 [0.0094;  3.0026]        0.5
## 
## Number of studies: k = 9
## Number of observations: o = 508
## 
##                         OR           95%-CI     z p-value
## Common effect model 0.6901 [0.5607; 0.8495] -3.50  0.0005
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0856 [0.0000; 2.4057]; tau = 0.2925 [0.0000; 1.5510]
##  I^2 = 25.6% [0.0%; 65.1%]; H = 1.16 [1.00; 1.69]
## 
## Test of heterogeneity:
##      Q d.f. p-value
##  10.75    8  0.2162
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
```

```r
forest.meta(sympdur.m10.mort28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-69-1.png)<!-- -->

```r
# Enrolment between 5 and 10 days after symptom onset
sympdur.510.mort28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_510_mort28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, 
                      random = F)
summary(sympdur.510.mort28)
```

```
##                   OR            95%-CI %W(common)
## ACTT-2        0.7311 [0.3360;  1.5909]        5.8
## COV-BARRIER   0.6140 [0.3640;  1.0356]       12.9
## Bari-SolidAct 1.0123 [0.3099;  3.3064]        2.5
## COVINIB       0.3212 [0.0220;  4.6848]        0.5
## TOFACOV       2.2140 [0.1700; 28.8346]        0.5
## Ghazaeian     0.4834 [0.0410;  5.7064]        0.6
## RECOVERY      0.7563 [0.6087;  0.9397]       74.9
## TACTIC-R      1.2964 [0.2578;  6.5186]        1.4
## RUXCOVID      1.6878 [0.1161; 24.5438]        0.5
## PANCOVID      0.4977 [0.0280;  8.8500]        0.4
## 
## Number of studies: k = 10
## Number of observations: o = 619
## 
##                         OR           95%-CI     z p-value
## Common effect model 0.7464 [0.6186; 0.9007] -3.05  0.0023
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.0210]; tau = 0 [0.0000; 0.1450]
##  I^2 = 0.0% [0.0%; 62.4%]; H = 1.00 [1.00; 1.63]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  2.88    9  0.9689
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
```

```r
forest.meta(sympdur.510.mort28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-69-2.png)<!-- -->

```r
# Enrolment less than 5 days after symptom onset
sympdur.5.mort28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_5_mort28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, 
                      random = F)
summary(sympdur.5.mort28)
```

```
##                   OR            95%-CI %W(common)
## ACTT-2        0.7105 [0.2317;  2.1788]        6.2
## COV-BARRIER   0.3162 [0.0922;  1.0847]        5.1
## Bari-SolidAct 1.6344 [0.2845;  9.3901]        2.5
## COVINIB       0.2792 [0.0177;  4.3940]        1.0
## TOFACOV       0.9634 [0.0472; 19.6721]        0.8
## Ghazaeian     0.5675 [0.0204; 15.7584]        0.7
## RECOVERY      0.9395 [0.6882;  1.2826]       79.7
## TACTIC-R      0.7875 [0.0937;  6.6163]        1.7
## RUXCOVID      0.1098 [0.0012;  9.9632]        0.4
## PANCOVID      0.5685 [0.0744;  4.3421]        1.9
## 
## Number of studies: k = 10
## Number of observations: o = 304
## 
##                         OR           95%-CI     z p-value
## Common effect model 0.8546 [0.6472; 1.1284] -1.11  0.2678
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0249 [0.0000; 0.3730]; tau = 0.1579 [0.0000; 0.6108]
##  I^2 = 0.0% [0.0%; 62.4%]; H = 1.00 [1.00; 1.63]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  5.14    9  0.8219
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
```

```r
forest.meta(sympdur.5.mort28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-69-3.png)<!-- -->

# Interaction: CRP on primary endpoint

```r
# str(df_crp_mort28)
crp.mort28 <- metagen(TE = log(log_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_crp_mort28,
                      # n.e = n_int + n_cont,
                      # n.c = n_control,
                      sm = "OR",
                      fixed = F, # the true interaction is assumed the same in all trials 
                      random = T, # the true interactions are assumed random across trials
                      method.tau = "ML", # same results with ML (-> see one-stage!)
                      hakn = T, # Hartung-Knapp- Sidik-Jonkman (HKSJ) modified estimate of the variance / 95% CI -> notes
                      adhoc.hakn.ci = "ci", # Argument 'adhoc.hakn.ci' must be "", "se", "ci", or "IQWiG6".
                      title = "Treatment-covariate interaction on primary endpoint: CRP",
                      # subset = trial %in% c("COV-BARRIER", "Bari-SolidAct", "ACTT-2", "TOFACOV", "COVINIB"),
                      # exclude = trial %in% c("TOFACOV", "COVINIB", "Ghazaeian") # incl in plot but exclude from analysis
                      )
summary(crp.mort28)
```

```
## Review:     Treatment-covariate interaction on primary endpoint: CRP
## 
##                   OR           95%-CI %W(random)
## Bari-SolidAct 1.0001 [0.9971; 1.0031]       18.3
## ACTT-2        0.9983 [0.9935; 1.0031]        7.1
## Ghazaeian     0.9966 [0.9591; 1.0357]        0.1
## TOFACOV       0.9893 [0.9428; 1.0381]        0.1
## COVINIB       1.0215 [0.9907; 1.0532]        0.2
## COV-BARRIER   1.0005 [0.9974; 1.0036]       16.9
## RECOVERY      1.0001 [0.9984; 1.0019]       55.5
## TACTIC-R      1.0009 [0.9899; 1.0120]        1.4
## RUXCOVID      0.9940 [0.9744; 1.0140]        0.4
## PANCOVID      0.9744 [0.9269; 1.0242]        0.1
## 
## Number of studies: k = 10
## 
##                                  OR           95%-CI    t p-value
## Random effects model (HK-CI) 1.0001 [0.9988; 1.0013] 0.08  0.9332
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.0001]; tau = 0 [0.0000; 0.0103]
##  I^2 = 0.0% [0.0%; 62.4%]; H = 1.00 [1.00; 1.63]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  4.10    9  0.9050
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hartung-Knapp adjustment for random effects model (df = )
```

```r
forest.meta(crp.mort28,
            hetstat = F,
            leftcols = c("studlab"),
            leftlabs = c("Trial"),
            # text.common = "Average interaction effect (common effect model)*",
            # text.random = "Average interaction effect (random effect model)*",
            text.random = "",
            title = "Treatment-covariate interaction on primary endpoint:Symptom duration",
            xlab = "lower CRP: greater effect <-> higher CRP: greater effect",
            xlab.pos = 0.0,
            fs.xlab = 11
)
```

![](two_stage_files/figure-html/unnamed-chunk-70-1.png)<!-- -->

# Subgroups: CRP on primary endpoint: Descriptive

```r
# Calculate the inverse variance
df_sg_crp_mort28$inverse_variance <- 1 / df_sg_crp_mort28$standard_error^2
```

# Subgroups: CRP on primary endpoint: Pooled across trials // Descriptive

```r
# CRP above 75
crp.above75.mort28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_crpabove75_mort28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, 
                      random = F)
summary(crp.above75.mort28)
```

```
##                   OR             95%-CI %W(common)
## ACTT-2        0.5400 [0.2852;   1.0224]        6.6
## COV-BARRIER   0.5539 [0.3364;   0.9120]       10.8
## Bari-SolidAct 0.8155 [0.3179;   2.0917]        3.0
## COVINIB       0.2566 [0.0160;   4.1244]        0.3
## TOFACOV       1.0000 [0.0013; 781.1913]        0.1
## Ghazaeian     0.5965 [0.0442;   8.0586]        0.4
## RECOVERY      0.8244 [0.6813;   0.9977]       74.1
## TACTIC-R      0.8409 [0.3433;   2.0597]        3.4
## RUXCOVID      1.1965 [0.2099;   6.8222]        0.9
## PANCOVID      0.0852 [0.0054;   1.3415]        0.4
## 
## Number of studies: k = 10
## Number of observations: o = 804
## 
##                         OR           95%-CI     z p-value
## Common effect model 0.7605 [0.6454; 0.8962] -3.27  0.0011
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0173 [0.0000; 0.4951]; tau = 0.1314 [0.0000; 0.7036]
##  I^2 = 0.0% [0.0%; 62.4%]; H = 1.00 [1.00; 1.63]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  6.73    9  0.6656
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
```

```r
forest.meta(crp.above75.mort28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-72-1.png)<!-- -->

```r
# CRP below 75
crp.below75.mort28 <- metagen(TE = log(hazard_odds_ratio),
                      seTE = standard_error,
                      studlab = trial,
                      data = df_sg_crpbelow75_mort28,
                      n.e = n_intervention,
                      n.c = n_control,
                      sm = "OR",
                      fixed = T, 
                      random = F)
summary(crp.below75.mort28)
```

```
##                   OR            95%-CI %W(common)
## ACTT-2        2.6842 [0.5217; 13.8098]        1.4
## COV-BARRIER   0.6043 [0.3482;  1.0487]       12.4
## Bari-SolidAct 0.6506 [0.1621;  2.6111]        2.0
## COVINIB       0.2437 [0.0140;  4.2471]        0.5
## TOFACOV       2.5400 [0.1929; 33.4421]        0.6
## Ghazaeian     1.0842 [0.1413;  8.3170]        0.9
## RECOVERY      0.8182 [0.6588;  1.0163]       80.4
## TACTIC-R      0.5298 [0.0340;  8.2460]        0.5
## RUXCOVID      4.4774 [0.3136; 63.9343]        0.5
## PANCOVID      0.9213 [0.1046;  8.1175]        0.8
## 
## Number of studies: k = 10
## Number of observations: o = 575
## 
##                         OR           95%-CI     z p-value
## Common effect model 0.8067 [0.6642; 0.9798] -2.17  0.0303
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 1.1274]; tau = 0 [0.0000; 1.0618]
##  I^2 = 0.0% [0.0%; 62.4%]; H = 1.00 [1.00; 1.63]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  6.45    9  0.6943
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
```

```r
forest.meta(crp.below75.mort28,
            hetstat = F,
            leftcols = c("studlab", "TE", "seTE", "n.e", "n.c"),
            leftlabs = c("Trial", "aOR", "stand. error", "events_int", "events_cont"),
            sortvar = +TE)
```

![](two_stage_files/figure-html/unnamed-chunk-72-2.png)<!-- -->

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
result_list[[5]] <- extract_interaction(comorb.count.mort28, "comorbidity count") 
result_list[[6]] <- extract_interaction(comorb.any.mort28, "any comorbidity") 
result_list[[7]] <- extract_interaction(comorb.noimmuno.mort28, "comorbidity_noimmuno") 
result_list[[8]] <- extract_interaction(comed.mort28, "comedication")
result_list[[9]] <- extract_interaction(vacc.ae28, "vaccination on AEs") 
result_list[[10]] <- extract_interaction(symp.mort28, "symptom duration") 
result_list[[11]] <- extract_interaction(crp.mort28, "crp") 
result_list[[12]] <- extract_interaction(atrisk.ae28, "at risk on AEs") 
result_list[[13]] <- extract_interaction(comed.ae28, "comedication on AEs") 

# Filter out NULL results and bind the results into a single data frame
interaction_df <- do.call(rbind, Filter(function(x) !is.null(x), result_list))

# Add the analysis approach
interaction_df$approach <- "two-stage"

# Nicely formatted table
kable(interaction_df, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|variable             | log_odds_ratio| ci_lower| ci_upper| standard_error| p_value|approach  |
|:--------------------|--------------:|--------:|--------:|--------------:|-------:|:---------|
|respiratory support  |          0.880|    0.698|    1.109|          0.101|   0.239|two-stage |
|ventilation          |          0.787|    0.572|    1.082|          0.124|   0.111|two-stage |
|age                  |          1.010|    1.001|    1.020|          0.004|   0.031|two-stage |
|comorbidity          |          1.202|    1.037|    1.394|          0.065|   0.020|two-stage |
|comorbidity count    |          1.093|    0.983|    1.216|          0.047|   0.089|two-stage |
|any comorbidity      |          1.456|    1.177|    1.802|          0.094|   0.003|two-stage |
|comorbidity_noimmuno |          1.103|    0.963|    1.263|          0.060|   0.137|two-stage |
|comedication         |          0.976|    0.766|    1.244|          0.124|   0.845|two-stage |
|vaccination on AEs   |          1.002|    0.698|    1.439|          0.185|   0.992|two-stage |
|symptom duration     |          0.998|    0.970|    1.028|          0.013|   0.905|two-stage |
|crp                  |          1.000|    0.999|    1.001|          0.001|   0.933|two-stage |
|at risk on AEs       |          0.961|    0.761|    1.213|          0.101|   0.703|two-stage |
|comedication on AEs  |          1.123|    0.869|    1.453|          0.112|   0.328|two-stage |

```r
# Save
saveRDS(interaction_df, file = "int_effects_two-stage.RData")
```

# Collect all subgroup effect estimates for forestplot // only descriptive purpose

```r
# Empty data frame to store the results
subgroup_df <- data.frame(
  variable = character(),
  odds_ratio = numeric(),
  ci_lower = numeric(),
  ci_upper = numeric(),
  standard_error = numeric(),
  n_intervention = numeric(),
  n_intervention_tot = numeric(),
  n_control = numeric(),
  n_control_tot = numeric()
)

# Function to extract subgroup results
extract_subgroup <- function(model, variable_name, n_int, n_int_tot, n_cont, n_cont_tot) {
  if (inherits(model, "metagen")) {
    odds_ratio <- exp(model$TE.common)
    ci_lower <- exp(model$lower.common)
    ci_upper <- exp(model$upper.common)
    standard_error <- model$seTE.common
  } else {
    stop("Unsupported model class")
  }
  # capture the results
  result <- data.frame(
    variable = variable_name,
    odds_ratio = odds_ratio,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    standard_error = standard_error,
    n_intervention = n_int,
    n_intervention_tot = n_int_tot,
    n_control = n_cont,
    n_control_tot = n_cont_tot
  )
  return(result)
}

# Loop through
result_list <- list()

result_list[[1]] <- extract_subgroup(rs.no.ox.mort28, "No oxygen",
                                     sum(df_sg_no_ox_mort28$n_intervention),
                                     sum(df_sg_no_ox_mort28$n_intervention_tot),
                                     sum(df_sg_no_ox_mort28$n_control),
                                     sum(df_sg_no_ox_mort28$n_control_tot))
result_list[[2]] <- extract_subgroup(rs.low.ox.mort28, "Low-flow oxygen",
                                     sum(df_sg_low_ox_mort28$n_intervention),
                                     sum(df_sg_low_ox_mort28$n_intervention_tot),
                                     sum(df_sg_low_ox_mort28$n_control),
                                     sum(df_sg_low_ox_mort28$n_control_tot))
result_list[[3]] <- extract_subgroup(rs.niv.mort28, "High-flow or non-invasive ventilation",
                                     sum(df_sg_niv_mort28$n_intervention),
                                     sum(df_sg_niv_mort28$n_intervention_tot),
                                     sum(df_sg_niv_mort28$n_control),
                                     sum(df_sg_niv_mort28$n_control_tot))
result_list[[4]] <- extract_subgroup(rs.ecmo.mort28, "Mechanical ventilation or ECMO",
                                     sum(df_sg_ecmo_mort28$n_intervention),
                                     sum(df_sg_ecmo_mort28$n_intervention_tot),
                                     sum(df_sg_ecmo_mort28$n_control),
                                     sum(df_sg_ecmo_mort28$n_control_tot))
result_list[[5]] <- extract_subgroup(no.vent.mort28, "No ventilation at baseline",
                                     sum(df_sg_no_vent_mort28$n_intervention),
                                     sum(df_sg_no_vent_mort28$n_intervention_tot),
                                     sum(df_sg_no_vent_mort28$n_control),
                                     sum(df_sg_no_vent_mort28$n_control_tot))
result_list[[6]] <- extract_subgroup(vent.mort28, "Ventilation at baseline",
                                     sum(df_sg_vent_mort28$n_intervention),
                                     sum(df_sg_vent_mort28$n_intervention_tot),
                                     sum(df_sg_vent_mort28$n_control),
                                     sum(df_sg_vent_mort28$n_control_tot))
result_list[[7]] <- extract_subgroup(age.above70.mort28, "70 years of age or older",
                                     sum(df_sg_old_mort28$n_intervention),
                                     sum(df_sg_old_mort28$n_intervention_tot),
                                     sum(df_sg_old_mort28$n_control),
                                     sum(df_sg_old_mort28$n_control_tot))
result_list[[8]] <- extract_subgroup(age.below70.mort28, "Below 70 years of age",
                                     sum(df_sg_young_mort28$n_intervention),
                                     sum(df_sg_young_mort28$n_intervention_tot),
                                     sum(df_sg_young_mort28$n_control),
                                     sum(df_sg_young_mort28$n_control_tot))
result_list[[9]] <- extract_subgroup(no.comorb.mort28, "No comorbidity",
                                     sum(df_sg_no_comorb_mort28$n_intervention),
                                     sum(df_sg_no_comorb_mort28$n_intervention_tot),
                                     sum(df_sg_no_comorb_mort28$n_control),
                                     sum(df_sg_no_comorb_mort28$n_control_tot))
result_list[[10]] <- extract_subgroup(one.comorb.mort28, "One comorbidity",
                                     sum(df_sg_one_comorb_mort28$n_intervention),
                                     sum(df_sg_one_comorb_mort28$n_intervention_tot),
                                     sum(df_sg_one_comorb_mort28$n_control),
                                     sum(df_sg_one_comorb_mort28$n_control_tot))
result_list[[11]] <- extract_subgroup(mult.comorb.mort28, "Multiple comorbidities",
                                     sum(df_sg_mult_comorb_mort28$n_intervention),
                                     sum(df_sg_mult_comorb_mort28$n_intervention_tot),
                                     sum(df_sg_mult_comorb_mort28$n_control),
                                     sum(df_sg_mult_comorb_mort28$n_control_tot))
result_list[[12]] <- extract_subgroup(immun.comorb.mort28, "Immunocompromised",
                                     sum(df_sg_immun_mort28$n_intervention),
                                     sum(df_sg_immun_mort28$n_intervention_tot),
                                     sum(df_sg_immun_mort28$n_control),
                                     sum(df_sg_immun_mort28$n_control_tot))
result_list[[13]] <- extract_subgroup(no.comed.mort28, "No Dexamethasone, no Tocilizumab",
                                     sum(df_sg_no_comed_mort28$n_intervention),
                                     sum(df_sg_no_comed_mort28$n_intervention_tot),
                                     sum(df_sg_no_comed_mort28$n_control),
                                     sum(df_sg_no_comed_mort28$n_control_tot))
result_list[[14]] <- extract_subgroup(dexa.comed.mort28, "Dexamethasone, but no Tocilizumab",
                                     sum(df_sg_dexa_mort28$n_intervention),
                                     sum(df_sg_dexa_mort28$n_intervention_tot),
                                     sum(df_sg_dexa_mort28$n_control),
                                     sum(df_sg_dexa_mort28$n_control_tot))
result_list[[15]] <- extract_subgroup(dexa.toci.comed.mort28, "Dexamethasone and Tocilizumab",
                                     sum(df_sg_dexa_toci_mort28$n_intervention),
                                     sum(df_sg_dexa_toci_mort28$n_intervention_tot),
                                     sum(df_sg_dexa_toci_mort28$n_control),
                                     sum(df_sg_dexa_toci_mort28$n_control_tot))
result_list[[16]] <- extract_subgroup(toci.comed.mort28, "Tocilizumab, but no Dexamethasone",
                                     sum(df_sg_toci_mort28$n_intervention),
                                     sum(df_sg_toci_mort28$n_intervention_tot),
                                     sum(df_sg_toci_mort28$n_control),
                                     sum(df_sg_toci_mort28$n_control_tot))
result_list[[17]] <- extract_subgroup(sympdur.m10.mort28, "Enrolment more than 10 days after symptom onset",
                                     sum(df_sg_m10_mort28$n_intervention),
                                     sum(df_sg_m10_mort28$n_intervention_tot),
                                     sum(df_sg_m10_mort28$n_control),
                                     sum(df_sg_m10_mort28$n_control_tot))
result_list[[18]] <- extract_subgroup(sympdur.510.mort28, "Enrolment between 5 and 10 days after symptom onset",
                                     sum(df_sg_510_mort28$n_intervention),
                                     sum(df_sg_510_mort28$n_intervention_tot),
                                     sum(df_sg_510_mort28$n_control),
                                     sum(df_sg_510_mort28$n_control_tot))
result_list[[19]] <- extract_subgroup(sympdur.5.mort28, "Enrolment 5 days or earlier after symptom onset",
                                     sum(df_sg_5_mort28$n_intervention),
                                     sum(df_sg_5_mort28$n_intervention_tot),
                                     sum(df_sg_5_mort28$n_control),
                                     sum(df_sg_5_mort28$n_control_tot))
result_list[[20]] <- extract_subgroup(crp.above75.mort28, "C-reactive protein 75mg/L or more",
                                     sum(df_sg_crpabove75_mort28$n_intervention),
                                     sum(df_sg_crpabove75_mort28$n_intervention_tot),
                                     sum(df_sg_crpabove75_mort28$n_control),
                                     sum(df_sg_crpabove75_mort28$n_control_tot))
result_list[[21]] <- extract_subgroup(crp.below75.mort28, "C-reactive protein less than 75mg/L",
                                     sum(df_sg_crpbelow75_mort28$n_intervention),
                                     sum(df_sg_crpbelow75_mort28$n_intervention_tot),
                                     sum(df_sg_crpbelow75_mort28$n_control),
                                     sum(df_sg_crpbelow75_mort28$n_control_tot))

# Filter out NULL results and bind the results into a single data frame
subgroup_df <- do.call(rbind, Filter(function(x) !is.null(x), result_list))

# Add the analysis approach
subgroup_df$approach <- "two-stage"

# Nicely formatted table
kable(subgroup_df, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|variable                                            | odds_ratio|  ci_lower|   ci_upper| standard_error| n_intervention| n_intervention_tot| n_control| n_control_tot|approach  |
|:---------------------------------------------------|----------:|---------:|----------:|--------------:|--------------:|------------------:|---------:|-------------:|:---------|
|No oxygen                                           |  0.8703228| 0.4656247|  1.6267646|      0.3191305|             20|                537|        24|           516|two-stage |
|Low-flow oxygen                                     |  0.8510901| 0.7134192|  1.0153278|      0.0900265|            305|               3919|       323|          3798|two-stage |
|High-flow or non-invasive ventilation               |  0.6630359| 0.5450615|  0.8065449|      0.0999664|            270|               1464|       326|          1364|two-stage |
|Mechanical ventilation or ECMO                      |  0.6886242| 0.4543353|  1.0437300|      0.2121775|             72|                239|        91|           234|two-stage |
|No ventilation at baseline                          |  0.8477962| 0.7153364|  1.0047836|      0.0866788|            325|               4456|       347|          4316|two-stage |
|Ventilation at baseline                             |  0.6752787| 0.5673755|  0.8037029|      0.0888303|            342|               1703|       417|          1600|two-stage |
|70 years of age or older                            |  0.9050917| 0.7580040|  1.0807213|      0.0904852|            369|               1439|       373|          1322|two-stage |
|Below 70 years of age                               |  0.6931462| 0.5881915|  0.8168285|      0.0837711|            298|               4720|       391|          4594|two-stage |
|No comorbidity                                      |  0.5898937| 0.4669158|  0.7452618|      0.1192845|            149|               2632|       219|          2590|two-stage |
|One comorbidity                                     |  0.8710405| 0.7027450|  1.0796399|      0.1095400|            229|               1876|       234|          1792|two-stage |
|Multiple comorbidities                              |  0.7565476| 0.6170682|  0.9275543|      0.1039743|            279|               1603|       305|          1489|two-stage |
|Immunocompromised                                   |  1.8503725| 0.4892680|  6.9979607|      0.6787022|             10|                 48|         6|            45|two-stage |
|No Dexamethasone, no Tocilizumab                    |  0.6390371| 0.4344043|  0.9400655|      0.1969357|             57|                968|        83|           944|two-stage |
|Dexamethasone, but no Tocilizumab                   |  0.7942599| 0.6798863|  0.9278740|      0.0793306|            428|               3869|       463|          3678|two-stage |
|Dexamethasone and Tocilizumab                       |  0.6924466| 0.5438695|  0.8816127|      0.1232276|            174|               1304|       214|          1277|two-stage |
|Tocilizumab, but no Dexamethasone                   |  2.4876095| 0.4882718| 12.6736816|      0.8307323|              8|                 18|         4|            16|two-stage |
|Enrolment more than 10 days after symptom onset     |  0.6901491| 0.5606728|  0.8495254|      0.1060071|            223|               2516|       285|          2398|two-stage |
|Enrolment between 5 and 10 days after symptom onset |  0.7464295| 0.6185943|  0.9006824|      0.0958444|            294|               2724|       325|          2649|two-stage |
|Enrolment 5 days or earlier after symptom onset     |  0.8545865| 0.6472309|  1.1283733|      0.1417958|            150|                931|       154|           879|two-stage |
|C-reactive protein 75mg/L or more                   |  0.7604859| 0.6453511|  0.8961615|      0.0837582|            372|               3192|       432|          3130|two-stage |
|C-reactive protein less than 75mg/L                 |  0.8066882| 0.6641745|  0.9797814|      0.0991815|            276|               2698|       299|          2562|two-stage |

```r
# Save
saveRDS(subgroup_df, file = "subgroup_effects_two-stage.RData")
```

# Collect all subgroup effect estimates for forestplot / for safety outcome

```r
# Empty data frame to store the results
subgroup_df_ae <- data.frame(
  variable = character(),
  odds_ratio = numeric(),
  ci_lower = numeric(),
  ci_upper = numeric(),
  standard_error = numeric(),
  n_intervention = numeric(),
  n_intervention_tot = numeric(),
  n_control = numeric(),
  n_control_tot = numeric()
)

# Function to extract subgroup results
extract_subgroup <- function(model, variable_name, n_int, n_int_tot, n_cont, n_cont_tot) {
  if (inherits(model, "metagen")) {
    odds_ratio <- exp(model$TE.common)
    ci_lower <- exp(model$lower.common)
    ci_upper <- exp(model$upper.common)
    standard_error <- model$seTE.common
  } else {
    stop("Unsupported model class")
  }
  # capture the results
  result <- data.frame(
    variable = variable_name,
    odds_ratio = odds_ratio,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    standard_error = standard_error,
    n_intervention = n_int,
    n_intervention_tot = n_int_tot,
    n_control = n_cont,
    n_control_tot = n_cont_tot
  )
  return(result)
}

# Loop through
result_list <- list()

result_list[[1]] <- extract_subgroup(vacc.yes.ae28, "Vaccinated",
                                     sum(df_sg_vaccyes_ae28$n_intervention),
                                     sum(df_sg_vaccyes_ae28$n_intervention_tot),
                                     sum(df_sg_vaccyes_ae28$n_control),
                                     sum(df_sg_vaccyes_ae28$n_control_tot))
result_list[[2]] <- extract_subgroup(vacc.no.ae28, "Unvaccinated",
                                     sum(df_sg_vaccno_ae28$n_intervention),
                                     sum(df_sg_vaccno_ae28$n_intervention_tot),
                                     sum(df_sg_vaccno_ae28$n_control),
                                     sum(df_sg_vaccno_ae28$n_control_tot))
result_list[[3]] <- extract_subgroup(atrisk.no.ae28, "Not at risk",
                                     sum(df_sg_atriskno_ae28$n_intervention),
                                     sum(df_sg_atriskno_ae28$n_intervention_tot),
                                     sum(df_sg_atriskno_ae28$n_control),
                                     sum(df_sg_atriskno_ae28$n_control_tot))
result_list[[4]] <- extract_subgroup(atrisk.yes.ae28, "At risk",
                                     sum(df_sg_atriskyes_ae28$n_intervention),
                                     sum(df_sg_atriskyes_ae28$n_intervention_tot),
                                     sum(df_sg_atriskyes_ae28$n_control),
                                     sum(df_sg_atriskyes_ae28$n_control_tot))
result_list[[5]] <- extract_subgroup(no.comed.ae28, "No Dexamethasone, no Tocilizumab",
                                     sum(df_sg_no_comed_ae28$n_intervention),
                                     sum(df_sg_no_comed_ae28$n_intervention_tot),
                                     sum(df_sg_no_comed_ae28$n_control),
                                     sum(df_sg_no_comed_ae28$n_control_tot))
result_list[[6]] <- extract_subgroup(dexa.comed.ae28, "Dexamethasone, but no Tocilizumab",
                                     sum(df_sg_dexa_ae28$n_intervention),
                                     sum(df_sg_dexa_ae28$n_intervention_tot),
                                     sum(df_sg_dexa_ae28$n_control),
                                     sum(df_sg_dexa_ae28$n_control_tot))
result_list[[7]] <- extract_subgroup(dexa.toci.comed.ae28, "Dexamethasone and Tocilizumab",
                                     sum(df_sg_dexa_toci_ae28$n_intervention),
                                     sum(df_sg_dexa_toci_ae28$n_intervention_tot),
                                     sum(df_sg_dexa_toci_ae28$n_control),
                                     sum(df_sg_dexa_toci_ae28$n_control_tot))
result_list[[8]] <- extract_subgroup(toci.comed.ae28, "Tocilizumab, but no Dexamethasone",
                                     sum(df_sg_toci_ae28$n_intervention),
                                     sum(df_sg_toci_ae28$n_intervention_tot),
                                     sum(df_sg_toci_ae28$n_control),
                                     sum(df_sg_toci_ae28$n_control_tot))

# Filter out NULL results and bind the results into a single data frame
subgroup_df_ae <- do.call(rbind, Filter(function(x) !is.null(x), result_list))

# Add the analysis approach
subgroup_df_ae$approach <- "two-stage"

# Nicely formatted table
kable(subgroup_df_ae, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



|variable                          | odds_ratio|  ci_lower|  ci_upper| standard_error| n_intervention| n_intervention_tot| n_control| n_control_tot|approach  |
|:---------------------------------|----------:|---------:|---------:|--------------:|--------------:|------------------:|---------:|-------------:|:---------|
|Vaccinated                        |  0.9053710| 0.6804382| 1.2046600|      0.1457209|            107|               1530|       109|          1425|two-stage |
|Unvaccinated                      |  0.9075733| 0.7347363| 1.1210680|      0.1077891|            204|               2412|       206|          2342|two-stage |
|Not at risk                       |  0.9431719| 0.8028343| 1.1080409|      0.0821955|            415|               3518|       399|          3394|two-stage |
|At risk                           |  0.9357528| 0.7737216| 1.1317163|      0.0970116|            310|               2025|       298|          1806|two-stage |
|No Dexamethasone, no Tocilizumab  |  0.6390371| 0.4344043| 0.9400655|      0.1969357|            248|                939|       260|           892|two-stage |
|Dexamethasone, but no Tocilizumab |  0.9789049| 0.8282295| 1.1569919|      0.0852792|            383|               3474|       348|          3242|two-stage |
|Dexamethasone and Tocilizumab     |  0.9935810| 0.7286274| 1.3548806|      0.1582443|             94|               1134|        85|          1063|two-stage |
|Tocilizumab, but no Dexamethasone |  0.4240208| 0.0287365| 6.2566342|      1.3732983|              0|                 10|         3|            12|two-stage |

```r
# Save
saveRDS(subgroup_df_ae, file = "subgroup_effects_safety_two-stage.RData")
```

# Forestplot subgroup - on primary endpoint

```r
# first take out ventilation from subgroup_df 
subgroup_df <- subgroup_df %>% 
  filter(!variable %in% c("No ventilation at baseline", "Ventilation at baseline"))

# take the effect estimates from subgroup_df... 
subgroup_df$inverse_variance <- 1 / subgroup_df$standard_error^2
events_i <- subgroup_df$n_intervention
tot_i <- subgroup_df$n_intervention_tot
events_c <- subgroup_df$n_control
tot_c <- subgroup_df$n_control_tot

# ...and add p-interaction from interaction_df (except for ventilation and the other sens-analyses & safety endpoint)
interaction_df_fp <- interaction_df %>% 
  select(variable, p_value) %>% 
  filter(!variable %in% c("ventilation", "comorbidity count", "any comorbidity", "comorbidity_noimmuno", "vaccination on AEs"
                          , "at risk on AEs", "comedication on AEs"))
empty_row <- data.frame(
  variable = "",
  p_value = "")
first_part <- interaction_df_fp[1:1, ]
second_part <- interaction_df_fp[2:nrow(interaction_df_fp), ]
interaction_df_fp <- rbind(first_part, empty_row, empty_row, empty_row, second_part)
first_part <- interaction_df_fp[1:5, ]
second_part <- interaction_df_fp[6:nrow(interaction_df_fp), ]
interaction_df_fp <- rbind(first_part, empty_row, second_part)
first_part <- interaction_df_fp[1:7, ]
second_part <- interaction_df_fp[8:nrow(interaction_df_fp), ]
interaction_df_fp <- rbind(first_part, empty_row, empty_row, empty_row, second_part)
first_part <- interaction_df_fp[1:11, ]
second_part <- interaction_df_fp[12:nrow(interaction_df_fp), ]
interaction_df_fp <- rbind(first_part, empty_row, empty_row, empty_row, second_part)
first_part <- interaction_df_fp[1:15, ]
second_part <- interaction_df_fp[16:nrow(interaction_df_fp), ]
interaction_df_fp <- rbind(first_part, empty_row, empty_row, second_part)
interaction_df_fp <- rbind(interaction_df_fp, empty_row)

p_int <- interaction_df_fp$p_value
# p_int <- round(p_int,2)

# ...and overall results from result_df
mort_28_OR <- result_df$hazard_odds_ratio[1]
mort_28_ci_lower <- result_df$ci_lower[1]
mort_28_ci_upper <- result_df$ci_upper[1]

# ...and ICEMAN assessments
iceman <- c("not assessed", "", "", "", "moderate credibility", "", "low credibility", "", "", "", "not assessed", "", "", "", "not assessed", "", "", "not assessed", "")

# build forestplot
base_data <- tibble(mean = subgroup_df$odds_ratio,
                    lower = subgroup_df$ci_lower,
                    upper = subgroup_df$ci_upper,
                    subgroup = as.character(subgroup_df$variable),
                    tot_i = as.character(tot_i),
                    events_i = as.character(events_i),
                    tot_c = as.character(tot_c),
                    events_c = as.character(events_c),
                    p_int = as.character(p_int),
                    iceman = as.character(iceman))
summary <- tibble(mean  = mort_28_OR,
                  lower = mort_28_ci_lower,
                  upper = mort_28_ci_upper,
                  subgroup = "Overall treatment effect",
                  summary = TRUE)
header <- tibble(subgroup = c("Subgroup"),
                 tot_i = c("JAKi"),
                 events_i = c("Events JAKi"),
                 tot_c = c("No JAKi"),
                 events_c = c("Events no JAKi"),
                 p_int = c("p-int*"),
                 iceman = c("ICEMAN\ncredibility assessment"),
                 summary = TRUE)
mort28_fp <- bind_rows(header,base_data,summary)

font <- "sans"

mort28_fp %>%
  forestplot(labeltext = c(subgroup, tot_i, events_i, tot_c, events_c, p_int, iceman),
             txt_gp = fpTxtGp(label = gpar(fontfamily = font, cex=1),
                              ticks = gpar(cex=0.88),
                              summary = gpar(cex=1),
                              xlab = gpar(cex=0.88)),
             # title = "Treatment effect on mortality at day 28 by subgroup",
             is.summary = summary,
             graph.pos = 6,
             clip = c(0.1, 2),
             hrzl_lines = list("2" = gpar(lty = 2),
                               "6" = gpar(lty = 2),
                               "8" = gpar(lty = 2),
                               "12" = gpar(lty = 2),
                               "16" = gpar(lty = 2),
                               "19" = gpar(lty = 2),                               
                               "21" = gpar(lty = 2)),
             xlog = T,
             # xticks = c(1),
             psize = sqrt(subgroup_df$inverse_variance),
             lty.ci = c(1),
             col = fpColors(box = "maroon4",
                            line = "maroon1",
                            summary = "magenta4",
                            hrz_lines = "gray63"),
             vertices = TRUE,
             xlab = "              Favours JAKi < > Favours No JAKi",
             zero = 1,
             grid = structure(c(0.71), gp = gpar(lty = 2, col = "gray63")), # ADAPT if new point estimate!!
             graphwidth = unit(100, "mm"), colgap = unit(2.5, "mm")
             )
```

![](two_stage_files/figure-html/unnamed-chunk-76-1.png)<!-- -->

```r
# ## main forestplot
# mort28_fp_main <- mort28_fp %>% 
#   filter(subgroup %in% c("Subgroup", "No ventilation at baseline", "Ventilation at baseline", 
#                          "70 years of age or older", 
#                          "Below 70 years of age", "No comorbidity", "One comorbidity", "Multiple comorbidities",
#                          "Immunocompromised", "No Dexamethasone, no Tocilizumab", 
#                          "Dexamethasone, but no Tocilizumab",
#                          "Dexamethasone and Tocilizumab", "Tocilizumab, but no Dexamethasone", 
#                          "Overall treatment effect"))
# font <- "sans"
# 
# mort28_fp_main %>%
#   forestplot(labeltext = c(subgroup, tot_i, events_i, tot_c, events_c, p_int, iceman),
#              txt_gp = fpTxtGp(label = gpar(fontfamily = font, cex=1),
#                               ticks = gpar(cex=0.88),
#                               summary = gpar(cex=1),
#                               xlab = gpar(cex=0.88)),
#              # title = "Treatment effect on mortality at day 28 by subgroup",
#              is.summary = summary,
#              graph.pos = 6,
#              clip = c(0.1, 2),
#              hrzl_lines = list("2" = gpar(lty = 2),
#                                "4" = gpar(lty = 2),
#                                "6" = gpar(lty = 2),
#                                "10" = gpar(lty = 2),
#                                "14" = gpar(lty = 2)),
#              xlog = T,
#              # xticks = c(1),
#              psize = sqrt(subgroup_df$inverse_variance),
#              lty.ci = c(1),
#              col = fpColors(box = "maroon4",
#                             line = "maroon1",
#                             summary = "magenta4",
#                             hrz_lines = "gray63"),
#              vertices = TRUE,
#              xlab = "              Favours JAKi < > Favours No JAKi",
#              zero = 1,
#              grid = structure(c(0.71), gp = gpar(lty = 2, col = "gray63")), # ADAPT if new point estimate!!
#              graphwidth = unit(100, "mm"), colgap = unit(2.5, "mm")
#              )
```
* The p-values for the interaction were obtained using a two-stage IPDMA approach, i.e. solely based on within-trial interactions ("deft"): 
First, to produce a treatment-covariate interaction estimate and its variance, a binomial regression was fitted in each trial separately, adjusted (where appropriate) for respiratory support and age, including the treatment and the treatment-covariate interaction, using restricted maximum likelihood estimation (with Firth penalisation correction in case of sparse data). 
Second, the interaction estimates were combined across trials in a random-effect model (the true interactions are assumed random across trials), using restricted maximum likelihood estimation and the confidence interval for the summary interaction derived using the Hartung-Knapp Sidik-Jonkman approach. 
Sizing of all squares are in proportion to the inverse variance of the estimates. 
For continuous covariates (age, symptom duration, and CRP), a cut-off was chosen for descriptive purpose, but these covariates were included as a continuous treatment-covariate interaction assuming linearity. Ordinal covariates (respiratory support, comorbidities and comedication) were included similarly. 

# Forestplot additional subgroups - on primary endpoint

```r
# mort28_fp_additional <- mort28_fp %>% 
#   filter(subgroup %in% c("Subgroup", "No oxygen", "Low-flow oxygen", 
#                          "High-flow or non-invasive ventilation", "Mechanical ventilation or ECMO", 
#                          "Enrolment more than 10 days after symptom onset", 
#                          "Enrolment between 5 and 10 days after symptom onset", 
#                          "Enrolment 5 days or earlier after symptom onset",
#                          "C-reactive protein 75mg/L or more", "C-reactive protein less than 75mg/L"
#                          # "Overall treatment effect"
#                          ))
# font <- "sans"
# 
# mort28_fp_additional %>%
#   forestplot(labeltext = c(subgroup, events_i, tot_i, events_c, tot_c, p_int),
#              txt_gp = fpTxtGp(label = gpar(fontfamily = font, cex=1),
#                               ticks = gpar(cex=0.88),
#                               summary = gpar(cex=1),
#                               xlab = gpar(cex=0.88)),
#              is.summary = summary,
#              graph.pos = 6,
#              clip = c(0.1, 2),
#              hrzl_lines = list("2" = gpar(lty = 2),
#                                "6" = gpar(lty = 2),
#                                "9" = gpar(lty = 2),
#                                "11" = gpar(lty = 2)),
#              xlog = FALSE,
#              xticks = c(0,0.25,0.5,0.75,1,1.25,1.5),
#              psize = sqrt(subgroup_df$inverse_variance),
#              lty.ci = c(1),
#              col = fpColors(box = "maroon4",
#                             line = "maroon1",
#                             summary = "magenta4",
#                             hrz_lines = "gray63"),
#              vertices = TRUE,
#              xlab = "                             Favours JAKi <-> Favours No JAKi",
#              zero = 1,
#              # grid = structure(c(0.71), gp = gpar(lty = 2, col = "gray63")), # ADAPT if new point estimate!!
#              graphwidth = unit(100, "mm"), colgap = unit(2.5, "mm")
#              )
```

# Forestplot subgroup - on safety endpoint

```r
# take the effect estimates from subgroup_df_ae... 
subgroup_df_ae$inverse_variance <- 1 / subgroup_df_ae$standard_error^2
events_i <- subgroup_df_ae$n_intervention
tot_i <- subgroup_df_ae$n_intervention_tot
events_c <- subgroup_df_ae$n_control
tot_c <- subgroup_df_ae$n_control_tot
# ...and p-interaction from interaction_df
interaction_df_fp_ae <- interaction_df %>% 
  select(variable, p_value) %>% 
  filter(variable %in% c("vaccination on AEs", "at risk on AEs", "comedication on AEs"))
empty_row <- data.frame(
  variable = "",
  p_value = "")
first_part <- interaction_df_fp_ae[1:1, ]
second_part <- interaction_df_fp_ae[2:nrow(interaction_df_fp_ae), ]
interaction_df_fp_ae <- rbind(first_part, empty_row, second_part)
first_part <- interaction_df_fp_ae[1:3, ]
second_part <- interaction_df_fp_ae[4:nrow(interaction_df_fp_ae), ]
interaction_df_fp_ae <- rbind(first_part, empty_row, second_part)
interaction_df_fp_ae <- rbind(interaction_df_fp_ae, empty_row, empty_row, empty_row)

p_int <- interaction_df_fp_ae$p_value

# ...and overall results from result_df
ae_28_OR <- result_df$hazard_odds_ratio[17]
ae_28_ci_lower <- result_df$ci_lower[17]
ae_28_ci_upper <- result_df$ci_upper[17]

# ...and ICEMAN assessments
iceman <- c("not assessed", "", "not assessed", "", "not assessed", "", "", "")

# build forestplot
base_data <- tibble(mean = subgroup_df_ae$odds_ratio,
                    lower = subgroup_df_ae$ci_lower,
                    upper = subgroup_df_ae$ci_upper,
                    subgroup = as.character(subgroup_df_ae$variable),
                    events_i = as.character(events_i),
                    tot_i = as.character(tot_i),
                    events_c = as.character(events_c),
                    tot_c = as.character(tot_c),
                    p_int = as.character(p_int),
                    iceman = as.character(iceman))
summary <- tibble(mean  = ae_28_OR,
                  lower = ae_28_ci_lower,
                  upper = ae_28_ci_upper,
                  subgroup = "Overall safety effect",
                  summary = TRUE)
header <- tibble(subgroup = c("Subgroup"),
                 events_i = c("Events int."),
                 tot_i = c("No. int."),
                 events_c = c("Events cont."),
                 tot_c = c("No. cont."),
                 p_int = c("p-int*"),
                 iceman = c("ICEMAN\ncredibility assessment"),
                 summary = TRUE)
ae28_fp <- bind_rows(header,base_data,summary)

ae28_fp_red <- ae28_fp %>% 
  filter(!subgroup %in% c("Overall safety effect"))

# forestplot
font <- "sans"

ae28_fp_red %>%
  forestplot(labeltext = c(subgroup, events_i, tot_i, events_c, tot_c, p_int, iceman),
             txt_gp = fpTxtGp(label = gpar(fontfamily = font, cex=1),
                              ticks = gpar(cex=0.88),
                              summary = gpar(cex=1),
                              xlab = gpar(cex=0.88)),
             is.summary = summary,
             graph.pos = 6,
             clip = c(0.1, 2),
             hrzl_lines = list("2" = gpar(lty = 2),
                               "4" = gpar(lty = 2),
                               "6" = gpar(lty = 2),
                               "10" = gpar(lty = 2)),
             
             xlog = TRUE,
             # xticks = c(1),
             xlim = c(0.5, 1), 
             psize = sqrt(subgroup_df_ae$inverse_variance),
             lty.ci = c(1),
             col = fpColors(box = "maroon4",
                            line = "maroon1",
                            summary = "magenta4",
                            hrz_lines = "gray63"),
             vertices = TRUE,
             xlab = "                                                      Less adverse events with JAKi < > More adverse events with JAKi",
             zero = 1,
             graphwidth = unit(100, "mm"), colgap = unit(2.5, "mm")
             )
```

![](two_stage_files/figure-html/unnamed-chunk-78-1.png)<!-- -->


# AESI

```r
df_aesi_actt2 <- readRDS("df_aesi_actt2.RData")
df_aesi_covbarrier <- readRDS("df_aesi_cov-barrier.RData")
df_aesi_barisolidact <- readRDS("df_aesi_barisolidact.RData")
df_aesi_covinib <- readRDS("df_aesi_covinib.RData")
df_aesi_tofacov <- readRDS("df_aesi_tofacov.RData")
df_aesi_ghazaeian <- readRDS("df_aesi_ghazaeian.RData")
df_aesi_recovery <- readRDS("df_aesi_recovery.RData")
df_aesi_tactic_r <- readRDS("df_aesi_tactic-r.RData")
df_aesi_ruxcovid <- read_excel("/Users/amstutzal/Library/CloudStorage/OneDrive-usb.ch/Dokumente - JAKi IPDMA data source management/General/RUXCOVID/RUXCOVID_desc.xlsx", sheet = "RUXCOVID_AESI")
df_aesi_pancovid <- readRDS("df_aesi_pancovid.RData")

df_aesi_actt2 <- df_aesi_actt2 %>%
  mutate(trial = "ACTT2") %>% 
  select(trial, trt, aesi)
df_aesi_covbarrier <- df_aesi_covbarrier %>%
  mutate(trial = "COV-BARRIER") %>% 
  select(trial, trt, aesi)
df_aesi_barisolidact <- df_aesi_barisolidact %>%
  mutate(trial = "BARI-SOLIDACT") %>% 
  select(trial, trt, aesi)
df_aesi_covinib <- df_aesi_covinib %>%
  mutate(trial = "COVINIB") %>% 
  select(trial, trt, aesi)
df_aesi_tofacov <- df_aesi_tofacov %>%
  mutate(trial = "TOFACOV") %>% 
  select(trial, trt, aesi)
df_aesi_ghazaeian <- df_aesi_ghazaeian %>%
  mutate(trial = "GHAZAEIAN") %>% 
  select(trial, trt, aesi)
df_aesi_recovery <- df_aesi_recovery %>%
  mutate(trial = "RECOVERY") %>% 
  select(trial, trt, aesi)
df_aesi_tactic_r <- df_aesi_tactic_r %>%
  mutate(trial = "TACTIC-R") %>% 
  select(trial, trt, aesi)
df_aesi_ruxcovid <- df_aesi_ruxcovid %>%
  mutate(trial = "RUXCOVID") %>% 
  select(trial, trt, aesi)
df_aesi_pancovid <- df_aesi_pancovid %>%
  mutate(trial = "RUXCOVID") %>% 
  select(trial, trt, aesi)

df_aesi_tot <- rbind(df_aesi_actt2, df_aesi_covbarrier, df_aesi_barisolidact, df_aesi_covinib, df_aesi_tofacov, df_aesi_ghazaeian, df_aesi_recovery, df_aesi_tactic_r, df_aesi_ruxcovid, df_aesi_pancovid) # ADD NEW TRIALS

# round(prop.table(table(df_aesi_tot$aesi, df_aesi_tot$trt),2)*100,0)
# addmargins(table(df_aesi_tot$aesi, df_aesi_tot$trt))

df_aesi_tot <- df_aesi_tot %>% 
  mutate(Group = case_when(trt == 0 ~ "No JAK inhibitor",
                         trt == 1 ~ "JAK inhibitor")) %>% 
  mutate(aesi = case_when(aesi == "mods" ~ "Multiple organ dysfunction syndrome",
                          aesi == "hepatox" ~ "Liver dysfunction",
                          aesi == "git_bl" ~ "Gastrointestinal perforation",
                          aesi == "malig" ~ "Malignancy",
                          aesi == "penia" ~ "Bone marrow suppression",
                          aesi == "cardiac" ~ "Cardiovascular and cardiac events",
                          aesi == "reactivate" ~ "Reactivation of chronic infection",
                          aesi == "sec_inf" ~ "Secondary infections",
                          aesi == "thrombo" ~ "Thromboembolic events",))

proportions <- df_aesi_tot %>%
  drop_na(Group) %>% 
  group_by(Group, aesi) %>%
  summarise(count = n()) %>%
  group_by(Group) %>%
  # mutate(proportion = (count / sum(count))*100) %>%
  mutate(denom = case_when(Group == "No JAK inhibitor" ~ 6063, #### ADAPT if NEW TRIALS!
                           Group == "JAK inhibitor" ~ 6339)) %>%  #### ADAPT if NEW TRIALS!
  mutate(proportion = (count / denom)*100)
ggplot(proportions, aes(x = proportion, y = aesi, color = Group, shape = Group)) +
  geom_point(size = 2.5) +
  labs(x = "Proportion (%), by treatment group", y = NULL) +
  theme_minimal() + 
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  theme(axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 9))
```

![](two_stage_files/figure-html/unnamed-chunk-79-1.png)<!-- -->

# AE

```r
# df_ae_actt2 <- readRDS("df_ae_actt2.RData")
# df_ae_covbarrier <- readRDS("df_ae_cov-barrier.RData")
# df_ae_barisolidact <- readRDS("df_ae_barisolidact.RData")
# df_ae_covinib <- readRDS("df_ae_covinib.RData")
# df_ae_tofacov <- readRDS("df_ae_tofacov.RData")
# df_ae_ghazaeian <- readRDS("df_ae_ghazaeian.RData")
# df_ae_recovery <- readRDS("df_ae_recovery.RData")
# df_ae_tactic_r <- readRDS("df_ae_tactic-r.RData")
# # df_ae_ruxcovid <- readRDS("df_aesi_ruxcovid.RData") # export from Virtual Desktop!
# df_ae_pancovid <- readRDS("df_ae_pancovid.RData")
# 
# df_ae_actt2 <- df_ae_actt2 %>%
#   mutate(trial = "ACTT2") %>% 
#   select(trial, trt, ae)
# df_ae_covbarrier <- df_ae_covbarrier %>%
#   mutate(trial = "COV-BARRIER") %>% 
#   select(trial, trt, ae)
# df_ae_barisolidact <- df_ae_barisolidact %>%
#   mutate(trial = "BARI-SOLIDACT") %>% 
#   rename(ae = MeddraPT) %>% 
#   select(trial, trt, ae)
# df_ae_covinib <- df_ae_covinib %>%
#   mutate(trial = "COVINIB") %>% 
#   select(trial, trt, ae)
# df_ae_tofacov <- df_ae_tofacov %>%
#   mutate(trial = "TOFACOV") %>% 
#   select(trial, trt, ae)
# df_ae_ghazaeian <- df_ae_ghazaeian %>%
#   mutate(trial = "GHAZAEIAN") %>% 
#   select(trial, trt, ae)
# df_ae_recovery <- df_ae_recovery %>%
#   mutate(trial = "RECOVERY") %>% 
#   select(trial, trt, ae)
# df_ae_tactic_r <- df_ae_tactic_r %>%
#   mutate(trial = "TACTIC-R") %>% 
#   select(trial, trt, ae)
# df_ae_pancovid <- df_ae_pancovid %>%
#   mutate(trial = "PANCOVID") %>%
#   rename(ae = AETerm) %>% 
#   select(trial, trt, ae)
# 
# df_ae_tot <- rbind(df_ae_actt2, df_ae_covbarrier, df_ae_barisolidact, df_ae_covinib, df_ae_tofacov, df_ae_ghazaeian, df_ae_recovery, df_ae_tactic_r, df_ae_pancovid)
# 
# # round(prop.table(table(df_ae_tot$ae, df_ae_tot$trt),2)*100,0)
# # addmargins(table(df_ae_tot$ae, df_ae_tot$trt))
# 
# ## GROUP the AEs
# # unique(df_ae_tot$ae)
# 
# df_ae_tot <- df_ae_tot %>% 
#   mutate(ARM = case_when(trt == 0 ~ "No JAK inhibitor",
#                          trt == 1 ~ "JAK inhibitor"))
# proportions <- df_ae_tot %>%
#   drop_na(ARM) %>% 
#   group_by(ARM, ae) %>%
#   summarise(count = n()) %>%
#   group_by(ARM) %>%
#   mutate(proportion = (count / sum(count))*100) %>% 
#   mutate("Proportion (%)" = round(proportion,0))
# ggplot(proportions, aes(x = proportion, y = ae, color = ARM, shape = ARM)) +
#   geom_point() +
#   labs(x = "Proportion (%) across all ae by ARM", y = "ae Category") +
#   theme_minimal()
```
