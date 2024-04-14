---
title: "TACTIC-R"
author: "A.Amstutz"
date: "2024-04-12"
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
library(haven) # Read sas files
library(here)
library(kableExtra)

library(jtools) # for summ() and plot_summs
library(sjPlot) # for tab_model
library(ggplot2) # survival/TTE analyses and other graphs
library(ggsurvfit) # survival/TTE analyses
library(survival) # survival/TTE analyses
library(gtsummary) # survival/TTE analyses
library(ggfortify) # autoplot
library(tidycmprsk) # competing risk analysis
library(ordinal) # clinstatus ordinal regression
library(logistf) # Firth regression in case of rare events

library(finalfit) # missing data exploration
library(mice) # multiple imputation
library(jomo) # multiple imputation
library(mitools) # multiple imputation
```

# Load Data


# Baseline Characteristics

```r
# dataframe 2, only data from those who were randomised, but long format, for each form/entry/record
df2 <- df2 %>% rename(id_pat = SUBJID_DER)
df2 <- df2 %>% # fill up NA
  fill(id_pat)

# dataframe 1, only with those who were randomised (n=282), but wide format (282 rows)
df <- df %>% rename(id_pat = SUBJID)
df <- df %>% # randomization variable, see codebook
  mutate(trt = case_when(RANDOM_TREAT == 2 ~ 1,
                         RANDOM_TREAT == 3 ~ 0))
df$trial <- "TACTIC-R"
df$JAKi <- "Baricitinib"
df$country <- "UK"
df$randdate <- as.Date(df$RANDOM_DATE, format = "%d/%m/%Y") # randomisation date

# Sex
df <- df %>% # Corresponds to publication
  mutate(sex = case_when(DEMO_SEX == 1 ~ "male",
                         DEMO_SEX == 2 ~ "female")) %>% # if missing, use info from risk score
  mutate(sex = case_when(is.na(sex) & RISK_MALEGEN == 1 ~ "male",
                         is.na(sex) & RISK_MALEGEN == 0 ~ "female",
                         TRUE ~ sex))
# Ethnicity
df <- df %>% # Corresponds to publication
  mutate(ethn = case_when(DEMO_ETHNIC == 1 ~ "White",
                          DEMO_ETHNIC == 2 ~ "Mixed",
                          DEMO_ETHNIC == 3 ~ "Asian or Asian British",
                          DEMO_ETHNIC == 4 ~ "Black or Black British",
                          DEMO_ETHNIC == 5 ~ "Other")) %>% # if missing, use info from risk score
  mutate(ethn = case_when(is.na(ethn) & RISK_NONWHITE == 0 ~ "White", 
                          TRUE ~ ethn)) %>% # if still missing, use info from DEMO_ETHNIC1
  mutate(ethn = case_when(is.na(ethn) & DEMO_ETHNIC1 == 1 ~ "White",
                          is.na(ethn) & DEMO_ETHNIC1 == 2 ~ "Mixed",
                          is.na(ethn) & DEMO_ETHNIC1 == 3 ~ "Asian or Asian British",
                          is.na(ethn) & DEMO_ETHNIC1 == 4 ~ "Black or Black British",
                          is.na(ethn) & DEMO_ETHNIC1 == 5 ~ "Other",
                          TRUE ~ ethn))

# AGE
df <- df %>% # Corresponds to publication
  mutate(age = as.numeric(df$DEMO_AGE)) %>% # if missing, use info from derived age
  mutate(age = case_when(is.na(age) & !is.na(DERIVE_AGE) ~ as.numeric(DERIVE_AGE), 
                          TRUE ~ age))
# df %>% 
#   filter(trt == 1) %>%
#   select(age) %>% 
#   summary()
df %>%
  ggplot(aes(x = age)) +
  geom_density(fill = "blue", color = "black") +
  labs(title = "Density Plot of Age",
       x = "Age",
       y = "Density")
```

![](TACTIC-R_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
### Clinical score at baseline

# POINT7_OS_POS	1	Death
# POINT7_OS_POS	2	Invasive mechanical ventilation
# POINT7_OS_POS	3	Non-invasive ventilation or high flow oxygen
# POINT7_OS_POS	4	Low flow oxygen
# POINT7_OS_POS	5	Hospitalised, no oxygen
# POINT7_OS_POS	6	Discharged, normal activities not resumed
# POINT7_OS_POS	7	Discharged, normal activities resumed

# double-check with the following for the follow-up scores since always in parallel (not for baseline, since this was collected during pre-screening, while the ordinal score was at randomization):

# RESP_DELIVER	1	Nasal cannula
# RESP_DELIVER	2	Venturi mask
# RESP_DELIVER	3	Ambu or rebreathing bag
# RESP_DELIVER	4	Non-invasive ventilation
# RESP_DELIVER	5	Continuous positive airway pressure (CPAP)
# RESP_DELIVER	6	Extracorporeal membrane oxygenation (ECMO)
# RESP_DELIVER	7	Invasive mechanical ventilation

# directly translatable into our score:
df <- df %>% # Corresponds to publication
  mutate(clinstatus_baseline = case_when(POINT7_OS_POS == 1 ~ 6,
                                         POINT7_OS_POS == 2 ~ 5,
                                         POINT7_OS_POS == 3 ~ 4,
                                         POINT7_OS_POS == 4 ~ 3,
                                         POINT7_OS_POS == 5 ~ 2,
                                         POINT7_OS_POS == 6 ~ 1,
                                         POINT7_OS_POS == 7 ~ 1))

### Co-medication at baseline
# REMDES_TRT_ENR & REMDES_TRT_ENR1 do not help // according to NIH guidelines:

# If dexamethasone is not available, alternative corticosteroids (e.g., prednisone, methylprednisolone, hydrocortisone) can be used, For these drugs, the total daily dose equivalencies to dexamethasone 6 mg (orally or intravenously) are:
# Prednisone 40 mg
# Methylprednisolone 32 mg
# Hydrocortisone 160 mg

# cortico
df_dexa <- df2 %>%
  filter(grepl("dexa|cort|glucoc|hydrocor|predn|solon|methyl", CM_NAME, ignore.case = TRUE) &
         !grepl("Dexafree|fludro|symb|pul|dakta|penici", CM_NAME, ignore.case = TRUE)) # exclude Dexafree, fludrocortisone, symbi/pulmicort, daktacort, Phenoxymethylpenicillin # exclude Solu-Medrol
df_dexa <- left_join(df_dexa, df[, c("randdate", "trt", "id_pat")], by = join_by(id_pat == id_pat)) # add randdate

# df_dexa %>% 
#   select(id_pat, SUBJID_DER1, SUBJID_DER2, SUBJID_DER3, CM_NAME, CM_DOSE, CMDOSU, CM_FREQ, CM_FREQ_OTH, CM_ROUTE, CMROUTEOTH, CM_START, CM_END, randdate, trt) %>% 
#   View()
# df_dexa <- df_dexa %>%
#   distinct(id_pat)
```
Discussion points:
