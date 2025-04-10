---
title: "descriptive"
author: "A.Amstutz"
date: "2023-12-29"
output:
  html_document:
    keep_md: true
    toc: true
    toc_float: true
    code_folding: hide
  pdf_document:
    toc: true
  word_document:
    toc: true
---

# Load packages

```r
library(tidyverse)
library(readxl)
library(writexl)
library(tableone)
library(data.table)
library(here)
library(kableExtra)
```

# Load standardized dataset of all trials

```r
## barisolidact
df_barisolidact <- readRDS("df_os_barisolidact.RData")
df_barisolidact <- df_barisolidact %>% 
    select(id_pat, trial, JAKi, trt, sex, age, ethn, country, icu, sympdur, vacc, clinstatus_baseline, vbaseline,
         comed_dexa, comed_rdv, comed_toci, comed_ab, comed_acoa, comed_interferon, comed_other,
         comed_cat,
         comorb_lung, comorb_liver, comorb_cvd, comorb_aht, comorb_dm, comorb_obese, comorb_smoker, immunosupp,
         comorb_autoimm, comorb_cancer, comorb_kidney, any_comorb, comorb_cat, comorb_any, comorb_count,
         crp, sero, vl_baseline, variant,
         mort_28, mort_28_dimp, mort_60, death_reached, death_time,
         new_mv_28, new_mvd_28,
         clinstatus_28_imp,
         discharge_reached, discharge_time, discharge_time_sens, discharge_reached_sus, discharge_time_sus,
         ae_28, ae_28_sev,
         vir_clear_5, vir_clear_10, vir_clear_15)
## actt2
df_actt2 <- readRDS("df_os_actt2.RData")
df_actt2 <- df_actt2 %>% 
    select(id_pat, trial, JAKi, trt, sex, age, ethn, country, icu, sympdur, vacc, clinstatus_baseline, vbaseline,
         comed_dexa, comed_rdv, comed_toci, comed_ab, comed_acoa, comed_interferon, comed_other,
         comed_cat,
         comorb_lung, comorb_liver, comorb_cvd, comorb_aht, comorb_dm, comorb_obese, comorb_smoker, immunosupp,
         comorb_autoimm, comorb_cancer, comorb_kidney, any_comorb, comorb_cat, comorb_any, comorb_count,
         crp, sero, vl_baseline, variant,
         mort_28, mort_28_dimp, mort_60, death_reached, death_time,
         new_mv_28, new_mvd_28,
         clinstatus_28_imp,
         discharge_reached, discharge_time, discharge_time_sens, discharge_reached_sus, discharge_time_sus,
         ae_28, ae_28_sev,
         vir_clear_5, vir_clear_10, vir_clear_15)
## ghazaeian
df_ghazaeian <- readRDS("df_os_ghazaeian.RData")
df_ghazaeian <- df_ghazaeian %>% 
    select(id_pat, trial, JAKi, trt, sex, age, ethn, country, icu, sympdur, vacc, clinstatus_baseline, vbaseline,
         comed_dexa, comed_rdv, comed_toci, comed_ab, comed_acoa, comed_interferon, comed_other,
         comed_cat,
         comorb_lung, comorb_liver, comorb_cvd, comorb_aht, comorb_dm, comorb_obese, comorb_smoker, immunosupp,
         comorb_autoimm, comorb_cancer, comorb_kidney, any_comorb, comorb_cat, comorb_any, comorb_count,
         crp, sero, vl_baseline, variant,
         mort_28, mort_28_dimp, mort_60, death_reached, death_time,
         new_mv_28, new_mvd_28,
         clinstatus_28_imp,
         discharge_reached, discharge_time, discharge_time_sens, discharge_reached_sus, discharge_time_sus,
         ae_28, ae_28_sev,
         vir_clear_5, vir_clear_10, vir_clear_15)
## tofacov
df_tofacov <- readRDS("df_os_tofacov.RData")
df_tofacov <- df_tofacov %>% 
    select(id_pat, trial, JAKi, trt, sex, age, ethn, country, icu, sympdur, vacc, clinstatus_baseline, vbaseline,
         comed_dexa, comed_rdv, comed_toci, comed_ab, comed_acoa, comed_interferon, comed_other,
         comed_cat,
         comorb_lung, comorb_liver, comorb_cvd, comorb_aht, comorb_dm, comorb_obese, comorb_smoker, immunosupp,
         comorb_autoimm, comorb_cancer, comorb_kidney, any_comorb, comorb_cat, comorb_any, comorb_count,
         crp, sero, vl_baseline, variant,
         mort_28, mort_28_dimp, mort_60, death_reached, death_time,
         new_mv_28, new_mvd_28,
         clinstatus_28_imp,
         discharge_reached, discharge_time, discharge_time_sens, discharge_reached_sus, discharge_time_sus,
         ae_28, ae_28_sev,
         vir_clear_5, vir_clear_10, vir_clear_15)
## covinib
df_covinib <- readRDS("df_os_covinib.RData")
df_covinib <- df_covinib %>% 
    select(id_pat, trial, JAKi, trt, sex, age, ethn, country, icu, sympdur, vacc, clinstatus_baseline, vbaseline,
         comed_dexa, comed_rdv, comed_toci, comed_ab, comed_acoa, comed_interferon, comed_other,
         comed_cat,
         comorb_lung, comorb_liver, comorb_cvd, comorb_aht, comorb_dm, comorb_obese, comorb_smoker, immunosupp,
         comorb_autoimm, comorb_cancer, comorb_kidney, any_comorb, comorb_cat, comorb_any, comorb_count,
         crp, sero, vl_baseline, variant,
         mort_28, mort_28_dimp, mort_60, death_reached, death_time,
         new_mv_28, new_mvd_28,
         clinstatus_28_imp,
         discharge_reached, discharge_time, discharge_time_sens, discharge_reached_sus, discharge_time_sus,
         ae_28, ae_28_sev,
         vir_clear_5, vir_clear_10, vir_clear_15)
## COV-BARRIER
df_covbarrier <- readRDS("df_os_cov-barrier.RData")
df_covbarrier <- df_covbarrier %>% 
    select(id_pat, trial, JAKi, trt, sex, age, ethn, country, icu, sympdur, vacc, clinstatus_baseline, vbaseline,
         comed_dexa, comed_rdv, comed_toci, comed_ab, comed_acoa, comed_interferon, comed_other,
         comed_cat,
         comorb_lung, comorb_liver, comorb_cvd, comorb_aht, comorb_dm, comorb_obese, comorb_smoker, immunosupp,
         comorb_autoimm, comorb_cancer, comorb_kidney, any_comorb, comorb_cat, comorb_any, comorb_count,
         crp, sero, vl_baseline, variant,
         mort_28, mort_28_dimp, mort_60, death_reached, death_time,
         new_mv_28, new_mvd_28,
         clinstatus_28_imp,
         discharge_reached, discharge_time, discharge_time_sens, discharge_reached_sus, discharge_time_sus,
         ae_28, ae_28_sev,
         vir_clear_5, vir_clear_10, vir_clear_15)
## Murugesan
df_murugesan <- readRDS("df_os_murugesan.RData")
df_murugesan <- df_murugesan %>% 
    select(id_pat, trial, JAKi, trt, sex, age, ethn, country, icu, sympdur, vacc, clinstatus_baseline, vbaseline,
         comed_dexa, comed_rdv, comed_toci, comed_ab, comed_acoa, comed_interferon, comed_other,
         comed_cat,
         comorb_lung, comorb_liver, comorb_cvd, comorb_aht, comorb_dm, comorb_obese, comorb_smoker, immunosupp,
         comorb_autoimm, comorb_cancer, comorb_kidney, any_comorb, comorb_cat, comorb_any, comorb_count,
         crp, sero, vl_baseline, variant,
         mort_28, mort_28_dimp, mort_60, death_reached, death_time,
         new_mv_28, new_mvd_28,
         clinstatus_28_imp,
         discharge_reached, discharge_time, discharge_time_sens, discharge_reached_sus, discharge_time_sus,
         ae_28, ae_28_sev,
         vir_clear_5, vir_clear_10, vir_clear_15)
## RECOVERY
df_recovery <- readRDS("df_os_recovery.RData")
df_recovery <- df_recovery %>% 
    select(id_pat, trial, JAKi, trt, sex, age, ethn, country, icu, sympdur, vacc, clinstatus_baseline, vbaseline,
         comed_dexa, comed_rdv, comed_toci, comed_ab, comed_acoa, comed_interferon, comed_other,
         comed_cat,
         comorb_lung, comorb_liver, comorb_cvd, comorb_aht, comorb_dm, comorb_obese, comorb_smoker, immunosupp,
         comorb_autoimm, comorb_cancer, comorb_kidney, any_comorb, comorb_cat, comorb_any, comorb_count,
         crp, sero, vl_baseline, variant,
         mort_28, mort_28_dimp, mort_60, death_reached, death_time,
         new_mv_28, new_mvd_28,
         clinstatus_28_imp,
         discharge_reached, discharge_time, discharge_time_sens, discharge_reached_sus, discharge_time_sus,
         ae_28, ae_28_sev,
         vir_clear_5, vir_clear_10, vir_clear_15)
## TACTIC-R
df_tactic_r <- readRDS("df_os_tactic-r.RData")
df_tactic_r <- df_tactic_r %>% 
    select(id_pat, trial, JAKi, trt, sex, age, ethn, country, icu, sympdur, vacc, clinstatus_baseline, vbaseline,
         comed_dexa, comed_rdv, comed_toci, comed_ab, comed_acoa, comed_interferon, comed_other,
         comed_cat,
         comorb_lung, comorb_liver, comorb_cvd, comorb_aht, comorb_dm, comorb_obese, comorb_smoker, immunosupp,
         comorb_autoimm, comorb_cancer, comorb_kidney, any_comorb, comorb_cat, comorb_any, comorb_count,
         crp, sero, vl_baseline, variant,
         mort_28, mort_28_dimp, mort_60, death_reached, death_time,
         new_mv_28, new_mvd_28,
         clinstatus_28_imp,
         discharge_reached, discharge_time, discharge_time_sens, discharge_reached_sus, discharge_time_sus,
         ae_28, ae_28_sev,
         vir_clear_5, vir_clear_10, vir_clear_15)

## RUXCOVID
df_ruxcovid <- read_excel("/Users/amstutzal/Library/CloudStorage/OneDrive-usb.ch/Dokumente - JAKi IPDMA data source management/General/RUXCOVID/RUXCOVID_desc.xlsx", sheet = "RUXCOVID")
df_tactic_r <- df_tactic_r %>% 
    select(id_pat, trial, JAKi, trt, sex, age, ethn, country, icu, sympdur, vacc, clinstatus_baseline, vbaseline,
         comed_dexa, comed_rdv, comed_toci, comed_ab, comed_acoa, comed_interferon, comed_other,
         comed_cat,
         comorb_lung, comorb_liver, comorb_cvd, comorb_aht, comorb_dm, comorb_obese, comorb_smoker, immunosupp,
         comorb_autoimm, comorb_cancer, comorb_kidney, any_comorb, comorb_cat, comorb_any, comorb_count,
         crp, sero, vl_baseline, variant,
         mort_28, mort_28_dimp, mort_60, death_reached, death_time,
         new_mv_28, new_mvd_28,
         clinstatus_28_imp,
         discharge_reached, discharge_time, discharge_time_sens, discharge_reached_sus, discharge_time_sus,
         ae_28, ae_28_sev,
         vir_clear_5, vir_clear_10, vir_clear_15)

## PANCOVID
df_pancovid <- readRDS("df_os_pancovid.RData")
df_pancovid <- df_pancovid %>% 
    select(id_pat, trial, JAKi, trt, sex, age, ethn, country, icu, sympdur, vacc, clinstatus_baseline, vbaseline,
         comed_dexa, comed_rdv, comed_toci, comed_ab, comed_acoa, comed_interferon, comed_other,
         comed_cat,
         comorb_lung, comorb_liver, comorb_cvd, comorb_aht, comorb_dm, comorb_obese, comorb_smoker, immunosupp,
         comorb_autoimm, comorb_cancer, comorb_kidney, any_comorb, comorb_cat, comorb_any, comorb_count,
         crp, sero, vl_baseline, variant,
         mort_28, mort_28_dimp, mort_60, death_reached, death_time,
         new_mv_28, new_mvd_28,
         clinstatus_28_imp,
         discharge_reached, discharge_time, discharge_time_sens, discharge_reached_sus, discharge_time_sus,
         ae_28, ae_28_sev,
         vir_clear_5, vir_clear_10, vir_clear_15)

## RUXCOVID-DEVENT
df_ruxcoviddevent <- readRDS("/Users/amstutzal/Library/CloudStorage/OneDrive-usb.ch/Dokumente - JAKi IPDMA data source management/General/RUXCOVID-DEVENT/df_os_ruxcoviddevent.RData")
df_ruxcoviddevent <- df_ruxcoviddevent %>% 
    select(id_pat, trial, JAKi, trt, sex, age, ethn, country, icu, sympdur, vacc, clinstatus_baseline, vbaseline,
         comed_dexa, comed_rdv, comed_toci, comed_ab, comed_acoa, comed_interferon, comed_other,
         comed_cat,
         comorb_lung, comorb_liver, comorb_cvd, comorb_aht, comorb_dm, comorb_obese, comorb_smoker, immunosupp,
         comorb_autoimm, comorb_cancer, comorb_kidney, any_comorb, comorb_cat, comorb_any, comorb_count,
         crp, sero, vl_baseline, variant,
         mort_28, mort_28_dimp, mort_60, death_reached, death_time,
         new_mv_28, new_mvd_28,
         clinstatus_28_imp,
         discharge_reached, discharge_time, discharge_time_sens, discharge_reached_sus, discharge_time_sus,
         ae_28, ae_28_sev,
         vir_clear_5, vir_clear_10, vir_clear_15)

## STOP-COVID
df_stopcovid <- readRDS("df_os_stopcovid.RData")
df_stopcovid <- df_stopcovid %>% 
    select(id_pat, trial, JAKi, trt, sex, age, ethn, country, icu, sympdur, vacc, clinstatus_baseline, vbaseline,
         comed_dexa, comed_rdv, comed_toci, comed_ab, comed_acoa, comed_interferon, comed_other,
         comed_cat,
         comorb_lung, comorb_liver, comorb_cvd, comorb_aht, comorb_dm, comorb_obese, comorb_smoker, immunosupp,
         comorb_autoimm, comorb_cancer, comorb_kidney, any_comorb, comorb_cat, comorb_any, comorb_count,
         crp, sero, vl_baseline, variant,
         mort_28, mort_28_dimp, mort_60, death_reached, death_time,
         new_mv_28, new_mvd_28,
         clinstatus_28_imp,
         discharge_reached, discharge_time, discharge_time_sens, discharge_reached_sus, discharge_time_sus,
         ae_28, ae_28_sev,
         vir_clear_5, vir_clear_10, vir_clear_15)

# append
df_tot <- rbind(df_barisolidact, df_actt2, df_ghazaeian, df_tofacov, df_covinib, df_covbarrier, df_recovery, df_tactic_r, df_ruxcovid, df_pancovid, df_ruxcoviddevent, df_stopcovid)
# save
saveRDS(df_tot, file = "df_tot_rux.RData")
```

## Baseline characteristics, reformatting

```r
vars.list <- c("trt", "trial", "age", "sex", "ethn", "country", "vacc", "sympdur", "icu", "clinstatus_baseline", "comorb_cat", "comed_cat", "comed_rdv", "crp", "sero", "vl_baseline")

df_baseline <- df_tot[,colnames(df_tot)%in%vars.list]
df_baseline <- df_baseline[,match(vars.list,colnames(df_baseline))]

colnames(df_baseline) <- vars.list <- c("ARM","Trial","Age, years", "Sex", "Ethnicity", "Country", "Vaccination", "Time from symptom onset to randomisation, days", "Patients admitted to intensive care unit","Clinical status on ordinal scale", "Comorbidities", "Dexamethasone and Tocilizumab", "Remdesivir","C-reactive protein concentration, mg/L", "Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N])", "Patients with undetectable viral load")

char_vars <- c("Trial","Sex", "Ethnicity", "Country", "Vaccination","Patients admitted to intensive care unit","Clinical status on ordinal scale","Comorbidities", "Dexamethasone and Tocilizumab", "Remdesivir","Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N])", "Patients with undetectable viral load")

# Convert character variables to factors
df_baseline <- df_baseline %>%
  mutate(across(all_of(char_vars), factor))


# Sex
# addmargins(table(df_baseline$Sex, df_baseline$Trial))
df_baseline <- df_baseline %>%
  mutate(Sex = case_when(Sex == "male" | Sex == "M" | Sex == "2" ~ "Male",
                         Sex == "female" | Sex == "F" | Sex == "1" ~ "Female"))

# Ethnicity
# addmargins(table(df_baseline$Ethnicity, df_baseline$Trial))
# unique(df_baseline$Ethnicity)
df_baseline <- df_baseline %>%
  mutate(Ethnicity = case_when(Ethnicity == "AMERICAN INDIAN OR ALASKA NATIVE" ~ "American Indian or Alaska Native",
                             Ethnicity == "BLACK OR AFRICAN AMERICAN" | Ethnicity == "Black or Black British" ~ "Black or African American",
                             Ethnicity == "Asian" | Ethnicity == "ASIAN" | Ethnicity == "Asian or Asian British" ~ "Asian",
                             Ethnicity == "caucasian" | Ethnicity == "White" | Ethnicity == "WHITE" ~ "Caucasian",
                             Ethnicity == "HISPANIC OR LATINO" | Ethnicity == "Latino" ~ "Hispanic or Latino",
                             Ethnicity == "MIXED" | Ethnicity == "Mixed" | Ethnicity == "MULTIPLE" ~ "Mixed",
                             Ethnicity == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ "Native Hawaiian or other Pacific Islander",
                             Ethnicity == "Persian/Mazani" ~ "Persian/Mazani"))


# Country
# table(df_baseline$Country, useNA = "always")
# addmargins(table(df_baseline$Country, df_baseline$Trial))
df_baseline <- df_baseline %>%
  mutate(Country = case_when(Country == "NORWAY" ~ "Norway",
                             Country == "FRANCE" ~ "France",
                             Country == "ITA" | Country == "ITALY" | Country == "Italy" ~ "Italy",
                             Country == "Iran" ~ "Iran",
                             Country == "GERMANY" | Country == "DEU" ~ "Germany",
                             Country == "SPAIN" | Country == "ESP" | Country == "Spain" ~ "Spain",
                             Country == "BELGIUM" ~ "Belgium",
                             Country == "PORTUGAL" ~ "Portugal",
                             Country == "IRELAND" ~ "Ireland",
                             Country == "LUXEMBOURG" ~ "Luxembourg",
                             Country == "AUSTRIA" ~ "Austria",
                             Country == "GBR" | Country == "UK" ~ "United Kingdom",
                             Country == "PORTUGAL" ~ "Portugal",
                             Country == "MEX" ~ "Mexico",
                             Country == "ARG" ~ "Argentina",
                             Country == "BRA" | Country == "Brazil" ~ "Brazil",
                             Country == "RUS" ~ "Russia",
                             Country == "JPN" ~ "Japan",
                             Country == "KOR" ~ "South Korea",
                             Country == "IND" ~ "India",
                             Country == "USA" ~ "USA",
                             Country == "Asia" ~ "Asia*",
                             Country == "Europe" ~ "Europe*",
                             Country == "North America" ~ "North America*"
                             ))

# Vaccination
# addmargins(table(df_baseline$Vaccination, df_baseline$Trial))
df_baseline <- df_baseline %>%
  mutate(Vaccination = case_when(Vaccination == "1" ~ "Any SARS CoV-2 vaccine",
                                Vaccination == "0" ~ "No SARS CoV-2 vaccine"))
# ICU
# addmargins(table(df_baseline$`Patients admitted to intensive care unit`, df_baseline$Trial))
df_baseline <- df_baseline %>%
  mutate(`Patients admitted to intensive care unit` = case_when(`Patients admitted to intensive care unit` == "1" ~ "Yes",
                                                                `Patients admitted to intensive care unit` == "0" ~ "No"))
# Comorbidities
# addmargins(table(df_baseline$Comorbidities, df_baseline$Trial))
df_baseline <- df_baseline %>%
  mutate(Comorbidities = case_when(Comorbidities == "1" ~ "No comorbidity",
                                   Comorbidities == "2" ~ "One comorbidity",
                                   Comorbidities == "3" ~ "Multiple comorbidities",
                                   Comorbidities == "4" ~ "Immunocompromised"))
df_baseline$Comorbidities <- ordered(df_baseline$Comorbidities,
                                  levels = c("No comorbidity", "One comorbidity",
                                             "Multiple comorbidities", "Immunocompromised"))
# Dexamethasone and Tocilizumab
# addmargins(table(df_baseline$`Dexamethasone and Tocilizumab`, df_baseline$Trial))
df_baseline <- df_baseline %>%
  mutate(`Dexamethasone and Tocilizumab` = case_when(`Dexamethasone and Tocilizumab` == "1" ~ "No Dexamethasone, no Tocilizumab",
                                   `Dexamethasone and Tocilizumab` == "2" ~ "Dexamethasone and Tocilizumab",
                                   `Dexamethasone and Tocilizumab` == "3" ~ "Dexamethasone but no Tocilizumab",
                                  `Dexamethasone and Tocilizumab` == "4" ~ "Tocolizumab but no Dexamethasone"))
df_baseline$`Dexamethasone and Tocilizumab` <- ordered(df_baseline$`Dexamethasone and Tocilizumab`,
                                  levels = c("No Dexamethasone, no Tocilizumab", "Dexamethasone but no Tocilizumab",
                                             "Dexamethasone and Tocilizumab", "Tocolizumab but no Dexamethasone"))

# Clinstatus_baseline
# addmargins(table(df_baseline$`Clinical status on ordinal scale`, df_baseline$Trial))
df_baseline <- df_baseline %>%
  mutate(`Clinical status on ordinal scale` = case_when(`Clinical status on ordinal scale` == "2" ~ "2: Hospitalised without need for oxygen therapy (WHO score 4)",
                                   `Clinical status on ordinal scale` == "3" ~ "3: Hospitalised with need for supplemental low-flow oxygen (WHO score 5)",
                                   `Clinical status on ordinal scale` == "4" ~ "4: Hospitalised with need for high-flow oxygen or non- invasive ventilation (WHO score 6)",
                                  `Clinical status on ordinal scale` == "5" ~ "5: Hospitalised with need for mechanical ventilation or ECMO (WHO score 7–9)"))

df_baseline$`Clinical status on ordinal scale` <- ordered(df_baseline$`Clinical status on ordinal scale`,
                                  levels = c("2: Hospitalised without need for oxygen therapy (WHO score 4)", "3: Hospitalised with need for supplemental low-flow oxygen (WHO score 5)", "4: Hospitalised with need for high-flow oxygen or non- invasive ventilation (WHO score 6)", "5: Hospitalised with need for mechanical ventilation or ECMO (WHO score 7–9)"))

# Remdesivir
# addmargins(table(df_baseline$Remdesivir, df_baseline$Trial))
df_baseline <- df_baseline %>%
  mutate(Remdesivir = case_when(Remdesivir == "1" ~ "Remdesivir",
                                Remdesivir == "0" ~ "No Remdesivir"))
# addmargins(table(df_baseline$Remdesivir, df_baseline$Trial, df_baseline$ARM))

# Seroconversion
# addmargins(table(df_baseline$`Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N])`, df_baseline$Trial))
df_baseline <- df_baseline %>%
  mutate(`Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N])` = case_when(`Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N])` == "1" ~ "Seroconverted",
                                `Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N])` == "0" ~ "Not seroconverted"))
# Viremia
# addmargins(table(df_baseline$`Patients with undetectable viral load`, df_baseline$Trial))
df_baseline <- df_baseline %>%
  mutate(`Patients with undetectable viral load` = case_when(`Patients with undetectable viral load` == "1" ~ "Undetectable viral load",
                                `Patients with undetectable viral load` == "0" ~ "Detectable viral load"))

# Replace the strata variable labels
# addmargins(table(df_baseline$ARM, df_baseline$Trial))
df_baseline$ARM <- ifelse(df_baseline$ARM == 0, "No JAK inhibitor", "JAK inhibitor")

# take out Age of RUXCOVID-DEVENT since it is binary, but do not show it as missing?
# addmargins(table(df_baseline$`Age, years`, df_baseline$Trial))
# class(df_baseline$`Age, years`)
df_baseline <- df_baseline %>% 
  mutate(`Age, years` = case_when(Trial == "RUXCOVID-DEVENT" ~ NA,
                                  TRUE ~ c(`Age, years`)))
df_baseline$`Age, years` <- as.numeric(df_baseline$`Age, years`)
# class(df_baseline$`Age, years`)
# addmargins(table(df_baseline$`Age, years`, df_baseline$Trial))
```

# Main Baseline table, by treatment arm (CAVE: RUXCOVID is included but only its categorical covariates // RUXCOVID-DEVENT excluded from Age due to binary variable)

```r
vars.list_main <- c("ARM","Age, years", "Sex", "Vaccination", "Time from symptom onset to randomisation, days", "Clinical status on ordinal scale", "Comorbidities", "Dexamethasone and Tocilizumab", "Remdesivir", "C-reactive protein concentration, mg/L", "Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N])", "Patients with undetectable viral load")
table_baseline <- CreateTableOne(data = df_baseline, vars = vars.list_main[!vars.list_main %in% c("ARM")], strata = "ARM", includeNA = F, test = F, addOverall = TRUE)
capture.output(table_baseline <- print(table_baseline, nonnormal = vars.list_main,catDigits = 1,SMD = TRUE,showAllLevels = TRUE,test = TRUE,printToggle = FALSE,missing = T))
```

```
## character(0)
```

```r
# print
kable(table_baseline, format = "markdown", table.attr = 'class="table"', caption = "Main Baseline characteristics, by arm") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



Table: Main Baseline characteristics, by arm

|                                                                                              |level                                                                                     |Overall               |JAK inhibitor         |No JAK inhibitor      |Missing |
|:---------------------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------|:---------------------|:---------------------|:---------------------|:-------|
|n                                                                                             |                                                                                          |12902                 |6647                  |6255                  |        |
|Age, years (median [IQR])                                                                     |                                                                                          |58.00 [47.00, 69.00]  |58.00 [48.00, 69.00]  |58.00 [47.00, 68.00]  |5.0     |
|Sex (%)                                                                                       |Female                                                                                    |4637 (35.9)           |2383 (35.9)           |2254 (36.0)           |0.0     |
|                                                                                              |Male                                                                                      |8265 (64.1)           |4264 (64.1)           |4001 (64.0)           |        |
|Vaccination (%)                                                                               |Any SARS CoV-2 vaccine                                                                    |3564 (32.7)           |1828 (32.4)           |1736 (33.1)           |15.6    |
|                                                                                              |No SARS CoV-2 vaccine                                                                     |7328 (67.3)           |3818 (67.6)           |3510 (66.9)           |        |
|Time from symptom onset to randomisation, days (median [IQR])                                 |                                                                                          |10.00 [7.00, 12.00]   |10.00 [7.00, 12.00]   |10.00 [7.00, 12.00]   |5.2     |
|Clinical status on ordinal scale (%)                                                          |2: Hospitalised without need for oxygen therapy (WHO score 4)                             |1152 ( 8.9)           |585 ( 8.8)            |567 ( 9.1)            |0.1     |
|                                                                                              |3: Hospitalised with need for supplemental low-flow oxygen (WHO score 5)                  |8078 (62.6)           |4111 (61.9)           |3967 (63.5)           |        |
|                                                                                              |4: Hospitalised with need for high-flow oxygen or non- invasive ventilation (WHO score 6) |2959 (22.9)           |1534 (23.1)           |1425 (22.8)           |        |
|                                                                                              |5: Hospitalised with need for mechanical ventilation or ECMO (WHO score 7–9)              |705 ( 5.5)            |414 ( 6.2)            |291 ( 4.7)            |        |
|Comorbidities (%)                                                                             |No comorbidity                                                                            |5428 (42.1)           |2750 (41.4)           |2678 (42.8)           |0.0     |
|                                                                                              |One comorbidity                                                                           |3909 (30.3)           |2009 (30.2)           |1900 (30.4)           |        |
|                                                                                              |Multiple comorbidities                                                                    |3467 (26.9)           |1838 (27.7)           |1629 (26.0)           |        |
|                                                                                              |Immunocompromised                                                                         |98 ( 0.8)             |50 ( 0.8)             |48 ( 0.8)             |        |
|Dexamethasone and Tocilizumab (%)                                                             |No Dexamethasone, no Tocilizumab                                                          |2101 (16.3)           |1069 (16.1)           |1032 (16.5)           |0.1     |
|                                                                                              |Dexamethasone but no Tocilizumab                                                          |2870 (22.3)           |1503 (22.6)           |1367 (21.9)           |        |
|                                                                                              |Dexamethasone and Tocilizumab                                                             |7890 (61.2)           |4055 (61.0)           |3835 (61.4)           |        |
|                                                                                              |Tocolizumab but no Dexamethasone                                                          |34 ( 0.3)             |18 ( 0.3)             |16 ( 0.3)             |        |
|Remdesivir (%)                                                                                |No Remdesivir                                                                             |9407 (72.9)           |4827 (72.6)           |4580 (73.2)           |0.0     |
|                                                                                              |Remdesivir                                                                                |3495 (27.1)           |1820 (27.4)           |1675 (26.8)           |        |
|C-reactive protein concentration, mg/L (median [IQR])                                         |                                                                                          |86.00 [42.00, 148.00] |85.00 [42.00, 150.00] |86.00 [42.00, 146.00] |9.8     |
|Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N]) (%) |Not seroconverted                                                                         |594 (33.9)            |285 (33.5)            |309 (34.4)            |86.4    |
|                                                                                              |Seroconverted                                                                             |1157 (66.1)           |567 (66.5)            |590 (65.6)            |        |
|Patients with undetectable viral load (%)                                                     |Detectable viral load                                                                     |9594 (97.3)           |4839 (97.4)           |4755 (97.2)           |23.6    |
|                                                                                              |Undetectable viral load                                                                   |265 ( 2.7)            |129 ( 2.6)            |136 ( 2.8)            |        |

# Appendix Baseline table, by treatment arm (CAVE: RUXCOVID is included but only its categorical covariates)

```r
vars.list_appendix <- c("ARM", "Ethnicity", "Country", "Patients admitted to intensive care unit")
table_baseline <- CreateTableOne(data = df_baseline, vars = vars.list_appendix[!vars.list_appendix %in% c("ARM")], strata = "ARM", includeNA = F, test = F, addOverall = TRUE)
capture.output(table_baseline <- print(table_baseline, nonnormal = vars.list_appendix,catDigits = 1,SMD = TRUE,showAllLevels = TRUE,test = TRUE,printToggle = FALSE,missing = T))
```

```
## character(0)
```

```r
# print
kable(table_baseline, format = "markdown", table.attr = 'class="table"', caption = "Appendix Baseline characteristics, by arm") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



Table: Appendix Baseline characteristics, by arm

|                                             |level                                     |Overall     |JAK inhibitor |No JAK inhibitor |Missing |
|:--------------------------------------------|:-----------------------------------------|:-----------|:-------------|:----------------|:-------|
|n                                            |                                          |12902       |6647          |6255             |        |
|Ethnicity (%)                                |American Indian or Alaska Native          |399 ( 3.5)  |191 ( 3.3)    |208 ( 3.8)       |11.9    |
|                                             |Asian                                     |812 ( 7.1)  |393 ( 6.7)    |419 ( 7.6)       |        |
|                                             |Black or African American                 |524 ( 4.6)  |272 ( 4.6)    |252 ( 4.6)       |        |
|                                             |Caucasian                                 |8895 (78.3) |4636 (79.1)   |4259 (77.3)      |        |
|                                             |Hispanic or Latino                        |539 ( 4.7)  |273 ( 4.7)    |266 ( 4.8)       |        |
|                                             |Mixed                                     |85 ( 0.7)   |40 ( 0.7)     |45 ( 0.8)        |        |
|                                             |Native Hawaiian or other Pacific Islander |16 ( 0.1)   |7 ( 0.1)      |9 ( 0.2)         |        |
|                                             |Persian/Mazani                            |97 ( 0.9)   |46 ( 0.8)     |51 ( 0.9)        |        |
|Country (%)                                  |Argentina                                 |229 ( 1.9)  |119 ( 1.9)    |110 ( 1.8)       |5.0     |
|                                             |Asia*                                     |67 ( 0.5)   |33 ( 0.5)     |34 ( 0.6)        |        |
|                                             |Austria                                   |6 ( 0.0)    |2 ( 0.0)      |4 ( 0.1)         |        |
|                                             |Belgium                                   |10 ( 0.1)   |4 ( 0.1)      |6 ( 0.1)         |        |
|                                             |Brazil                                    |655 ( 5.3)  |331 ( 5.3)    |324 ( 5.3)       |        |
|                                             |Europe*                                   |13 ( 0.1)   |6 ( 0.1)      |7 ( 0.1)         |        |
|                                             |France                                    |94 ( 0.8)   |50 ( 0.8)     |44 ( 0.7)        |        |
|                                             |Germany                                   |21 ( 0.2)   |10 ( 0.2)     |11 ( 0.2)        |        |
|                                             |India                                     |50 ( 0.4)   |19 ( 0.3)     |31 ( 0.5)        |        |
|                                             |Iran                                      |97 ( 0.8)   |46 ( 0.7)     |51 ( 0.8)        |        |
|                                             |Ireland                                   |9 ( 0.1)    |5 ( 0.1)      |4 ( 0.1)         |        |
|                                             |Italy                                     |166 ( 1.4)  |86 ( 1.4)     |80 ( 1.3)        |        |
|                                             |Japan                                     |38 ( 0.3)   |19 ( 0.3)     |19 ( 0.3)        |        |
|                                             |Luxembourg                                |1 ( 0.0)    |1 ( 0.0)      |0 ( 0.0)         |        |
|                                             |Mexico                                    |312 ( 2.5)  |152 ( 2.5)    |160 ( 2.6)       |        |
|                                             |North America*                            |953 ( 7.8)  |476 ( 7.7)    |477 ( 7.9)       |        |
|                                             |Norway                                    |127 ( 1.0)  |61 ( 1.0)     |66 ( 1.1)        |        |
|                                             |Portugal                                  |3 ( 0.0)    |2 ( 0.0)      |1 ( 0.0)         |        |
|                                             |Russia                                    |112 ( 0.9)  |58 ( 0.9)     |54 ( 0.9)        |        |
|                                             |South Korea                               |36 ( 0.3)   |16 ( 0.3)     |20 ( 0.3)        |        |
|                                             |Spain                                     |497 ( 4.1)  |251 ( 4.1)    |246 ( 4.1)       |        |
|                                             |United Kingdom                            |8423 (68.7) |4277 (69.0)   |4146 (68.4)      |        |
|                                             |USA                                       |340 ( 2.8)  |172 ( 2.8)    |168 ( 2.8)       |        |
|Patients admitted to intensive care unit (%) |No                                        |2945 (90.9) |1532 (90.8)   |1413 (90.9)      |74.9    |
|                                             |Yes                                       |296 ( 9.1)  |155 ( 9.2)    |141 ( 9.1)       |        |

# All Baseline Characteristics, by trial (CAVE: RUXCOVID is included but only its categorical covariates // RUXCOVID-DEVENT excluded from Age due to binary variable)

```r
# all participants, by trial
table_baseline_trial <- CreateTableOne(data = df_baseline, vars = vars.list[!vars.list %in% c("Trial", "ARM")], strata = "Trial", includeNA = F, test = F, addOverall = TRUE)
capture.output(table_baseline_trial <- print(table_baseline_trial, nonnormal = vars.list,catDigits = 1,SMD = TRUE,showAllLevels = TRUE,test = TRUE,printToggle = FALSE,missing = T))
```

```
## character(0)
```

```r
#print
kable(table_baseline_trial, format = "markdown", table.attr = 'class="table"', caption = "Baseline characteristics, by trial") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



Table: Baseline characteristics, by trial

|                                                                                              |level                                                                                     |Overall               |ACTT2                  |Bari-Solidact         |COV-BARRIER           |COVINIB               |Ghazaeian            |PANCOVID              |RECOVERY              |RUXCOVID    |RUXCOVID-DEVENT        |STOP-COVID           |TACTIC-R               |TOFACOV              |Missing |
|:---------------------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------|:---------------------|:----------------------|:---------------------|:---------------------|:---------------------|:--------------------|:---------------------|:---------------------|:-----------|:----------------------|:--------------------|:----------------------|:--------------------|:-------|
|n                                                                                             |                                                                                          |12902                 |1033                   |289                   |1626                  |110                   |97                   |287                   |8130                  |432         |211                    |289                  |282                    |116                  |        |
|Age, years (median [IQR])                                                                     |                                                                                          |58.00 [47.00, 69.00]  |56.00 [43.00, 67.00]   |60.00 [50.00, 69.00]  |58.00 [48.00, 68.00]  |55.00 [47.25, 62.00]  |51.00 [37.00, 64.00] |67.00 [62.00, 74.00]  |58.00 [47.00, 69.00]  |NA [NA, NA] |NA [NA, NA]            |57.00 [45.00, 67.00] |60.00 [52.00, 69.00]   |58.00 [50.75, 66.25] |5.0     |
|Sex (%)                                                                                       |Female                                                                                    |4637 (35.9)           |381 ( 36.9)            |218 (75.4)            |608 (37.4)            |34 ( 30.9)            |50 ( 51.5)           |99 ( 34.5)            |2764 ( 34.0)          |196 ( 45.4) |74 ( 35.1)             |101 ( 34.9)          |76 ( 27.0)             |36 ( 31.0)           |0.0     |
|                                                                                              |Male                                                                                      |8265 (64.1)           |652 ( 63.1)            |71 (24.6)             |1018 (62.6)           |76 ( 69.1)            |47 ( 48.5)           |188 ( 65.5)           |5366 ( 66.0)          |236 ( 54.6) |137 ( 64.9)            |188 ( 65.1)          |206 ( 73.0)            |80 ( 69.0)           |        |
|Ethnicity (%)                                                                                 |American Indian or Alaska Native                                                          |399 ( 3.5)            |10 (  1.0)             |0 ( NaN)              |348 (21.9)            |0 (  0.0)             |0 (  0.0)            |0 (  NaN)             |0 (  0.0)             |39 (  9.3)  |2 (  1.1)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |11.9    |
|                                                                                              |Asian                                                                                     |812 ( 7.1)            |101 (  9.9)            |0 ( NaN)              |175 (11.0)            |1 (  0.9)             |0 (  0.0)            |0 (  NaN)             |487 (  6.7)           |10 (  2.4)  |6 (  3.3)              |0 (  0.0)            |32 ( 12.2)             |0 (  0.0)            |        |
|                                                                                              |Black or African American                                                                 |524 ( 4.6)            |156 ( 15.3)            |0 ( NaN)              |77 ( 4.8)             |0 (  0.0)             |0 (  0.0)            |0 (  NaN)             |220 (  3.0)           |15 (  3.6)  |25 ( 13.7)             |0 (  0.0)            |31 ( 11.8)             |0 (  0.0)            |        |
|                                                                                              |Caucasian                                                                                 |8895 (78.3)           |496 ( 48.6)            |0 ( NaN)              |982 (61.7)            |90 ( 81.8)            |0 (  0.0)            |0 (  NaN)             |6512 ( 89.3)          |351 ( 83.4) |149 ( 81.9)            |0 (  0.0)            |199 ( 75.7)            |116 (100.0)          |        |
|                                                                                              |Hispanic or Latino                                                                        |539 ( 4.7)            |246 ( 24.1)            |0 ( NaN)              |0 ( 0.0)              |19 ( 17.3)            |0 (  0.0)            |0 (  NaN)             |0 (  0.0)             |1 (  0.2)   |0 (  0.0)              |273 (100.0)          |0 (  0.0)              |0 (  0.0)            |        |
|                                                                                              |Mixed                                                                                     |85 ( 0.7)             |0 (  0.0)              |0 ( NaN)              |5 ( 0.3)              |0 (  0.0)             |0 (  0.0)            |0 (  NaN)             |74 (  1.0)            |5 (  1.2)   |0 (  0.0)              |0 (  0.0)            |1 (  0.4)              |0 (  0.0)            |        |
|                                                                                              |Native Hawaiian or other Pacific Islander                                                 |16 ( 0.1)             |11 (  1.1)             |0 ( NaN)              |5 ( 0.3)              |0 (  0.0)             |0 (  0.0)            |0 (  NaN)             |0 (  0.0)             |0 (  0.0)   |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                                                                                              |Persian/Mazani                                                                            |97 ( 0.9)             |0 (  0.0)              |0 ( NaN)              |0 ( 0.0)              |0 (  0.0)             |97 (100.0)           |0 (  NaN)             |0 (  0.0)             |0 (  0.0)   |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|Country (%)                                                                                   |Argentina                                                                                 |229 ( 1.9)            |0 (  0.0)              |0 ( 0.0)              |229 (14.1)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  NaN)   |0 (  NaN)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |5.0     |
|                                                                                              |Asia*                                                                                     |67 ( 0.5)             |67 (  6.5)             |0 ( 0.0)              |0 ( 0.0)              |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  NaN)   |0 (  NaN)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                                                                                              |Austria                                                                                   |6 ( 0.0)              |0 (  0.0)              |6 ( 2.1)              |0 ( 0.0)              |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  NaN)   |0 (  NaN)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                                                                                              |Belgium                                                                                   |10 ( 0.1)             |0 (  0.0)              |10 ( 3.5)             |0 ( 0.0)              |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  NaN)   |0 (  NaN)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                                                                                              |Brazil                                                                                    |655 ( 5.3)            |0 (  0.0)              |0 ( 0.0)              |366 (22.5)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  NaN)   |0 (  NaN)              |289 (100.0)          |0 (  0.0)              |0 (  0.0)            |        |
|                                                                                              |Europe*                                                                                   |13 ( 0.1)             |13 (  1.3)             |0 ( 0.0)              |0 ( 0.0)              |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  NaN)   |0 (  NaN)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                                                                                              |France                                                                                    |94 ( 0.8)             |0 (  0.0)              |94 (32.5)             |0 ( 0.0)              |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  NaN)   |0 (  NaN)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                                                                                              |Germany                                                                                   |21 ( 0.2)             |0 (  0.0)              |1 ( 0.3)              |20 ( 1.2)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  NaN)   |0 (  NaN)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                                                                                              |India                                                                                     |50 ( 0.4)             |0 (  0.0)              |0 ( 0.0)              |50 ( 3.1)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  NaN)   |0 (  NaN)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                                                                                              |Iran                                                                                      |97 ( 0.8)             |0 (  0.0)              |0 ( 0.0)              |0 ( 0.0)              |0 (  0.0)             |97 (100.0)           |0 (  0.0)             |0 (  0.0)             |0 (  NaN)   |0 (  NaN)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                                                                                              |Ireland                                                                                   |9 ( 0.1)              |0 (  0.0)              |9 ( 3.1)              |0 ( 0.0)              |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  NaN)   |0 (  NaN)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                                                                                              |Italy                                                                                     |166 ( 1.4)            |0 (  0.0)              |25 ( 8.7)             |25 ( 1.5)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  NaN)   |0 (  NaN)              |0 (  0.0)            |0 (  0.0)              |116 (100.0)          |        |
|                                                                                              |Japan                                                                                     |38 ( 0.3)             |0 (  0.0)              |0 ( 0.0)              |38 ( 2.3)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  NaN)   |0 (  NaN)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                                                                                              |Luxembourg                                                                                |1 ( 0.0)              |0 (  0.0)              |1 ( 0.3)              |0 ( 0.0)              |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  NaN)   |0 (  NaN)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                                                                                              |Mexico                                                                                    |312 ( 2.5)            |0 (  0.0)              |0 ( 0.0)              |312 (19.2)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  NaN)   |0 (  NaN)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                                                                                              |North America*                                                                            |953 ( 7.8)            |953 ( 92.3)            |0 ( 0.0)              |0 ( 0.0)              |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  NaN)   |0 (  NaN)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                                                                                              |Norway                                                                                    |127 ( 1.0)            |0 (  0.0)              |127 (43.9)            |0 ( 0.0)              |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  NaN)   |0 (  NaN)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                                                                                              |Portugal                                                                                  |3 ( 0.0)              |0 (  0.0)              |3 ( 1.0)              |0 ( 0.0)              |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  NaN)   |0 (  NaN)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                                                                                              |Russia                                                                                    |112 ( 0.9)            |0 (  0.0)              |0 ( 0.0)              |112 ( 6.9)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  NaN)   |0 (  NaN)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                                                                                              |South Korea                                                                               |36 ( 0.3)             |0 (  0.0)              |0 ( 0.0)              |36 ( 2.2)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  NaN)   |0 (  NaN)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                                                                                              |Spain                                                                                     |497 ( 4.1)            |0 (  0.0)              |13 ( 4.5)             |87 ( 5.4)             |110 (100.0)           |0 (  0.0)            |287 (100.0)           |0 (  0.0)             |0 (  NaN)   |0 (  NaN)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                                                                                              |United Kingdom                                                                            |8423 (68.7)           |0 (  0.0)              |0 ( 0.0)              |11 ( 0.7)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |8130 (100.0)          |0 (  NaN)   |0 (  NaN)              |0 (  0.0)            |282 (100.0)            |0 (  0.0)            |        |
|                                                                                              |USA                                                                                       |340 ( 2.8)            |0 (  0.0)              |0 ( 0.0)              |340 (20.9)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  NaN)   |0 (  NaN)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|Vaccination (%)                                                                               |Any SARS CoV-2 vaccine                                                                    |3564 (32.7)           |0 (  0.0)              |102 (35.9)            |0 ( NaN)              |2 (  1.8)             |0 (  NaN)            |37 ( 12.9)            |3420 ( 42.1)          |0 (  0.0)   |0 (  0.0)              |0 (  0.0)            |0 (  NaN)              |3 (  2.6)            |15.6    |
|                                                                                              |No SARS CoV-2 vaccine                                                                     |7328 (67.3)           |1033 (100.0)           |182 (64.1)            |0 ( NaN)              |108 ( 98.2)           |0 (  NaN)            |250 ( 87.1)           |4710 ( 57.9)          |432 (100.0) |211 (100.0)            |289 (100.0)          |0 (  NaN)              |113 ( 97.4)          |        |
|Time from symptom onset to randomisation, days (median [IQR])                                 |                                                                                          |10.00 [7.00, 12.00]   |8.00 [5.00, 10.00]     |9.00 [7.00, 11.00]    |10.00 [8.00, 13.00]   |7.00 [5.00, 9.00]     |7.00 [6.00, 10.00]   |8.00 [5.00, 10.00]    |10.00 [7.00, 12.00]   |NA [NA, NA] |NA [NA, NA]            |9.00 [7.00, 11.00]   |10.00 [7.00, 13.00]    |8.00 [6.00, 10.00]   |5.2     |
|Patients admitted to intensive care unit (%)                                                  |No                                                                                        |2945 (90.9)           |0 (  NaN)              |160 (55.4)            |1525 (93.8)           |110 (100.0)           |97 (100.0)           |0 (  NaN)             |0 (  NaN)             |432 (100.0) |0 (  NaN)              |235 ( 81.3)          |270 ( 95.7)            |116 (100.0)          |74.9    |
|                                                                                              |Yes                                                                                       |296 ( 9.1)            |0 (  NaN)              |129 (44.6)            |101 ( 6.2)            |0 (  0.0)             |0 (  0.0)            |0 (  NaN)             |0 (  NaN)             |0 (  0.0)   |0 (  NaN)              |54 ( 18.7)           |12 (  4.3)             |0 (  0.0)            |        |
|Clinical status on ordinal scale (%)                                                          |2: Hospitalised without need for oxygen therapy (WHO score 4)                             |1152 ( 8.9)           |142 ( 13.7)            |0 ( 0.0)              |186 (11.5)            |35 ( 31.8)            |2 (  2.1)            |74 ( 25.8)            |465 (  5.7)           |141 ( 32.7) |0 (  0.0)              |71 ( 24.6)           |11 (  3.9)             |25 ( 21.6)           |0.1     |
|                                                                                              |3: Hospitalised with need for supplemental low-flow oxygen (WHO score 5)                  |8078 (62.6)           |564 ( 54.6)            |0 ( 0.0)              |962 (59.4)            |75 ( 68.2)            |95 ( 97.9)           |208 ( 72.5)           |5504 ( 67.7)          |268 ( 62.2) |0 (  0.0)              |181 ( 62.6)          |130 ( 46.1)            |91 ( 78.4)           |        |
|                                                                                              |4: Hospitalised with need for high-flow oxygen or non- invasive ventilation (WHO score 6) |2959 (22.9)           |216 ( 20.9)            |249 (86.2)            |370 (22.9)            |0 (  0.0)             |0 (  0.0)            |5 (  1.7)             |1921 ( 23.6)          |22 (  5.1)  |0 (  0.0)              |37 ( 12.8)           |139 ( 49.3)            |0 (  0.0)            |        |
|                                                                                              |5: Hospitalised with need for mechanical ventilation or ECMO (WHO score 7–9)              |705 ( 5.5)            |111 ( 10.7)            |40 (13.8)             |101 ( 6.2)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |240 (  3.0)           |0 (  0.0)   |211 (100.0)            |0 (  0.0)            |2 (  0.7)              |0 (  0.0)            |        |
|Comorbidities (%)                                                                             |No comorbidity                                                                            |5428 (42.1)           |169 ( 16.4)            |106 (36.7)            |370 (22.8)            |37 ( 33.6)            |41 ( 42.3)           |64 ( 22.3)            |4353 ( 53.5)          |106 ( 24.5) |24 ( 11.4)             |65 ( 22.5)           |49 ( 17.4)             |44 ( 37.9)           |0.0     |
|                                                                                              |One comorbidity                                                                           |3909 (30.3)           |288 ( 27.9)            |73 (25.3)             |477 (29.3)            |40 ( 36.4)            |22 ( 22.7)           |83 ( 28.9)            |2538 ( 31.2)          |136 ( 31.5) |43 ( 20.4)             |99 ( 34.3)           |64 ( 22.7)             |46 ( 39.7)           |        |
|                                                                                              |Multiple comorbidities                                                                    |3467 (26.9)           |546 ( 52.9)            |99 (34.3)             |761 (46.8)            |31 ( 28.2)            |32 ( 33.0)           |140 ( 48.8)           |1239 ( 15.2)          |190 ( 44.0) |144 ( 68.2)            |125 ( 43.3)          |134 ( 47.5)            |26 ( 22.4)           |        |
|                                                                                              |Immunocompromised                                                                         |98 ( 0.8)             |30 (  2.9)             |11 ( 3.8)             |18 ( 1.1)             |2 (  1.8)             |2 (  2.1)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)   |0 (  0.0)              |0 (  0.0)            |35 ( 12.4)             |0 (  0.0)            |        |
|Dexamethasone and Tocilizumab (%)                                                             |No Dexamethasone, no Tocilizumab                                                          |2101 (16.3)           |991 ( 95.9)            |18 ( 6.2)             |328 (20.3)            |94 ( 85.5)            |0 (  0.0)            |0 (  0.0)             |349 (  4.3)           |183 ( 42.4) |33 ( 15.6)             |62 ( 21.5)           |36 ( 12.8)             |7 (  6.0)            |0.1     |
|                                                                                              |Dexamethasone but no Tocilizumab                                                          |2870 (22.3)           |0 (  0.0)              |1 ( 0.3)              |0 ( 0.0)              |0 (  0.0)             |0 (  0.0)            |18 (  6.3)            |2590 ( 31.9)          |249 ( 57.6) |0 (  0.0)              |0 (  0.0)            |12 (  4.3)             |0 (  0.0)            |        |
|                                                                                              |Dexamethasone and Tocilizumab                                                             |7890 (61.2)           |42 (  4.1)             |270 (93.4)            |1291 (79.7)           |16 ( 14.5)            |97 (100.0)           |269 ( 93.7)           |5157 ( 63.4)          |0 (  0.0)   |178 ( 84.4)            |227 ( 78.5)          |234 ( 83.0)            |109 ( 94.0)          |        |
|                                                                                              |Tocolizumab but no Dexamethasone                                                          |34 ( 0.3)             |0 (  0.0)              |0 ( 0.0)              |0 ( 0.0)              |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |34 (  0.4)            |0 (  0.0)   |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|Remdesivir (%)                                                                                |No Remdesivir                                                                             |9407 (72.9)           |0 (  0.0)              |281 (97.2)            |1337 (82.2)           |110 (100.0)           |0 (  0.0)            |243 ( 84.7)           |6474 ( 79.6)          |404 ( 93.5) |97 ( 46.0)             |289 (100.0)          |159 ( 56.4)            |13 ( 11.2)           |0.0     |
|                                                                                              |Remdesivir                                                                                |3495 (27.1)           |1033 (100.0)           |8 ( 2.8)              |289 (17.8)            |0 (  0.0)             |97 (100.0)           |44 ( 15.3)            |1656 ( 20.4)          |28 (  6.5)  |114 ( 54.0)            |0 (  0.0)            |123 ( 43.6)            |103 ( 88.8)          |        |
|C-reactive protein concentration, mg/L (median [IQR])                                         |                                                                                          |86.00 [42.00, 148.00] |125.75 [64.93, 190.50] |89.00 [50.00, 142.00] |65.00 [32.00, 122.90] |79.15 [39.72, 134.60] |68.00 [33.50, 84.00] |66.85 [31.22, 113.05] |86.00 [43.00, 145.00] |NA [NA, NA] |167.65 [74.50, 346.02] |NA [NA, NA]          |109.00 [67.00, 159.00] |4.90 [2.40, 9.75]    |9.8     |
|Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N]) (%) |Not seroconverted                                                                         |594 (33.9)            |0 (  NaN)              |7 ( 5.0)              |0 ( NaN)              |0 (  NaN)             |0 (  NaN)            |0 (  NaN)             |587 ( 36.4)           |0 (  NaN)   |0 (  NaN)              |0 (  NaN)            |0 (  NaN)              |0 (  NaN)            |86.4    |
|                                                                                              |Seroconverted                                                                             |1157 (66.1)           |0 (  NaN)              |132 (95.0)            |0 ( NaN)              |0 (  NaN)             |0 (  NaN)            |0 (  NaN)             |1025 ( 63.6)          |0 (  NaN)   |0 (  NaN)              |0 (  NaN)            |0 (  NaN)              |0 (  NaN)            |        |
|Patients with undetectable viral load (%)                                                     |Detectable viral load                                                                     |9594 (97.3)           |526 ( 75.9)            |116 (86.6)            |1494 (97.2)           |0 (  NaN)             |0 (  NaN)            |0 (  NaN)             |7195 ( 99.8)          |0 (  NaN)   |0 (  NaN)              |0 (  NaN)            |263 ( 93.3)            |0 (  NaN)            |23.6    |
|                                                                                              |Undetectable viral load                                                                   |265 ( 2.7)            |167 ( 24.1)            |18 (13.4)             |43 ( 2.8)             |0 (  NaN)             |0 (  NaN)            |0 (  NaN)             |18 (  0.2)            |0 (  NaN)   |0 (  NaN)              |0 (  NaN)            |19 (  6.7)             |0 (  NaN)            |        |

# Missing data table for outcomes and adjustment variables, by trial

```r
# take out RUXCOVID, add it as its own line below
df_tot <- df_tot %>% 
  filter(!trial == "RUXCOVID")

vars.list <- c("trial", "age", "clinstatus_baseline",
               "mort_28", "mort_60", "new_mvd_28", "clinstatus_28_imp", 
                "ae_28", "vir_clear_5" ,"vir_clear_10", "vir_clear_15")
df_missing <- df_tot[,colnames(df_tot)%in%vars.list]
df_missing <- df_missing[,match(vars.list,colnames(df_missing))]

# Count missing values for each variable within each trial, and its proportion
age_missing_summary <- df_missing %>%
  group_by(trial) %>%
  summarise(across(starts_with("age"), list(
    count_missing = ~ sum(is.na(.)),
    denominator = ~ n(),
    prop_missing = ~ round(sum(is.na(.)) / n(), 3) * 100
  ))) %>%
  unite("age_summary", starts_with("age"), sep = "/", na.rm = TRUE) 

clinstatus_baseline_missing_summary <- df_missing %>%
  group_by(trial) %>%
  summarise(across(starts_with("clinstatus_baseline"), list(
    count_missing = ~ sum(is.na(.)),
    denominator = ~ n(),
    prop_missing = ~ round(sum(is.na(.)) / n(), 3) * 100
  ))) %>%
  unite("clinstatus_baseline_summary", starts_with("clinstatus_baseline"), sep = "/", na.rm = TRUE) 

mort_28_missing_summary <- df_missing %>%
  group_by(trial) %>%
  summarise(across(starts_with("mort_28"), list(
    count_missing = ~ sum(is.na(.)),
    denominator = ~ n(),
    prop_missing = ~ round(sum(is.na(.)) / n(), 3) * 100
  ))) %>%
  unite("mort_28_summary", starts_with("mort_28"), sep = "/", na.rm = TRUE) 

mort_60_missing_summary <- df_missing %>%
  group_by(trial) %>%
  summarise(across(starts_with("mort_60"), list(
    count_missing = ~ sum(is.na(.)),
    denominator = ~ n(),
    prop_missing = ~ round(sum(is.na(.)) / n(), 3) * 100
  ))) %>%
  unite("mort_60_summary", starts_with("mort_60"), sep = "/", na.rm = TRUE) 

new_mvd_28_missing_summary <- df_missing %>%
  group_by(trial) %>%
  summarise(across(starts_with("new_mvd_28"), list(
    count_missing = ~ sum(is.na(.)),
    denominator = ~ n(),
    prop_missing = ~ round(sum(is.na(.)) / n(), 3) * 100
  ))) %>%
  unite("new_mvd_28_summary", starts_with("new_mvd_28"), sep = "/", na.rm = TRUE) 

clinstatus_28_imp_missing_summary <- df_missing %>%
  group_by(trial) %>%
  summarise(across(starts_with("clinstatus_28_imp"), list(
    count_missing = ~ sum(is.na(.)),
    denominator = ~ n(),
    prop_missing = ~ round(sum(is.na(.)) / n(), 3) * 100
  ))) %>%
  unite("clinstatus_28_imp_summary", starts_with("clinstatus_28_imp"), sep = "/", na.rm = TRUE) 

ae_28_missing_summary <- df_missing %>%
  group_by(trial) %>%
  summarise(across(starts_with("ae_28"), list(
    count_missing = ~ sum(is.na(.)),
    denominator = ~ n(),
    prop_missing = ~ round(sum(is.na(.)) / n(), 3) * 100
  ))) %>%
  unite("ae_28_summary", starts_with("ae_28"), sep = "/", na.rm = TRUE) 

vir_clear_5_missing_summary <- df_missing %>%
  group_by(trial) %>%
  summarise(across(starts_with("vir_clear_5"), list(
    count_missing = ~ sum(is.na(.)),
    denominator = ~ n(),
    prop_missing = ~ round(sum(is.na(.)) / n(), 3) * 100
  ))) %>%
  unite("vir_clear_5_summary", starts_with("vir_clear_5"), sep = "/", na.rm = TRUE) 

vir_clear_10_missing_summary <- df_missing %>%
  group_by(trial) %>%
  summarise(across(starts_with("vir_clear_10"), list(
    count_missing = ~ sum(is.na(.)),
    denominator = ~ n(),
    prop_missing = ~ round(sum(is.na(.)) / n(), 3) * 100
  ))) %>%
  unite("vir_clear_10_summary", starts_with("vir_clear_10"), sep = "/", na.rm = TRUE) 

vir_clear_15_missing_summary <- df_missing %>%
  group_by(trial) %>%
  summarise(across(starts_with("vir_clear_15"), list(
    count_missing = ~ sum(is.na(.)),
    denominator = ~ n(),
    prop_missing = ~ round(sum(is.na(.)) / n(), 3) * 100
  ))) %>%
  unite("vir_clear_15_summary", starts_with("vir_clear_15"), sep = "/", na.rm = TRUE) 

missing_summary <- cbind(age_missing_summary, clinstatus_baseline_missing_summary, mort_28_missing_summary, mort_60_missing_summary, new_mvd_28_missing_summary, clinstatus_28_imp_missing_summary, ae_28_missing_summary, vir_clear_5_missing_summary, vir_clear_10_missing_summary, vir_clear_15_missing_summary)

missing_summary <- missing_summary %>% select(1 | ends_with("_summary"))

## add RUXCOVID
ruxcovid_missing <- readRDS("missing_summary_ruxcovid.rds")

missing_summary <- rbind(missing_summary, ruxcovid_missing)

#print
kable(missing_summary, format = "markdown", table.attr = 'class="table"', caption = "Missing values in outcomes and adjustment variables, by trial") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



Table: Missing values in outcomes and adjustment variables, by trial

|trial           |age_summary |clinstatus_baseline_summary |mort_28_summary |mort_60_summary |new_mvd_28_summary |clinstatus_28_imp_summary |ae_28_summary |vir_clear_5_summary |vir_clear_10_summary |vir_clear_15_summary |
|:---------------|:-----------|:---------------------------|:---------------|:---------------|:------------------|:-------------------------|:-------------|:-------------------|:--------------------|:--------------------|
|ACTT2           |0/1033/0    |0/1033/0                    |47/1033/4.5     |47/1033/4.5     |34/1033/3.3        |0/1033/0                  |0/1033/0      |286/1033/27.7       |278/1033/26.9        |274/1033/26.5        |
|Bari-Solidact   |0/289/0     |0/289/0                     |12/289/4.2      |12/289/4.2      |11/289/3.8         |0/289/0                   |0/289/0       |168/289/58.1        |162/289/56.1         |161/289/55.7         |
|COV-BARRIER     |0/1626/0    |7/1626/0.4                  |103/1626/6.3    |128/1626/7.9    |83/1626/5.1        |0/1626/0                  |0/1626/0      |648/1626/39.9       |566/1626/34.8        |530/1626/32.6        |
|COVINIB         |0/110/0     |0/110/0                     |3/110/2.7       |3/110/2.7       |3/110/2.7          |0/110/0                   |0/110/0       |110/110/100         |110/110/100          |110/110/100          |
|Ghazaeian       |0/97/0      |0/97/0                      |0/97/0          |0/97/0          |0/97/0             |0/97/0                    |0/97/0        |97/97/100           |97/97/100            |97/97/100            |
|PANCOVID        |0/287/0     |0/287/0                     |11/287/3.8      |11/287/3.8      |11/287/3.8         |0/287/0                   |0/287/0       |287/287/100         |287/287/100          |287/287/100          |
|RECOVERY        |0/8130/0    |0/8130/0                    |129/8130/1.6    |129/8130/1.6    |101/8130/1.2       |0/8130/0                  |0/8130/0      |613/8130/7.5        |420/8130/5.2         |347/8130/4.3         |
|RUXCOVID-DEVENT |0/211/0     |0/211/0                     |2/211/0.9       |2/211/0.9       |2/211/0.9          |211/211/100               |0/211/0       |211/211/100         |211/211/100          |211/211/100          |
|STOP-COVID      |0/289/0     |0/289/0                     |0/289/0         |0/289/0         |0/289/0            |0/289/0                   |0/289/0       |289/289/100         |289/289/100          |289/289/100          |
|TACTIC-R        |0/282/0     |0/282/0                     |14/282/5        |18/282/6.4      |0/282/0            |0/282/0                   |0/282/0       |232/282/82.3        |218/282/77.3         |217/282/77           |
|TOFACOV         |0/116/0     |0/116/0                     |0/116/0         |0/116/0         |0/116/0            |0/116/0                   |0/116/0       |116/116/100         |116/116/100          |116/116/100          |
|RUXCOVID        |0/432/0     |1/432/0.2                   |8/432/1.9       |8/432/1.9       |8/432/1.9          |0/432/0                   |20/432/4.6    |432/432/100         |432/432/100          |432/432/100          |

# Missing data table for all variables, by trial (excluding RUXCOVID, added separately in next chapter)

```r
# table(df_tot$mort_28,useNA = "always")
vars.list <- c("trt", "trial", "age", "sex", "ethn", "country", "vacc", "sympdur", "icu", "clinstatus_baseline", "comorb_cat", "comed_cat", "comed_rdv", "crp", "sero", "vl_baseline", "variant",
               "mort_28", "mort_60", "death_reached", "new_mv_28", "new_mvd_28", "clinstatus_28_imp", "discharge_reached",
               "discharge_reached_sus" , "ae_28","ae_28_sev" ,"vir_clear_5" ,"vir_clear_10", "vir_clear_15")

df_missing <- df_tot[,colnames(df_tot)%in%vars.list]
df_missing <- df_missing[,match(vars.list,colnames(df_missing))]

# colnames(df_missing) <- vars.list <- c("ARM","Trial","Age, years", "Sex", "Ethnicity", "Country", "Vaccinated", "Time from symptom onset to randomisation, days", "Patients admitted to intensive care unit","Clinical status on ordinal scale", "Comorbidities", "Dexamethasone and Tocilizumab", "Remdesivir","C-reactive protein concentration, mg/L", "Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N])", "Patients with detectable viral load", "SARS CoV-2 variant")

char_vars <- c("trt", "trial", "sex", "ethn", "country", "vacc", "icu", "clinstatus_baseline", "comorb_cat", "comed_cat", "comed_rdv", "sero", "vl_baseline", "variant",
               "mort_28", "mort_60", "death_reached", "new_mv_28", "new_mvd_28", "clinstatus_28_imp", "discharge_reached",
               "discharge_reached_sus" , "ae_28","ae_28_sev" ,"vir_clear_5" ,"vir_clear_10", "vir_clear_15")

# Convert character variables to factors
df_missing <- df_missing %>%
  mutate(across(all_of(char_vars), factor))
# take out Age of RUXCOVID-DEVENT since it is binary, but do not show it as missing
df_missing <- df_missing %>% 
  mutate(age = case_when(trial == "RUXCOVID-DEVENT" ~ NA,
                                  TRUE ~ c(age)))
df_missing$age <- as.numeric(df_missing$age)
# class(df_missing$age)
# addmargins(table(df_missing$age, df_missing$trial))

# all missing data, by trial
table_missing_trial <- CreateTableOne(data = df_missing, vars = vars.list[!vars.list %in% c("trt", "trial")], strata = "trial", includeNA = T, test = F, addOverall = TRUE)
capture.output(table_missing_trial <- print(table_missing_trial, nonnormal = vars.list,catDigits = 1,SMD = TRUE,showAllLevels = TRUE,test = TRUE,printToggle = FALSE,missing = TRUE))
```

```
## character(0)
```

```r
#print
kable(table_missing_trial, format = "markdown", table.attr = 'class="table"', caption = "Missing data, by trial") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



Table: Missing data, by trial

|                          |level                                     |Overall               |ACTT2                  |Bari-Solidact         |COV-BARRIER           |COVINIB               |Ghazaeian            |PANCOVID              |RECOVERY              |RUXCOVID-DEVENT        |STOP-COVID           |TACTIC-R               |TOFACOV              |Missing |
|:-------------------------|:-----------------------------------------|:---------------------|:----------------------|:---------------------|:---------------------|:---------------------|:--------------------|:---------------------|:---------------------|:----------------------|:--------------------|:----------------------|:--------------------|:-------|
|n                         |                                          |12470                 |1033                   |289                   |1626                  |110                   |97                   |287                   |8130                  |211                    |289                  |282                    |116                  |        |
|age (median [IQR])        |                                          |58.00 [47.00, 69.00]  |56.00 [43.00, 67.00]   |60.00 [50.00, 69.00]  |58.00 [48.00, 68.00]  |55.00 [47.25, 62.00]  |51.00 [37.00, 64.00] |67.00 [62.00, 74.00]  |58.00 [47.00, 69.00]  |NA [NA, NA]            |57.00 [45.00, 67.00] |60.00 [52.00, 69.00]   |58.00 [50.75, 66.25] |1.7     |
|sex (%)                   |1                                         |218 ( 1.7)            |0 (  0.0)              |218 ( 75.4)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |0.0     |
|                          |2                                         |71 ( 0.6)             |0 (  0.0)              |71 ( 24.6)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |F                                         |381 ( 3.1)            |381 ( 36.9)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |female                                    |3842 (30.8)           |0 (  0.0)              |0 (  0.0)             |608 ( 37.4)           |34 ( 30.9)            |50 ( 51.5)           |99 ( 34.5)            |2764 ( 34.0)          |74 ( 35.1)             |101 ( 34.9)          |76 ( 27.0)             |36 ( 31.0)           |        |
|                          |M                                         |652 ( 5.2)            |652 ( 63.1)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |male                                      |7306 (58.6)           |0 (  0.0)              |0 (  0.0)             |1018 ( 62.6)          |76 ( 69.1)            |47 ( 48.5)           |188 ( 65.5)           |5366 ( 66.0)          |137 ( 64.9)            |188 ( 65.1)          |206 ( 73.0)            |80 ( 69.0)           |        |
|ethn (%)                  |                                          |37 ( 0.3)             |0 (  0.0)              |0 (  0.0)             |34 (  2.1)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |3 (  1.4)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |7.1     |
|                          |AMERICAN INDIAN OR ALASKA NATIVE          |360 ( 2.9)            |10 (  1.0)             |0 (  0.0)             |348 ( 21.4)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |2 (  0.9)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |Asian                                     |1 ( 0.0)              |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |1 (  0.9)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |ASIAN                                     |769 ( 6.2)            |101 (  9.8)            |0 (  0.0)             |175 ( 10.8)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |487 (  6.0)           |6 (  2.8)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |Asian or Asian British                    |32 ( 0.3)             |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |32 ( 11.3)             |0 (  0.0)            |        |
|                          |BLACK OR AFRICAN AMERICAN                 |478 ( 3.8)            |156 ( 15.1)            |0 (  0.0)             |77 (  4.7)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |220 (  2.7)           |25 ( 11.8)             |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |Black or Black British                    |31 ( 0.2)             |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |31 ( 11.0)             |0 (  0.0)            |        |
|                          |caucasian                                 |116 ( 0.9)            |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |116 (100.0)          |        |
|                          |HISPANIC OR LATINO                        |519 ( 4.2)            |246 ( 23.8)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |273 ( 94.5)          |0 (  0.0)              |0 (  0.0)            |        |
|                          |Latino                                    |19 ( 0.2)             |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |19 ( 17.3)            |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |Mixed                                     |1 ( 0.0)              |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |1 (  0.4)              |0 (  0.0)            |        |
|                          |MIXED                                     |74 ( 0.6)             |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |74 (  0.9)            |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |MULTIPLE                                  |5 ( 0.0)              |0 (  0.0)              |0 (  0.0)             |5 (  0.3)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER |16 ( 0.1)             |11 (  1.1)             |0 (  0.0)             |5 (  0.3)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |NOT REPORTED                              |20 ( 0.2)             |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |20 (  9.5)             |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |Other                                     |19 ( 0.2)             |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |19 (  6.7)             |0 (  0.0)            |        |
|                          |OTHER                                     |139 ( 1.1)            |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |133 (  1.6)           |6 (  2.8)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |Persian/Mazani                            |97 ( 0.8)             |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |97 (100.0)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |UNKNOWN                                   |419 ( 3.4)            |13 (  1.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |406 (  5.0)           |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |White                                     |289 ( 2.3)            |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |90 ( 81.8)            |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |199 ( 70.6)            |0 (  0.0)            |        |
|                          |WHITE                                     |8139 (65.3)           |496 ( 48.0)            |0 (  0.0)             |982 ( 60.4)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |6512 ( 80.1)          |149 ( 70.6)            |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |NA                                        |890 ( 7.1)            |0 (  0.0)              |289 (100.0)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |287 (100.0)           |298 (  3.7)           |0 (  0.0)              |16 (  5.5)           |0 (  0.0)              |0 (  0.0)            |        |
|country (%)               |ARG                                       |229 ( 1.8)            |0 (  0.0)              |0 (  0.0)             |229 ( 14.1)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |1.7     |
|                          |Asia                                      |67 ( 0.5)             |67 (  6.5)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |AUSTRIA                                   |6 ( 0.0)              |0 (  0.0)              |6 (  2.1)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |BELGIUM                                   |10 ( 0.1)             |0 (  0.0)              |10 (  3.5)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |BRA                                       |366 ( 2.9)            |0 (  0.0)              |0 (  0.0)             |366 ( 22.5)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |Brazil                                    |289 ( 2.3)            |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |289 (100.0)          |0 (  0.0)              |0 (  0.0)            |        |
|                          |DEU                                       |20 ( 0.2)             |0 (  0.0)              |0 (  0.0)             |20 (  1.2)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |ESP                                       |87 ( 0.7)             |0 (  0.0)              |0 (  0.0)             |87 (  5.4)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |Europe                                    |13 ( 0.1)             |13 (  1.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |FRANCE                                    |94 ( 0.8)             |0 (  0.0)              |94 ( 32.5)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |GBR                                       |8141 (65.3)           |0 (  0.0)              |0 (  0.0)             |11 (  0.7)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |8130 (100.0)          |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |GERMANY                                   |1 ( 0.0)              |0 (  0.0)              |1 (  0.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |IND                                       |50 ( 0.4)             |0 (  0.0)              |0 (  0.0)             |50 (  3.1)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |Iran                                      |97 ( 0.8)             |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |97 (100.0)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |IRELAND                                   |9 ( 0.1)              |0 (  0.0)              |9 (  3.1)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |ITA                                       |25 ( 0.2)             |0 (  0.0)              |0 (  0.0)             |25 (  1.5)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |Italy                                     |116 ( 0.9)            |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |116 (100.0)          |        |
|                          |ITALY                                     |25 ( 0.2)             |0 (  0.0)              |25 (  8.7)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |JPN                                       |38 ( 0.3)             |0 (  0.0)              |0 (  0.0)             |38 (  2.3)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |KOR                                       |36 ( 0.3)             |0 (  0.0)              |0 (  0.0)             |36 (  2.2)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |LUXEMBOURG                                |1 ( 0.0)              |0 (  0.0)              |1 (  0.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |MEX                                       |312 ( 2.5)            |0 (  0.0)              |0 (  0.0)             |312 ( 19.2)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |North America                             |953 ( 7.6)            |953 ( 92.3)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |NORWAY                                    |127 ( 1.0)            |0 (  0.0)              |127 ( 43.9)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |PORTUGAL                                  |3 ( 0.0)              |0 (  0.0)              |3 (  1.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |RUS                                       |112 ( 0.9)            |0 (  0.0)              |0 (  0.0)             |112 (  6.9)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |Spain                                     |397 ( 3.2)            |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |110 (100.0)           |0 (  0.0)            |287 (100.0)           |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |SPAIN                                     |13 ( 0.1)             |0 (  0.0)              |13 (  4.5)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |UK                                        |282 ( 2.3)            |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |282 (100.0)            |0 (  0.0)            |        |
|                          |USA                                       |340 ( 2.7)            |0 (  0.0)              |0 (  0.0)             |340 ( 20.9)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |NA                                        |211 ( 1.7)            |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |211 (100.0)            |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|vacc (%)                  |0                                         |6896 (55.3)           |1033 (100.0)           |182 ( 63.0)           |0 (  0.0)             |108 ( 98.2)           |0 (  0.0)            |250 ( 87.1)           |4710 ( 57.9)          |211 (100.0)            |289 (100.0)          |0 (  0.0)              |113 ( 97.4)          |16.1    |
|                          |1                                         |3564 (28.6)           |0 (  0.0)              |102 ( 35.3)           |0 (  0.0)             |2 (  1.8)             |0 (  0.0)            |37 ( 12.9)            |3420 ( 42.1)          |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |3 (  2.6)            |        |
|                          |NA                                        |2010 (16.1)           |0 (  0.0)              |5 (  1.7)             |1626 (100.0)          |0 (  0.0)             |97 (100.0)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |282 (100.0)            |0 (  0.0)            |        |
|sympdur (median [IQR])    |                                          |10.00 [7.00, 12.00]   |8.00 [5.00, 10.00]     |9.00 [7.00, 11.00]    |10.00 [8.00, 13.00]   |7.00 [5.00, 9.00]     |7.00 [6.00, 10.00]   |8.00 [5.00, 10.00]    |10.00 [7.00, 12.00]   |NA [NA, NA]            |9.00 [7.00, 11.00]   |10.00 [7.00, 13.00]    |8.00 [6.00, 10.00]   |1.9     |
|icu (%)                   |0                                         |2513 (20.2)           |0 (  0.0)              |160 ( 55.4)           |1525 ( 93.8)          |110 (100.0)           |97 (100.0)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |235 ( 81.3)          |270 ( 95.7)            |116 (100.0)          |77.5    |
|                          |1                                         |296 ( 2.4)            |0 (  0.0)              |129 ( 44.6)           |101 (  6.2)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |54 ( 18.7)           |12 (  4.3)             |0 (  0.0)            |        |
|                          |NA                                        |9661 (77.5)           |1033 (100.0)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |287 (100.0)           |8130 (100.0)          |211 (100.0)            |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|clinstatus_baseline (%)   |2                                         |1011 ( 8.1)           |142 ( 13.7)            |0 (  0.0)             |186 ( 11.4)           |35 ( 31.8)            |2 (  2.1)            |74 ( 25.8)            |465 (  5.7)           |0 (  0.0)              |71 ( 24.6)           |11 (  3.9)             |25 ( 21.6)           |0.1     |
|                          |3                                         |7810 (62.6)           |564 ( 54.6)            |0 (  0.0)             |962 ( 59.2)           |75 ( 68.2)            |95 ( 97.9)           |208 ( 72.5)           |5504 ( 67.7)          |0 (  0.0)              |181 ( 62.6)          |130 ( 46.1)            |91 ( 78.4)           |        |
|                          |4                                         |2937 (23.6)           |216 ( 20.9)            |249 ( 86.2)           |370 ( 22.8)           |0 (  0.0)             |0 (  0.0)            |5 (  1.7)             |1921 ( 23.6)          |0 (  0.0)              |37 ( 12.8)           |139 ( 49.3)            |0 (  0.0)            |        |
|                          |5                                         |705 ( 5.7)            |111 ( 10.7)            |40 ( 13.8)            |101 (  6.2)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |240 (  3.0)           |211 (100.0)            |0 (  0.0)            |2 (  0.7)              |0 (  0.0)            |        |
|                          |NA                                        |7 ( 0.1)              |0 (  0.0)              |0 (  0.0)             |7 (  0.4)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|comorb_cat (%)            |1                                         |5322 (42.7)           |169 ( 16.4)            |106 ( 36.7)           |370 ( 22.8)           |37 ( 33.6)            |41 ( 42.3)           |64 ( 22.3)            |4353 ( 53.5)          |24 ( 11.4)             |65 ( 22.5)           |49 ( 17.4)             |44 ( 37.9)           |0.0     |
|                          |2                                         |3773 (30.3)           |288 ( 27.9)            |73 ( 25.3)            |477 ( 29.3)           |40 ( 36.4)            |22 ( 22.7)           |83 ( 28.9)            |2538 ( 31.2)          |43 ( 20.4)             |99 ( 34.3)           |64 ( 22.7)             |46 ( 39.7)           |        |
|                          |3                                         |3277 (26.3)           |546 ( 52.9)            |99 ( 34.3)            |761 ( 46.8)           |31 ( 28.2)            |32 ( 33.0)           |140 ( 48.8)           |1239 ( 15.2)          |144 ( 68.2)            |125 ( 43.3)          |134 ( 47.5)            |26 ( 22.4)           |        |
|                          |4                                         |98 ( 0.8)             |30 (  2.9)             |11 (  3.8)            |18 (  1.1)            |2 (  1.8)             |2 (  2.1)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |35 ( 12.4)             |0 (  0.0)            |        |
|comed_cat (%)             |1                                         |1918 (15.4)           |991 ( 95.9)            |18 (  6.2)            |328 ( 20.2)           |94 ( 85.5)            |0 (  0.0)            |0 (  0.0)             |349 (  4.3)           |33 ( 15.6)             |62 ( 21.5)           |36 ( 12.8)             |7 (  6.0)            |0.1     |
|                          |2                                         |7890 (63.3)           |42 (  4.1)             |270 ( 93.4)           |1291 ( 79.4)          |16 ( 14.5)            |97 (100.0)           |269 ( 93.7)           |5157 ( 63.4)          |178 ( 84.4)            |227 ( 78.5)          |234 ( 83.0)            |109 ( 94.0)          |        |
|                          |3                                         |2621 (21.0)           |0 (  0.0)              |1 (  0.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |18 (  6.3)            |2590 ( 31.9)          |0 (  0.0)              |0 (  0.0)            |12 (  4.3)             |0 (  0.0)            |        |
|                          |4                                         |34 ( 0.3)             |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |34 (  0.4)            |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |NA                                        |7 ( 0.1)              |0 (  0.0)              |0 (  0.0)             |7 (  0.4)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|comed_rdv (%)             |0                                         |9003 (72.2)           |0 (  0.0)              |281 ( 97.2)           |1337 ( 82.2)          |110 (100.0)           |0 (  0.0)            |243 ( 84.7)           |6474 ( 79.6)          |97 ( 46.0)             |289 (100.0)          |159 ( 56.4)            |13 ( 11.2)           |0.0     |
|                          |1                                         |3467 (27.8)           |1033 (100.0)           |8 (  2.8)             |289 ( 17.8)           |0 (  0.0)             |97 (100.0)           |44 ( 15.3)            |1656 ( 20.4)          |114 ( 54.0)            |0 (  0.0)            |123 ( 43.6)            |103 ( 88.8)          |        |
|crp (median [IQR])        |                                          |86.00 [42.00, 148.00] |125.75 [64.93, 190.50] |89.00 [50.00, 142.00] |65.00 [32.00, 122.90] |79.15 [39.72, 134.60] |68.00 [33.50, 84.00] |66.85 [31.22, 113.05] |86.00 [43.00, 145.00] |167.65 [74.50, 346.02] |NA [NA, NA]          |109.00 [67.00, 159.00] |4.90 [2.40, 9.75]    |6.7     |
|sero (%)                  |0                                         |594 ( 4.8)            |0 (  0.0)              |7 (  2.4)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |587 (  7.2)           |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |86.0    |
|                          |1                                         |1157 ( 9.3)           |0 (  0.0)              |132 ( 45.7)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |1025 ( 12.6)          |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |NA                                        |10719 (86.0)          |1033 (100.0)           |150 ( 51.9)           |1626 (100.0)          |110 (100.0)           |97 (100.0)           |287 (100.0)           |6518 ( 80.2)          |211 (100.0)            |289 (100.0)          |282 (100.0)            |116 (100.0)          |        |
|vl_baseline (%)           |0                                         |9594 (76.9)           |526 ( 50.9)            |116 ( 40.1)           |1494 ( 91.9)          |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |7195 ( 88.5)          |0 (  0.0)              |0 (  0.0)            |263 ( 93.3)            |0 (  0.0)            |20.9    |
|                          |1                                         |265 ( 2.1)            |167 ( 16.2)            |18 (  6.2)            |43 (  2.6)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |18 (  0.2)            |0 (  0.0)              |0 (  0.0)            |19 (  6.7)             |0 (  0.0)            |        |
|                          |NA                                        |2611 (20.9)           |340 ( 32.9)            |155 ( 53.6)           |89 (  5.5)            |110 (100.0)           |97 (100.0)           |287 (100.0)           |917 ( 11.3)           |211 (100.0)            |289 (100.0)          |0 (  0.0)              |116 (100.0)          |        |
|variant (%)               |Delta                                     |21 ( 0.2)             |0 (  0.0)              |21 (  7.3)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |99.7    |
|                          |Omicron                                   |10 ( 0.1)             |0 (  0.0)              |10 (  3.5)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |PRESENCE OF MUTATIONS E484Q AND L452R     |1 ( 0.0)              |0 (  0.0)              |1 (  0.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |NA                                        |12438 (99.7)          |1033 (100.0)           |257 ( 88.9)           |1626 (100.0)          |110 (100.0)           |97 (100.0)           |287 (100.0)           |8130 (100.0)          |211 (100.0)            |289 (100.0)          |282 (100.0)            |116 (100.0)          |        |
|mort_28 (%)               |0                                         |10601 (85.0)          |925 ( 89.5)            |241 ( 83.4)           |1312 ( 80.7)          |105 ( 95.5)           |90 ( 92.8)           |268 ( 93.4)           |6943 ( 85.4)          |92 ( 43.6)             |277 ( 95.8)          |233 ( 82.6)            |115 ( 99.1)          |2.6     |
|                          |1                                         |1548 (12.4)           |61 (  5.9)             |36 ( 12.5)            |211 ( 13.0)           |2 (  1.8)             |7 (  7.2)            |8 (  2.8)             |1058 ( 13.0)          |117 ( 55.5)            |12 (  4.2)           |35 ( 12.4)             |1 (  0.9)            |        |
|                          |NA                                        |321 ( 2.6)            |47 (  4.5)             |12 (  4.2)            |103 (  6.3)           |3 (  2.7)             |0 (  0.0)            |11 (  3.8)            |129 (  1.6)           |2 (  0.9)              |0 (  0.0)            |14 (  5.0)             |0 (  0.0)            |        |
|mort_60 (%)               |0                                         |10515 (84.3)          |925 ( 89.5)            |231 ( 79.9)           |1249 ( 76.8)          |105 ( 95.5)           |90 ( 92.8)           |266 ( 92.7)           |6943 ( 85.4)          |92 ( 43.6)             |277 ( 95.8)          |222 ( 78.7)            |115 ( 99.1)          |2.8     |
|                          |1                                         |1605 (12.9)           |61 (  5.9)             |46 ( 15.9)            |249 ( 15.3)           |2 (  1.8)             |7 (  7.2)            |10 (  3.5)            |1058 ( 13.0)          |117 ( 55.5)            |12 (  4.2)           |42 ( 14.9)             |1 (  0.9)            |        |
|                          |NA                                        |350 ( 2.8)            |47 (  4.5)             |12 (  4.2)            |128 (  7.9)           |3 (  2.7)             |0 (  0.0)            |11 (  3.8)            |129 (  1.6)           |2 (  0.9)              |0 (  0.0)            |18 (  6.4)             |0 (  0.0)            |        |
|death_reached (%)         |0                                         |10865 (87.1)          |972 ( 94.1)            |243 ( 84.1)           |1377 ( 84.7)          |108 ( 98.2)           |90 ( 92.8)           |277 ( 96.5)           |7072 ( 87.0)          |94 ( 44.5)             |277 ( 95.8)          |240 ( 85.1)            |115 ( 99.1)          |0.0     |
|                          |1                                         |1605 (12.9)           |61 (  5.9)             |46 ( 15.9)            |249 ( 15.3)           |2 (  1.8)             |7 (  7.2)            |10 (  3.5)            |1058 ( 13.0)          |117 ( 55.5)            |12 (  4.2)           |42 ( 14.9)             |1 (  0.9)            |        |
|new_mv_28 (%)             |0                                         |9617 (77.1)           |768 ( 74.3)            |174 ( 60.2)           |1141 ( 70.2)          |97 ( 88.2)            |90 ( 92.8)           |255 ( 88.9)           |6487 ( 79.8)          |0 (  0.0)              |260 ( 90.0)          |232 ( 82.3)            |113 ( 97.4)          |17.7    |
|                          |1                                         |651 ( 5.2)            |91 (  8.8)             |37 ( 12.8)            |138 (  8.5)           |8 (  7.3)             |0 (  0.0)            |13 (  4.5)            |332 (  4.1)           |0 (  0.0)              |17 (  5.9)           |13 (  4.6)             |2 (  1.7)            |        |
|                          |NA                                        |2202 (17.7)           |174 ( 16.8)            |78 ( 27.0)            |347 ( 21.3)           |5 (  4.5)             |7 (  7.2)            |19 (  6.6)            |1311 ( 16.1)          |211 (100.0)            |12 (  4.2)           |37 ( 13.1)             |1 (  0.9)            |        |
|new_mvd_28 (%)            |0                                         |9974 (80.0)           |847 ( 82.0)            |205 ( 70.9)           |1142 ( 70.2)          |97 ( 88.2)            |90 ( 92.8)           |255 ( 88.9)           |6639 ( 81.7)          |92 ( 43.6)             |260 ( 90.0)          |234 ( 83.0)            |113 ( 97.4)          |2.0     |
|                          |1                                         |2251 (18.1)           |152 ( 14.7)            |73 ( 25.3)            |401 ( 24.7)           |10 (  9.1)            |7 (  7.2)            |21 (  7.3)            |1390 ( 17.1)          |117 ( 55.5)            |29 ( 10.0)           |48 ( 17.0)             |3 (  2.6)            |        |
|                          |NA                                        |245 ( 2.0)            |34 (  3.3)             |11 (  3.8)            |83 (  5.1)            |3 (  2.7)             |0 (  0.0)            |11 (  3.8)            |101 (  1.2)           |2 (  0.9)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|clinstatus_28_imp (%)     |1                                         |9573 (76.8)           |824 ( 79.8)            |187 ( 64.7)           |1212 ( 74.5)          |103 ( 93.6)           |90 ( 92.8)           |165 ( 57.5)           |6405 ( 78.8)          |0 (  0.0)              |263 ( 91.0)          |214 ( 75.9)            |110 ( 94.8)          |1.7     |
|                          |2                                         |155 ( 1.2)            |19 (  1.8)             |9 (  3.1)             |24 (  1.5)            |0 (  0.0)             |0 (  0.0)            |63 ( 22.0)            |30 (  0.4)            |0 (  0.0)              |3 (  1.0)            |7 (  2.5)              |0 (  0.0)            |        |
|                          |3                                         |456 ( 3.7)            |39 (  3.8)             |18 (  6.2)            |66 (  4.1)            |3 (  2.7)             |0 (  0.0)            |38 ( 13.2)            |275 (  3.4)           |0 (  0.0)              |5 (  1.7)            |11 (  3.9)             |1 (  0.9)            |        |
|                          |4                                         |341 ( 2.7)            |23 (  2.2)             |13 (  4.5)            |23 (  1.4)            |0 (  0.0)             |0 (  0.0)            |6 (  2.1)             |264 (  3.2)           |0 (  0.0)              |1 (  0.3)            |8 (  2.8)              |3 (  2.6)            |        |
|                          |5                                         |305 ( 2.4)            |67 (  6.5)             |26 (  9.0)            |90 (  5.5)            |2 (  1.8)             |0 (  0.0)            |7 (  2.4)             |101 (  1.2)           |0 (  0.0)              |5 (  1.7)            |6 (  2.1)              |1 (  0.9)            |        |
|                          |6                                         |1429 (11.5)           |61 (  5.9)             |36 ( 12.5)            |211 ( 13.0)           |2 (  1.8)             |7 (  7.2)            |8 (  2.8)             |1055 ( 13.0)          |0 (  0.0)              |12 (  4.2)           |36 ( 12.8)             |1 (  0.9)            |        |
|                          |NA                                        |211 ( 1.7)            |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |211 (100.0)            |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|discharge_reached (%)     |0                                         |2770 (22.2)           |194 ( 18.8)            |96 ( 33.2)            |406 ( 25.0)           |7 (  6.4)             |7 (  7.2)            |122 ( 42.5)           |1678 ( 20.6)          |165 ( 78.2)            |26 (  9.0)           |63 ( 22.3)             |6 (  5.2)            |0.0     |
|                          |1                                         |9700 (77.8)           |839 ( 81.2)            |193 ( 66.8)           |1220 ( 75.0)          |103 ( 93.6)           |90 ( 92.8)           |165 ( 57.5)           |6452 ( 79.4)          |46 ( 21.8)             |263 ( 91.0)          |219 ( 77.7)            |110 ( 94.8)          |        |
|discharge_reached_sus (%) |0                                         |2780 (22.3)           |194 ( 18.8)            |98 ( 33.9)            |414 ( 25.5)           |7 (  6.4)             |7 (  7.2)            |122 ( 42.5)           |1678 ( 20.6)          |165 ( 78.2)            |26 (  9.0)           |63 ( 22.3)             |6 (  5.2)            |0.0     |
|                          |1                                         |9690 (77.7)           |839 ( 81.2)            |191 ( 66.1)           |1212 ( 74.5)          |103 ( 93.6)           |90 ( 92.8)           |165 ( 57.5)           |6452 ( 79.4)          |46 ( 21.8)             |263 ( 91.0)          |219 ( 77.7)            |110 ( 94.8)          |        |
|ae_28 (%)                 |0                                         |10411 (83.5)          |572 ( 55.4)            |165 ( 57.1)           |1104 ( 67.9)          |85 ( 77.3)            |96 ( 99.0)           |247 ( 86.1)           |7505 ( 92.3)          |98 ( 46.4)             |230 ( 79.6)          |215 ( 76.2)            |94 ( 81.0)           |0.0     |
|                          |1                                         |2059 (16.5)           |461 ( 44.6)            |124 ( 42.9)           |522 ( 32.1)           |25 ( 22.7)            |1 (  1.0)            |40 ( 13.9)            |625 (  7.7)           |113 ( 53.6)            |59 ( 20.4)           |67 ( 23.8)             |22 ( 19.0)           |        |
|ae_28_sev (%)             |0                                         |10411 (83.5)          |572 ( 55.4)            |165 ( 57.1)           |1104 ( 67.9)          |85 ( 77.3)            |96 ( 99.0)           |247 ( 86.1)           |7505 ( 92.3)          |98 ( 46.4)             |230 ( 79.6)          |215 ( 76.2)            |94 ( 81.0)           |0.0     |
|                          |1                                         |1234 ( 9.9)           |200 ( 19.4)            |61 ( 21.1)            |205 ( 12.6)           |14 ( 12.7)            |1 (  1.0)            |31 ( 10.8)            |560 (  6.9)           |46 ( 21.8)             |44 ( 15.2)           |50 ( 17.7)             |22 ( 19.0)           |        |
|                          |2                                         |339 ( 2.7)            |80 (  7.7)             |25 (  8.7)            |132 (  8.1)           |4 (  3.6)             |0 (  0.0)            |5 (  1.7)             |58 (  0.7)            |18 (  8.5)             |8 (  2.8)            |9 (  3.2)              |0 (  0.0)            |        |
|                          |3                                         |178 ( 1.4)            |52 (  5.0)             |16 (  5.5)            |76 (  4.7)            |1 (  0.9)             |0 (  0.0)            |1 (  0.3)             |5 (  0.1)             |20 (  9.5)             |2 (  0.7)            |5 (  1.8)              |0 (  0.0)            |        |
|                          |4                                         |97 ( 0.8)             |43 (  4.2)             |5 (  1.7)             |30 (  1.8)            |2 (  1.8)             |0 (  0.0)            |2 (  0.7)             |2 (  0.0)             |9 (  4.3)              |2 (  0.7)            |2 (  0.7)              |0 (  0.0)            |        |
|                          |5                                         |71 ( 0.6)             |20 (  1.9)             |7 (  2.4)             |35 (  2.2)            |2 (  1.8)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |5 (  2.4)              |1 (  0.3)            |1 (  0.4)              |0 (  0.0)            |        |
|                          |6                                         |41 ( 0.3)             |13 (  1.3)             |3 (  1.0)             |16 (  1.0)            |2 (  1.8)             |0 (  0.0)            |1 (  0.3)             |0 (  0.0)             |4 (  1.9)              |2 (  0.7)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |7                                         |31 ( 0.2)             |16 (  1.5)             |2 (  0.7)             |11 (  0.7)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |2 (  0.9)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |8                                         |17 ( 0.1)             |11 (  1.1)             |2 (  0.7)             |2 (  0.1)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |2 (  0.9)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |9                                         |17 ( 0.1)             |9 (  0.9)              |1 (  0.3)             |7 (  0.4)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |10                                        |8 ( 0.1)              |6 (  0.6)              |1 (  0.3)             |1 (  0.1)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |11                                        |4 ( 0.0)              |2 (  0.2)              |0 (  0.0)             |1 (  0.1)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |1 (  0.5)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |12                                        |5 ( 0.0)              |2 (  0.2)              |0 (  0.0)             |2 (  0.1)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |1 (  0.5)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |13                                        |4 ( 0.0)              |1 (  0.1)              |0 (  0.0)             |2 (  0.1)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |1 (  0.5)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |14                                        |3 ( 0.0)              |0 (  0.0)              |1 (  0.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |2 (  0.9)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |15                                        |3 ( 0.0)              |2 (  0.2)              |0 (  0.0)             |1 (  0.1)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |16                                        |1 ( 0.0)              |1 (  0.1)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |17                                        |1 ( 0.0)              |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |1 (  0.5)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |19                                        |3 ( 0.0)              |1 (  0.1)              |0 (  0.0)             |1 (  0.1)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |1 (  0.5)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |20                                        |1 ( 0.0)              |1 (  0.1)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |26                                        |1 ( 0.0)              |1 (  0.1)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0 (  0.0)              |0 (  0.0)            |        |
|vir_clear_5 (%)           |0                                         |8774 (70.4)           |496 ( 48.0)            |91 ( 31.5)            |734 ( 45.1)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |7418 ( 91.2)          |0 (  0.0)              |0 (  0.0)            |35 ( 12.4)             |0 (  0.0)            |24.5    |
|                          |1                                         |639 ( 5.1)            |251 ( 24.3)            |30 ( 10.4)            |244 ( 15.0)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |99 (  1.2)            |0 (  0.0)              |0 (  0.0)            |15 (  5.3)             |0 (  0.0)            |        |
|                          |NA                                        |3057 (24.5)           |286 ( 27.7)            |168 ( 58.1)           |648 ( 39.9)           |110 (100.0)           |97 (100.0)           |287 (100.0)           |613 (  7.5)           |211 (100.0)            |289 (100.0)          |232 ( 82.3)            |116 (100.0)          |        |
|vir_clear_10 (%)          |0                                         |8792 (70.5)           |456 ( 44.1)            |69 ( 23.9)            |654 ( 40.2)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |7575 ( 93.2)          |0 (  0.0)              |0 (  0.0)            |38 ( 13.5)             |0 (  0.0)            |22.1    |
|                          |1                                         |924 ( 7.4)            |299 ( 28.9)            |58 ( 20.1)            |406 ( 25.0)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |135 (  1.7)           |0 (  0.0)              |0 (  0.0)            |26 (  9.2)             |0 (  0.0)            |        |
|                          |NA                                        |2754 (22.1)           |278 ( 26.9)            |162 ( 56.1)           |566 ( 34.8)           |110 (100.0)           |97 (100.0)           |287 (100.0)           |420 (  5.2)           |211 (100.0)            |289 (100.0)          |218 ( 77.3)            |116 (100.0)          |        |
|vir_clear_15 (%)          |0                                         |8708 (69.8)           |398 ( 38.5)            |57 ( 19.7)            |584 ( 35.9)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |7633 ( 93.9)          |0 (  0.0)              |0 (  0.0)            |36 ( 12.8)             |0 (  0.0)            |21.2    |
|                          |1                                         |1123 ( 9.0)           |361 ( 34.9)            |71 ( 24.6)            |512 ( 31.5)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |150 (  1.8)           |0 (  0.0)              |0 (  0.0)            |29 ( 10.3)             |0 (  0.0)            |        |
|                          |NA                                        |2639 (21.2)           |274 ( 26.5)            |161 ( 55.7)           |530 ( 32.6)           |110 (100.0)           |97 (100.0)           |287 (100.0)           |347 (  4.3)           |211 (100.0)            |289 (100.0)          |217 ( 77.0)            |116 (100.0)          |        |

# RUXCOVID

```r
## load RUXCOVID
ruxcovid_baseline_table <- readRDS("table_baseline_ruxcovid_07052024.rds")
ruxcovid_all_table <- readRDS("table_missing_trial_ruxcovid.rds")

#print
kable(ruxcovid_baseline_table, format = "markdown", table.attr = 'class="table"', caption = "RUXCOVID baseline table") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```

```
## Warning in kable_styling(., bootstrap_options = "striped", full_width = FALSE):
## Please specify format in kable. kableExtra can customize either HTML or LaTeX
## outputs. See https://haozhu233.github.io/kableExtra/ for details.
```



Table: RUXCOVID baseline table

|                                                              |level                                                                                     |Overall              |JAK inhibitor        |No JAK inhibitor     |Missing |
|:-------------------------------------------------------------|:-----------------------------------------------------------------------------------------|:--------------------|:--------------------|:--------------------|:-------|
|n                                                             |                                                                                          |432                  |287                  |145                  |        |
|Trial (%)                                                     |RUXCOVID                                                                                  |432 (100.0)          |287 (100.0)          |145 (100.0)          |0.0     |
|Age, years (median [IQR])                                     |                                                                                          |57.00 [47.00, 67.00] |57.00 [47.00, 67.00] |57.00 [52.00, 67.00] |0.0     |
|Sex (%)                                                       |Female                                                                                    |197 ( 45.6)          |125 ( 43.6)          |72 ( 49.7)           |0.0     |
|                                                              |Male                                                                                      |235 ( 54.4)          |162 ( 56.4)          |73 ( 50.3)           |        |
|Ethnicity (%)                                                 |American Indian or Alaska Native                                                          |39 (  9.3)           |26 (  9.2)           |13 (  9.4)           |2.5     |
|                                                              |Asian                                                                                     |10 (  2.4)           |5 (  1.8)            |5 (  3.6)            |        |
|                                                              |Black or African American                                                                 |15 (  3.6)           |6 (  2.1)            |9 (  6.5)            |        |
|                                                              |Caucasian                                                                                 |351 ( 83.4)          |242 ( 85.8)          |109 ( 78.4)          |        |
|                                                              |Hispanic or Latino                                                                        |1 (  0.2)            |0 (  0.0)            |1 (  0.7)            |        |
|                                                              |Mixed                                                                                     |5 (  1.2)            |3 (  1.1)            |2 (  1.4)            |        |
|Vaccination (%)                                               |No SARS CoV-2 vaccine                                                                     |432 (100.0)          |287 (100.0)          |145 (100.0)          |0.0     |
|Time from symptom onset to randomisation, days (median [IQR]) |                                                                                          |11.00 [8.00, 14.00]  |11.00 [8.00, 14.00]  |11.00 [8.00, 13.25]  |0.2     |
|Patients admitted to intensive care unit (%)                  |No                                                                                        |432 (100.0)          |287 (100.0)          |145 (100.0)          |0.0     |
|Clinical status on ordinal scale (%)                          |2: Hospitalised without need for oxygen therapy (WHO score 4)                             |141 ( 32.7)          |94 ( 32.9)           |47 ( 32.4)           |0.2     |
|                                                              |3: Hospitalised with need for supplemental low-flow oxygen (WHO score 5)                  |268 ( 62.2)          |175 ( 61.2)          |93 ( 64.1)           |        |
|                                                              |4: Hospitalised with need for high-flow oxygen or non- invasive ventilation (WHO score 6) |22 (  5.1)           |17 (  5.9)           |5 (  3.4)            |        |
|                                                              |5: Hospitalised with need for mechanical ventilation or ECMO (WHO score 7â€“9)            |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|Comorbidities (%)                                             |No comorbidity                                                                            |106 ( 24.5)          |79 ( 27.5)           |27 ( 18.6)           |0.0     |
|                                                              |One comorbidity                                                                           |136 ( 31.5)          |86 ( 30.0)           |50 ( 34.5)           |        |
|                                                              |Multiple comorbidities                                                                    |190 ( 44.0)          |122 ( 42.5)          |68 ( 46.9)           |        |
|                                                              |Immunocompromised                                                                         |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|Dexamethasone and Tocilizumab (%)                             |No Dexamethasone, no Tocilizumab                                                          |183 ( 42.4)          |117 ( 40.8)          |66 ( 45.5)           |0.0     |
|                                                              |Dexamethasone but no Tocilizumab                                                          |249 ( 57.6)          |170 ( 59.2)          |79 ( 54.5)           |        |
|                                                              |Dexamethasone and Tocilizumab                                                             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                              |Tocolizumab but no Dexamethasone                                                          |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|Remdesivir (%)                                                |No Remdesivir                                                                             |404 ( 93.5)          |266 ( 92.7)          |138 ( 95.2)          |0.0     |
|                                                              |Remdesivir                                                                                |28 (  6.5)           |21 (  7.3)           |7 (  4.8)            |        |
|C-reactive protein concentration, mg/L (median [IQR])         |                                                                                          |44.61 [16.61, 89.95] |42.40 [16.60, 93.20] |45.00 [16.74, 81.60] |2.3     |

```r
#print
kable(ruxcovid_all_table, format = "markdown", table.attr = 'class="table"', caption = "RUXCOVID table, all covariates") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```

```
## Warning in kable_styling(., bootstrap_options = "striped", full_width = FALSE):
## Please specify format in kable. kableExtra can customize either HTML or LaTeX
## outputs. See https://haozhu233.github.io/kableExtra/ for details.
```



Table: RUXCOVID table, all covariates

|                          |level                            |Overall              |0                    |1                    |Missing |
|:-------------------------|:--------------------------------|:--------------------|:--------------------|:--------------------|:-------|
|n                         |                                 |432                  |145                  |287                  |        |
|trial (%)                 |RUXCOVID                         |432 (100.0)          |145 (100.0)          |287 (100.0)          |0.0     |
|age (median [IQR])        |                                 |57.00 [47.00, 67.00] |57.00 [52.00, 67.00] |57.00 [47.00, 67.00] |0.0     |
|sex (%)                   |female                           |197 ( 45.6)          |72 ( 49.7)           |125 ( 43.6)          |0.0     |
|                          |male                             |235 ( 54.4)          |73 ( 50.3)           |162 ( 56.4)          |        |
|ethn (%)                  |AMERICAN INDIAN OR ALASKA NATIVE |39 (  9.0)           |13 (  9.0)           |26 (  9.1)           |0.0     |
|                          |ASIAN                            |10 (  2.3)           |5 (  3.4)            |5 (  1.7)            |        |
|                          |BLACK OR AFRICAN AMERICAN        |15 (  3.5)           |9 (  6.2)            |6 (  2.1)            |        |
|                          |HISPANIC OR LATINO               |1 (  0.2)            |1 (  0.7)            |0 (  0.0)            |        |
|                          |MULTIPLE                         |5 (  1.2)            |2 (  1.4)            |3 (  1.0)            |        |
|                          |UNKNOWN                          |11 (  2.5)           |6 (  4.1)            |5 (  1.7)            |        |
|                          |WHITE                            |351 ( 81.2)          |109 ( 75.2)          |242 ( 84.3)          |        |
|vacc (%)                  |0                                |432 (100.0)          |145 (100.0)          |287 (100.0)          |0.0     |
|sympdur (median [IQR])    |                                 |11.00 [8.00, 14.00]  |11.00 [8.00, 13.25]  |11.00 [8.00, 14.00]  |0.2     |
|icu (%)                   |0                                |432 (100.0)          |145 (100.0)          |287 (100.0)          |0.0     |
|clinstatus_baseline (%)   |2                                |141 ( 32.6)          |47 ( 32.4)           |94 ( 32.8)           |0.2     |
|                          |3                                |268 ( 62.0)          |93 ( 64.1)           |175 ( 61.0)          |        |
|                          |4                                |22 (  5.1)           |5 (  3.4)            |17 (  5.9)           |        |
|                          |NA                               |1 (  0.2)            |0 (  0.0)            |1 (  0.3)            |        |
|comorb_cat (%)            |1                                |106 ( 24.5)          |27 ( 18.6)           |79 ( 27.5)           |0.0     |
|                          |2                                |136 ( 31.5)          |50 ( 34.5)           |86 ( 30.0)           |        |
|                          |3                                |190 ( 44.0)          |68 ( 46.9)           |122 ( 42.5)          |        |
|comed_cat (%)             |1                                |183 ( 42.4)          |66 ( 45.5)           |117 ( 40.8)          |0.0     |
|                          |3                                |249 ( 57.6)          |79 ( 54.5)           |170 ( 59.2)          |        |
|comed_rdv (%)             |0                                |404 ( 93.5)          |138 ( 95.2)          |266 ( 92.7)          |0.0     |
|                          |1                                |28 (  6.5)           |7 (  4.8)            |21 (  7.3)           |        |
|crp (median [IQR])        |                                 |44.61 [16.61, 89.95] |45.00 [16.74, 81.60] |42.40 [16.60, 93.20] |2.3     |
|mort_28 (%)               |0                                |412 ( 95.4)          |139 ( 95.9)          |273 ( 95.1)          |1.9     |
|                          |1                                |12 (  2.8)           |3 (  2.1)            |9 (  3.1)            |        |
|                          |NA                               |8 (  1.9)            |3 (  2.1)            |5 (  1.7)            |        |
|mort_60 (%)               |0                                |412 ( 95.4)          |139 ( 95.9)          |273 ( 95.1)          |1.9     |
|                          |1                                |12 (  2.8)           |3 (  2.1)            |9 (  3.1)            |        |
|                          |NA                               |8 (  1.9)            |3 (  2.1)            |5 (  1.7)            |        |
|death_reached (%)         |0                                |420 ( 97.2)          |142 ( 97.9)          |278 ( 96.9)          |0.0     |
|                          |1                                |12 (  2.8)           |3 (  2.1)            |9 (  3.1)            |        |
|new_mv_28 (%)             |0                                |395 ( 91.4)          |134 ( 92.4)          |261 ( 90.9)          |4.6     |
|                          |1                                |17 (  3.9)           |5 (  3.4)            |12 (  4.2)           |        |
|                          |NA                               |20 (  4.6)           |6 (  4.1)            |14 (  4.9)           |        |
|new_mvd_28 (%)            |0                                |395 ( 91.4)          |134 ( 92.4)          |261 ( 90.9)          |1.9     |
|                          |1                                |29 (  6.7)           |8 (  5.5)            |21 (  7.3)           |        |
|                          |NA                               |8 (  1.9)            |3 (  2.1)            |5 (  1.7)            |        |
|clinstatus_28_imp (%)     |1                                |397 ( 91.9)          |135 ( 93.1)          |262 ( 91.3)          |0.0     |
|                          |2                                |4 (  0.9)            |2 (  1.4)            |2 (  0.7)            |        |
|                          |3                                |7 (  1.6)            |2 (  1.4)            |5 (  1.7)            |        |
|                          |4                                |3 (  0.7)            |1 (  0.7)            |2 (  0.7)            |        |
|                          |5                                |9 (  2.1)            |2 (  1.4)            |7 (  2.4)            |        |
|                          |6                                |12 (  2.8)           |3 (  2.1)            |9 (  3.1)            |        |
|discharge_reached (%)     |0                                |30 (  6.9)           |7 (  4.8)            |23 (  8.0)           |1.6     |
|                          |1                                |395 ( 91.4)          |135 ( 93.1)          |260 ( 90.6)          |        |
|                          |NA                               |7 (  1.6)            |3 (  2.1)            |4 (  1.4)            |        |
|discharge_reached_sus (%) |0                                |30 (  6.9)           |7 (  4.8)            |23 (  8.0)           |1.6     |
|                          |1                                |395 ( 91.4)          |135 ( 93.1)          |260 ( 90.6)          |        |
|                          |NA                               |7 (  1.6)            |3 (  2.1)            |4 (  1.4)            |        |
|ae_28 (%)                 |0                                |364 ( 84.3)          |118 ( 81.4)          |246 ( 85.7)          |4.6     |
|                          |1                                |48 ( 11.1)           |21 ( 14.5)           |27 (  9.4)           |        |
|                          |NA                               |20 (  4.6)           |6 (  4.1)            |14 (  4.9)           |        |
|ae_28_sev (%)             |0                                |364 ( 84.3)          |118 ( 81.4)          |246 ( 85.7)          |4.6     |
|                          |1                                |28 (  6.5)           |11 (  7.6)           |17 (  5.9)           |        |
|                          |2                                |12 (  2.8)           |7 (  4.8)            |5 (  1.7)            |        |
|                          |3                                |2 (  0.5)            |1 (  0.7)            |1 (  0.3)            |        |
|                          |4                                |2 (  0.5)            |1 (  0.7)            |1 (  0.3)            |        |
|                          |5                                |2 (  0.5)            |0 (  0.0)            |2 (  0.7)            |        |
|                          |6                                |1 (  0.2)            |0 (  0.0)            |1 (  0.3)            |        |
|                          |10                               |1 (  0.2)            |1 (  0.7)            |0 (  0.0)            |        |
|                          |NA                               |20 (  4.6)           |6 (  4.1)            |14 (  4.9)           |        |
