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
# df2 <- df2 %>% # fill up NA / CAVE: does not correspond with PersonID and Label, rather use Label instead and extract id from it
#   fill(SUBJID_DER)
df2$id_pat <- sub(".*N(\\d+).*", "\\1", df2$Label)
# df2 <- df2 %>% 
#   select(id_pat, SUBJID_DER, SUBJID_DER122, PersonId, DATE_DEATH, Label, everything())
num_unique_id_pat <- length(unique(df2$id_pat)) # correct, 282 unique IDs, as per publication in Bari + SOC group and as dataframe 1

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
### Days with symptoms prior to randomization
df$sympdur_date <- as.Date(df$ONSET_FIRSTD, format = "%d/%m/%Y") # randomisation date
df <- df %>% 
  mutate(sympdur = randdate - sympdur_date)
df %>% 
  drop_na(sympdur) %>% 
  ggplot(aes(x = sympdur)) +
  geom_density(fill = "blue", color = "black") +
  labs(title = "Density Plot of Symptom Duration",
       x = "Symptom Duration",
       y = "Density")
```

![](TACTIC-R_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

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
## DEXA/CORTICO
df_dexa <- df2 %>%
  filter(grepl("dexa|cort|glucoc|hydrocor|predn|solon|methyl|Neofordex|Glensoludex|Martapan", CM_NAME, ignore.case = TRUE) &
         !grepl("Dexafree|fludro|symb|pul|dakta|penici", CM_NAME, ignore.case = TRUE)) # exclude Dexafree, fludrocortisone, symbi/pulmicort, daktacort, Phenoxymethylpenicillin 
# unique(df_dexa$CM_NAME)

# exclude non-baseline medication and unsuitable dexa equivalents (e.g. see NIH guidelines):
# If dexamethasone is not available, alternative corticosteroids (e.g., prednisone, methylprednisolone, hydrocortisone) can be used, For these drugs, the total daily dose equivalencies to dexamethasone 6 mg (orally or intravenously) are:
# Prednisone 40 mg
# Methylprednisolone 32 mg
# Hydrocortisone 160 mg

df_dexa <- left_join(df_dexa, df[, c("randdate", "trt", "id_pat")], by = join_by(id_pat == id_pat))
df_dexa$cm_start_date <- as.Date(df_dexa$CM_START, format = "%d/%m/%Y") 
# df_dexa %>%
#   select(id_pat, SUBJID_DER1, SUBJID_DER2, SUBJID_DER3, CM_NAME, CM_DOSE, CMDOSU, CM_FREQ, CM_FREQ_OTH, CM_ROUTE, CMROUTEOTH, CM_START, CM_END, randdate, cm_start_date, trt) %>%
#   View()
df_dexa <- df_dexa %>% 
  mutate(cm_baseline = case_when(cm_start_date <= randdate + 1 ~ 1, TRUE ~ 0)) %>% 
  filter(cm_baseline == 1) %>% # only those with steroid use at baseline + 1 day or before
  filter(!(CM_NAME == "Prednisolone" & CM_DOSE == 1)) %>% # exclude low-dose predni, medical history medication
  filter(!(CM_NAME == "Prednislone" & CM_DOSE == 3)) %>%  # exclude low-dose predni, medical history medication
  distinct(id_pat) %>% # only 1 entry per patient
  mutate(comed_dexa = 1)
df <- left_join(df, df_dexa[, c("comed_dexa", "id_pat")], by = join_by(id_pat == id_pat)) # merge to main df
df <- df %>% 
  mutate(comed_dexa = case_when(is.na(comed_dexa) ~ 0,
                          TRUE ~ comed_dexa))
# addmargins(table(df$comed_dexa, df$trt, useNA = "always")) # corresponds closely to trial publication Table 1 but they show use over entire study period 

# REMDESIVIR
# REMDES_TRT_ENR & REMDES_TRT_ENR1 do not help, instead use info from concomitant medication forms
# unique(df2$CM_NAME)
df_rdv <- df2 %>%
  filter(grepl("rem|rdv|veklury|vek|klur", CM_NAME, ignore.case = TRUE))
# unique(df_rdv$CM_NAME)
df_rdv <- left_join(df_rdv, df[, c("randdate", "trt", "id_pat")], by = join_by(id_pat == id_pat))
df_rdv$cm_start_date <- as.Date(df_rdv$CM_START, format = "%d/%m/%Y") 
df_rdv <- df_rdv %>% 
  mutate(cm_baseline = case_when(cm_start_date <= randdate + 1 ~ 1, TRUE ~ 0)) %>% 
  filter(cm_baseline == 1) %>% # only those with use at baseline + 1 day or before
  distinct(id_pat) %>% # only 1 entry per patient
  mutate(comed_rdv = 1)
df <- left_join(df, df_rdv[, c("comed_rdv", "id_pat")], by = join_by(id_pat == id_pat)) # merge to main df
df <- df %>% 
  mutate(comed_rdv = case_when(is.na(comed_rdv) ~ 0,
                          TRUE ~ comed_rdv))
# addmargins(table(df$comed_rdv, df$trt, useNA = "always")) # corresponds closely to trial publication Table 1 but they show use over entire study period 

# TOCILIZUMAB and other IL-6 inhibitors
# unique(df2$CM_NAME)
df_toci <- df2 %>%
  filter(grepl("toc|toz|sari|zumab|actem|kevzara", CM_NAME, ignore.case = TRUE) & 
           !grepl("tocopher|metoc", CM_NAME, ignore.case = TRUE)) # exclude Alphatocopherol & Metoclopromide
# unique(df_toci$CM_NAME)
df_toci <- left_join(df_toci, df[, c("randdate", "trt", "id_pat")], by = join_by(id_pat == id_pat))
df_toci$cm_start_date <- as.Date(df_toci$CM_START, format = "%d/%m/%Y") 
df_toci <- df_toci %>% 
  mutate(cm_baseline = case_when(cm_start_date <= randdate + 1 ~ 1, TRUE ~ 0)) %>% 
  filter(cm_baseline == 1) %>% # only those with use at baseline + 1 day or before
  distinct(id_pat) %>% # only 1 entry per patient
  mutate(comed_toci = 1)
df <- left_join(df, df_toci[, c("comed_toci", "id_pat")], by = join_by(id_pat == id_pat)) # merge to main df
df <- df %>% 
  mutate(comed_toci = case_when(is.na(comed_toci) ~ 0,
                          TRUE ~ comed_toci))
# addmargins(table(df$comed_toci, df$trt, useNA = "always")) # corresponds closely to trial publication Table 1 since they show use over entire study period 

# ANTICOAGULATION
df_acoa <- df2 %>%
  filter(grepl("coag|hepar|LMWH|UFH|parin|fragmin|lovenox|innohep|clexa", CM_NAME, ignore.case = TRUE))
# unique(df_acoa$CM_NAME)
df_acoa <- left_join(df_acoa, df[, c("randdate", "trt", "id_pat")], by = join_by(id_pat == id_pat))
df_acoa$cm_start_date <- as.Date(df_acoa$CM_START, format = "%d/%m/%Y") 
df_acoa <- df_acoa %>% 
  mutate(cm_baseline = case_when(cm_start_date <= randdate + 1 ~ 1, TRUE ~ 0)) %>% 
  filter(cm_baseline == 1) %>% # only those with use at baseline + 1 day or before
  filter(!(grepl("enox", CM_NAME, ignore.case = TRUE) & CM_DOSE == 40 & (CM_FREQ == 1 | CM_FREQ == 2))) %>%  # exclude low-dose proph. enoxa / but keep twice daily 40mg
  distinct(id_pat) %>% # only 1 entry per patient
  mutate(comed_acoa = 1)
# df_acoa %>% # keep the enoxa 40mg twice daily
#   select(id_pat, SUBJID_DER1, SUBJID_DER2, SUBJID_DER3, CM_NAME, CM_DOSE, CMDOSU, CM_FREQ, CM_FREQ_OTH, CM_ROUTE, CMROUTEOTH, CM_START, CM_END, randdate, cm_start_date, trt) %>%
#   View()
df <- left_join(df, df_acoa[, c("comed_acoa", "id_pat")], by = join_by(id_pat == id_pat)) # merge to main df
df <- df %>% 
  mutate(comed_acoa = case_when(is.na(comed_acoa) ~ 0,
                          TRUE ~ comed_acoa))
# addmargins(table(df$comed_acoa, df$trt, useNA = "always")) 

# INTERFERON
df_interferon <- df2 %>%
  filter(grepl("IFN|interf|SNG001", CM_NAME, ignore.case = TRUE)) # not a single case (check protocol)
df$comed_interferon <- 0 # see protocol
# unique(df_interferon$CM_NAME)
# df_interferon <- left_join(df_interferon, df[, c("randdate", "trt", "id_pat")], by = join_by(id_pat == id_pat))
# df_interferon$cm_start_date <- as.Date(df_interferon$CM_START, format = "%d/%m/%Y") 
# df_interferon <- df_interferon %>% 
#   mutate(cm_baseline = case_when(cm_start_date <= randdate + 1 ~ 1, TRUE ~ 0)) %>% 
#   filter(cm_baseline == 1) %>% # only those with use at baseline + 1 day or before
#   filter(!(grepl("enox", CM_NAME, ignore.case = TRUE) & CM_DOSE == 40 & (CM_FREQ == 1 | CM_FREQ == 2))) %>%  # exclude low-dose proph. enoxa / but keep twice daily 40mg
#   distinct(id_pat) %>% # only 1 entry per patient
#   mutate(comed_interferon = 1)
# # df_interferon %>% 
# #   select(id_pat, SUBJID_DER1, SUBJID_DER2, SUBJID_DER3, CM_NAME, CM_DOSE, CMDOSU, CM_FREQ, CM_FREQ_OTH, CM_ROUTE, CMROUTEOTH, CM_START, CM_END, randdate, cm_start_date, trt) %>%
# #   View()
# df <- left_join(df, df_interferon[, c("comed_interferon", "id_pat")], by = join_by(id_pat == id_pat)) # merge to main df
# df <- df %>% 
#   mutate(comed_interferon = case_when(is.na(comed_interferon) ~ 0,
#                           TRUE ~ comed_interferon))
# addmargins(table(df$comed_interferon, df$trt, useNA = "always")) 

# ANTIBIOTICS
# df2 %>% 
#   select(id_pat, SUBJID_DER1, SUBJID_DER2, SUBJID_DER3, CM_NAME, CM_DOSE, CMDOSU, CM_FREQ, CM_FREQ_OTH, CM_ROUTE, CMROUTEOTH, CM_START, CM_END) %>%
#   View()
df_ab <- df2 %>%
  filter(grepl("amox|peni|cefo|ceft|cefu|cipro|clar|clotr|trimox|doxy|fluc|genta|levoflo|merop|metronid|tazo|piper|vanco", CM_NAME, ignore.case = TRUE))
# unique(df_ab$CM_NAME)
df_ab <- left_join(df_ab, df[, c("randdate", "trt", "id_pat")], by = join_by(id_pat == id_pat))
df_ab$cm_start_date <- as.Date(df_ab$CM_START, format = "%d/%m/%Y") 
df_ab <- df_ab %>% 
  mutate(cm_baseline = case_when(cm_start_date <= randdate + 1 ~ 1, TRUE ~ 0)) %>% 
  filter(cm_baseline == 1) %>% # only those with use at baseline + 1 day or before
  distinct(id_pat) %>% # only 1 entry per patient
  mutate(comed_ab = 1)
df <- left_join(df, df_ab[, c("comed_ab", "id_pat")], by = join_by(id_pat == id_pat)) # merge to main df
df <- df %>% 
  mutate(comed_ab = case_when(is.na(comed_ab) ~ 0,
                          TRUE ~ comed_ab))
# addmargins(table(df$comed_ab, df$trt, useNA = "always")) 

# ANY OTHER COMEDICATION
df_comed_other <- df2 %>%
  filter(!is.na(CM_NAME))
# unique(df_comed_other$CM_NAME)
df_comed_other <- left_join(df_comed_other, df[, c("randdate", "trt", "id_pat", "comed_ab", "comed_acoa", "comed_dexa", "comed_rdv", "comed_toci", "comed_interferon")], by = join_by(id_pat == id_pat))
df_comed_other$cm_start_date <- as.Date(df_comed_other$CM_START, format = "%d/%m/%Y") 
df_comed_other <- df_comed_other %>% 
  mutate(cm_baseline = case_when(cm_start_date <= randdate + 1 ~ 1, TRUE ~ 0)) %>% 
  filter(cm_baseline == 1) %>% # only those with use at baseline + 1 day or before
  # filter(comed_ab == 0 & comed_acoa == 0 & comed_dexa == 0 & comed_rdv == 0 & comed_toci & comed_interferon == 0) %>% # to be discussed, participants often have several
  distinct(id_pat) %>% # only 1 entry per patient
  mutate(comed_other = 1)
df <- left_join(df, df_comed_other[, c("comed_other", "id_pat")], by = join_by(id_pat == id_pat)) # merge to main df
df <- df %>% 
  mutate(comed_other = case_when(is.na(comed_other) ~ 0,
                          TRUE ~ comed_other))
# addmargins(table(df$comed_other, df$trt, useNA = "always")) # to be discussed, participants often have several

## GROUP them for the subgroup analysis, according to protocol
df <- df %>% # there are no missings in comed_dexa and comed_toci
  mutate(comed_cat = case_when(comed_dexa == 0 & comed_toci == 0 ~ 1, # patients without Dexa nor Toci
                               comed_dexa == 1 & comed_toci == 1 ~ 2, # patients with Dexa and Toci
                               comed_dexa == 1 & comed_toci == 0 ~ 3, # patients with Dexa but no Toci
                               comed_dexa == 0 & comed_toci == 1 ~ 4)) # patients with Toci but no Dexa
# addmargins(table(df$comed_cat, df$trt, useNA = "always"))


### Comorbidities at baseline
df <- df %>% # 3 missing
  mutate(comorb_lung = case_when(MH_ASTHMA == 1 | MH_OBSTRUCT == 1 | MH_LUNGDISEASE == 1 | MH_INTERSTIT == 1 ~ 1,
                                 MH_ASTHMA == 0 & MH_OBSTRUCT == 0 & MH_LUNGDISEASE == 0 & MH_INTERSTIT == 0 ~ 0))
df <- df %>% # no missing
  mutate(comorb_liver = case_when(MH_LIVERDIS == 1 ~ 1,
                                  MH_LIVERDIS == 0 ~ 0))
df <- df %>% # 1 missing
  mutate(comorb_cvd = case_when(MH_HEARTD == 1 | MH_STROKE == 1 | MH_ISCHAEMIC == 1 | MH_ATRIALFIB == 1 | MH_OTHCARDIAC == 1 ~ 1,
                                MH_HEARTD == 0 & MH_STROKE == 0 & MH_ISCHAEMIC == 0 & MH_ATRIALFIB == 0 & MH_OTHCARDIAC == 0 ~ 0))
df <- df %>% # no missing
  mutate(comorb_aht = case_when(MH_HYPT == 1 ~ 1,
                                MH_HYPT == 0 ~ 0))
df <- df %>% # no missing
  mutate(comorb_dm = case_when(MH_DIABETES == 1 ~ 1,
                               MH_DIABETES == 0 ~ 0))
df <- df %>% # 3 missing
  mutate(comorb_obese = case_when(MH_OBESE == 1 ~ 1,
                               MH_OBESE == 0 ~ 0))
df <- df %>% # 5 missing
  mutate(comorb_smoker = case_when(MH_SMOKING == 2 ~ 1,
                               MH_SMOKING == 1 | MH_SMOKING == 3 ~ 0))
df <- df %>% # no missing
  mutate(comorb_kidney = case_when(MH_CHRONKIDND == 1 ~ 1,
                               MH_CHRONKIDND == 0 ~ 0))
df <- df %>% # 1 missing
  mutate(comorb_cancer = case_when(MH_CANCERPAST == 1 | MH_CURRCANC == 1 ~ 1,
                               MH_CANCERPAST == 0 & MH_CURRCANC == 0 ~ 0))
df <- df %>% # no missing
  mutate(comorb_autoimm = case_when(MH_AUTOIMM == 1 ~ 1,
                               MH_AUTOIMM == 0 ~ 0))
df <- df %>% # no missing
  mutate(immunosupp = case_when(MH_IMMUNODEF == 1 | MH_PRONETOINF == 1 ~ 1,
                               MH_IMMUNODEF == 0 & MH_PRONETOINF == 0 ~ 0))

df <- df %>% 
  mutate(any_comorb = case_when(comorb_lung == 1 | comorb_liver == 1 | comorb_cvd == 1 |
                                  comorb_aht == 1 | comorb_dm == 1 | comorb_obese == 1 | comorb_smoker == 1
                                | immunosupp == 1 | comorb_cancer == 1 | comorb_autoimm == 1 | comorb_kidney == 1 
                                  ~ 1,
                                comorb_lung == 0 & comorb_liver == 0 & comorb_cvd == 0 &
                                  comorb_aht == 0 & comorb_dm == 0 & comorb_obese == 0 & comorb_smoker == 0
                                & immunosupp == 0 & comorb_cancer == 0 & comorb_autoimm == 0 & comorb_kidney == 0
                                ~ 0))
# table(df$any_comorb, useNA = "always") # 4 missing

# the remaining 4 missing have only NA in 1 comorb category => no evidence for comorbidity -> recode as 0
# df %>%
#   select(id_pat, any_comorb, comorb_lung, comorb_liver, comorb_cvd, comorb_aht, comorb_dm, comorb_obese, comorb_smoker, immunosupp, comorb_kidney, comorb_autoimm, comorb_cancer) %>%
#   filter(is.na(any_comorb)) %>%
#   View()
df <- df %>% 
  mutate(any_comorb = case_when(is.na(any_comorb) ~ 0,
                                TRUE ~ any_comorb))

## group them for the subgroup analysis, according to protocol // count all pre-defined comorbidities per patient first
comorb <- df %>% 
  select(id_pat, comorb_lung, comorb_liver, comorb_cvd, comorb_aht, comorb_dm, comorb_obese, comorb_smoker, immunosupp, comorb_kidney, comorb_autoimm, comorb_cancer)
comorb$comorb_count <- NA
for (i in 1:dim(comorb)[[1]]) {
  comorb$comorb_count[i] <- ifelse(
    sum(comorb[i, ] %in% c(1)) > 0,
    sum(comorb[i, ] %in% c(1)),
    NA
  )
}
comorb <- comorb %>% 
  mutate(comorb_count = case_when(comorb_lung == 0 & comorb_liver == 0 & comorb_cvd == 0 &
                                  comorb_aht == 0 & comorb_dm == 0 & comorb_obese == 0 & comorb_smoker == 0
                                & immunosupp == 0 & comorb_cancer == 0 & comorb_autoimm == 0 & comorb_kidney == 0 ~ 0,
                                TRUE ~ comorb_count))
# the remaining 4 missing have only NA in 1 comorb category => no evidence for comorbidity -> recode as 0
comorb <- comorb %>% 
  mutate(comorb_count = case_when(is.na(comorb_count) ~ 0,
                                TRUE ~ comorb_count))
df <- left_join(df, comorb[, c("comorb_count", "id_pat")], by = join_by(id_pat == id_pat)) ## merge imputed variable back
df <- df %>% # same 4 missing
  mutate(comorb_cat = case_when(immunosupp == 1 ~ 4, # immunocompromised
                                comorb_count == 0 ~ 1, # no comorbidity
                                comorb_count == 1 ~ 2, # one comorbidity
                                comorb_count >1 & (immunosupp == 0 | is.na(immunosupp)) ~ 3)) # multiple comorbidities
table(df$comorb_cat, useNA = "always")
```

```
## 
##    1    2    3    4 <NA> 
##   49   64  134   35    0
```

```r
df <- df %>%
  mutate(comorb_any = case_when(comorb_count == 0 ~ 0, # no comorbidity
                                comorb_count >0 ~ 1)) # any comorbidity


### CRP at baseline
# summary(df$CRP, useNA = "always")
df$crp_date <- as.Date(df$CRP_DAT, format = "%d/%m/%Y") 
df_crp <- df %>% 
  mutate(crp_baseline = case_when(crp_date <= (randdate + 1) ~ 1, TRUE ~ 0)) %>% 
  filter(crp_baseline == 1) %>% 
  rename(crp = CRP)
df <- left_join(df, df_crp[, c("crp", "id_pat")], by = join_by(id_pat == id_pat)) ## merge to main df
# summary(df$crp, useNA = "always")
df %>% 
  drop_na(crp) %>% 
  ggplot(aes(x = crp)) +
  geom_density(fill = "blue", color = "black") +
  labs(title = "Density Plot of CRP",
       x = "CRP",
       y = "Density")
```

![](TACTIC-R_files/figure-html/unnamed-chunk-2-3.png)<!-- -->

```r
### Viremia / Undetectable VL at baseline
# transform all vl date variables
df$vl_baseline_date <- as.Date(df$COVID19_TSTRCNT, format = "%d/%m/%Y") 
df <- df %>% 
  mutate(vl_baseline_mark = case_when(vl_baseline_date <= (randdate + 1) ~ 1,
                                      vl_baseline_date > (randdate + 1) ~ 0)) # mark the baseline VL, if after baseline mark as 0, otherwise NA
# table(df$vl_baseline_mark) # double-check with trial team, for n=4 probably data entry error
df <- df %>% 
  mutate(vl_baseline = case_when(COVID19_POS == 0 ~ 1, # 1=pos / 0=neg=undetectable
                                 COVID19_POS == 1 ~ 0)) 


# date_columns <- c("COVID19_TSTRCNT", "COVID19_RECDAT", "COVID19_RECDAT1", "COVID19_RECDAT2", "COVID19_RECDAT3", "COVID19_RECDAT4", "COVID19_RECDAT5", "COVID19_RECDAT6", "COVID19_RECDAT7", "COVID19_RECDAT8")
# df <- df %>%
#   mutate_at(vars(date_columns), list(~ as.Date(., format = "%d/%m/%Y")))
# df <- df %>% # earliest should be COVID19_TSTRCNT, but just to be sure and take all available data into account
#   mutate(vl_earliest_date = pmin(COVID19_TSTRCNT, COVID19_RECDAT, COVID19_RECDAT1, COVID19_RECDAT2, COVID19_RECDAT3, COVID19_RECDAT4, COVID19_RECDAT5, COVID19_RECDAT6, COVID19_RECDAT7, COVID19_RECDAT8, na.rm = TRUE)) %>% 
#   mutate(vl_baseline_mark = case_when(vl_earliest_date <= (randdate + 1) ~ 1)) %>% # mark the baseline VL, otherwise NA
#   mutate(vl_baseline_ori = case_when(vl_baseline_mark == 1 & !is.na(COVID19_TSTRCNT) & COVID19_TSTRCNT < COVID19_RECDAT ~ COVID19_RESULT,
#                                  vl_baseline_mark == 1 & is.na(COVID19_TSTRCNT) & !is.na(COVID19_RECDAT) & COVID19_RECDAT < COVID19_RECDAT1 ~ COVID19_RESULT1,
#                                  vl_baseline_mark == 1 & is.na(COVID19_TSTRCNT) & is.na(COVID19_RECDAT) & !is.na(COVID19_RECDAT1) & COVID19_RECDAT1 < COVID19_RECDAT2 ~ COVID19_RESULT2,
#                                  vl_baseline_mark == 1 & is.na(COVID19_TSTRCNT) & is.na(COVID19_RECDAT) & is.na(COVID19_RECDAT1) & !is.na(COVID19_RECDAT2) & COVID19_RECDAT2 < COVID19_RECDAT3 ~ COVID19_RESULT3,
#                                  vl_baseline_mark == 1 & is.na(COVID19_TSTRCNT) & is.na(COVID19_RECDAT) & is.na(COVID19_RECDAT1) & is.na(COVID19_RECDAT2) & !is.na(COVID19_RECDAT3) & COVID19_RECDAT3 < COVID19_RECDAT4 ~ COVID19_RESULT4,
#                                  vl_baseline_mark == 1 & is.na(COVID19_TSTRCNT) & is.na(COVID19_RECDAT) & is.na(COVID19_RECDAT1) & is.na(COVID19_RECDAT2) & is.na(COVID19_RECDAT3) & !is.na(COVID19_RECDAT4) & COVID19_RECDAT4 < COVID19_RECDAT5 ~ COVID19_RESULT5,
#                                  )) %>% 
#   mutate(vl_baseline = case_when(vl_baseline_ori == 0 ~ 1, # 1=pos / 0=neg=undetectable
#                                  vl_baseline_ori == 1 ~ 0)) 
# df %>% 
#   select(id_pat, randdate, vl_baseline_mark, vl_baseline, COVID19_TSTRCNT, COVID19_RESULT, 
#                               COVID19_RECDAT, COVID19_RESULT1,
#                               COVID19_RECDAT1, COVID19_RESULT2,
#                               COVID19_RECDAT2, COVID19_RESULT3,
#                               COVID19_RECDAT3, COVID19_RESULT4,
#          COVID19_RECDAT4, COVID19_RESULT5,
#          COVID19_RECDAT5, COVID19_RECDAT6, COVID19_RECDAT7, COVID19_RECDAT8) %>%
#   View()


### Serology at baseline // not available
### COVID-19 vaccination at baseline // not available
### Variant // not available
```
Discussion points:

# Endpoints

```r
### FIRST, extract all status info from dataframe 1
# transform all resp scores
point7_transform <- function(df, clinstatus_var, pos_var) {
  df <- df %>%
    mutate({{ clinstatus_var }} :=
             case_when({{ pos_var }} %in% c(6, 7) ~ 1,
                       {{ pos_var }} == 5 ~ 2,
                       {{ pos_var }} == 4 ~ 3,
                       {{ pos_var }} == 3 ~ 4,
                       {{ pos_var }} == 2 ~ 5,
                       {{ pos_var }} == 1 ~ 6)) %>%
    mutate({{ clinstatus_var }} := factor({{ clinstatus_var }}, levels = 1:6))
}
df <- point7_transform(df, clinstatus_1, POINT7_OS_POS1) ## POINT7_OS_POS1 = D2 = clinstatus_1 (1 day after clinstatus_baseline) / D1 = baseline/randomization day
df <- point7_transform(df, clinstatus_2, POINT7_OS_POS2)
df <- point7_transform(df, clinstatus_3, POINT7_OS_POS3)
df <- point7_transform(df, clinstatus_4, POINT7_OS_POS4)
df <- point7_transform(df, clinstatus_5, POINT7_OS_POS5)
df <- point7_transform(df, clinstatus_6, POINT7_OS_POS6) 
df <- point7_transform(df, clinstatus_7, POINT7_OS_POS7)
df <- point7_transform(df, clinstatus_8, POINT7_OS_POS8)
df <- point7_transform(df, clinstatus_9, POINT7_OS_POS9)

# table(df$clinstatus_2, df$RESP_DELIVER4) # the scores corresponds quite well with the resp_deliver variable. Take the ordinal score first and double-check with PI: E.g. those with clinstatus == 4 (NIV) but resp_deliver == 1 (Nasal cannula) or == 2 (mask)
# table(df$clinstatus_5, df$RESP_DELIVER7, useNA = "always") # and resp_deliver unfortunately does not contain more info, i.e., to fill up clinstatus

status_columns <- grep("^PART_STATUS", names(df), value = TRUE)
withdraw_columns <- grep("^CONSENT_CONT", names(df), value = TRUE)
discharge_columns <- grep("^VISIT_DISCHARGE", names(df), value = TRUE)

# df %>% 
#   select(id_pat,
#          contains("RESP_DELIVER"), # RESP_DELIVER3 = D2 = POINT7_OS_POS1 = clinstatus_1 // corresponds well
#          contains("clinstatus_"),
#          contains("PART_STATUS"),
#          # contains("VISIT_DAY"), # does not any info
#          # contains("VISIT_TYPE"), # does not contain more info
#          # contains("CONSENT_CONT"),
#          contains("VISIT_DISCHARGE"),
#          ) %>%
#   # filter(is.na(clinstatus_1)) %>% # RESP_DELIVER3 and onwards unfortunately also missing if clinstatus_1 and onwards missing
#   # PART_STATUS (alive/dead) and onwards unfortunately also missing if clinstatus_1 and onwards missing
#   # filter_at(vars(status_columns), any_vars(. == 0)) %>% # PART_STATUSx (alive=1/dead=0) corresponds well with clinstatus_ (PART_STATUS2 = clinstatus_1) and does not contain more info - except id 590013 -> died at baseline?
#   # filter_at(vars(withdraw_columns), any_vars(. == 1)) %>% # CONSENT_CONTx (withdrew=1) corresponds well with NA in clinstatus_ (CONSENT_CONT2 = clinstatus_1) and does not contain more info
#   filter_at(vars(discharge_columns), any_vars(. == 1)) %>% # VISIT_DISCHARGEx (discharged=1) corresponds well with score in clinstatus_ (VISIT_DISCHARGE2 = clinstatus_1) - except 020040 -> discharged with NIV?
#   View()

# df %>% 
#   filter(id_pat == "590013") %>% 
#   View()

# => 1) find out about 590013, 2) use PART_STATUS == 0 to count deaths and clinstatus_ for time to death, 3) use CONSENT_CONTx to mark withdrawals and time to withdrawal (CONSENT_CONT2 = clinstatus_1 = time to withdrawal = 1 day), 4) use VISIT_DISCHARGEx or VISDATx to mark discharge and time to discharge (VISIT_DISCHARGE2 = clinstatus_1, i.e. time to discharge = 1 day)

df <- df %>%
  rename(discharge_bas = VISIT_DISCHARGE,
         discharge_bas_date = VISDAT1,
         discharge_0 = VISIT_DISCHARGE1,
         discharge_0_date = VISDAT2,
         discharge_1 = VISIT_DISCHARGE2,
         discharge_1_date = VISDAT3,
         discharge_2 = VISIT_DISCHARGE3,
         discharge_2_date = VISDAT4,
         discharge_3 = VISIT_DISCHARGE4,
         discharge_3_date = VISDAT5,
         discharge_4 = VISIT_DISCHARGE5,
         discharge_4_date = VISDAT6,
         discharge_5 = VISIT_DISCHARGE6,
         discharge_5_date = VISDAT7,
         discharge_6 = VISIT_DISCHARGE7,
         discharge_6_date = VISDAT8,
         discharge_7 = VISIT_DISCHARGE8,
         discharge_7_date = VISDAT9,
         discharge_8 = VISIT_DISCHARGE9,
         discharge_8_date = VISDAT10,
         discharge_9 = VISIT_DISCHARGE10,
         discharge_9_date = VISDAT11)


### SECOND, extract all status info from dataframe 2
df2$death_date <- as.Date(df2$DATE_DEATH, format = "%d/%m/%Y") # unique entries (distinct(id_pat))
df2$withdraw_date <- as.Date(df2$CONS_WITHDAT, format = "%d/%m/%Y") # unique entries (distinct(id_pat))
df2 <- df2 %>% 
  mutate(lastvisit = case_when(EOS_LV == "2" ~ "baseline",
                               EOS_LV == "DIS1" ~ "discharge",
                               TRUE ~ EOS_LV)) # unique entries (distinct(id_pat)), missing for 11
df2 <- df2 %>% 
  mutate(lastvisit_status = case_when(EOS_RSN == 1 ~ "completed",
                                      EOS_RSN == 2 ~ "withdrawal",
                                      EOS_RSN == 3 ~ "withdrawal_phys",
                                      EOS_RSN == 4 ~ "withdrawal_ae",
                                      EOS_RSN == 5 ~ "death",
                                      EOS_RSN == 98 ~ "other",
                                      EOS_RSN == 99 ~ "withdrawal_other"
                                      )) # unique entries (distinct(id_pat)), missing for 11
df2 <- df2 %>% mutate(lastvisit_status_other = EOS_RSNOTH)
df2$lastvisit_status_date <- as.Date(df2$ET_EOSDAT, format = "%d/%m/%Y") # unique entries (distinct(id_pat))
df2 <- df2 %>% 
  mutate(treat_stop_status = case_when(TREAT_CESSATION == 1 ~ "noCOVID",
                                      TREAT_CESSATION == 2 ~ "progress_primendpoint",
                                      TREAT_CESSATION == 3 ~ "sar",
                                      TREAT_CESSATION == 4 ~ "dvt",
                                      TREAT_CESSATION == 5 ~ "withdrawal",
                                      ))
df2 <- df2 %>% 
  mutate(sae_outcome = case_when(SAEOUT == 1 ~ "recovered",
                                      SAEOUT == 2 ~ "ongoing",
                                      SAEOUT == 3 ~ "recovered_seq",
                                      SAEOUT == 4 ~ "worsening",
                                      SAEOUT == 5 ~ "death",
                                      SAEOUT == 6 ~ "unknown",
                                      ))
df2$sae_outcome_date <- as.Date(df2$SAERESDAT, format = "%d/%m/%Y") # unique entries (distinct(id_pat))

df2 <- point7_transform(df2, clinstatus_10, POINT7_OS_POS) 
df2 <- point7_transform(df2, clinstatus_11, POINT7_OS_POS1)
df2 <- point7_transform(df2, clinstatus_12, POINT7_OS_POS2)
df2 <- point7_transform(df2, clinstatus_13, POINT7_OS_POS3)
df2 <- point7_transform(df2, clinstatus_dis, POINT7_OS_POS4)
df2 <- point7_transform(df2, clinstatus_28, POINT7_OS_POS5)
df2 <- point7_transform(df2, clinstatus_90, POINT7_OS_POS6)

# table(df2$clinstatus_11, df2$RESP_DELIVER1, useNA = "always") # the scores corresponds quite well with the resp_deliver variable. Take the ordinal score first and double-check with PI: E.g. those with clinstatus == 4 (NIV) but resp_deliver == 1 (Nasal cannula) or == 2 (mask) and resp_deliver unfortunately does not contain more info, i.e., to fill up clinstatus

df_status <- df2 %>% 
  select(id_pat, death_date, withdraw_date, lastvisit, lastvisit_status, lastvisit_status_other, lastvisit_status_date, treat_stop_status, sae_outcome, sae_outcome_date, clinstatus_10, clinstatus_11, clinstatus_12, clinstatus_13, clinstatus_dis, clinstatus_28, clinstatus_90, PART_STATUS, PART_STATUS1, PART_STATUS2, PART_STATUS3, PART_STATUS4, PART_STATUS5, PART_STATUS6, VISIT_DISCHARGE3, VISDAT3, SUBJID_DER36, VISIT_DISCHARGE4, VISDAT4, SUBJID_DER57, PARTIC_ICU, PARTIC_ICU1, PARTIC_ICU2, PARTIC_ICU3, PARTIC_ICU4, PARTUP_READMIT) %>% 
  filter(!(is.na(death_date) & is.na(withdraw_date) & is.na(lastvisit) & is.na(lastvisit_status) & is.na(lastvisit_status_other) & is.na(lastvisit_status_date) & is.na(treat_stop_status) & is.na(sae_outcome) & is.na(sae_outcome_date) & is.na(clinstatus_10) & is.na(clinstatus_11) & is.na(clinstatus_12) & is.na(clinstatus_13) & is.na(clinstatus_dis) & is.na(clinstatus_28) & is.na(clinstatus_90) & is.na(VISIT_DISCHARGE3) & is.na(VISIT_DISCHARGE4) & is.na(PART_STATUS) & is.na(PART_STATUS1) & is.na(PART_STATUS2) & is.na(PART_STATUS3) & is.na(PART_STATUS4) & is.na(PART_STATUS5) & is.na(PART_STATUS6) & is.na(PARTIC_ICU) & is.na(PARTIC_ICU1) & is.na(PARTIC_ICU2) & is.na(PARTIC_ICU3) & is.na(PARTIC_ICU4)))

df_status <- df_status %>% 
  mutate(duplicate_id = duplicated(id_pat) | duplicated(id_pat, fromLast = TRUE))
df_status <- df_status %>% 
  filter(!(sae_outcome == "death" & is.na(lastvisit_status) & is.na(death_date) & duplicate_id == T)) # the sae column does not contain more info than the other death variables, just creates 10 dupl
df_status <- df_status %>% 
  mutate(duplicate_id2 = duplicated(id_pat) | duplicated(id_pat, fromLast = TRUE))
df_status <- df_status %>% 
  filter(!(duplicate_id2 == T & sae_outcome == "recovered_seq")) # the sae column does not contain more info than the other death variables, just creates 2 dupl
df_status <- df_status %>% 
  mutate(duplicate_id3 = duplicated(id_pat) | duplicated(id_pat, fromLast = TRUE))
df_status <- df_status %>% 
  filter(!(duplicate_id3 == T & id_pat == "020035" & sae_outcome_date == "2021-04-29")) # for this pat, the sae_outcome_date does not add more info, just creates 2 dupl
df_status <- df_status %>% 
  mutate(duplicate_id4 = duplicated(id_pat) | duplicated(id_pat, fromLast = TRUE))
df_status <- df_status %>% # clean each duplicate case => we land exactly at n=282
  filter(!(duplicate_id4 == T & id_pat == "020052" & sae_outcome_date == "2021-02-06")) %>% 
  filter(!(duplicate_id4 == T & id_pat == "080002" & is.na(death_date))) %>% 
  filter(!(duplicate_id4 == T & id_pat == "080011" & is.na(death_date))) %>%
  filter(!(duplicate_id4 == T & id_pat == "080026" & is.na(death_date))) %>%
  filter(!(duplicate_id4 == T & id_pat == "090005" & is.na(lastvisit_status))) %>%
  filter(!(duplicate_id4 == T & id_pat == "090006" & sae_outcome_date == "2020-10-10")) %>%
  filter(!(duplicate_id4 == T & id_pat == "090006" & sae_outcome_date == "2020-10-10")) %>%
  filter(!(duplicate_id4 == T & id_pat == "090028" & is.na(death_date))) %>%
  filter(!(duplicate_id4 == T & id_pat == "090047" & is.na(death_date))) %>%
  filter(!(duplicate_id4 == T & id_pat == "240002" & is.na(lastvisit_status)))

# df_status %>%
#   # filter(duplicate_id4 == T) %>%
#   filter(lastvisit_status == "death" | sae_outcome == "death" | !is.na(death_date) | PART_STATUS == 0 | PART_STATUS1 == 0 | PART_STATUS2 == 0 | PART_STATUS3 == 0 | PART_STATUS4 == 0 | PART_STATUS5 == 0 | PART_STATUS6 == 0) %>% # PART_STATUS does not add any additional info
#   View()

df_status <- df_status %>%
  rename(death_date_df2 = death_date,
         withdraw_date_df2 = withdraw_date,
         discharge_11_df2 = VISIT_DISCHARGE3,
         discharge_11_date_df2 = VISDAT3,
         discharge_12_df2 = VISIT_DISCHARGE4,
         discharge_12_date_df2 = VISDAT4,
         icu_10_df2 = PARTIC_ICU,
         icu_11_df2 = PARTIC_ICU1,
         icu_12_df2 = PARTIC_ICU2,
         icu_13_df2 = PARTIC_ICU3,
         icu_14_df2 = PARTIC_ICU4)

df <- left_join(df, df_status[, c("id_pat", "death_date_df2", "withdraw_date_df2", "lastvisit", "lastvisit_status", "lastvisit_status_other", "lastvisit_status_date", "treat_stop_status", "sae_outcome", "sae_outcome_date", "clinstatus_10", "clinstatus_11", "clinstatus_12", "clinstatus_13", "clinstatus_dis", "clinstatus_28", "clinstatus_90", "discharge_11_df2", "discharge_11_date_df2", "discharge_12_df2", "discharge_12_date_df2", "icu_10_df2", "icu_11_df2", "icu_12_df2", "icu_13_df2", "icu_14_df2", "PARTUP_READMIT")], by = join_by(id_pat == id_pat)) ## merge to main df

df_status <- df %>% 
  select(id_pat, randdate, trt, death_date_df2, withdraw_date_df2, lastvisit, lastvisit_status, lastvisit_status_other, lastvisit_status_date, treat_stop_status, sae_outcome, sae_outcome_date, clinstatus_baseline, clinstatus_1, clinstatus_2, clinstatus_3, clinstatus_4, clinstatus_5, clinstatus_6, clinstatus_7, clinstatus_8, clinstatus_9, clinstatus_10, clinstatus_11, clinstatus_12, clinstatus_13, clinstatus_dis, clinstatus_28, clinstatus_90, CONSENT_CONT, CONSENT_CONT1, CONSENT_CONT2, CONSENT_CONT3, CONSENT_CONT4, CONSENT_CONT5, CONSENT_CONT6, CONSENT_CONT7, CONSENT_CONT8, CONSENT_CONT9, CONSENT_CONT10, discharge_bas, discharge_bas_date, discharge_0, discharge_0_date, discharge_1, discharge_1_date, discharge_2, discharge_2_date, discharge_3, discharge_3_date, discharge_4, discharge_4_date, discharge_5, discharge_5_date, discharge_6, discharge_6_date, discharge_7, discharge_7_date, discharge_8, discharge_8_date, discharge_9, discharge_9_date, discharge_11_df2, discharge_11_date_df2, discharge_12_df2, discharge_12_date_df2, PARTIC_ICU, PARTIC_ICU1, PARTIC_ICU2, PARTIC_ICU3, PARTIC_ICU4, PARTIC_ICU5, PARTIC_ICU6, PARTIC_ICU7, PARTIC_ICU8, PARTIC_ICU9, PARTIC_ICU10, icu_10_df2, icu_11_df2, icu_12_df2, icu_13_df2, icu_14_df2, PARTUP_READMIT)

# df_status %>%
#   # filter(!is.na(death_date_df2) | lastvisit_status == "death" | sae_outcome == "death" | clinstatus_baseline == "6" | clinstatus_1 == "6" | clinstatus_2 == "6" | clinstatus_3 == "6" | clinstatus_4 == "6" | clinstatus_5 == "6" | clinstatus_6 == "6" | clinstatus_7 == "6" | clinstatus_8 == "6" | clinstatus_9 == "6" | clinstatus_10 == "6" | clinstatus_11 == "6" | clinstatus_12 == "6" | clinstatus_13 == "6" | clinstatus_dis == "6" | clinstatus_28 == "6" | clinstatus_90 == "6"
#   #        ) %>%
#   filter(!is.na(withdraw_date_df2) | lastvisit_status == "withdrawal" | lastvisit_status == "withdrawal_ae" | lastvisit_status == "withdrawal_phys" | CONSENT_CONT == 1 | CONSENT_CONT1 == 1 | CONSENT_CONT2 == 1 | CONSENT_CONT3 == 1 | CONSENT_CONT4 == 1 | CONSENT_CONT5 == 1 | CONSENT_CONT6 == 1 | CONSENT_CONT7 == 1 | CONSENT_CONT8 == 1 | CONSENT_CONT9 == 1 | CONSENT_CONT10 == 1
#          ) %>%
#   # filter(!is.na(discharge_12_date_df2) | discharge_12_df2 == 1
#   #        ) %>%
#   View()

# 1) use death_date_df2 to indicate deaths and time to death (the one who died after day 90 is NA)
# 2) use withdraw_date_df2 to indicate withdrawals (due to self and investigator) and time to withdrawal
# except for 020095 add 2021-01-27 as withdraw_date_df2 (D1 physician withdrew consent) 
# and for 520021 add 2021-02-07 as withdraw_date_df2 (D11 AE withdrawal, went to ICU) but recovered on 2021-02-12 but probably still at hospital => add clinstatus 2
# some withdrew consent but data was collected afterwards, i.e. include in time to discharge / clinstatus
# 3) use discharge_12_df2 and discharge_12_date_df2 for time to discharge (all other discharge variables do not contain more info!) and then combine with clinstatus

df_status <- df_status %>%
  mutate(withdraw_date = case_when(id_pat == "020095" ~ as.Date("2021-01-27"),
                                   id_pat == "520021" ~ as.Date("2021-02-07"),
                                   TRUE ~ withdraw_date_df2)) %>% 
  mutate(withdraw_d = as.numeric(withdraw_date - randdate)) %>%
  rename(death_date = death_date_df2) %>% 
  mutate(death_d = as.numeric(death_date - randdate)) %>%
  mutate(discharge_date = as.Date(discharge_12_date_df2, format = "%d/%m/%Y")) %>% 
  mutate(discharge_d = as.numeric(discharge_date - randdate))

df_status <- df_status %>% 
  select(id_pat, trt, randdate, death_date, death_d, withdraw_date, withdraw_d, discharge_date, discharge_d, lastvisit, lastvisit_status, lastvisit_status_other, lastvisit_status_date, treat_stop_status, sae_outcome, sae_outcome_date, clinstatus_baseline, clinstatus_1, clinstatus_2, clinstatus_3, clinstatus_4, clinstatus_5, clinstatus_6, clinstatus_7, clinstatus_8, clinstatus_9, clinstatus_10, clinstatus_11, clinstatus_12, clinstatus_13, clinstatus_dis, clinstatus_28, clinstatus_90, PARTIC_ICU, PARTIC_ICU1, PARTIC_ICU2, PARTIC_ICU3, PARTIC_ICU4, PARTIC_ICU5, PARTIC_ICU6, PARTIC_ICU7, PARTIC_ICU8, PARTIC_ICU9, PARTIC_ICU10, icu_10_df2, icu_11_df2, icu_12_df2, icu_13_df2, icu_14_df2, PARTUP_READMIT)

df_status <- df_status %>% # if discharge date = death date => set discharge variables to missing
  mutate(discharge_d = case_when(discharge_date == death_date ~ NA,
                                    TRUE ~ discharge_d)) %>% 
  mutate(discharge_date = case_when(discharge_date == death_date ~ NA,
                                    TRUE ~ discharge_date))
df_status <- df_status %>% 
  mutate(discharge_reached = case_when(!is.na(discharge_d) & discharge_d <= 28 ~ 1,
                                       TRUE ~ 0))
df_status <- df_status %>% 
  mutate(death_reached = case_when(!is.na(death_d) ~ 1,
                                       TRUE ~ 0))

# df_status %>% 
#   filter(is.na(discharge_date) & is.na(death_date) & is.na(withdraw_date)) %>% 
#   View()

# 1) if clinstatus_28 more than 1, then add 28 to time to discharge_d (discharge_reached is correct = 0)
# 2) if clinstatus_28 = 1, but ICU variables = 1, then add 28 to time to discharge_d (but change discharge_reached to 1)

df_status <- df_status %>% 
  mutate(discharge_d = case_when(is.na(discharge_date) & is.na(death_date) & is.na(withdraw_date) & 
                                   clinstatus_28 %in% c("2","3","4","5") ~ 28,
                                       TRUE ~ discharge_d)) %>% 
  mutate(discharge_d = case_when(is.na(discharge_date) & is.na(death_date) & is.na(withdraw_date) & 
                                   clinstatus_28 == "1" & (icu_13_df2 == 1 | icu_14_df2 == 1) ~ 28,
                                       TRUE ~ discharge_d)) %>% 
  mutate(discharge_reached = case_when(is.na(discharge_date) & is.na(death_date) & is.na(withdraw_date) & 
                                   clinstatus_28 == "1" & (icu_13_df2 == 1 | icu_14_df2 == 1) ~ 1,
                                       TRUE ~ discharge_reached))

# df_status %>% 
#   filter(is.na(discharge_date) & is.na(death_date) & is.na(withdraw_date)) %>% 
#   View()

# 1) if clinstatus_28 = 1, no ICU, add latest clinstatus info to discharge_d and change discharge_reached to 1 // always change discharge_reached first, before filling up discharge_d

df_status <- df_status %>% 
  mutate(discharge_reached = case_when(is.na(discharge_date) & is.na(death_date) & is.na(withdraw_date) & is.na(discharge_d) 
                                  & is.na(clinstatus_3) & clinstatus_28 == 1 ~ 1,
                                       TRUE ~ discharge_reached)) %>%
  mutate(discharge_d = case_when(is.na(discharge_date) & is.na(death_date) & is.na(withdraw_date) & is.na(discharge_d) 
                                  & is.na(clinstatus_3) & clinstatus_28 == 1 ~ 3,
                                       TRUE ~ discharge_d)) %>%
  mutate(discharge_reached = case_when(is.na(discharge_date) & is.na(death_date) & is.na(withdraw_date) & is.na(discharge_d) 
                                  & is.na(clinstatus_dis) & clinstatus_28 == 1 ~ 1,
                                       TRUE ~ discharge_reached)) %>% 
  mutate(discharge_d = case_when(is.na(discharge_date) & is.na(death_date) & is.na(withdraw_date) & is.na(discharge_d) 
                                  & is.na(clinstatus_dis) & clinstatus_28 == 1 ~ 14,
                                       TRUE ~ discharge_d))

# df_status %>%
#   filter(is.na(discharge_date) & is.na(death_date) & is.na(withdraw_date)) %>%
#   View()

# 1) if clinstatus_28 is missing, no ICU, they are probably LTFU, i.e. change to discharge_reached = 1, and add latest clinstatus info to discharge_d

df_status <- df_status %>% 
  mutate(discharge_reached = case_when(is.na(discharge_date) & is.na(death_date) & is.na(withdraw_date) & is.na(discharge_d) 
                                  & is.na(clinstatus_28) ~ 1,
                                       TRUE ~ discharge_reached)) %>%
  mutate(ltfu = case_when(is.na(discharge_date) & is.na(death_date) & is.na(withdraw_date) & is.na(discharge_d) 
                                  & is.na(clinstatus_28) ~ 1,
                                       TRUE ~ 0)) %>%
  mutate(discharge_d = case_when(is.na(discharge_date) & is.na(death_date) & is.na(withdraw_date) & is.na(discharge_d) 
                                  & is.na(clinstatus_28) & is.na(clinstatus_5) ~ 5,
                                       TRUE ~ discharge_d)) %>%
  mutate(discharge_d = case_when(is.na(discharge_date) & is.na(death_date) & is.na(withdraw_date) & is.na(discharge_d) 
                                  & is.na(clinstatus_28) & is.na(clinstatus_4) ~ 4,
                                       TRUE ~ discharge_d))

df_status <- df_status %>% 
  select(id_pat, trt, randdate, ltfu, discharge_reached, discharge_date, discharge_d, death_reached, death_date, death_d, withdraw_date, withdraw_d, lastvisit, lastvisit_status, lastvisit_status_other, lastvisit_status_date, treat_stop_status, sae_outcome, sae_outcome_date, clinstatus_baseline, clinstatus_1, clinstatus_2, clinstatus_3, clinstatus_4, clinstatus_5, clinstatus_6, clinstatus_7, clinstatus_8, clinstatus_9, clinstatus_10, clinstatus_11, clinstatus_12, clinstatus_13, clinstatus_dis, clinstatus_28, clinstatus_90, PARTIC_ICU, PARTIC_ICU1, PARTIC_ICU2, PARTIC_ICU3, PARTIC_ICU4, PARTIC_ICU5, PARTIC_ICU6, PARTIC_ICU7, PARTIC_ICU8, PARTIC_ICU9, PARTIC_ICU10, icu_10_df2, icu_11_df2, icu_12_df2, icu_13_df2, icu_14_df2, PARTUP_READMIT)

# all lastvisit_status == other were LTFU after dischargem but some of these do have clinstatus_28 info!
df_status <- df_status %>% 
  mutate(ltfu = case_when(lastvisit_status == "other" ~ 1,
                                       TRUE ~ ltfu))

df <- left_join(df, df_status[, c("id_pat", "ltfu", "discharge_reached", "discharge_date", "discharge_d", "death_reached", "death_date", "death_d", "withdraw_date", "withdraw_d")], by = join_by(id_pat == id_pat)) ## merge to main df


# 590013: died at baseline
# 020040: discharged "with NIV" // all other info overwrites this data entry mistake
# 020104: Died after 90 day follow-up due to a newly diagnosed cancer
# 020035: was the one transferred to different hospital for ECMO and recovered 3 months later => discharge_reached == 0 / discharge_d == 28 / death_reached == 0 / death_d == 90
df <- df %>% 
  mutate(discharge_reached = case_when(id_pat == "020035" ~ 0,
                                       TRUE ~ discharge_reached)) %>% 
  mutate(discharge_d = case_when(id_pat == "020035" ~ 28,
                                       TRUE ~ discharge_d)) %>%
  mutate(death_d = case_when(id_pat == "020035" ~ 90,
                                       TRUE ~ death_d))

# df %>% 
#   select(id_pat, trt, randdate, ltfu, discharge_reached, discharge_date, discharge_d, death_reached, death_date, death_d, withdraw_date, withdraw_d, lastvisit, lastvisit_status, lastvisit_status_other, lastvisit_status_date, treat_stop_status, sae_outcome, sae_outcome_date, clinstatus_baseline, clinstatus_1, clinstatus_2, clinstatus_3, clinstatus_4, clinstatus_5, clinstatus_6, clinstatus_7, clinstatus_8, clinstatus_9, clinstatus_10, clinstatus_11, clinstatus_12, clinstatus_13, clinstatus_dis, clinstatus_28, clinstatus_90, PARTIC_ICU, PARTIC_ICU1, PARTIC_ICU2, PARTIC_ICU3, PARTIC_ICU4, PARTIC_ICU5, PARTIC_ICU6, PARTIC_ICU7, PARTIC_ICU8, PARTIC_ICU9, PARTIC_ICU10, icu_10_df2, icu_11_df2, icu_12_df2, icu_13_df2, icu_14_df2) %>% 
#   View()


# (i) Primary outcome: Mortality at day 28
df <- df %>% # 9 early withdrawals / 5 early discharged without any follow-up info (LTFU)
  mutate(mort_28 = case_when(death_reached == 1 & death_d <29 ~ 1, # this includes those discharged and then died (as long as within 28 days)
                             death_reached == 1 & death_d >28 ~ 0, # alive at day 28 but died later, i.e. info available
                             discharge_d >= 28 ~ 0, # discharged at day 28 or later, proof of still alive at day 28
                             discharge_d < 28 & death_reached == 0 & ltfu == 0 & is.na(withdraw_date) ~ 0, # all discharged were discharged alive and not to hospice
                             clinstatus_28 == 1 ~ 0, # info available at 28d fup
                             clinstatus_90 == 1 ~ 0, # info available at 90d fup
                             sae_outcome == "recovered" & sae_outcome_date > randdate + 28 ~ 0, # SAE info available after 28d -> proof still alive
                             )) 
# df %>%
#   select(id_pat, mort_28, trt, randdate, ltfu, discharge_reached, discharge_date, discharge_d, death_reached, death_date, death_d, withdraw_date, withdraw_d, lastvisit, lastvisit_status, lastvisit_status_other, lastvisit_status_date, treat_stop_status, sae_outcome, sae_outcome_date, clinstatus_baseline, clinstatus_1, clinstatus_2, clinstatus_3, clinstatus_4, clinstatus_5, clinstatus_6, clinstatus_7, clinstatus_8, clinstatus_9, clinstatus_10, clinstatus_11, clinstatus_12, clinstatus_13, clinstatus_dis, clinstatus_28, clinstatus_90, PARTIC_ICU, PARTIC_ICU1, PARTIC_ICU2, PARTIC_ICU3, PARTIC_ICU4, PARTIC_ICU5, PARTIC_ICU6, PARTIC_ICU7, PARTIC_ICU8, PARTIC_ICU9, PARTIC_ICU10, icu_10_df2, icu_11_df2, icu_12_df2, icu_13_df2, icu_14_df2) %>%
#   filter(is.na(mort_28)) %>%
#   View()

# First, keep mort_28 as complete case

# Second, use multiple imputation (see below)

# Third, apply a deterministic imputation (see notes): we use the same rules as ACTT2 => No transfer to hospice happened -> assign "alive"
df <- df %>%
  mutate(mort_28_dimp = case_when(is.na(mort_28) ~ 0,
                             TRUE ~ c(mort_28)))
# table(df$mort_28, useNA = "always")

# (ii) Mortality at day 60
df <- df %>% # 10 early withdrawals (before day 60) and no info later / 8 early discharge (before day 60) and no info later
  mutate(mort_60 = case_when(death_reached == 1 & death_d <61 ~ 1, # this includes those discharged and then died (as long as within 60 days)
                             death_reached == 1 & death_d >60 ~ 0, # alive at day 60 but died later, i.e. info available
                             discharge_d >= 60 ~ 0, # discharged at day 60 or later, proof of still alive at day 60
                             discharge_d < 60 & death_reached == 0 & ltfu == 0 & is.na(withdraw_date) ~ 0, # all discharged were discharged alive and not to hospice
                             clinstatus_28 == 1 ~ 0, # info available at 28d fup
                             clinstatus_90 == 1 ~ 0, # info available at 90d fup
                             sae_outcome == "recovered" & sae_outcome_date > randdate + 60 ~ 0, # SAE info available after 60d -> proof still alive
                             )) 
# df %>%
#   select(id_pat, mort_28, mort_60, trt, randdate, ltfu, discharge_reached, discharge_date, discharge_d, death_reached, death_date, death_d, withdraw_date, withdraw_d, lastvisit, lastvisit_status, lastvisit_status_other, lastvisit_status_date, treat_stop_status, sae_outcome, sae_outcome_date, clinstatus_baseline, clinstatus_1, clinstatus_2, clinstatus_3, clinstatus_4, clinstatus_5, clinstatus_6, clinstatus_7, clinstatus_8, clinstatus_9, clinstatus_10, clinstatus_11, clinstatus_12, clinstatus_13, clinstatus_dis, clinstatus_28, clinstatus_90, PARTIC_ICU, PARTIC_ICU1, PARTIC_ICU2, PARTIC_ICU3, PARTIC_ICU4, PARTIC_ICU5, PARTIC_ICU6, PARTIC_ICU7, PARTIC_ICU8, PARTIC_ICU9, PARTIC_ICU10, icu_10_df2, icu_11_df2, icu_12_df2, icu_13_df2, icu_14_df2, PARTUP_READMIT) %>%
#   filter(is.na(mort_60)) %>%
#   # filter(id_pat == "020035") %>% 
#   View()


# (iii) Time to death within max. follow-up time (90 days)
df <- df %>% # first, death_d, then withdraw_d, then discharge_d, then max follow-up
  mutate(death_time = case_when(!is.na(death_d) ~ death_d, 
                                !is.na(withdraw_d) ~ withdraw_d, 
                                !is.na(discharge_d) ~ discharge_d))


# (iv) New mechanical ventilation among survivors within 28 days. TACTIC included across all clinstatus.
df <- df %>% # The NAs are not eligible (died -> 35 or clinstatus_baseline == 5 -> 2) and thus are excluded from denominator. The missing mort_28 were not ventilated (LTFU after discharge)
  mutate(new_mv_28 = case_when((clinstatus_baseline %in% c("2","3","4")) & (mort_28 == 0 | is.na(mort_28)) 
                               & (clinstatus_1 == 5 | clinstatus_2 == 5 | clinstatus_3 == 5 | clinstatus_4 == 5 
                                  | clinstatus_5 == 5 | clinstatus_6 == 5 | clinstatus_7 == 5 | clinstatus_8 == 5
                                  | clinstatus_9 == 5 | clinstatus_10 == 5 | clinstatus_11 == 5 | clinstatus_12 == 5
                                  | clinstatus_13 == 5 | clinstatus_dis == 5 | clinstatus_28 == 5) ~ 1,
                               (clinstatus_baseline %in% c("2","3","4")) & (mort_28 == 0 | is.na(mort_28)) ~ 0))

# (iv) Alternative definition/analysis: New mechanical ventilation OR death within 28 days => include all in denominator.
df <- df %>% 
  mutate(new_mvd_28 = case_when(new_mv_28 == 1 | mort_28 == 1 ~ 1,
                                new_mv_28 == 0 | mort_28 == 0 ~ 0))
# table(df$new_mvd_28, useNA = "always") # no missing


# (v) Clinical status at day 28
df <- df %>% # same 14 missing as in mort_28
  mutate(clinstatus_28 = case_when(mort_28 == 1 ~ 6, # died within 28d
                                   clinstatus_28 == 6 ~ 6,
                                   clinstatus_28 == 5 ~ 5,
                                   clinstatus_28 == 4 ~ 4,
                                   clinstatus_28 == 3 ~ 3,
                                   clinstatus_28 == 2 ~ 2,
                                   clinstatus_28 == 1 ~ 1,
                                   discharge_d <29 & !is.na(mort_28) ~ 1 # discharged alive / discharge criteria within 28d
  ))
df$clinstatus_28 <- factor(df$clinstatus_28, levels = 1:6)

## Imputation according to protocol: If there was daily data for the ordinal score available but with missing data for single days, then we carried last observed value forward unless for day 28, whereby we first considered data from the window (+/-3 days) -> no window data => LVCF
df$clinstatus_baseline <- factor(df$clinstatus_baseline, levels = 1:6) # function needs factor
dfcs <- df %>% 
    select(id_pat, clinstatus_baseline, clinstatus_1, clinstatus_2, clinstatus_3, clinstatus_4, clinstatus_5, clinstatus_6, clinstatus_7, clinstatus_8, clinstatus_9, clinstatus_10, clinstatus_11, clinstatus_12, clinstatus_13, clinstatus_dis, clinstatus_28)
impute_last_forw = function(df){
  first = which(names(df)%in%c("clinstatus_baseline"))
  last = which(names(df)%in%c("clinstatus_28"))
  for (i in 1:dim(df)[[1]]){
    for (j in first[1]:last[1]){
      p = df[i, j]
      df[i,j] <- 
        ifelse(!is.na(df[i, j]), p, df[i, j-1])
    }
  }
  df
}
dfcs <- impute_last_forw(dfcs)
dfcs <- dfcs %>% # To control, don't overwrite
  rename(clinstatus_28_imp = clinstatus_28)
df <- left_join(df, dfcs[, c("clinstatus_28_imp", "id_pat")], by = join_by(id_pat == id_pat))
# table(df$clinstatus_28_imp, useNA = "always")


# (vi) Time to discharge or reaching discharge criteria up to day 28
# table(df$discharge_reached, useNA = "always")
df <- df %>% # first, discharge_d, then withdraw_d, then death_d, then max follow-up
  mutate(discharge_time = case_when(!is.na(discharge_d) ~ discharge_d, 
                                !is.na(withdraw_d) ~ withdraw_d, 
                                !is.na(death_d) ~ death_d))
# table(df$discharge_time, useNA = "always")
df <- df %>% # restrict to max fup time 28d
  mutate(discharge_time = case_when(discharge_time >28 ~ 28,
                                    TRUE ~ discharge_time))
df <- df %>% # add 28d for those that died - as a sens-variable
  mutate(discharge_time_sens = case_when(mort_28 == 1 ~ 28,
                                    TRUE ~ discharge_time))

# (vi) Sens-analysis: Alternative definition/analysis of outcome: time to sustained discharge within 28 days -> cannot be differentiated in dataset
df$discharge_reached_sus <- df$discharge_reached
df$discharge_time_sus <- df$discharge_time


# (vii) Viral clearance up to day 5, day 10, and day 15 (Viral load value <LOQ and/or undectectable)
df_vl <- df2 %>% 
  select(id_pat, COVID19_RECDAT, COVID19_RECDAT1, COVID19_RECDAT2, COVID19_RECDAT3, COVID19_RECDAT4, 
         COVID19_RESULT, COVID19_RESULT1, COVID19_RESULT2, COVID19_RESULT3, COVID19_RESULT4) %>% 
  filter(!(is.na(COVID19_RECDAT) & is.na(COVID19_RECDAT1) & is.na(COVID19_RECDAT2) & is.na(COVID19_RECDAT3) & is.na(COVID19_RECDAT4) & is.na(COVID19_RESULT) & is.na(COVID19_RESULT1) & is.na(COVID19_RESULT2) & is.na(COVID19_RESULT3) & is.na(COVID19_RESULT4))) %>% 
  rename(result_date_12 = COVID19_RECDAT,
         result_date_13 = COVID19_RECDAT1,
         result_date_14 = COVID19_RECDAT2,
         result_date_15 = COVID19_RECDAT3,
         result_date_16 = COVID19_RECDAT4,
         COVID19_RESULT12 = COVID19_RESULT,
         COVID19_RESULT13 = COVID19_RESULT1,
         COVID19_RESULT14 = COVID19_RESULT2,
         COVID19_RESULT15 = COVID19_RESULT3,
         COVID19_RESULT16 = COVID19_RESULT4)

df <- left_join(df, df_vl[, c("id_pat", "result_date_12", "result_date_13", "result_date_14", "result_date_15", "result_date_16", "COVID19_RESULT12", "COVID19_RESULT13", "COVID19_RESULT14", "COVID19_RESULT15", "COVID19_RESULT16")], by = join_by(id_pat == id_pat))
df <- df %>% 
  rename(result_date_1 = COVID19_RECDAT,
         result_date_2 = COVID19_RECDAT1,
         result_date_3 = COVID19_RECDAT2,
         result_date_4 = COVID19_RECDAT3,
         result_date_5 = COVID19_RECDAT4,
         result_date_6 = COVID19_RECDAT5,
         result_date_7 = COVID19_RECDAT6,
         result_date_8 = COVID19_RECDAT7,
         result_date_9 = COVID19_RECDAT8,
         )

df_vl <- df %>% 
    select(id_pat, randdate, vl_baseline, result_date_1, COVID19_RESULT1, result_date_2, COVID19_RESULT2, result_date_3, COVID19_RESULT3, result_date_4, COVID19_RESULT4,  result_date_5, COVID19_RESULT5, result_date_6, COVID19_RESULT6, result_date_7, COVID19_RESULT7, result_date_8, COVID19_RESULT8, result_date_9, COVID19_RESULT9,result_date_12, COVID19_RESULT12, result_date_13, COVID19_RESULT13, result_date_14, COVID19_RESULT14, result_date_15, COVID19_RESULT15, result_date_16, COVID19_RESULT16)

df_vl <- df_vl %>%
  mutate_at(vars(starts_with("result_date_")), as.Date, format = "%d/%m/%Y")

# Calculate time difference for each result_date variable relative to 5 days after randdate
date_cols <- grep("^result_date_", colnames(df_vl), value = TRUE)
for (col in date_cols) {
  df_vl <- df_vl %>%
    mutate(!!paste0("time_diff_", col) := abs(.data[[col]] - (randdate + days(5))))
}

# Find the result_date with the minimum time difference for each row
```
Discussion points:


