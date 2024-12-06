##### 
# load packages
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

#####
# Load data / AD sets
df <- read_sas("E:/NVT-RA-12435/files_NVT_SA_CINC424J12301/Files/Analysis Ready Datasets/adsl_mse.sas7bdat")
df_bs <- read_sas("E:/NVT-RA-12435/files_NVT_SA_CINC424J12301/Files/Analysis Ready Datasets/adbs_mse.sas7bdat")
#df_bs2 <- read_sas("E:/NVT-RA-12435/files_NVT_SA_CINC424J12301/Files/Analysis Ready Datasets/adb1_mse.sas7bdat")
df_ae <- read_sas("E:/NVT-RA-12435/files_NVT_SA_CINC424J12301/Files/Analysis Ready Datasets/adae_mse.sas7bdat")
df_comed <- read_sas("E:/NVT-RA-12435/files_NVT_SA_CINC424J12301/Files/Analysis Ready Datasets/adcm_mse.sas7bdat")
df_outcome <- read_sas("E:/NVT-RA-12435/files_NVT_SA_CINC424J12301/Files/Analysis Ready Datasets/adeff_mse.sas7bdat")
df_lab <- read_sas("E:/NVT-RA-12435/files_NVT_SA_CINC424J12301/Files/Analysis Ready Datasets/adlb_mse.sas7bdat")
#df_vs <- read_sas("E:/NVT-RA-12435/files_NVT_SA_CINC424J12301/Files/Analysis Ready Datasets/advs_mse.sas7bdat")
df_tte <- read_sas("E:/NVT-RA-12435/files_NVT_SA_CINC424J12301/Files/Analysis Ready Datasets/adtte_mse.sas7bdat")
df_score <- read_sas("E:/NVT-RA-12435/files_NVT_SA_CINC424J12301/Files/Analysis Ready Datasets/adqs_mse.sas7bdat")
df_risk <- read_sas("E:/NVT-RA-12435/files_NVT_SA_CINC424J12301/Files/Analysis Ready Datasets/adrisk_mse.sas7bdat")
df_dispo <- read_sas("E:/NVT-RA-12435/files_NVT_SA_CINC424J12301/Files/Raw Datasets/ds_mse.sas7bdat")
df_medicalhist <- read_sas("E:/NVT-RA-12435/files_NVT_SA_CINC424J12301/Files/Raw Datasets/mh_mse.sas7bdat")

#####
addmargins(table(df$clinstatus_28_imp, df$trt, useNA = "always"))

# Baseline data
df <- df %>% # keep only those randomized. Corresponds to publication
  filter(RANDFL == "Y")
df$country <- NA # not provided due to anonymization process
df <- df %>% rename(id_pat = SUBJID)
df <- df %>% mutate(randdate = as.Date(RANDDTM)) # has been adapted due to anonymization process?
df <- df %>% # ITT population
  mutate(trt = case_when(TRT01PL == "Ruxolitinib 5 mg bid" ~ 1,
                         TRUE ~ 0))
df$trial <- c("RUXCOVID")
df$JAKi <- c("Ruxolitinib")
df <- df %>% # no missing in sex
  mutate(sex = case_when(SEX == "F" ~ "female",
                         SEX == "M" ~ "male"))
df <- df %>% # no missing in ethnicity
  mutate(ethn = case_when(RACE == "UNKNOWN" & ETHNIC == "HISPANIC OR LATINO" ~ "HISPANIC OR LATINO",
                          TRUE ~ c(RACE)))
df$age <- as.numeric(df$AGE)

# ICU care: See publication, no recruitment in ICU
df$icu <- 0

# Days with symptoms prior to randomization
df_symp <- df_bs %>% 
  rename(id_pat = SUBJID) %>% 
  filter(PARAM == "Number of days between onset of symptoms and randomization") %>% 
  select(id_pat, PARAM, AVAL) %>% 
  rename(sympdur = AVAL)
df <- left_join(df, df_symp[, c("id_pat", "sympdur")], by = join_by(id_pat == id_pat))
#df %>%
#  drop_na(sympdur) %>%
#  ggplot(aes(x = sympdur)) +
#  geom_density(fill = "blue", color = "black") +
#  labs(title = "Density Plot of Symptom Duration",
#       x = "Symptom Duration",
#       y = "Density")

# table(df_vs$PARAM, useNA = "always") # check National Early Warning Score 2 -> not needed! => df_vs not needed

# Severity of COVID-19 with respect to respiratory support at randomisation
df_clinstatus_baseline <- df_bs %>% 
  rename(id_pat = SUBJID) %>% 
  filter(PARAM == "Baseline clinical status based on 9-point scale (3,4,5)") %>% 
  select(id_pat, PARAM, AVAL) %>% 
  rename(clinstatus_bs = AVAL)
df <- left_join(df, df_clinstatus_baseline[, c("id_pat", "clinstatus_bs")], by = join_by(id_pat == id_pat))
# transform the WHO score used in RUXCOVID into our score
score_transform <- function(df, clinstatus_var, score_var) {
  df <- df %>%
    mutate({{ clinstatus_var }} :=
             case_when({{ score_var }} %in% c(0, 1, 2) ~ 1,
                       {{ score_var }} == 3 ~ 2,
                       {{ score_var }} == 4 ~ 3,
                       {{ score_var }} == 5 ~ 4,
                       {{ score_var }} %in% c(6, 7) ~ 5,
                       {{ score_var }} == 8 ~ 6)) %>%
    mutate({{ clinstatus_var }} := factor({{ clinstatus_var }}, levels = 1:6))
}
df <- score_transform(df, clinstatus_baseline, clinstatus_bs)
# addmargins(table(df$clinstatus_baseline, df$trt, useNA = "always")) # 1 missing, corresponds to publication
df <- df %>%
  mutate(vbaseline = case_when(clinstatus_baseline == "2" | clinstatus_baseline == "3" ~ 0,
                               clinstatus_baseline == "4" | clinstatus_baseline == "5" ~ 1))

### Co-medication at baseline
# DEXA
df_dexa <- df_bs %>% 
  rename(id_pat = SUBJID) %>% 
  filter(PARAM == "Systemic Corticosteroids Use at Baseline") %>% 
  select(id_pat, PARAM, AVALC) %>%
  mutate(comed_dexa = case_when(AVALC == "Y" ~ 1,
                                AVALC == "N" ~ 0))
df <- left_join(df, df_dexa[, c("id_pat", "comed_dexa")], by = join_by(id_pat == id_pat))
# table(df$comed_dexa, df$trt, useNA = "always") # corresponds to publications
# RDV
df_rdv <- df_bs %>% 
  rename(id_pat = SUBJID) %>% 
  filter(PARAM == "REMDESIVIR Use at Baseline") %>% 
  select(id_pat, PARAM, AVALC) %>%
  mutate(comed_rdv = case_when(AVALC == "Y" ~ 1,
                                AVALC == "N" ~ 0))
df <- left_join(df, df_rdv[, c("id_pat", "comed_rdv")], by = join_by(id_pat == id_pat))
# table(df$comed_rdv, df$trt, useNA = "always") # corresponds to publications
# TOCI // missing by design -> see eligibilty criteria
df$comed_toci <- 0
# ANTIBIOTICS
df_ab <- df_bs %>% 
  rename(id_pat = SUBJID) %>% 
  filter(PARAM == "Antibiotics Use at Baseline") %>% 
  select(id_pat, PARAM, AVALC) %>%
  mutate(comed_ab = case_when(AVALC == "Y" ~ 1,
                               AVALC == "N" ~ 0))
df <- left_join(df, df_ab[, c("id_pat", "comed_ab")], by = join_by(id_pat == id_pat))
# ANTICOA
df_acoa <- df_bs %>% 
  rename(id_pat = SUBJID) %>% 
  filter(PARAM == "Antithrombotics Use at Baseline") %>% 
  select(id_pat, PARAM, AVALC) %>%
  mutate(comed_acoa = case_when(AVALC == "Y" ~ 1,
                              AVALC == "N" ~ 0))
df <- left_join(df, df_acoa[, c("id_pat", "comed_acoa")], by = join_by(id_pat == id_pat))
# IFN
df$comed_interferon <- 0 # excluded according to protocol
# other
df_comed_other <- df_comed %>% 
  rename(id_pat = SUBJID) %>%
  filter(EPOCH == "SCREENING") %>% 
  select(id_pat, CMDECOD) %>%
  mutate(comed_other = case_when(!is.na(CMDECOD) ~ 1)) %>% 
  distinct(id_pat) %>% 
  mutate(comed_other = 1)
df <- left_join(df, df_comed_other[, c("id_pat", "comed_other")], by = join_by(id_pat == id_pat))
df <- df %>% 
  mutate(comed_other = case_when(comed_dexa == 1 | comed_rdv == 1 | comed_ab == 1 | comed_acoa == 1 ~ 0,
                                 comed_other == 1 ~ 1
                                 ))
# group them for the subgroup analysis, according to protocol
df <- df %>%
  mutate(comed_cat = case_when(comed_dexa == 0 & (comed_toci == 0 | is.na(comed_toci)) ~ 1, # patients without Dexamethasone nor Tocilizumab
                               comed_dexa == 1 & (comed_toci == 0 | is.na(comed_toci)) ~ 2, # patients with Dexamethasone but no Tocilizumab
                               comed_dexa == 1 & comed_toci == 1 ~ 3, # patients with Dexamethasone and Tocilizumab
                               comed_dexa == 0 & comed_toci == 1 ~ 4)) # patients with Tocilizumab but no Dexamethasone (if exist)

### Comorbidity at baseline, including immunocompromised // double-check the raw datasets!
# aHT
df_aht <- df_bs %>% 
  rename(id_pat = SUBJID) %>% 
  filter(PARAM == "HYPERTENSION at Baseline") %>% 
  select(id_pat, PARAM, AVALC) %>%
  mutate(comorb_aht = case_when(AVALC == "Y" ~ 1,
                                AVALC == "N" ~ 0))
df <- left_join(df, df_aht[, c("id_pat", "comorb_aht")], by = join_by(id_pat == id_pat))
# Lung
df_ast <- df_bs %>% 
  rename(id_pat = SUBJID) %>% 
  filter(PARAM == "ASTHMA at Baseline") %>% 
  select(id_pat, PARAM, AVALC) %>%
  mutate(comorb_ast = case_when(AVALC == "Y" ~ 1,
                                AVALC == "N" ~ 0))
df <- left_join(df, df_ast[, c("id_pat", "comorb_ast")], by = join_by(id_pat == id_pat))
df_copd <- df_bs %>% 
  rename(id_pat = SUBJID) %>% 
  filter(PARAM == "COPD at Baseline") %>% 
  select(id_pat, PARAM, AVALC) %>%
  mutate(comorb_copd = case_when(AVALC == "Y" ~ 1,
                                AVALC == "N" ~ 0))
df <- left_join(df, df_copd[, c("id_pat", "comorb_copd")], by = join_by(id_pat == id_pat))
df <- df %>% 
  mutate(comorb_lung = case_when(comorb_ast == 1 | comorb_copd == 1 ~ 1,
                                 comorb_ast == 0 & comorb_copd == 0 ~ 0))
# Cancer
df_cancer <- df_bs %>% 
  rename(id_pat = SUBJID) %>% 
  filter(PARAM == "MALIGNANT NEOPLASM at Baseline") %>% 
  select(id_pat, PARAM, AVALC) %>%
  mutate(comorb_cancer = case_when(AVALC == "Y" ~ 1,
                                 AVALC == "N" ~ 0))
df <- left_join(df, df_cancer[, c("id_pat", "comorb_cancer")], by = join_by(id_pat == id_pat))
# CKD
df_ckd <- df_bs %>% 
  rename(id_pat = SUBJID) %>% 
  filter(PARAM == "CHRONIC KIDNEY DISEASE at Baseline") %>% 
  select(id_pat, PARAM, AVALC) %>%
  mutate(comorb_kidney = case_when(AVALC == "Y" ~ 1,
                                   AVALC == "N" ~ 0))
df <- left_join(df, df_ckd[, c("id_pat", "comorb_kidney")], by = join_by(id_pat == id_pat))
# DM
df_dm <- df_bs %>% 
  rename(id_pat = SUBJID) %>% 
  filter(PARAM == "DIABETES at Baseline") %>% 
  select(id_pat, PARAM, AVALC) %>%
  mutate(comorb_dm = case_when(AVALC == "Y" ~ 1,
                                   AVALC == "N" ~ 0))
df <- left_join(df, df_dm[, c("id_pat", "comorb_dm")], by = join_by(id_pat == id_pat))
# heart: CVD and CHD
df_stroke <- df_bs %>% 
  rename(id_pat = SUBJID) %>% 
  filter(PARAM == "CEREBROVASCULAR DISEASE at Baseline") %>% 
  select(id_pat, PARAM, AVALC) %>%
  mutate(comorb_stroke = case_when(AVALC == "Y" ~ 1,
                                AVALC == "N" ~ 0))
df <- left_join(df, df_stroke[, c("id_pat", "comorb_stroke")], by = join_by(id_pat == id_pat))
df_chd <- df_bs %>% 
  rename(id_pat = SUBJID) %>% 
  filter(PARAM == "CHRONIC HEART DISEASE at Baseline") %>% 
  select(id_pat, PARAM, AVALC) %>%
  mutate(comorb_chd = case_when(AVALC == "Y" ~ 1,
                                 AVALC == "N" ~ 0))
df <- left_join(df, df_chd[, c("id_pat", "comorb_chd")], by = join_by(id_pat == id_pat))
df <- df %>% 
  mutate(comorb_cvd = case_when(comorb_stroke == 1 | comorb_chd == 1 ~ 1,
                                 comorb_stroke == 0 & comorb_chd == 0 ~ 0))
# obesity
df <- df %>% 
  mutate(comorb_obese = case_when(BMIGR1 == "> 30 kg/m2" ~ 1,
                                  BMIGR1 == "<= 30 kg/m2" ~ 0))
df$comorb_liver <- NA
df$immunosupp <- NA
df$comorb_autoimm <- NA
df$comorb_smoker <- NA

df <- df %>%
  mutate(any_comorb = case_when(comorb_lung == 1 | comorb_cvd == 1 |
                                  comorb_aht == 1 | comorb_dm == 1 | comorb_obese == 1
                                | comorb_cancer == 1 | comorb_kidney == 1
                                ~ 1,
                                comorb_lung == 0 & comorb_cvd == 0 &
                                  comorb_aht == 0 & comorb_dm == 0 & comorb_obese == 0 
                                & comorb_cancer == 0 & comorb_kidney == 0
                                ~ 0))

# the remaining missing have only NA in 1 comorb category => no evidence for comorbidity -> recode as 0
df <- df %>%
  mutate(any_comorb = case_when(is.na(any_comorb) ~ 0,
                                   TRUE ~ any_comorb))

## group them for the subgroup analysis, according to protocol // count all pre-defined comorbidities per patient first
comorb <- df %>%
  select(id_pat, comorb_lung, comorb_cvd, comorb_aht, comorb_dm, comorb_obese, comorb_kidney, comorb_cancer)
comorb$comorb_count <- NA
for (i in 1:dim(comorb)[[1]]) {
  comorb$comorb_count[i] <- ifelse(
    sum(comorb[i, ] %in% c(1)) > 0,
    sum(comorb[i, ] %in% c(1)),
    NA
  )
}
comorb <- comorb %>%
  mutate(comorb_count = case_when(comorb_lung == 0 & comorb_cvd == 0 &
                                    comorb_aht == 0 & comorb_dm == 0 & comorb_obese == 0
                                  & comorb_cancer == 0 & comorb_kidney == 0 ~ 0,
                                  TRUE ~ comorb_count))
df <- left_join(df, comorb[, c("comorb_count", "id_pat")], by = join_by(id_pat == id_pat)) ## merge imputed variable back
df <- df %>%
  mutate(comorb_cat = case_when(# immunosupp == 1 ~ 4, # immunocompromised
                                comorb_count == 0 ~ 1, # no comorbidity
                                comorb_count == 1 ~ 2, # one comorbidity
                                comorb_count >1 ~ 3)) # multiple comorbidities
df <- df %>%
  mutate(comorb_cat = case_when(is.na(comorb_cat) ~ 1,
                                TRUE ~ comorb_cat))
# addmargins(table(df$comorb_cat, df$trt, useNA = "always"))
df <- df %>%
  mutate(comorb_any = case_when(comorb_count == 0 ~ 0, # no comorbidity
                                comorb_count >0 ~ 1)) # any comorbidity

# CRP
df_crp <- df_bs %>% 
  rename(id_pat = SUBJID) %>% 
  filter(PARAM == "Baseline C Reactive Protein (mg/L)") %>% 
  select(id_pat, PARAM, AVAL) %>% 
  rename(crp = AVAL)
df <- left_join(df, df_crp[, c("id_pat", "crp")], by = join_by(id_pat == id_pat))
#df %>%
#  drop_na(crp) %>%
#  ggplot(aes(x = crp)) +
#  geom_density(fill = "blue", color = "black") +
#  labs(title = "Density Plot of Symptom Duration",
#       x = "CRP",
#       y = "Density")

# Viremia // not available
# Vaccination // not available, but noone could be vaccinated (recruitment up to Sept 19, 2020)
df$vacc <- 0

# Variant // not available
# Serology // not available

df <- df %>% 
  mutate(at_risk = case_when(age>=65 | comorb_cvd==1 | comorb_smoker==1 ~ 1,
                             TRUE ~ 0))


###############
## OUTCOMES
# time to event set
df_tte <- df_tte %>%
  rename(id_pat = SUBJID)

# discontinuation set, get all important info from that set and add it to the TTE set
df_dispo <- df_dispo %>% 
  mutate(id_pat = sapply(str_split(USUBJID, "_"), function(x) x[2]))
df_withdr <- df_dispo %>% 
  filter(DSDECOD == "WITHDRAWAL OF INFORMED CONSENT") %>%
  filter(DSSCAT == "STUDY INFORMED CONSENT") %>%
  filter(!(EPOCH == "SCREENING")) %>%
  select(id_pat, DSDECOD, DSSTDY, DSSCAT, EPOCH) %>% 
  rename(withdraw = DSDECOD,
         withdraw_time = DSSTDY)
df_tte <- left_join(df_tte, df_withdr[, c("id_pat", "withdraw", "withdraw_time")], by = join_by(id_pat == id_pat))
df_ltfu <- df_dispo %>% 
  filter(DSDECOD == "LOST TO FOLLOW-UP") %>%
  select(id_pat, DSDECOD, DSSTDY, DSSCAT, EPOCH) %>% 
  rename(ltfu = DSDECOD,
         ltfu_time = DSSTDY)
df_tte <- left_join(df_tte, df_ltfu[, c("id_pat", "ltfu", "ltfu_time")], by = join_by(id_pat == id_pat))
df_ae_dispo <- df_dispo %>% 
  filter(DSDECOD == "ADVERSE EVENT") %>%
  filter(DSSCAT == "TREATMENT DISPOSITION") %>%
  select(id_pat, DSDECOD, DSSTDY, DSSCAT, EPOCH) %>% 
  rename(ae = DSDECOD,
         ae_time = DSSTDY)
df_tte <- left_join(df_tte, df_ae_dispo[, c("id_pat", "ae", "ae_time")], by = join_by(id_pat == id_pat))
# df_withdr_subj <- df_dispo %>% 
#   filter(DSDECOD == "SUBJECT DECISION") %>%
#   filter(!(EPOCH == "SCREENING")) %>%
#   select(id_pat, DSDECOD, DSSTDY, DSSCAT, EPOCH) %>% 
#   rename(withdraw2 = DSDECOD,
#          withdraw_time2 = DSSTDY)
# df_tte <- left_join(df_tte, df_withdr_subj[, c("id_pat", "withdraw2", "withdraw_time2")], by = join_by(id_pat == id_pat))
# df_withdr_phys <- df_dispo %>% 
#   filter(DSDECOD == "PHYSICIAN DECISION") %>%
#   filter(!(EPOCH == "SCREENING")) %>%
#   select(id_pat, DSDECOD, DSSTDY, DSSCAT, EPOCH) %>% 
#   rename(withdraw3 = DSDECOD,
#          withdraw_time3 = DSSTDY)
# df_tte <- left_join(df_tte, df_withdr_phys[, c("id_pat", "withdraw3", "withdraw_time3")], by = join_by(id_pat == id_pat))


# (i) Primary outcome: Mortality at day 28
df_ttd <- df_tte %>% # death set
  filter(PARAM == "Time to death (Days)") %>%
  select(id_pat, PARAM, AVAL, CNSR, withdraw, withdraw_time, ae, ae_time, ltfu, ltfu_time)
df_ttd <- df_ttd %>% # identify deaths
  mutate(mort_28 = case_when(CNSR == 0 ~ 1,
                             CNSR == 1 & is.na(withdraw) ~ 0 # all withdrawals were before day 28; the LTFU was after
                             ))
df <- left_join(df, df_ttd[, c("mort_28", "id_pat")], by = join_by(id_pat == id_pat)) ## merge to main df
addmargins(table(df$mort_28, df$trt, useNA = "always")) # corresponds to publication, incl. the 8 withdrawals

# First, keep mort_28 as complete case

# Second, use multiple imputation (see below)

# Third, apply a deterministic imputation (see notes): we use the same rules as ACTT2 => transfer that died are already accounted for, for the remaining -> assign "alive"
df <- df %>%
  mutate(mort_28_dimp = case_when(is.na(mort_28) ~ 0,
                                  TRUE ~ c(mort_28)))

# (ii) Mortality at day 60 // not available -> use mort_28
df <- df %>%
  mutate(mort_60 = mort_28)

# (iii) Time to death within max. follow-up time (systematically until 28 days!)
df_ttd <- df_ttd %>% # use censoring variable to identify deaths within 28 days, don't bother about the others
  mutate(death_reached = case_when(CNSR == 0 ~ 1,
                                   CNSR == 1 ~ 0))
df_ttd$death_time <- df_ttd$AVAL
df <- left_join(df, df_ttd[, c("death_reached", "death_time", "id_pat")], by = join_by(id_pat == id_pat)) ## merge to main df
table(df$death_time, df$death_reached, useNA = "always")
# Cap at 28 days, since this was max. systematic follow-up, no deaths recorded afterwards anymore
df <- df %>%
  mutate(death_time = case_when(death_time >28 ~ 28,
                                TRUE ~ c(death_time)))

#table(df$mort_28, useNA = "always")
#table(df$mort_60, useNA = "always")
#table(df$mort_60, df$death_reached, useNA = "always")
#table(df$mort_60, df$death_time, useNA = "always")
#table(df$death_reached, df$death_time, useNA = "always")

# (iv) Alternative definition/analysis: New mechanical ventilation OR death within 28 days
df_ttmv <- df_tte %>% 
  filter(PARAM == "Time to death or first mechanical ventilation (Days)") %>%
  select(id_pat, PARAM, AVAL, CNSR, withdraw, withdraw_time, ae, ae_time, ltfu, ltfu_time)
df_ttmv <- df_ttmv %>% 
  mutate(new_mvd_28 = case_when(CNSR == 0 ~ 1,
                             CNSR == 1 & is.na(withdraw) ~ 0)) # all withdrawals were before day 28; the LTFU was after
df <- left_join(df, df_ttmv[, c("new_mvd_28", "id_pat")], by = join_by(id_pat == id_pat)) ## merge to main df
addmargins(table(df$new_mvd_28, df$trt, useNA = "always"))

# (iv) New mechanical ventilation among survivors within 28 days
df <- df %>% 
  mutate(new_mv_28 = case_when(clinstatus_baseline %in% c("1","2","3","4") & new_mvd_28 == 1 & mort_28 == 0 ~ 1,
                               clinstatus_baseline %in% c("1","2","3","4") & new_mvd_28 == 0 & mort_28 == 0 ~ 0))

# (vi) Time to discharge or reaching discharge criteria up to day 28
df_ttdis <- df_tte %>% 
  filter(PARAM == "Time to hospital discharge (Days)") %>%
  select(id_pat, PARAM, AVAL, CNSR, withdraw, withdraw_time, ae, ae_time, ltfu, ltfu_time)
df_ttdis <- df_ttdis %>% 
  mutate(discharge_reached = case_when(CNSR == 0 & AVAL < 29  ~ 1,
                                       CNSR == 0 & AVAL > 28  ~ 0,
                                CNSR == 1 & is.na(withdraw) ~ 0)) %>%  # one withdrawal after discharge (count as 1)
  mutate(discharge_time = AVAL)
df <- left_join(df, df_ttdis[, c("discharge_reached", "discharge_time", "id_pat")], by = join_by(id_pat == id_pat)) ## merge to main df
df <- df %>% 
  mutate(discharge_reached = case_when(is.na(discharge_reached) & mort_28 == 1 ~ 0,
                                       TRUE ~ discharge_reached))
addmargins(table(df$discharge_reached, df$trt, useNA = "always")) # 1 missing less than death, correct.
df <- df %>% # Cap at 28 days
  mutate(discharge_time = case_when(discharge_time >28 ~ 28,
                                    TRUE ~ c(discharge_time)))
df <- df %>% # add 28d for those that died // Patients who died prior to day 28 are assumed not having reached discharge, i.e. counted as 28 days
  mutate(discharge_time_sens = case_when(mort_28 == 1 ~ 28,
                                         TRUE ~ discharge_time))

# (v) Clinical status at day 28
## first, curate clinical score long format dataset
df_score <- df_score %>% 
  mutate(id_pat = sapply(str_split(USUBJID, "_"), function(x) x[2]))
df_score <- score_transform(df_score, clinstatus, AVAL) # transform the score to our score
df_score <- df_score %>%
  filter(!(EPOCH == "SCREENING"))
# second, reformat into wide format
df_cs_wide <- df_score %>% ## the ones with missing baseline clinstatus are missing
  pivot_wider(id_cols = c("id_pat"),
              names_from = "AVISIT",
              values_from = "clinstatus") %>% 
  rename(clinstatus_1 = `Day 1`,
         clinstatus_2 = `Day 2`,
         clinstatus_3 = `Day 3`,
         clinstatus_4 = `Day 4`,
         clinstatus_5 = `Day 5`,
         clinstatus_6 = `Day 6`,
         clinstatus_7 = `Day 7`,
         clinstatus_8 = `Day 8`,
         clinstatus_9 = `Day 9`,
         clinstatus_10 = `Day 10`,
         clinstatus_11 = `Day 11`,
         clinstatus_12 = `Day 12`,
         clinstatus_13 = `Day 13`,
         clinstatus_14 = `Day 14`,
         clinstatus_15 = `Day 15`,
         clinstatus_16 = `Day 16`,
         clinstatus_17 = `Day 17`,
         clinstatus_18 = `Day 18`,
         clinstatus_19 = `Day 19`,
         clinstatus_20 = `Day 20`,
         clinstatus_21 = `Day 21`,
         clinstatus_22 = `Day 22`,
         clinstatus_23 = `Day 23`,
         clinstatus_24 = `Day 24`,
         clinstatus_25 = `Day 25`,
         clinstatus_26 = `Day 26`,
         clinstatus_27 = `Day 27`,
         clinstatus_28 = `Day 28`,
         clinstatus_29 = `Day 29`) %>% 
  select(id_pat, clinstatus_1, clinstatus_2, clinstatus_3, clinstatus_4, clinstatus_5, clinstatus_6, clinstatus_7, 
         clinstatus_8, clinstatus_9, clinstatus_10, clinstatus_11, clinstatus_12, clinstatus_13, clinstatus_14, 
         clinstatus_15, clinstatus_16, clinstatus_17, clinstatus_18, clinstatus_19, clinstatus_20, clinstatus_21, 
         clinstatus_22, clinstatus_23, clinstatus_24, clinstatus_25, clinstatus_26, clinstatus_27, clinstatus_28, 
         clinstatus_29)

df <- left_join(df, df_cs_wide, by = join_by(id_pat == id_pat)) 

## Imputation according to protocol: If there was daily data for the ordinal score available but with missing data for single days, then we carried last observed value forward unless for day 28, whereby we first considered data from the window (+/-3 days) but there was nothing in that window => LVCF
#table(df$clinstatus_28_imp, useNA = "always")
df <- df %>% # see imputation rules
  mutate(clinstatus_28 = case_when(is.na(clinstatus_28) & !is.na(clinstatus_29) ~ clinstatus_29,
                                             TRUE ~ clinstatus_28))
df <- df %>% # see imputation rules
  mutate(clinstatus_28 = case_when(is.na(clinstatus_28) & !is.na(clinstatus_27) ~ clinstatus_27,
                                       TRUE ~ clinstatus_28))
df <- df %>% # see imputation rules
  mutate(clinstatus_baseline_imp = case_when(is.na(clinstatus_baseline) ~ "5",
                                       TRUE ~ clinstatus_baseline))
dfcs <- df %>% # see imputation rules
  select(id_pat, clinstatus_baseline_imp, clinstatus_1, clinstatus_2, clinstatus_3, clinstatus_4, clinstatus_5, clinstatus_6, clinstatus_7, clinstatus_8, clinstatus_9, clinstatus_10, clinstatus_11, clinstatus_12, clinstatus_13, clinstatus_14, clinstatus_15, clinstatus_16, clinstatus_17, clinstatus_18, clinstatus_19, clinstatus_20, clinstatus_21, clinstatus_22, clinstatus_23, clinstatus_24, clinstatus_25, clinstatus_26, clinstatus_27, clinstatus_28)

impute_last_forw = function(df){
  first = which(names(df)%in%c("clinstatus_baseline_imp"))
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
dfcs <- dfcs %>% 
  rename(clinstatus_28_imp = clinstatus_28)
df <- left_join(df, dfcs[, c("clinstatus_28_imp", "id_pat")], by = join_by(id_pat == id_pat)) # Merge imputed variable back
#table(df$clinstatus_28_imp, useNA = "always")
#table(df$discharge_reached, df$clinstatus_28_imp)

# (vi) Sens-analysis: Alternative definition/analysis of outcome: time to sustained discharge within 28 days. There are 8 re-admissions ??
df <- df %>%
  mutate(discharge_reached_sus = discharge_reached) %>% 
  mutate(discharge_time_sus = discharge_time)


# (vii) Viral clearance up to day 5, day 10, and day 15 (Viral load value <LOQ and/or undectectable) // not available

# (viii) Quality of life at day 28: Not available


# (ix) Participants with an adverse event grade 3 or 4, or a serious adverse event, excluding death, by day 28
df_ae <- df_ae %>% 
  mutate(id_pat = sapply(str_split(USUBJID, "_"), function(x) x[2]))
df_ae34 <- df_ae %>%
  filter(AETOXGRN %in% c(3,4,5) | AESER == "Y") %>%
  filter(EPOCH == "TREATMENT") %>% 
  filter(ASTDY <29)
# Keep just 1 id_pat (-> ANY adverse event grade 3 (severe), 4 (serious))
df_ae34_unique <- df_ae34 %>% distinct(id_pat, .keep_all = TRUE)
# Assign the outcome
df_ae34_unique$ae_28 <- 1
#table(df_ae34_unique$ae_28)
# merge
df <- left_join(df, df_ae34_unique[, c("ae_28", "id_pat")], by = join_by(id_pat == id_pat)) ## merge variable to main df
# the remaining missing have no AE grade 34 -> recode as 0
df <- df %>%
  mutate(ae_28 = case_when(is.na(ae_28) ~ 0,
                           TRUE ~ ae_28))
#table(df$ae_28, df$mort_28, useNA = "always")
#addmargins(table(df$ae_28, df$trt, useNA = "always"))

# (ix) Sens-analysis: Alternative definition/analysis of outcome: incidence rate ratio (Poisson regression) -> AE per person by d28
ae_npp <- df_ae34 %>%
  group_by(id_pat)%>%
  summarise(ae_28_sev = n())
df <- left_join(df, ae_npp[, c("ae_28_sev", "id_pat")], by = join_by(id_pat == id_pat)) # merge variable to main df
# the remaining missing have no AE grade 34 -> recode as 0 and exclude deaths
df <- df %>%
  mutate(ae_28_sev = case_when(is.na(ae_28_sev) ~ 0,
                               TRUE ~ ae_28_sev))
# table(df$ae_28_sev, df$mort_28, useNA = "always")
# addmargins(table(df$ae_28_sev, df$trt, useNA = "always"))

# (ix) Sens-analysis: Alternative definition/analysis of outcome: time to first (of these) adverse event, within 28 days, considering death as a competing risk (=> censor and set to 28 days)
# re-discuss

# (x) Adverse events of special interest within 28 days: a) thromboembolic events (venous thromboembolism, pulmonary embolism, arterial thrombosis), b) secondary infections (bacterial pneumonia including ventilator-associated pneumonia, meningitis and encephalitis, endocarditis and bacteremia, invasive fungal infection including pulmonary aspergillosis), c) Reactivation of chronic infection including tuberculosis, herpes simplex, cytomegalovirus, herpes zoster and hepatitis B, d) serious cardiovascular and cardiac events (including stroke and myocardial infarction), e) events related to signs of bone marrow suppression (anemia, lymphocytopenia, thrombocytopenia, pancytopenia), f) malignancy, g) gastrointestinal perforation (incl. gastrointestinal bleeding/diverticulitis), h) liver dysfunction/hepatotoxicity (grade 3 and 4)
df_ae_tot <- df_ae %>%
  filter(EPOCH == "TREATMENT") %>% 
  filter(ASTDY <29)
df_ae_tot <- df_ae_tot %>%
  rename(ae = AEDECOD,
         ae_class = AESOC,
         ae_desc = AEHLT) %>%
  select(id_pat, ae, ae_desc, ae_class, AEHLGT, AELLT)
df_ae_tot <- left_join(df_ae_tot, df[, c("trt", "id_pat")], by = join_by(id_pat == id_pat))

df_thrombo <- df_ae_tot %>% # a) thromboembolic events (venous thromboembolism, pulmonary embolism, arterial thrombosis)
  filter(grepl("thrombos|embo|occl", ae, ignore.case = TRUE)) %>%
  mutate(aesi = "thrombo")
df_sec_inf <- df_ae_tot %>% # b) secondary infections (bacterial pneumonia including ventilator-associated pneumonia, meningitis and encephalitis, endocarditis and bacteremia, invasive fungal infection including pulmonary aspergillosis), but not COVID-19 pneumonia!
  filter(ae_class %in% c("Infections and infestations") & !grepl("shock|herpes|COVID-19|sinusitis|Hordeolum|antibiotic|conjunctivi|ear", ae, ignore.case = TRUE)) %>%
  mutate(aesi = "sec_inf")
df_reactivate <- df_ae_tot %>% # c) Reactivation of chronic infection including tuberculosis, herpes simplex, cytomegalovirus, herpes zoster and hepatitis B
  filter(grepl("zoster|herpes|cytom|tuber|tb|myco", ae, ignore.case = TRUE)) %>%
  mutate(aesi = "reactivate")
df_cardiac <- df_ae_tot %>% # d) serious cardiovascular and cardiac events (including stroke and myocardial infarction) (excl. hypertension)
  filter(ae_class %in% c("Cardiac disorders") | grepl("stroke|cerebrovascular|infarction|ischaemia|ischemia", ae, ignore.case = TRUE) | grepl("stroke|cerebrovascular|infarction|ischaemia|ischemia", ae_desc, ignore.case = TRUE)) %>%
  mutate(aesi = "cardiac")
df_penia <- df_ae_tot %>% # e) events related to signs of bone marrow suppression (anemia, lymphocytopenia, thrombocytopenia, pancytopenia)
  filter(grepl("penia|anemia|anaemia", ae, ignore.case = TRUE) | grepl("penia|anemia|anaemia", ae_desc, ignore.case = TRUE)) %>%
  mutate(aesi = "penia")
df_malig <- df_ae_tot %>% # f) malignancy
  filter(ae_class %in% c("Neoplasms benign, malignant and unspecified (incl cysts and polyps)")) %>%
  mutate(aesi = "malig")
df_git_bl <- df_ae_tot %>% # g) gastrointestinal perforation (incl. gastrointestinal bleeding/diverticulitis)
  filter(ae_class %in% c("Hepatobiliary disorders","Gastrointestinal disorders") & (grepl("hemor|haemor|bleed", ae, ignore.case = TRUE) | grepl("hemor|haemor|bleed", ae_desc, ignore.case = TRUE))) %>%
  mutate(aesi = "git_bl")
df_hepatox <- df_ae_tot %>% # h) liver dysfunction/hepatotoxicity (grade 3 and 4)
  filter(ae_class %in% c("Hepatobiliary disorders") & grepl("hepatox|tox|liver injury|damage|failure|hypertrans|abnormal|hyperbili", ae, ignore.case = TRUE)) %>%
  mutate(aesi = "hepatox")
df_mods <- df_ae_tot %>% # i) Multiple organ dysfunction syndrome and septic shock
  filter(grepl("Multiple organ dysfunction syndrome|mods|shock", ae, ignore.case = TRUE)) %>%
  mutate(aesi = "mods")

df_aesi <- rbind(df_mods, df_hepatox, df_git_bl, df_malig, df_penia, df_cardiac, df_reactivate, df_sec_inf, df_thrombo)
df_aesi <- df_aesi %>%
  select(id_pat, trt, aesi, ae, ae_desc, ae_class)
table(df_aesi$trt, df_aesi$aesi)
# double-check if there are any duplicate AEs within the same person and if it is the same event or distinct ones
df_aesi <- df_aesi %>%
  group_by(id_pat) %>%
  mutate(duplicate_id = duplicated(ae) & !is.na(ae)) %>%
  ungroup()
df_aesi <- df_aesi %>%
  filter(duplicate_id == F)
# Save
saveRDS(df_aesi, file = "df_aesi_ruxcovid.RData")
# table, by arm
char_vars <- c("trt", "aesi")
df_aesi <- df_aesi %>%
  mutate(across(all_of(char_vars), factor)) %>% 
  select("trt", "aesi")
aesi_table <- CreateTableOne(data = df_aesi, vars = char_vars[!char_vars %in% c("trt")], strata = "trt", includeNA = FALSE, test = F, addOverall = TRUE)
capture.output(aesi_table <- print(aesi_table, nonnormal = vars.list,catDigits = 1,SMD = TRUE,showAllLevels = TRUE,test = TRUE,printToggle = FALSE,missing = FALSE))
#print
kable(aesi_table, format = "markdown", table.attr = 'class="table"', caption = "AESI, by arm") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
saveRDS(aesi_table, file = "aesi_table.rds")

# (xi) Adverse events, any grade and serious adverse event, excluding death, within 28 days, grouped by organ classes
df_ae <- df_ae_tot %>%
  select(id_pat, trt, ae, ae_desc, ae_class)
# double-check if there are any duplicate AEs within the same person and if it is the same event or distinct ones
df_ae <- df_ae %>%
  group_by(id_pat) %>%
  mutate(duplicate_id = duplicated(ae) & !is.na(ae)) %>%
  ungroup()
df_ae <- df_ae %>%
  filter(duplicate_id == F)
# Save
saveRDS(df_ae, file = "df_ae_ruxcovid.RData")
# table, by arm
char_vars <- c("trt", "ae", "ae_class")
df_ae <- df_ae %>%
  mutate(across(all_of(char_vars), factor)) %>% 
  select("trt", "ae", "ae_class")
ae_table <- CreateTableOne(data = df_ae, vars = c("ae"), strata = "trt", includeNA = FALSE, test = F, addOverall = TRUE)
capture.output(ae_table <- print(ae_table, nonnormal = vars.list,catDigits = 1,SMD = TRUE,showAllLevels = TRUE,test = TRUE,printToggle = FALSE,missing = FALSE))
#print
kable(ae_table, format = "markdown", table.attr = 'class="table"', caption = "AE, by arm") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
saveRDS(ae_table, file = "ae_table.rds")


###############
## Define datasets
# keep the overall set
df_all <- df
# reduce the df set to our standardized set across all trials
df <- df %>%
  select(id_pat, trt, sex, age, trial, JAKi,
         ethn,
         country, icu, sympdur,
         vacc,
         clinstatus_baseline, vbaseline,
         comed_dexa, comed_rdv, comed_toci, comed_ab, comed_acoa, comed_interferon, comed_other,
         comed_cat,
         comorb_lung, comorb_liver, comorb_cvd, comorb_aht, comorb_dm, comorb_obese, comorb_smoker, immunosupp,
         comorb_autoimm, comorb_cancer, comorb_kidney,
         any_comorb, comorb_cat, comorb_any, comorb_count,
         crp, at_risk,
         # sero, variant,
         # vl_baseline,
         mort_28, mort_28_dimp,
         mort_60, death_reached, death_time,
         new_mv_28, new_mvd_28,
         clinstatus_28_imp,
         discharge_reached, discharge_time, discharge_time_sens, discharge_reached_sus, discharge_time_sus,
         ae_28, ae_28_sev
         # vir_clear_5, vir_clear_10, vir_clear_15
  )

# export for one-stage model, i.e., add missing variables
df_os <- df
df_os$sero <- NA
df_os$variant <- NA
df_os$vl_baseline <- NA
df_os$vir_clear_5 <- NA
df_os$vir_clear_10 <- NA
df_os$vir_clear_15 <- NA
# Save
saveRDS(df_os, file = "df_os_ruxcovid.RData")


######
# Bar plot, missing data, each data point, standardized one-stage dataset
original_order <- colnames(df_os)
missing_plot <- df_os %>%
  summarise_all(~ mean(is.na(.))) %>%
  gather() %>%
  mutate(key = factor(key, levels = original_order)) %>%
  ggplot(aes(x = key, y = value)) +
  geom_bar(stat = "identity") +
  labs(x = "Columns", y = "Proportion of Missing Values", title = "Missing Data - standardized one-stage dataset") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 1)
#print(missing_plot)

# 1. Missing variables:
#  * Baseline:
#  - country
#  - sero, variant, viremia
#  - smoking, liver, immuno, autoimmun

#  * Outcomes:
#  - qol_28, viremia
# 2. Missing data in:
#  - clinstatus_baseline, crp, 
# - comed_other, comorb_obese
# - sympdur
# - crp
# - new_mv_28 / ae_28: also, some part of denominator
# - new_mvd_28, mort_28/60


#####
# Missing data: Explore for MI

# keep the core df
# names(df_all)
df_core <- df_all %>%
  select(id_pat, trt, sex, age, trial, JAKi, ethn,
         vacc,
         country, icu, sympdur, clinstatus_baseline, vbaseline,
         comed_dexa, comed_rdv, comed_toci, comed_ab, comed_acoa, comed_interferon, comed_other,
         comed_cat,
         comorb_lung, comorb_liver, comorb_cvd, comorb_aht, comorb_dm, comorb_obese, comorb_smoker, immunosupp,
         comorb_autoimm, comorb_cancer, comorb_kidney,
         any_comorb, comorb_cat, comorb_any, comorb_count,
         crp, 
         # vl_baseline,
         # sero, variant,
         clinstatus_1, clinstatus_2, clinstatus_3, clinstatus_4, clinstatus_5, clinstatus_6, clinstatus_7, clinstatus_8, clinstatus_9, clinstatus_10, clinstatus_11, clinstatus_12, clinstatus_13, clinstatus_14, clinstatus_15, clinstatus_16, clinstatus_17, clinstatus_18, clinstatus_19, clinstatus_20, clinstatus_21, clinstatus_22, clinstatus_23, clinstatus_24, clinstatus_25, clinstatus_26, clinstatus_27, clinstatus_28, clinstatus_29,
         clinstatus_28_imp,
         mort_28, mort_28_dimp, mort_60, death_reached, death_time,
         new_mv_28, new_mvd_28,
         discharge_reached, discharge_time, discharge_time_sens, discharge_reached_sus, discharge_time_sus,
         #vir_clear_5, vir_clear_10, vir_clear_15,
         ae_28, ae_28_sev
  )

# Convert character variables to factors
char_vars <- c("id_pat", "sex", "trial", "JAKi", "country", "icu", "ethn", "clinstatus_baseline", "vbaseline",
               "comed_dexa", "comed_rdv", "comed_toci", "comed_ab", "comed_acoa", "comed_interferon", "comed_other", "comed_cat",
               "comorb_lung", "comorb_liver", "comorb_cvd", "comorb_aht", "comorb_dm", "comorb_obese", "comorb_smoker", "immunosupp", "any_comorb", "comorb_cat", "comorb_any", "comorb_autoimm","comorb_cancer", "comorb_kidney", "clinstatus_28_imp", "mort_28", "mort_28_dimp", "mort_60", "death_reached", "new_mv_28", "new_mvd_28","discharge_reached", "discharge_reached_sus", "ae_28")
df_core <- df_core %>%
  mutate(across(all_of(char_vars), factor))

# Bar plot, missing data, each data point, core dataset
original_order <- colnames(df_core)
missing_plot <- df_core %>%
  summarise_all(~ mean(is.na(.))) %>%
  gather() %>%
  mutate(key = factor(key, levels = original_order)) %>%
  ggplot(aes(x = key, y = value)) +
  geom_bar(stat = "identity") +
  labs(x = "Columns", y = "Proportion of Missing Values", title = "Missing Data - core dataset") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 1)
#print(missing_plot)
# Bar plot, missing data, each data point, core dataset, by arm
df_core_int <- df_core %>%
  filter(trt == 1)
original_order <- colnames(df_core_int)
missing_plot <- df_core_int %>% # Intervention arm
  summarise_all(~ mean(is.na(.))) %>%
  gather() %>%
  mutate(key = factor(key, levels = original_order)) %>%
  ggplot(aes(x = key, y = value)) +
  geom_bar(stat = "identity") +
  labs(x = "Columns", y = "Proportion of Missing Values", title = "Missing Data - core dataset, intervention") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 1)
#print(missing_plot)
df_core_cont <- df_core %>%
  filter(trt == 0)
original_order <- colnames(df_core_cont)
missing_plot <- df_core_cont %>% # Control arm
  summarise_all(~ mean(is.na(.))) %>%
  gather() %>%
  mutate(key = factor(key, levels = original_order)) %>%
  ggplot(aes(x = key, y = value)) +
  geom_bar(stat = "identity") +
  labs(x = "Columns", y = "Proportion of Missing Values", title = "Missing Data - core dataset, control") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 1)
#print(missing_plot)

### Baseline table, by individuals with no missing data vs any missing data (or only in mort_28)
# df_core <- df_core %>% mutate(complete = ifelse(rowSums(is.na(.)) > 0, 0, 1));table(df_core$complete) # ANY missing
df_core$resp<-ifelse(is.na(df_core$mort_28), 0, 1);table(df_core$resp) # only mort_28 missing

### Define variables to be included in imputation set
table(df_core$comorb_cat)
df_imp <- df_core %>%
  select("id_pat"
         , "trt", "sex", "age" , "ethn"
         #, "country" # no info
         , "sympdur"
         # ,"vacc" # no info
         # , "trial", "JAKi"  # only 0
         ,"clinstatus_baseline"
         # , "vbaseline" # derived
         , "comed_rdv" # no info
         # , "comed_toci", "comed_interferon" # no info
         #,  "comed_cat", # derived
         , "comed_dexa", "comed_ab"
         , "comed_acoa" # no info
         #, "comed_other" # no much more info
         # , "comorb_lung", "comorb_liver", "comorb_cvd", "comorb_aht", "comorb_dm", "comorb_obese",
         # "comorb_smoker", "immunosupp", "comorb_autoimm", "comorb_cancer", "comorb_kidney", "any_comorb",
         # "comorb_count",
         # "comorb_any",
         ,"comorb_cat" # derived from above, contains most information, and needed as interaction term
         ,"crp"
         #,"vl_baseline" # no info
         # , "sero" , "variant" # no info
         , clinstatus_1, clinstatus_2, clinstatus_3, clinstatus_4, clinstatus_5, clinstatus_6, clinstatus_7, clinstatus_8, clinstatus_9, clinstatus_10, clinstatus_11, clinstatus_12, clinstatus_13, clinstatus_14, clinstatus_15, clinstatus_16, clinstatus_17, clinstatus_18, clinstatus_19, clinstatus_20, clinstatus_21, clinstatus_22, clinstatus_23, clinstatus_24, clinstatus_25, clinstatus_26, clinstatus_27, clinstatus_28
         # , "clinstatus_28_imp" # imputed via LOVCF above
         , "mort_28"
         # , "mort_28_dimp" # imputed deterministically
         # , "mort_60" # does not contain any additional information compared to death reached
         , "death_reached", "death_time", "new_mv_28", "new_mvd_28", "discharge_reached", "discharge_time"
         # , "discharge_reached_sus", "discharge_time_sus" # same as discharge, does not contain any addition information
         , "ae_28", "ae_28_sev"
         #, "vir_clear_5", "vir_clear_10", "vir_clear_15"
  )

# First, table and visualize missing data in various ways
# df_imp %>%
#   ff_glimpse() # from finalfit package
#df_imp %>%
#  missing_plot() # from finalfit package
#explanatory = c("age",
#                "clinstatus_baseline", "sex",
#                "ethn", "country", "sympdur", "comorb_cat", "comed_dexa", "comed_ab", "comed_other", "crp", "vl_baseline", "ae_28_sev")
#dependent = "mort_28"
#df_imp %>% # from finalfit package, missing plot
#  missing_pairs(dependent, explanatory, position = "fill", )

# Second, let's explore the missingness patterns
md.pattern(df_imp[,c("mort_28", "age",
                     "clinstatus_baseline", "sex",
                     "ethn", "sympdur", "comorb_cat", "comed_dexa", "comed_ab", "comed_rdv", "comed_acoa","crp", "ae_28_sev")], rotate.names = T)
# Third, let's explore if the variables from my substantive model plus auxiliary variables are associated with mort_28
mort28.aux <- glm(mort_28 ~ trt
                  + age
                  + clinstatus_baseline
                  + sex
                  #+ ethn
                  + sympdur
                  + comorb_cat
                  + comed_dexa
                  + comed_ab
                  + comed_rdv
                  + comed_acoa
                  + crp
                  # + ae_28_sev
                  ,family="binomial"
                  ,data=df_imp)
summary(mort28.aux)
# Fourth, let's explore if they are associated with missingness of mort_28:
#df_imp %>%
#  missing_compare(dependent, explanatory) %>%
#  knitr::kable(row.names=FALSE, align = c("l", "l", "r", "r", "r"))
# Fifth, check age
summary(df_imp$age)
hist(df_imp$age, breaks=50) # looks fine
# Sixth, check sympdur
summary(df_imp$sympdur)
hist(df_imp$sympdur, breaks=50) # skewed -> transform
df_imp$sqsympdur=sqrt(df_imp$sympdur)
hist(df_imp$sqsympdur) # looks fine
# Seventh, check crp
summary(df_imp$crp)
hist(df_imp$crp, breaks=50) # outliers
df_imp <- df_imp %>% # truncate outliers > 500
  mutate(crptrunc = case_when(crp > 500 ~ 500,
                              TRUE ~ crp))
hist(df_imp$crptrunc, breaks=50)
df_imp$sqcrptrunc=sqrt(df_imp$crptrunc)
hist(df_imp$sqcrptrunc, breaks=50) # looks fine


### Reshape to long format
#names(df_imp)
# str(df_imp)
df_imp <- df_imp %>% # rename to differentiate between baseline clinical status and follow-up clinical statuses
  rename(clinicalstatus_baseline = clinstatus_baseline)
# reshape
df_imp_long <- df_imp %>%
  pivot_longer(cols = starts_with("clinstatus"), names_to = "time", values_to = "clinstatus")
# names(df_imp_long)
# Convert time to numeric
df_imp_long$time <- as.numeric(gsub("clinstatus_", "", df_imp_long$time))
# class(df_imp_long$time)
# summary(df_imp_long$time)
# have clinstatus as numeric
# class(df_imp_long$clinstatus)
# table(df_imp_long$clinstatus_n)
df_imp_long$clinstatus_f <- factor(df_imp_long$clinstatus, levels = 1:6)
df_imp_long$clinstatus_n <- as.numeric(df_imp_long$clinstatus_f)
# table(df_imp_long$clinstatus_n)

### We will impute separately by treatment arm, since we have to expect an effect modification between outcome x trt over time
df_imp_long_int <- df_imp_long %>%
  filter(trt == 1)
df_imp_long_cont <- df_imp_long %>%
  filter(trt == 0)

## Explore distribution of clinical status over time, by arm
plot_clinstat_int <- ggplot(df_imp_long_int, aes(x = time, y = clinstatus_n)) +
  stat_summary(fun = "mean", geom = "point", color = "blue", size = 3, na.rm = TRUE) +
  geom_smooth(method = "loess", se = FALSE, color = "red", na.rm = TRUE) +
  labs(title = "Distribution of Clinical Status Over Time / Intervention",
       x = "Time",
       y = "Mean Clinical Status") +
  theme_minimal()
#print(plot_clinstat_int)
plot_clinstat_cont <- ggplot(df_imp_long_cont, aes(x = time, y = clinstatus_n)) +
  stat_summary(fun = "mean", geom = "point", color = "blue", size = 3, na.rm = TRUE) +
  geom_smooth(method = "loess", se = FALSE, color = "red", na.rm = TRUE) +
  labs(title = "Distribution of Clinical Status Over Time / Control",
       x = "Time",
       y = "Mean Clinical Status") +
  theme_minimal()
#print(plot_clinstat_cont)




##### 
# MULTIPLE IMPUTATION
# INTERVENTION group
## jomo only accepts numeric or factors, check and adapt
#str(df_imp_long_int)
#df_imp_long_int$timesq <- sqrt(df_imp_long_int$time) # see X below
#df_imp_long_int <- df_imp_long_int %>%
#  mutate(clinicalstatus_baseline = case_when(is.na(clinicalstatus_baseline) ~ "5", # only for X below, see rules in Notes
#                                             TRUE ~ clinicalstatus_baseline))
#attach(df_imp_long_int)
#Y2<-data.frame(mort_28 # level 2 variables (baseline patient characteristics)
#               , age
#               , sex
#               , ethn
#               , comed_dexa
#               , comed_ab
#               , comed_rdv
#               , comed_acoa
#               , comorb_cat
#               , sqsympdur
#               , sqcrptrunc
#               , ae_28_sev
#)
#Y<-data.frame(clinstatus_n) # level 1 variable within clustering variable
#X <- cbind(1, data.frame(clinicalstatus_baseline, time, timesq)) # matrix modelling linearity of clinstatus throughout day 28
#clus<-data.frame(id_pat) # clustering variable (patient)
#Z<-data.frame(rep(1,dim(df_imp_long_int)[1]),df_imp_long_int[,c("time")]) # random intercept and random slope
#colnames(Z)<-c("const", "time")
#
#nimp<-30 # set number of iterations
#
## run jomo
# dry run
#imputed_int_mcmc<-jomo.MCMCchain(Y=Y, Y2=Y2, X=X, Z=Z, clus=clus, nburn=2)
#set.seed(1569)
#imputed_int <- jomo(Y=Y, Y2=Y2, X=X, Z=Z, clus=clus, nburn=1000, nbetween=1000, nimp=nimp)

# convert to jomo object, split imputations, and exclude original data (imputation "0")
#imp.list_int <- imputationList(split(imputed_int, imputed_int$Imputation)[-1])

# checks
#round(prop.table(table(imp.list_int[[1]]$`1`$mort_28, useNA = "always"))*100,1) # first imputed dataset
#round(prop.table(table(imp.list_int[[1]]$`2`$mort_28, useNA = "always"))*100,1) # second imputed dataset
#round(prop.table(table(df_imp_long_int$mort_28, useNA = "always"))*100,1) # original data
#summary(imp.list_int[[1]]$`1`$sqsympdur)
#summary(imp.list_int[[1]]$`2`$sqcrptrunc)


#### CONTROL group
## jomo only accepts numeric or factors, check and adapt
# str(df_imp_long_cont)
#df_imp_long_cont$timesq <- sqrt(df_imp_long_cont$time) # see X below
#df_imp_long_cont <- df_imp_long_cont %>%
#  mutate(clinicalstatus_baseline = case_when(is.na(clinicalstatus_baseline) ~ "5", # only for X below, see rules in Notes
#                                             TRUE ~ clinicalstatus_baseline))
#attach(df_imp_long_cont)
#Y2<-data.frame(mort_28 # level 2 variables (baseline patient characteristics)
#               , age
#               , sex
#               , ethn
#               , comed_dexa
#               , comed_ab
#               , comed_rdv
#               , comed_acoa
#               , comorb_cat
#               , sqsympdur
#               , sqcrptrunc
#               , ae_28_sev
#)
#Y<-data.frame(clinstatus_n) # level 1 variable within clustering variable
#X <- cbind(1, data.frame(clinicalstatus_baseline, time, timesq)) # matrix modelling linearity of clinstatus throughout day 28
#clus<-data.frame(id_pat) # clustering variable (patient)
#Z<-data.frame(rep(1,dim(df_imp_long_cont)[1]),df_imp_long_cont[,c("time")]) # random intercept and random slope
#colnames(Z)<-c("const", "time")
#
#nimp<-30 # set number of iterations
#
# run jomo
#set.seed(1569)
#imputed_cont <- jomo(Y=Y, Y2=Y2, X=X, Z=Z, clus=clus, nburn=1000, nbetween=1000, nimp=nimp)

# convert to jomo object, split imputations, and exclude original data (imputation "0")
#imp.list_cont <- imputationList(split(imputed_cont, imputed_cont$Imputation)[-1])

# checks
#round(prop.table(table(imp.list_cont[[1]]$`1`$mort_28, useNA = "always"))*100,1) # first imputed dataset
#round(prop.table(table(imp.list_cont[[1]]$`2`$mort_28, useNA = "always"))*100,1) # second imputed dataset
#round(prop.table(table(df_imp_long_cont$mort_28, useNA = "always"))*100,1) # original data
#summary(imp.list_cont[[1]]$`1`$comorb_cat)
#summary(imp.list_cont[[1]]$`2`$sqsympdur)


#### Add trt back, change from long to wide format, and finally combine the two data frames
#imputed_int$trt <- 1
#imputed_int_s <- imputed_int %>% # remove imputation variables, not needed anymore
#  select(trt, age, sex, ethn, comed_dexa, comed_ab, comed_rdv, comed_acoa, comorb_cat, sqsympdur, sqcrptrunc, ae_28_sev, mort_28, clinicalstatus_baseline, clus, Imputation)
#imputed_int_wide <- imputed_int_s %>% # change from long to wide format, i.e. remove duplicates within Imputation sets
#  group_by(Imputation) %>%
#  distinct(clus, .keep_all = TRUE)

#imputed_cont$trt <- 0 # treatment variable
#imputed_cont_s <- imputed_cont %>% # remove imputation variables, not needed anymore
#  select(trt, age, sex, ethn, comed_dexa, comed_ab, comed_rdv, comed_acoa, comorb_cat, sqsympdur, sqcrptrunc, ae_28_sev, mort_28, clinicalstatus_baseline, clus, Imputation)
#imputed_cont_wide <- imputed_cont_s %>% # change from long to wide format, i.e. remove duplicates within Imputation sets
#  group_by(Imputation) %>%
#  distinct(clus, .keep_all = TRUE)

#imputed_combined <- rbind(imputed_cont_wide, imputed_int_wide)


#### Convert combined df to jomo object, split imputations, and exclude original data (imputation "0")
#imp.list <- imputationList(split(imputed_combined, imputed_combined$Imputation)[-1])


### Checks
#round(prop.table(table(imp.list[[1]]$`1`$mort_28, imp.list[[1]]$`1`$trt, useNA = "always"),2)*100,1) # first imputed dataset
#round(prop.table(table(imp.list[[1]]$`2`$mort_28, imp.list[[1]]$`2`$trt, useNA = "always"),2)*100,1) # second imputed dataset
#round(prop.table(table(df_imp$mort_28, df_imp$trt, useNA = "always"),2)*100,1) # original data
#summary(imp.list[[1]]$`1`$comorb_cat)
#summary(imp.list[[1]]$`2`$sqsympdur)



######
# PRIMARY endpoint

addmargins(table(df$mort_28, df$trt, useNA = "always"))
addmargins(table(df$mort_28_dimp, df$trt, useNA = "always"))
df$clinstatus_baseline_n <- as.numeric(df$clinstatus_baseline)

# Complete case analysis, substantive model
mort.28 <- df %>%
  glm(mort_28 ~ trt
      + age + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(mort.28))

# Deterministic imputation
mort.28.dimp <- df %>%
  glm(mort_28_dimp ~ trt
      + age + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(mort.28.dimp))

# Multiple imputation analysis under MAR; use mitools package to fit imputed and combined data list and apply Rubin's rules
#mort.28.mi <- imp.list %>%
#  with(glm(mort_28 ~ trt
#           + age
#           + clinicalstatus_baseline
#           , family = binomial)) %>%
#        pool() %>%
#        summary(conf.int = T, exponentiate = T)
#print(mort.28.mi)


# unadjusted estimator for the (absolute) risk difference
mort.28.prop.test <- prop.test(x = with(df, table(trt, mort_28)))
print(mort.28.prop.test)
# Estimate
-diff(mort.28.prop.test$estimate)
# Confidence Interval
mort.28.prop.test$conf.int
# P-Value
mort.28.prop.test$p.value

# Covariate-Adjusted Analysis
# Fit the `glm` object
# Same as Complete case analysis, substantive model // but don't use piping, otherwise problem in margins::margins
df_mort28_comp <- df %>% filter(!is.na(mort_28))
mort.28.cov.adj <-
  glm(formula = mort_28 ~ trt + age + clinstatus_baseline,
      data = df_mort28_comp,
      family = binomial(link = "logit")
  )
# Print a summary of the `glm` object
summary(mort.28.cov.adj)
# Predict Pr{Y = 1 | Z = 1, X} // equals: E(Y|Z=1,X)
pr_y1_z1 <-
  predict(
    object = mort.28.cov.adj,
    newdata =
      df_mort28_comp %>%
      dplyr::mutate(
        trt = 1
      ),
    type = "response"
  )
# Predict Pr{Y = 1 | Z = 0, X} // equals: E(Y|Z=0,X)
pr_y1_z0 <-
  predict(
    object = mort.28.cov.adj,
    newdata =
      df_mort28_comp %>%
      dplyr::mutate(
        trt = 0
      ),
    type = "response"
  )

# Estimate RD
adj_mean = mean(pr_y1_z1) - mean(pr_y1_z0)
print(adj_mean)
mort.28.ame$adj_mean <- adj_mean
# Standard Error RD
# The variance/standard error can be calculted as 1/n times the sample variance of:
# Z/P(Z=1)*[Y-E(Y|Z=1,X)] + E(Y|Z=1,X) - ((1-Z)/(1-P(1=Z))*[Y-E(Y|Z=0,X)] + E(Y|Z=0,X))
p_arm = mean(df_mort28_comp$trt==1)
adj_se = sqrt(
  var((df_mort28_comp$trt==1)/p_arm * (df_mort28_comp$mort_28 - pr_y1_z1) + pr_y1_z1 -
        ((df_mort28_comp$trt==0)/(1-p_arm) * (df_mort28_comp$mort_28-pr_y1_z0) + pr_y1_z0))/
    nrow(df_mort28_comp))
print(adj_se)
adj_mean_se <- adj_se
# Confidence Interval
c(adj_mean-qnorm(0.975)*adj_se, adj_mean+qnorm(0.975)*adj_se)
adj_mean_ci <- c(adj_mean-qnorm(0.975)*adj_se, adj_mean+qnorm(0.975)*adj_se)
# Or, we can obtain the standard error of the estimate two ways. The first way is using the margins::margins() command, using the robust standard errors from sandwich::vcovHC // The second way to obtain these would be the bias corrected and accelerated (BCa) non-parametric bootstrap
# Youâ€™ll see that we now have a standard error, p-value under the hypothesis that the marginal effect is 0, and a 95% Confidence Interval for the estimate.

# library(sandwich)
# library(margins)# does not work in this environment, take it from the manually calculated approach above
#mort.28.cov.adj.ame <-
#  margins::margins(
#    model = mort.28.cov.adj,
#    # Specify treatment variable
#    variables = "trt",
#    # Convert to outcome scale, not link scale
#    type = "response",
#    # Obtain robust standard errors
#    vcov = sandwich::vcovHC(x = mort.28.cov.adj, type = "HC3")
#  )
# summary(object = mort.28.cov.adj.ame, level = 0.95)
# mort.28.ame <- summary(object = mort.28.cov.adj.ame, level = 0.95)




######
# 60 day mortality

table(df$mort_60, df$trt, useNA = "always")
mort.60 <- df %>%
  glm(mort_60 ~ trt
      + age + clinstatus_baseline
      , family = "binomial", data=.)

######
# time to death

#table(df$death_reached, df$death_time, useNA = "always")
#table(df$death_reached, df$mort_60, useNA = "always")

# df %>%
#   drop_na(death_time) %>%
#   filter(death_reached == 1) %>%
#   group_by(trt) %>%
#   summarise(median = median(death_time),
#             IQR = IQR(death_time),
#             Q1 = quantile(death_time, probs = 0.25),
#             Q3 = quantile(death_time, probs = 0.75))

# time to death, by group. Kaplan-Meier estimate of conditional survival probability.
km.ttdeath.check <- with(df, Surv(death_time, death_reached))
# head(km.ttdeath.check, 100)

km.ttdeath_trt <- survfit(Surv(death_time, death_reached) ~ trt, data=df)
# summary(km.ttdeath_trt, times = 28)
#ttdeath_28d_tbl <- km.ttdeath_trt %>%
#  tbl_survfit(
#    times = 28,
#    label_header = "**28-d survival (95% CI)**"
#  )
# Nicely formatted table
#kable(ttdeath_28d_tbl, format = "markdown", table.attr = 'class="table"') %>%
#  kable_styling(bootstrap_options = "striped", full_width = FALSE)

#autoplot(km.ttdeath_trt)
#survfit2(Surv(death_time, death_reached) ~ trt, data=df) %>%
#  ggsurvfit() +
#  labs(
#    x = "Days",
#    y = "Overall survival probability"
#  ) +
#  add_confidence_interval() +
#  add_risktable()

# testing: simple log-rank
# survdiff(Surv(death_time, death_reached) ~ trt, data = df)
# testing: cox ph
ttdeath <- df %>%
  coxph(Surv(death_time, death_reached) ~ trt
        + age + clinstatus_baseline
        , data =.)
# ttdeath_reg_tbl <- tbl_regression(ttdeath, exp = TRUE)
# Nicely formatted table
# kable(ttdeath_reg_tbl, format = "markdown", table.attr = 'class="table"') %>%
#   kable_styling(bootstrap_options = "striped", full_width = FALSE)


#####
# New MV

addmargins(table(df$new_mv_28, df$trt, useNA = "always"))
new.mv.28 <- df %>%
  glm(new_mv_28 ~ trt
      + age
      + clinstatus_baseline
      , family = "binomial", data=.)

# (iv) Alternative definition/analysis: New mechanical ventilation OR death within 28 days => include all in denominator.
addmargins(table(df$new_mvd_28, df$trt, useNA = "always"))
new.mvd.28 <- df %>%
  glm(new_mvd_28 ~ trt
      + age
      + clinstatus_baseline
      , family = "binomial", data=.)


#####
# Clinstatus day 28
table(df$clinstatus_28_imp, df$trt, useNA = "always")
clin.28 <- df %>%
  clm(clinstatus_28_imp ~ trt
      + age
      + clinstatus_baseline
      , link= c("logit"), data=.)
# Summary and extract coefficients
coefficients_table <- summary(clin.28)$coefficients
# Calculate Odds Ratios and Confidence Intervals
odds_ratios <- exp(coefficients_table[, "Estimate"])
ci_lower <- exp(coefficients_table[, "Estimate"] - 1.96 * coefficients_table[, "Std. Error"])
ci_upper <- exp(coefficients_table[, "Estimate"] + 1.96 * coefficients_table[, "Std. Error"])
# Create a data frame to store Odds Ratios and CIs
clin.28_tbl <- data.frame(
  "Variable" = rownames(coefficients_table),
  "Odds Ratio" = odds_ratios,
  "CI Lower" = ci_lower,
  "CI Upper" = ci_upper
)
# Nicely formatted table
kable(clin.28_tbl, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)


#####
# Time to discharge

# Kaplan-Meier estimate of conditional discharge probability
# Censoring the deaths => Cause-specific hazards, i.e., represents the rate per unit of time of the event among those not having failed from other events. Instantaneous rate of occurrence of the given type of event in subjects who are currently eventâ€free. But by simply censoring the competing event, we bias in favour of comparator (if treatment leads to less deaths)
km.ttdischarge.check <- with(df, Surv(discharge_time, discharge_reached))
# head(km.ttdischarge.check, 100)
km.ttdischarge_trt <- survfit(Surv(discharge_time, discharge_reached) ~ trt, data=df)
# summary(km.ttdischarge_trt, times = 28)
#ttdischarge_28d_tbl <- km.ttdischarge_trt %>%
#  tbl_survfit(
#    times = 28,
#    label_header = "**28-d hospitalization (95% CI)**"
#  )
# Nicely formatted table
#kable(ttdischarge_28d_tbl, format = "markdown", table.attr = 'class="table"') %>%
#  kable_styling(bootstrap_options = "striped", full_width = FALSE)
#survfit2(Surv(discharge_time, discharge_reached) ~ trt, data=df) %>%
#  ggsurvfit() +
#  labs(
#    x = "Days",
#    y = "Overall hospitalization probability"
#  ) +
#  add_confidence_interval() +
#  add_risktable()
# testing: cox ph
ttdischarge <- df %>%
  coxph(Surv(discharge_time, discharge_reached) ~ trt
        + age + clinstatus_baseline
        , data =.)
#ttdischarge_reg_tbl <- tbl_regression(ttdischarge, exp = TRUE)
# Nicely formatted table
#kable(ttdischarge_reg_tbl, format = "markdown", table.attr = 'class="table"') %>%
#  kable_styling(bootstrap_options = "striped", full_width = FALSE)

# Sub-distribution hazards, i.e., represents the rate per unit of time of the event as well as the influence of competing events. Instantaneous rate of occurrence of the given type of event in subjects who have not yet experienced an event of that type.
df <- df %>% # cuminc needs a factor variable with censored patients coded as 0, the event as 1 and the competing event as 2.
  mutate(discharge_reached_comp = case_when (discharge_reached == 0 & (mort_28 == 0 | is.na(mort_28)) ~ 0,
                                             discharge_reached == 1 & (mort_28 == 0 | is.na(mort_28)) ~ 1,
                                             mort_28 == 1 ~ 2))

df$discharge_reached_comp <- as.factor(df$discharge_reached_comp)
# table(df$discharge_reached_comp)
# table(df$discharge_reached_comp, df$discharge_time)
# table(df$discharge_reached, df$discharge_time)

# Cumulative incidence for the event=discharge (1) and the competing event=death (2)

# testing: Fine-Gray regression (but crr/cuminc not working in this environment -> use coxph)
table(df$discharge_reached_comp, useNA = "always")
df2 <- df %>% 
  filter(!(is.na(discharge_reached_comp)))
cuminc <- survfit(Surv(discharge_time, discharge_reached_comp, type = "mstate") ~ trt, data = df2)
plot(cuminc)
df_fg <- finegray(Surv(discharge_time, discharge_reached_comp, type = "mstate") ~ ., etype = 1, data = df2)
ttdischarge.comp <- coxph(Surv(fgstart, fgstop, fgstatus) ~ trt + age, id=id_pat, weight = fgwt, robust=T, data = df_fg)

#ttdischarge_comp_reg_tbl <- tbl_regression(ttdischarge.comp, exp = TRUE)
# Nicely formatted table
#kable(ttdischarge_comp_reg_tbl, format = "markdown", table.attr = 'class="table"') %>%
#  kable_styling(bootstrap_options = "striped", full_width = FALSE)

# Censoring and assigned worst outcome (28d) to competing event (death) // hypothetical estimand where no-one died. (Another option could be, but we don't do it, is to exclude the deaths entirely, i.e. discharge among those that survived (but that might bias in favour of those in control that died more, i.e. healthier comparator))
#survfit2(Surv(discharge_time_sens, discharge_reached) ~ trt, data=df) %>%
#  ggsurvfit() +
#  labs(
#    x = "Days",
#    y = "Overall hospitalization probability"
#  ) +
#  add_confidence_interval() +
#  add_risktable()
# testing: cox ph
ttdischarge.sens <- df %>%
  coxph(Surv(discharge_time_sens, discharge_reached) ~ trt
        + age + clinstatus_baseline
        , data =.)
#ttdischarge_sens_reg_tbl <- tbl_regression(ttdischarge.sens, exp = TRUE)
# Nicely formatted table
#kable(ttdischarge_sens_reg_tbl, format = "markdown", table.attr = 'class="table"') %>%
#  kable_styling(bootstrap_options = "striped", full_width = FALSE)

# Sens-analysis: Alternative definition/analysis of outcome: time to sustained discharge within 28 days
# Use cause-specific hazards
#survfit2(Surv(discharge_time_sus, discharge_reached_sus) ~ trt, data=df) %>%
#  ggsurvfit() +
#  labs(
#    x = "Days",
#    y = "Overall sustained hospitalization probability"
#  ) +
#  add_confidence_interval() +
#  add_risktable()
# testing: cox ph
ttdischarge.sus <- df %>%
  coxph(Surv(discharge_time_sus, discharge_reached_sus) ~ trt
        + age + clinstatus_baseline
        , data =.)
#ttdischarge_sus_reg_tbl <- tbl_regression(ttdischarge.sus, exp = TRUE)
# Nicely formatted table
#kable(ttdischarge_sus_reg_tbl, format = "markdown", table.attr = 'class="table"') %>%
#  kable_styling(bootstrap_options = "striped", full_width = FALSE)


#####
# Adverse events
table(df$ae_28, df$trt, useNA = "always")
ae.28 <- df %>%
  glm(ae_28 ~ trt
      + age + clinstatus_baseline
      , family = "binomial", data=.)

# (ix) Sens-analysis: Alternative definition/analysis of outcome: incidence rate ratio (Poisson regression) -> AE per person by d28
table(df$ae_28_sev, df$trt, useNA = "always")
ae.28.sev <- df %>%
  glm(ae_28_sev ~ trt
      + age + clinstatus_baseline
      , family = "poisson", data=.)
library(glmmTMB)
ae.28.sev <- df %>%
  glmmTMB(ae_28_sev ~ trt
      + age + clinstatus_baseline
      , family = "nbinom2", data=.)

################
##### INTERACTION/SUBGROUP ANALYSES
table(df$clinstatus_baseline, df$mort_28, useNA = "always")
table(df$vbaseline, df$mort_28, useNA = "always")
df$clinstatus_baseline_n <- as.numeric(df$clinstatus_baseline)

#######
# ventilation
mort.28.vent <- df %>%
  glm(mort_28 ~ trt*clinstatus_baseline_n
      + age
      #+ clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(mort.28.vent))
exp(confint(mort.28.vent))

# mort.28.vent.vb <- df %>%
#   glm(mort_28 ~ trt*vbaseline
#       + age
#       # + clinstatus_baseline
#       , family = "binomial", data=.)
# exp(coef(mort.28.vent.vb))
# exp(confint(mort.28.vent.vb)) # 0 events in one group

mort.28.vent.vb.firth <- df %>%
  logistf(mort_28 ~ trt*vbaseline
          + age
          # + clinstatus_baseline
          , data=.)
summary(mort.28.vent.vb.firth)


# effect by subgroup
mort.28.vent.vb.yes.firth <- df %>%
  filter(vbaseline == 1) %>% # ventilated
  logistf(mort_28 ~ trt
      + age
      # + clinstatus_baseline
      , family = "binomial", data=.)
summary(mort.28.vent.vb.yes.firth)

mort.28.vent.vb.no <- df %>%
  filter(vbaseline == 0) %>% # not ventilated
  glm(mort_28 ~ trt
      + age
      # + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(mort.28.vent.vb.no))
exp(confint(mort.28.vent.vb.no))

mort.28.vent.rs.2 <- df %>%
  filter(clinstatus_baseline == "2") %>% # no oxygen
  glm(mort_28 ~ trt
      + age
      # + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(mort.28.vent.rs.2))
exp(confint(mort.28.vent.rs.2))

mort.28.vent.rs.3 <- df %>%
  filter(clinstatus_baseline == "3") %>% # LF oxygen
  glm(mort_28 ~ trt
      + age
      # + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(mort.28.vent.rs.3))
exp(confint(mort.28.vent.rs.3))

mort.28.vent.rs.4.firth <- df %>%
  filter(clinstatus_baseline == "4") %>% # HF oxygen/NIV
  logistf(mort_28 ~ trt
      + age
      # + clinstatus_baseline
      , family = "binomial", data=.)
summary(mort.28.vent.rs.4.firth)

# mort.28.vent.rs.5 <- df %>%
#   filter(clinstatus_baseline == "5") %>% # ECMO
#   glm(mort_28 ~ trt
#       + age
#       # + clinstatus_baseline
#       , family = "binomial", data=.)

#######
# age

#table(df$age, df$mort_28, useNA = "always")
mort.28.age <- df %>%
  glm(mort_28 ~ trt*age
      #+ age
      + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(mort.28.age))
exp(confint(mort.28.age))

# effect by subgroup
df <- df %>%
  mutate(age_70 = case_when(age < 65 ~ 0,
                            age > 64 ~ 1))
table(df$age_70, useNA = "always")
mort.28.age.a70 <- df %>%
  filter(age_70 == 1) %>% # 70 and above
  glm(mort_28 ~ trt
      # + age
      + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(mort.28.age.a70))

mort.28.age.b70 <- df %>%
  filter(age_70 == 0) %>% # below 70
  glm(mort_28 ~ trt
      # + age
      + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(mort.28.age.b70))
exp(confint(mort.28.age.b70))


#######
# comorbidity
# 4 comorbidity categories as numeric/continuous, i.e., linear interaction
table(df$comorb_cat, df$mort_28, useNA = "always")
# class(df$comorb_cat)
mort.28.comorb <- df %>%
  glm(mort_28 ~ trt*comorb_cat
      + age
      + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(mort.28.comorb))
exp(confint(mort.28.comorb))

# 4 comorbidity categories as factor
df$comorb_cat_f <- as.factor(df$comorb_cat)
# table(df$comorb_cat_f, df$mort_28, useNA = "always")
#mort.28.comorb.f <- df %>%
#  glm(mort_28 ~ trt*comorb_cat_f
#      + age
#      #+ clinstatus_baseline
#      , family = "binomial", data=.)
#summ(mort.28.comorb.f, exp = T, confint = T, model.info = T, model.fit = F, digits = 2)

# full comorbidity count
# table(df$comorb_count, df$mort_28, useNA = "always")
mort.28.comorb.count <- df %>%
  glm(mort_28 ~ trt*comorb_count
      + age
      + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(mort.28.comorb.count))
exp(confint(mort.28.comorb.count))

# any comorbidity
# table(df$comorb_any, df$mort_28, useNA = "always")
mort.28.comorb.any.firth <- df %>%
  logistf(mort_28 ~ trt*comorb_any
      + age
      + clinstatus_baseline_n
      , family = "binomial", data=.)
exp(coef(mort.28.comorb.any.firth))
exp(confint(mort.28.comorb.any.firth))

# effect by subgroup
mort.28.comorb.1.firth <- df %>%
  filter(comorb_cat == 1) %>% # no comorbidity
  logistf(mort_28 ~ trt
      + age
      #+ clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(mort.28.comorb.1.firth))
exp(confint(mort.28.comorb.1.firth))

mort.28.comorb.2 <- df %>%
  filter(comorb_cat == 2) %>% # 1 comorbidity
  glm(mort_28 ~ trt
      + age
      + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(mort.28.comorb.2))
exp(confint(mort.28.comorb.2))

mort.28.comorb.3 <- df %>%
  filter(comorb_cat == 3) %>% # multiple comorbidities
  glm(mort_28 ~ trt
      + age
      + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(mort.28.comorb.3))
exp(confint(mort.28.comorb.3))

# mort.28.comorb.4 <- df %>%
#   filter(comorb_cat == 4) %>% # immunocompromised
#   logistf(mort_28 ~ trt
#           + age
#           + clinstatus_baseline
#           , data=.)



#######
# comedication
# 4 comedication categories as numeric/continuous, i.e., linear interaction // but only 1 and 3 exist in RUXCOVID
table(df$comed_cat, df$trt, useNA = "always")
# 1: patients without Dexamethasone nor Tocilizumab => JAKi effect alone
# 2: patients with Dexamethasone and Tocilizumab => JAKi effect with Dexa + Toci
# 3: patients with Dexamethasone but no Tocilizumab => JAKi effect with Dexa only
# 4: patients with Tocilizumab but no Dexamethasone (if exist) => JAKi effect with Toci only
mort.28.comed <- df %>%
  glm(mort_28 ~ trt*comed_cat
      + age
      + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(mort.28.comed))
exp(confint(mort.28.comed))

# comedication as ordinal factor
df$comed_cat_f <- factor(df$comed_cat, levels = 1:4)
# table(df$comed_cat_f, df$mort_28, useNA = "always")
mort.28.comed.f <- df %>%
  glm(mort_28 ~ trt*comed_cat_f
      + age
      + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(mort.28.comed.f))
exp(confint(mort.28.comed.f))

# effect by subgroup
mort.28.comed.1 <- df %>%
  filter(comed_cat == 1) %>% # without Dexamethasone nor Tocilizumab
  glm(mort_28 ~ trt
      + age
      + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(mort.28.comed.1))
exp(confint(mort.28.comed.1))

mort.28.comed.2 <- df %>%
  filter(comed_cat == 2) %>% # Dexamethasone but no Tocilizumab
  glm(mort_28 ~ trt
      + age
      + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(mort.28.comed.2))
exp(confint(mort.28.comed.2))

#######
# at risk on AEs
#table(df$ae_28, df$at_risk)
ae.28.atrisk <- df %>%
  glm(ae_28 ~ trt*at_risk
      + age
      + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(ae.28.atrisk))
exp(confint(ae.28.atrisk))

# effect by subgroup
ae.28.atrisk.0 <- df %>%
  filter(at_risk == 0) %>% 
  glm(ae_28 ~ trt
      + age
      + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(ae.28.atrisk.0))
exp(confint(ae.28.atrisk.0))

ae.28.atrisk.1 <- df %>%
  filter(at_risk == 1) %>% 
  glm(ae_28 ~ trt
      + age
      + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(ae.28.atrisk.1))
exp(confint(ae.28.atrisk.1))

#######
# comed on AEs
#table(df$comed_cat, df$ae_28)
ae.28.comed <- df %>%
  glm(ae_28 ~ trt*comed_cat
      + age
      + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(ae.28.comed))
exp(confint(ae.28.comed))

# effect by subgroup
ae.28.comed.1 <- df %>%
  filter(comed_cat == 1) %>% 
  glm(ae_28 ~ trt
      + age
      + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(ae.28.comed.1))
exp(confint(ae.28.comed.1))

ae.28.comed.2 <- df %>%
  filter(comed_cat == 2) %>% 
  glm(ae_28 ~ trt
      + age
      + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(ae.28.comed.2))
exp(confint(ae.28.comed.2))

#######
# symptom duration
table(df$sympdur, df$mort_28, useNA = "always")
mort.28.symp <- df %>%
  glm(mort_28 ~ trt*sympdur
      + age
      + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(mort.28.symp))
exp(confint(mort.28.symp))

# effect by subgroup
df <- df %>%
  mutate(sympdur_cat = case_when(sympdur < 6 ~ 2,
                                 sympdur > 5 & sympdur < 11 ~ 1,
                                 sympdur > 10 ~ 0))
#table(df$sympdur_cat, useNA = "always")
#table(df$sympdur, useNA = "always")
mort.28.sympdur.a10 <- df %>%
  filter(sympdur_cat == 0) %>% # more than 10 days
  glm(mort_28 ~ trt
      + age
      + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(mort.28.sympdur.a10))
exp(confint(mort.28.sympdur.a10))

mort.28.sympdur.510.firth <- df %>%
  filter(sympdur_cat == 1) %>% # 5-10 days
  logistf(mort_28 ~ trt
      + age
      + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(mort.28.sympdur.510.firth))
exp(confint(mort.28.sympdur.510.firth))

mort.28.sympdur.b5 <- df %>%
  filter(sympdur_cat == 2) %>% # 5d or less
  glm(mort_28 ~ trt
      + age
      + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(mort.28.sympdur.b5))
exp(confint(mort.28.sympdur.b5))


#######
# CRP
#table(df$crp, df$mort_28, useNA = "always")
mort.28.crp <- df %>%
  glm(mort_28 ~ trt*crp
      + age
      + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(mort.28.crp))
exp(confint(mort.28.crp))

# # truncate outliers > 500
# df <- df %>%
#   mutate(crp_trunc = case_when(crp > 500 ~ 500,
#                                TRUE ~ crp))
# mort.28.crp.trunc <- df %>%
#   glm(mort_28 ~ trt*crp_trunc
#       + age
#       + clinstatus_baseline
#       , family = "binomial", data=.)
# summ(mort.28.crp.trunc, exp = T, confint = T, model.info = T, model.fit = F, digits = 2)

# effect by subgroup
df <- df %>%
  mutate(crp_75 = case_when(crp < 75 ~ 1,
                            crp > 74 ~ 0))
#table(df$crp_75, df$mort_28, useNA = "always")
mort.28.crp.b75.firth <- df %>%
  filter(crp_75 == 1) %>% # below 75
  logistf(mort_28 ~ trt
      + age
      + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(mort.28.crp.b75.firth))
exp(confint(mort.28.crp.b75.firth))

mort.28.crp.a75 <- df %>%
  filter(crp_75 == 0) %>% # 75 and above
  glm(mort_28 ~ trt
      + age
      + clinstatus_baseline
      , family = "binomial", data=.)
exp(coef(mort.28.crp.a75))
exp(confint(mort.28.crp.a75))



#######
# COLLECT TREATMENT EFFECT RESULTS


# Empty data frame to store the results
result_df <- data.frame(
  variable = character(),
  hazard_odds_ratio = numeric(),
  ci_lower = numeric(),
  ci_upper = numeric(),
  standard_error = numeric(),
  p_value = numeric(),
  n_int = numeric(),
  n_cont = numeric(),
  e_int = numeric(),
  e_cont = numeric()
)

# Function to extract treatment results from different model types (glm, clm, coxph and crr)
extract_trt_results <- function(model, variable_name, n_int, n_cont, e_int, e_cont) {
  if (inherits(model, "glm") || inherits(model, "clm")) {
    trt_coef <- coef(model)["trt"]
    hazard_odds_ratio <- exp(trt_coef)
    ci <- exp(confint(model)["trt", ])
    se <- summary(model)$coefficients["trt", "Std. Error"]
    p_value <- summary(model)$coefficients["trt", "Pr(>|z|)"]
  } else if (inherits(model, "coxph")) {
    trt_coef <- coef(model)["trt"]
    hazard_odds_ratio <- exp(trt_coef)
    ci <- exp(confint(model)["trt", ])
    se <- summary(model)$coefficients["trt", "se(coef)"]
    p_value <- summary(model)$coefficients["trt", "Pr(>|z|)"]
  } else if (inherits(model, "tidycrr")) {
    trt_coef <- coef(model)["trt"]
    hazard_odds_ratio <- exp(trt_coef)
    ci <- c(exp(model$tidy$conf.low[1]), exp(model$tidy$conf.high[1]))
    se <- model$tidy$std.error[1]
    p_value <- model$tidy$p.value[1]
  } else if (inherits(model, "summary.margins")) {
    hazard_odds_ratio <- model$AME ### CAVE: this is not an HR or OR, but a marginal RD
    ci <- c(model$lower, model$upper)
    se <- model$SE
    p_value <- model$p
  } else if (inherits(model, "data.frame")) {
    hazard_odds_ratio <- model$estimate[2]
    ci <- c(model$`2.5 %`[2], model$`97.5 %`[2])
    se <- model$std.error[2]
    p_value <- model$p.value[2]
  } else if (inherits(model, "glmmTMB")) {
    trt_coef <- confint(model)["trt", "Estimate"]
    hazard_odds_ratio <- exp(trt_coef)
    ci <- c(exp(confint(model)["trt", "2.5 %"]), exp(confint(model)["trt", "97.5 %"]))
    se <- summary(model)$coefficients$cond["trt", "Std. Error"]
    p_value <- summary(model)$coefficients$cond["trt", "Pr(>|z|)"]
  } else {
    stop("Unsupported model class")
  }
  # capture the results
  result <- data.frame(
    variable = variable_name,
    hazard_odds_ratio = hazard_odds_ratio,
    ci_lower = ci[1],
    ci_upper = ci[2],
    standard_error = se,
    p_value = p_value,
    n_int = n_int,
    n_cont = n_cont,
    e_int = e_int,
    e_cont = e_cont
  )
  return(result)
}

# Loop through
result_list <- list()

result_list[[1]] <- extract_trt_results(mort.28, "death at day 28",
                                        addmargins(table(df$mort_28, df$trt))[3,2], 
                                        addmargins(table(df$mort_28, df$trt))[3,1],
                                        addmargins(table(df$mort_28, df$trt))[2,2], 
                                        addmargins(table(df$mort_28, df$trt))[2,1])
result_list[[2]] <- extract_trt_results(mort.28.dimp, "death at day 28_dimp",
                                        addmargins(table(df$mort_28_dimp, df$trt))[3,2], 
                                        addmargins(table(df$mort_28_dimp, df$trt))[3,1],
                                        addmargins(table(df$mort_28_dimp, df$trt))[2,2], 
                                        addmargins(table(df$mort_28_dimp, df$trt))[2,1]) 
#result_list[[3]] <- extract_trt_results(mort.28.mi, "death at day 28_mi",
#                                        addmargins(table(df$mort_28, df$trt))[3,2], 
#                                        addmargins(table(df$mort_28, df$trt))[3,1],
#                                        addmargins(table(df$mort_28, df$trt))[2,2], 
#                                        addmargins(table(df$mort_28, df$trt))[2,1])
#result_list[[4]] <- extract_trt_results(mort.28.ame, "death at day 28_marginal",
#                                        addmargins(table(df$mort_28, df$trt))[3,2], addmargins(table(df$mort_28, df$trt))[3,1]) # adj: age, clinstatus
result_list[[5]] <- extract_trt_results(mort.60, "death at day 60",
                                        addmargins(table(df$mort_60, df$trt))[3,2], 
                                        addmargins(table(df$mort_60, df$trt))[3,1],
                                        addmargins(table(df$mort_60, df$trt))[2,2], 
                                        addmargins(table(df$mort_60, df$trt))[2,1])
result_list[[6]] <- extract_trt_results(ttdeath, "death within fup",
                                        addmargins(table(df$death_reached, df$trt))[3,2], 
                                        addmargins(table(df$death_reached, df$trt))[3,1],
                                        addmargins(table(df$death_reached, df$trt))[2,2], 
                                        addmargins(table(df$death_reached, df$trt))[2,1]) 
result_list[[7]] <- extract_trt_results(new.mv.28, "new MV within 28d",
                                        addmargins(table(df$new_mv_28, df$trt))[3,2], 
                                        addmargins(table(df$new_mv_28, df$trt))[3,1],
                                        addmargins(table(df$new_mv_28, df$trt))[2,2], 
                                        addmargins(table(df$new_mv_28, df$trt))[2,1]) 
result_list[[8]] <- extract_trt_results(new.mvd.28, "new MV or death within 28d",
                                        addmargins(table(df$new_mvd_28, df$trt))[3,2], 
                                        addmargins(table(df$new_mvd_28, df$trt))[3,1],
                                        addmargins(table(df$new_mvd_28, df$trt))[2,2], 
                                        addmargins(table(df$new_mvd_28, df$trt))[2,1])
result_list[[9]] <- extract_trt_results(clin.28, "clinical status at day 28",
                                        addmargins(table(df$clinstatus_28_imp, df$trt))[7,2], 
                                        addmargins(table(df$clinstatus_28_imp, df$trt))[7,1],
                                        NA,
                                        NA) 
result_list[[10]] <- extract_trt_results(ttdischarge, "discharge within 28 days",
                                         addmargins(table(df$discharge_reached, df$trt))[3,2], 
                                         addmargins(table(df$discharge_reached, df$trt))[3,1],
                                         addmargins(table(df$discharge_reached, df$trt))[2,2],
                                         addmargins(table(df$discharge_reached, df$trt))[2,1])
result_list[[11]] <- extract_trt_results(ttdischarge.comp, "discharge within 28 days, death=comp.event",
                                         addmargins(table(df$discharge_reached, df$trt))[3,2], 
                                         addmargins(table(df$discharge_reached, df$trt))[3,1],
                                         addmargins(table(df$discharge_reached, df$trt))[2,2],
                                         addmargins(table(df$discharge_reached, df$trt))[2,1])
result_list[[12]] <- extract_trt_results(ttdischarge.sens, "discharge within 28 days, death=hypo.event",
                                         addmargins(table(df$discharge_reached, df$trt))[3,2], 
                                         addmargins(table(df$discharge_reached, df$trt))[3,1],
                                         addmargins(table(df$discharge_reached, df$trt))[2,2],
                                         addmargins(table(df$discharge_reached, df$trt))[2,1])
result_list[[13]] <- extract_trt_results(ttdischarge.sus, "sustained discharge within 28 days",
                                         addmargins(table(df$discharge_reached_sus, df$trt))[3,2], 
                                         addmargins(table(df$discharge_reached_sus, df$trt))[3,1],
                                         addmargins(table(df$discharge_reached_sus, df$trt))[2,2], 
                                         addmargins(table(df$discharge_reached_sus, df$trt))[2,1]) 
#result_list[[14]] <- extract_trt_results(vir.clear.5, "viral clearance until day 5",
#                                         addmargins(table(df$vir_clear_5, df$trt))[3,2], addmargins(table(df$vir_clear_5, df$trt))[3,1]) # adj: age, clinstatus
#result_list[[15]] <- extract_trt_results(vir.clear.10, "viral clearance until day 10",
#                                         addmargins(table(df$vir_clear_10, df$trt))[3,2], addmargins(table(df$vir_clear_10, df$trt))[3,1]) # adj: age, clinstatus
#result_list[[16]] <- extract_trt_results(vir.clear.15, "viral clearance until day 15",
#                                         addmargins(table(df$vir_clear_15, df$trt))[3,2], addmargins(table(df$vir_clear_15, df$trt))[3,1]) # adj: age, clinstatus
result_list[[17]] <- extract_trt_results(ae.28, "Any AE grade 3,4 within 28 days",
                                         addmargins(table(df$ae_28, df$trt))[3,2], 
                                         addmargins(table(df$ae_28, df$trt))[3,1],
                                         addmargins(table(df$ae_28, df$trt))[2,2], 
                                         addmargins(table(df$ae_28, df$trt))[2,1]) 
result_list[[18]] <- extract_trt_results(ae.28.sev, "AEs grade 3,4 within 28 days",
                                         addmargins(table(df$ae_28_sev, df$trt))[9,2], 
                                         addmargins(table(df$ae_28_sev, df$trt))[9,1],
                                         NA,
                                         NA)

# Filter out NULL results and bind the results into a single data frame
result_df <- do.call(rbind, Filter(function(x) !is.null(x), result_list))

# Add the trial name and JAKi
result_df$trial <- "RUXCOVID"
result_df$JAKi <- "Ruxolitinib"

# Nicely formatted table
kable(result_df, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)

result_ame <- data.frame(
  variable = "death at day 28_marginal",
  hazard_odds_ratio = adj_mean,
  ci_lower = adj_mean_ci[1],
  ci_upper = adj_mean_ci[2],
  standard_error = adj_mean_se,
  p_value = NA,
  n_int = 287,
  n_cont = 145,
  e_int = 9,
  e_cont = 3,
  trial = "RUXCOVID",
  JAKi = "Ruxolitinib"
)
result_df <- rbind(result_df, result_ame)

# Save
saveRDS(result_df, file = "trt_effects_ruxcovid_06122024.RData")



#######
# COLLECT TREATMENT INTERACTION RESULTS

# Empty data frame to store the results
interaction_df <- data.frame(
  variable = character(),
  log_odds_ratio = numeric(),
  ci_lower = numeric(),
  ci_upper = numeric(),
  standard_error = numeric(),
  p_value = numeric()
)
# Extract and format results for the interaction term
extract_interaction <- function(model, variable_name) {
  if (inherits(model, "glm") || inherits(model, "clm")) {
    trt_coef <- coef(model)[grep("^trt:", names(coef(model)))]
    log_odds_ratio <- exp(trt_coef)
    ci <- exp(confint(model)[grep("^trt:", names(coef(model))), ])
    se <- summary(model)$coefficients[grep("^trt:", names(coef(model))), "Std. Error"]
    p_value <- summary(model)$coefficients[grep("^trt:", names(coef(model))), "Pr(>|z|)"]
  } else if (inherits(model, "logistf")) {
    trt_coef <- coef(model)[grep("^trt:", names(coef(model)))]
    log_odds_ratio <- exp(trt_coef)
    ci <- exp(confint(model)[grep("^trt:", names(coef(model))), ])
    se <- sqrt(diag(vcov(model)))[grep("^trt:", names(coef(model)))]
    p_value <- model$prob[grep("^trt:", names(coef(model)))]
  } else {
    stop("Unsupported model class")
  }
  # capture the results
  result <- data.frame(
    variable = variable_name,
    log_odds_ratio = log_odds_ratio,
    ci_lower = ci[1],
    ci_upper = ci[2],
    standard_error = se,
    p_value = p_value
  )
  return(result)
}
# Loop through
result_list <- list()

result_list[[1]] <- extract_interaction(mort.28.vent, "respiratory support") # adj: age, clinstatus
result_list[[2]] <- extract_interaction(mort.28.vent.vb.firth, "ventilation_firth") # adj: age, clinstatus
result_list[[3]] <- extract_interaction(mort.28.age, "age") # adj: age, clinstatus
result_list[[4]] <- extract_interaction(mort.28.comorb, "comorbidity") # adj: age, clinstatus
result_list[[5]] <- extract_interaction(mort.28.comorb.count, "comorbidity_count") # adj: age, clinstatus
result_list[[6]] <- extract_interaction(mort.28.comorb.any.firth, "comorbidity_any_firth") # adj: age, clinstatus
result_list[[7]] <- extract_interaction(mort.28.comorb, "comorbidity_noimmuno") # adj: age, clinstatus
result_list[[8]] <- extract_interaction(mort.28.comed, "comedication") # adj: age, clinstatus
# result_list[[9]] <- extract_interaction(ae.28.vacc, "vaccination on AEs") # vacc not available
result_list[[10]] <- extract_interaction(mort.28.symp, "symptom duration") # adj: age, clinstatus
result_list[[11]] <- extract_interaction(mort.28.crp, "crp") # adj: age, clinstatus
# result_list[[12]] <- extract_interaction(mort.28.var, "variant") # variant not available
result_list[[13]] <- extract_interaction(ae.28.atrisk, "at risk on AEs")
result_list[[14]] <- extract_interaction(ae.28.comed, "comedication on AEs")

# Filter out NULL results and bind the results into a single data frame
interaction_df <- do.call(rbind, Filter(function(x) !is.null(x), result_list))

# Add the trial name and JAKi
interaction_df$trial <- "RUXCOVID"
interaction_df$JAKi <- "Ruxolitinib"

# Nicely formatted table
kable(interaction_df, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)

# Save
saveRDS(interaction_df, file = "int_effects_ruxcovid_31082024.RData")


#######
# COLLECT SUBGROUP EFFECT RESULTS

# Empty data frame to store the results
subgroup_df <- data.frame(
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

# Function to extract subgroup treatment results
extract_subgroup_results <- function(model, variable_name, n_int, n_int_tot, n_cont, n_cont_tot) {
  if (inherits(model, "glm")) {
    trt_coef <- coef(model)["trt"]
    hazard_odds_ratio <- exp(trt_coef)
    ci <- exp(confint(model)["trt", ])
    se <- summary(model)$coefficients["trt", "Std. Error"]
    p_value <- summary(model)$coefficients["trt", "Pr(>|z|)"]
  } else if (inherits(model, "logistf")) {
    trt_coef <- coef(model)[grep("^trt", names(coef(model)))]
    hazard_odds_ratio <- exp(trt_coef)
    ci <- exp(confint(model)[grep("^trt", names(coef(model))), ])
    se <- sqrt(diag(vcov(model)))[grep("^trt", names(coef(model)))]
    p_value <- model$prob[grep("^trt", names(coef(model)))]
  } else {
    stop("Unsupported model class")
  }
  # capture the results
  result <- data.frame(
    variable = variable_name,
    hazard_odds_ratio = hazard_odds_ratio,
    ci_lower = ci[1],
    ci_upper = ci[2],
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

result_list[[1]] <- extract_subgroup_results(mort.28.vent.vb.yes.firth, "High-flow or non-invasive, mechanical, or ECMO_firth",
                                             addmargins(table(df$vbaseline, df$mort_28, df$trt))[2,2,2],
                                             addmargins(table(df$vbaseline, df$mort_28, df$trt))[2,3,2],
                                             addmargins(table(df$vbaseline, df$mort_28, df$trt))[2,2,1],
                                             addmargins(table(df$vbaseline, df$mort_28, df$trt))[2,3,1])
result_list[[2]] <- extract_subgroup_results(mort.28.vent.vb.no, "None or low-flow oxygen",
                                             addmargins(table(df$vbaseline, df$mort_28, df$trt))[1,2,2],
                                             addmargins(table(df$vbaseline, df$mort_28, df$trt))[1,3,2],
                                             addmargins(table(df$vbaseline, df$mort_28, df$trt))[1,2,1],
                                             addmargins(table(df$vbaseline, df$mort_28, df$trt))[1,3,1])
result_list[[3]] <- extract_subgroup_results(mort.28.vent.rs.2, "No oxygen",
                                             addmargins(table(df$clinstatus_baseline, df$mort_28, df$trt))[2,2,2],
                                             addmargins(table(df$clinstatus_baseline, df$mort_28, df$trt))[2,3,2],
                                             addmargins(table(df$clinstatus_baseline, df$mort_28, df$trt))[2,2,1],
                                             addmargins(table(df$clinstatus_baseline, df$mort_28, df$trt))[2,3,1])
result_list[[4]] <- extract_subgroup_results(mort.28.vent.rs.3, "low-flow oxygen",
                                             addmargins(table(df$clinstatus_baseline, df$mort_28, df$trt))[3,2,2],
                                             addmargins(table(df$clinstatus_baseline, df$mort_28, df$trt))[3,3,2],
                                             addmargins(table(df$clinstatus_baseline, df$mort_28, df$trt))[3,2,1],
                                             addmargins(table(df$clinstatus_baseline, df$mort_28, df$trt))[3,3,1])
result_list[[5]] <- extract_subgroup_results(mort.28.vent.rs.4.firth, "high-flow oxygen / NIV_firth",
                                             addmargins(table(df$clinstatus_baseline, df$mort_28, df$trt))[4,2,2],
                                             addmargins(table(df$clinstatus_baseline, df$mort_28, df$trt))[4,3,2],
                                             addmargins(table(df$clinstatus_baseline, df$mort_28, df$trt))[4,2,1],
                                             addmargins(table(df$clinstatus_baseline, df$mort_28, df$trt))[4,3,1])
#result_list[[6]] <- extract_subgroup_results(mort.28.vent.rs.5, "Mechanical ventilation / ECMO",
#                                             addmargins(table(df$clinstatus_baseline, df$mort_28, df$trt))[5,2,2],
#                                             addmargins(table(df$clinstatus_baseline, df$mort_28, df$trt))[5,3,2],
#                                             addmargins(table(df$clinstatus_baseline, df$mort_28, df$trt))[5,2,1],
#                                             addmargins(table(df$clinstatus_baseline, df$mort_28, df$trt))[5,3,1])
result_list[[7]] <- extract_subgroup_results(mort.28.age.a70, "70 years and above",
                                             addmargins(table(df$age_70, df$mort_28, df$trt))[2,2,2],
                                             addmargins(table(df$age_70, df$mort_28, df$trt))[2,3,2],
                                             addmargins(table(df$age_70, df$mort_28, df$trt))[2,2,1],
                                             addmargins(table(df$age_70, df$mort_28, df$trt))[2,3,1])
result_list[[8]] <- extract_subgroup_results(mort.28.age.b70, "below 70 years",
                                             addmargins(table(df$age_70, df$mort_28, df$trt))[1,2,2],
                                             addmargins(table(df$age_70, df$mort_28, df$trt))[1,3,2],
                                             addmargins(table(df$age_70, df$mort_28, df$trt))[1,2,1],
                                             addmargins(table(df$age_70, df$mort_28, df$trt))[1,3,1])
result_list[[9]] <- extract_subgroup_results(mort.28.comorb.1.firth, "No comorbidity_firth",
                                             addmargins(table(df$comorb_cat_f, df$mort_28, df$trt))[1,2,2],
                                             addmargins(table(df$comorb_cat_f, df$mort_28, df$trt))[1,3,2],
                                             addmargins(table(df$comorb_cat_f, df$mort_28, df$trt))[1,2,1],
                                             addmargins(table(df$comorb_cat_f, df$mort_28, df$trt))[1,3,1])
result_list[[10]] <- extract_subgroup_results(mort.28.comorb.2, "One comorbidity",
                                              addmargins(table(df$comorb_cat_f, df$mort_28, df$trt))[2,2,2],
                                              addmargins(table(df$comorb_cat_f, df$mort_28, df$trt))[2,3,2],
                                              addmargins(table(df$comorb_cat_f, df$mort_28, df$trt))[2,2,1],
                                              addmargins(table(df$comorb_cat_f, df$mort_28, df$trt))[2,3,1])
result_list[[11]] <- extract_subgroup_results(mort.28.comorb.3, "Multiple comorbidities",
                                              addmargins(table(df$comorb_cat_f, df$mort_28, df$trt))[3,2,2],
                                              addmargins(table(df$comorb_cat_f, df$mort_28, df$trt))[3,3,2],
                                              addmargins(table(df$comorb_cat_f, df$mort_28, df$trt))[3,2,1],
                                              addmargins(table(df$comorb_cat_f, df$mort_28, df$trt))[3,3,1])
#result_list[[12]] <- extract_subgroup_results(mort.28.comorb.4.firth, "Immunocompromised_firth",
#                                              addmargins(table(df$comorb_cat_f, df$mort_28, df$trt))[4,2,2],
#                                              addmargins(table(df$comorb_cat_f, df$mort_28, df$trt))[4,3,2],
#                                              addmargins(table(df$comorb_cat_f, df$mort_28, df$trt))[4,2,1],
#                                              addmargins(table(df$comorb_cat_f, df$mort_28, df$trt))[4,3,1])
result_list[[13]] <- extract_subgroup_results(mort.28.comed.1, "No Dexa, no Tocilizumab",
                                              addmargins(table(df$comed_cat, df$mort_28, df$trt))[1,2,2],
                                              addmargins(table(df$comed_cat, df$mort_28, df$trt))[1,3,2],
                                              addmargins(table(df$comed_cat, df$mort_28, df$trt))[1,2,1],
                                              addmargins(table(df$comed_cat, df$mort_28, df$trt))[1,3,1])
result_list[[14]] <- extract_subgroup_results(mort.28.comed.2, "Dexa, but no Tocilizumab",
                                              addmargins(table(df$comed_cat, df$mort_28, df$trt))[2,2,2],
                                              addmargins(table(df$comed_cat, df$mort_28, df$trt))[2,3,2],
                                              addmargins(table(df$comed_cat, df$mort_28, df$trt))[2,2,1],
                                              addmargins(table(df$comed_cat, df$mort_28, df$trt))[2,3,1])
result_list[[15]] <- extract_subgroup_results(mort.28.sympdur.a10, "More than 10 days",
                                              addmargins(table(df$sympdur_cat, df$mort_28, df$trt))[1,2,2],
                                              addmargins(table(df$sympdur_cat, df$mort_28, df$trt))[1,3,2],
                                              addmargins(table(df$sympdur_cat, df$mort_28, df$trt))[1,2,1],
                                              addmargins(table(df$sympdur_cat, df$mort_28, df$trt))[1,3,1])
result_list[[16]] <- extract_subgroup_results(mort.28.sympdur.510.firth, "Between 5-10 days_firth",
                                              addmargins(table(df$sympdur_cat, df$mort_28, df$trt))[2,2,2],
                                              addmargins(table(df$sympdur_cat, df$mort_28, df$trt))[2,3,2],
                                              addmargins(table(df$sympdur_cat, df$mort_28, df$trt))[2,2,1],
                                              addmargins(table(df$sympdur_cat, df$mort_28, df$trt))[2,3,1])
result_list[[17]] <- extract_subgroup_results(mort.28.sympdur.b5, "5 days and less",
                                              addmargins(table(df$sympdur_cat, df$mort_28, df$trt))[3,2,2],
                                              addmargins(table(df$sympdur_cat, df$mort_28, df$trt))[3,3,2],
                                              addmargins(table(df$sympdur_cat, df$mort_28, df$trt))[3,2,1],
                                              addmargins(table(df$sympdur_cat, df$mort_28, df$trt))[3,3,1])
result_list[[18]] <- extract_subgroup_results(mort.28.crp.a75, "CRP 75 and higher",
                                              addmargins(table(df$crp_75, df$mort_28, df$trt))[1,2,2],
                                              addmargins(table(df$crp_75, df$mort_28, df$trt))[1,3,2],
                                              addmargins(table(df$crp_75, df$mort_28, df$trt))[1,2,1],
                                              addmargins(table(df$crp_75, df$mort_28, df$trt))[1,3,1])
result_list[[19]] <- extract_subgroup_results(mort.28.crp.b75.firth, "CRP below 75_firth",
                                              addmargins(table(df$crp_75, df$mort_28, df$trt))[2,2,2],
                                              addmargins(table(df$crp_75, df$mort_28, df$trt))[2,3,2],
                                              addmargins(table(df$crp_75, df$mort_28, df$trt))[2,2,1],
                                              addmargins(table(df$crp_75, df$mort_28, df$trt))[2,3,1])
result_list[[20]] <- extract_subgroup_results(ae.28.atrisk.0, "Not at risk",
                                              addmargins(table(df$at_risk, df$ae_28, df$trt))[1,2,2],
                                              addmargins(table(df$at_risk, df$ae_28, df$trt))[1,3,2],
                                              addmargins(table(df$at_risk, df$ae_28, df$trt))[1,2,1],
                                              addmargins(table(df$at_risk, df$ae_28, df$trt))[1,3,1])
result_list[[21]] <- extract_subgroup_results(ae.28.atrisk.1, "At risk",
                                              addmargins(table(df$at_risk, df$ae_28, df$trt))[2,2,2],
                                              addmargins(table(df$at_risk, df$ae_28, df$trt))[2,3,2],
                                              addmargins(table(df$at_risk, df$ae_28, df$trt))[2,2,1],
                                              addmargins(table(df$at_risk, df$ae_28, df$trt))[2,3,1])
result_list[[22]] <- extract_subgroup_results(ae.28.comed.1, "No Dexa, no Tocilizumab_AE",
                                              addmargins(table(df$comed_cat, df$ae_28, df$trt))[1,2,2],
                                              addmargins(table(df$comed_cat, df$ae_28, df$trt))[1,3,2],
                                              addmargins(table(df$comed_cat, df$ae_28, df$trt))[1,2,1],
                                              addmargins(table(df$comed_cat, df$ae_28, df$trt))[1,3,1])
result_list[[23]] <- extract_subgroup_results(ae.28.comed.2, "Dexa, but no Tocilizumab_AE",
                                              addmargins(table(df$comed_cat, df$ae_28, df$trt))[2,2,2],
                                              addmargins(table(df$comed_cat, df$ae_28, df$trt))[2,3,2],
                                              addmargins(table(df$comed_cat, df$ae_28, df$trt))[2,2,1],
                                              addmargins(table(df$comed_cat, df$ae_28, df$trt))[2,3,1])

# Filter out NULL results and bind the results into a single data frame
subgroup_df <- do.call(rbind, Filter(function(x) !is.null(x), result_list))

# Add the trial name and JAKi
subgroup_df$trial <- "RUXCOVID"
subgroup_df$JAKi <- "Ruxolitinib"

# Nicely formatted table
kable(subgroup_df, format = "markdown", table.attr = 'class="table"') %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)

# Save
saveRDS(subgroup_df, file = "subgroup_effects_ruxcovid_31082024.RData")

