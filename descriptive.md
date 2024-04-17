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

# Load entire dataset (from one-stage.Rmd)
## Make sure to run one-stage before

```r
df_tot <- readRDS("df_tot.RData") # without Murugesan
# df_tot_Muru <- readRDS("df_tot_Muru.RData") # with Murugesan
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
# unique(df_baseline$Sex)
df_baseline <- df_baseline %>%
  mutate(Sex = case_when(Sex == "male" | Sex == "M" | Sex == "2" ~ "Male",
                         Sex == "female" | Sex == "F" | Sex == "1" ~ "Female"))

# Ethnicity
# table(df_baseline$Ethnicity, useNA = "always")
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
                             Country == "GBR" ~ "United Kingdom",
                             Country == "PORTUGAL" ~ "Portugal",
                             Country == "MEX" ~ "Mexico",
                             Country == "ARG" ~ "Argentina",
                             Country == "BRA" ~ "Brasil",
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
df_baseline <- df_baseline %>%
  mutate(Vaccination = case_when(Vaccination == "1" ~ "Any SARS CoV-2 vaccine",
                                Vaccination == "0" ~ "No SARS CoV-2 vaccine"))
# ICU
df_baseline <- df_baseline %>%
  mutate(`Patients admitted to intensive care unit` = case_when(`Patients admitted to intensive care unit` == "1" ~ "Yes",
                                                                `Patients admitted to intensive care unit` == "0" ~ "No"))
# Comorbidities
df_baseline <- df_baseline %>%
  mutate(Comorbidities = case_when(Comorbidities == "1" ~ "No comorbidity",
                                   Comorbidities == "2" ~ "One comorbidity",
                                   Comorbidities == "3" ~ "Multiple comorbidities",
                                   Comorbidities == "4" ~ "Immunocompromised"))
df_baseline$Comorbidities <- ordered(df_baseline$Comorbidities,
                                  levels = c("No comorbidity", "One comorbidity",
                                             "Multiple comorbidities", "Immunocompromised"))
# Dexamethasone and Tocilizumab
df_baseline <- df_baseline %>%
  mutate(`Dexamethasone and Tocilizumab` = case_when(`Dexamethasone and Tocilizumab` == "1" ~ "No Dexamethasone, no Tocilizumab",
                                   `Dexamethasone and Tocilizumab` == "2" ~ "Dexamethasone and Tocilizumab",
                                   `Dexamethasone and Tocilizumab` == "3" ~ "Dexamethasone but no Tocilizumab",
                                  `Dexamethasone and Tocilizumab` == "4" ~ "Tocolizumab but no Dexamethasone"))
df_baseline$`Dexamethasone and Tocilizumab` <- ordered(df_baseline$`Dexamethasone and Tocilizumab`,
                                  levels = c("No Dexamethasone, no Tocilizumab", "Dexamethasone but no Tocilizumab",
                                             "Dexamethasone and Tocilizumab", "Tocolizumab but no Dexamethasone"))

# Clinstatus_baseline
df_baseline <- df_baseline %>%
  mutate(`Clinical status on ordinal scale` = case_when(`Clinical status on ordinal scale` == "2" ~ "2: Hospitalised without need for oxygen therapy (WHO score 4)",
                                   `Clinical status on ordinal scale` == "3" ~ "3: Hospitalised with need for supplemental low-flow oxygen (WHO score 5)",
                                   `Clinical status on ordinal scale` == "4" ~ "4: Hospitalised with need for high-flow oxygen or non- invasive ventilation (WHO score 6)",
                                  `Clinical status on ordinal scale` == "5" ~ "5: Hospitalised with need for mechanical ventilation or ECMO (WHO score 7–9)"))

df_baseline$`Clinical status on ordinal scale` <- ordered(df_baseline$`Clinical status on ordinal scale`,
                                  levels = c("2: Hospitalised without need for oxygen therapy (WHO score 4)", "3: Hospitalised with need for supplemental low-flow oxygen (WHO score 5)", "4: Hospitalised with need for high-flow oxygen or non- invasive ventilation (WHO score 6)", "5: Hospitalised with need for mechanical ventilation or ECMO (WHO score 7–9)"))

# Remdesivir
df_baseline <- df_baseline %>%
  mutate(Remdesivir = case_when(Remdesivir == "1" ~ "Remdesivir",
                                Remdesivir == "0" ~ "No Remdesivir"))
# Seroconversion
df_baseline <- df_baseline %>%
  mutate(`Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N])` = case_when(`Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N])` == "1" ~ "Seroconverted",
                                `Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N])` == "0" ~ "Not seroconverted"))
# Viremia
df_baseline <- df_baseline %>%
  mutate(`Patients with undetectable viral load` = case_when(`Patients with undetectable viral load` == "1" ~ "Undetectable viral load",
                                `Patients with undetectable viral load` == "0" ~ "Detectable viral load"))

# Replace the strata variable labels
df_baseline$ARM <- ifelse(df_baseline$ARM == 0, "No JAK inhibitor", "JAK inhibitor")
```

# Main Baseline table, by treatment arm

```r
vars.list_main <- c("ARM","Age, years", "Sex", "Vaccination", "Time from symptom onset to randomisation, days", "Clinical status on ordinal scale", "Comorbidities", "Dexamethasone and Tocilizumab", "Remdesivir", "C-reactive protein concentration, mg/L", "Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N])", "Patients with undetectable viral load")
table_baseline <- CreateTableOne(data = df_baseline, vars = vars.list_main[!vars.list_main %in% c("ARM")], strata = "ARM", includeNA = F, test = F, addOverall = TRUE)
capture.output(table_baseline <- print(table_baseline, nonnormal = vars.list_main,catDigits = 1,SMD = TRUE,showAllLevels = TRUE,test = TRUE,printToggle = FALSE,missing = TRUE))
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
|n                                                                                             |                                                                                          |11683                 |5907                  |5776                  |        |
|Age, years (median [IQR])                                                                     |                                                                                          |58.00 [47.00, 68.00]  |58.00 [47.00, 69.00]  |58.00 [47.00, 68.00]  |0.0     |
|Sex (%)                                                                                       |Female                                                                                    |4167 (35.7)           |2097 (35.5)           |2070 (35.8)           |0.0     |
|                                                                                              |Male                                                                                      |7516 (64.3)           |3810 (64.5)           |3706 (64.2)           |        |
|Vaccination (%)                                                                               |Any SARS CoV-2 vaccine                                                                    |3527 (36.5)           |1809 (36.9)           |1718 (36.0)           |17.2    |
|                                                                                              |No SARS CoV-2 vaccine                                                                     |6146 (63.5)           |3097 (63.1)           |3049 (64.0)           |        |
|Time from symptom onset to randomisation, days (median [IQR])                                 |                                                                                          |10.00 [7.00, 12.00]   |10.00 [7.00, 12.00]   |10.00 [7.00, 12.00]   |0.2     |
|Clinical status on ordinal scale (%)                                                          |2: Hospitalised without need for oxygen therapy (WHO score 4)                             |866 ( 7.4)            |415 ( 7.0)            |451 ( 7.8)            |0.1     |
|                                                                                              |3: Hospitalised with need for supplemental low-flow oxygen (WHO score 5)                  |7421 (63.6)           |3743 (63.4)           |3678 (63.7)           |        |
|                                                                                              |4: Hospitalised with need for high-flow oxygen or non- invasive ventilation (WHO score 6) |2895 (24.8)           |1497 (25.4)           |1398 (24.2)           |        |
|                                                                                              |5: Hospitalised with need for mechanical ventilation or ECMO (WHO score 7–9)              |494 ( 4.2)            |250 ( 4.2)            |244 ( 4.2)            |        |
|Comorbidities (%)                                                                             |No comorbidity                                                                            |5151 (44.2)           |2573 (43.6)           |2578 (44.7)           |0.2     |
|                                                                                              |One comorbidity                                                                           |3548 (30.4)           |1801 (30.5)           |1747 (30.3)           |        |
|                                                                                              |Multiple comorbidities                                                                    |2868 (24.6)           |1475 (25.0)           |1393 (24.2)           |        |
|                                                                                              |Immunocompromised                                                                         |98 ( 0.8)             |50 ( 0.8)             |48 ( 0.8)             |        |
|Dexamethasone and Tocilizumab (%)                                                             |No Dexamethasone, no Tocilizumab                                                          |1823 (15.6)           |896 (15.2)            |927 (16.1)            |0.1     |
|                                                                                              |Dexamethasone but no Tocilizumab                                                          |7216 (61.8)           |3663 (62.0)           |3553 (61.6)           |        |
|                                                                                              |Dexamethasone and Tocilizumab                                                             |2603 (22.3)           |1328 (22.5)           |1275 (22.1)           |        |
|                                                                                              |Tocolizumab but no Dexamethasone                                                          |34 ( 0.3)             |18 ( 0.3)             |16 ( 0.3)             |        |
|Remdesivir (%)                                                                                |No Remdesivir                                                                             |8892 (76.1)           |4216 (71.4)           |4676 (81.0)           |0.0     |
|                                                                                              |Remdesivir                                                                                |2791 (23.9)           |1691 (28.6)           |1100 (19.0)           |        |
|C-reactive protein concentration, mg/L (median [IQR])                                         |                                                                                          |85.00 [42.00, 147.00] |84.00 [42.00, 148.00] |86.00 [42.00, 146.00] |4.1     |
|Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N]) (%) |Not seroconverted                                                                         |594 (33.9)            |285 (33.5)            |309 (34.4)            |85.0    |
|                                                                                              |Seroconverted                                                                             |1157 (66.1)           |567 (66.5)            |590 (65.6)            |        |
|Patients with undetectable viral load (%)                                                     |Detectable viral load                                                                     |9594 (97.3)           |4839 (97.4)           |4755 (97.2)           |15.6    |
|                                                                                              |Undetectable viral load                                                                   |265 ( 2.7)            |129 ( 2.6)            |136 ( 2.8)            |        |

# Appendix Baseline table, by treatment arm

```r
vars.list_appendix <- c("ARM", "Ethnicity", "Country", "Patients admitted to intensive care unit")
table_baseline <- CreateTableOne(data = df_baseline, vars = vars.list_appendix[!vars.list_appendix %in% c("ARM")], strata = "ARM", includeNA = F, test = F, addOverall = TRUE)
capture.output(table_baseline <- print(table_baseline, nonnormal = vars.list_appendix,catDigits = 1,SMD = TRUE,showAllLevels = TRUE,test = TRUE,printToggle = FALSE,missing = TRUE))
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
|n                                            |                                          |11683       |5907          |5776             |        |
|Ethnicity (%)                                |American Indian or Alaska Native          |358 ( 3.4)  |165 ( 3.1)    |193 ( 3.7)       |10.2    |
|                                             |Asian                                     |796 ( 7.6)  |383 ( 7.2)    |413 ( 8.0)       |        |
|                                             |Black or African American                 |484 ( 4.6)  |246 ( 4.6)    |238 ( 4.6)       |        |
|                                             |Caucasian                                 |8395 (80.0) |4276 (80.7)   |4119 (79.3)      |        |
|                                             |Hispanic or Latino                        |265 ( 2.5)  |137 ( 2.6)    |128 ( 2.5)       |        |
|                                             |Mixed                                     |80 ( 0.8)   |37 ( 0.7)     |43 ( 0.8)        |        |
|                                             |Native Hawaiian or other Pacific Islander |16 ( 0.2)   |7 ( 0.1)      |9 ( 0.2)         |        |
|                                             |Persian/Mazani                            |97 ( 0.9)   |46 ( 0.9)     |51 ( 1.0)        |        |
|Country (%)                                  |Argentina                                 |229 ( 2.0)  |119 ( 2.1)    |110 ( 2.0)       |2.4     |
|                                             |Asia*                                     |67 ( 0.6)   |33 ( 0.6)     |34 ( 0.6)        |        |
|                                             |Austria                                   |6 ( 0.1)    |2 ( 0.0)      |4 ( 0.1)         |        |
|                                             |Belgium                                   |10 ( 0.1)   |4 ( 0.1)      |6 ( 0.1)         |        |
|                                             |Brasil                                    |366 ( 3.2)  |187 ( 3.2)    |179 ( 3.2)       |        |
|                                             |Europe*                                   |13 ( 0.1)   |6 ( 0.1)      |7 ( 0.1)         |        |
|                                             |France                                    |94 ( 0.8)   |50 ( 0.9)     |44 ( 0.8)        |        |
|                                             |Germany                                   |21 ( 0.2)   |10 ( 0.2)     |11 ( 0.2)        |        |
|                                             |India                                     |50 ( 0.4)   |19 ( 0.3)     |31 ( 0.6)        |        |
|                                             |Iran                                      |97 ( 0.9)   |46 ( 0.8)     |51 ( 0.9)        |        |
|                                             |Ireland                                   |9 ( 0.1)    |5 ( 0.1)      |4 ( 0.1)         |        |
|                                             |Italy                                     |166 ( 1.5)  |86 ( 1.5)     |80 ( 1.4)        |        |
|                                             |Japan                                     |38 ( 0.3)   |19 ( 0.3)     |19 ( 0.3)        |        |
|                                             |Luxembourg                                |1 ( 0.0)    |1 ( 0.0)      |0 ( 0.0)         |        |
|                                             |Mexico                                    |312 ( 2.7)  |152 ( 2.6)    |160 ( 2.8)       |        |
|                                             |North America*                            |953 ( 8.4)  |476 ( 8.2)    |477 ( 8.5)       |        |
|                                             |Norway                                    |127 ( 1.1)  |61 ( 1.1)     |66 ( 1.2)        |        |
|                                             |Portugal                                  |3 ( 0.0)    |2 ( 0.0)      |1 ( 0.0)         |        |
|                                             |Russia                                    |112 ( 1.0)  |58 ( 1.0)     |54 ( 1.0)        |        |
|                                             |South Korea                               |36 ( 0.3)   |16 ( 0.3)     |20 ( 0.4)        |        |
|                                             |Spain                                     |210 ( 1.8)  |106 ( 1.8)    |104 ( 1.8)       |        |
|                                             |United Kingdom                            |8141 (71.4) |4140 (71.8)   |4001 (71.1)      |        |
|                                             |USA                                       |340 ( 3.0)  |172 ( 3.0)    |168 ( 3.0)       |        |
|Patients admitted to intensive care unit (%) |No                                        |2278 (90.4) |1129 (89.9)   |1149 (90.9)      |78.4    |
|                                             |Yes                                       |242 ( 9.6)  |127 (10.1)    |115 ( 9.1)       |        |


# All Baseline Characteristics, by trial

```r
# all participants, by trial
table_baseline_trial <- CreateTableOne(data = df_baseline, vars = vars.list[!vars.list %in% c("Trial", "ARM")], strata = "Trial", includeNA = F, test = F, addOverall = TRUE)
capture.output(table_baseline_trial <- print(table_baseline_trial, nonnormal = vars.list,catDigits = 1,SMD = TRUE,showAllLevels = TRUE,test = TRUE,printToggle = FALSE,missing = TRUE))
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

|                                                                                              |level                                                                                     |Overall               |ACTT2                  |Bari-Solidact         |COV-BARRIER           |COVINIB               |Ghazaeian            |RECOVERY              |TACTIC-R               |TOFACOV              |Missing |
|:---------------------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------|:---------------------|:----------------------|:---------------------|:---------------------|:---------------------|:--------------------|:---------------------|:----------------------|:--------------------|:-------|
|n                                                                                             |                                                                                          |11683                 |1033                   |289                   |1626                  |110                   |97                   |8130                  |282                    |116                  |        |
|Age, years (median [IQR])                                                                     |                                                                                          |58.00 [47.00, 68.00]  |56.00 [43.00, 67.00]   |60.00 [50.00, 69.00]  |58.00 [48.00, 68.00]  |55.00 [47.25, 62.00]  |51.00 [37.00, 64.00] |58.00 [47.00, 69.00]  |60.00 [52.00, 69.00]   |58.00 [50.75, 66.25] |0.0     |
|Sex (%)                                                                                       |Female                                                                                    |4167 (35.7)           |381 ( 36.9)            |218 (75.4)            |608 (37.4)            |34 ( 30.9)            |50 ( 51.5)           |2764 ( 34.0)          |76 (27.0)              |36 ( 31.0)           |0.0     |
|                                                                                              |Male                                                                                      |7516 (64.3)           |652 ( 63.1)            |71 (24.6)             |1018 (62.6)           |76 ( 69.1)            |47 ( 48.5)           |5366 ( 66.0)          |206 (73.0)             |80 ( 69.0)           |        |
|Ethnicity (%)                                                                                 |American Indian or Alaska Native                                                          |358 ( 3.4)            |10 (  1.0)             |0 ( NaN)              |348 (21.9)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 ( 0.0)               |0 (  0.0)            |10.2    |
|                                                                                              |Asian                                                                                     |796 ( 7.6)            |101 (  9.9)            |0 ( NaN)              |175 (11.0)            |1 (  0.9)             |0 (  0.0)            |487 (  6.7)           |32 (12.2)              |0 (  0.0)            |        |
|                                                                                              |Black or African American                                                                 |484 ( 4.6)            |156 ( 15.3)            |0 ( NaN)              |77 ( 4.8)             |0 (  0.0)             |0 (  0.0)            |220 (  3.0)           |31 (11.8)              |0 (  0.0)            |        |
|                                                                                              |Caucasian                                                                                 |8395 (80.0)           |496 ( 48.6)            |0 ( NaN)              |982 (61.7)            |90 ( 81.8)            |0 (  0.0)            |6512 ( 89.3)          |199 (75.7)             |116 (100.0)          |        |
|                                                                                              |Hispanic or Latino                                                                        |265 ( 2.5)            |246 ( 24.1)            |0 ( NaN)              |0 ( 0.0)              |19 ( 17.3)            |0 (  0.0)            |0 (  0.0)             |0 ( 0.0)               |0 (  0.0)            |        |
|                                                                                              |Mixed                                                                                     |80 ( 0.8)             |0 (  0.0)              |0 ( NaN)              |5 ( 0.3)              |0 (  0.0)             |0 (  0.0)            |74 (  1.0)            |1 ( 0.4)               |0 (  0.0)            |        |
|                                                                                              |Native Hawaiian or other Pacific Islander                                                 |16 ( 0.2)             |11 (  1.1)             |0 ( NaN)              |5 ( 0.3)              |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 ( 0.0)               |0 (  0.0)            |        |
|                                                                                              |Persian/Mazani                                                                            |97 ( 0.9)             |0 (  0.0)              |0 ( NaN)              |0 ( 0.0)              |0 (  0.0)             |97 (100.0)           |0 (  0.0)             |0 ( 0.0)               |0 (  0.0)            |        |
|Country (%)                                                                                   |Argentina                                                                                 |229 ( 2.0)            |0 (  0.0)              |0 ( 0.0)              |229 (14.1)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 ( NaN)               |0 (  0.0)            |2.4     |
|                                                                                              |Asia*                                                                                     |67 ( 0.6)             |67 (  6.5)             |0 ( 0.0)              |0 ( 0.0)              |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 ( NaN)               |0 (  0.0)            |        |
|                                                                                              |Austria                                                                                   |6 ( 0.1)              |0 (  0.0)              |6 ( 2.1)              |0 ( 0.0)              |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 ( NaN)               |0 (  0.0)            |        |
|                                                                                              |Belgium                                                                                   |10 ( 0.1)             |0 (  0.0)              |10 ( 3.5)             |0 ( 0.0)              |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 ( NaN)               |0 (  0.0)            |        |
|                                                                                              |Brasil                                                                                    |366 ( 3.2)            |0 (  0.0)              |0 ( 0.0)              |366 (22.5)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 ( NaN)               |0 (  0.0)            |        |
|                                                                                              |Europe*                                                                                   |13 ( 0.1)             |13 (  1.3)             |0 ( 0.0)              |0 ( 0.0)              |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 ( NaN)               |0 (  0.0)            |        |
|                                                                                              |France                                                                                    |94 ( 0.8)             |0 (  0.0)              |94 (32.5)             |0 ( 0.0)              |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 ( NaN)               |0 (  0.0)            |        |
|                                                                                              |Germany                                                                                   |21 ( 0.2)             |0 (  0.0)              |1 ( 0.3)              |20 ( 1.2)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 ( NaN)               |0 (  0.0)            |        |
|                                                                                              |India                                                                                     |50 ( 0.4)             |0 (  0.0)              |0 ( 0.0)              |50 ( 3.1)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 ( NaN)               |0 (  0.0)            |        |
|                                                                                              |Iran                                                                                      |97 ( 0.9)             |0 (  0.0)              |0 ( 0.0)              |0 ( 0.0)              |0 (  0.0)             |97 (100.0)           |0 (  0.0)             |0 ( NaN)               |0 (  0.0)            |        |
|                                                                                              |Ireland                                                                                   |9 ( 0.1)              |0 (  0.0)              |9 ( 3.1)              |0 ( 0.0)              |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 ( NaN)               |0 (  0.0)            |        |
|                                                                                              |Italy                                                                                     |166 ( 1.5)            |0 (  0.0)              |25 ( 8.7)             |25 ( 1.5)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 ( NaN)               |116 (100.0)          |        |
|                                                                                              |Japan                                                                                     |38 ( 0.3)             |0 (  0.0)              |0 ( 0.0)              |38 ( 2.3)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 ( NaN)               |0 (  0.0)            |        |
|                                                                                              |Luxembourg                                                                                |1 ( 0.0)              |0 (  0.0)              |1 ( 0.3)              |0 ( 0.0)              |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 ( NaN)               |0 (  0.0)            |        |
|                                                                                              |Mexico                                                                                    |312 ( 2.7)            |0 (  0.0)              |0 ( 0.0)              |312 (19.2)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 ( NaN)               |0 (  0.0)            |        |
|                                                                                              |North America*                                                                            |953 ( 8.4)            |953 ( 92.3)            |0 ( 0.0)              |0 ( 0.0)              |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 ( NaN)               |0 (  0.0)            |        |
|                                                                                              |Norway                                                                                    |127 ( 1.1)            |0 (  0.0)              |127 (43.9)            |0 ( 0.0)              |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 ( NaN)               |0 (  0.0)            |        |
|                                                                                              |Portugal                                                                                  |3 ( 0.0)              |0 (  0.0)              |3 ( 1.0)              |0 ( 0.0)              |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 ( NaN)               |0 (  0.0)            |        |
|                                                                                              |Russia                                                                                    |112 ( 1.0)            |0 (  0.0)              |0 ( 0.0)              |112 ( 6.9)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 ( NaN)               |0 (  0.0)            |        |
|                                                                                              |South Korea                                                                               |36 ( 0.3)             |0 (  0.0)              |0 ( 0.0)              |36 ( 2.2)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 ( NaN)               |0 (  0.0)            |        |
|                                                                                              |Spain                                                                                     |210 ( 1.8)            |0 (  0.0)              |13 ( 4.5)             |87 ( 5.4)             |110 (100.0)           |0 (  0.0)            |0 (  0.0)             |0 ( NaN)               |0 (  0.0)            |        |
|                                                                                              |United Kingdom                                                                            |8141 (71.4)           |0 (  0.0)              |0 ( 0.0)              |11 ( 0.7)             |0 (  0.0)             |0 (  0.0)            |8130 (100.0)          |0 ( NaN)               |0 (  0.0)            |        |
|                                                                                              |USA                                                                                       |340 ( 3.0)            |0 (  0.0)              |0 ( 0.0)              |340 (20.9)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 ( NaN)               |0 (  0.0)            |        |
|Vaccination (%)                                                                               |Any SARS CoV-2 vaccine                                                                    |3527 (36.5)           |0 (  0.0)              |102 (35.9)            |0 ( NaN)              |2 (  1.8)             |0 (  NaN)            |3420 ( 42.1)          |0 ( NaN)               |3 (  2.6)            |17.2    |
|                                                                                              |No SARS CoV-2 vaccine                                                                     |6146 (63.5)           |1033 (100.0)           |182 (64.1)            |0 ( NaN)              |108 ( 98.2)           |0 (  NaN)            |4710 ( 57.9)          |0 ( NaN)               |113 ( 97.4)          |        |
|Time from symptom onset to randomisation, days (median [IQR])                                 |                                                                                          |10.00 [7.00, 12.00]   |8.00 [5.00, 10.00]     |9.00 [7.00, 11.00]    |10.00 [8.00, 13.00]   |7.00 [5.00, 9.00]     |7.00 [6.00, 10.00]   |10.00 [7.00, 12.00]   |10.00 [7.00, 13.00]    |8.00 [6.00, 10.00]   |0.2     |
|Patients admitted to intensive care unit (%)                                                  |No                                                                                        |2278 (90.4)           |0 (  NaN)              |160 (55.4)            |1525 (93.8)           |110 (100.0)           |97 (100.0)           |0 (  NaN)             |270 (95.7)             |116 (100.0)          |78.4    |
|                                                                                              |Yes                                                                                       |242 ( 9.6)            |0 (  NaN)              |129 (44.6)            |101 ( 6.2)            |0 (  0.0)             |0 (  0.0)            |0 (  NaN)             |12 ( 4.3)              |0 (  0.0)            |        |
|Clinical status on ordinal scale (%)                                                          |2: Hospitalised without need for oxygen therapy (WHO score 4)                             |866 ( 7.4)            |142 ( 13.7)            |0 ( 0.0)              |186 (11.5)            |35 ( 31.8)            |2 (  2.1)            |465 (  5.7)           |11 ( 3.9)              |25 ( 21.6)           |0.1     |
|                                                                                              |3: Hospitalised with need for supplemental low-flow oxygen (WHO score 5)                  |7421 (63.6)           |564 ( 54.6)            |0 ( 0.0)              |962 (59.4)            |75 ( 68.2)            |95 ( 97.9)           |5504 ( 67.7)          |130 (46.1)             |91 ( 78.4)           |        |
|                                                                                              |4: Hospitalised with need for high-flow oxygen or non- invasive ventilation (WHO score 6) |2895 (24.8)           |216 ( 20.9)            |249 (86.2)            |370 (22.9)            |0 (  0.0)             |0 (  0.0)            |1921 ( 23.6)          |139 (49.3)             |0 (  0.0)            |        |
|                                                                                              |5: Hospitalised with need for mechanical ventilation or ECMO (WHO score 7–9)              |494 ( 4.2)            |111 ( 10.7)            |40 (13.8)             |101 ( 6.2)            |0 (  0.0)             |0 (  0.0)            |240 (  3.0)           |2 ( 0.7)               |0 (  0.0)            |        |
|Comorbidities (%)                                                                             |No comorbidity                                                                            |5151 (44.2)           |151 ( 14.9)            |106 (36.7)            |370 (22.8)            |37 ( 33.6)            |41 ( 42.3)           |4353 ( 53.5)          |49 (17.4)              |44 ( 37.9)           |0.2     |
|                                                                                              |One comorbidity                                                                           |3548 (30.4)           |288 ( 28.4)            |73 (25.3)             |477 (29.3)            |40 ( 36.4)            |22 ( 22.7)           |2538 ( 31.2)          |64 (22.7)              |46 ( 39.7)           |        |
|                                                                                              |Multiple comorbidities                                                                    |2868 (24.6)           |546 ( 53.8)            |99 (34.3)             |761 (46.8)            |31 ( 28.2)            |32 ( 33.0)           |1239 ( 15.2)          |134 (47.5)             |26 ( 22.4)           |        |
|                                                                                              |Immunocompromised                                                                         |98 ( 0.8)             |30 (  3.0)             |11 ( 3.8)             |18 ( 1.1)             |2 (  1.8)             |2 (  2.1)            |0 (  0.0)             |35 (12.4)              |0 (  0.0)            |        |
|Dexamethasone and Tocilizumab (%)                                                             |No Dexamethasone, no Tocilizumab                                                          |1823 (15.6)           |991 ( 95.9)            |18 ( 6.2)             |328 (20.3)            |94 ( 85.5)            |0 (  0.0)            |349 (  4.3)           |36 (12.8)              |7 (  6.0)            |0.1     |
|                                                                                              |Dexamethasone but no Tocilizumab                                                          |7216 (61.8)           |42 (  4.1)             |270 (93.4)            |1291 (79.7)           |16 ( 14.5)            |97 (100.0)           |5157 ( 63.4)          |234 (83.0)             |109 ( 94.0)          |        |
|                                                                                              |Dexamethasone and Tocilizumab                                                             |2603 (22.3)           |0 (  0.0)              |1 ( 0.3)              |0 ( 0.0)              |0 (  0.0)             |0 (  0.0)            |2590 ( 31.9)          |12 ( 4.3)              |0 (  0.0)            |        |
|                                                                                              |Tocolizumab but no Dexamethasone                                                          |34 ( 0.3)             |0 (  0.0)              |0 ( 0.0)              |0 ( 0.0)              |0 (  0.0)             |0 (  0.0)            |34 (  0.4)            |0 ( 0.0)               |0 (  0.0)            |        |
|Remdesivir (%)                                                                                |No Remdesivir                                                                             |8892 (76.1)           |518 ( 50.1)            |281 (97.2)            |1337 (82.2)           |110 (100.0)           |0 (  0.0)            |6474 ( 79.6)          |159 (56.4)             |13 ( 11.2)           |0.0     |
|                                                                                              |Remdesivir                                                                                |2791 (23.9)           |515 ( 49.9)            |8 ( 2.8)              |289 (17.8)            |0 (  0.0)             |97 (100.0)           |1656 ( 20.4)          |123 (43.6)             |103 ( 88.8)          |        |
|C-reactive protein concentration, mg/L (median [IQR])                                         |                                                                                          |85.00 [42.00, 147.00] |125.75 [64.93, 190.50] |89.00 [50.00, 142.00] |65.00 [32.00, 122.90] |79.15 [39.72, 134.60] |68.00 [33.50, 84.00] |86.00 [43.00, 145.00] |109.00 [67.00, 159.00] |4.90 [2.40, 9.75]    |4.1     |
|Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N]) (%) |Not seroconverted                                                                         |594 (33.9)            |0 (  NaN)              |7 ( 5.0)              |0 ( NaN)              |0 (  NaN)             |0 (  NaN)            |587 ( 36.4)           |0 ( NaN)               |0 (  NaN)            |85.0    |
|                                                                                              |Seroconverted                                                                             |1157 (66.1)           |0 (  NaN)              |132 (95.0)            |0 ( NaN)              |0 (  NaN)             |0 (  NaN)            |1025 ( 63.6)          |0 ( NaN)               |0 (  NaN)            |        |
|Patients with undetectable viral load (%)                                                     |Detectable viral load                                                                     |9594 (97.3)           |526 ( 75.9)            |116 (86.6)            |1494 (97.2)           |0 (  NaN)             |0 (  NaN)            |7195 ( 99.8)          |263 (93.3)             |0 (  NaN)            |15.6    |
|                                                                                              |Undetectable viral load                                                                   |265 ( 2.7)            |167 ( 24.1)            |18 (13.4)             |43 ( 2.8)             |0 (  NaN)             |0 (  NaN)            |18 (  0.2)            |19 ( 6.7)              |0 (  NaN)            |        |


# Missing data table for outcomes and adjustment variables, by trial

```r
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

#print
kable(missing_summary, format = "markdown", table.attr = 'class="table"', caption = "Missing values in outcomes and adjustment variables, by trial") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



Table: Missing values in outcomes and adjustment variables, by trial

|trial         |age_summary |clinstatus_baseline_summary |mort_28_summary |mort_60_summary |new_mvd_28_summary |clinstatus_28_imp_summary |ae_28_summary  |vir_clear_5_summary |vir_clear_10_summary |vir_clear_15_summary |
|:-------------|:-----------|:---------------------------|:---------------|:---------------|:------------------|:-------------------------|:--------------|:-------------------|:--------------------|:--------------------|
|ACTT2         |0/1033/0    |0/1033/0                    |47/1033/4.5     |47/1033/4.5     |34/1033/3.3        |0/1033/0                  |61/1033/5.9    |286/1033/27.7       |278/1033/26.9        |274/1033/26.5        |
|Bari-Solidact |0/289/0     |0/289/0                     |12/289/4.2      |12/289/4.2      |11/289/3.8         |0/289/0                   |36/289/12.5    |168/289/58.1        |162/289/56.1         |161/289/55.7         |
|COV-BARRIER   |0/1626/0    |7/1626/0.4                  |103/1626/6.3    |128/1626/7.9    |83/1626/5.1        |0/1626/0                  |301/1626/18.5  |648/1626/39.9       |566/1626/34.8        |530/1626/32.6        |
|COVINIB       |0/110/0     |0/110/0                     |3/110/2.7       |3/110/2.7       |3/110/2.7          |0/110/0                   |2/110/1.8      |110/110/100         |110/110/100          |110/110/100          |
|Ghazaeian     |0/97/0      |0/97/0                      |0/97/0          |0/97/0          |0/97/0             |0/97/0                    |0/97/0         |97/97/100           |97/97/100            |97/97/100            |
|RECOVERY      |0/8130/0    |0/8130/0                    |129/8130/1.6    |129/8130/1.6    |101/8130/1.2       |0/8130/0                  |1172/8130/14.4 |613/8130/7.5        |420/8130/5.2         |347/8130/4.3         |
|TACTIC-R      |0/282/0     |0/282/0                     |14/282/5        |18/282/6.4      |0/282/0            |0/282/0                   |35/282/12.4    |232/282/82.3        |218/282/77.3         |217/282/77           |
|TOFACOV       |0/116/0     |0/116/0                     |0/116/0         |0/116/0         |0/116/0            |0/116/0                   |0/116/0        |116/116/100         |116/116/100          |116/116/100          |

# Missing data table for all variables, by trial

```r
table(df_tot$mort_28,useNA = "always")
```

```
## 
##    0    1 <NA> 
## 9964 1411  308
```

```r
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

|                          |level                                     |Overall               |ACTT2                  |Bari-Solidact         |COV-BARRIER           |COVINIB               |Ghazaeian            |RECOVERY              |TACTIC-R               |TOFACOV              |Missing |
|:-------------------------|:-----------------------------------------|:---------------------|:----------------------|:---------------------|:---------------------|:---------------------|:--------------------|:---------------------|:----------------------|:--------------------|:-------|
|n                         |                                          |11683                 |1033                   |289                   |1626                  |110                   |97                   |8130                  |282                    |116                  |        |
|age (median [IQR])        |                                          |58.00 [47.00, 68.00]  |56.00 [43.00, 67.00]   |60.00 [50.00, 69.00]  |58.00 [48.00, 68.00]  |55.00 [47.25, 62.00]  |51.00 [37.00, 64.00] |58.00 [47.00, 69.00]  |60.00 [52.00, 69.00]   |58.00 [50.75, 66.25] |0.0     |
|sex (%)                   |1                                         |218 ( 1.9)            |0 (  0.0)              |218 ( 75.4)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0.0     |
|                          |2                                         |71 ( 0.6)             |0 (  0.0)              |71 ( 24.6)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |F                                         |381 ( 3.3)            |381 ( 36.9)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |female                                    |3568 (30.5)           |0 (  0.0)              |0 (  0.0)             |608 ( 37.4)           |34 ( 30.9)            |50 ( 51.5)           |2764 ( 34.0)          |76 ( 27.0)             |36 ( 31.0)           |        |
|                          |M                                         |652 ( 5.6)            |652 ( 63.1)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |male                                      |6793 (58.1)           |0 (  0.0)              |0 (  0.0)             |1018 ( 62.6)          |76 ( 69.1)            |47 ( 48.5)           |5366 ( 66.0)          |206 ( 73.0)            |80 ( 69.0)           |        |
|ethn (%)                  |                                          |34 ( 0.3)             |0 (  0.0)              |0 (  0.0)             |34 (  2.1)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |5.0     |
|                          |AMERICAN INDIAN OR ALASKA NATIVE          |358 ( 3.1)            |10 (  1.0)             |0 (  0.0)             |348 ( 21.4)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |Asian                                     |1 ( 0.0)              |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |1 (  0.9)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |ASIAN                                     |763 ( 6.5)            |101 (  9.8)            |0 (  0.0)             |175 ( 10.8)           |0 (  0.0)             |0 (  0.0)            |487 (  6.0)           |0 (  0.0)              |0 (  0.0)            |        |
|                          |Asian or Asian British                    |32 ( 0.3)             |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |32 ( 11.3)             |0 (  0.0)            |        |
|                          |BLACK OR AFRICAN AMERICAN                 |453 ( 3.9)            |156 ( 15.1)            |0 (  0.0)             |77 (  4.7)            |0 (  0.0)             |0 (  0.0)            |220 (  2.7)           |0 (  0.0)              |0 (  0.0)            |        |
|                          |Black or Black British                    |31 ( 0.3)             |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |31 ( 11.0)             |0 (  0.0)            |        |
|                          |caucasian                                 |116 ( 1.0)            |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |116 (100.0)          |        |
|                          |HISPANIC OR LATINO                        |246 ( 2.1)            |246 ( 23.8)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |Latino                                    |19 ( 0.2)             |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |19 ( 17.3)            |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |Mixed                                     |1 ( 0.0)              |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |1 (  0.4)              |0 (  0.0)            |        |
|                          |MIXED                                     |74 ( 0.6)             |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |74 (  0.9)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |MULTIPLE                                  |5 ( 0.0)              |0 (  0.0)              |0 (  0.0)             |5 (  0.3)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER |16 ( 0.1)             |11 (  1.1)             |0 (  0.0)             |5 (  0.3)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |Other                                     |19 ( 0.2)             |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |19 (  6.7)             |0 (  0.0)            |        |
|                          |OTHER                                     |133 ( 1.1)            |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |133 (  1.6)           |0 (  0.0)              |0 (  0.0)            |        |
|                          |Persian/Mazani                            |97 ( 0.8)             |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |97 (100.0)           |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |UNKNOWN                                   |419 ( 3.6)            |13 (  1.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |406 (  5.0)           |0 (  0.0)              |0 (  0.0)            |        |
|                          |White                                     |289 ( 2.5)            |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |90 ( 81.8)            |0 (  0.0)            |0 (  0.0)             |199 ( 70.6)            |0 (  0.0)            |        |
|                          |WHITE                                     |7990 (68.4)           |496 ( 48.0)            |0 (  0.0)             |982 ( 60.4)           |0 (  0.0)             |0 (  0.0)            |6512 ( 80.1)          |0 (  0.0)              |0 (  0.0)            |        |
|                          |NA                                        |587 ( 5.0)            |0 (  0.0)              |289 (100.0)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |298 (  3.7)           |0 (  0.0)              |0 (  0.0)            |        |
|country (%)               |ARG                                       |229 ( 2.0)            |0 (  0.0)              |0 (  0.0)             |229 ( 14.1)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |0.0     |
|                          |Asia                                      |67 ( 0.6)             |67 (  6.5)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |AUSTRIA                                   |6 ( 0.1)              |0 (  0.0)              |6 (  2.1)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |BELGIUM                                   |10 ( 0.1)             |0 (  0.0)              |10 (  3.5)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |BRA                                       |366 ( 3.1)            |0 (  0.0)              |0 (  0.0)             |366 ( 22.5)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |DEU                                       |20 ( 0.2)             |0 (  0.0)              |0 (  0.0)             |20 (  1.2)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |ESP                                       |87 ( 0.7)             |0 (  0.0)              |0 (  0.0)             |87 (  5.4)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |Europe                                    |13 ( 0.1)             |13 (  1.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |FRANCE                                    |94 ( 0.8)             |0 (  0.0)              |94 ( 32.5)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |GBR                                       |8141 (69.7)           |0 (  0.0)              |0 (  0.0)             |11 (  0.7)            |0 (  0.0)             |0 (  0.0)            |8130 (100.0)          |0 (  0.0)              |0 (  0.0)            |        |
|                          |GERMANY                                   |1 ( 0.0)              |0 (  0.0)              |1 (  0.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |IND                                       |50 ( 0.4)             |0 (  0.0)              |0 (  0.0)             |50 (  3.1)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |Iran                                      |97 ( 0.8)             |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |97 (100.0)           |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |IRELAND                                   |9 ( 0.1)              |0 (  0.0)              |9 (  3.1)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |ITA                                       |25 ( 0.2)             |0 (  0.0)              |0 (  0.0)             |25 (  1.5)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |Italy                                     |116 ( 1.0)            |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |116 (100.0)          |        |
|                          |ITALY                                     |25 ( 0.2)             |0 (  0.0)              |25 (  8.7)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |JPN                                       |38 ( 0.3)             |0 (  0.0)              |0 (  0.0)             |38 (  2.3)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |KOR                                       |36 ( 0.3)             |0 (  0.0)              |0 (  0.0)             |36 (  2.2)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |LUXEMBOURG                                |1 ( 0.0)              |0 (  0.0)              |1 (  0.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |MEX                                       |312 ( 2.7)            |0 (  0.0)              |0 (  0.0)             |312 ( 19.2)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |North America                             |953 ( 8.2)            |953 ( 92.3)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |NORWAY                                    |127 ( 1.1)            |0 (  0.0)              |127 ( 43.9)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |PORTUGAL                                  |3 ( 0.0)              |0 (  0.0)              |3 (  1.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |RUS                                       |112 ( 1.0)            |0 (  0.0)              |0 (  0.0)             |112 (  6.9)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |Spain                                     |110 ( 0.9)            |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |110 (100.0)           |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |SPAIN                                     |13 ( 0.1)             |0 (  0.0)              |13 (  4.5)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |UK                                        |282 ( 2.4)            |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |282 (100.0)            |0 (  0.0)            |        |
|                          |USA                                       |340 ( 2.9)            |0 (  0.0)              |0 (  0.0)             |340 ( 20.9)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|vacc (%)                  |0                                         |6146 (52.6)           |1033 (100.0)           |182 ( 63.0)           |0 (  0.0)             |108 ( 98.2)           |0 (  0.0)            |4710 ( 57.9)          |0 (  0.0)              |113 ( 97.4)          |17.2    |
|                          |1                                         |3527 (30.2)           |0 (  0.0)              |102 ( 35.3)           |0 (  0.0)             |2 (  1.8)             |0 (  0.0)            |3420 ( 42.1)          |0 (  0.0)              |3 (  2.6)            |        |
|                          |NA                                        |2010 (17.2)           |0 (  0.0)              |5 (  1.7)             |1626 (100.0)          |0 (  0.0)             |97 (100.0)           |0 (  0.0)             |282 (100.0)            |0 (  0.0)            |        |
|sympdur (median [IQR])    |                                          |10.00 [7.00, 12.00]   |8.00 [5.00, 10.00]     |9.00 [7.00, 11.00]    |10.00 [8.00, 13.00]   |7.00 [5.00, 9.00]     |7.00 [6.00, 10.00]   |10.00 [7.00, 12.00]   |10.00 [7.00, 13.00]    |8.00 [6.00, 10.00]   |0.2     |
|icu (%)                   |0                                         |2278 (19.5)           |0 (  0.0)              |160 ( 55.4)           |1525 ( 93.8)          |110 (100.0)           |97 (100.0)           |0 (  0.0)             |270 ( 95.7)            |116 (100.0)          |78.4    |
|                          |1                                         |242 ( 2.1)            |0 (  0.0)              |129 ( 44.6)           |101 (  6.2)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |12 (  4.3)             |0 (  0.0)            |        |
|                          |NA                                        |9163 (78.4)           |1033 (100.0)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |8130 (100.0)          |0 (  0.0)              |0 (  0.0)            |        |
|clinstatus_baseline (%)   |2                                         |866 ( 7.4)            |142 ( 13.7)            |0 (  0.0)             |186 ( 11.4)           |35 ( 31.8)            |2 (  2.1)            |465 (  5.7)           |11 (  3.9)             |25 ( 21.6)           |0.1     |
|                          |3                                         |7421 (63.5)           |564 ( 54.6)            |0 (  0.0)             |962 ( 59.2)           |75 ( 68.2)            |95 ( 97.9)           |5504 ( 67.7)          |130 ( 46.1)            |91 ( 78.4)           |        |
|                          |4                                         |2895 (24.8)           |216 ( 20.9)            |249 ( 86.2)           |370 ( 22.8)           |0 (  0.0)             |0 (  0.0)            |1921 ( 23.6)          |139 ( 49.3)            |0 (  0.0)            |        |
|                          |5                                         |494 ( 4.2)            |111 ( 10.7)            |40 ( 13.8)            |101 (  6.2)           |0 (  0.0)             |0 (  0.0)            |240 (  3.0)           |2 (  0.7)              |0 (  0.0)            |        |
|                          |NA                                        |7 ( 0.1)              |0 (  0.0)              |0 (  0.0)             |7 (  0.4)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|comorb_cat (%)            |1                                         |5151 (44.1)           |151 ( 14.6)            |106 ( 36.7)           |370 ( 22.8)           |37 ( 33.6)            |41 ( 42.3)           |4353 ( 53.5)          |49 ( 17.4)             |44 ( 37.9)           |0.2     |
|                          |2                                         |3548 (30.4)           |288 ( 27.9)            |73 ( 25.3)            |477 ( 29.3)           |40 ( 36.4)            |22 ( 22.7)           |2538 ( 31.2)          |64 ( 22.7)             |46 ( 39.7)           |        |
|                          |3                                         |2868 (24.5)           |546 ( 52.9)            |99 ( 34.3)            |761 ( 46.8)           |31 ( 28.2)            |32 ( 33.0)           |1239 ( 15.2)          |134 ( 47.5)            |26 ( 22.4)           |        |
|                          |4                                         |98 ( 0.8)             |30 (  2.9)             |11 (  3.8)            |18 (  1.1)            |2 (  1.8)             |2 (  2.1)            |0 (  0.0)             |35 ( 12.4)             |0 (  0.0)            |        |
|                          |NA                                        |18 ( 0.2)             |18 (  1.7)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|comed_cat (%)             |1                                         |1823 (15.6)           |991 ( 95.9)            |18 (  6.2)            |328 ( 20.2)           |94 ( 85.5)            |0 (  0.0)            |349 (  4.3)           |36 ( 12.8)             |7 (  6.0)            |0.1     |
|                          |2                                         |2603 (22.3)           |0 (  0.0)              |1 (  0.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |2590 ( 31.9)          |12 (  4.3)             |0 (  0.0)            |        |
|                          |3                                         |7216 (61.8)           |42 (  4.1)             |270 ( 93.4)           |1291 ( 79.4)          |16 ( 14.5)            |97 (100.0)           |5157 ( 63.4)          |234 ( 83.0)            |109 ( 94.0)          |        |
|                          |4                                         |34 ( 0.3)             |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |34 (  0.4)            |0 (  0.0)              |0 (  0.0)            |        |
|                          |NA                                        |7 ( 0.1)              |0 (  0.0)              |0 (  0.0)             |7 (  0.4)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|comed_rdv (%)             |0                                         |8892 (76.1)           |518 ( 50.1)            |281 ( 97.2)           |1337 ( 82.2)          |110 (100.0)           |0 (  0.0)            |6474 ( 79.6)          |159 ( 56.4)            |13 ( 11.2)           |0.0     |
|                          |1                                         |2791 (23.9)           |515 ( 49.9)            |8 (  2.8)             |289 ( 17.8)           |0 (  0.0)             |97 (100.0)           |1656 ( 20.4)          |123 ( 43.6)            |103 ( 88.8)          |        |
|crp (median [IQR])        |                                          |85.00 [42.00, 147.00] |125.75 [64.93, 190.50] |89.00 [50.00, 142.00] |65.00 [32.00, 122.90] |79.15 [39.72, 134.60] |68.00 [33.50, 84.00] |86.00 [43.00, 145.00] |109.00 [67.00, 159.00] |4.90 [2.40, 9.75]    |4.1     |
|sero (%)                  |0                                         |594 ( 5.1)            |0 (  0.0)              |7 (  2.4)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |587 (  7.2)           |0 (  0.0)              |0 (  0.0)            |85.0    |
|                          |1                                         |1157 ( 9.9)           |0 (  0.0)              |132 ( 45.7)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |1025 ( 12.6)          |0 (  0.0)              |0 (  0.0)            |        |
|                          |NA                                        |9932 (85.0)           |1033 (100.0)           |150 ( 51.9)           |1626 (100.0)          |110 (100.0)           |97 (100.0)           |6518 ( 80.2)          |282 (100.0)            |116 (100.0)          |        |
|vl_baseline (%)           |0                                         |9594 (82.1)           |526 ( 50.9)            |116 ( 40.1)           |1494 ( 91.9)          |0 (  0.0)             |0 (  0.0)            |7195 ( 88.5)          |263 ( 93.3)            |0 (  0.0)            |15.6    |
|                          |1                                         |265 ( 2.3)            |167 ( 16.2)            |18 (  6.2)            |43 (  2.6)            |0 (  0.0)             |0 (  0.0)            |18 (  0.2)            |19 (  6.7)             |0 (  0.0)            |        |
|                          |NA                                        |1824 (15.6)           |340 ( 32.9)            |155 ( 53.6)           |89 (  5.5)            |110 (100.0)           |97 (100.0)           |917 ( 11.3)           |0 (  0.0)              |116 (100.0)          |        |
|variant (%)               |Delta                                     |21 ( 0.2)             |0 (  0.0)              |21 (  7.3)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |99.7    |
|                          |Omicron                                   |10 ( 0.1)             |0 (  0.0)              |10 (  3.5)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |PRESENCE OF MUTATIONS E484Q AND L452R     |1 ( 0.0)              |0 (  0.0)              |1 (  0.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |NA                                        |11651 (99.7)          |1033 (100.0)           |257 ( 88.9)           |1626 (100.0)          |110 (100.0)           |97 (100.0)           |8130 (100.0)          |282 (100.0)            |116 (100.0)          |        |
|mort_28 (%)               |0                                         |9964 (85.3)           |925 ( 89.5)            |241 ( 83.4)           |1312 ( 80.7)          |105 ( 95.5)           |90 ( 92.8)           |6943 ( 85.4)          |233 ( 82.6)            |115 ( 99.1)          |2.6     |
|                          |1                                         |1411 (12.1)           |61 (  5.9)             |36 ( 12.5)            |211 ( 13.0)           |2 (  1.8)             |7 (  7.2)            |1058 ( 13.0)          |35 ( 12.4)             |1 (  0.9)            |        |
|                          |NA                                        |308 ( 2.6)            |47 (  4.5)             |12 (  4.2)            |103 (  6.3)           |3 (  2.7)             |0 (  0.0)            |129 (  1.6)           |14 (  5.0)             |0 (  0.0)            |        |
|mort_60 (%)               |0                                         |9880 (84.6)           |925 ( 89.5)            |231 ( 79.9)           |1249 ( 76.8)          |105 ( 95.5)           |90 ( 92.8)           |6943 ( 85.4)          |222 ( 78.7)            |115 ( 99.1)          |2.9     |
|                          |1                                         |1466 (12.5)           |61 (  5.9)             |46 ( 15.9)            |249 ( 15.3)           |2 (  1.8)             |7 (  7.2)            |1058 ( 13.0)          |42 ( 14.9)             |1 (  0.9)            |        |
|                          |NA                                        |337 ( 2.9)            |47 (  4.5)             |12 (  4.2)            |128 (  7.9)           |3 (  2.7)             |0 (  0.0)            |129 (  1.6)           |18 (  6.4)             |0 (  0.0)            |        |
|death_reached (%)         |0                                         |10085 (86.3)          |972 ( 94.1)            |242 ( 83.7)           |1377 ( 84.7)          |108 ( 98.2)           |90 ( 92.8)           |6943 ( 85.4)          |238 ( 84.4)            |115 ( 99.1)          |1.1     |
|                          |1                                         |1469 (12.6)           |61 (  5.9)             |47 ( 16.3)            |249 ( 15.3)           |2 (  1.8)             |7 (  7.2)            |1058 ( 13.0)          |44 ( 15.6)             |1 (  0.9)            |        |
|                          |NA                                        |129 ( 1.1)            |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |129 (  1.6)           |0 (  0.0)              |0 (  0.0)            |        |
|new_mv_28 (%)             |0                                         |9102 (77.9)           |768 ( 74.3)            |174 ( 60.2)           |1141 ( 70.2)          |97 ( 88.2)            |90 ( 92.8)           |6487 ( 79.8)          |232 ( 82.3)            |113 ( 97.4)          |16.8    |
|                          |1                                         |621 ( 5.3)            |91 (  8.8)             |37 ( 12.8)            |138 (  8.5)           |8 (  7.3)             |0 (  0.0)            |332 (  4.1)           |13 (  4.6)             |2 (  1.7)            |        |
|                          |NA                                        |1960 (16.8)           |174 ( 16.8)            |78 ( 27.0)            |347 ( 21.3)           |5 (  4.5)             |7 (  7.2)            |1311 ( 16.1)          |37 ( 13.1)             |1 (  0.9)            |        |
|new_mvd_28 (%)            |0                                         |9367 (80.2)           |847 ( 82.0)            |205 ( 70.9)           |1142 ( 70.2)          |97 ( 88.2)            |90 ( 92.8)           |6639 ( 81.7)          |234 ( 83.0)            |113 ( 97.4)          |2.0     |
|                          |1                                         |2084 (17.8)           |152 ( 14.7)            |73 ( 25.3)            |401 ( 24.7)           |10 (  9.1)            |7 (  7.2)            |1390 ( 17.1)          |48 ( 17.0)             |3 (  2.6)            |        |
|                          |NA                                        |232 ( 2.0)            |34 (  3.3)             |11 (  3.8)            |83 (  5.1)            |3 (  2.7)             |0 (  0.0)            |101 (  1.2)           |0 (  0.0)              |0 (  0.0)            |        |
|clinstatus_28_imp (%)     |1                                         |9145 (78.3)           |824 ( 79.8)            |187 ( 64.7)           |1212 ( 74.5)          |103 ( 93.6)           |90 ( 92.8)           |6405 ( 78.8)          |214 ( 75.9)            |110 ( 94.8)          |0.0     |
|                          |2                                         |89 ( 0.8)             |19 (  1.8)             |9 (  3.1)             |24 (  1.5)            |0 (  0.0)             |0 (  0.0)            |30 (  0.4)            |7 (  2.5)              |0 (  0.0)            |        |
|                          |3                                         |413 ( 3.5)            |39 (  3.8)             |18 (  6.2)            |66 (  4.1)            |3 (  2.7)             |0 (  0.0)            |275 (  3.4)           |11 (  3.9)             |1 (  0.9)            |        |
|                          |4                                         |334 ( 2.9)            |23 (  2.2)             |13 (  4.5)            |23 (  1.4)            |0 (  0.0)             |0 (  0.0)            |264 (  3.2)           |8 (  2.8)              |3 (  2.6)            |        |
|                          |5                                         |293 ( 2.5)            |67 (  6.5)             |26 (  9.0)            |90 (  5.5)            |2 (  1.8)             |0 (  0.0)            |101 (  1.2)           |6 (  2.1)              |1 (  0.9)            |        |
|                          |6                                         |1409 (12.1)           |61 (  5.9)             |36 ( 12.5)            |211 ( 13.0)           |2 (  1.8)             |7 (  7.2)            |1055 ( 13.0)          |36 ( 12.8)             |1 (  0.9)            |        |
|discharge_reached (%)     |0                                         |2457 (21.0)           |194 ( 18.8)            |96 ( 33.2)            |406 ( 25.0)           |7 (  6.4)             |7 (  7.2)            |1678 ( 20.6)          |63 ( 22.3)             |6 (  5.2)            |0.0     |
|                          |1                                         |9226 (79.0)           |839 ( 81.2)            |193 ( 66.8)           |1220 ( 75.0)          |103 ( 93.6)           |90 ( 92.8)           |6452 ( 79.4)          |219 ( 77.7)            |110 ( 94.8)          |        |
|discharge_reached_sus (%) |0                                         |2467 (21.1)           |194 ( 18.8)            |98 ( 33.9)            |414 ( 25.5)           |7 (  6.4)             |7 (  7.2)            |1678 ( 20.6)          |63 ( 22.3)             |6 (  5.2)            |0.0     |
|                          |1                                         |9216 (78.9)           |839 ( 81.2)            |191 ( 66.1)           |1212 ( 74.5)          |103 ( 93.6)           |90 ( 92.8)           |6452 ( 79.4)          |219 ( 77.7)            |110 ( 94.8)          |        |
|ae_28 (%)                 |0                                         |8727 (74.7)           |562 ( 54.4)            |165 ( 57.1)           |1011 ( 62.2)          |85 ( 77.3)            |96 ( 99.0)           |6499 ( 79.9)          |215 ( 76.2)            |94 ( 81.0)           |13.8    |
|                          |1                                         |1349 (11.5)           |410 ( 39.7)            |88 ( 30.4)            |314 ( 19.3)           |23 ( 20.9)            |1 (  1.0)            |459 (  5.6)           |32 ( 11.3)             |22 ( 19.0)           |        |
|                          |NA                                        |1607 (13.8)           |61 (  5.9)             |36 ( 12.5)            |301 ( 18.5)           |2 (  1.8)             |0 (  0.0)            |1172 ( 14.4)          |35 ( 12.4)             |0 (  0.0)            |        |
|ae_28_sev (%)             |0                                         |8727 (74.7)           |562 ( 54.4)            |165 ( 57.1)           |1011 ( 62.2)          |85 ( 77.3)            |96 ( 99.0)           |6499 ( 79.9)          |215 ( 76.2)            |94 ( 81.0)           |13.8    |
|                          |1                                         |882 ( 7.5)            |198 ( 19.2)            |49 ( 17.0)            |154 (  9.5)           |14 ( 12.7)            |1 (  1.0)            |419 (  5.2)           |25 (  8.9)             |22 ( 19.0)           |        |
|                          |2                                         |210 ( 1.8)            |77 (  7.5)             |15 (  5.2)            |77 (  4.7)            |3 (  2.7)             |0 (  0.0)            |34 (  0.4)            |4 (  1.4)              |0 (  0.0)            |        |
|                          |3                                         |92 ( 0.8)             |38 (  3.7)             |11 (  3.8)            |36 (  2.2)            |1 (  0.9)             |0 (  0.0)            |4 (  0.0)             |2 (  0.7)              |0 (  0.0)            |        |
|                          |4                                         |57 ( 0.5)             |36 (  3.5)             |4 (  1.4)             |12 (  0.7)            |2 (  1.8)             |0 (  0.0)            |2 (  0.0)             |1 (  0.4)              |0 (  0.0)            |        |
|                          |5                                         |35 ( 0.3)             |15 (  1.5)             |2 (  0.7)             |17 (  1.0)            |1 (  0.9)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |6                                         |19 ( 0.2)             |7 (  0.7)              |3 (  1.0)             |7 (  0.4)             |2 (  1.8)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |7                                         |17 ( 0.1)             |13 (  1.3)             |1 (  0.3)             |3 (  0.2)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |8                                         |8 ( 0.1)              |7 (  0.7)              |1 (  0.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |9                                         |11 ( 0.1)             |6 (  0.6)              |1 (  0.3)             |4 (  0.2)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |10                                        |6 ( 0.1)              |4 (  0.4)              |1 (  0.3)             |1 (  0.1)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |11                                        |3 ( 0.0)              |3 (  0.3)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |12                                        |2 ( 0.0)              |1 (  0.1)              |0 (  0.0)             |1 (  0.1)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |13                                        |3 ( 0.0)              |1 (  0.1)              |0 (  0.0)             |2 (  0.1)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |16                                        |1 ( 0.0)              |1 (  0.1)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |20                                        |1 ( 0.0)              |1 (  0.1)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |22                                        |1 ( 0.0)              |1 (  0.1)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |26                                        |1 ( 0.0)              |1 (  0.1)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)              |0 (  0.0)            |        |
|                          |NA                                        |1607 (13.8)           |61 (  5.9)             |36 ( 12.5)            |301 ( 18.5)           |2 (  1.8)             |0 (  0.0)            |1172 ( 14.4)          |35 ( 12.4)             |0 (  0.0)            |        |
|vir_clear_5 (%)           |0                                         |8774 (75.1)           |496 ( 48.0)            |91 ( 31.5)            |734 ( 45.1)           |0 (  0.0)             |0 (  0.0)            |7418 ( 91.2)          |35 ( 12.4)             |0 (  0.0)            |19.4    |
|                          |1                                         |639 ( 5.5)            |251 ( 24.3)            |30 ( 10.4)            |244 ( 15.0)           |0 (  0.0)             |0 (  0.0)            |99 (  1.2)            |15 (  5.3)             |0 (  0.0)            |        |
|                          |NA                                        |2270 (19.4)           |286 ( 27.7)            |168 ( 58.1)           |648 ( 39.9)           |110 (100.0)           |97 (100.0)           |613 (  7.5)           |232 ( 82.3)            |116 (100.0)          |        |
|vir_clear_10 (%)          |0                                         |8792 (75.3)           |456 ( 44.1)            |69 ( 23.9)            |654 ( 40.2)           |0 (  0.0)             |0 (  0.0)            |7575 ( 93.2)          |38 ( 13.5)             |0 (  0.0)            |16.8    |
|                          |1                                         |924 ( 7.9)            |299 ( 28.9)            |58 ( 20.1)            |406 ( 25.0)           |0 (  0.0)             |0 (  0.0)            |135 (  1.7)           |26 (  9.2)             |0 (  0.0)            |        |
|                          |NA                                        |1967 (16.8)           |278 ( 26.9)            |162 ( 56.1)           |566 ( 34.8)           |110 (100.0)           |97 (100.0)           |420 (  5.2)           |218 ( 77.3)            |116 (100.0)          |        |
|vir_clear_15 (%)          |0                                         |8708 (74.5)           |398 ( 38.5)            |57 ( 19.7)            |584 ( 35.9)           |0 (  0.0)             |0 (  0.0)            |7633 ( 93.9)          |36 ( 12.8)             |0 (  0.0)            |15.9    |
|                          |1                                         |1123 ( 9.6)           |361 ( 34.9)            |71 ( 24.6)            |512 ( 31.5)           |0 (  0.0)             |0 (  0.0)            |150 (  1.8)           |29 ( 10.3)             |0 (  0.0)            |        |
|                          |NA                                        |1852 (15.9)           |274 ( 26.5)            |161 ( 55.7)           |530 ( 32.6)           |110 (100.0)           |97 (100.0)           |347 (  4.3)           |217 ( 77.0)            |116 (100.0)          |        |
