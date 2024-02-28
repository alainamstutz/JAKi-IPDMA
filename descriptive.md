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
vars.list <- c("trt", "trial", "JAKi", "age", "sex", "ethn", "country", "vacc", "sympdur", "icu", "clinstatus_baseline", "comorb_cat", "comed_cat", "comed_rdv", "crp", "sero", "vl_baseline")

df_baseline <- df_tot[,colnames(df_tot)%in%vars.list]
df_baseline <- df_baseline[,match(vars.list,colnames(df_baseline))]

colnames(df_baseline) <- vars.list <- c("ARM","Trial","JAK inhibitor","Age, years", "Sex", "Ethnicity", "Country", "Vaccination", "Time from symptom onset to randomisation, days", "Patients admitted to intensive care unit","Clinical status on ordinal scale", "Comorbidities", "Dexamethasone and Tocilizumab", "Remdesivir","C-reactive protein concentration, mg/L", "Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N])", "Patients with undetectable viral load")

char_vars <- c("Trial","JAK inhibitor","Sex", "Ethnicity", "Country", "Vaccination","Patients admitted to intensive care unit","Clinical status on ordinal scale","Comorbidities", "Dexamethasone and Tocilizumab", "Remdesivir","Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N])", "Patients with undetectable viral load")

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
                             Ethnicity == "BLACK OR AFRICAN AMERICAN" ~ "Black or African American",
                             Ethnicity == "Asian" | Ethnicity == "ASIAN" ~ "Asian",
                             Ethnicity == "caucasian" | Ethnicity == "White" | Ethnicity == "WHITE" ~ "Caucasian",
                             Ethnicity == "HISPANIC OR LATINO" | Ethnicity == "Latino" ~ "Hispanic or Latino",
                             Ethnicity == "MIXED" | Ethnicity == "MULTIPLE" ~ "Mixed",
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
vars.list_main <- c("ARM","JAK inhibitor","Age, years", "Sex", "Vaccination", "Time from symptom onset to randomisation, days", "Clinical status on ordinal scale", "Comorbidities", "Dexamethasone and Tocilizumab", "Remdesivir", "C-reactive protein concentration, mg/L", "Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N])", "Patients with undetectable viral load")
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
|n                                                                                             |                                                                                          |11401                 |5770                  |5631                  |        |
|JAK inhibitor (%)                                                                             |Baricitinib                                                                               |11188 (98.1)          |5666 (98.2)           |5522 (98.1)           |0.0     |
|                                                                                              |Tofacitinib                                                                               |213 ( 1.9)            |104 ( 1.8)            |109 ( 1.9)            |        |
|Age, years (median [IQR])                                                                     |                                                                                          |58.00 [47.00, 68.00]  |58.00 [47.00, 69.00]  |58.00 [47.00, 68.00]  |0.0     |
|Sex (%)                                                                                       |Female                                                                                    |4091 (35.9)           |2068 (35.8)           |2023 (35.9)           |0.0     |
|                                                                                              |Male                                                                                      |7310 (64.1)           |3702 (64.2)           |3608 (64.1)           |        |
|Vaccination (%)                                                                               |Any SARS CoV-2 vaccine                                                                    |3527 (36.5)           |1809 (36.9)           |1718 (36.0)           |15.2    |
|                                                                                              |No SARS CoV-2 vaccine                                                                     |6146 (63.5)           |3097 (63.1)           |3049 (64.0)           |        |
|Time from symptom onset to randomisation, days (median [IQR])                                 |                                                                                          |10.00 [7.00, 12.00]   |10.00 [7.00, 12.00]   |10.00 [7.00, 12.00]   |0.2     |
|Clinical status on ordinal scale (%)                                                          |2: Hospitalised without need for oxygen therapy (WHO score 4)                             |855 ( 7.5)            |412 ( 7.1)            |443 ( 7.9)            |0.1     |
|                                                                                              |3: Hospitalised with need for supplemental low-flow oxygen (WHO score 5)                  |7291 (64.0)           |3678 (63.8)           |3613 (64.2)           |        |
|                                                                                              |4: Hospitalised with need for high-flow oxygen or non- invasive ventilation (WHO score 6) |2756 (24.2)           |1428 (24.8)           |1328 (23.6)           |        |
|                                                                                              |5: Hospitalised with need for mechanical ventilation or ECMO (WHO score 7–9)              |492 ( 4.3)            |250 ( 4.3)            |242 ( 4.3)            |        |
|Comorbidities (%)                                                                             |No comorbidity                                                                            |5102 (44.8)           |2550 (44.3)           |2552 (45.4)           |0.2     |
|                                                                                              |One comorbidity                                                                           |3484 (30.6)           |1774 (30.8)           |1710 (30.4)           |        |
|                                                                                              |Multiple comorbidities                                                                    |2734 (24.0)           |1405 (24.4)           |1329 (23.6)           |        |
|                                                                                              |Immunocompromised                                                                         |63 ( 0.6)             |33 ( 0.6)             |30 ( 0.5)             |        |
|Dexamethasone and Tocilizumab (%)                                                             |No Dexamethasone, no Tocilizumab                                                          |1787 (15.7)           |881 (15.3)            |906 (16.1)            |0.1     |
|                                                                                              |Dexamethasone but no Tocilizumab                                                          |6982 (61.3)           |3546 (61.5)           |3436 (61.1)           |        |
|                                                                                              |Dexamethasone and Tocilizumab                                                             |2591 (22.7)           |1323 (22.9)           |1268 (22.5)           |        |
|                                                                                              |Tocolizumab but no Dexamethasone                                                          |34 ( 0.3)             |18 ( 0.3)             |16 ( 0.3)             |        |
|Remdesivir (%)                                                                                |No Remdesivir                                                                             |8733 (76.6)           |4136 (71.7)           |4597 (81.6)           |0.0     |
|                                                                                              |Remdesivir                                                                                |2668 (23.4)           |1634 (28.3)           |1034 (18.4)           |        |
|C-reactive protein concentration, mg/L (median [IQR])                                         |                                                                                          |84.00 [41.00, 146.00] |83.37 [41.00, 148.00] |85.00 [41.00, 145.00] |4.2     |
|Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N]) (%) |Not seroconverted                                                                         |594 (33.9)            |285 (33.5)            |309 (34.4)            |84.6    |
|                                                                                              |Seroconverted                                                                             |1157 (66.1)           |567 (66.5)            |590 (65.6)            |        |
|Patients with undetectable viral load (%)                                                     |Detectable viral load                                                                     |9331 (97.4)           |4707 (97.4)           |4624 (97.4)           |16.0    |
|                                                                                              |Undetectable viral load                                                                   |246 ( 2.6)            |124 ( 2.6)            |122 ( 2.6)            |        |

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
|n                                            |                                          |11401       |5770          |5631             |        |
|Ethnicity (%)                                |American Indian or Alaska Native          |358 ( 3.5)  |165 ( 3.2)    |193 ( 3.8)       |10.3    |
|                                             |Asian                                     |764 ( 7.5)  |369 ( 7.1)    |395 ( 7.8)       |        |
|                                             |Black or African American                 |453 ( 4.4)  |230 ( 4.4)    |223 ( 4.4)       |        |
|                                             |Caucasian                                 |8196 (80.1) |4179 (80.8)   |4017 (79.4)      |        |
|                                             |Hispanic or Latino                        |265 ( 2.6)  |137 ( 2.6)    |128 ( 2.5)       |        |
|                                             |Mixed                                     |79 ( 0.8)   |37 ( 0.7)     |42 ( 0.8)        |        |
|                                             |Native Hawaiian or other Pacific Islander |16 ( 0.2)   |7 ( 0.1)      |9 ( 0.2)         |        |
|                                             |Persian/Mazani                            |97 ( 0.9)   |46 ( 0.9)     |51 ( 1.0)        |        |
|Country (%)                                  |Argentina                                 |229 ( 2.0)  |119 ( 2.1)    |110 ( 2.0)       |0.0     |
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
|Patients admitted to intensive care unit (%) |No                                        |2008 (89.7) |1001 (89.5)   |1007 (90.0)      |80.4    |
|                                             |Yes                                       |230 (10.3)  |118 (10.5)    |112 (10.0)       |        |


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

|                                                                                              |level                                                                                     |Overall               |ACTT2                  |Bari-Solidact         |COV-BARRIER           |COVINIB               |Ghazaeian            |RECOVERY              |TOFACOV              |Missing |
|:---------------------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------|:---------------------|:----------------------|:---------------------|:---------------------|:---------------------|:--------------------|:---------------------|:--------------------|:-------|
|n                                                                                             |                                                                                          |11401                 |1033                   |289                   |1626                  |110                   |97                   |8130                  |116                  |        |
|JAK inhibitor (%)                                                                             |Baricitinib                                                                               |11188 (98.1)          |1033 (100.0)           |289 (100.0)           |1626 (100.0)          |110 (100.0)           |0 (  0.0)            |8130 (100.0)          |0 (  0.0)            |0.0     |
|                                                                                              |Tofacitinib                                                                               |213 ( 1.9)            |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |97 (100.0)           |0 (  0.0)             |116 (100.0)          |        |
|Age, years (median [IQR])                                                                     |                                                                                          |58.00 [47.00, 68.00]  |56.00 [43.00, 67.00]   |60.00 [50.00, 69.00]  |58.00 [48.00, 68.00]  |55.00 [47.25, 62.00]  |51.00 [37.00, 64.00] |58.00 [47.00, 69.00]  |58.00 [50.75, 66.25] |0.0     |
|Sex (%)                                                                                       |Female                                                                                    |4091 (35.9)           |381 ( 36.9)            |218 ( 75.4)           |608 ( 37.4)           |34 ( 30.9)            |50 ( 51.5)           |2764 ( 34.0)          |36 ( 31.0)           |0.0     |
|                                                                                              |Male                                                                                      |7310 (64.1)           |652 ( 63.1)            |71 ( 24.6)            |1018 ( 62.6)          |76 ( 69.1)            |47 ( 48.5)           |5366 ( 66.0)          |80 ( 69.0)           |        |
|Ethnicity (%)                                                                                 |American Indian or Alaska Native                                                          |358 ( 3.5)            |10 (  1.0)             |0 (  NaN)             |348 ( 21.9)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |10.3    |
|                                                                                              |Asian                                                                                     |764 ( 7.5)            |101 (  9.9)            |0 (  NaN)             |175 ( 11.0)           |1 (  0.9)             |0 (  0.0)            |487 (  6.7)           |0 (  0.0)            |        |
|                                                                                              |Black or African American                                                                 |453 ( 4.4)            |156 ( 15.3)            |0 (  NaN)             |77 (  4.8)            |0 (  0.0)             |0 (  0.0)            |220 (  3.0)           |0 (  0.0)            |        |
|                                                                                              |Caucasian                                                                                 |8196 (80.1)           |496 ( 48.6)            |0 (  NaN)             |982 ( 61.7)           |90 ( 81.8)            |0 (  0.0)            |6512 ( 89.3)          |116 (100.0)          |        |
|                                                                                              |Hispanic or Latino                                                                        |265 ( 2.6)            |246 ( 24.1)            |0 (  NaN)             |0 (  0.0)             |19 ( 17.3)            |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                                                                                              |Mixed                                                                                     |79 ( 0.8)             |0 (  0.0)              |0 (  NaN)             |5 (  0.3)             |0 (  0.0)             |0 (  0.0)            |74 (  1.0)            |0 (  0.0)            |        |
|                                                                                              |Native Hawaiian or other Pacific Islander                                                 |16 ( 0.2)             |11 (  1.1)             |0 (  NaN)             |5 (  0.3)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                                                                                              |Persian/Mazani                                                                            |97 ( 0.9)             |0 (  0.0)              |0 (  NaN)             |0 (  0.0)             |0 (  0.0)             |97 (100.0)           |0 (  0.0)             |0 (  0.0)            |        |
|Country (%)                                                                                   |Argentina                                                                                 |229 ( 2.0)            |0 (  0.0)              |0 (  0.0)             |229 ( 14.1)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |0.0     |
|                                                                                              |Asia*                                                                                     |67 ( 0.6)             |67 (  6.5)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                                                                                              |Austria                                                                                   |6 ( 0.1)              |0 (  0.0)              |6 (  2.1)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                                                                                              |Belgium                                                                                   |10 ( 0.1)             |0 (  0.0)              |10 (  3.5)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                                                                                              |Brasil                                                                                    |366 ( 3.2)            |0 (  0.0)              |0 (  0.0)             |366 ( 22.5)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                                                                                              |Europe*                                                                                   |13 ( 0.1)             |13 (  1.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                                                                                              |France                                                                                    |94 ( 0.8)             |0 (  0.0)              |94 ( 32.5)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                                                                                              |Germany                                                                                   |21 ( 0.2)             |0 (  0.0)              |1 (  0.3)             |20 (  1.2)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                                                                                              |India                                                                                     |50 ( 0.4)             |0 (  0.0)              |0 (  0.0)             |50 (  3.1)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                                                                                              |Iran                                                                                      |97 ( 0.9)             |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |97 (100.0)           |0 (  0.0)             |0 (  0.0)            |        |
|                                                                                              |Ireland                                                                                   |9 ( 0.1)              |0 (  0.0)              |9 (  3.1)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                                                                                              |Italy                                                                                     |166 ( 1.5)            |0 (  0.0)              |25 (  8.7)            |25 (  1.5)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |116 (100.0)          |        |
|                                                                                              |Japan                                                                                     |38 ( 0.3)             |0 (  0.0)              |0 (  0.0)             |38 (  2.3)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                                                                                              |Luxembourg                                                                                |1 ( 0.0)              |0 (  0.0)              |1 (  0.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                                                                                              |Mexico                                                                                    |312 ( 2.7)            |0 (  0.0)              |0 (  0.0)             |312 ( 19.2)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                                                                                              |North America*                                                                            |953 ( 8.4)            |953 ( 92.3)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                                                                                              |Norway                                                                                    |127 ( 1.1)            |0 (  0.0)              |127 ( 43.9)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                                                                                              |Portugal                                                                                  |3 ( 0.0)              |0 (  0.0)              |3 (  1.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                                                                                              |Russia                                                                                    |112 ( 1.0)            |0 (  0.0)              |0 (  0.0)             |112 (  6.9)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                                                                                              |South Korea                                                                               |36 ( 0.3)             |0 (  0.0)              |0 (  0.0)             |36 (  2.2)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                                                                                              |Spain                                                                                     |210 ( 1.8)            |0 (  0.0)              |13 (  4.5)            |87 (  5.4)            |110 (100.0)           |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                                                                                              |United Kingdom                                                                            |8141 (71.4)           |0 (  0.0)              |0 (  0.0)             |11 (  0.7)            |0 (  0.0)             |0 (  0.0)            |8130 (100.0)          |0 (  0.0)            |        |
|                                                                                              |USA                                                                                       |340 ( 3.0)            |0 (  0.0)              |0 (  0.0)             |340 ( 20.9)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|Vaccination (%)                                                                               |Any SARS CoV-2 vaccine                                                                    |3527 (36.5)           |0 (  0.0)              |102 ( 35.9)           |0 (  NaN)             |2 (  1.8)             |0 (  NaN)            |3420 ( 42.1)          |3 (  2.6)            |15.2    |
|                                                                                              |No SARS CoV-2 vaccine                                                                     |6146 (63.5)           |1033 (100.0)           |182 ( 64.1)           |0 (  NaN)             |108 ( 98.2)           |0 (  NaN)            |4710 ( 57.9)          |113 ( 97.4)          |        |
|Time from symptom onset to randomisation, days (median [IQR])                                 |                                                                                          |10.00 [7.00, 12.00]   |8.00 [5.00, 10.00]     |9.00 [7.00, 11.00]    |10.00 [8.00, 13.00]   |7.00 [5.00, 9.00]     |7.00 [6.00, 10.00]   |10.00 [7.00, 12.00]   |8.00 [6.00, 10.00]   |0.2     |
|Patients admitted to intensive care unit (%)                                                  |No                                                                                        |2008 (89.7)           |0 (  NaN)              |160 ( 55.4)           |1525 ( 93.8)          |110 (100.0)           |97 (100.0)           |0 (  NaN)             |116 (100.0)          |80.4    |
|                                                                                              |Yes                                                                                       |230 (10.3)            |0 (  NaN)              |129 ( 44.6)           |101 (  6.2)           |0 (  0.0)             |0 (  0.0)            |0 (  NaN)             |0 (  0.0)            |        |
|Clinical status on ordinal scale (%)                                                          |2: Hospitalised without need for oxygen therapy (WHO score 4)                             |855 ( 7.5)            |142 ( 13.7)            |0 (  0.0)             |186 ( 11.5)           |35 ( 31.8)            |2 (  2.1)            |465 (  5.7)           |25 ( 21.6)           |0.1     |
|                                                                                              |3: Hospitalised with need for supplemental low-flow oxygen (WHO score 5)                  |7291 (64.0)           |564 ( 54.6)            |0 (  0.0)             |962 ( 59.4)           |75 ( 68.2)            |95 ( 97.9)           |5504 ( 67.7)          |91 ( 78.4)           |        |
|                                                                                              |4: Hospitalised with need for high-flow oxygen or non- invasive ventilation (WHO score 6) |2756 (24.2)           |216 ( 20.9)            |249 ( 86.2)           |370 ( 22.9)           |0 (  0.0)             |0 (  0.0)            |1921 ( 23.6)          |0 (  0.0)            |        |
|                                                                                              |5: Hospitalised with need for mechanical ventilation or ECMO (WHO score 7–9)              |492 ( 4.3)            |111 ( 10.7)            |40 ( 13.8)            |101 (  6.2)           |0 (  0.0)             |0 (  0.0)            |240 (  3.0)           |0 (  0.0)            |        |
|Comorbidities (%)                                                                             |No comorbidity                                                                            |5102 (44.8)           |151 ( 14.9)            |106 ( 36.7)           |370 ( 22.8)           |37 ( 33.6)            |41 ( 42.3)           |4353 ( 53.5)          |44 ( 37.9)           |0.2     |
|                                                                                              |One comorbidity                                                                           |3484 (30.6)           |288 ( 28.4)            |73 ( 25.3)            |477 ( 29.3)           |40 ( 36.4)            |22 ( 22.7)           |2538 ( 31.2)          |46 ( 39.7)           |        |
|                                                                                              |Multiple comorbidities                                                                    |2734 (24.0)           |546 ( 53.8)            |99 ( 34.3)            |761 ( 46.8)           |31 ( 28.2)            |32 ( 33.0)           |1239 ( 15.2)          |26 ( 22.4)           |        |
|                                                                                              |Immunocompromised                                                                         |63 ( 0.6)             |30 (  3.0)             |11 (  3.8)            |18 (  1.1)            |2 (  1.8)             |2 (  2.1)            |0 (  0.0)             |0 (  0.0)            |        |
|Dexamethasone and Tocilizumab (%)                                                             |No Dexamethasone, no Tocilizumab                                                          |1787 (15.7)           |991 ( 95.9)            |18 (  6.2)            |328 ( 20.3)           |94 ( 85.5)            |0 (  0.0)            |349 (  4.3)           |7 (  6.0)            |0.1     |
|                                                                                              |Dexamethasone but no Tocilizumab                                                          |6982 (61.3)           |42 (  4.1)             |270 ( 93.4)           |1291 ( 79.7)          |16 ( 14.5)            |97 (100.0)           |5157 ( 63.4)          |109 ( 94.0)          |        |
|                                                                                              |Dexamethasone and Tocilizumab                                                             |2591 (22.7)           |0 (  0.0)              |1 (  0.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |2590 ( 31.9)          |0 (  0.0)            |        |
|                                                                                              |Tocolizumab but no Dexamethasone                                                          |34 ( 0.3)             |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |34 (  0.4)            |0 (  0.0)            |        |
|Remdesivir (%)                                                                                |No Remdesivir                                                                             |8733 (76.6)           |518 ( 50.1)            |281 ( 97.2)           |1337 ( 82.2)          |110 (100.0)           |0 (  0.0)            |6474 ( 79.6)          |13 ( 11.2)           |0.0     |
|                                                                                              |Remdesivir                                                                                |2668 (23.4)           |515 ( 49.9)            |8 (  2.8)             |289 ( 17.8)           |0 (  0.0)             |97 (100.0)           |1656 ( 20.4)          |103 ( 88.8)          |        |
|C-reactive protein concentration, mg/L (median [IQR])                                         |                                                                                          |84.00 [41.00, 146.00] |125.75 [64.93, 190.50] |89.00 [50.00, 142.00] |65.00 [32.00, 122.90] |79.15 [39.72, 134.60] |68.00 [33.50, 84.00] |86.00 [43.00, 145.00] |4.90 [2.40, 9.75]    |4.2     |
|Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N]) (%) |Not seroconverted                                                                         |594 (33.9)            |0 (  NaN)              |7 (  5.0)             |0 (  NaN)             |0 (  NaN)             |0 (  NaN)            |587 ( 36.4)           |0 (  NaN)            |84.6    |
|                                                                                              |Seroconverted                                                                             |1157 (66.1)           |0 (  NaN)              |132 ( 95.0)           |0 (  NaN)             |0 (  NaN)             |0 (  NaN)            |1025 ( 63.6)          |0 (  NaN)            |        |
|Patients with undetectable viral load (%)                                                     |Detectable viral load                                                                     |9331 (97.4)           |526 ( 75.9)            |116 ( 86.6)           |1494 ( 97.2)          |0 (  NaN)             |0 (  NaN)            |7195 ( 99.8)          |0 (  NaN)            |16.0    |
|                                                                                              |Undetectable viral load                                                                   |246 ( 2.6)            |167 ( 24.1)            |18 ( 13.4)            |43 (  2.8)            |0 (  NaN)             |0 (  NaN)            |18 (  0.2)            |0 (  NaN)            |        |


# Missing data table, by trial

```r
table(df_tot$mort_28,useNA = "always")
```

```
## 
##    0    1 <NA> 
## 9731 1376  294
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

|                          |level                                     |Overall               |ACTT2                  |Bari-Solidact         |COV-BARRIER           |COVINIB               |Ghazaeian            |RECOVERY              |TOFACOV              |Missing |
|:-------------------------|:-----------------------------------------|:---------------------|:----------------------|:---------------------|:---------------------|:---------------------|:--------------------|:---------------------|:--------------------|:-------|
|n                         |                                          |11401                 |1033                   |289                   |1626                  |110                   |97                   |8130                  |116                  |        |
|age (median [IQR])        |                                          |58.00 [47.00, 68.00]  |56.00 [43.00, 67.00]   |60.00 [50.00, 69.00]  |58.00 [48.00, 68.00]  |55.00 [47.25, 62.00]  |51.00 [37.00, 64.00] |58.00 [47.00, 69.00]  |58.00 [50.75, 66.25] |0.0     |
|sex (%)                   |1                                         |218 ( 1.9)            |0 (  0.0)              |218 ( 75.4)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |0.0     |
|                          |2                                         |71 ( 0.6)             |0 (  0.0)              |71 ( 24.6)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |F                                         |381 ( 3.3)            |381 ( 36.9)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |female                                    |3492 (30.6)           |0 (  0.0)              |0 (  0.0)             |608 ( 37.4)           |34 ( 30.9)            |50 ( 51.5)           |2764 ( 34.0)          |36 ( 31.0)           |        |
|                          |M                                         |652 ( 5.7)            |652 ( 63.1)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |male                                      |6587 (57.8)           |0 (  0.0)              |0 (  0.0)             |1018 ( 62.6)          |76 ( 69.1)            |47 ( 48.5)           |5366 ( 66.0)          |80 ( 69.0)           |        |
|ethn (%)                  |                                          |34 ( 0.3)             |0 (  0.0)              |0 (  0.0)             |34 (  2.1)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |5.1     |
|                          |AMERICAN INDIAN OR ALASKA NATIVE          |358 ( 3.1)            |10 (  1.0)             |0 (  0.0)             |348 ( 21.4)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |Asian                                     |1 ( 0.0)              |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |1 (  0.9)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |ASIAN                                     |763 ( 6.7)            |101 (  9.8)            |0 (  0.0)             |175 ( 10.8)           |0 (  0.0)             |0 (  0.0)            |487 (  6.0)           |0 (  0.0)            |        |
|                          |BLACK OR AFRICAN AMERICAN                 |453 ( 4.0)            |156 ( 15.1)            |0 (  0.0)             |77 (  4.7)            |0 (  0.0)             |0 (  0.0)            |220 (  2.7)           |0 (  0.0)            |        |
|                          |caucasian                                 |116 ( 1.0)            |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |116 (100.0)          |        |
|                          |HISPANIC OR LATINO                        |246 ( 2.2)            |246 ( 23.8)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |Latino                                    |19 ( 0.2)             |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |19 ( 17.3)            |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |MIXED                                     |74 ( 0.6)             |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |74 (  0.9)            |0 (  0.0)            |        |
|                          |MULTIPLE                                  |5 ( 0.0)              |0 (  0.0)              |0 (  0.0)             |5 (  0.3)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER |16 ( 0.1)             |11 (  1.1)             |0 (  0.0)             |5 (  0.3)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |OTHER                                     |133 ( 1.2)            |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |133 (  1.6)           |0 (  0.0)            |        |
|                          |Persian/Mazani                            |97 ( 0.9)             |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |97 (100.0)           |0 (  0.0)             |0 (  0.0)            |        |
|                          |UNKNOWN                                   |419 ( 3.7)            |13 (  1.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |406 (  5.0)           |0 (  0.0)            |        |
|                          |White                                     |90 ( 0.8)             |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |90 ( 81.8)            |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |WHITE                                     |7990 (70.1)           |496 ( 48.0)            |0 (  0.0)             |982 ( 60.4)           |0 (  0.0)             |0 (  0.0)            |6512 ( 80.1)          |0 (  0.0)            |        |
|                          |NA                                        |587 ( 5.1)            |0 (  0.0)              |289 (100.0)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |298 (  3.7)           |0 (  0.0)            |        |
|country (%)               |ARG                                       |229 ( 2.0)            |0 (  0.0)              |0 (  0.0)             |229 ( 14.1)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |0.0     |
|                          |Asia                                      |67 ( 0.6)             |67 (  6.5)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |AUSTRIA                                   |6 ( 0.1)              |0 (  0.0)              |6 (  2.1)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |BELGIUM                                   |10 ( 0.1)             |0 (  0.0)              |10 (  3.5)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |BRA                                       |366 ( 3.2)            |0 (  0.0)              |0 (  0.0)             |366 ( 22.5)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |DEU                                       |20 ( 0.2)             |0 (  0.0)              |0 (  0.0)             |20 (  1.2)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |ESP                                       |87 ( 0.8)             |0 (  0.0)              |0 (  0.0)             |87 (  5.4)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |Europe                                    |13 ( 0.1)             |13 (  1.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |FRANCE                                    |94 ( 0.8)             |0 (  0.0)              |94 ( 32.5)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |GBR                                       |8141 (71.4)           |0 (  0.0)              |0 (  0.0)             |11 (  0.7)            |0 (  0.0)             |0 (  0.0)            |8130 (100.0)          |0 (  0.0)            |        |
|                          |GERMANY                                   |1 ( 0.0)              |0 (  0.0)              |1 (  0.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |IND                                       |50 ( 0.4)             |0 (  0.0)              |0 (  0.0)             |50 (  3.1)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |Iran                                      |97 ( 0.9)             |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |97 (100.0)           |0 (  0.0)             |0 (  0.0)            |        |
|                          |IRELAND                                   |9 ( 0.1)              |0 (  0.0)              |9 (  3.1)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |ITA                                       |25 ( 0.2)             |0 (  0.0)              |0 (  0.0)             |25 (  1.5)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |Italy                                     |116 ( 1.0)            |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |116 (100.0)          |        |
|                          |ITALY                                     |25 ( 0.2)             |0 (  0.0)              |25 (  8.7)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |JPN                                       |38 ( 0.3)             |0 (  0.0)              |0 (  0.0)             |38 (  2.3)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |KOR                                       |36 ( 0.3)             |0 (  0.0)              |0 (  0.0)             |36 (  2.2)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |LUXEMBOURG                                |1 ( 0.0)              |0 (  0.0)              |1 (  0.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |MEX                                       |312 ( 2.7)            |0 (  0.0)              |0 (  0.0)             |312 ( 19.2)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |North America                             |953 ( 8.4)            |953 ( 92.3)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |NORWAY                                    |127 ( 1.1)            |0 (  0.0)              |127 ( 43.9)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |PORTUGAL                                  |3 ( 0.0)              |0 (  0.0)              |3 (  1.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |RUS                                       |112 ( 1.0)            |0 (  0.0)              |0 (  0.0)             |112 (  6.9)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |Spain                                     |110 ( 1.0)            |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |110 (100.0)           |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |SPAIN                                     |13 ( 0.1)             |0 (  0.0)              |13 (  4.5)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |USA                                       |340 ( 3.0)            |0 (  0.0)              |0 (  0.0)             |340 ( 20.9)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|vacc (%)                  |0                                         |6146 (53.9)           |1033 (100.0)           |182 ( 63.0)           |0 (  0.0)             |108 ( 98.2)           |0 (  0.0)            |4710 ( 57.9)          |113 ( 97.4)          |15.2    |
|                          |1                                         |3527 (30.9)           |0 (  0.0)              |102 ( 35.3)           |0 (  0.0)             |2 (  1.8)             |0 (  0.0)            |3420 ( 42.1)          |3 (  2.6)            |        |
|                          |NA                                        |1728 (15.2)           |0 (  0.0)              |5 (  1.7)             |1626 (100.0)          |0 (  0.0)             |97 (100.0)           |0 (  0.0)             |0 (  0.0)            |        |
|sympdur (median [IQR])    |                                          |10.00 [7.00, 12.00]   |8.00 [5.00, 10.00]     |9.00 [7.00, 11.00]    |10.00 [8.00, 13.00]   |7.00 [5.00, 9.00]     |7.00 [6.00, 10.00]   |10.00 [7.00, 12.00]   |8.00 [6.00, 10.00]   |0.2     |
|icu (%)                   |0                                         |2008 (17.6)           |0 (  0.0)              |160 ( 55.4)           |1525 ( 93.8)          |110 (100.0)           |97 (100.0)           |0 (  0.0)             |116 (100.0)          |80.4    |
|                          |1                                         |230 ( 2.0)            |0 (  0.0)              |129 ( 44.6)           |101 (  6.2)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |NA                                        |9163 (80.4)           |1033 (100.0)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |8130 (100.0)          |0 (  0.0)            |        |
|clinstatus_baseline (%)   |2                                         |855 ( 7.5)            |142 ( 13.7)            |0 (  0.0)             |186 ( 11.4)           |35 ( 31.8)            |2 (  2.1)            |465 (  5.7)           |25 ( 21.6)           |0.1     |
|                          |3                                         |7291 (64.0)           |564 ( 54.6)            |0 (  0.0)             |962 ( 59.2)           |75 ( 68.2)            |95 ( 97.9)           |5504 ( 67.7)          |91 ( 78.4)           |        |
|                          |4                                         |2756 (24.2)           |216 ( 20.9)            |249 ( 86.2)           |370 ( 22.8)           |0 (  0.0)             |0 (  0.0)            |1921 ( 23.6)          |0 (  0.0)            |        |
|                          |5                                         |492 ( 4.3)            |111 ( 10.7)            |40 ( 13.8)            |101 (  6.2)           |0 (  0.0)             |0 (  0.0)            |240 (  3.0)           |0 (  0.0)            |        |
|                          |NA                                        |7 ( 0.1)              |0 (  0.0)              |0 (  0.0)             |7 (  0.4)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|comorb_cat (%)            |1                                         |5102 (44.8)           |151 ( 14.6)            |106 ( 36.7)           |370 ( 22.8)           |37 ( 33.6)            |41 ( 42.3)           |4353 ( 53.5)          |44 ( 37.9)           |0.2     |
|                          |2                                         |3484 (30.6)           |288 ( 27.9)            |73 ( 25.3)            |477 ( 29.3)           |40 ( 36.4)            |22 ( 22.7)           |2538 ( 31.2)          |46 ( 39.7)           |        |
|                          |3                                         |2734 (24.0)           |546 ( 52.9)            |99 ( 34.3)            |761 ( 46.8)           |31 ( 28.2)            |32 ( 33.0)           |1239 ( 15.2)          |26 ( 22.4)           |        |
|                          |4                                         |63 ( 0.6)             |30 (  2.9)             |11 (  3.8)            |18 (  1.1)            |2 (  1.8)             |2 (  2.1)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |NA                                        |18 ( 0.2)             |18 (  1.7)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|comed_cat (%)             |1                                         |1787 (15.7)           |991 ( 95.9)            |18 (  6.2)            |328 ( 20.2)           |94 ( 85.5)            |0 (  0.0)            |349 (  4.3)           |7 (  6.0)            |0.1     |
|                          |2                                         |2591 (22.7)           |0 (  0.0)              |1 (  0.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |2590 ( 31.9)          |0 (  0.0)            |        |
|                          |3                                         |6982 (61.2)           |42 (  4.1)             |270 ( 93.4)           |1291 ( 79.4)          |16 ( 14.5)            |97 (100.0)           |5157 ( 63.4)          |109 ( 94.0)          |        |
|                          |4                                         |34 ( 0.3)             |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |34 (  0.4)            |0 (  0.0)            |        |
|                          |NA                                        |7 ( 0.1)              |0 (  0.0)              |0 (  0.0)             |7 (  0.4)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|comed_rdv (%)             |0                                         |8733 (76.6)           |518 ( 50.1)            |281 ( 97.2)           |1337 ( 82.2)          |110 (100.0)           |0 (  0.0)            |6474 ( 79.6)          |13 ( 11.2)           |0.0     |
|                          |1                                         |2668 (23.4)           |515 ( 49.9)            |8 (  2.8)             |289 ( 17.8)           |0 (  0.0)             |97 (100.0)           |1656 ( 20.4)          |103 ( 88.8)          |        |
|crp (median [IQR])        |                                          |84.00 [41.00, 146.00] |125.75 [64.93, 190.50] |89.00 [50.00, 142.00] |65.00 [32.00, 122.90] |79.15 [39.72, 134.60] |68.00 [33.50, 84.00] |86.00 [43.00, 145.00] |4.90 [2.40, 9.75]    |4.2     |
|sero (%)                  |0                                         |594 ( 5.2)            |0 (  0.0)              |7 (  2.4)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |587 (  7.2)           |0 (  0.0)            |84.6    |
|                          |1                                         |1157 (10.1)           |0 (  0.0)              |132 ( 45.7)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |1025 ( 12.6)          |0 (  0.0)            |        |
|                          |NA                                        |9650 (84.6)           |1033 (100.0)           |150 ( 51.9)           |1626 (100.0)          |110 (100.0)           |97 (100.0)           |6518 ( 80.2)          |116 (100.0)          |        |
|vl_baseline (%)           |0                                         |9331 (81.8)           |526 ( 50.9)            |116 ( 40.1)           |1494 ( 91.9)          |0 (  0.0)             |0 (  0.0)            |7195 ( 88.5)          |0 (  0.0)            |16.0    |
|                          |1                                         |246 ( 2.2)            |167 ( 16.2)            |18 (  6.2)            |43 (  2.6)            |0 (  0.0)             |0 (  0.0)            |18 (  0.2)            |0 (  0.0)            |        |
|                          |NA                                        |1824 (16.0)           |340 ( 32.9)            |155 ( 53.6)           |89 (  5.5)            |110 (100.0)           |97 (100.0)           |917 ( 11.3)           |116 (100.0)          |        |
|variant (%)               |Delta                                     |21 ( 0.2)             |0 (  0.0)              |21 (  7.3)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |99.7    |
|                          |Omicron                                   |10 ( 0.1)             |0 (  0.0)              |10 (  3.5)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |PRESENCE OF MUTATIONS E484Q AND L452R     |1 ( 0.0)              |0 (  0.0)              |1 (  0.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |NA                                        |11369 (99.7)          |1033 (100.0)           |257 ( 88.9)           |1626 (100.0)          |110 (100.0)           |97 (100.0)           |8130 (100.0)          |116 (100.0)          |        |
|mort_28 (%)               |0                                         |9731 (85.4)           |925 ( 89.5)            |241 ( 83.4)           |1312 ( 80.7)          |105 ( 95.5)           |90 ( 92.8)           |6943 ( 85.4)          |115 ( 99.1)          |2.6     |
|                          |1                                         |1376 (12.1)           |61 (  5.9)             |36 ( 12.5)            |211 ( 13.0)           |2 (  1.8)             |7 (  7.2)            |1058 ( 13.0)          |1 (  0.9)            |        |
|                          |NA                                        |294 ( 2.6)            |47 (  4.5)             |12 (  4.2)            |103 (  6.3)           |3 (  2.7)             |0 (  0.0)            |129 (  1.6)           |0 (  0.0)            |        |
|mort_60 (%)               |0                                         |9658 (84.7)           |925 ( 89.5)            |231 ( 79.9)           |1249 ( 76.8)          |105 ( 95.5)           |90 ( 92.8)           |6943 ( 85.4)          |115 ( 99.1)          |2.8     |
|                          |1                                         |1424 (12.5)           |61 (  5.9)             |46 ( 15.9)            |249 ( 15.3)           |2 (  1.8)             |7 (  7.2)            |1058 ( 13.0)          |1 (  0.9)            |        |
|                          |NA                                        |319 ( 2.8)            |47 (  4.5)             |12 (  4.2)            |128 (  7.9)           |3 (  2.7)             |0 (  0.0)            |129 (  1.6)           |0 (  0.0)            |        |
|death_reached (%)         |0                                         |9847 (86.4)           |972 ( 94.1)            |242 ( 83.7)           |1377 ( 84.7)          |108 ( 98.2)           |90 ( 92.8)           |6943 ( 85.4)          |115 ( 99.1)          |1.1     |
|                          |1                                         |1425 (12.5)           |61 (  5.9)             |47 ( 16.3)            |249 ( 15.3)           |2 (  1.8)             |7 (  7.2)            |1058 ( 13.0)          |1 (  0.9)            |        |
|                          |NA                                        |129 ( 1.1)            |0 (  0.0)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |129 (  1.6)           |0 (  0.0)            |        |
|new_mv_28 (%)             |0                                         |8870 (77.8)           |768 ( 74.3)            |174 ( 60.2)           |1141 ( 70.2)          |97 ( 88.2)            |90 ( 92.8)           |6487 ( 79.8)          |113 ( 97.4)          |16.9    |
|                          |1                                         |608 ( 5.3)            |91 (  8.8)             |37 ( 12.8)            |138 (  8.5)           |8 (  7.3)             |0 (  0.0)            |332 (  4.1)           |2 (  1.7)            |        |
|                          |NA                                        |1923 (16.9)           |174 ( 16.8)            |78 ( 27.0)            |347 ( 21.3)           |5 (  4.5)             |7 (  7.2)            |1311 ( 16.1)          |1 (  0.9)            |        |
|new_mvd_28 (%)            |0                                         |9133 (80.1)           |847 ( 82.0)            |205 ( 70.9)           |1142 ( 70.2)          |97 ( 88.2)            |90 ( 92.8)           |6639 ( 81.7)          |113 ( 97.4)          |2.0     |
|                          |1                                         |2036 (17.9)           |152 ( 14.7)            |73 ( 25.3)            |401 ( 24.7)           |10 (  9.1)            |7 (  7.2)            |1390 ( 17.1)          |3 (  2.6)            |        |
|                          |NA                                        |232 ( 2.0)            |34 (  3.3)             |11 (  3.8)            |83 (  5.1)            |3 (  2.7)             |0 (  0.0)            |101 (  1.2)           |0 (  0.0)            |        |
|clinstatus_28_imp (%)     |1                                         |8931 (78.3)           |824 ( 79.8)            |187 ( 64.7)           |1212 ( 74.5)          |103 ( 93.6)           |90 ( 92.8)           |6405 ( 78.8)          |110 ( 94.8)          |0.0     |
|                          |2                                         |82 ( 0.7)             |19 (  1.8)             |9 (  3.1)             |24 (  1.5)            |0 (  0.0)             |0 (  0.0)            |30 (  0.4)            |0 (  0.0)            |        |
|                          |3                                         |402 ( 3.5)            |39 (  3.8)             |18 (  6.2)            |66 (  4.1)            |3 (  2.7)             |0 (  0.0)            |275 (  3.4)           |1 (  0.9)            |        |
|                          |4                                         |326 ( 2.9)            |23 (  2.2)             |13 (  4.5)            |23 (  1.4)            |0 (  0.0)             |0 (  0.0)            |264 (  3.2)           |3 (  2.6)            |        |
|                          |5                                         |287 ( 2.5)            |67 (  6.5)             |26 (  9.0)            |90 (  5.5)            |2 (  1.8)             |0 (  0.0)            |101 (  1.2)           |1 (  0.9)            |        |
|                          |6                                         |1373 (12.0)           |61 (  5.9)             |36 ( 12.5)            |211 ( 13.0)           |2 (  1.8)             |7 (  7.2)            |1055 ( 13.0)          |1 (  0.9)            |        |
|discharge_reached (%)     |0                                         |2394 (21.0)           |194 ( 18.8)            |96 ( 33.2)            |406 ( 25.0)           |7 (  6.4)             |7 (  7.2)            |1678 ( 20.6)          |6 (  5.2)            |0.0     |
|                          |1                                         |9007 (79.0)           |839 ( 81.2)            |193 ( 66.8)           |1220 ( 75.0)          |103 ( 93.6)           |90 ( 92.8)           |6452 ( 79.4)          |110 ( 94.8)          |        |
|discharge_reached_sus (%) |0                                         |2404 (21.1)           |194 ( 18.8)            |98 ( 33.9)            |414 ( 25.5)           |7 (  6.4)             |7 (  7.2)            |1678 ( 20.6)          |6 (  5.2)            |0.0     |
|                          |1                                         |8997 (78.9)           |839 ( 81.2)            |191 ( 66.1)           |1212 ( 74.5)          |103 ( 93.6)           |90 ( 92.8)           |6452 ( 79.4)          |110 ( 94.8)          |        |
|ae_28 (%)                 |0                                         |8512 (74.7)           |562 ( 54.4)            |165 ( 57.1)           |1011 ( 62.2)          |85 ( 77.3)            |96 ( 99.0)           |6499 ( 79.9)          |94 ( 81.0)           |13.8    |
|                          |1                                         |1317 (11.6)           |410 ( 39.7)            |88 ( 30.4)            |314 ( 19.3)           |23 ( 20.9)            |1 (  1.0)            |459 (  5.6)           |22 ( 19.0)           |        |
|                          |NA                                        |1572 (13.8)           |61 (  5.9)             |36 ( 12.5)            |301 ( 18.5)           |2 (  1.8)             |0 (  0.0)            |1172 ( 14.4)          |0 (  0.0)            |        |
|ae_28_sev (%)             |0                                         |8512 (74.7)           |562 ( 54.4)            |165 ( 57.1)           |1011 ( 62.2)          |85 ( 77.3)            |96 ( 99.0)           |6499 ( 79.9)          |94 ( 81.0)           |13.8    |
|                          |1                                         |857 ( 7.5)            |198 ( 19.2)            |49 ( 17.0)            |154 (  9.5)           |14 ( 12.7)            |1 (  1.0)            |419 (  5.2)           |22 ( 19.0)           |        |
|                          |2                                         |206 ( 1.8)            |77 (  7.5)             |15 (  5.2)            |77 (  4.7)            |3 (  2.7)             |0 (  0.0)            |34 (  0.4)            |0 (  0.0)            |        |
|                          |3                                         |90 ( 0.8)             |38 (  3.7)             |11 (  3.8)            |36 (  2.2)            |1 (  0.9)             |0 (  0.0)            |4 (  0.0)             |0 (  0.0)            |        |
|                          |4                                         |56 ( 0.5)             |36 (  3.5)             |4 (  1.4)             |12 (  0.7)            |2 (  1.8)             |0 (  0.0)            |2 (  0.0)             |0 (  0.0)            |        |
|                          |5                                         |35 ( 0.3)             |15 (  1.5)             |2 (  0.7)             |17 (  1.0)            |1 (  0.9)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |6                                         |19 ( 0.2)             |7 (  0.7)              |3 (  1.0)             |7 (  0.4)             |2 (  1.8)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |7                                         |17 ( 0.1)             |13 (  1.3)             |1 (  0.3)             |3 (  0.2)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |8                                         |8 ( 0.1)              |7 (  0.7)              |1 (  0.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |9                                         |11 ( 0.1)             |6 (  0.6)              |1 (  0.3)             |4 (  0.2)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |10                                        |6 ( 0.1)              |4 (  0.4)              |1 (  0.3)             |1 (  0.1)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |11                                        |3 ( 0.0)              |3 (  0.3)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |12                                        |2 ( 0.0)              |1 (  0.1)              |0 (  0.0)             |1 (  0.1)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |13                                        |3 ( 0.0)              |1 (  0.1)              |0 (  0.0)             |2 (  0.1)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |16                                        |1 ( 0.0)              |1 (  0.1)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |20                                        |1 ( 0.0)              |1 (  0.1)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |22                                        |1 ( 0.0)              |1 (  0.1)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |26                                        |1 ( 0.0)              |1 (  0.1)              |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)            |        |
|                          |NA                                        |1572 (13.8)           |61 (  5.9)             |36 ( 12.5)            |301 ( 18.5)           |2 (  1.8)             |0 (  0.0)            |1172 ( 14.4)          |0 (  0.0)            |        |
|vir_clear_5 (%)           |0                                         |8739 (76.7)           |496 ( 48.0)            |91 ( 31.5)            |734 ( 45.1)           |0 (  0.0)             |0 (  0.0)            |7418 ( 91.2)          |0 (  0.0)            |17.9    |
|                          |1                                         |624 ( 5.5)            |251 ( 24.3)            |30 ( 10.4)            |244 ( 15.0)           |0 (  0.0)             |0 (  0.0)            |99 (  1.2)            |0 (  0.0)            |        |
|                          |NA                                        |2038 (17.9)           |286 ( 27.7)            |168 ( 58.1)           |648 ( 39.9)           |110 (100.0)           |97 (100.0)           |613 (  7.5)           |116 (100.0)          |        |
|vir_clear_10 (%)          |0                                         |8754 (76.8)           |456 ( 44.1)            |69 ( 23.9)            |654 ( 40.2)           |0 (  0.0)             |0 (  0.0)            |7575 ( 93.2)          |0 (  0.0)            |15.3    |
|                          |1                                         |898 ( 7.9)            |299 ( 28.9)            |58 ( 20.1)            |406 ( 25.0)           |0 (  0.0)             |0 (  0.0)            |135 (  1.7)           |0 (  0.0)            |        |
|                          |NA                                        |1749 (15.3)           |278 ( 26.9)            |162 ( 56.1)           |566 ( 34.8)           |110 (100.0)           |97 (100.0)           |420 (  5.2)           |116 (100.0)          |        |
|vir_clear_15 (%)          |0                                         |8672 (76.1)           |398 ( 38.5)            |57 ( 19.7)            |584 ( 35.9)           |0 (  0.0)             |0 (  0.0)            |7633 ( 93.9)          |0 (  0.0)            |14.3    |
|                          |1                                         |1094 ( 9.6)           |361 ( 34.9)            |71 ( 24.6)            |512 ( 31.5)           |0 (  0.0)             |0 (  0.0)            |150 (  1.8)           |0 (  0.0)            |        |
|                          |NA                                        |1635 (14.3)           |274 ( 26.5)            |161 ( 55.7)           |530 ( 32.6)           |110 (100.0)           |97 (100.0)           |347 (  4.3)           |116 (100.0)          |        |
