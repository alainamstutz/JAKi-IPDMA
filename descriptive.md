---
title: "descriptive"
author: "A.Amstutz"
date: "2023-12-29"
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
library(data.table)
library(here)
library(kableExtra)
```

# Load entire dataset (from one-stage.Rmd)
## Make sure to run one-stage before

```r
df_tot <- readRDS("df_tot.RData") # without Murugesan
df_tot_Muru <- readRDS("df_tot_Muru.RData") # with Murugesan
```

# Baseline characteristics, reformatting

```r
vars.list <- c("trt", "trial", "age", "sex", "ethn", "country", "vacc", "sympdur", "icu", "clinstatus_baseline", "comorb_cat", "comed_cat", "comed_rdv", "crp", "sero", "vl_baseline", "variant")

df_baseline <- df_tot_Muru[,colnames(df_tot_Muru)%in%vars.list]
df_baseline <- df_baseline[,match(vars.list,colnames(df_baseline))]

colnames(df_baseline) <- vars.list <- c("ARM","Trial","Age, years", "Sex", "Ethnicity", "Country", "Vaccinated", "Time from symptom onset to randomisation, days", "Patients admitted to intensive care unit","Clinical status on ordinal scale", "Comorbidities", "Comedication", "Remdesivir","C-reactive protein concentration, mg/L", "Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N])", "Patients with detectable viral load", "SARS CoV-2 variant")

char_vars <- c("Trial","Sex", "Ethnicity", "Country", "Vaccinated","Patients admitted to intensive care unit","Clinical status on ordinal scale","Comorbidities", "Comedication", "Remdesivir","Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N])", "Patients with detectable viral load", "SARS CoV-2 variant")

# Convert character variables to factors
df_baseline <- df_baseline %>%
  mutate(across(all_of(char_vars), factor))

# Sex
df_baseline <- df_baseline %>%
  mutate(Sex = case_when(Sex == "male" | Sex == "M" | Sex == "2" ~ "Male",
                         Sex == "female" | Sex == "F" | Sex == "1" ~ "Female"))

# Country
# table(df_baseline$Country, useNA = "always")
df_baseline <- df_baseline %>%
  mutate(Country = case_when(Country == "NORWAY" ~ "Norway",
                             Country == "FRANCE" ~ "France",
                             Country == "ITA" | Country == "ITALY" ~ "Italy",
                             Country == "GERMANY" | Country == "DEU" ~ "Germany",
                             Country == "SPAIN" | Country == "ESP" ~ "Spain",
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
                             Country == "IND" ~ "India"
                             ))

# Vaccinated
df_baseline <- df_baseline %>%
  mutate(Vaccinated = case_when(Vaccinated == "1" ~ "Any SARS CoV-2 vaccine",
                                Vaccinated == "0" ~ "No vaccine"))
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
# Comedication
df_baseline <- df_baseline %>%
  mutate(Comedication = case_when(Comedication == "1" ~ "No Dexamethasone, no Tocilizumab",
                                   Comedication == "2" ~ "Dexamethasone and Tocilizumab",
                                   Comedication == "3" ~ "Dexamethasone but no Tocilizumab"))
# Remdesivir
df_baseline <- df_baseline %>%
  mutate(Remdesivir = case_when(Remdesivir == "1" ~ "Remdesivir",
                                Remdesivir == "0" ~ "No Remdesivir"))
# Seroconversion
df_baseline <- df_baseline %>%
  mutate(`Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N])` = case_when(`Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N])` == "1" ~ "Seroconverted",
                                `Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N])` == "0" ~ "Not seroconverted"))
# Anticoagulation
df_baseline <- df_baseline %>%
  mutate(`Patients with detectable viral load` = case_when(`Patients with detectable viral load` == "1" ~ "Undetectable viral load",
                                `Patients with detectable viral load` == "0" ~ "Detectable viral load"))

# Replace the strata variable labels
df_baseline$ARM <- ifelse(df_baseline$ARM == 0, "No JAK inhibitor", "JAK inhibitor")

# Create ordinal factors
# df_baseline$regcursat <- ordered(df_baseline$regcursat, 
#                                   levels = c("Very dissatisfied", "Somewhat dissatisfied", 
#                                              "Neutral", "Somewhat satisfied", "Very satisfied"))
```

## Baseline table, by treatment arm

```r
# all participants, by arm
table_baseline <- CreateTableOne(data = df_baseline, vars = vars.list[!vars.list %in% c("ARM")], strata = "ARM", includeNA = T, test = F, addOverall = TRUE)
capture.output(table_baseline <- print(table_baseline, nonnormal = vars.list,catDigits = 1,SMD = TRUE,showAllLevels = TRUE,test = TRUE,printToggle = FALSE,missing = TRUE))
```

```
## character(0)
```

```r
# print
kable(table_baseline, format = "markdown", table.attr = 'class="table"', caption = "Baseline characteristics, by arm") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```



Table: Baseline characteristics, by arm

|                                                                                              |level                                     |Overall               |JAK inhibitor         |No JAK inhibitor      |Missing |
|:---------------------------------------------------------------------------------------------|:-----------------------------------------|:---------------------|:---------------------|:---------------------|:-------|
|n                                                                                             |                                          |3371                  |1684                  |1687                  |        |
|Trial (%)                                                                                     |ACTT2                                     |1033 (30.6)           |515 (30.6)            |518 (30.7)            |0.0     |
|                                                                                              |Bari-Solidact                             |289 ( 8.6)            |145 ( 8.6)            |144 ( 8.5)            |        |
|                                                                                              |COV-BARRIER                               |1626 (48.2)           |815 (48.4)            |811 (48.1)            |        |
|                                                                                              |COVINIB                                   |110 ( 3.3)            |55 ( 3.3)             |55 ( 3.3)             |        |
|                                                                                              |Ghazaeian                                 |97 ( 2.9)             |46 ( 2.7)             |51 ( 3.0)             |        |
|                                                                                              |Murugesan                                 |100 ( 3.0)            |50 ( 3.0)             |50 ( 3.0)             |        |
|                                                                                              |TOFACOV                                   |116 ( 3.4)            |58 ( 3.4)             |58 ( 3.4)             |        |
|Age, years (median [IQR])                                                                     |                                          |57.00 [46.00, 67.00]  |57.00 [45.75, 67.00]  |57.00 [46.00, 67.00]  |0.0     |
|Sex (%)                                                                                       |Female                                    |1353 (40.1)           |679 (40.3)            |674 (40.0)            |0.0     |
|                                                                                              |Male                                      |2018 (59.9)           |1005 (59.7)           |1013 (60.0)           |        |
|Ethnicity (%)                                                                                 |                                          |34 ( 1.0)             |13 ( 0.8)             |21 ( 1.2)             |8.6     |
|                                                                                              |AMERICAN INDIAN OR ALASKA NATIVE          |358 (10.6)            |165 ( 9.8)            |193 (11.4)            |        |
|                                                                                              |Asian                                     |1 ( 0.0)              |0 ( 0.0)              |1 ( 0.1)              |        |
|                                                                                              |ASIAN                                     |276 ( 8.2)            |129 ( 7.7)            |147 ( 8.7)            |        |
|                                                                                              |BLACK OR AFRICAN AMERICAN                 |233 ( 6.9)            |117 ( 6.9)            |116 ( 6.9)            |        |
|                                                                                              |caucasian                                 |116 ( 3.4)            |58 ( 3.4)             |58 ( 3.4)             |        |
|                                                                                              |HISPANIC OR LATINO                        |246 ( 7.3)            |125 ( 7.4)            |121 ( 7.2)            |        |
|                                                                                              |Indian                                    |100 ( 3.0)            |50 ( 3.0)             |50 ( 3.0)             |        |
|                                                                                              |Latino                                    |19 ( 0.6)             |12 ( 0.7)             |7 ( 0.4)              |        |
|                                                                                              |MULTIPLE                                  |5 ( 0.1)              |4 ( 0.2)              |1 ( 0.1)              |        |
|                                                                                              |NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER |16 ( 0.5)             |7 ( 0.4)              |9 ( 0.5)              |        |
|                                                                                              |Persian/Mazani                            |97 ( 2.9)             |46 ( 2.7)             |51 ( 3.0)             |        |
|                                                                                              |UNKNOWN                                   |13 ( 0.4)             |7 ( 0.4)              |6 ( 0.4)              |        |
|                                                                                              |White                                     |90 ( 2.7)             |43 ( 2.6)             |47 ( 2.8)             |        |
|                                                                                              |WHITE                                     |1478 (43.8)           |763 (45.3)            |715 (42.4)            |        |
|                                                                                              |NA                                        |289 ( 8.6)            |145 ( 8.6)            |144 ( 8.5)            |        |
|Country (%)                                                                                   |Argentina                                 |229 ( 6.8)            |119 ( 7.1)            |110 ( 6.5)            |53.3    |
|                                                                                              |Austria                                   |6 ( 0.2)              |2 ( 0.1)              |4 ( 0.2)              |        |
|                                                                                              |Belgium                                   |10 ( 0.3)             |4 ( 0.2)              |6 ( 0.4)              |        |
|                                                                                              |Brasil                                    |366 (10.9)            |187 (11.1)            |179 (10.6)            |        |
|                                                                                              |France                                    |94 ( 2.8)             |50 ( 3.0)             |44 ( 2.6)             |        |
|                                                                                              |Germany                                   |21 ( 0.6)             |10 ( 0.6)             |11 ( 0.7)             |        |
|                                                                                              |India                                     |50 ( 1.5)             |19 ( 1.1)             |31 ( 1.8)             |        |
|                                                                                              |Ireland                                   |9 ( 0.3)              |5 ( 0.3)              |4 ( 0.2)              |        |
|                                                                                              |Italy                                     |50 ( 1.5)             |28 ( 1.7)             |22 ( 1.3)             |        |
|                                                                                              |Japan                                     |38 ( 1.1)             |19 ( 1.1)             |19 ( 1.1)             |        |
|                                                                                              |Luxembourg                                |1 ( 0.0)              |1 ( 0.1)              |0 ( 0.0)              |        |
|                                                                                              |Mexico                                    |312 ( 9.3)            |152 ( 9.0)            |160 ( 9.5)            |        |
|                                                                                              |Norway                                    |127 ( 3.8)            |61 ( 3.6)             |66 ( 3.9)             |        |
|                                                                                              |Portugal                                  |3 ( 0.1)              |2 ( 0.1)              |1 ( 0.1)              |        |
|                                                                                              |Russia                                    |112 ( 3.3)            |58 ( 3.4)             |54 ( 3.2)             |        |
|                                                                                              |South Korea                               |36 ( 1.1)             |16 ( 1.0)             |20 ( 1.2)             |        |
|                                                                                              |Spain                                     |100 ( 3.0)            |51 ( 3.0)             |49 ( 2.9)             |        |
|                                                                                              |United Kingdom                            |11 ( 0.3)             |4 ( 0.2)              |7 ( 0.4)              |        |
|                                                                                              |NA                                        |1796 (53.3)           |896 (53.2)            |900 (53.3)            |        |
|Vaccinated (%)                                                                                |Any SARS CoV-2 vaccine                    |107 ( 3.2)            |54 ( 3.2)             |53 ( 3.1)             |51.3    |
|                                                                                              |No vaccine                                |1536 (45.6)           |766 (45.5)            |770 (45.6)            |        |
|                                                                                              |NA                                        |1728 (51.3)           |864 (51.3)            |864 (51.2)            |        |
|Time from symptom onset to randomisation, days (median [IQR])                                 |                                          |9.00 [6.00, 12.00]    |9.00 [6.00, 12.00]    |9.00 [7.00, 12.00]    |0.7     |
|Patients admitted to intensive care unit (%)                                                  |No                                        |2108 (62.5)           |1051 (62.4)           |1057 (62.7)           |30.6    |
|                                                                                              |Yes                                       |230 ( 6.8)            |118 ( 7.0)            |112 ( 6.6)            |        |
|                                                                                              |NA                                        |1033 (30.6)           |515 (30.6)            |518 (30.7)            |        |
|Clinical status on ordinal scale (%)                                                          |2                                         |487 (14.4)            |234 (13.9)            |253 (15.0)            |0.2     |
|                                                                                              |3                                         |1790 (53.1)           |912 (54.2)            |878 (52.0)            |        |
|                                                                                              |4                                         |835 (24.8)            |413 (24.5)            |422 (25.0)            |        |
|                                                                                              |5                                         |252 ( 7.5)            |123 ( 7.3)            |129 ( 7.6)            |        |
|                                                                                              |NA                                        |7 ( 0.2)              |2 ( 0.1)              |5 ( 0.3)              |        |
|Comorbidities (%)                                                                             |Immunocompromised                         |61 ( 1.8)             |33 ( 2.0)             |28 ( 1.7)             |0.5     |
|                                                                                              |Multiple comorbidities                    |1525 (45.2)           |774 (46.0)            |751 (44.5)            |        |
|                                                                                              |No comorbidity                            |788 (23.4)            |382 (22.7)            |406 (24.1)            |        |
|                                                                                              |One comorbidity                           |979 (29.0)            |487 (28.9)            |492 (29.2)            |        |
|                                                                                              |NA                                        |18 ( 0.5)             |8 ( 0.5)              |10 ( 0.6)             |        |
|Comedication (%)                                                                              |Dexamethasone and Tocilizumab             |1 ( 0.0)              |0 ( 0.0)              |1 ( 0.1)              |30.9    |
|                                                                                              |Dexamethasone but no Tocilizumab          |1883 (55.9)           |952 (56.5)            |931 (55.2)            |        |
|                                                                                              |No Dexamethasone, no Tocilizumab          |447 (13.3)            |215 (12.8)            |232 (13.8)            |        |
|                                                                                              |NA                                        |1040 (30.9)           |517 (30.7)            |523 (31.0)            |        |
|Remdesivir (%)                                                                                |No Remdesivir                             |2261 (67.1)           |874 (51.9)            |1387 (82.2)           |0.0     |
|                                                                                              |Remdesivir                                |1110 (32.9)           |810 (48.1)            |300 (17.8)            |        |
|C-reactive protein concentration, mg/L (median [IQR])                                         |                                          |61.00 [25.14, 115.00] |62.00 [25.33, 114.73] |60.01 [25.12, 115.54] |40.3    |
|Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N]) (%) |Not seroconverted                         |7 ( 0.2)              |6 ( 0.4)              |1 ( 0.1)              |95.9    |
|                                                                                              |Seroconverted                             |132 ( 3.9)            |67 ( 4.0)             |65 ( 3.9)             |        |
|                                                                                              |NA                                        |3232 (95.9)           |1611 (95.7)           |1621 (96.1)           |        |
|Patients with detectable viral load (%)                                                       |Detectable viral load                     |1610 (47.8)           |807 (47.9)            |803 (47.6)            |50.4    |
|                                                                                              |Undetectable viral load                   |61 ( 1.8)             |31 ( 1.8)             |30 ( 1.8)             |        |
|                                                                                              |NA                                        |1700 (50.4)           |846 (50.2)            |854 (50.6)            |        |
|SARS CoV-2 variant (%)                                                                        |Delta                                     |21 ( 0.6)             |13 ( 0.8)             |8 ( 0.5)              |99.1    |
|                                                                                              |Omicron                                   |10 ( 0.3)             |6 ( 0.4)              |4 ( 0.2)              |        |
|                                                                                              |PRESENCE OF MUTATIONS E484Q AND L452R     |1 ( 0.0)              |1 ( 0.1)              |0 ( 0.0)              |        |
|                                                                                              |NA                                        |3339 (99.1)           |1664 (98.8)           |1675 (99.3)           |        |

## Baseline table, by trial

```r
# all participants, by trial
table_baseline_trial <- CreateTableOne(data = df_baseline, vars = vars.list[!vars.list %in% c("Trial", "ARM")], strata = "Trial", includeNA = T, test = F, addOverall = TRUE)
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

|                                                                                              |level                                     |Overall               |ACTT2                |Bari-Solidact         |COV-BARRIER           |COVINIB               |Ghazaeian            |Murugesan            |TOFACOV              |Missing |
|:---------------------------------------------------------------------------------------------|:-----------------------------------------|:---------------------|:--------------------|:---------------------|:---------------------|:---------------------|:--------------------|:--------------------|:--------------------|:-------|
|n                                                                                             |                                          |3371                  |1033                 |289                   |1626                  |110                   |97                   |100                  |116                  |        |
|Age, years (median [IQR])                                                                     |                                          |57.00 [46.00, 67.00]  |56.00 [43.00, 67.00] |60.00 [50.00, 69.00]  |58.00 [48.00, 68.00]  |55.00 [47.25, 62.00]  |51.00 [37.00, 64.00] |46.50 [37.75, 55.00] |58.00 [50.75, 66.25] |0.0     |
|Sex (%)                                                                                       |Female                                    |1353 (40.1)           |381 ( 36.9)          |218 ( 75.4)           |608 ( 37.4)           |34 ( 30.9)            |50 ( 51.5)           |26 ( 26.0)           |36 ( 31.0)           |0.0     |
|                                                                                              |Male                                      |2018 (59.9)           |652 ( 63.1)          |71 ( 24.6)            |1018 ( 62.6)          |76 ( 69.1)            |47 ( 48.5)           |74 ( 74.0)           |80 ( 69.0)           |        |
|Ethnicity (%)                                                                                 |                                          |34 ( 1.0)             |0 (  0.0)            |0 (  0.0)             |34 (  2.1)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |8.6     |
|                                                                                              |AMERICAN INDIAN OR ALASKA NATIVE          |358 (10.6)            |10 (  1.0)           |0 (  0.0)             |348 ( 21.4)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |Asian                                     |1 ( 0.0)              |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |1 (  0.9)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |ASIAN                                     |276 ( 8.2)            |101 (  9.8)          |0 (  0.0)             |175 ( 10.8)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |BLACK OR AFRICAN AMERICAN                 |233 ( 6.9)            |156 ( 15.1)          |0 (  0.0)             |77 (  4.7)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |caucasian                                 |116 ( 3.4)            |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |116 (100.0)          |        |
|                                                                                              |HISPANIC OR LATINO                        |246 ( 7.3)            |246 ( 23.8)          |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |Indian                                    |100 ( 3.0)            |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |100 (100.0)          |0 (  0.0)            |        |
|                                                                                              |Latino                                    |19 ( 0.6)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |19 ( 17.3)            |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |MULTIPLE                                  |5 ( 0.1)              |0 (  0.0)            |0 (  0.0)             |5 (  0.3)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER |16 ( 0.5)             |11 (  1.1)           |0 (  0.0)             |5 (  0.3)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |Persian/Mazani                            |97 ( 2.9)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |97 (100.0)           |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |UNKNOWN                                   |13 ( 0.4)             |13 (  1.3)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |White                                     |90 ( 2.7)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |90 ( 81.8)            |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |WHITE                                     |1478 (43.8)           |496 ( 48.0)          |0 (  0.0)             |982 ( 60.4)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |NA                                        |289 ( 8.6)            |0 (  0.0)            |289 (100.0)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|Country (%)                                                                                   |Argentina                                 |229 ( 6.8)            |0 (  0.0)            |0 (  0.0)             |229 ( 14.1)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |53.3    |
|                                                                                              |Austria                                   |6 ( 0.2)              |0 (  0.0)            |6 (  2.1)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |Belgium                                   |10 ( 0.3)             |0 (  0.0)            |10 (  3.5)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |Brasil                                    |366 (10.9)            |0 (  0.0)            |0 (  0.0)             |366 ( 22.5)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |France                                    |94 ( 2.8)             |0 (  0.0)            |94 ( 32.5)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |Germany                                   |21 ( 0.6)             |0 (  0.0)            |1 (  0.3)             |20 (  1.2)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |India                                     |50 ( 1.5)             |0 (  0.0)            |0 (  0.0)             |50 (  3.1)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |Ireland                                   |9 ( 0.3)              |0 (  0.0)            |9 (  3.1)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |Italy                                     |50 ( 1.5)             |0 (  0.0)            |25 (  8.7)            |25 (  1.5)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |Japan                                     |38 ( 1.1)             |0 (  0.0)            |0 (  0.0)             |38 (  2.3)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |Luxembourg                                |1 ( 0.0)              |0 (  0.0)            |1 (  0.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |Mexico                                    |312 ( 9.3)            |0 (  0.0)            |0 (  0.0)             |312 ( 19.2)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |Norway                                    |127 ( 3.8)            |0 (  0.0)            |127 ( 43.9)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |Portugal                                  |3 ( 0.1)              |0 (  0.0)            |3 (  1.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |Russia                                    |112 ( 3.3)            |0 (  0.0)            |0 (  0.0)             |112 (  6.9)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |South Korea                               |36 ( 1.1)             |0 (  0.0)            |0 (  0.0)             |36 (  2.2)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |Spain                                     |100 ( 3.0)            |0 (  0.0)            |13 (  4.5)            |87 (  5.4)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |United Kingdom                            |11 ( 0.3)             |0 (  0.0)            |0 (  0.0)             |11 (  0.7)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |NA                                        |1796 (53.3)           |1033 (100.0)         |0 (  0.0)             |340 ( 20.9)           |110 (100.0)           |97 (100.0)           |100 (100.0)          |116 (100.0)          |        |
|Vaccinated (%)                                                                                |Any SARS CoV-2 vaccine                    |107 ( 3.2)            |0 (  0.0)            |102 ( 35.3)           |0 (  0.0)             |2 (  1.8)             |0 (  0.0)            |0 (  0.0)            |3 (  2.6)            |51.3    |
|                                                                                              |No vaccine                                |1536 (45.6)           |1033 (100.0)         |182 ( 63.0)           |0 (  0.0)             |108 ( 98.2)           |0 (  0.0)            |100 (100.0)          |113 ( 97.4)          |        |
|                                                                                              |NA                                        |1728 (51.3)           |0 (  0.0)            |5 (  1.7)             |1626 (100.0)          |0 (  0.0)             |97 (100.0)           |0 (  0.0)            |0 (  0.0)            |        |
|Time from symptom onset to randomisation, days (median [IQR])                                 |                                          |9.00 [6.00, 12.00]    |8.00 [5.00, 10.00]   |9.00 [7.00, 11.00]    |10.00 [8.00, 13.00]   |7.00 [5.00, 9.00]     |7.00 [6.00, 10.00]   |5.00 [3.00, 7.00]    |8.00 [6.00, 10.00]   |0.7     |
|Patients admitted to intensive care unit (%)                                                  |No                                        |2108 (62.5)           |0 (  0.0)            |160 ( 55.4)           |1525 ( 93.8)          |110 (100.0)           |97 (100.0)           |100 (100.0)          |116 (100.0)          |30.6    |
|                                                                                              |Yes                                       |230 ( 6.8)            |0 (  0.0)            |129 ( 44.6)           |101 (  6.2)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |NA                                        |1033 (30.6)           |1033 (100.0)         |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|Clinical status on ordinal scale (%)                                                          |2                                         |487 (14.4)            |142 ( 13.7)          |0 (  0.0)             |186 ( 11.4)           |35 ( 31.8)            |2 (  2.1)            |97 ( 97.0)           |25 ( 21.6)           |0.2     |
|                                                                                              |3                                         |1790 (53.1)           |564 ( 54.6)          |0 (  0.0)             |962 ( 59.2)           |75 ( 68.2)            |95 ( 97.9)           |3 (  3.0)            |91 ( 78.4)           |        |
|                                                                                              |4                                         |835 (24.8)            |216 ( 20.9)          |249 ( 86.2)           |370 ( 22.8)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |5                                         |252 ( 7.5)            |111 ( 10.7)          |40 ( 13.8)            |101 (  6.2)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |NA                                        |7 ( 0.2)              |0 (  0.0)            |0 (  0.0)             |7 (  0.4)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|Comorbidities (%)                                                                             |Immunocompromised                         |61 ( 1.8)             |30 (  2.9)           |11 (  3.8)            |16 (  1.0)            |2 (  1.8)             |2 (  2.1)            |0 (  0.0)            |0 (  0.0)            |0.5     |
|                                                                                              |Multiple comorbidities                    |1525 (45.2)           |546 ( 52.9)          |99 ( 34.3)            |762 ( 46.9)           |31 ( 28.2)            |32 ( 33.0)           |29 ( 29.0)           |26 ( 22.4)           |        |
|                                                                                              |No comorbidity                            |788 (23.4)            |151 ( 14.6)          |106 ( 36.7)           |370 ( 22.8)           |37 ( 33.6)            |41 ( 42.3)           |39 ( 39.0)           |44 ( 37.9)           |        |
|                                                                                              |One comorbidity                           |979 (29.0)            |288 ( 27.9)          |73 ( 25.3)            |478 ( 29.4)           |40 ( 36.4)            |22 ( 22.7)           |32 ( 32.0)           |46 ( 39.7)           |        |
|                                                                                              |NA                                        |18 ( 0.5)             |18 (  1.7)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|Comedication (%)                                                                              |Dexamethasone and Tocilizumab             |1 ( 0.0)              |0 (  0.0)            |1 (  0.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |30.9    |
|                                                                                              |Dexamethasone but no Tocilizumab          |1883 (55.9)           |0 (  0.0)            |270 ( 93.4)           |1291 ( 79.4)          |16 ( 14.5)            |97 (100.0)           |100 (100.0)          |109 ( 94.0)          |        |
|                                                                                              |No Dexamethasone, no Tocilizumab          |447 (13.3)            |0 (  0.0)            |18 (  6.2)            |328 ( 20.2)           |94 ( 85.5)            |0 (  0.0)            |0 (  0.0)            |7 (  6.0)            |        |
|                                                                                              |NA                                        |1040 (30.9)           |1033 (100.0)         |0 (  0.0)             |7 (  0.4)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|Remdesivir (%)                                                                                |No Remdesivir                             |2261 (67.1)           |518 ( 50.1)          |281 ( 97.2)           |1337 ( 82.2)          |110 (100.0)           |0 (  0.0)            |2 (  2.0)            |13 ( 11.2)           |0.0     |
|                                                                                              |Remdesivir                                |1110 (32.9)           |515 ( 49.9)          |8 (  2.8)             |289 ( 17.8)           |0 (  0.0)             |97 (100.0)           |98 ( 98.0)           |103 ( 88.8)          |        |
|C-reactive protein concentration, mg/L (median [IQR])                                         |                                          |61.00 [25.14, 115.00] |NA [NA, NA]          |89.00 [50.00, 142.00] |65.00 [32.00, 122.90] |79.15 [39.72, 134.60] |68.00 [33.50, 84.00] |10.00 [5.49, 24.21]  |4.90 [2.40, 9.75]    |40.3    |
|Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N]) (%) |Not seroconverted                         |7 ( 0.2)              |0 (  0.0)            |7 (  2.4)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |95.9    |
|                                                                                              |Seroconverted                             |132 ( 3.9)            |0 (  0.0)            |132 ( 45.7)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |NA                                        |3232 (95.9)           |1033 (100.0)         |150 ( 51.9)           |1626 (100.0)          |110 (100.0)           |97 (100.0)           |100 (100.0)          |116 (100.0)          |        |
|Patients with detectable viral load (%)                                                       |Detectable viral load                     |1610 (47.8)           |0 (  0.0)            |116 ( 40.1)           |1494 ( 91.9)          |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |50.4    |
|                                                                                              |Undetectable viral load                   |61 ( 1.8)             |0 (  0.0)            |18 (  6.2)            |43 (  2.6)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |NA                                        |1700 (50.4)           |1033 (100.0)         |155 ( 53.6)           |89 (  5.5)            |110 (100.0)           |97 (100.0)           |100 (100.0)          |116 (100.0)          |        |
|SARS CoV-2 variant (%)                                                                        |Delta                                     |21 ( 0.6)             |0 (  0.0)            |21 (  7.3)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |99.1    |
|                                                                                              |Omicron                                   |10 ( 0.3)             |0 (  0.0)            |10 (  3.5)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |PRESENCE OF MUTATIONS E484Q AND L452R     |1 ( 0.0)              |0 (  0.0)            |1 (  0.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                                                                                              |NA                                        |3339 (99.1)           |1033 (100.0)         |257 ( 88.9)           |1626 (100.0)          |110 (100.0)           |97 (100.0)           |100 (100.0)          |116 (100.0)          |        |

## Missing data table, by trial

```r
table(df_tot_Muru$mort_28,useNA = "always")
```

```
## 
##    0    1 <NA> 
## 3053  318    0
```

```r
vars.list <- c("trt", "trial", "age", "sex", "ethn", "country", "vacc", "sympdur", "icu", "clinstatus_baseline", "comorb_cat", "comed_cat", "comed_rdv", "crp", "sero", "vl_baseline", "variant",
               "mort_28", "mort_60", "death_reached", "new_mv_28", "new_mvd_28", "clinstatus_28_imp", "discharge_reached",
               "discharge_reached_sus" , "ae_28","ae_28_sev" ,"vir_clear_5" ,"vir_clear_10", "vir_clear_15")

df_missing <- df_tot_Muru[,colnames(df_tot_Muru)%in%vars.list]
df_missing <- df_missing[,match(vars.list,colnames(df_missing))]

# colnames(df_missing) <- vars.list <- c("ARM","Trial","Age, years", "Sex", "Ethnicity", "Country", "Vaccinated", "Time from symptom onset to randomisation, days", "Patients admitted to intensive care unit","Clinical status on ordinal scale", "Comorbidities", "Comedication", "Remdesivir","C-reactive protein concentration, mg/L", "Seroconversion (patients with detectable anti-SARS-CoV-2 antibodies [anti-RBD or anti-N])", "Patients with detectable viral load", "SARS CoV-2 variant")

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

|                          |level                                     |Overall               |ACTT2                |Bari-Solidact         |COV-BARRIER           |COVINIB               |Ghazaeian            |Murugesan            |TOFACOV              |Missing |
|:-------------------------|:-----------------------------------------|:---------------------|:--------------------|:---------------------|:---------------------|:---------------------|:--------------------|:--------------------|:--------------------|:-------|
|n                         |                                          |3371                  |1033                 |289                   |1626                  |110                   |97                   |100                  |116                  |        |
|age (median [IQR])        |                                          |57.00 [46.00, 67.00]  |56.00 [43.00, 67.00] |60.00 [50.00, 69.00]  |58.00 [48.00, 68.00]  |55.00 [47.25, 62.00]  |51.00 [37.00, 64.00] |46.50 [37.75, 55.00] |58.00 [50.75, 66.25] |0.0     |
|sex (%)                   |1                                         |218 ( 6.5)            |0 (  0.0)            |218 ( 75.4)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |0.0     |
|                          |2                                         |71 ( 2.1)             |0 (  0.0)            |71 ( 24.6)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |F                                         |407 (12.1)            |381 ( 36.9)          |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |26 ( 26.0)           |0 (  0.0)            |        |
|                          |female                                    |728 (21.6)            |0 (  0.0)            |0 (  0.0)             |608 ( 37.4)           |34 ( 30.9)            |50 ( 51.5)           |0 (  0.0)            |36 ( 31.0)           |        |
|                          |M                                         |726 (21.5)            |652 ( 63.1)          |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |74 ( 74.0)           |0 (  0.0)            |        |
|                          |male                                      |1221 (36.2)           |0 (  0.0)            |0 (  0.0)             |1018 ( 62.6)          |76 ( 69.1)            |47 ( 48.5)           |0 (  0.0)            |80 ( 69.0)           |        |
|ethn (%)                  |                                          |34 ( 1.0)             |0 (  0.0)            |0 (  0.0)             |34 (  2.1)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |8.6     |
|                          |AMERICAN INDIAN OR ALASKA NATIVE          |358 (10.6)            |10 (  1.0)           |0 (  0.0)             |348 ( 21.4)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |Asian                                     |1 ( 0.0)              |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |1 (  0.9)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |ASIAN                                     |276 ( 8.2)            |101 (  9.8)          |0 (  0.0)             |175 ( 10.8)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |BLACK OR AFRICAN AMERICAN                 |233 ( 6.9)            |156 ( 15.1)          |0 (  0.0)             |77 (  4.7)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |caucasian                                 |116 ( 3.4)            |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |116 (100.0)          |        |
|                          |HISPANIC OR LATINO                        |246 ( 7.3)            |246 ( 23.8)          |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |Indian                                    |100 ( 3.0)            |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |100 (100.0)          |0 (  0.0)            |        |
|                          |Latino                                    |19 ( 0.6)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |19 ( 17.3)            |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |MULTIPLE                                  |5 ( 0.1)              |0 (  0.0)            |0 (  0.0)             |5 (  0.3)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER |16 ( 0.5)             |11 (  1.1)           |0 (  0.0)             |5 (  0.3)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |Persian/Mazani                            |97 ( 2.9)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |97 (100.0)           |0 (  0.0)            |0 (  0.0)            |        |
|                          |UNKNOWN                                   |13 ( 0.4)             |13 (  1.3)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |White                                     |90 ( 2.7)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |90 ( 81.8)            |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |WHITE                                     |1478 (43.8)           |496 ( 48.0)          |0 (  0.0)             |982 ( 60.4)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |NA                                        |289 ( 8.6)            |0 (  0.0)            |289 (100.0)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|country (%)               |ARG                                       |229 ( 6.8)            |0 (  0.0)            |0 (  0.0)             |229 ( 14.1)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |30.6    |
|                          |AUSTRIA                                   |6 ( 0.2)              |0 (  0.0)            |6 (  2.1)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |BELGIUM                                   |10 ( 0.3)             |0 (  0.0)            |10 (  3.5)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |BRA                                       |366 (10.9)            |0 (  0.0)            |0 (  0.0)             |366 ( 22.5)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |DEU                                       |20 ( 0.6)             |0 (  0.0)            |0 (  0.0)             |20 (  1.2)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |ESP                                       |87 ( 2.6)             |0 (  0.0)            |0 (  0.0)             |87 (  5.4)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |FRANCE                                    |94 ( 2.8)             |0 (  0.0)            |94 ( 32.5)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |GBR                                       |11 ( 0.3)             |0 (  0.0)            |0 (  0.0)             |11 (  0.7)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |GERMANY                                   |1 ( 0.0)              |0 (  0.0)            |1 (  0.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |IND                                       |50 ( 1.5)             |0 (  0.0)            |0 (  0.0)             |50 (  3.1)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |India                                     |100 ( 3.0)            |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |100 (100.0)          |0 (  0.0)            |        |
|                          |Iran                                      |97 ( 2.9)             |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |97 (100.0)           |0 (  0.0)            |0 (  0.0)            |        |
|                          |IRELAND                                   |9 ( 0.3)              |0 (  0.0)            |9 (  3.1)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |ITA                                       |25 ( 0.7)             |0 (  0.0)            |0 (  0.0)             |25 (  1.5)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |Italy                                     |116 ( 3.4)            |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |116 (100.0)          |        |
|                          |ITALY                                     |25 ( 0.7)             |0 (  0.0)            |25 (  8.7)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |JPN                                       |38 ( 1.1)             |0 (  0.0)            |0 (  0.0)             |38 (  2.3)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |KOR                                       |36 ( 1.1)             |0 (  0.0)            |0 (  0.0)             |36 (  2.2)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |LUXEMBOURG                                |1 ( 0.0)              |0 (  0.0)            |1 (  0.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |MEX                                       |312 ( 9.3)            |0 (  0.0)            |0 (  0.0)             |312 ( 19.2)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |NORWAY                                    |127 ( 3.8)            |0 (  0.0)            |127 ( 43.9)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |PORTUGAL                                  |3 ( 0.1)              |0 (  0.0)            |3 (  1.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |RUS                                       |112 ( 3.3)            |0 (  0.0)            |0 (  0.0)             |112 (  6.9)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |Spain                                     |110 ( 3.3)            |0 (  0.0)            |0 (  0.0)             |0 (  0.0)             |110 (100.0)           |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |SPAIN                                     |13 ( 0.4)             |0 (  0.0)            |13 (  4.5)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |USA                                       |340 (10.1)            |0 (  0.0)            |0 (  0.0)             |340 ( 20.9)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |NA                                        |1033 (30.6)           |1033 (100.0)         |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|vacc (%)                  |0                                         |1536 (45.6)           |1033 (100.0)         |182 ( 63.0)           |0 (  0.0)             |108 ( 98.2)           |0 (  0.0)            |100 (100.0)          |113 ( 97.4)          |51.3    |
|                          |1                                         |107 ( 3.2)            |0 (  0.0)            |102 ( 35.3)           |0 (  0.0)             |2 (  1.8)             |0 (  0.0)            |0 (  0.0)            |3 (  2.6)            |        |
|                          |NA                                        |1728 (51.3)           |0 (  0.0)            |5 (  1.7)             |1626 (100.0)          |0 (  0.0)             |97 (100.0)           |0 (  0.0)            |0 (  0.0)            |        |
|sympdur (median [IQR])    |                                          |9.00 [6.00, 12.00]    |8.00 [5.00, 10.00]   |9.00 [7.00, 11.00]    |10.00 [8.00, 13.00]   |7.00 [5.00, 9.00]     |7.00 [6.00, 10.00]   |5.00 [3.00, 7.00]    |8.00 [6.00, 10.00]   |0.7     |
|icu (%)                   |0                                         |2108 (62.5)           |0 (  0.0)            |160 ( 55.4)           |1525 ( 93.8)          |110 (100.0)           |97 (100.0)           |100 (100.0)          |116 (100.0)          |30.6    |
|                          |1                                         |230 ( 6.8)            |0 (  0.0)            |129 ( 44.6)           |101 (  6.2)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |NA                                        |1033 (30.6)           |1033 (100.0)         |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|clinstatus_baseline (%)   |2                                         |487 (14.4)            |142 ( 13.7)          |0 (  0.0)             |186 ( 11.4)           |35 ( 31.8)            |2 (  2.1)            |97 ( 97.0)           |25 ( 21.6)           |0.2     |
|                          |3                                         |1790 (53.1)           |564 ( 54.6)          |0 (  0.0)             |962 ( 59.2)           |75 ( 68.2)            |95 ( 97.9)           |3 (  3.0)            |91 ( 78.4)           |        |
|                          |4                                         |835 (24.8)            |216 ( 20.9)          |249 ( 86.2)           |370 ( 22.8)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |5                                         |252 ( 7.5)            |111 ( 10.7)          |40 ( 13.8)            |101 (  6.2)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |NA                                        |7 ( 0.2)              |0 (  0.0)            |0 (  0.0)             |7 (  0.4)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|comorb_cat (%)            |1                                         |788 (23.4)            |151 ( 14.6)          |106 ( 36.7)           |370 ( 22.8)           |37 ( 33.6)            |41 ( 42.3)           |39 ( 39.0)           |44 ( 37.9)           |0.5     |
|                          |2                                         |979 (29.0)            |288 ( 27.9)          |73 ( 25.3)            |478 ( 29.4)           |40 ( 36.4)            |22 ( 22.7)           |32 ( 32.0)           |46 ( 39.7)           |        |
|                          |3                                         |1525 (45.2)           |546 ( 52.9)          |99 ( 34.3)            |762 ( 46.9)           |31 ( 28.2)            |32 ( 33.0)           |29 ( 29.0)           |26 ( 22.4)           |        |
|                          |4                                         |61 ( 1.8)             |30 (  2.9)           |11 (  3.8)            |16 (  1.0)            |2 (  1.8)             |2 (  2.1)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |NA                                        |18 ( 0.5)             |18 (  1.7)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|comed_cat (%)             |1                                         |447 (13.3)            |0 (  0.0)            |18 (  6.2)            |328 ( 20.2)           |94 ( 85.5)            |0 (  0.0)            |0 (  0.0)            |7 (  6.0)            |30.9    |
|                          |2                                         |1 ( 0.0)              |0 (  0.0)            |1 (  0.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |3                                         |1883 (55.9)           |0 (  0.0)            |270 ( 93.4)           |1291 ( 79.4)          |16 ( 14.5)            |97 (100.0)           |100 (100.0)          |109 ( 94.0)          |        |
|                          |NA                                        |1040 (30.9)           |1033 (100.0)         |0 (  0.0)             |7 (  0.4)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|comed_rdv (%)             |0                                         |2261 (67.1)           |518 ( 50.1)          |281 ( 97.2)           |1337 ( 82.2)          |110 (100.0)           |0 (  0.0)            |2 (  2.0)            |13 ( 11.2)           |0.0     |
|                          |1                                         |1110 (32.9)           |515 ( 49.9)          |8 (  2.8)             |289 ( 17.8)           |0 (  0.0)             |97 (100.0)           |98 ( 98.0)           |103 ( 88.8)          |        |
|crp (median [IQR])        |                                          |61.00 [25.14, 115.00] |NA [NA, NA]          |89.00 [50.00, 142.00] |65.00 [32.00, 122.90] |79.15 [39.72, 134.60] |68.00 [33.50, 84.00] |10.00 [5.49, 24.21]  |4.90 [2.40, 9.75]    |40.3    |
|sero (%)                  |0                                         |7 ( 0.2)              |0 (  0.0)            |7 (  2.4)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |95.9    |
|                          |1                                         |132 ( 3.9)            |0 (  0.0)            |132 ( 45.7)           |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |NA                                        |3232 (95.9)           |1033 (100.0)         |150 ( 51.9)           |1626 (100.0)          |110 (100.0)           |97 (100.0)           |100 (100.0)          |116 (100.0)          |        |
|vl_baseline (%)           |0                                         |1610 (47.8)           |0 (  0.0)            |116 ( 40.1)           |1494 ( 91.9)          |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |50.4    |
|                          |1                                         |61 ( 1.8)             |0 (  0.0)            |18 (  6.2)            |43 (  2.6)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |NA                                        |1700 (50.4)           |1033 (100.0)         |155 ( 53.6)           |89 (  5.5)            |110 (100.0)           |97 (100.0)           |100 (100.0)          |116 (100.0)          |        |
|variant (%)               |Delta                                     |21 ( 0.6)             |0 (  0.0)            |21 (  7.3)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |99.1    |
|                          |Omicron                                   |10 ( 0.3)             |0 (  0.0)            |10 (  3.5)            |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |PRESENCE OF MUTATIONS E484Q AND L452R     |1 ( 0.0)              |0 (  0.0)            |1 (  0.3)             |0 (  0.0)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |NA                                        |3339 (99.1)           |1033 (100.0)         |257 ( 88.9)           |1626 (100.0)          |110 (100.0)           |97 (100.0)           |100 (100.0)          |116 (100.0)          |        |
|mort_28 (%)               |0                                         |3053 (90.6)           |972 ( 94.1)          |253 ( 87.5)           |1415 ( 87.0)          |108 ( 98.2)           |90 ( 92.8)           |100 (100.0)          |115 ( 99.1)          |0.0     |
|                          |1                                         |318 ( 9.4)            |61 (  5.9)           |36 ( 12.5)            |211 ( 13.0)           |2 (  1.8)             |7 (  7.2)            |0 (  0.0)            |1 (  0.9)            |        |
|mort_60 (%)               |0                                         |3005 (89.1)           |972 ( 94.1)          |243 ( 84.1)           |1377 ( 84.7)          |108 ( 98.2)           |90 ( 92.8)           |100 (100.0)          |115 ( 99.1)          |0.0     |
|                          |1                                         |366 (10.9)            |61 (  5.9)           |46 ( 15.9)            |249 ( 15.3)           |2 (  1.8)             |7 (  7.2)            |0 (  0.0)            |1 (  0.9)            |        |
|death_reached (%)         |0                                         |3004 (89.1)           |972 ( 94.1)          |242 ( 83.7)           |1377 ( 84.7)          |108 ( 98.2)           |90 ( 92.8)           |100 (100.0)          |115 ( 99.1)          |0.0     |
|                          |1                                         |367 (10.9)            |61 (  5.9)           |47 ( 16.3)            |249 ( 15.3)           |2 (  1.8)             |7 (  7.2)            |0 (  0.0)            |1 (  0.9)            |        |
|new_mv_28 (%)             |0                                         |2600 (77.1)           |794 ( 76.9)          |185 ( 64.0)           |1218 ( 74.9)          |100 ( 90.9)           |90 ( 92.8)           |100 (100.0)          |113 ( 97.4)          |14.5    |
|                          |1                                         |283 ( 8.4)            |91 (  8.8)           |37 ( 12.8)            |145 (  8.9)           |8 (  7.3)             |0 (  0.0)            |0 (  0.0)            |2 (  1.7)            |        |
|                          |NA                                        |488 (14.5)            |148 ( 14.3)          |67 ( 23.2)            |263 ( 16.2)           |2 (  1.8)             |7 (  7.2)            |0 (  0.0)            |1 (  0.9)            |        |
|new_mvd_28 (%)            |0                                         |2770 (82.2)           |881 ( 85.3)          |216 ( 74.7)           |1270 ( 78.1)          |100 ( 90.9)           |90 ( 92.8)           |100 (100.0)          |113 ( 97.4)          |0.0     |
|                          |1                                         |601 (17.8)            |152 ( 14.7)          |73 ( 25.3)            |356 ( 21.9)           |10 (  9.1)            |7 (  7.2)            |0 (  0.0)            |3 (  2.6)            |        |
|clinstatus_28_imp (%)     |1                                         |2673 (79.3)           |871 ( 84.3)          |187 ( 64.7)           |1212 ( 74.5)          |103 ( 93.6)           |90 ( 92.8)           |100 (100.0)          |110 ( 94.8)          |0.0     |
|                          |2                                         |47 ( 1.4)             |14 (  1.4)           |9 (  3.1)             |24 (  1.5)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |3                                         |109 ( 3.2)            |21 (  2.0)           |18 (  6.2)            |66 (  4.1)            |3 (  2.7)             |0 (  0.0)            |0 (  0.0)            |1 (  0.9)            |        |
|                          |4                                         |54 ( 1.6)             |15 (  1.5)           |13 (  4.5)            |23 (  1.4)            |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |3 (  2.6)            |        |
|                          |5                                         |170 ( 5.0)            |51 (  4.9)           |26 (  9.0)            |90 (  5.5)            |2 (  1.8)             |0 (  0.0)            |0 (  0.0)            |1 (  0.9)            |        |
|                          |6                                         |318 ( 9.4)            |61 (  5.9)           |36 ( 12.5)            |211 ( 13.0)           |2 (  1.8)             |7 (  7.2)            |0 (  0.0)            |1 (  0.9)            |        |
|discharge_reached (%)     |0                                         |716 (21.2)            |194 ( 18.8)          |96 ( 33.2)            |406 ( 25.0)           |7 (  6.4)             |7 (  7.2)            |0 (  0.0)            |6 (  5.2)            |0.0     |
|                          |1                                         |2655 (78.8)           |839 ( 81.2)          |193 ( 66.8)           |1220 ( 75.0)          |103 ( 93.6)           |90 ( 92.8)           |100 (100.0)          |110 ( 94.8)          |        |
|discharge_reached_sus (%) |0                                         |726 (21.5)            |194 ( 18.8)          |98 ( 33.9)            |414 ( 25.5)           |7 (  6.4)             |7 (  7.2)            |0 (  0.0)            |6 (  5.2)            |0.0     |
|                          |1                                         |2645 (78.5)           |839 ( 81.2)          |191 ( 66.1)           |1212 ( 74.5)          |103 ( 93.6)           |90 ( 92.8)           |100 (100.0)          |110 ( 94.8)          |        |
|ae_28 (%)                 |0                                         |1675 (49.7)           |0 (  0.0)            |0 (  0.0)             |1300 ( 80.0)          |85 ( 77.3)            |96 ( 99.0)           |100 (100.0)          |94 ( 81.0)           |45.4    |
|                          |1                                         |164 ( 4.9)            |0 (  0.0)            |0 (  0.0)             |118 (  7.3)           |23 ( 20.9)            |1 (  1.0)            |0 (  0.0)            |22 ( 19.0)           |        |
|                          |NA                                        |1532 (45.4)           |1033 (100.0)         |289 (100.0)           |208 ( 12.8)           |2 (  1.8)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|ae_28_sev (%)             |0                                         |1675 (49.7)           |0 (  0.0)            |0 (  0.0)             |1300 ( 80.0)          |85 ( 77.3)            |96 ( 99.0)           |100 (100.0)          |94 ( 81.0)           |45.4    |
|                          |1                                         |113 ( 3.4)            |0 (  0.0)            |0 (  0.0)             |76 (  4.7)            |14 ( 12.7)            |1 (  1.0)            |0 (  0.0)            |22 ( 19.0)           |        |
|                          |2                                         |28 ( 0.8)             |0 (  0.0)            |0 (  0.0)             |25 (  1.5)            |3 (  2.7)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |3                                         |10 ( 0.3)             |0 (  0.0)            |0 (  0.0)             |9 (  0.6)             |1 (  0.9)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |4                                         |6 ( 0.2)              |0 (  0.0)            |0 (  0.0)             |4 (  0.2)             |2 (  1.8)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |5                                         |2 ( 0.1)              |0 (  0.0)            |0 (  0.0)             |1 (  0.1)             |1 (  0.9)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |6                                         |3 ( 0.1)              |0 (  0.0)            |0 (  0.0)             |1 (  0.1)             |2 (  1.8)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |8                                         |2 ( 0.1)              |0 (  0.0)            |0 (  0.0)             |2 (  0.1)             |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |NA                                        |1532 (45.4)           |1033 (100.0)         |289 (100.0)           |208 ( 12.8)           |2 (  1.8)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|vir_clear_5 (%)           |0                                         |825 (24.5)            |0 (  0.0)            |91 ( 31.5)            |734 ( 45.1)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |67.4    |
|                          |1                                         |274 ( 8.1)            |0 (  0.0)            |30 ( 10.4)            |244 ( 15.0)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |NA                                        |2272 (67.4)           |1033 (100.0)         |168 ( 58.1)           |648 ( 39.9)           |110 (100.0)           |97 (100.0)           |100 (100.0)          |116 (100.0)          |        |
|vir_clear_10 (%)          |0                                         |723 (21.4)            |0 (  0.0)            |69 ( 23.9)            |654 ( 40.2)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |64.8    |
|                          |1                                         |464 (13.8)            |0 (  0.0)            |58 ( 20.1)            |406 ( 25.0)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |NA                                        |2184 (64.8)           |1033 (100.0)         |162 ( 56.1)           |566 ( 34.8)           |110 (100.0)           |97 (100.0)           |100 (100.0)          |116 (100.0)          |        |
|vir_clear_15 (%)          |0                                         |641 (19.0)            |0 (  0.0)            |57 ( 19.7)            |584 ( 35.9)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |63.7    |
|                          |1                                         |583 (17.3)            |0 (  0.0)            |71 ( 24.6)            |512 ( 31.5)           |0 (  0.0)             |0 (  0.0)            |0 (  0.0)            |0 (  0.0)            |        |
|                          |NA                                        |2147 (63.7)           |1033 (100.0)         |161 ( 55.7)           |530 ( 32.6)           |110 (100.0)           |97 (100.0)           |100 (100.0)          |116 (100.0)          |        |
