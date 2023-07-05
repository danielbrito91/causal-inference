library(dplyr)
library(haven)
library(estimatr)

install.packages("tidyverse")

abortion_raw <- read_dta(url("https://raw.github.com/scunning1975/mixtape/master/abortion.dta"))

abortion <- abortion_raw %>%
    mutate(
        repeal = as_factor(repeal), # Tratamento
        year = as_factor(year), #ohe year
        fip = as_factor(fip), # 
        fa = as_factor(fa),
        younger = as_factor(younger),
        yr = as_factor(case_when(repeal==1 & younger==1 ~ 1, TRUE ~ 0)), 
        wm = as_factor(case_when(wht==1 & male==1 ~ 1, TRUE ~ 0)),
        wf = as_factor(case_when(wht==1 & male==0 ~ 1, TRUE ~ 0)),
        bm = as_factor(case_when(wht==0 & male==1 ~ 1, TRUE ~ 0)),
        bf = as_factor(case_when(wht==0 & male==0 ~ 1, TRUE ~ 0))
        ) %>%
    filter(bf==1 & (age==15 | age==25))

regddd <- lm_robust(lnr ~ repeal*year + younger*repeal + younger*year + yr*year +
            fip*t + acc + ir + pi + alcohol + crack + poverty + income + ur,
            data = abortion, weights=totpop, clusters=fip)


abortion$totpop
