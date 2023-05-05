
### Packages

library(ggplot2)

### Test regressions

regdata <- copy(data)
regdata <- regdata[, c("d_age_5cat","d_sex","d_gor","d_marital","d_hiqual","d_ethnicity_5cat","s_current_smoker","w_absent_sick")]
reg1 <- glm(w_absent_sick ~ ., family = binomial(link = "logit"), data = regdata)
reg1
summary(reg1)

## halo main report takes in explanatory
##    ALL:          age(continuous), age^2, region1 region2 region3 (north, mid, south divide), sex,
##                  marital (binary of married/cohabiting or single), education (degree vs not), comorbidities (assuming we dont take any)
##    3L NSSEC:     NOT in econ inactive outcome
##    Econ stat:    binary of economically active or inactive
##                  ONLY in benefit related work outcome
##    Unemployed:   binary of employed vs unemployed
##                  ONLY in benefit related work outcome
# questions:
# (1) if we're using youth data, how to represent degree level education, unlikely that they would be able to hold one in the first place because of age?
# (2) halo uses probability of retirement - is there point for indicator of retirement, or looking for retirement age?
# (3) halo uses age (continuous) and age squared, but is there potential for using age categories instead (linear spline or something?)

## To regress for:
##    Sick absence -        w_absent_sick
##    Unemployment -        l_econ_stat_2cat
##    Econ inactive/unemp - l_econ_stat_3cat
##    Retired -             l_econ_stat_7cat
##    dHH income pm -
##    dSOcialBen income -
##    dIndivs employed -
##    dUC proxy receipt -   b_UniCred
##

####
#### ABSENCE ####

### s_current_smoker ###

regdata <- copy(data)
regdata <- regdata[, c("s_current_smoker","d_age","d_sex","d_gor","d_marital","d_hiqual","d_ethnicity_5cat","w_sic_1dig","w_absent_sick")]
regdata[, d_age2 := d_age^2] ## transform variables
regdata <- regdata[d_marital == "sep_div_wid", d_marital := "single"]
regdata <- regdata[d_hiqual != "degree", d_hiqual := "no_degree"]
reg2 <- glm(w_absent_sick ~ ., family = binomial(link = "logit"), data = regdata)
reg2
summary(reg2)

regdata <- copy(data)
regdata <- regdata[, c("d_age","d_sex","d_gor","d_marital","d_hiqual",
                       "d_ethnicity_5cat",
                       "s_current_smoker",
                       "w_absent_sick","w_absent_other")]
regdata[, d_age2 := d_age^2] ## transform variables
regdata <- regdata[d_marital == "sep_div_wid", d_marital := "single"]
regdata <- regdata[d_hiqual != "degree", d_hiqual := "no_degree"]
reg3 <- glm(w_absent_other ~ ., family = binomial(link = "logit"), data = regdata)
reg3
summary(reg3)
reg4 <- glm(w_absent_other ~ s_current_smoker + d_age + d_age2 + d_sex + d_gor + d_marital + d_hiqual + w_absent_sick, family = binomial(link = "logit"), data = regdata)
reg4
summary(reg4)

### s_ncigs ###

regdata <- copy(data)
regdata <- regdata[, c("d_age","d_sex","d_gor","d_marital","d_hiqual",
                       "d_ethnicity_5cat",
                       "s_ncigs",
                       "w_absent_sick")]
regdata[, d_age2 := d_age^2] ## transform variables
regdata <- regdata[d_marital == "sep_div_wid", d_marital := "single"]
regdata <- regdata[d_hiqual != "degree", d_hiqual := "no_degree"]
reg5 <- glm(w_absent_sick ~ ., family = binomial(link = "logit"), data = regdata)
reg5
summary(reg5)

regdata <- copy(data)
regdata <- regdata[, c("d_age","d_sex","d_gor","d_marital","d_hiqual",
                       "d_ethnicity_5cat",
                       "s_ncigs",
                       "w_absent_sick","w_absent_other")]
regdata[, d_age2 := d_age^2] ## transform variables
regdata <- regdata[d_marital == "sep_div_wid", d_marital := "single"]
regdata <- regdata[d_hiqual != "degree", d_hiqual := "no_degree"]
reg6 <- glm(w_absent_other ~ ., family = binomial(link = "logit"), data = regdata)
reg6
summary(reg6)
reg7 <- glm(w_absent_other ~ s_ncigs + d_age + d_age2 + d_sex + d_gor + d_marital + d_hiqual + w_absent_sick, family = binomial(link = "logit"), data = regdata)
reg7
summary(reg7)


####
#### EMPLOYMENT ####

### Unemployed ###

regdata <- copy(data)
regdata <- regdata[, c("d_age","d_sex","d_gor","d_marital","d_hiqual",
                       "d_ethnicity_5cat",
                       "s_current_smoker",
                       "l_econ_stat_2cat")]
regdata[, d_age2 := d_age^2] ## transform variables
regdata <- regdata[d_marital == "sep_div_wid", d_marital := "single"]
regdata <- regdata[d_hiqual != "degree", d_hiqual := "no_degree"]
reg8 <- glm(l_econ_stat_2cat ~ ., family = binomial(link = "logit"), data = regdata)
reg8
summary(reg8)

regdata <- copy(data)
regdata <- regdata[, c("d_age","d_sex","d_gor","d_marital","d_hiqual",
                       "d_ethnicity_5cat",
                       "s_current_smoker",
                       "l_econ_stat_3cat")]
regdata[, d_age2 := d_age^2] ## transform variables
regdata <- regdata[d_marital == "sep_div_wid", d_marital := "single"]
regdata <- regdata[d_hiqual != "degree", d_hiqual := "no_degree"]
regdata <- regdata[l_econ_stat_3cat == "unemployed", l_econ_stat_3cat := "inactive"]
regdata <- regdata[, l_econ_stat_3cat := factor(l_econ_stat_3cat, levels = c("employed","inactive"))]
reg9 <- glm(l_econ_stat_3cat ~ ., family = binomial(link = "logit"), data = regdata)
reg9
summary(reg9)

regdata <- copy(data)
regdata <- regdata[, c("d_age","d_sex","d_gor","d_marital","d_hiqual",
                       "d_ethnicity_5cat",
                       "s_current_smoker",
                       "l_econ_stat_7cat")]
regdata[, d_age2 := d_age^2] ## transform variables
regdata <- regdata[d_marital == "sep_div_wid", d_marital := "single"]
regdata <- regdata[d_hiqual != "degree", d_hiqual := "no_degree"]
regdata <- regdata[! is.na(l_econ_stat_7cat) & l_econ_stat_7cat != "retired", l_econ_stat_7cat := "not_retired"]
regdata <- regdata[, l_econ_stat_7cat := factor(l_econ_stat_7cat, levels = c("not_retired","retired"))]
reg10 <- glm(l_econ_stat_7cat ~ ., family = binomial(link = "logit"), data = regdata)
reg10
summary(reg10)


####
#### BENEFITS ####

regdata <- copy(data)
regdata <- regdata[, c("d_age","d_sex","d_gor","d_marital","d_hiqual",
                       "d_ethnicity_5cat",
                       "s_current_smoker",
                       "b_UniCred")]
regdata[, d_age2 := d_age^2] ## transform variables
regdata <- regdata[d_marital == "sep_div_wid", d_marital := "single"]
regdata <- regdata[d_hiqual != "degree", d_hiqual := "no_degree"]
reg11 <- glm(b_UniCred ~ ., family = binomial(link = "logit"), data = regdata)
reg11
summary(reg11)

