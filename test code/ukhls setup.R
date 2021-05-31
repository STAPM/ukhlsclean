library(ukhlsclean)
library(data.table)
library(dplyr)

## set directory
root <- "C:/"
path <- "Users/cm1djm/Documents/Datasets/UKHLS/tab/"

ids <- c("id","hidp","year","month","wave_no","weight_lw","weight_xw")
dem <- c("age","gender","gor","marstat","hiqual","ethnicity_5cat")
smk <- c("non_smoker","current_smoker","ever_smoked","othersmoker_hhold")
job <- c("econ_stat_3cat","econ_stat_7cat","real_grss_pay_usual","hours")
health <- c("disability","pregnant","sf12_mcs","sf12_pcs",
            "sf1","sf2a","sf2b","sf3a","sf3b","sf4a","sf4b",
            "sf5","sf6a","sf6b","sf6c","sf7")

keep_vars     <- c(ids,dem,job,health,smk)
complete_vars <- c("year","age","gender")

ukhls <- combine_waves(list(ukhls_read_wave1(root,path),
                            ukhls_read_wave2(root,path),
                            ukhls_read_wave3(root,path),
                            ukhls_read_wave4(root,path),
                            ukhls_read_wave5(root,path),
                            ukhls_read_wave6(root,path),
                            ukhls_read_wave7(root,path),
                            ukhls_read_wave8(root,path),
                            ukhls_read_wave9(root,path),
                            ukhls_read_wave10(root,path)
                            ) )

clean_data <- ukhls_clean_global(ukhls,
                                 ages = 16:89,
                                 keep_vars = keep_vars,
                                 complete_vars = complete_vars)
