
#### read raw data using the package and save to a version-labelled dataset in the
#### intermediate data folder

library(ukhlsclean)
library(data.table)

## input arguments

root  <- "X:/"
file  <- "HAR_PR/PR/USoc/Data/SN6614_2022_11_29/tab/ukhls"
full  <- TRUE # full interviews (no proxies) only
waves <- 1:12
ages  <- 16:89
country <- "UK"
keep_vars <- NULL
complete_vars <- c("d_age","d_sex","d_country","l_econ_stat_3cat")


##########################
### Full UKHLS panel data

fulldata <- ukhlsclean(root = root,
                       file = file,
                       full = full,
                       waves = waves,
                       ages = ages,
                       country = country,
                       keep_vars = keep_vars,
                       complete_vars = complete_vars)



