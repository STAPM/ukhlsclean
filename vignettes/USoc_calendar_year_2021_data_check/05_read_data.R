
#### read raw data using the package and save to a version-labelled dataset in the
#### intermediate data folder

## input arguments

root  <- "X:/"
file  <- "HAR_PR/PR/USoc/Data/Calendar Year Datasets/SN9193_2024_01_22/tab"
full  <- TRUE # full interviews (no proxies) only
ages  <- 16:89
country <- "UK"
keep_vars <- NULL
complete_vars <- c("d_age","d_sex","d_country","l_econ_stat_3cat")
inflation_index <- "cpih"


############################################
### UKHLS Calendar Year 2021 cross-section

data2021 <- ukhlsclean_2021(root = root,
                            file = file,
                            full = full,
                            ages = ages,
                            country = country,
                            keep_vars = keep_vars,
                            complete_vars = complete_vars,
                            inflation_index = inflation_index)



