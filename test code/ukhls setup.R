library(ukhlsclean)
library(data.table)
library(dplyr)


## set directory
root <- "C:/"
path <- "Users/cm1djm/Documents/Datasets/UKHLS/tab/"

ukhls <- combine_waves(list(ukhls_read_wave1(root,path),
                            ukhls_read_wave2(root,path),
                            ukhls_read_wave3(root,path),
                            ukhls_read_wave4(root,path),
                            ukhls_read_wave5(root,path),
                            ukhls_read_wave6(root,path),
                            ukhls_read_wave7(root,path),
                            ukhls_read_wave8(root,path),
                            ukhls_read_wave9(root,path)
                            )
                       )

data <- ukhls

clean_data <- data %>%
  ukhlsclean::clean_demographic() %>%
  ukhlsclean::clean_education() %>%
  ukhlsclean::clean_health() %>%
  ukhlsclean::clean_smoke() %>%
  ukhlsclean::clean_econ_status() %>%
  ukhlsclean::clean_hhold() %>%
  ukhlsclean::clean_hours_earn()
