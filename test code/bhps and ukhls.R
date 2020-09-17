library(ukhlsclean)
library(data.table)
library(dplyr)

## directory for working on mac
root <- "/Volumes/"
path <- "ScHARR Work/Data/UKHLS/raw data/"

## directory for working on desktop
root <- "C:/"
path <- "Users/User/Documents/Datasets/UKHLS/tab/"

data <- combine_waves(list(bhps_read_wave1(root,path),bhps_read_wave2(root,path),bhps_read_wave3(root,path),
                           bhps_read_wave4(root,path),bhps_read_wave5(root,path),bhps_read_wave6(root,path),
                           bhps_read_wave7(root,path),bhps_read_wave8(root,path),bhps_read_wave9(root,path),
                           bhps_read_wave10(root,path),bhps_read_wave11(root,path),bhps_read_wave12(root,path),
                           bhps_read_wave13(root,path),bhps_read_wave14(root,path),bhps_read_wave15(root,path),
                           bhps_read_wave16(root,path),bhps_read_wave17(root,path),bhps_read_wave18(root,path),
                           ukhls_read_wave1(root,path),ukhls_read_wave2(root,path),
                           ukhls_read_wave3(root,path),ukhls_read_wave4(root,path),
                           ukhls_read_wave5(root,path),ukhls_read_wave6(root,path),
                           ukhls_read_wave7(root,path),ukhls_read_wave8(root,path),
                           ukhls_read_wave9(root,path)
                           ),ukhls=TRUE)

clean_data <- data %>%
  ukhlsclean::clean_demographic()



