
### run the workflow to generate descriptive tables and plots for the UKHLS longitudinal data

ukhls_version_no <- gsub("\\.", "-", packageVersion("ukhlsclean"))
path <- here::here("vignettes/USoc_longitudinal_data_check")

source(paste0(path, "/03_load_packages.R"))
source(paste0(path, "/05_read_data.R"))
source(paste0(path, "/10_longitudinal_data_plots.R"))
source(paste0(path, "/11_longitudinal_data_plots_1.R"))
source(paste0(path, "/12_longitudinal_data_plots_2.R"))
source(paste0(path, "/13_longitudinal_data_plots_3.R"))
source(paste0(path, "/14_longitudinal_data_plots_4.R"))
source(paste0(path, "/15_longitudinal_data_plots_5.R"))
source(paste0(path, "/16_longitudinal_data_plots_6.R"))
source(paste0(path, "/17_longitudinal_data_plots_7_hours.R"))
