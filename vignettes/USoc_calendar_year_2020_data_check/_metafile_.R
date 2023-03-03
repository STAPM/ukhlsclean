
### run the workflow to generate descriptives tables and plots for UKHLS Calendar Year 2020 data

ukhls_version_no <- gsub("\\.", "-", packageVersion("ukhlsclean"))
path <- here::here("vignettes/USoc_calendar_year_2020_data_check")

source(paste0(path, "/03_load_packages.R"))
source(paste0(path, "/05_read_data.R"))
source(paste0(path, "/10_calendar_year_data_plots.R"))
