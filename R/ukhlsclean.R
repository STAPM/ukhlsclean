#' Read, Clean, Combine Understanding Society data
#'
#' A wrapper function for applying all of the reading and cleaning functions, selecting the
#' desired variables/observations for the analysis, and specifying complete case
#' restrictions.
#'
#' UKDS Study Number: \href{https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=6614}{Understanding Society: Waves 1-12, 2009-2021 and Harmonised BHPS: Waves 1-18, 1991-2009}
#'
#' @source University of Essex, Institute for Social and Economic Research. (2022). Understanding Society: Waves 1-12, 2009-2021 and Harmonised BHPS: Waves 1-18, 1991-2009. [data collection]. 17th Edition. UK Data Service. SN: 6614, \href{https://doi.org/10.5255/UKDA-SN-6614-18}{DOI: 10.5255/UKDA-SN-6614-18}
#'
#'
#' @param root Character - the root directory.
#' @param file Character - the file path and name.
#' @param full Logical - TRUE if restricting the sample to full interviews only (excluding proxies)
#' @param waves Integer vector - the waves of the UKHLS to retain (defaults to all - 1 to 11).
#' @param ages Integer vector - the ages in single years to retain (defaults to NULL - all ages).
#' @param country Character - country to produce data for. One of c("england","wales","scotland","northern_ireland"). Defaults to NULL which includes all UK.
#' @param keep_vars Character vector - the names of the variables to keep (defaults NULL - keep all variables).
#' @param complete_vars Character vector - the names of the variables on which the selection of complete cases will be based (defaults to NULL - keep all observations).
#' @param youth. Logical - TRUE if also processing the youth data files
#' @return Returns a new set of variables
#' @export

ukhlsclean <- function(root = "X:/",
                       file = "HAR_PR/PR/USoc/Data/SN6614_2022_11_29/tab/ukhls",
                       full = TRUE,
                       waves = 1:12,
                       ages = NULL,
                       country = NULL,
                       keep_vars = NULL,
                       complete_vars = NULL,
                       youth = FALSE){

cat(crayon::red("Cleaning the Understanding Society Longitudinal Data\n\n"))

start_time <- Sys.time()

###############################################################################
#### For each wave, wrap the reading function in the global cleaning function

data_list <- list()

## identify if running the calendar year code
calendar_year <- FALSE

### Wave 1

if (1 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave1(root = root, file = file, full = full),
                              ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, calendar_year = calendar_year)

  data_list <- append(data_list, list(wave)) ; rm(wave)
}

### Wave 2

if (2 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave2(root = root, file = file, full = full),
                              ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, calendar_year = calendar_year)

  data_list <- append(data_list, list(wave)) ; rm(wave)
}

### Wave 3

if (3 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave3(root = root, file = file, full = full),
                              ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, calendar_year = calendar_year)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 4

if (4 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave4(root = root, file = file, full = full),
                              ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, calendar_year = calendar_year)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 5

if (5 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave5(root = root, file = file, full = full),
                              ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, calendar_year = calendar_year)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 6

if (6 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave6(root = root, file = file, full = full),
                              ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, calendar_year = calendar_year)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 7

if (7 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave7(root = root, file = file, full = full),
                              ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, calendar_year = calendar_year)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 8

if (8 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave8(root = root, file = file, full = full),
                              ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, calendar_year = calendar_year)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 9

if (9 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave9(root = root, file = file, full = full),
                              ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, calendar_year = calendar_year)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 10

if (10 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave10(root = root, file = file, full = full),
                              ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, calendar_year = calendar_year)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 11

if (11 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave11(root = root, file = file, full = full),
                               ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, calendar_year = calendar_year)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 12

if (12 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave12(root = root, file = file, full = full),
                             ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, calendar_year = calendar_year)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}


#############################################################
### Combine all waves in the list into a single dataset and
### apply filters

data <- ukhlsclean::ukhls_combine_waves(data_list)

data <- ukhlsclean::select_data(data = data,
                                ages = ages,
                                country = country,
                                keep_vars = keep_vars,
                                complete_vars = complete_vars,
                                calendar_year = calendar_year)

###################################
### Combine youth data

if (youth == TRUE){
cat(crayon::magenta("\n\t\tYouth data... \n"))

youth_data <- ukhls_clean_youth(ukhls_read_youth(root = root, file = file)) # change :::

# ## Other smokers in hh
# if ("current_smoker" %in% colnames(data)) {
#
#   # indicator for any other household smokers
#   data[!(wave_no %in% c(1,3,4)) & current_smoker == "smoker", smoke := 1]
#   data[!(wave_no %in% c(1,3,4)) & current_smoker == "non_smoker", smoke := 0]
#   data[!(wave_no %in% c(1,3,4)), num_smoker_hhold := sum(smoke, na.rm=TRUE), by = c("wave_no","hidp")]
#   # number of other smokers = number of smokers - respondent
#   data[!(wave_no %in% c(1,3,4)), num_othersmoker_hhold := num_smoker_hhold - smoke]
#   # create a binary indicator
#   data[!(wave_no %in% c(1,3,4)) & num_othersmoker_hhold > 0, othersmoker_hhold := "yes"]
#   data[!(wave_no %in% c(1,3,4)) & num_othersmoker_hhold == 0, othersmoker_hhold := "no"]
#
#   data[, othersmoker_hhold := as.factor(othersmoker_hhold)]
#
#   data[, othersmoker_hhold := NA]
# }

### Merging other smokers in household to youth data
data_hholdsmoke <- data.table::copy(data[, c("hidp","wave","s_othersmoker_hhold")])
youth_data <- merge(youth_data, data_hholdsmoke, by = c("hidp","wave"), all.x=TRUE) ##### ???
### if na, does this mean youth taken survey with adult ? or other ?

#
# hh_smk_data <- data[s_current_smoker == "smoker", ]
# hh_smk_data <- data[, .N, by = c("hidp","wave")]
# hh_smk_data <- hh_smk_data[N >= 1, s_othersmoker_hhold := "yes"]
# hh_smk_data <- hh_smk_data[N == 0, s_othersmoker_hhold := "no"]
# hh_smk_data[, N := NULL]
# youth_data <- merge(youth_data, hh_smk_data, by = c("hidp","wave"), all.x = TRUE)
# }

data <- rbind(data, youth_data, use.names = TRUE, fill = TRUE)

data <- data[, wave := factor(wave, levels = c("UKHLS Wave 1", "UKHLS Wave 2", "UKHLS Wave 3",
                                               "UKHLS Wave 4", "UKHLS Wave 5", "UKHLS Wave 6",
                                               "UKHLS Wave 7", "UKHLS Wave 8", "UKHLS Wave 9",
                                               "UKHLS Wave 10", "UKHLS Wave 11", "UKHLS Wave 12",
                                               "UKHLS Youth Wave 1", "UKHLS Youth Wave 2", "UKHLS Youth Wave 3",
                                               "UKHLS Youth Wave 4", "UKHLS Youth Wave 5", "UKHLS Youth Wave 6",
                                               "UKHLS Youth Wave 7", "UKHLS Youth Wave 8", "UKHLS Youth Wave 9",
                                               "UKHLS Youth Wave 10", "UKHLS Youth Wave 11", "UKHLS Youth Wave 12"
                                               ))]

cat(crayon::red("\tUKHLS Youth dataset appended"))

}
#######################
## Record time taken

end_time <- Sys.time()

tdiff <- difftime(end_time, start_time, units = "mins")

time <- paste0("\nTotal Data reading and cleaning time: ", round(tdiff,2), " minutes\n")

cat(crayon::red(time))

return(data)
}
