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
#' @param complete_vars Character vector - the names of the variables on which the selection of complete cases will be based (defaults to NULL - keep all observations).
#' @param youth. Logical - TRUE if also processing the youth data files.
#' @param child. Logical - TRUE if also processing the child data files.
#' @param inflation_index Character - one of c("cpih","rpi"). Default option is CPIH
#' @return Returns a new set of variables
#' @export

ukhlsclean <- function(root = "X:/",
                       file = "HAR_PR/PR/USoc/Data/SN6614_2022_11_29/tab/ukhls",
                       full = TRUE,
                       waves = 1:14,
                       ages = NULL,
                       country = NULL,
                       complete_vars = NULL,
                       youth = FALSE,
                       child = FALSE,
                       inflation_index = "cpih"){

cat(crayon::bold(crayon::underline(crayon::green("Cleaning the Understanding Society Longitudinal Data\n\n"))))

start_time <- Sys.time()

if (inflation_index != "rpi"){
  cat(crayon::bold(crayon::red("CPIH used for inflation adjustment\n\n")))

} else if (inflation_index == "rpi"){
  cat(crayon::bold(crayon::red("RPI used for inflation adjustment\n\n")))

}

### select inflation data to use. Default if not specified is cpih

  inflation <- ukhlsclean::cpih
if (inflation_index == "rpi"){
  inflation <- ukhlsclean::rpi
}

###############################################################################
#### For each wave, wrap the reading function in the global cleaning function

data_list <- list()

## identify if running the calendar year code
calendar_year <- FALSE

### Wave 1

if (1 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave1(root = root, file = file, full = full),
                              ages = ages, complete_vars = complete_vars, calendar_year = calendar_year, inflation = inflation)

  data_list <- append(data_list, list(wave)) ; rm(wave)
}

### Wave 2

if (2 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave2(root = root, file = file, full = full),
                              ages = ages, complete_vars = complete_vars, calendar_year = calendar_year, inflation = inflation)

  data_list <- append(data_list, list(wave)) ; rm(wave)
}

### Wave 3

if (3 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave3(root = root, file = file, full = full),
                              ages = ages, complete_vars = complete_vars, calendar_year = calendar_year, inflation = inflation)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 4

if (4 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave4(root = root, file = file, full = full),
                              ages = ages, complete_vars = complete_vars, calendar_year = calendar_year, inflation = inflation)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 5

if (5 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave5(root = root, file = file, full = full),
                              ages = ages, complete_vars = complete_vars, calendar_year = calendar_year, inflation = inflation)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 6

if (6 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave6(root = root, file = file, full = full),
                              ages = ages, complete_vars = complete_vars, calendar_year = calendar_year, inflation = inflation)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 7

if (7 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave7(root = root, file = file, full = full),
                              ages = ages, complete_vars = complete_vars, calendar_year = calendar_year, inflation = inflation)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 8

if (8 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave8(root = root, file = file, full = full),
                              ages = ages, complete_vars = complete_vars, calendar_year = calendar_year, inflation = inflation)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 9

if (9 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave9(root = root, file = file, full = full),
                              ages = ages, complete_vars = complete_vars, calendar_year = calendar_year, inflation = inflation)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 10

if (10 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave10(root = root, file = file, full = full),
                              ages = ages, complete_vars = complete_vars, calendar_year = calendar_year, inflation = inflation)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 11

if (11 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave11(root = root, file = file, full = full),
                               ages = ages, complete_vars = complete_vars, calendar_year = calendar_year, inflation = inflation)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 12

if (12 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave12(root = root, file = file, full = full),
                             ages = ages, complete_vars = complete_vars, calendar_year = calendar_year, inflation = inflation)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 13

if (13 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave13(root = root, file = file, full = full),
                             ages = ages, complete_vars = complete_vars, calendar_year = calendar_year, inflation = inflation)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 14

if (14 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave14(root = root, file = file, full = full),
                             ages = ages, complete_vars = complete_vars, calendar_year = calendar_year, inflation = inflation)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}


#############################################################
### Combine all waves in the list into a single dataset and
### apply filters

data <- ukhlsclean::ukhls_combine_waves(data_list)


### commenting out - this function is called in the global cleaning function

#data <- ukhlsclean::select_data(data = data,
#                                ages = ages,
#                                country = country,
#                                keep_vars = keep_vars,
#                                complete_vars = complete_vars,
#                                calendar_year = calendar_year)

########################################
### Summation of smokers in household
data <- ukhls_combine_hhold_smokers(data)

#####################################
### Lag household smokers to (t-1)
data <- data[order(pidp, wave_no), lag.s_hhold_smokers := data.table::shift(s_hhold_smokers, n=1, type="lag"), by=pidp]

###################################
### Combine youth data

if (youth == TRUE){
cat(crayon::blue("\n\t\tYouth data... \n"))

youth_data <- ukhls_clean_youth(ukhls_read_youth(root = root, file = file)) # change :::

### Merging other smokers in household to youth data
data_hholdsmoke <- data.table::copy(main[, c("hidp","wave","s_current_smoker")])
data_hholdsmoke <- data_hholdsmoke[, s_current_smoker := ifelse(s_current_smoker == "non_smoker", 0, ifelse(s_current_smoker == "smoker", 1, NA_real_))]
data_hholdsmoke <- data_hholdsmoke[, .SD[which.max(s_current_smoker)], by = c("hidp")]
data_hholdsmoke <- data_hholdsmoke[, s_hhold_smokers := s_current_smoker]
data_hholdsmoke <- data_hholdsmoke[, s_current_smoker := NULL]
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

cat(crayon::bold(crayon::green("\tUKHLS Youth dataset appended")))

}
#######################
## Record time taken

end_time <- Sys.time()

tdiff <- difftime(end_time, start_time, units = "mins")

time <- paste0("\nComplete.\n\nTotal Data reading and cleaning time: ", round(tdiff,2), " minutes\n")

cat(crayon::bold(crayon::underline(crayon::green(time))))

return(data)
}
