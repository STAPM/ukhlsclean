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
#' @param ages Integer vector - the ages in single years to retain (defaults to 16 to 89 years).
#' @param country Character - country to produce data for. One of c("UK","england","wales","scotland","northern_ireland"). Defaults to all UK.
#' @param keep_vars Character vector - the names of the variables to keep (defaults to NULL - retaining all variables).
#' @param complete_vars Character vector - the names of the variables on which the selection of complete cases will be based (defaults to year, age and sex).
#' @return Returns a new set of variables
#' @export

ukhlsclean <- function(root = "X:/",
                       file = "HAR_PR/PR/USoc/Data/SN6614_2022_11_29/tab/ukhls",
                       full = TRUE,
                       waves = 1:12,
                       ages = 16:89,
                       country = "UK",
                       keep_vars = NULL,
                       complete_vars = c("d_age","d_sex")){

cat(crayon::bgWhite("Cleaning the Understanding Society Data\n\n"))

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
### Combine all waves in the list into a single dataset

data <- ukhlsclean::ukhls_combine_waves(data_list)

#######################
## Record time taken

end_time <- Sys.time()

tdiff <- difftime(end_time, start_time, units = "mins")

time <- paste0("Total Data reading and cleaning time: ", round(tdiff,2), " minutes\n")

cat(crayon::bgWhite(time))

return(data)
}
