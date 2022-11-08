#' Read, Clean, Combine Understanding Society data
#'
#' A wrapper function for applying all of the reading and cleaning functions, selecting the
#' desired variables/observations for the analysis, and specifying complete case
#' restrictions.
#'
#' @param root Character - the root directory.
#' @param file Character - the file path and name.
#' @param full Logical - TRUE if restricting the sample to full interviews only (excluding proxies)
#' @param waves Integer vector - the waves of the UKHLS to retain (defaults to all - 1 to 11).
#' @param ages Integer vector - the ages in single years to retain (defaults to 16 to 89 years).
#' @param keep_vars Character vector - the names of the variables to keep (defaults to NULL - retaining all variables).
#' @param complete_vars Character vector - the names of the variables on which the selection of complete cases will be based (defaults to year, age and gender).
#' @return Returns a new set of variables
#' @export

ukhlsclean <- function(root,
                       file,
                       full = TRUE,
                       waves = 1:11,
                       ages = 16:89,
                       keep_vars = NULL,
                       complete_vars = c("age","sex")){

cat(crayon::bgMagenta("Cleaning the Understanding Society Data\n\n"))

start_time <- Sys.time()

###############################################################################
#### For each wave, wrap the reading function in the global cleaning function

data_list <- list()

### Wave 1

if (1 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave1(root = root, file = file, full = full),
                              ages = ages, keep_vars = keep_vars, complete_vars = complete_vars)

  data_list <- append(data_list, list(wave)) ; rm(wave)
}

### Wave 2

if (2 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave2(root = root, file = file, full = full),
                              ages = ages, keep_vars = keep_vars, complete_vars = complete_vars)

  data_list <- append(data_list, list(wave)) ; rm(wave)
}

### Wave 3

if (3 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave3(root = root, file = file, full = full),
                              ages = ages, keep_vars = keep_vars, complete_vars = complete_vars)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 4

if (4 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave4(root = root, file = file, full = full),
                              ages = ages, keep_vars = keep_vars, complete_vars = complete_vars)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 5

if (5 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave5(root = root, file = file, full = full),
                              ages = ages, keep_vars = keep_vars, complete_vars = complete_vars)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 6

if (6 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave6(root = root, file = file, full = full),
                              ages = ages, keep_vars = keep_vars, complete_vars = complete_vars)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 7

if (7 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave7(root = root, file = file, full = full),
                              ages = ages, keep_vars = keep_vars, complete_vars = complete_vars)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 8

if (8 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave8(root = root, file = file, full = full),
                              ages = ages, keep_vars = keep_vars, complete_vars = complete_vars)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 9

if (9 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave9(root = root, file = file, full = full),
                              ages = ages, keep_vars = keep_vars, complete_vars = complete_vars)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 10

if (10 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave10(root = root, file = file, full = full),
                              ages = ages, keep_vars = keep_vars, complete_vars = complete_vars)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

### Wave 11

if (11 %in% waves){

  wave <- ukhls_clean_global(ukhls_read_wave11(root = root, file = file, full = full),
                               ages = ages, keep_vars = keep_vars, complete_vars = complete_vars)

  data_list <- append(data_list, list(wave)) ; rm(wave)

}

#############################################################
### Combine all waves in the list into a single dataset

data <- ukhlsclean::ukhls_combine_waves(data_list)

#######################
## Record time taken

end_time <- Sys.time()

tdiff <- difftime(end_time, start_time, units = "mins")

time <- paste0("Total Data reading and cleaning time: ", round(tdiff,2), " minutes")

cat(crayon::bgMagenta(time))

return(data)
}
