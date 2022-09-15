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

###############################################################################
#### For each wave, wrap the reading function in the global cleaning function

list <- NULL

### Wave 1

if (1 %in% waves){

  wave1 <- ukhls_clean_global(ukhls_read_wave1(root = root, file = file, full = full),
                              waves = waves, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars)

  if (is.null(list)) {
    list <- list(wave1 = wave1)
  } else {
    list <- c(list, wave1)
  }

}

### Wave 2

if (2 %in% waves){

  wave2 <- ukhls_clean_global(ukhls_read_wave2(root = root, file = file, full = full),
                              waves = waves, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars)

  if (is.null(list)) {
    list <- list(wave2 = wave2)
  } else {
    list <- c(list, list(wave2 = wave2))
  }

}

### Wave 3

if (3 %in% waves){

  wave3 <- ukhls_clean_global(ukhls_read_wave3(root = root, file = file, full = full),
                              waves = waves, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars)

  if (is.null(list)) {
    list <- list(wave3 = wave3)
  } else {
    list <- c(list, list(wave3 = wave3))
  }

}

### Wave 4

if (4 %in% waves){

  wave4 <- ukhls_clean_global(ukhls_read_wave4(root = root, file = file, full = full),
                              waves = waves, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars)

  if (is.null(list)) {
    list <- list(wave4 = wave4)
  } else {
    list <- c(list, list(wave4 = wave4))
  }

}

### Wave 5

if (5 %in% waves){

  wave5 <- ukhls_clean_global(ukhls_read_wave5(root = root, file = file, full = full),
                              waves = waves, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars)

  if (is.null(list)) {
    list <- list(wave5 = wave5)
  } else {
    list <- c(list, list(wave5 = wave5))
  }

}

### Wave 6

if (6 %in% waves){

  wave6 <- ukhls_clean_global(ukhls_read_wave6(root = root, file = file, full = full),
                              waves = waves, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars)

  if (is.null(list)) {
    list <- list(wave6 = wave6)
  } else {
    list <- c(list, list(wave6 = wave6))
  }

}

### Wave 7

if (7 %in% waves){

  wave7 <- ukhls_clean_global(ukhls_read_wave7(root = root, file = file, full = full),
                              waves = waves, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars)

  if (is.null(list)) {
    list <- list(wave7 = wave7)
  } else {
    list <- c(list, list(wave7 = wave7))
  }

}

### Wave 8

if (8 %in% waves){

  wave8 <- ukhls_clean_global(ukhls_read_wave8(root = root, file = file, full = full),
                              waves = waves, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars)

  if (is.null(list)) {
    list <- list(wave8 = wave8)
  } else {
    list <- c(list, list(wave8 = wave8))
  }

}

### Wave 9

if (9 %in% waves){

  wave9 <- ukhls_clean_global(ukhls_read_wave9(root = root, file = file, full = full),
                              waves = waves, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars)

  if (is.null(list)) {
    list <- list(wave9 = wave9)
  } else {
    list <- c(list, list(wave9 = wave9))
  }

}

### Wave 10

if (10 %in% waves){

  wave10 <- ukhls_clean_global(ukhls_read_wave10(root = root, file = file, full = full),
                              waves = waves, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars)

  if (is.null(list)) {
    list <- list(wave10 = wave10)
  } else {
    list <- c(list, list(wave10 = wave10))
  }

}

### Wave 11

if (11 %in% waves){

  wave11 <- ukhls_clean_global(ukhls_read_wave11(root = root, file = file, full = full),
                               waves = waves, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars)

  if (is.null(list)) {
    list <- list(wave11 = wave11)
  } else {
    list <- c(list, list(wave11 = wave11))
  }

}

#############################################################
### Combine all waves in the list into a single dataset

data <- ukhlsclean::combine_waves(list)

return(data)
}
