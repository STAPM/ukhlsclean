#' Clean UKHLS data
#'
#' A wrapper function for applying all of the cleaning functions, selecting the
#' desired variables/observations for the analysis, and specifying complete case
#' restrictions.
#'
#' @param data Data table - the combined Understanding Society dataset for one wave.
#' @param ages Integer vector - the ages in single years to retain (defaults to 16 to 89 years).
#' @param country Character - country to produce data for. One of c("UK","england","wales","scotland","northern_ireland"). Defaults to all UK.
#' @param keep_vars Character vector - the names of the variables to keep (defaults to NULL - retaining all variables).
#' @param complete_vars Character vector - the names of the variables on which the selection of
#' complete cases will be based (defaults to year, age and gender).
#' @param calendar_year Logical - TRUE when the code is processing calendar year data and merges in ONS population counts data.
#' @return Returns a new set of variables
#' @export
ukhls_clean_global <- function(data,
                               ages = 16:89,
                               country = "UK",
                               keep_vars = NULL,
                               complete_vars = c("year", "age", "sex"),
                               calendar_year
) {

  ## fix bug that occurs if age is not in keep_vars

  if (!("age" %in% names(data)) ) {
    ages = NULL
  }

  #######################################################################
  #### Save out a dataset containing the key identifiers and weights ####

  main_data <- data[, c("id", "hidp", "wave_no", "bhps_sample",
                        "year", "month", "day", "weight_xw")]


  ### demographics

  cat(crayon::red("\n\t\tDemographic variables module\n"))

  demographics <- ukhlsclean::ukhls_clean_demographic(data = data)

  ### health and well-being

  cat(crayon::red("\n\t\tHealth and wellbeing variables module\n"))

  health <- ukhlsclean::ukhls_clean_health(data = data)

  ### alcohol

  cat(crayon::red("\n\t\tAlcohol variables module\n"))

  alcohol <- ukhlsclean::ukhls_clean_alcohol(data = data)

  ### smoking

  cat(crayon::red("\n\t\tSmoking variables module\n"))

  smoke <- ukhlsclean::ukhls_clean_smoke(data = data)

  ### labour market

  cat(crayon::red("\n\t\tLabour market variables module\n"))

  lmkt <- ukhlsclean::ukhls_clean_econstat(data = data)

  ### work

  cat(crayon::red("\n\t\tWork variables module\n"))

  work <- ukhlsclean::ukhls_clean_work(data = data)

  ### household

  cat(crayon::red("\n\t\tFamily and household variables module\n\n"))

  hhold <- ukhlsclean::ukhls_clean_hhold(data = data)

  ######################
  ### Merge datasets ###

  merged_data <- merge(main_data, demographics, by = c("id", "hidp", "wave_no"))
  merged_data <- merge(merged_data, health,     by = c("id", "hidp", "wave_no"))
  merged_data <- merge(merged_data, alcohol,    by = c("id", "hidp", "wave_no"))
  merged_data <- merge(merged_data, smoke,      by = c("id", "hidp", "wave_no"))
  merged_data <- merge(merged_data, lmkt,       by = c("id", "hidp", "wave_no"))
  merged_data <- merge(merged_data, work,       by = c("id", "hidp", "wave_no"))
  merged_data <- merge(merged_data, hhold,      by = c("id", "hidp", "wave_no"))

  ######################
  ### Merge in the population counts data

  if (calendar_year == TRUE){

    pop_counts <- gen_scaling_factor(data = merged_data,
                                     pop_data = ukhlsclean::PopulationCounts)

    merged_data <- merge(merged_data, pop_counts, by = c("year","d_age","d_sex","d_country"), all.x = TRUE, sort = FALSE)

    setcolorder(merged_data, c("id","hidp","wave_no","bhps_sample","year","month","day","weight_xw","pop_factor"))
  }

  ############################
  ### Apply data filtering ###

  final_data <- ukhlsclean::select_data(
    data = merged_data,
    ages = ages,
    country = country,
    keep_vars = keep_vars,
    complete_vars = complete_vars
  )


  return(final_data)
}
