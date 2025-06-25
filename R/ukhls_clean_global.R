#' Clean UKHLS data
#'
#' A wrapper function for applying all of the cleaning functions, selecting the
#' desired variables/observations for the analysis, and specifying complete case
#' restrictions.
#'
#' @param data Data table - the combined Understanding Society dataset for one wave.
#' @param ages Integer vector - the ages in single years to retain (defaults to NULL - all ages).
#' @param country Character - country to produce data for. One of c("england","wales","scotland","northern_ireland"). Defaults to NULL which includes all UK.
#' @param keep_vars Character vector - the names of the variables to keep (defaults NULL - keep all variables).
#' @param complete_vars Character vector - the names of the variables on which the selection of complete cases will be based (defaults to NULL - keep all observations).
#' @param calendar_year Logical - TRUE when the code is processing calendar year data (defaults to FALSE).
#' @param inflation Data table. Inflation data input for real-terms adjustments. Defaults to CPIH.
#'
#' @return Returns a new set of variables
#' @export
ukhls_clean_global <- function(data,
                               ages = 16:89,
                               country = NULL,
                               complete_vars = NULL,
                               calendar_year = FALSE,
                               inflation = ukhlsclean::cpih
) {

  ## fix bug that occurs if age is not in keep_vars

  if (!("age" %in% names(data)) ) {
    ages = NULL
  }

  #######################################################################
  #### Save out a dataset containing the key identifiers and weights ####

  main_data <- data[, c("pidp", "id", "hidp", "wave", "wave_no", "bhps_sample", ## Added wave (identifier)
                        "year", "month", "day", "weight_xw")]


  ### demographics

  demographics <- ukhlsclean::ukhls_clean_demographic(data = data)

  ### income

  income <- ukhlsclean::ukhls_clean_income(data = data)

  ### health and well-being

  health <- ukhlsclean::ukhls_clean_health(data = data)

  ### health conditions

  health_conditions <- ukhlsclean::ukhls_clean_health_conditions(data = data)

  ### alcohol

  alcohol <- ukhlsclean::ukhls_clean_alcohol(data = data)

  ### smoking

  smoke <- ukhlsclean::ukhls_clean_smoke(data = data)

  ### labour market

  lmkt <- ukhlsclean::ukhls_clean_econstat(data = data,
                                           inflation = inflation)

  ### work

  work <- ukhlsclean::ukhls_clean_work(data = data, calendar_year = calendar_year)

  ### benefits

  benefit <- ukhlsclean::ukhls_clean_benefit(data = data)

  ### household

  hhold <- ukhlsclean::ukhls_clean_hhold(data = data, calendar_year = calendar_year, inflation = inflation)

  ######################
  ### Merge datasets ###

  merged_data <- merge(main_data, demographics,        by = c("pidp", "id", "hidp", "wave_no"))
  merged_data <- merge(merged_data, income,            by = c("pidp", "id", "hidp", "wave_no"))
  merged_data <- merge(merged_data, health,            by = c("pidp", "id", "hidp", "wave_no"))
  merged_data <- merge(merged_data, health_conditions, by = c("pidp", "id", "hidp", "wave_no"))
  merged_data <- merge(merged_data, alcohol,           by = c("pidp", "id", "hidp", "wave_no"))
  merged_data <- merge(merged_data, smoke,             by = c("pidp", "id", "hidp", "wave_no"))
  merged_data <- merge(merged_data, lmkt,              by = c("pidp", "id", "hidp", "wave_no"))
  merged_data <- merge(merged_data, work,              by = c("pidp", "id", "hidp", "wave_no"))
  merged_data <- merge(merged_data, benefit,           by = c("pidp", "id", "hidp", "wave_no"))
  merged_data <- merge(merged_data, hhold,             by = c("pidp", "id", "hidp", "wave_no"))

  ####################################################
  ### Post process benefits by employment activity ###

  merged_data <- ukhls_post_clean_benefit(data = merged_data)

  ######################
  ### Merge in the population counts data

  if (calendar_year == TRUE){

    pop_counts <- gen_scaling_factor(data = merged_data,
                                     pop_data = ukhlsclean::PopulationCounts)

    merged_data <- merge(merged_data, pop_counts, by = c("year","d_age","d_sex","d_country"), all.x = TRUE, sort = FALSE)

    setcolorder(merged_data, c("pidp", "id","hidp","wave","wave_no","bhps_sample","year","month","day","weight_xw","pop_factor")) ## added wave identifier
  }

  ############################
  ### Apply data filtering ###

  final_data <- ukhlsclean::select_data(
    data = merged_data,
    ages = ages,
    country = country,
    complete_vars = complete_vars,
    calendar_year = calendar_year
  )


  return(final_data)
}
