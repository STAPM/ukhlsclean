#' Generate Population Scaling Factor
#'
#' Merge ONS mid-year population estimates into the calendar year data to generate a population scaling factor
#' for calculating estimates of total population statistics. The scaling factor is calculated by summing the total
#' weighted number of individuals in each sex, country, and single-year age cell in the sample and dividing the total
#' population by this figure, to get a number of total individuals in the population represented by each individual in the
#' sample.
#'
#' @param data data table - merged data generated from different cleaning modules in `ukhls_clean_global`
#' @param pop_data data table - population counts by single year of age, sex, and UK nation (defaults to `PopulationCounts`)
#'
#' @return data table
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
gen_scaling_factor <- function(data,
                               pop_data = ukhlsclean::PopulationCounts){

  ### keep only non-missing age, country, and sex from the main data and
  ### drop 90+

  #data <- data[d_age < 90 & year == 2020,]
  data <- data[d_age < 90,]

  for(c in c("d_age","d_sex","d_country")) {

    data <- data[!is.na(get(c))]

  }


  ### collapse main data, calculate the weighted sum of individuals by
  ### summing up the population weight that has mean 1

  data_counts <- data[, .(N = sum(weight_xw, na.rm = TRUE)), by = c("year","d_age","d_sex","d_country")]

  ### merge in the population counts data. divide total population by the
  ### weighted sum in the data to get the scaling factor - the number of individuals
  ### in the total population each individual in the sample represents

  data_counts_merge <- merge(data_counts, pop_data, by = c("year","d_age","d_sex","d_country"), all.x = TRUE, all.y = FALSE)

  data_counts_merge[, pop_factor := pop_count / N]

  #### return
  data_counts_merge <- data_counts_merge[, c("year","d_age","d_sex","d_country","pop_factor")]

  return(data_counts_merge)
}
