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
#' @param data set of combined data
#' @return Returns new set of combined data with smoking household counts
#' @export

ukhls_combine_hhold_smokers <- function(data = NULL) {

  hhold <- copy(data)
  hhold <- hhold[, c("hidp", "id","s_current_smoker")]
  hhold <- dcast(hhold, hidp ~ s_current_smoker)
  hhold <- hhold[, s_hhold_smokers := smoker]
  hhold <- hhold[, `NA` := NULL]
  hhold <- hhold[, non_smoker := NULL]
  hhold <- hhold[, smoker := NULL]
  data <- suppressWarnings(suppressMessages(merge(data, hhold, by = "hidp")))

  return(data)
}
