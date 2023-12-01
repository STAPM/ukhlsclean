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
  hhold <- hhold[, c("hidp", "id","s_current_smoker","wave_no")]
  hhold[, curr_smoker := ifelse(s_current_smoker == "smoker", 1, 0)]

  ## create total number of smokers per household per wave

  hhold[, tot_smoker_hhold := sum(curr_smoker), by = c("hidp","wave_no")]
  hhold <- hhold[order(hidp),]

  hhold[, s_hhold_smokers := NA_real_ ]
  hhold[tot_smoker_hhold == 0, s_hhold_smokers := 0]
  hhold[curr_smoker == 0 & tot_smoker_hhold > 0, s_hhold_smokers := 1]
  hhold[curr_smoker == 1 & tot_smoker_hhold > 1, s_hhold_smokers := 1]
  hhold[curr_smoker == 1 & tot_smoker_hhold == 1, s_hhold_smokers := 0]
  hhold[, c("s_current_smoker","curr_smoker","tot_smoker_hhold") := NULL]

  data <- merge(data, hhold, by = c("id","hidp","wave_no"))

  return(data)
}
