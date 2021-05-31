#' Clean Smoking Variables
#'
#' Clean all variables related to smoking behaviour - current smoking status, smoking history, and
#' level of cigarette consumption for current smokers.
#'
#' @export
clean_smoke <- function(data = NULL) {

  cat(crayon::green("\tCleaning smoking variables\n"))

  # ensure data is sorted
  data <- data[order(id,wave_no),]

  # non-smoking status
  data[wave_no %in% c(2,5) & smever == 2, non_smoker := "yes"]
  data[wave_no %in% c(2,5) & smever == 1 & smnow == 2, non_smoker := "yes"]
  data[wave_no %in% c(2,5) & smnow == 1, non_smoker := "no"]

  data[wave_no %in% c(6:10) & smoker == 1, non_smoker := "no"]
  data[wave_no %in% c(6:10) & smoker == 2, non_smoker := "yes"]

  # current smoker status
  data[wave_no %in% c(2,5) & smever == 1 & smnow == 1, current_smoker := "yes"]
  data[wave_no %in% c(2,5) & smever == 1 & smnow == 2, current_smoker := "no"]
  data[wave_no %in% c(2,5) & smever == 2, current_smoker := "no"]
  data[wave_no %in% c(6:10) & smoker == 1, current_smoker := "yes"]
  data[wave_no %in% c(6:10) & smoker == 2, current_smoker := "no"]

  # ever smoked
  data[wave_no %in% c(2,5) & smever == 2, ever_smoked := "no"]
  data[wave_no %in% c(2,5) & smever == 1, ever_smoked := "yes"]

  # age started smoking
  data[smagbg == 0, smagbg := NA]
  data[, smk_age_start := pmin(smagbg,na.rm=TRUE), by = "id"]

  data[, smk_age_start := nafill(smk_age_start, type = "nocb"), by = "id"]
  data[, smk_age_start := nafill(smk_age_start, type = "locf"), by = "id"]

  # indicator for any other household smokers
  data[!(wave_no %in% c(1,3,4)) & current_smoker == "yes", smoke := 1]
  data[!(wave_no %in% c(1,3,4)) & current_smoker == "no", smoke := 0]
  data[!(wave_no %in% c(1,3,4)), num_smoker_hhold := sum(smoke, na.rm=TRUE), by = c("wave","hidp")]
      # number of other smokers = number of smokers - respondent
  data[!(wave_no %in% c(1,3,4)), num_othersmoker_hhold := num_smoker_hhold - smoke]
      # create a binary indicator
  data[!(wave_no %in% c(1,3,4)) & num_othersmoker_hhold > 0, othersmoker_hhold := "yes"]
  data[!(wave_no %in% c(1,3,4)) & num_othersmoker_hhold == 0, othersmoker_hhold := "no"]


  # make factors
  data[, non_smoker   := as.factor(data$non_smoker)]
  data[, ever_smoked  := as.factor(data$ever_smoked)]
  data[, current_smoker := as.factor(data$current_smoker)]
  data[, othersmoker_hhold := as.factor(data$othersmoker_hhold)]

  # remove raw variables no longer needed
  data <- subset(data,select = -c(smever, smnow, smcigs, smncigs, aglquit,
                                  smagbg, smoker, smoke, num_smoker_hhold,
                                  num_othersmoker_hhold))


  return(data)
}


