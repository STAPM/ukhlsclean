#' Clean Smoking Variables
#'
#' Clean all variables related to smoking behaviour - current smoking status, smoking history, and
#' level of cigarette consumption for current smokers.
#'
#' @param data Data table. Understanding Society data produced using the read functions.
#'
#' @export
clean_smoke <- function(data = NULL) {

  #cat(crayon::green("\tCleaning smoking variables\n"))

  # ensure data is sorted
  data <- data[order(id,wave_no),]

  if ("smever" %in% colnames(data)) {

  ########################
  ### non-smoking status #

  data[wave_no %in% c(2,5) & smever == 2, non_smoker := "yes"]
  data[wave_no %in% c(2,5) & smever == 1 & smnow == 2, non_smoker := "yes"]
  data[wave_no %in% c(2,5) & smnow == 1, non_smoker := "no"]

  ###########################
  ## current smoker status ##
  data[wave_no %in% c(2,5) & smever == 1 & smnow == 1, current_smoker := "yes"]
  data[wave_no %in% c(2,5) & smever == 1 & smnow == 2, current_smoker := "no"]
  data[wave_no %in% c(2,5) & smever == 2, current_smoker := "no"]

  ##################
  ## ever smoked ###
  data[wave_no %in% c(2,5) & smever == 2, ever_smoked := "no"]
  data[wave_no %in% c(2,5) & smever == 1, ever_smoked := "yes"]



  data[, non_smoker   := as.factor(data$non_smoker)]
  data[, ever_smoked  := as.factor(data$ever_smoked)]
  data[, current_smoker := as.factor(data$current_smoker)]

  } else {

  data[, ever_smoked := NA]
  }



  if ("smoker" %in% colnames(data)) {

  ##########################
  # current smoker status ##

  data[wave_no %in% c(6:11) & smoker == 1, current_smoker := "yes"]
  data[wave_no %in% c(6:11) & smoker == 2, current_smoker := "no"]

  #######################
  # non-smoker status ###

  data[wave_no %in% c(6:11) & smoker == 1, non_smoker := "no"]
  data[wave_no %in% c(6:11) & smoker == 2, non_smoker := "yes"]

  data[, c("smoker") := NULL]

  }

  if (!("smoker" %in% colnames(data)) & !("smever" %in% colnames(data)) ){

  data[, current_smoker := NA]
  data[, non_smoker := NA]
  }

  if ("smagbg" %in% colnames(data)) {

  ########################
  # age started smoking ##

  data[smagbg == 0, smagbg := NA]
  data[, smk_age_start := pmin(smagbg,na.rm=TRUE), by = "id"]

  data[, smk_age_start := nafill(smk_age_start, type = "nocb"), by = "id"]
  data[, smk_age_start := nafill(smk_age_start, type = "locf"), by = "id"]

  data[, c("smagbg") := NULL]
  } else {

  data[, smk_age_start := NA]
  }

  if ("current_smoker" %in% colnames(data)) {

  # indicator for any other household smokers
  data[!(wave_no %in% c(1,3,4)) & current_smoker == "yes", smoke := 1]
  data[!(wave_no %in% c(1,3,4)) & current_smoker == "no", smoke := 0]
  data[!(wave_no %in% c(1,3,4)), num_smoker_hhold := sum(smoke, na.rm=TRUE), by = c("wave","hidp")]
      # number of other smokers = number of smokers - respondent
  data[!(wave_no %in% c(1,3,4)), num_othersmoker_hhold := num_smoker_hhold - smoke]
      # create a binary indicator
  data[!(wave_no %in% c(1,3,4)) & num_othersmoker_hhold > 0, othersmoker_hhold := "yes"]
  data[!(wave_no %in% c(1,3,4)) & num_othersmoker_hhold == 0, othersmoker_hhold := "no"]

  data[, othersmoker_hhold := as.factor(data$othersmoker_hhold)]

  } else {

  data[, othersmoker_hhold := NA]
  }


  ##################
  ## RETAIN THE CLEANED VARIABLES

  final_data <- data[, c("id", "hidp", "wave_no",
                         "current_smoker", "ever_smoked", "non_smoker", "smk_age_start",
                         "othersmoker_hhold")]

  var_names <- c("current_smoker", "ever_smoked", "non_smoker", "smk_age_start",
                 "othersmoker_hhold")

  setnames(final_data, var_names, paste0("s_", var_names))


  return(final_data)
}


