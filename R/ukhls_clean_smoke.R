#' Clean Smoking Variables
#'
#' Clean all variables related to smoking behaviour - current smoking status, smoking history, and
#' level of cigarette consumption for current smokers.
#'
#' @param data Data table. Understanding Society data produced using the read functions.
#'
#' @export
ukhls_clean_smoke <- function(data = NULL) {

  data <- data[order(id,wave_no),]

  #####################################################
  #### Waves 2 and 5 - smoking status, ever smoked ####

  if ("smever" %in% colnames(data)) {

  ## current smoker status
  data[wave_no %in% c(2,5) & smever == 1 & smnow == 1, current_smoker := "smoker"]
  data[wave_no %in% c(2,5) & smever == 1 & smnow == 2, current_smoker := "non_smoker"]
  data[wave_no %in% c(2,5) & smever == 2, current_smoker := "non_smoker"]

  ## ever smoked
  data[wave_no %in% c(2,5) & smever == 2, ever_smoked := "never_smoked"]
  data[wave_no %in% c(2,5) & smever == 1, ever_smoked := "smoked"]

  data[, ever_smoked  := as.factor(ever_smoked)]
  data[, current_smoker := as.factor(current_smoker)]

  } else {

  data[, ever_smoked := NA]
  }

  ###########################################
  #### Wave 3 and 4, current smoker status ##
#
#   if("smoke_freq" %in% colnames(data)) {
#
#     data[wave_no %in% c(3,4) & (smoke_freq == 4 | smoke_freq == 5), current_smoker := "smoker"]
#     data[wave_no %in% c(3,4) & ! (smoke_freq == 4 | smoke_freq == 5), current_smoker := "non_smoker"]
#     data[wave_no %in% c(3,4) & is.na(smoke_freq), current_smoker := NA]
#
#     ### Cannot do ever_smoked because no way of determining if they have never smoked before on a wave-by-wave basis
#   }

  ### Dropped because waves 3 and 4 only include responses age cat 16-21

  ###########################################
  ##### Waves 6 - 11 current smoker status ##

  if ("smoker" %in% colnames(data)) {

  data[smoker == 1, current_smoker := "smoker"]
  data[smoker == 2, current_smoker := "non_smoker"]

  data[, current_smoker := as.factor(current_smoker)]

  }

  if (!("current_smoker" %in% colnames(data)) ){

  data[, current_smoker := NA]
  }

  ########################################
  # age started smoking (waves 2 and 5) ##

  if ("smagbg" %in% colnames(data)){

  data[smagbg == 0, smagbg := NA]
  data[, smk_age_start := smagbg]

  data[, c("smagbg") := NULL]
  } else {

  data[, smk_age_start := NA]
  }

  #################################################################################
  #### Other smokers in household - waves 2, 5-11 (derived from current_smoker ####

  if ("current_smoker" %in% colnames(data)) {

  # indicator for any other household smokers
  data[!(wave_no %in% c(1,3,4)) & current_smoker == "smoker", smoke := 1]
  data[!(wave_no %in% c(1,3,4)) & current_smoker == "non_smoker", smoke := 0]
  data[!(wave_no %in% c(1,3,4)), num_smoker_hhold := sum(smoke, na.rm=TRUE), by = c("wave_no","hidp")]
      # number of other smokers = number of smokers - respondent
  data[!(wave_no %in% c(1,3,4)), num_othersmoker_hhold := num_smoker_hhold - smoke]
      # create a binary indicator
  data[!(wave_no %in% c(1,3,4)) & num_othersmoker_hhold > 0, othersmoker_hhold := "yes"]
  data[!(wave_no %in% c(1,3,4)) & num_othersmoker_hhold == 0, othersmoker_hhold := "no"]

  data[, othersmoker_hhold := as.factor(othersmoker_hhold)]

  } else {

  data[, othersmoker_hhold := NA]
  }

  #######################################################
  #### Number of cigarettes smoked by current smokers

  if ("ncigs" %in% colnames(data)) {

  data[, ncigs := ncigs]

  } else {

    data[, ncigs := NA]
  }

  #######################################################
  #### e-cigarette use (WAVES 7 - 12)

  if ("ecigs" %in% colnames(data)){
    data[ecigs %in% 1, ecig := "never_used_ecig"]
    data[ecigs %in% 2:3, ecig := "not_currently_using_ecig"]
    data[ecigs %in% 4:5, ecig := "infrequent_ecig_use"]
    data[ecigs %in% 6, ecig := "frequent_ecig_use"]

  } else {

    data[, ecig := NA]
  }



  if ("ecigs" %in% colnames(data)){
    data[ecigs %in% 1:3, ecig_current := "non_ecig_user"]
    data[ecigs %in% 4:6, ecig_current := "ecig_user"]

  } else {

    data[, ecig_current := NA]
  }


  ##################
  ## RETAIN THE CLEANED VARIABLES

  final_data <- data[, c("pidp", "id", "hidp", "wave_no",
                         "current_smoker", "ncigs", "ever_smoked", "smk_age_start",
                         "ecig", "ecig_current")]

  var_names <- c("current_smoker", "ncigs", "ever_smoked", "smk_age_start",
                 "ecig", "ecig_current")

  setnames(final_data, var_names, paste0("s_", var_names))


  return(final_data)
}


