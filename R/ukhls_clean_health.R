#' Clean Health and Caring Variables
#'
#' Produce clean versions of variables measuring health outcomes. Also apply the
#' algorithm developed by Gray A, Rivero-Arias O, Clarke P (2006) to estimate
#' EQ-5D utility values from SF-12 responses.
#'
#' @param data Data table. Understanding Society data produced using the read functions.
#'
#' @export
ukhls_clean_health <- function(data = NULL) {

  #############################################################
  ### individual is a carer for someone in their household ####

  data[caring == 1, care_hhold := "carer"]
  data[caring != 1 | is.na(caring), care_hhold := "not_carer"]

  data[, care_hhold := as.factor(care_hhold)]

  #########################################################
  ### individual has a long-standing illness/disability ###

  data[lt_sick == 1, disability := "disability"]
  data[lt_sick == 2, disability := "no_disability"]

  data[, disability := as.factor(disability)]

  ################################
  ### satisfaction with health ###

  data[health_satisf == 1, satisfaction_health := "completely_disatisfied"]
  data[health_satisf == 2, satisfaction_health := "mostly_disatisfied"]
  data[health_satisf == 3, satisfaction_health := "somewhat_disatisfied"]
  data[health_satisf == 4, satisfaction_health := "neutral"]
  data[health_satisf == 5, satisfaction_health := "somewhat_satisfied"]
  data[health_satisf == 6, satisfaction_health := "mostly_satisfied"]
  data[health_satisf == 7, satisfaction_health := "completely_satisfied"]

  data[, satisfaction_health := factor(satisfaction_health,
                                       levels = c("completely_disatisfied", "mostly_disatisfied", "somewhat_disatisfied",
                                                  "neutral", "somewhat_satisfied", "mostly_satisfied",
                                                  "completely_satisfied"))]

  #########################################
  ### satisfaction with life in general ###

  data[life_satisf == 1, satisfaction_life := "completely_disatisfied"]
  data[life_satisf == 2, satisfaction_life := "mostly_disatisfied"]
  data[life_satisf == 3, satisfaction_life := "somewhat_disatisfied"]
  data[life_satisf == 4, satisfaction_life := "neutral"]
  data[life_satisf == 5, satisfaction_life := "somewhat_satisfied"]
  data[life_satisf == 6, satisfaction_life := "mostly_satisfied"]
  data[life_satisf == 7, satisfaction_life := "completely_satisfied"]

  data[, satisfaction_life := factor(satisfaction_life,
                                       levels = c("completely_disatisfied", "mostly_disatisfied", "somewhat_disatisfied",
                                                  "neutral", "somewhat_satisfied", "mostly_satisfied",
                                                  "completely_satisfied"))]

  ##########################
  ### currently pregnant ###

  if("pregout1" %in% colnames(data)) {

  data[pregout1 == 4 | pregout2 == 4 , pregnant := 1]
  data[is.na(pregnant) , pregnant := 0]

  data[, c("pregout1","pregout2") := NULL]

  } else {

  data[, pregnant := NA]
  }

  if("pregout3" %in% colnames(data)) {

    data[pregout3 == 4, pregnant := 1]
    data[is.na(pregnant) , pregnant := 0]

    data[, c("pregout3") := NULL]

  }


  if("pregout4" %in% colnames(data)) {

    data[pregout4 == 4, pregnant := 1]
    data[is.na(pregnant) , pregnant := 0]

    data[, c("pregout4") := NULL]

  }
  if("pregout5" %in% colnames(data)) {

    data[pregout5 == 4, pregnant := 1]
    data[is.na(pregnant) , pregnant := 0]

    data[, c("pregout5") := NULL]

  }

  data[, pregnant := factor(pregnant, levels = 0:1, labels = c("not_pregnant","pregnant"))]

  ###################################
  ##### EQ-5d mapped from SF-12 #####

  ## Use the algorithm from :
  ## https://www.herc.ox.ac.uk/downloads/downloads-supporting-material-1/sf-12-responses-and-eq-5d-utility-values
  ##
  ## Gray A, Rivero-Arias O, Clarke P. Estimating the association between SF-12 responses and EQ-5D utility values
  ## by response mapping. Medical Decision Making 2006; 26(1):18-29.

  ## Rename the SF-12 Variables

  setnames(data,
           c("sf1","sf2a","sf2b","sf3a","sf3b","sf4a","sf4b","sf5","sf6a","sf6b","sf6c","sf7"),
           c("sfstat","sfmode","sfstaira","sfless","sflimit","sflesse","sfcarful","sfpainb","sfcalm","sfener","sflow","sfvisit"))

  eq5d_data <- ukhlsclean::eq5d(data = data,
                                matrix = ukhlsclean::CoefficientMatrix,
                                seed = 0)

  merged_data <- merge(data, eq5d_data, by = c("pidp","id","wave_no"), sort = F, all.x = TRUE)

  ##################
  ## RETAIN THE CLEANED VARIABLES

  final_data <- merged_data[, c("pidp", "id", "hidp", "wave_no",
                                "eq5d_score", "pregnant", "disability", "care_hhold",
                                "satisfaction_health", "satisfaction_life")]

  var_names <- c("eq5d_score", "pregnant", "disability", "care_hhold",
                 "satisfaction_health", "satisfaction_life")

  setnames(final_data, var_names, paste0("h_", var_names))


  return(final_data)

}
