#' Clean Alcohol Variables
#'
#' Clean all alcohol related variables.
#'
#' @param data Data table. Understanding Society data produced using the read functions.
#'
#' @export
ukhls_clean_alcohol <- function(data = NULL) {

  #cat(crayon::green("\tCleaning alcohol variables\n"))

  # ensure data is sorted
  data <- data[order(id, wave_no),]

  #### wave 7 and 9 variables
  if("auditc1" %in% colnames(data)) {


  # drink in the last 12 months (no = current abstainer)
  data[auditc1 == 1, current_abstainer := 0]
  data[auditc1 == 2, current_abstainer := 1]

  # always been a non-drinker (yes = always abstainer)
  data[auditc1 == 1, always_abstainer := 0]
  data[auditc2 == 2, always_abstainer := 0]
  data[auditc2 == 1, always_abstainer := 1]

  ##################
  # make factors

  data[, current_abstainer := factor(current_abstainer,
                                     levels = c(0,1),
                                     labels = c("no","yes")) ]
  data[, always_abstainer := factor(always_abstainer,
                                    levels = c(0,1),
                                    labels = c("no","yes")) ]

  data[, auditc3_freq_drinks := factor(auditc3,
                                       levels = 1:5,
                                       labels = c("Never","Monthly or less","2-4 times per month","2-3 per week","4+ times per week")) ]

  data[, auditc4_ndrinks := factor(auditc4,
                                   levels = 1:5,
                                   labels = c("1-2","3-4","5-6","7-9","10+")) ]

  data[, auditc5_freq_binge := factor(auditc5,
                                      levels = 1:5,
                                      labels = c("Never","Less than monthly","Monthly","Weekly","Daily")) ]

  #########################
  # construct audit-c score

  ### score based on the sum of audit scores for c3-c5, if the individual has
  ### had a drink in the last 12 months (auditc1 == 1)

  data[auditc1 == 1, audit_score := auditc3 + auditc4 + auditc5]
  data[auditc1 == 2, audit_score := 0]



  } else {

  data[, current_abstainer := NA]
  data[, always_abstainer := NA]
  data[, auditc3_freq_drinks := NA]
  data[, auditc4_ndrinks := NA]
  data[, auditc5_freq_binge := NA]
  data[, audit_score := NA]

  }

  ##################
  ## RETAIN THE CLEANED VARIABLES

  final_data <- data[, c("pidp", "id", "hidp", "wave_no",
                         "current_abstainer", "always_abstainer",
                         "auditc3_freq_drinks", "auditc4_ndrinks", "auditc5_freq_binge", "audit_score")]

  var_names <- c("current_abstainer", "always_abstainer",
                 "auditc3_freq_drinks", "auditc4_ndrinks", "auditc5_freq_binge", "audit_score")

  setnames(final_data, var_names, paste0("a_", var_names))

  return(final_data)

}
