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
  data[wave_no %in% c(7,9,11) & auditc1 == 1, current_abstainer := 0]
  data[wave_no %in% c(7,9,11) & auditc1 == 2, current_abstainer := 1]

  # always been a non-drinker (yes = always abstainer)
  data[wave_no %in% c(7,9,11) & auditc1 == 1, always_abstainer := 0]
  data[wave_no %in% c(7,9,11) & auditc2 == 2, always_abstainer := 0]
  data[wave_no %in% c(7,9,11) & auditc2 == 1, always_abstainer := 1]

  # drinks on a typical day
  data[wave_no %in% c(7,9,11) & current_abstainer == 1 , auditc4 := 0]

  # frequency of binge drinking (6+ drinks in one day)
  data[wave_no %in% c(7,9,11) & current_abstainer == 1 , auditc5 := 1]



  # make factors

  data[, current_abstainer := factor(current_abstainer,
                                     levels = c(0,1),
                                     labels = c("no","yes")) ]
  data[, always_abstainer := factor(always_abstainer,
                                    levels = c(0,1),
                                    labels = c("no","yes")) ]
  data[, ndrinks := factor(auditc4,
                           levels = c(0,1,2,3,4,5),
                           labels = c("0","1-2","3-4","5-6","7-9","10+")) ]

  data[, freq_binge := factor(auditc5,
                              levels = 1:5,
                              labels = c("Never","Less than monthly","Monthly","Weekly","Daily")) ]


  } else {

  data[, current_abstainer := NA]
  data[, always_abstainer := NA]
  data[, ndrinks := NA]
  data[, freq_binge := NA]

  }

  ##################
  ## RETAIN THE CLEANED VARIABLES

  final_data <- data[, c("pidp", "id", "hidp", "wave_no",
                         "current_abstainer", "always_abstainer", "ndrinks", "freq_binge")]

  var_names <- c("current_abstainer", "always_abstainer", "ndrinks", "freq_binge")

  setnames(final_data, var_names, paste0("a_", var_names))

  return(final_data)

}
