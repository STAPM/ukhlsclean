#' Clean Household Variables
#'
#' Clean all household level variables; household type (composition of adults and children),
#' household size, ownership status, and age of the youngest child in the household.
#'
#' @param data Data table. Understanding Society data produced using the read functions.
#'
#' @export
clean_hhold <- function(data = NULL) {


  ############################################
  ### generate number of adults variable   ###
  ### from hhsize and hhkids for waves 5-9 ###

  data[wave %in% c("UKHLS Wave 5","UKHLS Wave 6",
                   "UKHLS Wave 7","UKHLS Wave 8","UKHLS Wave 9") , hh_numadult := hh_size - hh_numchild]

  ###############################################################
  ### condense the household type variable into 6 categories ###

  data[hh_type %in% c(1,2,3),       hh_type_6cat := "1 adult 0 children"]
  data[hh_type %in% c(4,5),         hh_type_6cat := "1 adult 1+ children"]
  data[hh_type %in% c(6,8,16,17),   hh_type_6cat := "2 adults 0 children"]
  data[hh_type %in% c(10,11,12,18), hh_type_6cat := "2 adults 1+ children"]
  data[hh_type %in% c(19,22),       hh_type_6cat := "3+ adults 0 children"]
  data[hh_type %in% c(20,21,23),    hh_type_6cat := "3+ adults 1+ children"]
  data[, hh_type_6cat := as.factor(hh_type_6cat)]

  ##############################################################
  ## categorical variable for age of youngest child in hhold ###

  data[ hh_numchild02 == 0 &
          hh_numchild34 == 0 &
          hh_numchild511 == 0 &
          hh_numchild1215 == 0 ,       hh_age_yngchl := "no children"]

  data[ hh_numchild02 > 0 &
          hh_numchild34 == 0 &
          hh_numchild511 == 0 &
          hh_numchild1215 == 0 ,       hh_age_yngchl := "age 0-2"]

  data[ hh_numchild34 > 0 &
          hh_numchild511 == 0 &
          hh_numchild1215 == 0 ,       hh_age_yngchl := "age 3-4"]

  data[ hh_numchild511 > 0 &
          hh_numchild1215 == 0 ,       hh_age_yngchl := "age 5-11"]

  data[ hh_numchild1215 > 0 ,       hh_age_yngchl := "age 12-15"]

  data[, hh_age_yngchl := factor(hh_age_yngchl,
                               levels = c("no children",
                                          "age 0-2",
                                          "age 3-4",
                                          "age 5-11",
                                          "age 12-15"))]
  ##############
  ### tenure ###

  data[hh_tenure %in% c(1,2),   hh_hometenure := "owner occupier"]
  data[hh_tenure %in% c(3,4,8), hh_hometenure := "social renter"]
  data[hh_tenure %in% c(5,6,7), hh_hometenure := "private renter"]
  data[, hh_hometenure := factor(hh_hometenure,
                               levels = c("owner occupier",
                                          "private renter",
                                          "social renter")) ]

  ##################
  ## RETAIN THE CLEANED VARIABLES

  final_data <- data[, c("id", "hidp", "wave_no",
                         "hh_hometenure", "hh_age_yngchl", "hh_type_6cat",
                         "hh_numadult", "hh_numchild", "hh_size")]

  return(final_data)
}

