#' Clean Household Variables
#'
#' Clean all household level variables; household type (composition of adults and children),
#' household size, ownership status, and age of the youngest child in the household.
#'
#' @param data Data table. Understanding Society data produced using the read functions.
#'
#' @export
ukhls_clean_hhold <- function(data = NULL,
                              inflation = ukhlsclean::cpih) {


  ############################################
  ### generate number of adults variable   ###
  ### from hhsize and hhkids for waves 5-9 ###

  data[ , hh_numadult := hh_size - hh_numchild]

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
                                          "age 12-15"),
                               labels = c("no_children",
                                          "0-2",
                                          "3-4",
                                          "5-11",
                                          "12-15"))]
  ##############
  ### tenure ###

  data[hh_tenure %in% c(1,2),   hh_hometenure := "owner_occupier"]
  data[hh_tenure %in% c(3,4,8), hh_hometenure := "social_renter"]
  data[hh_tenure %in% c(5,6,7), hh_hometenure := "private_renter"]
  data[, hh_hometenure := factor(hh_hometenure,
                               levels = c("owner_occupier",
                                          "private_renter",
                                          "social_renter")) ]

  #################################
  ### income and social benefit ###
  # notes: potential for sorting into income groups?

  #gross
  data[, hh_fi_mo_grss := hh_fihhmngrs1_dv]
  data[, hh_fi_mo_grsslabour := hh_fihhmnlabgrs_dv]

  #net
  data[, hh_fi_mo_net := hh_fihhmnnet1_dv]
  data[, hh_fi_mo_netlabour := hh_fihhmnlabnet_dv, ]
  data[, hh_fi_mo_socben := hh_fihhmnsben_dv]

  #construct real term
  merge[ , hh_fi_real_mo_grss       := hh_fi_mo_grss*(100/index)]
  merge[ , hh_fi_real_mo_grsslabour := hh_fi_mo_grsslabour*(100/index)]
  merge[ , hh_fi_real_mo_net        := hh_fi_mo_net*(100/index)]
  merge[ , hh_fi_real_mo_netlabour  := hh_fi_mo_netlabour*(100/index)]
  merge[ , hh_fi_real_mo_socben     := hh_fi_mo_socben*(100/index)]

  ##################
  ## RETAIN THE CLEANED VARIABLES

  final_data <- data[, c("pidp", "id", "hidp", "wave_no",
                         "hh_hometenure", "hh_age_yngchl", "hh_type_6cat",
                         "hh_numadult", "hh_numchild", "hh_size",
                         "hh_fi_mo_grss", "hh_fi_mo_grsslabour",
                         "hh_fi_mo_net", "hh_fi_mo_netlabour", "hh_fi_mo_socben",
                         "hh_fi_real_mo_grss","hh_fi_real_mo_grsslabour",
                         "hh_fi_real_mo_net","hh_fi_real_mo_netlabour","hh_fi_real_mo_socben")]

  return(final_data)
}

