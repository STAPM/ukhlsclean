#' Clean Labour Market
#'
#' Generate three variables indicating economic status at different levels of detail,
#' and earnings variables. Variables with "pay" in the name relate to earnings from employment,
#' "earnings" refers to employed and self employed earnings combined.
#'
#' @param data Data table. Understanding Society data produced using the read functions.
#'
#' @export
ukhls_clean_econstat <- function(data = NULL) {

  ### 2 categories - work/not work #####

  data[econ_stat == 1 , econ_stat_2cat := "employed"]
  data[econ_stat == 2 , econ_stat_2cat := "employed"]
  data[econ_stat == 3 , econ_stat_2cat := "not_employed"]
  data[econ_stat == 4 , econ_stat_2cat := "not_employed"]
  data[econ_stat == 5 , econ_stat_2cat := "not_employed"]
  data[econ_stat == 6 , econ_stat_2cat := "not_employed"]
  data[econ_stat == 7 , econ_stat_2cat := "not_employed"]
  data[econ_stat == 8 , econ_stat_2cat := "not_employed"]
  data[econ_stat == 9 , econ_stat_2cat := "not_employed"]
  data[econ_stat == 10, econ_stat_2cat := "not_employed"]
  data[econ_stat == 11, econ_stat_2cat := "not_employed"]

  data[,econ_stat_2cat := factor(econ_stat_2cat,
                                 levels = c("employed","not_employed"),
                                 labels = c("employed","not_employed"))]

  ### 3 categories - employed/unemployed/inactive

  data[econ_stat == 1 , econ_stat_3cat := "employed"]
  data[econ_stat == 2 , econ_stat_3cat := "employed"]
  data[econ_stat == 3 , econ_stat_3cat := "unemployed"]
  data[econ_stat == 4 , econ_stat_3cat := "inactive"]
  data[econ_stat == 5 , econ_stat_3cat := "inactive"]
  data[econ_stat == 6 , econ_stat_3cat := "inactive"]
  data[econ_stat == 7 , econ_stat_3cat := "inactive"]
  data[econ_stat == 8 , econ_stat_3cat := "inactive"]
  data[econ_stat == 9 , econ_stat_3cat := "inactive"]
  data[econ_stat == 10, econ_stat_3cat := "inactive"]
  data[econ_stat == 11, econ_stat_3cat := "inactive"]

  data[,econ_stat_3cat := factor(econ_stat_3cat,
                                 levels = c("employed","unemployed","inactive"),
                                 labels = c("employed","unemployed","inactive"))]

  ### 7 categories

  data[econ_stat == 1 , econ_stat_7cat := "self_employed"]
  data[econ_stat == 2 , econ_stat_7cat := "employed"]
  data[econ_stat == 3 , econ_stat_7cat := "unemployed"]
  data[econ_stat == 4 , econ_stat_7cat := "retired"]
  data[econ_stat == 5 , econ_stat_7cat := "other"]
  data[econ_stat == 6 , econ_stat_7cat := "other"]
  data[econ_stat == 7 , econ_stat_7cat := "education"]
  data[econ_stat == 8 , econ_stat_7cat := "sick"]
  data[econ_stat == 9 , econ_stat_7cat := "other"]
  data[econ_stat == 10, econ_stat_7cat := "other"]
  data[econ_stat == 11, econ_stat_7cat := "other"]

  data[,econ_stat_7cat := factor(econ_stat_7cat,
                                 levels = c("employed","self_employed","unemployed","sick","retired","education","other"),
                                 labels = c("employed","self_employed","unemployed","sick","retired","education","other"))]

  ######################################
  ### Constructing NS-SEC ##############

  data[nssec_5cat == 1, ns_sec := "Management & professional"]
  data[nssec_5cat == 2, ns_sec := "Intermediate"]
  data[nssec_5cat == 3, ns_sec := "Small employers & own account"]
  data[nssec_5cat == 4, ns_sec := "Lower supervisory & technical"]
  data[nssec_5cat == 5, ns_sec := "Semi-routine & routine"]
  data[econ_stat_2cat == "not_employed", ns_sec := "Not employed"]#

  data[, ns_sec := factor(ns_sec,
                         levels = c("Management & professional","Intermediate","Small employers & own account","Lower supervisory & technical","Semi-routine & routine","Not employed"),
                         labels = c("Management & professional","Intermediate","Small employers & own account","Lower supervisory & technical","Semi-routine & routine","Not employed"))]


  ######################################
  ### Ever-employment ##################

  # data[jbhas == 1, ever_empl := 1] # currently working
  # data[is.na(jbhas), ever_empl := NA]
  # data[jbhas == 2 & jboff == 1, ever_empl := 1] # not currently working, has job
  # data[jbhas == 2 & is.na(jboff), ever_empl := NA]
  # data[jbhas == 2 & jboff == 3, ever_empl := 1] # currently not working, on wat list
  # data[jbhas == 2 & jboff == 2, ever_empl := 0] # not currently working and not working because no job|
  # data[jbhad == 1, ever_empl := 1] # not currently working, not working because no job, and had job
  # data[jbhad == 2, ever_empl := 0] # not currently working, not working because no job and never worked


  ################################################
  ###### Hours and Earnings Variables ############

  ## merge in CPI inflation figures and deflate

  merge <- merge.data.table(data,
                            ukhlsclean::cpi,
                            by = c("year","month"),
                            all.x = TRUE)

  ####################################################
  # combine employed and self employed earnings

  ## usual monthly pay
  merge[is.na(grss_pay_usual)  & !is.na(grss_semp), grss_earnings_usual := grss_semp]
  merge[!is.na(grss_pay_usual) &  is.na(grss_semp), grss_earnings_usual := grss_pay_usual]
  merge[!is.na(grss_pay_usual) & !is.na(grss_semp), grss_earnings_usual := grss_semp+grss_pay_usual]

  ## last monthly pay
  merge[is.na(grss_pay_last)  & !is.na(grss_semp), grss_earnings_last := grss_semp]
  merge[!is.na(grss_pay_last) &  is.na(grss_semp), grss_earnings_last := grss_pay_last]
  merge[!is.na(grss_pay_last) & !is.na(grss_semp), grss_earnings_last := grss_semp+grss_pay_last]

  #####################################################
  # separate employment and self-employment earnings

  ## usual monthly pay
  merge[, grss_earnings_usual_empl := grss_pay_usual]
  merge[, grss_earnings_last_empl := grss_pay_last]

  ## usual monthly pay
  merge[, grss_earnings_usual_semp := grss_semp]

  #####################################################


  # construct real terms variables
  merge[ , real_grss_earnings_usual_empl := grss_earnings_usual_empl*(100/cpi_value)]
  merge[ , real_grss_earnings_usual      := grss_earnings_usual*(100/cpi_value)] ## should this be grss_earnings_usual? # changed from grss_pay_usual
  merge[ , real_grss_earnings_last_empl  := grss_earnings_last_empl*(100/cpi_value)]
  merge[ , real_grss_earnings_last       := grss_earnings_last*(100/cpi_value)] ## should this be grss_earnings_last? # changed from grss_pay_last
  merge[ , real_grss_earnings_usual_semp := grss_earnings_usual_semp*(100/cpi_value)]

  ##################
  ## RETAIN THE CLEANED VARIABLES

  final_data <- merge[, c("pidp", "id", "hidp", "wave_no",
                          "econ_stat_2cat", "econ_stat_3cat", "econ_stat_7cat", "ns_sec",
                          "grss_earnings_usual", "grss_earnings_last",
                          "grss_earnings_usual_empl", "grss_earnings_last_empl",
                          "real_grss_earnings_usual", "real_grss_earnings_last",
                          "real_grss_earnings_usual_empl", "real_grss_earnings_last_empl",
                          "real_grss_earnings_usual_semp")]

  var_names <- c("econ_stat_2cat", "econ_stat_3cat", "econ_stat_7cat", "ns_sec",
                 "grss_earnings_usual", "grss_earnings_last",
                 "grss_earnings_usual_empl", "grss_earnings_last_empl",
                 "real_grss_earnings_usual", "real_grss_earnings_last",
                 "real_grss_earnings_usual_empl", "real_grss_earnings_last_empl",
                 "real_grss_earnings_usual_semp")

  setnames(final_data, var_names, paste0("l_", var_names))


  return(final_data)
}
