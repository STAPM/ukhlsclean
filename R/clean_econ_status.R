#' Clean Labour Market
#'
#' Generate three variables indicating economic status at different levels of detail,
#' and earnings variables. Variables with "pay" in the name relate to earnings from employment,
#' "earnings" refers to employed and self employed earnings combined.
#'
#' @param data Data table. Understanding Society data produced using the read functions.
#'
#' @export
clean_econ_status <- function(data = NULL) {

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

  data[econ_stat == 1 , econ_stat_7cat := "self-employed"]
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
                                 levels = c("employed","self-employed","unemployed","sick","retired","education","other"),
                                 labels = c("employed","self-employed","unemployed","sick","retired","education","other"))]


  ################################################
  ###### Hours and Earnings Variables ############

  ## merge in CPI inflation figures and deflate

  merge <- merge.data.table(data,
                            ukhlsclean::cpi,
                            by = c("year","month"),
                            all.x = TRUE)

  # combine employed and self employed earnings
  merge[is.na(grss_pay_usual)  & !is.na(grss_semp), grss_earnings_usual := grss_semp]
  merge[!is.na(grss_pay_usual) &  is.na(grss_semp), grss_earnings_usual := grss_pay_usual]
  merge[!is.na(grss_pay_usual) & !is.na(grss_semp), grss_earnings_usual := grss_semp+grss_pay_usual]

  merge[is.na(grss_pay_last)  & !is.na(grss_semp), grss_earnings_last := grss_semp]
  merge[!is.na(grss_pay_last) &  is.na(grss_semp), grss_earnings_last := grss_pay_last]
  merge[!is.na(grss_pay_last) & !is.na(grss_semp), grss_earnings_last := grss_semp+grss_pay_last]

  merge[is.na(grss_lab_inc)  & !is.na(grss_semp), grss_earnings_lab := grss_semp]
  merge[!is.na(grss_lab_inc) &  is.na(grss_semp), grss_earnings_lab := grss_lab_inc]
  merge[!is.na(grss_lab_inc) & !is.na(grss_semp), grss_earnings_lab := grss_semp+grss_lab_inc]

  merge[grss_earnings_lab <= 0, grss_earnings_lab := NA]

  # construct real terms variables
  merge[ , real_grss_pay_usual      := grss_pay_usual*(100/cpi_value)]
  merge[ , real_grss_earnings_usual := grss_pay_usual*(100/cpi_value)]
  merge[ , real_grss_pay_last       := grss_pay_last*(100/cpi_value)]
  merge[ , real_grss_earnings_last  := grss_pay_last*(100/cpi_value)]
  merge[ , real_grss_semp           := grss_semp*(100/cpi_value)]
  merge[ , real_grss_earnings_lab   := grss_earnings_lab*(100/cpi_value)]


  ##################
  ## RETAIN THE CLEANED VARIABLES

  final_data <- merge[, c("id", "hidp", "wave_no",
                          "econ_stat_2cat", "econ_stat_3cat", "econ_stat_7cat",
                          "real_grss_pay_usual", "real_grss_earnings_usual", "real_grss_pay_last", "real_grss_earnings_last",
                          "real_grss_semp", "real_grss_earnings_lab",
                          "grss_pay_usual", "grss_earnings_usual", "grss_pay_last", "grss_earnings_last",
                          "grss_semp", "grss_earnings_lab")]

  var_names <- c("econ_stat_2cat", "econ_stat_3cat", "econ_stat_7cat",
                 "real_grss_pay_usual", "real_grss_earnings_usual", "real_grss_pay_last", "real_grss_earnings_last",
                 "real_grss_semp", "real_grss_earnings_lab",
                 "grss_pay_usual", "grss_earnings_usual", "grss_pay_last", "grss_earnings_last",
                 "grss_semp", "grss_earnings_lab")

  setnames(final_data, var_names, paste0("l_", var_names))


  return(final_data)
}
