#' Clean Hours and Earnings Variables
#'
#' cleans earnings variables and converts into real terms. Variables with "pay"
#' in the name relate to earnings from employment, "earnings" refers to employed
#' and self employed earnings combined.
#'
#' @export
clean_hours_earn <- function(data = NULL) {

  cat(crayon::green("\tCleaning hours and earnings variables\n"))

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


  merge <- subset(merge,select = -c(cpi_value,grss_lab_inc))

  return(merge)
}
