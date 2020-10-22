#' Clean Hours and Earnings Variables
#'
#' cleans earnings variables and converts into real terms.
#'
#' @export
clean_hours_earn <- function(data = NULL) {

  ## merge in CPI inflation figures and deflate

  merge <- merge.data.table(data,
                            ukhlsclean::cpi,
                            by = c("year","month"),
                            all.x = TRUE)

  # deal with negative values for gross labour income
  merge[grss_lab_inc <= 0, grss_lab_inc := NA]

  # construct real terms variables
  merge[ , real_grss_pay_usual := grss_pay_usual*(100/cpi_value)]
  merge[ , real_grss_pay_last := grss_pay_last*(100/cpi_value)]
  merge[ , real_grss_lab_inc := grss_lab_inc*(100/cpi_value)]


  merge <- subset(merge,select = -c(cpi_value))

  return(merge)
}
