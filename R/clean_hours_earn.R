#' Clean Hours and Earnings Variables
#'
#' Reads and does basic cleaning on the UKHLS demographic variables -
#' age, gender, ethnicity, region and urban vs rural.
#'
#' @export
clean_hours_earn <- function(data = NULL) {

  ## merge in CPI inflation figures and deflate

  merge <- merge.data.table(data,
                            ukhlsclean::cpi,
                            by = c("year","month"),
                            all.x = TRUE)
  ### marital status
  merge[ , real_grss_pay_usual := grss_pay_usual*(100/cpi_value)]
  merge[ , real_grss_pay_last := grss_pay_last*(100/cpi_value)]


  data <- subset(data,select = -c(grss_lab_inc))

  return(data)
}
