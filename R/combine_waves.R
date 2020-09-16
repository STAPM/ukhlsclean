#' Combine waves of UKHLS
#'
#' Combines years of data when provided as a list of data tables.
#'
#' @param data_list List of data tables to combine.
#' @param ukhls TRUE if `data_list` contains UKHLS waves
#'
#'
combine_waves <- function(
  data_list,
  ukhls = TRUE
) {

  data <- data.table::rbindlist(data_list, use.names = T, fill = T)

  if (ukhls == TRUE) {
  data <- data[order(pid,pidp,wave_no),]
  setcolorder(data, c("pid", "pidp", "wave_no"))
  } else {
  data <- data[order(pid,pidp,wave_no),]
  setcolorder(data, c("pid", "wave_no"))
  }
  # create a year-quarter time variable
  ##data$time <- as.yearqtr(paste0(data$year, "-", data$quarter))


  return(data)
}
