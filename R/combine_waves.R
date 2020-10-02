#' Combine waves of UKHLS
#'
#' Combines years of data when provided as a list of data tables.
#'
#' @param data_list List of data tables to combine.
#'
#' @export
combine_waves <- function(
  data_list
) {

  # combine all the data tables
  data <- data.table::rbindlist(data_list, use.names = T, fill = T)

  # remove the pid and pidp identifiers
  data <- subset(data,select = -c(pid,pidp))

  # order rows and columns
  data <- data[order(id,wave_no),]
  setcolorder(data, c("id","hidp","wave","wave_no","dataset","bhps_sample"))

  # create a year-quarter time variable
  ##data$time <- as.yearqtr(paste0(data$year, "-", data$quarter))

  return(data)
}
