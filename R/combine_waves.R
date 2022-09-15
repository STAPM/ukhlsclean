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
  if("pid" %in% colnames(data)) {

  data[, "pid" := NULL]

  }

  data[, "pidp" := NULL]

  # order rows and columns
  data <- data[order(id,wave_no),]

  # create a year-quarter time variable
  ##data$time <- as.yearqtr(paste0(data$year, "-", data$quarter))

  # create flags for each wave

  data[, wave_1  := ifelse(wave_no == 1,1,0)]
  data[, wave_2  := ifelse(wave_no == 2,1,0)]
  data[, wave_3  := ifelse(wave_no == 3,1,0)]
  data[, wave_4  := ifelse(wave_no == 4,1,0)]
  data[, wave_5  := ifelse(wave_no == 5,1,0)]
  data[, wave_6  := ifelse(wave_no == 6,1,0)]
  data[, wave_7  := ifelse(wave_no == 7,1,0)]
  data[, wave_8  := ifelse(wave_no == 8,1,0)]
  data[, wave_9  := ifelse(wave_no == 9,1,0)]
  data[, wave_10 := ifelse(wave_no == 10,1,0)]
  data[, wave_11 := ifelse(wave_no == 11,1,0)]

  data[, wave_1  := max(wave_1) , by = "id"]
  data[, wave_2  := max(wave_2) , by = "id"]
  data[, wave_3  := max(wave_3) , by = "id"]
  data[, wave_4  := max(wave_4) , by = "id"]
  data[, wave_5  := max(wave_5) , by = "id"]
  data[, wave_6  := max(wave_6) , by = "id"]
  data[, wave_7  := max(wave_7) , by = "id"]
  data[, wave_8  := max(wave_8) , by = "id"]
  data[, wave_9  := max(wave_9) , by = "id"]
  data[, wave_10 := max(wave_10), by = "id"]
  data[, wave_11 := max(wave_10), by = "id"]

  ## calculate number of waves each individual is in

  data[, nwaves := .N, by = c("id")]

  # order columns
  setcolorder(data, c("id","hidp","wave_no","bhps_sample","nwaves",
                      "wave_1","wave_2","wave_3","wave_4","wave_5","wave_6",
                      "wave_7","wave_8","wave_9","wave_10","wave_11"))


  return(data)
}
