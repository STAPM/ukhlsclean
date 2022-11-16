#' Clean Work
#'
#' Generate variables (other than pay) relating to work, including hours of work for both
#' employed and self-employed, and industry / occupation.
#'
#' @param data Data table. Understanding Society data produced using the read functions.
#'
#' @export
ukhls_clean_work <- function(data = NULL) {

  ##################
  ## Hours, separated by employment / self-employment

  if ("hours" %in% colnames(data)){
  data[, hours_emp := hours]

  }

  if ("s.emp_hours" %in% colnames(data)){
  data[, hours_semp := s.emp_hours]

  }

  if (c("s.emp_hours","hours") %in% colnames(data)){
  data[is.na(hours_semp) & !is.na(hours_emp), hours := hours_emp]
  data[is.na(hours_emp) & !is.na(hours_semp), hours := hours_semp]
  data[!is.na(hours_emp) & !is.na(hours_semp), hours := hours_semp + hours_emp]

  }

  ##################
  ## INDUSTRY ######

  data[, sic := sic07]

  data[sic07 %in% 1:3, industry := "Agriculture, forestry and fishing"]
  data[sic07 %in% 5:9, industry := "Mining"]
  data[sic07 %in% 10:32, industry := "Manufacturing"]

  data[, sic := factor(sic,

                       levels = c(1:3, 5:9, 10:32),

                       labels = c("Crop and animal production, hunting and related service activities",
                                  "Forestry and logging",
                                  "Fishing and aquaculture",

                                  "Mining of coal and lignite",
                                  "Extraction of crude petroleum and natural gas",
                                  "Mining of metal ores",
                                  "Other mining and quarrying",
                                  "Mining support service activities",

                                  "Manufacture of food products",
                                  "Manufacture of beverages",
                                  "Manufacture of tobacco products",
                                  "Manufacture of textiles",
                                  "Manufacture of wearing apparel",
                                  "Manufacture of leather and related products",
                                  "Manufacture of wood and of products of wood and cork, except furniture",
                                  "Manufacture of paper and paper products",
                                  "Printing and reproduction of recorded media",
                                  "Manufacture of coke and refined petroleum products",
                                  "Manufacture of chemicals and chemical products")) ]


  ##################
  ## RETAIN THE CLEANED VARIABLES

  final_data <- data[, c("id", "hidp", "wave_no",
                         "hours_emp", "hours_semp", "hours", "sic")]

  var_names <- c("hours_emp", "hours_semp", "hours", "sic")

  setnames(final_data, var_names, paste0("w_", var_names))


  return(final_data)
}
