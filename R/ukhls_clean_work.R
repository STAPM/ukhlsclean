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

  # if ("hours" %in% colnames(data)){
  # data[, hours_empl := hours]
  #
  # } else {
  # data[, hours_empl := NA]
  #
  # }
  #
  # if ("s.emp_hours" %in% colnames(data)){
  # data[, hours_semp := s.emp_hours]
  #
  # } else {
  # data[, hours_semp := NA]
  #
  # }
  #
  # if ( all(c("hours_semp","hours_emp") %in% colnames(data)) ){
  # data[is.na(hours_semp) & !is.na(hours_empl), hours := hours_empl]
  # data[is.na(hours_empl) & !is.na(hours_semp), hours := hours_semp]
  # data[!is.na(hours_empl) & !is.na(hours_semp), hours := hours_semp + hours_empl] # this is broken ? summation involving NA returns NA
  #
  # } else {
  #
  # data[, hours := NA]
  # }

  data[, hours_empl := hours]
  data[, hours_semp := s.emp_hours]
  data[!is.na(hours_empl), hours := hours_empl]
  data[!is.na(hours_semp), hours := hours_semp]
  data[!is.na(hours_empl) & !is.na(hours_semp), hours := hours_empl+hours_semp]


  ##########################
  ## SICKNESS ABSENCE ######

  data[jbhas == 1, absent := 0]

  # data[jbhas == 1, absent_sick := 0]
  # data[jbhas == 1, absent_matleave := 0]
  # data[jbhas == 1, absent_annualleave := 0]
  # data[jbhas == 1, absent_other := 0]

  # data[is.na(jbhas), absent_sick := NA]
  # data[is.na(jbhas), absent_matleave := NA]
  # data[is.na(jbhas), absent_annualleave := NA]
  # data[is.na(jbhas), absent_other := NA]

  # data[jbhas == 2 & jboff != 1, absent_sick := 0]
  # data[jbhas == 2 & jboff != 1, absent_matleave := 0]
  # data[jbhas == 2 & jboff != 1, absent_annualleave := 0]
  # data[jbhas == 2 & jboff != 1, absent_other := 0]

  data[jbhas == 2 & jboff == 1, absent := 1]

  data[, absent_sick := absent]
  data[!is.na(absent) & jboffy != 3, absent_sick := 0]
  data[, absent_matleave := absent]
  data[!is.na(absent) & jboffy != 1, absent_matleave := 0]
  data[, absent_annualleave := absent]
  data[!is.na(absent) & jboffy != 2, absent_annualleave := 0]
  data[, absent_other := absent]
  data[!is.na(absent) & jboffy == 3, absent_other := 0] ## this makes absent other equal to NOT absent because of sick
  # data[!is.na(absent) & (jboffy != 1 | jboffy != 2 | jboffy != 3), absent_other := 0] ## this makes absent other equal to NOT absent because of sick

  #
  # data[jbhas == 2 & jboffy == 1, absent_matleave := 1]
  # data[jbhas == 2 & jboffy == 2, absent_annualleave := 1]
  # data[jbhas == 2 & (jboffy == 1 | jboffy == 2 | jboffy == 3), absent_other := 0]
  #
  # data[jbhas == 2 & (jboffy != 1 & jboffy != 2 & jboffy != 3), absent_sick := 0]
  # data[jbhas == 2 & (jboffy != 1 & jboffy != 2 & jboffy != 3), absent_matleave := 0]
  # data[jbhas == 2 & (jboffy != 1 & jboffy != 2 & jboffy != 3), absent_annualleave := 0]
  # data[jbhas == 2 & (jboffy != 1 & jboffy != 2 & jboffy != 3), absent_other := 1]


  ##################
  ## INDUSTRY ######

  data[, sic := sic07]

  data[sic07 %in% 1:3, sic_1dig := "Agriculture, forestry and fishing"]
  data[sic07 %in% 5:9, sic_1dig := "Mining and quarrying"]
  data[sic07 %in% 10:33, sic_1dig := "Manufacturing"]
  data[sic07 %in% 35:39, sic_1dig := "Utilities"]
  data[sic07 %in% 41:43, sic_1dig := "Construction"]
  data[sic07 %in% 45, sic_1dig := "Motor trades"]
  data[sic07 %in% 46, sic_1dig := "Wholesale trade"]
  data[sic07 %in% 47, sic_1dig := "Retail trade"]
  data[sic07 %in% 49:53, sic_1dig := "Transport and storage"]
  data[sic07 %in% 55:56, sic_1dig := "Accommodation and food services"]
  data[sic07 %in% 58:63, sic_1dig := "Information and communication"]
  data[sic07 %in% 64:66, sic_1dig := "Finance and insurance"]
  data[sic07 %in% 68, sic_1dig := "Property"]
  data[sic07 %in% 69:75, sic_1dig := "Professional, scientific, and technical"]
  data[sic07 %in% 77:82, sic_1dig := "Business admin and support services"]
  data[sic07 %in% 84, sic_1dig := "Public administration"]
  data[sic07 %in% 85, sic_1dig := "Education"]
  data[sic07 %in% 86:88, sic_1dig := "Health"]
  data[sic07 %in% 90:93, sic_1dig := "Arts, entertainment, and recreation"]
  data[sic07 %in% 94:99, sic_1dig := "Other"]

  ind_levels <- c("Agriculture, forestry and fishing", "Mining and quarrying", "Manufacturing", "Utilities",
                  "Construction", "Motor trades", "Wholesale trade", "Retail trade", "Transport and storage",
                  "Accommodation and food services", "Information and communication", "Finance and insurance",
                  "Property", "Professional, scientific, and technical", "Business admin and support services",
                  "Public administration", "Education", "Health", "Arts, entertainment, and recreation", "Other")

  data[, sic_1dig := factor(sic_1dig, levels = ind_levels)]

  data[, sic_2dig := factor(sic,

                       levels = c(1:3, 5:9, 10:33, 35:39,
                                  41:43, 45, 46, 47, 49:53,
                                  55:56, 58:63, 64:66,
                                  68, 69:75, 77:82,
                                  84, 85, 86:88, 90:93, 94:99),

                       labels = c(## Agriculture, forestry and fishing
                                  "Crop and animal production, hunting and related service activities",
                                  "Forestry and logging",
                                  "Fishing and aquaculture",
                                  ## Mining and quarrying
                                  "Mining of coal and lignite",
                                  "Extraction of crude petroleum and natural gas",
                                  "Mining of metal ores",
                                  "Other mining and quarrying",
                                  "Mining support service activities",
                                  ## Manufacturing
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
                                  "Manufacture of chemicals and chemical products",
                                  "Manufacture of basic pharmaceutical products and pharmaceutical preparations",
                                  "Manufacture of rubber and plastic products",
                                  "Manufacture of other non-metallic mineral products",
                                  "Manufacture of basic metals",
                                  "Manufacture of fabricated metal products, except machinery and equipment",
                                  "Manufacture of computer, electronic and optical products",
                                  "Manufacture of electrical equipment",
                                  "Manufacture of machinery and equipment n.e.c.",
                                  "Manufacture of motor vehicles, trailers and semi-trailers",
                                  "Manufacture of other transport equipment",
                                  "Manufacture of furniture",
                                  "Other manufacturing",
                                  "Repair and installation of machinery and equipment",
                                  ## Utilities
                                  "Electricity, gas, steam and air conditioning supply",
                                  "Water collection, treatment and supply",
                                  "Sewerage",
                                  "Waste collection, treatment and disposal activities; materials recovery",
                                  "Remediation activities and other waste management services",
                                  ## Construction
                                  "Construction of buildings",
                                  "Civil engineering",
                                  "Specialised construction activities",
                                  ## Motor trades
                                  "Wholesale and retail trade and repair of motor vehicles and motorcycles",
                                  ## Wholesale trade
                                  "Wholesale trade, except of motor vehicles and motorcycles",
                                  ## Retail trade
                                  "Retail trade, except of motor vehicles and motorcycles",
                                  ## Transport and storage
                                  "Land transport and transport via pipelines",
                                  "Water transport",
                                  "Air transport",
                                  "Warehousing and support activities for transportation",
                                  "Postal and courier activities",
                                  ## Accommodation and food services
                                  "Accommodation",
                                  "Food and beverage service activities",
                                  ## Information and communication
                                  "Publishing activities",
                                  "Motion picture, video and television programme production, sound recording and music publishing",
                                  "Programming and broadcasting activities",
                                  "Telecommunications",
                                  "Computer programming, consultancy and related activities",
                                  "Information service activities",
                                  ## Finance and insurance
                                  "Financial service activities, except insurance and pension funding",
                                  "Insurance, reinsurance and pension funding, except compulsory social security",
                                  "Activities auxiliary to financial services and insurance activities",
                                  ## Property
                                  "Real estate activities",
                                  ## Professional, scientific, and technical
                                  "Legal and accounting activities",
                                  "Activities of head offices; management consultancy activities",
                                  "Architectural and engineering activities; technical testing and analysis",
                                  "Scientific research and development",
                                  "Advertising and market research",
                                  "Other professional, scientific and technical activities",
                                  "Veterinary activities",
                                  ## Business admin and support services
                                  "Rental and leasing activities",
                                  "Employment activities",
                                  "Travel agency, tour operator and other reservation service and related activities",
                                  "Security and investigation activities",
                                  "Services to buildings and landscape activities",
                                  "Office administrative, office support and other business support activities",
                                  ## Public administration
                                  "Public administration and defence; compulsory social security",
                                  ## Education
                                  "Education",
                                  ## Health
                                  "Human health activities",
                                  "Residential care activities",
                                  "Social work activities without accommodation",
                                  ## Arts, entertainment and recreation
                                  "Creative, arts and entertainment activities",
                                  "Libraries, archives, museums and other cultural activities",
                                  "Gambling and betting activities",
                                  "Sports activities and amusement and recreation activities",
                                  ## Other
                                  "Activities of membership organisations",
                                  "Repair of computers and personal and household goods",
                                  "Other personal service activities",
                                  "Activities of households as employers of domestic personnel",
                                  "Undifferentiated goods- and services-producing activities of private households for own use",
                                  "Activities of extraterritorial organisations and bodies"
                                  )) ]


  ##################
  ## RETAIN THE CLEANED VARIABLES

  final_data <- data[, c("pidp", "id", "hidp", "wave_no",
                         "hours_empl", "hours_semp", "hours", "nssec", "nssec_3cat", "nssec_5cat", "nssec_8cat", "sic_1dig", "sic_2dig",
                         "absent_sick","absent_matleave","absent_annualleave","absent_other")]

  var_names <- c("hours_empl", "hours_semp", "hours", "nssec_3cat", "nssec_5cat", "nssec_8cat", "sic_1dig", "sic_2dig",
                 "absent_sick", "absent_matleave", "absent_annualleave", "absent_other")

  setnames(final_data, var_names, paste0("w_", var_names))


  return(final_data)
}
