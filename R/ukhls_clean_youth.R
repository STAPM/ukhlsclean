#' Clean Youth data Variables
#'
#' Produce clean versions of variables measuring youth smoking outcomes.
#'
#' @param data Data table. Understanding Society data produced using the read functions.
#'
#' @export
ukhls_clean_youth <- function(youth_data = NULL) {

  # youth_data <- ukhls_read_youth()

  cat(crayon::magenta("\nCleaning UKHLS Youth datasets"))

  youth_data <- youth_data[order(id), ]

  youth_data <- youth_data[, c("id","hidp","wave","wave_no","bhps_sample",
                               "year","month","day","weight_xw",
                               "age","sex","region","country","area","ethnicity",
                               "ypevrsmo","ypsmofrq","ypevresmo",
                               "deceased","deceased_when")]

  cat(crayon::red("\nAge..."))

  #################
  ### age bands ###
  # youth_data[ , d_age_12cat := c("16-19", "20-24", "25-29", "30-34", "35-39", "40-44",
  #                        "45-49", "50-54", "55-59", "60-64", "65-69",
  #                        "70+")[findInterval(age, c(16, seq(20,65,5), 70))]]
  # youth_data[ , d_age_12cat := as.factor(d_age_12cat)]
  # youth_data[ , d_age_5cat := c("16-24", "25-34", "35-49", "50-64", "65+")[findInterval(age, c(16, 25, 35, 50, 65))]]
  # youth_data[ , d_age_5cat := as.factor(d_age_5cat)]
  ### There shouldn't be an appropriate age cat similar to adult data because not many are old enough to be categorised
  youth_data[! age <= 15, d_age_12cat := c("16-19", "20-24", "25-29", "30-34", "35-39", "40-44",
                                           "45-49", "50-54", "55-59", "60-64", "65-69",
                                           "70+")[findInterval(age, c(16, seq(20,65,5), 70))]]
  youth_data[age <= 15, d_age_12cat := "<16"]
  youth_data[ , d_age_12cat := as.factor(d_age_12cat)]

  youth_data[! age <= 15, d_age_5cat := c("16-24", "25-34", "35-49", "50-64", "65+")[findInterval(age, c(16, 25, 35, 50, 65))]]
  youth_data[age <= 15, d_age_5cat := "<16"]
  youth_data[ , d_age_5cat := as.factor(d_age_5cat)]

  cat(crayon::red("\tSex..."))

  ##############
  ### sex ###
  youth_data[, sex := factor(sex, levels = 1:2, labels = c("male","female"))]

  cat(crayon::red("\tEthnicity..."))

  #################
  ### ethnicity ###

  if ("ethnicity" %in% names(youth_data)){
  ##### 9-categories
  youth_data[ethnicity %in% c(1)        , d_ethnicity_9cat := "white_british"]
  youth_data[ethnicity %in% c(2:4)      , d_ethnicity_9cat := "white_non_british"]
  youth_data[ethnicity %in% c(5:8)      , d_ethnicity_9cat := "mixed"]
  youth_data[ethnicity %in% c(9)        , d_ethnicity_9cat := "indian"]
  youth_data[ethnicity %in% c(10)       , d_ethnicity_9cat := "pakistani"]
  youth_data[ethnicity %in% c(11)       , d_ethnicity_9cat := "bangladeshi"]
  youth_data[ethnicity %in% c(12:13,17) , d_ethnicity_9cat := "other_asian"]
  youth_data[ethnicity %in% c(14:16)    , d_ethnicity_9cat := "black"]
  youth_data[ethnicity %in% c(17,97)    , d_ethnicity_9cat := "other"]

  ##### 5-categories
  youth_data[ethnicity %in% c(1:4)     , d_ethnicity_5cat := "white"]
  youth_data[ethnicity %in% c(14:16)   , d_ethnicity_5cat := "black"]
  youth_data[ethnicity %in% c(9:13)    , d_ethnicity_5cat := "asian"]
  youth_data[ethnicity %in% c(5:8)     , d_ethnicity_5cat := "mixed"]
  youth_data[ethnicity %in% c(17,97)   , d_ethnicity_5cat := "other"]

  ##### 2-categories
  youth_data[ethnicity %in% c(1:4) , d_ethnicity_2cat := "white"]
  youth_data[ethnicity %in% c(5:97) , d_ethnicity_2cat := "non_white"]

  youth_data[,d_ethnicity_9cat := as.factor(d_ethnicity_9cat)]
  youth_data[,d_ethnicity_5cat := as.factor(d_ethnicity_5cat)]
  youth_data[,d_ethnicity_2cat := as.factor(d_ethnicity_2cat)]

} else {

  youth_data[,d_ethnicity_9cat := NA]
  youth_data[,d_ethnicity_5cat := NA]
  youth_data[,d_ethnicity_2cat := NA]
}

  cat(crayon::red("\tRegion..."))

  ################
  ### location #####
  youth_data[, region := as.character(region)]

  youth_data[region == 1  , region := "north_east"]
  youth_data[region == 2  , region := "north_west"]
  youth_data[region == 3  , region := "yorkshire"]
  youth_data[region == 4  , region := "east_midlands"]
  youth_data[region == 5  , region := "west_midlands"]
  youth_data[region == 6  , region := "east"]
  youth_data[region == 7  , region := "london"]
  youth_data[region == 8  , region := "south_east"]
  youth_data[region == 9  , region := "south_west"]
  youth_data[region == 10 , region := "wales"]
  youth_data[region == 11 , region := "scotland"]
  youth_data[region == 12 , region := "northern_ireland"]

  youth_data[,region := as.factor(region)]

  ## country ####
  youth_data[, country := as.character(country)]

  youth_data[country %in% 1:9, country := "england"]
  youth_data[country == 10 , country := "wales"]
  youth_data[country == 11 , country := "scotland"]
  youth_data[country == 12 , country := "northern_ireland"]

  youth_data[,country := factor(country, levels = c("england","wales","scotland","northern_ireland"))]

  ## area - rural/urban #####
  youth_data[, area := as.character(area)]

  youth_data[area == 1  , area := "urban"]
  youth_data[area == 2  , area := "rural"]

  youth_data[, area := as.factor(area)]

  #################
  ### smoking #####

  cat(crayon::red("\tSmoking..."))

  # s_current_smoker
  youth_data[ypevrsmo == 1 & ypsmofrq > 2, current_smk := 1]
  youth_data[ypevrsmo == 1 & ypsmofrq <= 2, current_smk := 2]
  youth_data[ypevrsmo == 2, current_smk := 2]
  youth_data[is.na(ypevrsmo), current_smk := NA_integer_]

  dummy_data <- data.table::copy(youth_data)
  dummy_data <- dummy_data[, .(s_current_smoker = suppressWarnings(min(current_smk, na.rm = TRUE))), by = c("id","age")] # suppressWarnings(
  dummy_data[s_current_smoker == Inf, s_current_smoker := NA_integer_]
  dummy_data[, s_current_smoker := as.character(s_current_smoker)]
  dummy_data[s_current_smoker == "2", s_current_smoker := "non_smoker"]
  dummy_data[s_current_smoker == "1", s_current_smoker := "smoker"]
  youth_data <- merge(youth_data, dummy_data, by = c("id","age"), all.x = TRUE)
  rm(dummy_data)
  youth_data[, current_smk := NULL]

  # s_yth_smk_age_start
  youth_data[, s_smk_age_start := NA_integer_]
  dummy_data <- data.table::copy(youth_data)
  dummy_data <- dummy_data[s_current_smoker=="smoker", .(s_yth_smk_age_start = suppressWarnings(min(age, na.rm = TRUE))), by = id]
  dummy_data[s_yth_smk_age_start == Inf, s_yth_smk_age_start := NA_integer_]
  youth_data <- merge(youth_data, dummy_data, by = "id", all.x = TRUE) #############
  rm(dummy_data)

  # s_ncigs
  youth_data[, s_ncigs := NA_integer_]

  # s_ever_smoked (ypevrsmo)
  setnames(youth_data, "ypevrsmo", "s_ever_smoked")

  # s_othersmoker_hhold
  #pending

  # s_ecig
  if ("ypevresmo" %in% colnames(youth_data)) {
    youth_data[ypevresmo %in% 1, s_ecig := "never_used_ecig"]
    youth_data[ypevresmo %in% 2:3, s_ecig := "not_currently_using_ecig"]
    youth_data[ypevresmo %in% 4:5, s_ecig := "infrequent_ecig_use"]
    youth_data[ypevresmo %in% 6, s_ecig := "frequent_ecig_use"]
  } else {
    youth_data[, s_ecig := NA_character_]
  }

  # s_ecig_current
  if ("ypevresmo" %in% colnames(youth_data)) {
    youth_data[ypevresmo %in% 1:3, s_ecig_current := "non_ecig_user"]
    youth_data[ypevresmo %in% 4:6, s_ecig_current := "ecig_user"]
  } else {
    youth_data[, s_ecig_current := NA_character_]
  }

  youth_data[, ypevresmo := NULL]

  # # youth_data[ypevrsmo == 1 & is.na(ypsmofrq), ] ### Missing values for current - unsure what to do
  #
  # # eversmoke <- copy(youth_data)
  #
  # # youth_data[, ypevrsmo := NULL]
  # youth_data[, ypsmofrq := NULL]
  # youth_data[, age := paste0("curr.smk_",age)]

  # youth_data <- youth_data[current == Inf, current := NA_integer_]


#
#   youth_data <- dcast(youth_data,
#                    id ~ age,
#                    value.var = "current")

  # #### ever smoke indicator
  #
  # eversmoke[, ypsmofrq := NULL]
  # eversmoke[, age := paste0(age,"yro")]
  #
  # eversmoke.m1 <- dcast(eversmoke,
  #                       id ~ age,
  #                       value.var = "ypevrsmo",
  #                       fill = NA,
  #                       fun.aggregate = NULL)
  # unique(eversmoke.m1$`10yro`)
  #
  # setnames(youth_data,
  #          "id",
  #          "id")


  ##############
  ### WAVES ####

  youth_data <- youth_data[order(id, wave_no),]


  # create flags for each wave

  youth_data[, wave_1  := ifelse(wave_no == 1,1,0)]
  youth_data[, wave_2  := ifelse(wave_no == 2,1,0)]
  youth_data[, wave_3  := ifelse(wave_no == 3,1,0)]
  youth_data[, wave_4  := ifelse(wave_no == 4,1,0)]
  youth_data[, wave_5  := ifelse(wave_no == 5,1,0)]
  youth_data[, wave_6  := ifelse(wave_no == 6,1,0)]
  youth_data[, wave_7  := ifelse(wave_no == 7,1,0)]
  youth_data[, wave_8  := ifelse(wave_no == 8,1,0)]
  youth_data[, wave_9  := ifelse(wave_no == 9,1,0)]
  youth_data[, wave_10 := ifelse(wave_no == 10,1,0)]
  youth_data[, wave_11 := ifelse(wave_no == 11,1,0)]
  youth_data[, wave_12 := ifelse(wave_no == 12,1,0)]

  youth_data[, wave_1  := max(wave_1) , by = "id"]
  youth_data[, wave_2  := max(wave_2) , by = "id"]
  youth_data[, wave_3  := max(wave_3) , by = "id"]
  youth_data[, wave_4  := max(wave_4) , by = "id"]
  youth_data[, wave_5  := max(wave_5) , by = "id"]
  youth_data[, wave_6  := max(wave_6) , by = "id"]
  youth_data[, wave_7  := max(wave_7) , by = "id"]
  youth_data[, wave_8  := max(wave_8) , by = "id"]
  youth_data[, wave_9  := max(wave_9) , by = "id"]
  youth_data[, wave_10 := max(wave_10), by = "id"]
  youth_data[, wave_11 := max(wave_11), by = "id"]
  youth_data[, wave_12 := max(wave_12), by = "id"]

  ## calculate number of waves each individual is in

  youth_data[, nwaves := .N, by = c("id")]

  # order columns

  setcolorder(youth_data,c("id","hidp","wave","wave_no","bhps_sample", ### dataset variable dropped here
                           "nwaves",
                           "wave_1","wave_2","wave_3","wave_4","wave_5","wave_6",
                           "wave_7","wave_8","wave_9","wave_10","wave_11","wave_12",
                           "year","month","day","weight_xw",
                           "age","d_age_5cat","d_age_12cat","sex",
                           "region","country","area",
                           "d_ethnicity_2cat","d_ethnicity_5cat","d_ethnicity_9cat",
                           "s_current_smoker","s_ncigs","s_ever_smoked","s_smk_age_start","s_yth_smk_age_start",
                           # "s_othersmoker_hhold",
                           "s_ecig","s_ecig_current",
                           "deceased","deceased_when"))

  setnames(youth_data,
           c("id","age","sex","region","country","area"),
           c("id","d_age","d_sex","d_gor","d_country","d_area"))

  #
  # final_data <- youth_data[, c("id",
  #                        "curr.smk_10", "curr.smk_11", "curr.smk_12", "curr.smk_13", "curr.smk_14", "curr.smk_15", "curr.smk_16")]
  #
  # var_names <- c("curr.smk_10", "curr.smk_11", "curr.smk_12", "curr.smk_13", "curr.smk_14", "curr.smk_15", "curr.smk_16")
  #
  # setnames(final_data, var_names, paste0("yp_", var_names))

  cat(crayon::magenta("\n...cleaned"))

  return(youth_data)
}
