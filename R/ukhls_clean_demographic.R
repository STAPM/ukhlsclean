#' Clean Demographic Variables
#'
#' Reads and does basic cleaning on the UKHLS demographic variables -
#' age, sex, ethnicity, region and urban vs rural.
#'
#' @param data Data table. Understanding Society data produced using the read functions.
#'
#' @export
ukhls_clean_demographic <- function(data = NULL) {

  #################
  ### age bands ###

  data[ , age_12cat := c("16-19", "20-24", "25-29", "30-34", "35-39", "40-44",
                         "45-49", "50-54", "55-59", "60-64", "65-69",
                         "70+")[findInterval(age, c(16, seq(20,65,5), 70))]]
  data[ , age_12cat := as.factor(age_12cat)]

  data[ , age_5cat := c("16-24", "25-34", "35-49", "50-64", "65+")[findInterval(age, c(16, 25, 35, 50, 65))]]
  data[ , age_5cat := as.factor(age_5cat)]

  ##############
  ### sex ###

  data[, sex := factor(sex, levels = 1:2, labels = c("male","female"))]

  #################
  ### ethnicity ###

  if ("ethnicity_raw" %in% colnames(data)){
  ##### 9-categories
  data[ethnicity_raw %in% c(1)        , ethnicity_9cat := "white_british"]
  data[ethnicity_raw %in% c(2:4)      , ethnicity_9cat := "white_non_british"]
  data[ethnicity_raw %in% c(5:8)      , ethnicity_9cat := "mixed"]
  data[ethnicity_raw %in% c(9)        , ethnicity_9cat := "indian"]
  data[ethnicity_raw %in% c(10)       , ethnicity_9cat := "pakistani"]
  data[ethnicity_raw %in% c(11)       , ethnicity_9cat := "bangladeshi"]
  data[ethnicity_raw %in% c(12:13,17) , ethnicity_9cat := "other_asian"]
  data[ethnicity_raw %in% c(14:16)    , ethnicity_9cat := "black"]
  data[ethnicity_raw %in% c(17,97)    , ethnicity_9cat := "other"]

  ##### 5-categories
  data[ethnicity_raw %in% c(1:4)     , ethnicity_5cat := "white"]
  data[ethnicity_raw %in% c(14:16)   , ethnicity_5cat := "black"]
  data[ethnicity_raw %in% c(9:13)    , ethnicity_5cat := "asian"]
  data[ethnicity_raw %in% c(5:8)     , ethnicity_5cat := "mixed"]
  data[ethnicity_raw %in% c(17,97)   , ethnicity_5cat := "other"]

  ##### 2-categories
  data[ethnicity_raw %in% c(1:4) , ethnicity_2cat := "white"]
  data[ethnicity_raw %in% c(5:97) , ethnicity_2cat := "non_white"]

  data[,ethnicity_9cat := as.factor(ethnicity_9cat)]
  data[,ethnicity_5cat := as.factor(ethnicity_5cat)]
  data[,ethnicity_2cat := as.factor(ethnicity_2cat)]

  } else if ("ethn_dv" %in% colnames(data)) {
  ##### 9-categories
  data[ethn_dv %in% c(1)        , ethnicity_9cat := "white_british"]
  data[ethn_dv %in% c(2:4)      , ethnicity_9cat := "white_non_british"]
  data[ethn_dv %in% c(5:8)      , ethnicity_9cat := "mixed"]
  data[ethn_dv %in% c(9)        , ethnicity_9cat := "indian"]
  data[ethn_dv %in% c(10)       , ethnicity_9cat := "pakistani"]
  data[ethn_dv %in% c(11)       , ethnicity_9cat := "bangladeshi"]
  data[ethn_dv %in% c(12:13,17) , ethnicity_9cat := "other_asian"]
  data[ethn_dv %in% c(14:16)    , ethnicity_9cat := "black"]
  data[ethn_dv %in% c(17,97)    , ethnicity_9cat := "other"]

  ##### 5-categories
  data[ethn_dv %in% c(1:4)     , ethnicity_5cat := "white"]
  data[ethn_dv %in% c(14:16)   , ethnicity_5cat := "black"]
  data[ethn_dv %in% c(9:13)    , ethnicity_5cat := "asian"]
  data[ethn_dv %in% c(5:8)     , ethnicity_5cat := "mixed"]
  data[ethn_dv %in% c(17,97)   , ethnicity_5cat := "other"]

  ##### 2-categories
  data[ethn_dv %in% c(1:4) , ethnicity_2cat := "white"]
  data[ethn_dv %in% c(5:97) , ethnicity_2cat := "non_white"]

  data[,ethnicity_9cat := as.factor(ethnicity_9cat)]
  data[,ethnicity_5cat := as.factor(ethnicity_5cat)]
  data[,ethnicity_2cat := as.factor(ethnicity_2cat)]

  } else {

  data[,ethnicity_9cat := NA]
  data[,ethnicity_5cat := NA]
  data[,ethnicity_2cat := NA]
  }

  ################
  ### region #####

  data[region == 1  , gor := "north_east"]
  data[region == 2  , gor := "north_west"]
  data[region == 3  , gor := "yorkshire"]
  data[region == 4  , gor := "east_midlands"]
  data[region == 5  , gor := "west_midlands"]
  data[region == 6  , gor := "east"]
  data[region == 7  , gor := "london"]
  data[region == 8  , gor := "south_east"]
  data[region == 9  , gor := "south_west"]
  data[region == 10 , gor := "wales"]
  data[region == 11 , gor := "scotland"]
  data[region == 12 , gor := "northern_ireland"]

  data[,gor := as.factor(gor)]

  data[region %in% 1:9, country := "england"]
  data[region == 10 , country := "wales"]
  data[region == 11 , country := "scotland"]
  data[region == 12 , country := "northern_ireland"]

  data[,country := factor(country, levels = c("england","wales","scotland","northern_ireland"))]

  #####################################
  ### index of multiple deprivation ###

  ## USoc codes 1 as most deprived. reverse.

  if ("imdq_e" %in% colnames(data)){
  data[country == "england",          imd_quintile := imdq_e]
  data[country == "wales",            imd_quintile := imdq_w]
  data[country == "scotland",         imd_quintile := imdq_s]
  data[country == "northern_ireland", imd_quintile := imdq_ni]

  data[,imd_quintile := factor(imd_quintile,
                               levels = rev(1:5),
                               labels = c("1_least_deprived","2","3","4","5_most_deprived")) ]
  } else {
  data[,imd_quintile := NA]
  }
  ############################
  ### area - rural/urban #####

  data[urban == 1  , area := "urban"]
  data[urban == 2  , area := "rural"]

  data[,area := as.factor(area)]

  ########################
  ### marital status #####

  data[marstat == 1         , marital := "single"]
  data[marstat %in% c(2,3)  , marital := "married"]
  data[marstat %in% c(4:9)  , marital := "sep_div_wid"]

  data[, marital := factor(marital, levels = c("married","single","sep_div_wid"))]

  ###############################
  ### highest qualification #####

  data[highest_qual == 1, hiqual := "degree"]
  data[highest_qual == 2, hiqual := "other_he"]
  data[highest_qual == 3, hiqual := "alevel"]
  data[highest_qual == 4, hiqual := "gcse"]
  data[highest_qual == 5, hiqual := "other_qual"]
  data[highest_qual == 9, hiqual := "no_qual"]

  data[, hiqual := factor(hiqual, levels = c("no_qual", "other_qual", "gcse",
                                             "alevel", "other_he", "degree"))]

  ##################
  ## RETAIN THE CLEANED VARIABLES

  final_data <- data[, c("pidp", "id", "hidp", "wave_no",
                         "age", "age_5cat", "age_12cat", "sex", "imd_quintile",
                         "gor", "country", "area", "marital", "hiqual",
                         "ethnicity_2cat", "ethnicity_5cat", "ethnicity_9cat")]


  var_names <- c("age", "age_5cat", "age_12cat", "sex", "imd_quintile",
                 "gor", "country", "area", "marital", "hiqual",
                 "ethnicity_2cat", "ethnicity_5cat", "ethnicity_9cat")

  setnames(final_data, var_names, paste0("d_", var_names))

return(final_data)
}
