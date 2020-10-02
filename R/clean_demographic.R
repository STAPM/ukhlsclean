#' Clean Demographic Variables
#'
#' Reads and does basic cleaning on the UKHLS demographic variables -
#' age, gender, ethnicity, region and urban vs rural.
#'
#' @export
clean_demographic <- function(data = NULL) {

  ### age bands

  data[ , ageband := c("16-19",
                       "20-24",
                       "25-29",
                       "30-34",
                       "35-39",
                       "40-44",
                       "45-49",
                       "50-54",
                       "55-59",
                       "60-64",
                       "65-69",
                       "70+")[findInterval(age, c(16, seq(20,65,5), 70))]]
  data$ageband <- as.factor(data$ageband)

  ### gender

  data[sex == 1, gender := "male"]
  data[sex == 2, gender := "female"]

  data$gender <- as.factor(data$gender)
  data <- subset(data,select = -c(sex))

  ### ethnicity

  ##### 9-categories
  # UKHLS cross wave
  data[dataset == "UKHLS" & ethnicity_raw %in% c(1)        , ethnicity_9cat := "white_british"]
  data[dataset == "UKHLS" & ethnicity_raw %in% c(2:4)      , ethnicity_9cat := "white_non_british"]
  data[dataset == "UKHLS" & ethnicity_raw %in% c(5:8)      , ethnicity_9cat := "mixed"]
  data[dataset == "UKHLS" & ethnicity_raw %in% c(9)        , ethnicity_9cat := "indian"]
  data[dataset == "UKHLS" & ethnicity_raw %in% c(10)       , ethnicity_9cat := "pakistani"]
  data[dataset == "UKHLS" & ethnicity_raw %in% c(11)       , ethnicity_9cat := "bangladeshi"]
  data[dataset == "UKHLS" & ethnicity_raw %in% c(12:13,17) , ethnicity_9cat := "other_asian"]
  data[dataset == "UKHLS" & ethnicity_raw %in% c(14:16)    , ethnicity_9cat := "black"]
  data[dataset == "UKHLS" & ethnicity_raw %in% c(17,97)    , ethnicity_9cat := "other"]

  ##### 5-categories
  # BHPS wave 1-12
  data[dataset == "BHPS" & wave_no < 13 & ethnicity_raw %in% c(1)   , ethnicity_5cat := "white"]
  data[dataset == "BHPS" & wave_no < 13 & ethnicity_raw %in% c(2:4) , ethnicity_5cat := "black"]
  data[dataset == "BHPS" & wave_no < 13 & ethnicity_raw %in% c(5:8) , ethnicity_5cat := "asian"]
  data[dataset == "BHPS" & wave_no < 13 & ethnicity_raw == 9        , ethnicity_5cat := "other"]
  # BHPS wave 13-18
  data[dataset == "BHPS" & wave_no >= 13 & ethnicity_raw %in% c(1:5)      , ethnicity_5cat := "white"]
  data[dataset == "BHPS" & wave_no >= 13 & ethnicity_raw %in% c(14:16)    , ethnicity_5cat := "black"]
  data[dataset == "BHPS" & wave_no >= 13 & ethnicity_raw %in% c(10:13,17) , ethnicity_5cat := "asian"]
  data[dataset == "BHPS" & wave_no >= 13 & ethnicity_raw %in% c(6:9)      , ethnicity_5cat := "mixed"]
  data[dataset == "BHPS" & wave_no >= 13 & ethnicity_raw == 18            , ethnicity_5cat := "other"]
  # UKHLS cross wave
  data[dataset == "UKHLS" & ethnicity_raw %in% c(1:4)     , ethnicity_5cat := "white"]
  data[dataset == "UKHLS" & ethnicity_raw %in% c(14:16)   , ethnicity_5cat := "black"]
  data[dataset == "UKHLS" & ethnicity_raw %in% c(9:13)    , ethnicity_5cat := "asian"]
  data[dataset == "UKHLS" & ethnicity_raw %in% c(5:8)     , ethnicity_5cat := "mixed"]
  data[dataset == "UKHLS" & ethnicity_raw %in% c(17,97)   , ethnicity_5cat := "other"]

  ##### 2-categories
  # BHPS wave 1-12
  data[dataset == "BHPS" & wave_no < 13 & ethnicity_raw %in% c(1)   , ethnicity_2cat := "white"]
  data[dataset == "BHPS" & wave_no < 13 & ethnicity_raw %in% c(2:9) , ethnicity_2cat := "non_white"]
  # BHPS wave 13-18
  data[dataset == "BHPS" & wave_no >= 13 & ethnicity_raw %in% c(1:5) , ethnicity_2cat := "white"]
  data[dataset == "BHPS" & wave_no >= 13 & ethnicity_raw %in% c(6:18), ethnicity_2cat := "non_white"]
  # UKHLS cross wave
  data[dataset == "UKHLS" & ethnicity_raw %in% c(1,2,3,4) , ethnicity_2cat := "white"]
  data[dataset == "UKHLS" & ethnicity_raw %in% c(5:97) , ethnicity_2cat := "non_white"]

  data$ethnicity_9cat <- as.factor(data$ethnicity_9cat)
  data$ethnicity_5cat <- as.factor(data$ethnicity_5cat)
  data$ethnicity_2cat <- as.factor(data$ethnicity_2cat)

  data <- subset(data,select = -c(ethnicity_raw))

  ## fill in missing values for BHPS data
  data$ethnicity_5cat <- with(data, ave(ethnicity_5cat, id, FUN = function(x)
                                        replace(x, is.na(x), x[!is.na(x)][1L])))
  data$ethnicity_2cat <- with(data, ave(ethnicity_2cat, id, FUN = function(x)
                                         replace(x, is.na(x), x[!is.na(x)][1L])))
  ### region

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

  data$gor <- as.factor(data$gor)

  data <- subset(data,select = -c(region))

### area

  data[urban == 1  , area := "urban"]
  data[urban == 2  , area := "rural"]

  data$area <- as.factor(data$area)

  data <- subset(data,select = -c(urban))

return(data)
}
