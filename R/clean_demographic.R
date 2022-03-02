#' Clean Demographic Variables
#'
#' Reads and does basic cleaning on the UKHLS demographic variables -
#' age, gender, ethnicity, region and urban vs rural.
#'
#' @export
clean_demographic <- function(data = NULL) {

  cat(crayon::green("\tCleaning demographic variables\n"))

  ### age bands

  data[ , ageband := c("16-19", "20-24", "25-29", "30-34", "35-39", "40-44",
                       "45-49", "50-54", "55-59", "60-64", "65-69",
                       "70+")[findInterval(age, c(16, seq(20,65,5), 70))]]
  data$ageband <- as.factor(data$ageband)

  data[ , age_cat := c("16-24", "25-34", "35-49", "50-64", "65+")[findInterval(age, c(16, 25, 35, 50, 65))]]
  data$age_cat <- as.factor(data$age_cat)

  ### gender

  data[sex == 1, gender := "male"]
  data[sex == 2, gender := "female"]

  data[,gender := as.factor(gender)]
  data <- subset(data,select = -c(sex))
  setnames(data, "gender", "sex")

  ### ethnicity

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
  data[ethnicity_raw %in% c(1,2,3,4) , ethnicity_2cat := "white"]
  data[ethnicity_raw %in% c(5:97) , ethnicity_2cat := "non_white"]

  data[,ethnicity_9cat := as.factor(ethnicity_9cat)]
  data[,ethnicity_5cat := as.factor(ethnicity_5cat)]
  data[,ethnicity_2cat := as.factor(ethnicity_2cat)]

  data <- subset(data,select = -c(ethnicity_raw))

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

  data[,gor := as.factor(gor)]

  data <- subset(data,select = -c(region))

  ### area

  data[urban == 1  , area := "urban"]
  data[urban == 2  , area := "rural"]

  data[,area := as.factor(area)]

  data <- subset(data,select = -c(urban))

  ### marital status
  data[mlstat == 1         , marstat := "single"]
  data[mlstat %in% c(2,3)  , marstat := "married"]
  data[mlstat %in% c(4:9)  , marstat := "sep_div_wid"]

  data[,marstat := as.factor(marstat)]

  data <- subset(data,select = -c(mlstat))


  ### highest qualification

  data[highest_qual == 1, hiqual := "degree"]
  data[highest_qual == 2, hiqual := "other_he"]
  data[highest_qual == 3, hiqual := "alevel"]
  data[highest_qual == 4, hiqual := "gcse"]
  data[highest_qual == 5, hiqual := "other_qual"]
  data[highest_qual == 9, hiqual := "no_qual"]

  data[,hiqual <- as.factor(hiqual)]
  data <- subset(data,select = -c(highest_qual))

return(data)
}
