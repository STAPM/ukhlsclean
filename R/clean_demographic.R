
#' Clean Demographic Variables
#'
#' Reads and does basic cleaning on the UKHLS demographic variables - gender, ethnicity, region.
#'
#'
#' @export
clean_demographic <- function(data = NULL) {

### gender

data[sex == 1, gender := "male"]
data[sex == 2, gender := "female"]

data$gender <- as.factor(data$gender)
data <- subset(data,select = -c(sex))

### ethnicity

data[ethnicity_raw %in% c(1,2,3,4) , ethnicity_5cat := "white"]
data[ethnicity_raw %in% c(14,15,16) , ethnicity_5cat := "black"]
data[ethnicity_raw %in% c(9,10,11,12,13,17) , ethnicity_5cat := "asian"]
data[ethnicity_raw %in% c(5,6,7,8) , ethnicity_5cat := "mixed"]
data[ethnicity_raw == 97 , ethnicity_5cat := "other"]

data[ethnicity_raw %in% c(1,2,3,4) , ethnicity_2cat := "white"]
data[ethnicity_raw %in% c(5:97) , ethnicity_2cat := "non_white"]

data$ethnicity_5cat <- as.factor(data$ethnicity_5cat)
data$ethnicity_2cat <- as.factor(data$ethnicity_2cat)

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

data$gor <- as.factor(data$gor)

data <- subset(data,select = -c(region))

### area

data[urban == 1  , area := "urban"]
data[urban == 2  , area := "rural"]

data$area <- as.factor(data$area)

data <- subset(data,select = -c(urban))

return(data)
}
