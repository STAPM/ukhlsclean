#' Clean Health and Caring Variables
#'
#' @export
clean_health <- function(data = NULL) {

  ### individual is a carer for someone in their household

  data[caring == 1, care_hhold := "yes"]
  data[caring != 1 | is.na(caring), care_hhold := "no"]

  data$care_hhold <- as.factor(data$care_hhold)
  ### individual has a long-standing illness/disability

  data[lt_sick == 1, disability := "yes"]
  data[lt_sick == 2, disability := "no"]

  data$disability <- as.factor(data$disability)

  ### satisfaction with health
  data[health_satisf == 1, satisfaction_health := "completely disatisfied"]
  data[health_satisf == 2, satisfaction_health := "mostly disatisfied"]
  data[health_satisf == 3, satisfaction_health := "somewhat disatisfied"]
  data[health_satisf == 4, satisfaction_health := "neutral"]
  data[health_satisf == 5, satisfaction_health := "somewhat satisfied"]
  data[health_satisf == 6, satisfaction_health := "mostly satisfied"]
  data[health_satisf == 7, satisfaction_health := "completely satisfied"]

  ### satisfaction with life in general
  data[life_satisf == 1, satisfaction_life := "completely disatisfied"]
  data[life_satisf == 2, satisfaction_life := "mostly disatisfied"]
  data[life_satisf == 3, satisfaction_life := "somewhat disatisfied"]
  data[life_satisf == 4, satisfaction_life := "neutral"]
  data[life_satisf == 5, satisfaction_life := "somewhat satisfied"]
  data[life_satisf == 6, satisfaction_life := "mostly satisfied"]
  data[life_satisf == 7, satisfaction_life := "completely satisfied"]

  ### satisfaction with life in general
  data[gen_health == 1, general_health := "excellent"]
  data[gen_health == 2, general_health := "very good"]
  data[gen_health == 3, general_health := "good"]
  data[gen_health == 4, general_health := "fair"]
  data[gen_health == 5, general_health := "poor"]

  ### currently pregnant
  data[pregout1 == 4 | pregout2 == 4 |
       pregout3 == 4 | pregout4 == 4 | pregout5 == 4, pregnant := 1]
  data[is.na(pregnant) , pregnant := 0]

  ### remove raw variables
  data <- subset(data,select = -c(lt_sick,caring,health_satisf,life_satisf,gen_health,
                                  pregout1,pregout2,pregout3,pregout4,pregout5))

}
