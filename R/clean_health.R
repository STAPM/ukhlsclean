#' Clean Health and Caring Variables
#'
#' @export
clean_health <- function(data = NULL) {

  ### individual is a carer for someone in their household

  data[caring == 1, care_hhold := "yes"]
  data[caring == 2, care_hhold := "no"]

  data$care_hhold <- as.factor(data$care_hhold)
  data <- subset(data,select = -c(caring))

  ### individual has a long-standing illness/disability

  data[lt_sick == 1, disability := "yes"]
  data[lt_sick == 2, disability := "no"]

  data$disability <- as.factor(data$disability)
  data <- subset(data,select = -c(lt_sick))

}
