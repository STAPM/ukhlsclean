#' Clean Education Variables
#'
#' @export
clean_education <- function(data = NULL) {

  ### highest qualification

  data[highest_qual == 1, hiqual := "degree"]
  data[highest_qual == 2, hiqual := "other_he"]
  data[highest_qual == 3, hiqual := "alevel"]
  data[highest_qual == 4, hiqual := "gcse"]
  data[highest_qual == 5, hiqual := "other_qual"]
  data[highest_qual == 9, hiqual := "no_qual"]

  data$hiqual <- as.factor(data$hiqual)
  data <- subset(data,select = -c(highest_qual))

}
