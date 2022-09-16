#' Clean Economic Status
#'
#' Generate three variables indicating economic status at different levels of detail; working vs not
#' working; employed, unemployed or inactive; and employed, self-employed, unemployed, sick, retired,
#' education, or other.
#'
#' @export
clean_econ_status <- function(data = NULL) {

  #cat(crayon::green("\tCleaning economic status variables\n"))

  ### 2 categories - work/not work

  data[econ_stat == 1 , econ_stat_2cat := "employed"]
  data[econ_stat == 2 , econ_stat_2cat := "employed"]
  data[econ_stat == 3 , econ_stat_2cat := "not_employed"]
  data[econ_stat == 4 , econ_stat_2cat := "not_employed"]
  data[econ_stat == 5 , econ_stat_2cat := "not_employed"]
  data[econ_stat == 6 , econ_stat_2cat := "not_employed"]
  data[econ_stat == 7 , econ_stat_2cat := "not_employed"]
  data[econ_stat == 8 , econ_stat_2cat := "not_employed"]
  data[econ_stat == 9 , econ_stat_2cat := "not_employed"]
  data[econ_stat == 10, econ_stat_2cat := "not_employed"]
  data[econ_stat == 11, econ_stat_2cat := "not_employed"]

  data[,econ_stat_2cat := factor(econ_stat_2cat,
                                 levels = c("employed","not_employed"),
                                 labels = c("employed","not_employed"))]

  ### 3 categories - employed/unemployed/inactive

  data[econ_stat == 1 , econ_stat_3cat := "employed"]
  data[econ_stat == 2 , econ_stat_3cat := "employed"]
  data[econ_stat == 3 , econ_stat_3cat := "unemployed"]
  data[econ_stat == 4 , econ_stat_3cat := "inactive"]
  data[econ_stat == 5 , econ_stat_3cat := "inactive"]
  data[econ_stat == 6 , econ_stat_3cat := "inactive"]
  data[econ_stat == 7 , econ_stat_3cat := "inactive"]
  data[econ_stat == 8 , econ_stat_3cat := "inactive"]
  data[econ_stat == 9 , econ_stat_3cat := "inactive"]
  data[econ_stat == 10, econ_stat_3cat := "inactive"]
  data[econ_stat == 11, econ_stat_3cat := "inactive"]

  data[,econ_stat_3cat := factor(econ_stat_3cat,
                                 levels = c("employed","unemployed","inactive"),
                                 labels = c("employed","unemployed","inactive"))]

  ### 7 categories - employed/self-employed/unemployed/sick/retired/education/other

  data[econ_stat == 1 , econ_stat_7cat := "self-employed"]
  data[econ_stat == 2 , econ_stat_7cat := "employed"]
  data[econ_stat == 3 , econ_stat_7cat := "unemployed"]
  data[econ_stat == 4 , econ_stat_7cat := "retired"]
  data[econ_stat == 5 , econ_stat_7cat := "other"]
  data[econ_stat == 6 , econ_stat_7cat := "other"]
  data[econ_stat == 7 , econ_stat_7cat := "education"]
  data[econ_stat == 8 , econ_stat_7cat := "sick"]
  data[econ_stat == 9 , econ_stat_7cat := "other"]
  data[econ_stat == 10, econ_stat_7cat := "other"]
  data[econ_stat == 11, econ_stat_7cat := "other"]

  data[,econ_stat_7cat := factor(econ_stat_7cat,
                                 levels = c("employed","self-employed","unemployed","sick","retired","education","other"),
                                 labels = c("employed","self-employed","unemployed","sick","retired","education","other"))]

  # remove original variable

  data <- subset(data,select = -c(econ_stat))

  return(data)
}
