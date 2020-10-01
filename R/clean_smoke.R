#' Clean Smoking Variables
#'
#' Clean smoking variables and backwards impute smoking behaviour to waves prior to the smoking
#' questions being asked in the UKHLS.
#'
#' @export
clean_smoke <- function(data = NULL) {

  # ensure data is sorted
  data <- data[order(pidp,wave),]

  # non-smoking status
  data[wave %in% c("UKHLS Wave 2","UKHLS Wave 5"), non_smoker := ifelse(smever==2,"yes","no")]
  data[wave %in% c("UKHLS Wave 6","UKHLS Wave 7",
                   "UKHLS Wave 8","UKHLS Wave 9"), non_smoker := ifelse(smoker==2,"yes","no")]

  # current smoker status
  data[wave %in% c("UKHLS Wave 2","UKHLS Wave 5") & smever == 1 & smnow == 1, current_smoker := "yes"]
  data[wave %in% c("UKHLS Wave 2","UKHLS Wave 5") & smever == 1 & smnow == 2, current_smoker := "no"]
  data[wave %in% c("UKHLS Wave 2","UKHLS Wave 5") & smever == 2, current_smoker := "no"]
  data[wave %in% c("UKHLS Wave 6","UKHLS Wave 7",
                   "UKHLS Wave 8","UKHLS Wave 9"), current_smoker := ifelse(smoker==2,"no","yes")]

  # former smoker status
  data[wave %in% c("UKHLS Wave 2","UKHLS Wave 5") & smever == 1 & smnow == 1, former_smoker := "no"]
  data[wave %in% c("UKHLS Wave 2","UKHLS Wave 5") & smever == 1 & smnow == 2, former_smoker := "yes"]
  data[wave %in% c("UKHLS Wave 2","UKHLS Wave 5") & smever == 2, former_smoker := "no"]


  data$non_smoker     <- as.factor(data$non_smoker)
  data$former_smoker  <- as.factor(data$former_smoker)
  data$current_smoker <- as.factor(data$current_smoker)

  # age started smoking
  data[smagbg == 0, smagbg := NA]
  data[bhps_sample == FALSE, smk_age_start := min(smagbg,na.rm=TRUE), by = "pidp"]
  data[bhps_sample == TRUE , smk_age_start := min(smagbg,na.rm=TRUE), by = "pid"]

  # remove raw variables no longer needed
  data <- subset(data,select = -c(smever,smnow,smcigs,smncigs,aglquit,smagbg,smoker))


  return(data)
}


