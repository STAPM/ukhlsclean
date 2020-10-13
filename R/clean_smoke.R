#' Clean Smoking Variables
#'
#' Clean smoking variables and backwards impute smoking behaviour to waves prior to the smoking
#' questions being asked in the UKHLS.
#'
#' @export
clean_smoke <- function(data = NULL) {

  # ensure data is sorted
  data <- data[order(id,wave),]

  # non-smoking status
  data[wave %in% c("UKHLS Wave 2","UKHLS Wave 5") & smever == 2, non_smoker := "yes"]
  data[wave %in% c("UKHLS Wave 2","UKHLS Wave 5") & smever == 1 & smnow == 2, non_smoker := "yes"]
  data[wave %in% c("UKHLS Wave 2","UKHLS Wave 5") & smnow == 1, non_smoker := "no"]

  data[wave %in% c("UKHLS Wave 6","UKHLS Wave 7",
                   "UKHLS Wave 8","UKHLS Wave 9") & smoker == 1, non_smoker := "no"]
  data[wave %in% c("UKHLS Wave 6","UKHLS Wave 7",
                   "UKHLS Wave 8","UKHLS Wave 9") & smoker == 2, non_smoker := "yes"]

  # current smoker status
  data[wave %in% c("UKHLS Wave 2","UKHLS Wave 5") & smever == 1 & smnow == 1, current_smoker := "yes"]
  data[wave %in% c("UKHLS Wave 2","UKHLS Wave 5") & smever == 1 & smnow == 2, current_smoker := "no"]
  data[wave %in% c("UKHLS Wave 2","UKHLS Wave 5") & smever == 2, current_smoker := "no"]
  data[wave %in% c("UKHLS Wave 6","UKHLS Wave 7",
                   "UKHLS Wave 8","UKHLS Wave 9") & smoker == 1, current_smoker := "yes"]
  data[wave %in% c("UKHLS Wave 6","UKHLS Wave 7",
                   "UKHLS Wave 8","UKHLS Wave 9") & smoker == 2, current_smoker := "no"]

  # ever smoked
  data[wave %in% c("UKHLS Wave 2","UKHLS Wave 5") & smever == 2, ever_smoked := "no"]
  data[wave %in% c("UKHLS Wave 2","UKHLS Wave 5") & smever == 1, ever_smoked := "yes"]

  # make factors
  data$non_smoker   <- as.factor(data$non_smoker)
  data$ever_smoked  <- as.factor(data$ever_smoked)
  data$current_smoker <- as.factor(data$current_smoker)

  # age started smoking
  data[smagbg == 0, smagbg := NA]
  data[, smk_age_start := pmin(smagbg,na.rm=TRUE), by = "id"]

  data[, smk_age_start := nafill(smk_age_start, type = "nocb"), by = "id"]
  data[, smk_age_start := nafill(smk_age_start, type = "locf"), by = "id"]

  # remove raw variables no longer needed
  data <- subset(data,select = -c(smever,smnow,smcigs,smncigs,aglquit,smagbg,smoker))


  return(data)
}


