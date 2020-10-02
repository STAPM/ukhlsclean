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

  # fill in NA for former smoker in later waves which only asks if a current smoker or not. differentiate between
  # a non and a former smoker using the wave 2 and wave 5 information on smoking history

  # if individual is a current smoker, can't be a former smoker
  data[is.na(former_smoker) & current_smoker == "yes", former_smoker := "no"]
  # if ever a previous current smoker, then "non_smokers" need to be changed to former smokers
      # calculate multiple lags to pick up smoking behaviour in previous waves
  data[ , c("lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag8") := shift(current_smoker, n=(1:8), type = "lag"), by = "id"]

      # create individual level identifier for ever having smoked.
  data[ , ever := ifelse(smever==1,1,NA)]
  data[(lag1=="yes"|lag2=="yes"|lag3=="yes"|lag4=="yes"|
          lag5=="yes"|lag6=="yes"|lag7=="yes"|lag8=="yes")  , ever := 1]
  data[, ever := nafill(ever, type = "nocb"), by = "id"]
  data[, ever := nafill(ever, type = "locf"), by = "id"]

     # use information on ever having smoked to

  data[is.na(ever) & is.na(former_smoker) & current_smoker == "no", former_smoker := "no"]
  data[is.na(ever), non_smoker := "yes"]
  data[ever == 1 & is.na(former_smoker) & current_smoker == "no", former_smoker := "yes"]
  data[ever == 1 & former_smoker == "yes", non_smoker := "no"]

  # age started smoking
  data[smagbg == 0, smagbg := NA]
  data[, smk_age_start := pmin(smagbg,na.rm=TRUE), by = "id"]

  data[, smk_age_start := nafill(smk_age_start, type = "nocb"), by = "id"]
  data[, smk_age_start := nafill(smk_age_start, type = "locf"), by = "id"]

  # remove raw variables no longer needed
  data <- subset(data,select = -c(smever,smnow,smcigs,smncigs,aglquit,smagbg,smoker,ever,
                                  lag1,lag2,lag3,lag4,lag5,lag6,lag7,lag8))


  return(data)
}


