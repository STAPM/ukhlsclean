#' Clean Smoking Variables
#'
#' Clean smoking variables and backwards impute smoking behaviour to waves prior to the smoking
#' questions being asked in the UKHLS.
#'
#' @export
clean_smoke <- function(data = NULL) {

  # ensure data is sorted
  data <- data[order(pidp,wave),]


  return(data)
}


