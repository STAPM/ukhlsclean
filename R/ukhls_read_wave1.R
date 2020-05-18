
#' Read Understanding Society Wave 1
#'
#' Reads and does basic cleaning on the UKHLS first wave.
#'
#' A sample of the population living in private households. All persons living in the house, including those
#' under 2 years were eligible for inclusion. At addresses where there were more than two children under 16,
#' two children were selected at random. Information was obtained directly from persons aged 13 and
#' over. Information about children aged 0-12 was obtained from a parent, with the child present.
#'
#' MISSING VALUES
#'
#' \itemize{
#' \item -1 Don't know.
#' \item -2 Refused: Used only for variables on the nurse schedules, this code indicates that a
#' respondent refused a particular measurement or test or the measurement was attempted but not
#' obtained or not attempted.
#' \item -8 Not applicable: Used to signify that a particular variable did not apply to a given respondent
#' usually because of internal routing. For example, men in women only questions.
#' \item -9 Missing
#' }
#'
#' @param root Character - the root directory.
#' @param file Character - the file path and name.
#' @importFrom data.table :=
#' @return Returns a data table. Note that:
#' \itemize{
#' \item Missing data ("NA", "", "-1", "-2", "-6", "-7", "-9", "-90", "-90.0", "N/A") is replace with NA,
#' except -8 ("don't know") as this is data.
#' \item All variable names are converted to lower case.
#' \item Each data point is assigned a weight of 1 as there is no weight variable supplied.
#' \item A single sampling cluster is assigned.
#' \item The probabilistic sampling unit have the year appended to them.
#' }
#' @export
ukhls_read_wave1 <- function(
  root = c("C:/"),
  file = "Users/User/Documents/Datasets/UKHLS/tab/ukhls_w1/a_indresp.tab"
) {



  data <- data.table::fread(
    paste0(root[1], file),
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars  <- colnames(data[ , c(1,6,7,16,17)])
  x_vars <- colnames(data[ , c(12,13,14,23,25,1295,1302,1303,1339)])
  weight_vars <- colnames(data[,c(1398)])
  health_vars <- colnames(data[,c(147:168,183:199,201:234,1379,1380)])
  work_vars <- colnames(data[,c(260,261,264:267,269,272,320,332,333,344:356,358:369,370:381)])
  finance_vars <- colnames(data[,c(530,531)])
  benefit_vars <- colnames(data[,c(1114:1125)])
  income_vars <- colnames(data[,c(1254:1273)])

  names <- c(id_vars,x_vars,work_vars,finance_vars,benefit_vars,income_vars,health_vars,weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","a_psu","a_strata","a_istrtdatm","a_istrtdaty","a_sex","a_dvage","a_birthy","a_jbstat",
                         "a_mlstat","a_hiqual_dv","a_ethn_dv","a_country","a_gor_dv",
                         ## Health Variables
                         "a_sf12pcs_dv","a_sf12mcs_dv",
                         "a_hcond1","a_hcond2","a_hcond3","a_hcond4","a_hcond5","a_hcond6","a_hcond7","a_hcond8","a_hcond9",
                         "a_hcond10","a_hcond11","a_hcond12","a_hcond13","a_hcond14","a_hcond15","a_hcond16","a_hcond17",
                         "a_hconds01","a_hconds02","a_hconds03","a_hconds04","a_hconds05","a_hconds06","a_hconds07","a_hconds08","a_hconds09",
                         "a_hconds10","a_hconds11","a_hconds12","a_hconds13","a_hconds14","a_hconds15","a_hconds16","a_hconds17",
                         "a_hconda01","a_hconda02","a_hconda03","a_hconda04","a_hconda05","a_hconda06","a_hconda07","a_hconda08","a_hconda09",
                         "a_hconda10","a_hconda11","a_hconda12","a_hconda13","a_hconda14","a_hconda15","a_hconda16","a_hconda17",
                         ## Weights
                         "a_indinus_xw"),

                       c("pidp","psu","strata","month","year","sex","age","birthyear","econstat",
                         "marstat","hiqual","ethnicity","country","region",
                         ## Health Variables
                         "sf12pcs","sf12mcs",
                         "hcond1","hcond2","hcond3","hcond4","hcond5","hcond6","hcond7","hcond8","hcond9",
                         "hcond10","hcond11","hcond12","hcond13","hcond14","hcond15","hcond16","hcond17",
                         "hconds01","hconds02","hconds03","hconds04","hconds05","hconds06","hconds07","hconds08","hconds09",
                         "hconds10","hconds11","hconds12","hconds13","hconds14","hconds15","hconds16","hconds17",
                         "hconda01","hconda02","hconda03","hconda04","hconda05","hconda06","hconda07","hconda08","hconda09",
                         "hconda10","hconda11","hconda12","hconda13","hconda14","hconda15","hconda16","hconda17",
                         ## Weights
                         "weight"))


  data[ , quarter := c(1:4)[findInterval(month, c(1, 4, 7, 10))]]

  return(data[])
}
