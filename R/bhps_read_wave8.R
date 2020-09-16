#' Read British Household Panel Survey Wave 8
#'
#' Reads and does basic cleaning on the BHPS 8th wave.
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
#' @param path Character - the file path and name.
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
bhps_read_wave8 <- function(
  root = c("C:/"),
  path = "Users/User/Documents/Datasets/UKHLS/tab/"
) {

  print("Reading BHPS Wave 8")
  data <- data.table::fread(
    paste0(root[1], path, "bhps_w8/bh_indresp.tab"),
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- colnames(data[ , c(1,4,5)])
  demographic_vars <- colnames(data[ , c(33,955,32,1140,119)])
  econ_stat_vars   <- colnames(data[,c(34)])
  weight_vars      <- colnames(data[,c(1068,1066)])

  names <- c(id_vars,demographic_vars,econ_stat_vars,weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,
                       c("pid","bh_hidp","bh_pno",
                         ## demographic
                         "bh_sex","bh_age_dv","bh_doby","bh_gor_dv","bh_race",
                         ## economic stauts
                         "bh_jbstat",
                         ## weight
                         "bh_xrwght","bh_lrwght"),
                       c("pid","hidp","person_number",
                         ## demographic
                         "sex","age","birth_year","region","ethnicity_raw",
                         ## economic status
                         "econ_stat",
                         ## weight
                         "weight","weight_l"))

  data$wave <- "BHPS Wave 8"

  data$bhps <- TRUE
  ######## Add in cross-wave data

  data.xwave <- data.table::fread(
    paste0(root[1], path, "bhps_wx/xwaveid_bh.tab"),
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.xwave, names(data.xwave), tolower(names(data.xwave)))

  xwave_vars  <- colnames(data.xwave[ , c(1,7,8,17,18,34)])

  data.xwave <- data.xwave[ , xwave_vars, with = F]
  data.table::setnames(data.xwave,
                       # old names
                       c("pid","psu","strata","dcsedfl_dv","dcsedw_dv_bh"),
                       # new names
                       c("pid","psu","strata","deceased","deceased_when"))

  ####### Combine - keep all observations in the main data and drop excess xwave observations

  data_merged <- merge(x = data,
                       y = data.xwave,
                       by="pid",
                       all.x=TRUE,
                       all.y=FALSE)

  return(data_merged[])
}
