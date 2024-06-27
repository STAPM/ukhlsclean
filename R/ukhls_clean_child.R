#' Read Understanding Society Child Data
#'
#' Reads and performs basic cleaning operations on all waves of the UKHLS Child data.
#' Missing values as detailed below are all set to NA.
#'
#' A sample of the population living in private households. All persons living in the household, including those
#' under 2 years were eligible for inclusion. At addresses where there were more than two children under 16,
#' two children were selected at random. Information was obtained directly from persons aged 13 and
#' over. Information about children aged 0-12 was obtained from a parent, with the child present.
#'
#' MISSING VALUES
#'
#' \itemize{
#' \item -1 Don't know. When the respondent does not know the answer to a question.
#' \item -2 Refused: When the respondent refuses to answer a question.
#' \item -7 Proxy: A question not included in the subset of questions asked of proxy respondents.
#' obtained or not attempted.
#' \item -8 Not applicable: Used to signify that a particular variable did not apply to a given respondent
#' usually because of internal routing. For example, men in women only questions.
#' \item -9 Missing by error or implausible answer.
#' }
#'
#' @source University of Essex, Institute for Social and Economic Research. (2022). Understanding Society: Waves 1-12, 2009-2021
#' and Harmonised BHPS: Waves 1-18, 1991-2009. [data collection]. 17th Edition. UK Data Service. SN: 6614,
#' \href{https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=6614}{DOI: 10.5255/UKDA-SN-6614-18}
#'
#' @param root Character - the root directory.
#' @param file Character - the file path and name.
#' @param full Logical - TRUE if restricting the sample to full interviews only (excluding proxies)
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
ukhlsclean_child <- function(
    root = root, # c("X:/"),
    file = file, # "HAR_PR/PR/USoc/Data/SN6614_2022_11_29/tab/ukhls",
    full = TRUE
) {

  path <- here::here(paste0(root, file))

  cat(crayon::magenta("\nReading UKHLS Child datasets"))

  ################
  #### Wave 1

  cat(crayon::red("\nWave 1 child data..."))

  child_data <- data.table::fread(
    paste0(path, "/a_child.tab"),
    showProgress = TRUE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )

  data.table::setnames(child_data, names(child_data), tolower(names(child_data)))

  id_vars          <- Hmisc::Cs(pidp,a_hidp)
  demographic_vars <- Hmisc::Cs(a_dvage,a_sex)
  weight_vars      <- Hmisc::Cs(a_psnenus_xw)

  names <- c(id_vars, demographic_vars, weight_vars)
  names <- tolower(names)

  child_data <- child_data[ , names, with = F]

  data.table::setnames(child_data,

                       c("pidp","a_hidp",
                         ## demographic
                         "a_dvage","a_sex",
                         ## weights
                         "a_psnenus_xw"
                       ),
                       c("pidp","hidp",
                         ## demographic
                         "age","sex",
                         ## weights
                         "weight_xw"
                       ))

  child_data[, wave := "UKHLS Child Wave 1"]
  child_data[, wave_no := 1]
  child_data[, bhps_sample := FALSE]
  child_data[, dataset := "UKHLS"]
  child_data[, id := pidp]

  ################
  #### Wave 2

  cat(crayon::red("\nWave 2 child data..."))

  data_merge <- data.table::fread(
    paste0(path, "/b_child.tab"),
    showProgress = TRUE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )

  data.table::setnames(data_merge, names(data_merge), tolower(names(data_merge)))

  id_vars          <- Hmisc::Cs(pidp,pid,b_hidp)
  demographic_vars <- Hmisc::Cs(b_dvage,b_sex)
  weight_vars      <- Hmisc::Cs(b_psnenub_xw)

  names <- c(id_vars, demographic_vars,weight_vars)
  names <- tolower(names)

  data_merge <- data_merge[ , names, with = F]

  data.table::setnames(data_merge,

                       c("pidp","pid","b_hidp",
                         ## demographic
                         "b_dvage","b_sex",
                         ## weights
                         "b_psnenub_xw"
                       ),
                       c("pidp","pid","hidp",
                         ## demographic
                         "age","sex",
                         ## weights
                         "weight_xw"
                       ))

  data_merge[, wave := "UKHLS Child Wave 2"]
  data_merge[, wave_no := 2]
  data_merge[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data_merge[, dataset := "UKHLS"]
  data_merge[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  child_data <- data.table::rbindlist(list(child_data, data_merge), fill = TRUE)


  return(child_data)
}
