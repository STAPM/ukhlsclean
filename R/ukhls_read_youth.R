#' Read Understanding Society Wave 1
#'
#' Reads and performs basic cleaning operations on all waves of the UKHLS Youth data. Missing values as detailed below are all set to NA.
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
ukhls_read_youth <- function(
    root = root, # c("X:/"),
    file = file, # "HAR_PR/PR/USoc/Data/SN6614_2022_11_29/tab/ukhls",
    full = TRUE
) {

  cat(crayon::magenta("\tReading UKHLS Youth datasets"))

  cat(crayon::red("\tWave 1..."))

  path <- here::here(paste0(root, file))

  youth_data <- data.table::fread(
    paste0(path, "/a_youth.tab"),
    showProgress = TRUE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain youth interviews (21 is youth interview value)
    youth_data <- youth_data[a_ivfio==21,]
  }

  data.table::setnames(youth_data, names(youth_data), tolower(names(youth_data)))

  id_vars          <- Hmisc::Cs(pidp,a_hidp,a_pno,a_psu,a_strata)
  demographic_vars <- Hmisc::Cs(a_dvage)
  smoke_vars       <- Hmisc::Cs(a_ypevrsmo,a_ypsmofrq
                                # ,a_ypevresmo
                                )
  alc_vars         <- Hmisc::Cs(a_ypevralc)

  names <- c(id_vars, demographic_vars, smoke_vars, alc_vars)
  names <- tolower(names)

  youth_data <- youth_data[ , names, with = F]

  data.table::setnames(youth_data,

                       c("pidp","a_hidp","a_pno","a_psu","a_strata",
                         ## demographic
                         "a_dvage",
                         ## smoking variables
                         "a_ypevrsmo","a_ypsmofrq",
                         # "a_ypevresmo", #e smoke waves 7:12
                         ## alcohol variables
                         "a_ypevralc" # waves 1:12
                       ),
                       c("pidp","hidp","person_number","psu","strata",
                         ## demographic
                         "age",
                         ## smoking variables
                         "ypevrsmo","ypsmofrq",
                         # "ypevresmo",
                         ## alcohol variables
                         "ypevralc"
                       ))

  #### Wave 2

  cat(crayon::red("\tWave 2..."))

  path <- here::here(paste0(root, file))

  data_merge <- data.table::fread(
    paste0(path, "/b_youth.tab"),
    showProgress = TRUE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain youth interviews (21 is youth interview value)
    data_merge <- data_merge[b_ivfio==21,]
  }

  data.table::setnames(data_merge, names(data_merge), tolower(names(data_merge)))

  id_vars          <- Hmisc::Cs(pidp,b_hidp,b_pno,b_psu,b_strata)
  demographic_vars <- Hmisc::Cs(b_dvage)
  smoke_vars       <- Hmisc::Cs(b_ypevrsmo,b_ypsmofrq
                                # ,b_ypevresmo
                                )
  alc_vars         <- Hmisc::Cs(b_ypevralc)

  names <- c(id_vars, demographic_vars, smoke_vars, alc_vars)
  names <- tolower(names)

  data_merge <- data_merge[ , names, with = F]

  data.table::setnames(data_merge,

                       c("pidp","b_hidp","b_pno","b_psu","b_strata",
                         ## demographic
                         "b_dvage",
                         ## smoking variables
                         "b_ypevrsmo","b_ypsmofrq",
                         # "b_ypevresmo", #e smoke waves 7:12
                         ## alcohol variables
                         "b_ypevralc" # waves 1:12
                       ),
                       c("pidp","hidp","person_number","psu","strata",
                         ## demographic
                         "age",
                         ## smoking variables
                         "ypevrsmo","ypsmofrq",
                         # "ypevresmo",
                         ## alcohol variables
                         "ypevralc"
                       ))

  youth_data <- rbind(youth_data, data_merge)

  #### Wave 3

  cat(crayon::red("\tWave 3..."))

  path <- here::here(paste0(root, file))

  data_merge <- data.table::fread(
    paste0(path, "/c_youth.tab"),
    showProgress = TRUE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain youth interviews (21 is youth interview value)
    data_merge <- data_merge[c_ivfio==21,]
  }

  data.table::setnames(data_merge, names(data_merge), tolower(names(data_merge)))

  id_vars          <- Hmisc::Cs(pidp,c_hidp,c_pno,c_psu,c_strata)
  demographic_vars <- Hmisc::Cs(c_dvage)
  smoke_vars       <- Hmisc::Cs(c_ypevrsmo,c_ypsmofrq
                                # ,c_ypevresmo
                                )
  alc_vars         <- Hmisc::Cs(c_ypevralc)

  names <- c(id_vars, demographic_vars, smoke_vars, alc_vars)
  names <- tolower(names)

  data_merge <- data_merge[ , names, with = F]

  data.table::setnames(data_merge,

                       c("pidp","c_hidp","c_pno","c_psu","c_strata",
                         ## demographic
                         "c_dvage",
                         ## smoking variables
                         "c_ypevrsmo","c_ypsmofrq",
                         # "c_ypevresmo", #e smoke waves 7:12
                         ## alcohol variables
                         "c_ypevralc" # waves 1:12
                       ),
                       c("pidp","hidp","person_number","psu","strata",
                         ## demographic
                         "age",
                         ## smoking variables
                         "ypevrsmo","ypsmofrq",
                         # "ypevresmo",
                         ## alcohol variables
                         "ypevralc"
                       ))

  youth_data <- rbind(youth_data, data_merge)

  #### Wave 4

  cat(crayon::red("\tWave 4..."))

  path <- here::here(paste0(root, file))

  data_merge <- data.table::fread(
    paste0(path, "/d_youth.tab"),
    showProgress = TRUE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain youth interviews (21 is youth interview value)
    data_merge <- data_merge[d_ivfio==21,]
  }

  data.table::setnames(data_merge, names(data_merge), tolower(names(data_merge)))

  id_vars          <- Hmisc::Cs(pidp,d_hidp,d_pno,d_psu,d_strata)
  demographic_vars <- Hmisc::Cs(d_dvage)
  smoke_vars       <- Hmisc::Cs(d_ypevrsmo,d_ypsmofrq
                                # ,d_ypevresmo
                                )
  alc_vars         <- Hmisc::Cs(d_ypevralc)

  names <- c(id_vars, demographic_vars, smoke_vars, alc_vars)
  names <- tolower(names)

  data_merge <- data_merge[ , names, with = F]

  data.table::setnames(data_merge,

                       c("pidp","d_hidp","d_pno","d_psu","d_strata",
                         ## demographic
                         "d_dvage",
                         ## smoking variables
                         "d_ypevrsmo","d_ypsmofrq",
                         # "d_ypevresmo", #e smoke waves 7:12
                         ## alcohol variables
                         "d_ypevralc" # waves 1:12
                       ),
                       c("pidp","hidp","person_number","psu","strata",
                         ## demographic
                         "age",
                         ## smoking variables
                         "ypevrsmo","ypsmofrq",
                         # "ypevresmo",
                         ## alcohol variables
                         "ypevralc"
                       ))

  youth_data <- rbind(youth_data, data_merge)

  #### Wave 5

  cat(crayon::red("\tWave 5..."))

  path <- here::here(paste0(root, file))

  data_merge <- data.table::fread(
    paste0(path, "/e_youth.tab"),
    showProgress = TRUE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain youth interviews (21 is youth interview value)
    data_merge <- data_merge[e_ivfio==21,]
  }

  data.table::setnames(data_merge, names(data_merge), tolower(names(data_merge)))

  id_vars          <- Hmisc::Cs(pidp,e_hidp,e_pno,e_psu,e_strata)
  demographic_vars <- Hmisc::Cs(e_dvage)
  smoke_vars       <- Hmisc::Cs(e_ypevrsmo,e_ypsmofrq
                                # ,e_ypevresmo
                                )
  alc_vars         <- Hmisc::Cs(e_ypevralc)

  names <- c(id_vars, demographic_vars, smoke_vars, alc_vars)
  names <- tolower(names)

  data_merge <- data_merge[ , names, with = F]

  data.table::setnames(data_merge,

                       c("pidp","e_hidp","e_pno","e_psu","e_strata",
                         ## demographic
                         "e_dvage",
                         ## smoking variables
                         "e_ypevrsmo","e_ypsmofrq",
                         # "e_ypevresmo", #e smoke waves 7:12
                         ## alcohol variables
                         "e_ypevralc" # waves 1:12
                       ),
                       c("pidp","hidp","person_number","psu","strata",
                         ## demographic
                         "age",
                         ## smoking variables
                         "ypevrsmo","ypsmofrq",
                         # "ypevresmo",
                         ## alcohol variables
                         "ypevralc"
                       ))

  youth_data <- rbind(youth_data, data_merge)

  #### Wave 6

  cat(crayon::red("\tWave 6..."))

  path <- here::here(paste0(root, file))

  data_merge <- data.table::fread(
    paste0(path, "/f_youth.tab"),
    showProgress = TRUE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain youth interviews (21 is youth interview value)
    data_merge <- data_merge[f_ivfio==21,]
  }

  data.table::setnames(data_merge, names(data_merge), tolower(names(data_merge)))

  id_vars          <- Hmisc::Cs(pidp,f_hidp,f_pno,f_psu,f_strata)
  demographic_vars <- Hmisc::Cs(f_dvage)
  smoke_vars       <- Hmisc::Cs(f_ypevrsmo,f_ypsmofrq
                                # ,f_ypevresmo
                                )
  alc_vars         <- Hmisc::Cs(f_ypevralc)

  names <- c(id_vars, demographic_vars, smoke_vars, alc_vars)
  names <- tolower(names)

  data_merge <- data_merge[ , names, with = F]

  data.table::setnames(data_merge,

                       c("pidp","f_hidp","f_pno","f_psu","f_strata",
                         ## demographic
                         "f_dvage",
                         ## smoking variables
                         "f_ypevrsmo","f_ypsmofrq",
                         # "f_ypevresmo", #e smoke waves 7:12
                         ## alcohol variables
                         "f_ypevralc" # waves 1:12
                       ),
                       c("pidp","hidp","person_number","psu","strata",
                         ## demographic
                         "age",
                         ## smoking variables
                         "ypevrsmo","ypsmofrq",
                         # "ypevresmo",
                         ## alcohol variables
                         "ypevralc"
                       ))

  youth_data <- rbind(youth_data, data_merge)

  #### Wave 7

  cat(crayon::red("\tWave 7..."))

  path <- here::here(paste0(root, file))

  data_merge <- data.table::fread(
    paste0(path, "/g_youth.tab"),
    showProgress = TRUE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain youth interviews (21 is youth interview value)
    data_merge <- data_merge[g_ivfio==21,]
  }

  data.table::setnames(data_merge, names(data_merge), tolower(names(data_merge)))

  id_vars          <- Hmisc::Cs(pidp,g_hidp,g_pno,g_psu,g_strata)
  demographic_vars <- Hmisc::Cs(g_dvage)
  smoke_vars       <- Hmisc::Cs(g_ypevrsmo,g_ypsmofrq
                                # ,g_ypevresmo
                                )
  alc_vars         <- Hmisc::Cs(g_ypevralc)

  names <- c(id_vars, demographic_vars, smoke_vars, alc_vars)
  names <- tolower(names)

  data_merge <- data_merge[ , names, with = F]

  data.table::setnames(data_merge,

                       c("pidp","g_hidp","g_pno","g_psu","g_strata",
                         ## demographic
                         "g_dvage",
                         ## smoking variables
                         "g_ypevrsmo","g_ypsmofrq",
                         # "g_ypevresmo", #e smoke waves 7:12
                         ## alcohol variables
                         "g_ypevralc" # waves 1:12
                       ),
                       c("pidp","hidp","person_number","psu","strata",
                         ## demographic
                         "age",
                         ## smoking variables
                         "ypevrsmo","ypsmofrq",
                         # "ypevresmo",
                         ## alcohol variables
                         "ypevralc"
                       ))

  youth_data <- rbind(youth_data, data_merge)

  #### Wave 8

  cat(crayon::red("\tWave 8..."))

  path <- here::here(paste0(root, file))

  data_merge <- data.table::fread(
    paste0(path, "/h_youth.tab"),
    showProgress = TRUE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain youth interviews (21 is youth interview value)
    data_merge <- data_merge[h_ivfio==21,]
  }

  data.table::setnames(data_merge, names(data_merge), tolower(names(data_merge)))

  id_vars          <- Hmisc::Cs(pidp,h_hidp,h_pno,h_psu,h_strata)
  demographic_vars <- Hmisc::Cs(h_dvage)
  smoke_vars       <- Hmisc::Cs(h_ypevrsmo,h_ypsmofrq
                                # ,h_ypevresmo
                                )
  alc_vars         <- Hmisc::Cs(h_ypevralc)

  names <- c(id_vars, demographic_vars, smoke_vars, alc_vars)
  names <- tolower(names)

  data_merge <- data_merge[ , names, with = F]

  data.table::setnames(data_merge,

                       c("pidp","h_hidp","h_pno","h_psu","h_strata",
                         ## demographic
                         "h_dvage",
                         ## smoking variables
                         "h_ypevrsmo","h_ypsmofrq",
                         # "h_ypevresmo", #e smoke waves 7:12
                         ## alcohol variables
                         "h_ypevralc" # waves 1:12
                       ),
                       c("pidp","hidp","person_number","psu","strata",
                         ## demographic
                         "age",
                         ## smoking variables
                         "ypevrsmo","ypsmofrq",
                         # "ypevresmo",
                         ## alcohol variables
                         "ypevralc"
                       ))

  youth_data <- rbind(youth_data, data_merge)

  #### Wave 9

  cat(crayon::red("\tWave 9..."))

  path <- here::here(paste0(root, file))

  data_merge <- data.table::fread(
    paste0(path, "/i_youth.tab"),
    showProgress = TRUE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain youth interviews (21 is youth interview value)
    data_merge <- data_merge[i_ivfio==21,]
  }

  data.table::setnames(data_merge, names(data_merge), tolower(names(data_merge)))

  id_vars          <- Hmisc::Cs(pidp,i_hidp,i_pno,i_psu,i_strata)
  demographic_vars <- Hmisc::Cs(i_dvage)
  smoke_vars       <- Hmisc::Cs(i_ypevrsmo,i_ypsmofrq
                                # ,i_ypevresmo
                                )
  alc_vars         <- Hmisc::Cs(i_ypevralc)

  names <- c(id_vars, demographic_vars, smoke_vars, alc_vars)
  names <- tolower(names)

  data_merge <- data_merge[ , names, with = F]

  data.table::setnames(data_merge,

                       c("pidp","i_hidp","i_pno","i_psu","i_strata",
                         ## demographic
                         "i_dvage",
                         ## smoking variables
                         "i_ypevrsmo","i_ypsmofrq",
                         # "i_ypevresmo", #e smoke waves 7:12
                         ## alcohol variables
                         "i_ypevralc" # waves 1:12
                       ),
                       c("pidp","hidp","person_number","psu","strata",
                         ## demographic
                         "age",
                         ## smoking variables
                         "ypevrsmo","ypsmofrq",
                         # "ypevresmo",
                         ## alcohol variables
                         "ypevralc"
                       ))

  youth_data <- rbind(youth_data, data_merge)

  #### Wave 10

  cat(crayon::red("\tWave 10..."))

  path <- here::here(paste0(root, file))

  data_merge <- data.table::fread(
    paste0(path, "/j_youth.tab"),
    showProgress = TRUE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain youth interviews (21 is youth interview value)
    data_merge <- data_merge[j_ivfio==21,]
  }

  data.table::setnames(data_merge, names(data_merge), tolower(names(data_merge)))

  id_vars          <- Hmisc::Cs(pidp,j_hidp,j_pno,j_psu,j_strata)
  demographic_vars <- Hmisc::Cs(j_dvage)
  smoke_vars       <- Hmisc::Cs(j_ypevrsmo,j_ypsmofrq
                                # ,j_ypevresmo
                                )
  alc_vars         <- Hmisc::Cs(j_ypevralc)

  names <- c(id_vars, demographic_vars, smoke_vars, alc_vars)
  names <- tolower(names)

  data_merge <- data_merge[ , names, with = F]

  data.table::setnames(data_merge,

                       c("pidp","j_hidp","j_pno","j_psu","j_strata",
                         ## demographic
                         "j_dvage",
                         ## smoking variables
                         "j_ypevrsmo","j_ypsmofrq",
                         # "j_ypevresmo", #e smoke waves 7:12
                         ## alcohol variables
                         "j_ypevralc" # waves 1:12
                       ),
                       c("pidp","hidp","person_number","psu","strata",
                         ## demographic
                         "age",
                         ## smoking variables
                         "ypevrsmo","ypsmofrq",
                         # "ypevresmo",
                         ## alcohol variables
                         "ypevralc"
                       ))

  youth_data <- rbind(youth_data, data_merge)

  #### Wave 11

  cat(crayon::red("\tWave 11..."))

  path <- here::here(paste0(root, file))

  data_merge <- data.table::fread(
    paste0(path, "/k_youth.tab"),
    showProgress = TRUE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain youth interviews (21 is youth interview value)
    data_merge <- data_merge[k_ivfio==21,]
  }

  data.table::setnames(data_merge, names(data_merge), tolower(names(data_merge)))

  id_vars          <- Hmisc::Cs(pidp,k_hidp,k_pno,k_psu,k_strata)
  demographic_vars <- Hmisc::Cs(k_dvage)
  smoke_vars       <- Hmisc::Cs(k_ypevrsmo,k_ypsmofrq
                                # ,k_ypevresmo
                                )
  alc_vars         <- Hmisc::Cs(k_ypevralc)

  names <- c(id_vars, demographic_vars, smoke_vars, alc_vars)
  names <- tolower(names)

  data_merge <- data_merge[ , names, with = F]

  data.table::setnames(data_merge,

                       c("pidp","k_hidp","k_pno","k_psu","k_strata",
                         ## demographic
                         "k_dvage",
                         ## smoking variables
                         "k_ypevrsmo","k_ypsmofrq",
                         # "k_ypevresmo", #e smoke waves 7:12
                         ## alcohol variables
                         "k_ypevralc" # waves 1:12
                       ),
                       c("pidp","hidp","person_number","psu","strata",
                         ## demographic
                         "age",
                         ## smoking variables
                         "ypevrsmo","ypsmofrq",
                         # "ypevresmo",
                         ## alcohol variables
                         "ypevralc"
                       ))

  youth_data <- rbind(youth_data, data_merge)

  #### Wave 12

  cat(crayon::red("\tWave 12..."))

  path <- here::here(paste0(root, file))

  data_merge <- data.table::fread(
    paste0(path, "/l_youth.tab"),
    showProgress = TRUE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain youth interviews (21 is youth interview value)
    data_merge <- data_merge[l_ivfio==21,]
  }

  data.table::setnames(data_merge, names(data_merge), tolower(names(data_merge)))

  id_vars          <- Hmisc::Cs(pidp,l_hidp,l_pno,l_psu,l_strata)
  demographic_vars <- Hmisc::Cs(l_dvage)
  smoke_vars       <- Hmisc::Cs(l_ypevrsmo,l_ypsmofrq
                                # ,l_ypevresmo
                                )
  alc_vars         <- Hmisc::Cs(l_ypevralc)

  names <- c(id_vars, demographic_vars, smoke_vars, alc_vars)
  names <- tolower(names)

  data_merge <- data_merge[ , names, with = F]

  data.table::setnames(data_merge,

                       c("pidp","l_hidp","l_pno","l_psu","l_strata",
                         ## demographic
                         "l_dvage",
                         ## smoking variables
                         "l_ypevrsmo","l_ypsmofrq",
                         # "l_ypevresmo", #e smoke waves 7:12
                         ## alcohol variables
                         "l_ypevralc" # waves 1:12
                       ),
                       c("pidp","hidp","person_number","psu","strata",
                         ## demographic
                         "age",
                         ## smoking variables
                         "ypevrsmo","ypsmofrq",
                         # "ypevresmo",
                         ## alcohol variables
                         "ypevralc"
                       ))

  youth_data <- rbind(youth_data, data_merge)


  return(youth_data)
}
