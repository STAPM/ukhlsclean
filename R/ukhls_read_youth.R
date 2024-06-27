#' Read Understanding Society Youth Data
#'
#' Reads and performs basic cleaning operations on all waves of the UKHLS Youth data.
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
ukhls_read_youth <- function(
    root = root, # c("X:/"),
    file = file, # "HAR_PR/PR/USoc/Data/SN6614_2022_11_29/tab/ukhls",
    full = TRUE
) {

  cat(crayon::magenta("\nReading UKHLS Youth datasets"))

  cat(crayon::red("\nWave 1 youth data..."))

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

  id_vars          <- Hmisc::Cs(pidp,a_hidp,a_pno,a_psu,a_strata,a_intdaty_dv,a_intdatm_dv,a_intdatd_dv)
  demographic_vars <- Hmisc::Cs(a_dvage,a_ypsex,a_gor_dv,a_country,a_urban_dv)
  smoke_vars       <- Hmisc::Cs(a_ypevrsmo,a_ypsmofrq
                                # ,a_ypevresmo
                                )
  alc_vars         <- Hmisc::Cs(a_ypevralc)
  weight_vars      <- Hmisc::Cs(a_ythscus_xw)

  names <- c(id_vars, demographic_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  youth_data <- youth_data[ , names, with = F]

  data.table::setnames(youth_data,

                       c("pidp","a_hidp","a_pno","a_psu","a_strata","a_intdaty_dv","a_intdatm_dv","a_intdatd_dv",
                         ## demographic
                         "a_dvage","a_ypsex","a_gor_dv","a_country","a_urban_dv",
                         ## smoking variables
                         "a_ypevrsmo","a_ypsmofrq",
                         # "a_ypevresmCo", #e smCCoke waves 7:12
                         ## alcohol variables
                         "a_ypevralc", # waves 1:12
                         ## weights
                         "a_ythscus_xw"
                       ),
                       c("pidp","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "age","sex","region","country","area",
                         ## smoking variables
                         "ypevrsmo","ypsmofrq",
                         # "ypevresmo",
                         ## alcohol variables
                         "ypevralc",
                         ## weights
                         "weight_xw"
                       ))

  youth_data[, wave := "UKHLS Youth Wave 1"]
  youth_data[, wave_no := 1]
  # youth_data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)] ### because doesnt exist without pid, setting as false for now
  youth_data[, bhps_sample := FALSE]
  youth_data[, dataset := "UKHLS"]
  # youth_data[, id := ifelse(bhps_sample==FALSE, pidp, pid)]
  youth_data[, id := pidp]

  #########################################
  ######## ADD IN CROSS-WAVE DATA #########
  cat(crayon::red("\tCross-Wave..."))
  data.xwave <- data.table::fread(
    paste0(path, "/xwavedat.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.xwave, names(data.xwave), tolower(names(data.xwave)))
  xwave_vars  <- colnames(data.xwave[ , c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv")])
  data.xwave <- data.xwave[ , xwave_vars, with = F]
  data.table::setnames(data.xwave,
                       # old names
                       c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv"),
                       # new names
                       c("pidp","ethnicity","deceased","deceased_when"))

  ####### Combine - keep all observations in the main data and drop excess xwave observations
  youth_data <- merge(x = youth_data,
                       y = data.xwave,
                       by="pidp",
                       all.x=TRUE,
                       all.y=FALSE)


  #### Wave 2

  cat(crayon::red("\nWave 2 youth data..."))

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

  id_vars          <- Hmisc::Cs(pidp,pid,b_hidp,b_pno,b_psu,b_strata,b_intdaty_dv,b_intdatm_dv,b_intdatd_dv)
  demographic_vars <- Hmisc::Cs(b_dvage,b_ypsex,b_gor_dv,b_country,b_urban_dv)
  smoke_vars       <- Hmisc::Cs(b_ypevrsmo,b_ypsmofrq
                                # ,b_ypevresmo
                                )
  alc_vars         <- Hmisc::Cs(b_ypevralc)
  weight_vars      <- Hmisc::Cs(b_ythscub_xw)

  names <- c(id_vars, demographic_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data_merge <- data_merge[ , names, with = F]

  data.table::setnames(data_merge,

                       c("pidp","pid","b_hidp","b_pno","b_psu","b_strata","b_intdaty_dv","b_intdatm_dv","b_intdatd_dv",
                         ## demographic
                         "b_dvage","b_ypsex","b_gor_dv","b_country","b_urban_dv",
                         ## smoking variables
                         "b_ypevrsmo","b_ypsmofrq",
                         # "b_ypevresmo", #e smoke waves 7:12
                         ## alcohol variables
                         "b_ypevralc", # waves 1:12
                         ## weights
                         "b_ythscub_xw"
                       ),
                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "age","sex","region","country","area",
                         ## smoking variables
                         "ypevrsmo","ypsmofrq",
                         # "ypevresmo",
                         # alcohol variables
                         "ypevralc",
                         ## weights
                         "weight_xw"
                       ))

  data_merge[, wave := "UKHLS Youth Wave 2"]
  data_merge[, wave_no := 2]
  data_merge[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data_merge[, dataset := "UKHLS"]
  data_merge[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  #########################################
  ######## ADD IN CROSS-WAVE DATA #########
  cat(crayon::red("\tCross-Wave..."))
  data.xwave <- data.table::fread(
    paste0(path, "/xwavedat.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.xwave, names(data.xwave), tolower(names(data.xwave)))
  xwave_vars  <- colnames(data.xwave[ , c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv")])
  data.xwave <- data.xwave[ , xwave_vars, with = F]
  data.table::setnames(data.xwave,
                       # old names
                       c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv"),
                       # new names
                       c("pidp","ethnicity","deceased","deceased_when"))

  ####### Combine - keep all observations in the main data and drop excess xwave observations
  data_merge <- merge(x = data_merge,
                      y = data.xwave,
                      by="pidp",
                      all.x=TRUE,
                      all.y=FALSE)


  youth_data <- data.table::rbindlist(list(youth_data, data_merge), fill = TRUE)


  #### Wave 3

  cat(crayon::red("\nWave 3 youth data..."))

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

  id_vars          <- Hmisc::Cs(pidp,pid,c_hidp,c_pno,c_psu,c_strata,c_intdaty_dv,c_intdatm_dv,c_intdatd_dv)
  demographic_vars <- Hmisc::Cs(c_dvage,c_ypsex,c_gor_dv,c_country,c_urban_dv)
  smoke_vars       <- Hmisc::Cs(c_ypevrsmo,c_ypsmofrq
                                # ,c_ypevresmo
                                )
  alc_vars         <- Hmisc::Cs(c_ypevralc)
  weight_vars      <- Hmisc::Cs(c_ythscub_xw)

  names <- c(id_vars, demographic_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data_merge <- data_merge[ , names, with = F]

  data.table::setnames(data_merge,

                       c("pidp","pid","c_hidp","c_pno","c_psu","c_strata","c_intdaty_dv","c_intdatm_dv","c_intdatd_dv",
                         ## demographic
                         "c_dvage","c_ypsex","c_gor_dv","c_country","c_urban_dv",
                         ## smoking variables
                         "c_ypevrsmo","c_ypsmofrq",
                         # "c_ypevresmo", #e smoke waves 7:12
                         ## alcohol variables
                         "c_ypevralc",
                         ## weights
                         "c_ythscub_xw"
                       ),
                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "age","sex","region","country","area",
                         ## smoking variables
                         "ypevrsmo","ypsmofrq",
                         # "ypevresmo",
                         ## alcohol variables
                         "ypevralc",
                         ## weights
                         "weight_xw"
                       ))

  data_merge[, wave := "UKHLS Youth Wave 3"]
  data_merge[, wave_no := 3]
  data_merge[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data_merge[, dataset := "UKHLS"]
  data_merge[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  #########################################
  ######## ADD IN CROSS-WAVE DATA #########
  cat(crayon::red("\tCross-Wave..."))
  data.xwave <- data.table::fread(
    paste0(path, "/xwavedat.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.xwave, names(data.xwave), tolower(names(data.xwave)))
  xwave_vars  <- colnames(data.xwave[ , c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv")])
  data.xwave <- data.xwave[ , xwave_vars, with = F]
  data.table::setnames(data.xwave,
                       # old names
                       c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv"),
                       # new names
                       c("pidp","ethnicity","deceased","deceased_when"))

  ####### Combine - keep all observations in the main data and drop excess xwave observations
  data_merge <- merge(x = data_merge,
                      y = data.xwave,
                      by="pidp",
                      all.x=TRUE,
                      all.y=FALSE)

  youth_data <- rbind(youth_data, data_merge)


  #### Wave 4

  cat(crayon::red("\nWave 4 youth data..."))

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

  id_vars          <- Hmisc::Cs(pidp,pid,d_hidp,d_pno,d_psu,d_strata,d_intdaty_dv,d_intdatm_dv,d_intdatd_dv)
  demographic_vars <- Hmisc::Cs(d_dvage,d_ypsex,d_gor_dv,d_country,d_urban_dv)
  smoke_vars       <- Hmisc::Cs(d_ypevrsmo,d_ypsmofrq
                                # ,d_ypevresmo
                                )
  alc_vars         <- Hmisc::Cs(d_ypevralc)
  weight_vars      <- Hmisc::Cs(d_ythscub_xw)

  names <- c(id_vars, demographic_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data_merge <- data_merge[ , names, with = F]

  data.table::setnames(data_merge,

                       c("pidp","pid","d_hidp","d_pno","d_psu","d_strata","d_intdaty_dv","d_intdatm_dv","d_intdatd_dv",
                         ## demographic
                         "d_dvage","d_ypsex","d_gor_dv","d_country","d_urban_dv",
                         ## smoking variables
                         "d_ypevrsmo","d_ypsmofrq",
                         # "d_ypevresmo", #e smoke waves 7:12
                         ## alcohol variables
                         "d_ypevralc",
                         ## weights
                         "d_ythscub_xw"
                       ),
                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "age","sex","region","country","area",
                         # smoking variables
                         "ypevrsmo","ypsmofrq",
                         # "ypevresmo",
                         ## alcohol variables
                         "ypevralc",
                         ## weights
                         "weight_xw"
                       ))

  data_merge[, wave := "UKHLS Youth Wave 4"]
  data_merge[, wave_no := 4]
  data_merge[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data_merge[, dataset := "UKHLS"]
  data_merge[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  #########################################
  ######## ADD IN CROSS-WAVE DATA #########
  cat(crayon::red("\tCross-Wave..."))
  data.xwave <- data.table::fread(
    paste0(path, "/xwavedat.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.xwave, names(data.xwave), tolower(names(data.xwave)))
  xwave_vars  <- colnames(data.xwave[ , c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv")])
  data.xwave <- data.xwave[ , xwave_vars, with = F]
  data.table::setnames(data.xwave,
                       # old names
                       c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv"),
                       # new names
                       c("pidp","ethnicity","deceased","deceased_when"))

  ####### Combine - keep all observations in the main data and drop excess xwave observations
  data_merge <- merge(x = data_merge,
                      y = data.xwave,
                      by="pidp",
                      all.x=TRUE,
                      all.y=FALSE)

  youth_data <- rbind(youth_data, data_merge)


  #### Wave 5

  cat(crayon::red("\nWave 5 youth data..."))

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

  id_vars          <- Hmisc::Cs(pidp,pid,e_hidp,e_pno,e_psu,e_strata,e_intdaty_dv,e_intdatm_dv,e_intdatd_dv)
  demographic_vars <- Hmisc::Cs(e_dvage,e_ypsex,e_gor_dv,e_country,e_urban_dv)
  smoke_vars       <- Hmisc::Cs(e_ypevrsmo,e_ypsmofrq
                                # ,e_ypevresmo
                                )
  alc_vars         <- Hmisc::Cs(e_ypevralc)
  weight_vars      <- Hmisc::Cs(e_ythscub_xw)

  names <- c(id_vars, demographic_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data_merge <- data_merge[ , names, with = F]

  data.table::setnames(data_merge,

                       c("pidp","pid","e_hidp","e_pno","e_psu","e_strata","e_intdaty_dv","e_intdatm_dv","e_intdatd_dv",
                         ## demographic
                         "e_dvage","e_ypsex","e_gor_dv","e_country","e_urban_dv",
                         ## smoking variables
                         "e_ypevrsmo","e_ypsmofrq",
                         # "e_ypevresmo", #e smoke waves 7:12
                         ## alcohol variables
                         "e_ypevralc",
                         ## weights
                         "e_ythscub_xw"
                       ),
                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "age","sex","region","country","area",
                         ## smoking variables
                         "ypevrsmo","ypsmofrq",
                         # "ypevresmo",
                         ## alcohol variables
                         "ypevralc",
                         ## weights
                         "weight_xw"
                       ))

  data_merge[, wave := "UKHLS Youth Wave 5"]
  data_merge[, wave_no := 5]
  data_merge[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data_merge[, dataset := "UKHLS"]
  data_merge[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  #########################################
  ######## ADD IN CROSS-WAVE DATA #########
  cat(crayon::red("\tCross-Wave..."))
  data.xwave <- data.table::fread(
    paste0(path, "/xwavedat.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.xwave, names(data.xwave), tolower(names(data.xwave)))
  xwave_vars  <- colnames(data.xwave[ , c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv")])
  data.xwave <- data.xwave[ , xwave_vars, with = F]
  data.table::setnames(data.xwave,
                       # old names
                       c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv"),
                       # new names
                       c("pidp","ethnicity","deceased","deceased_when"))

  ####### Combine - keep all observations in the main data and drop excess xwave observations
  data_merge <- merge(x = data_merge,
                      y = data.xwave,
                      by="pidp",
                      all.x=TRUE,
                      all.y=FALSE)

  youth_data <- rbind(youth_data, data_merge)


  #### Wave 6

  cat(crayon::red("\nWave 6 youth data..."))

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

  id_vars          <- Hmisc::Cs(pidp,pid,f_hidp,f_pno,f_psu,f_strata,f_intdaty_dv,f_intdatm_dv,f_intdatd_dv)
  demographic_vars <- Hmisc::Cs(f_dvage,f_ypsex,f_gor_dv,f_country,f_urban_dv)
  smoke_vars       <- Hmisc::Cs(f_ypevrsmo,f_ypsmofrq
                                # ,f_ypevresmo
                                )
  alc_vars         <- Hmisc::Cs(f_ypevralc)
  weight_vars      <- Hmisc::Cs(f_ythscui_xw)

  names <- c(id_vars, demographic_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data_merge <- data_merge[ , names, with = F]

  data.table::setnames(data_merge,

                       c("pidp","pid","f_hidp","f_pno","f_psu","f_strata","f_intdaty_dv","f_intdatm_dv","f_intdatd_dv",
                         ## demographic
                         "f_dvage","f_ypsex","f_gor_dv","f_country","f_urban_dv",
                         ## smoking variables
                         "f_ypevrsmo","f_ypsmofrq",
                         # "f_ypevresmo", #e smoke waves 7:12
                         ## alcohol variables
                         "f_ypevralc",
                         ## weights
                         "f_ythscui_xw"
                       ),
                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "age","sex","region","country","area",
                         ## smoking variables
                         "ypevrsmo","ypsmofrq",
                         # "ypevresmo",
                         ## alcohol variables
                         "ypevralc",
                         ## weights
                         "weight_xw"
                       ))

  data_merge[, wave := "UKHLS Youth Wave 6"]
  data_merge[, wave_no := 6]
  data_merge[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data_merge[, dataset := "UKHLS"]
  data_merge[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  #########################################
  ######## ADD IN CROSS-WAVE DATA #########
  cat(crayon::red("\tCross-Wave..."))
  data.xwave <- data.table::fread(
    paste0(path, "/xwavedat.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.xwave, names(data.xwave), tolower(names(data.xwave)))
  xwave_vars  <- colnames(data.xwave[ , c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv")])
  data.xwave <- data.xwave[ , xwave_vars, with = F]
  data.table::setnames(data.xwave,
                       # old names
                       c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv"),
                       # new names
                       c("pidp","ethnicity","deceased","deceased_when"))

  ####### Combine - keep all observations in the main data and drop excess xwave observations
  data_merge <- merge(x = data_merge,
                      y = data.xwave,
                      by="pidp",
                      all.x=TRUE,
                      all.y=FALSE)

  youth_data <- rbind(youth_data, data_merge)


  #### Wave 7

  cat(crayon::red("\nWave 7 youth data..."))

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

  id_vars          <- Hmisc::Cs(pidp,pid,g_hidp,g_pno,g_psu,g_strata,g_intdaty_dv,g_intdatm_dv,g_intdatd_dv)
  demographic_vars <- Hmisc::Cs(g_dvage,g_ypsex,g_gor_dv,g_country,g_urban_dv)
  smoke_vars       <- Hmisc::Cs(g_ypevrsmo,g_ypsmofrq
                                ,g_ypevresmo
                                )
  alc_vars         <- Hmisc::Cs(g_ypevralc)
  weight_vars      <- Hmisc::Cs(g_ythscui_xw)

  names <- c(id_vars, demographic_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data_merge <- data_merge[ , names, with = F]

  data.table::setnames(data_merge,

                       c("pidp","pid","g_hidp","g_pno","g_psu","g_strata","g_intdaty_dv","g_intdatm_dv","g_intdatd_dv",
                         ## demographic
                         "g_dvage","g_ypsex","g_gor_dv","g_country","g_urban_dv",
                         ## smoking variables
                         "g_ypevrsmo","g_ypsmofrq",
                         "g_ypevresmo", #e smoke waves 7:12
                         ## alcohol variables
                         "g_ypevralc",
                         ## weights
                         "g_ythscui_xw"
                       ),
                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "age","sex","region","country","area",
                         ## smoking variables
                         "ypevrsmo","ypsmofrq",
                         "ypevresmo",
                         ## alcohol variables
                         "ypevralc",
                         ## weights
                         "weight_xw"
                       ))

  data_merge[, wave := "UKHLS Youth Wave 7"]
  data_merge[, wave_no := 7]
  data_merge[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data_merge[, dataset := "UKHLS"]
  data_merge[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  #########################################
  ######## ADD IN CROSS-WAVE DATA #########
  cat(crayon::red("\tCross-Wave..."))
  data.xwave <- data.table::fread(
    paste0(path, "/xwavedat.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.xwave, names(data.xwave), tolower(names(data.xwave)))
  xwave_vars  <- colnames(data.xwave[ , c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv")])
  data.xwave <- data.xwave[ , xwave_vars, with = F]
  data.table::setnames(data.xwave,
                       # old names
                       c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv"),
                       # new names
                       c("pidp","ethnicity","deceased","deceased_when"))

  ####### Combine - keep all observations in the main data and drop excess xwave observations
  data_merge <- merge(x = data_merge,
                      y = data.xwave,
                      by="pidp",
                      all.x=TRUE,
                      all.y=FALSE)

  youth_data <- rbind(youth_data, data_merge, fill = TRUE)


  #### Wave 8

  cat(crayon::red("\nWave 8 youth data..."))

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

  id_vars          <- Hmisc::Cs(pidp,pid,h_hidp,h_pno,h_psu,h_strata,h_intdaty_dv,h_intdatm_dv,h_intdatd_dv)
  demographic_vars <- Hmisc::Cs(h_dvage,h_ypsex,h_gor_dv,h_country,h_urban_dv)
  smoke_vars       <- Hmisc::Cs(h_ypevrsmo,h_ypsmofrq
                                ,h_ypevresmo
                                )
  alc_vars         <- Hmisc::Cs(h_ypevralc)
  weight_vars      <- Hmisc::Cs(h_ythscui_xw)

  names <- c(id_vars, demographic_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data_merge <- data_merge[ , names, with = F]

  data.table::setnames(data_merge,

                       c("pidp","pid","h_hidp","h_pno","h_psu","h_strata","h_intdaty_dv","h_intdatm_dv","h_intdatd_dv",
                         ## demographic
                         "h_dvage","h_ypsex","h_gor_dv","h_country","h_urban_dv",
                         ## smoking variables
                         "h_ypevrsmo","h_ypsmofrq",
                         "h_ypevresmo", #e smoke waves 7:12
                         ## alcohol variables
                         "h_ypevralc",
                         ## weights
                         "h_ythscui_xw"
                       ),
                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "age","sex","region","country","area",
                         ## smoking variables
                         "ypevrsmo","ypsmofrq",
                         "ypevresmo",
                         # alcohol variables
                         "ypevralc",
                         ## weights
                         "weight_xw"
                       ))

  data_merge[, wave := "UKHLS Youth Wave 8"]
  data_merge[, wave_no := 8]
  data_merge[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data_merge[, dataset := "UKHLS"]
  data_merge[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  #########################################
  ######## ADD IN CROSS-WAVE DATA #########
  cat(crayon::red("\tCross-Wave..."))
  data.xwave <- data.table::fread(
    paste0(path, "/xwavedat.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.xwave, names(data.xwave), tolower(names(data.xwave)))
  xwave_vars  <- colnames(data.xwave[ , c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv")])
  data.xwave <- data.xwave[ , xwave_vars, with = F]
  data.table::setnames(data.xwave,
                       # old names
                       c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv"),
                       # new names
                       c("pidp","ethnicity","deceased","deceased_when"))

  ####### Combine - keep all observations in the main data and drop excess xwave observations
  data_merge <- merge(x = data_merge,
                      y = data.xwave,
                      by="pidp",
                      all.x=TRUE,
                      all.y=FALSE)

  youth_data <- rbind(youth_data, data_merge)


  #### Wave 9

  cat(crayon::red("\nWave 9 youth data..."))

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

  id_vars          <- Hmisc::Cs(pidp,pid,i_hidp,i_pno,i_psu,i_strata,i_intdaty_dv,i_intdatm_dv,i_intdatd_dv)
  demographic_vars <- Hmisc::Cs(i_dvage,i_ypsex,i_gor_dv,i_country,i_urban_dv)
  smoke_vars       <- Hmisc::Cs(i_ypevrsmo,i_ypsmofrq
                                ,i_ypevresmo
                                )
  alc_vars         <- Hmisc::Cs(i_ypevralc)
  weight_vars      <- Hmisc::Cs(i_ythscui_xw)

  names <- c(id_vars, demographic_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data_merge <- data_merge[ , names, with = F]

  data.table::setnames(data_merge,

                       c("pidp","pid","i_hidp","i_pno","i_psu","i_strata","i_intdaty_dv","i_intdatm_dv","i_intdatd_dv",
                         ## demographic
                         "i_dvage","i_ypsex","i_gor_dv","i_country","i_urban_dv",
                         ## smoking variables
                         "i_ypevrsmo","i_ypsmofrq",
                         "i_ypevresmo", #e smoke waves 7:12
                         ## alcohol variables
                         "i_ypevralc",
                         ## weights
                         "i_ythscui_xw"
                       ),
                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "age","sex","region","country","area",
                         ## smoking variables
                         "ypevrsmo","ypsmofrq",
                         "ypevresmo",
                         ## alcohol variables
                         "ypevralc",
                         ## weights
                         "weight_xw"
                       ))

  data_merge[, wave := "UKHLS Youth Wave 9"]
  data_merge[, wave_no := 9]
  data_merge[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data_merge[, dataset := "UKHLS"]
  data_merge[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  #########################################
  ######## ADD IN CROSS-WAVE DATA #########
  cat(crayon::red("\tCross-Wave..."))
  data.xwave <- data.table::fread(
    paste0(path, "/xwavedat.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.xwave, names(data.xwave), tolower(names(data.xwave)))
  xwave_vars  <- colnames(data.xwave[ , c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv")])
  data.xwave <- data.xwave[ , xwave_vars, with = F]
  data.table::setnames(data.xwave,
                       # old names
                       c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv"),
                       # new names
                       c("pidp","ethnicity","deceased","deceased_when"))

  ####### Combine - keep all observations in the main data and drop excess xwave observations
  data_merge <- merge(x = data_merge,
                      y = data.xwave,
                      by="pidp",
                      all.x=TRUE,
                      all.y=FALSE)

  youth_data <- rbind(youth_data, data_merge)


  #### Wave 10

  cat(crayon::red("\nWave 10 youth data..."))

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

  id_vars          <- Hmisc::Cs(pidp,pid,j_hidp,j_pno,j_psu,j_strata,j_intdaty_dv,j_intdatm_dv,j_intdatd_dv)
  demographic_vars <- Hmisc::Cs(j_dvage,j_ypsex,j_gor_dv,j_country,j_urban_dv)
  smoke_vars       <- Hmisc::Cs(j_ypevrsmo,j_ypsmofrq
                                ,j_ypevresmo
                                )
  alc_vars         <- Hmisc::Cs(j_ypevralc)
  weight_vars      <- Hmisc::Cs(j_ythscui_xw)

  names <- c(id_vars, demographic_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data_merge <- data_merge[ , names, with = F]

  data.table::setnames(data_merge,

                       c("pidp","pid","j_hidp","j_pno","j_psu","j_strata","j_intdaty_dv","j_intdatm_dv","j_intdatd_dv",
                         ## demographic
                         "j_dvage","j_ypsex","j_gor_dv","j_country","j_urban_dv",
                         ## smoking variables
                         "j_ypevrsmo","j_ypsmofrq",
                         "j_ypevresmo", #e smoke waves 7:12
                         ## alcohol variables
                         "j_ypevralc",
                         ## weights
                         "j_ythscui_xw"
                       ),
                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "age","sex","region","country","area",
                         ## smoking variables
                         "ypevrsmo","ypsmofrq",
                         "ypevresmo",
                         ## alcohol variables
                         "ypevralc",
                         ## weights
                         "weight_xw"
                       ))

  data_merge[, wave := "UKHLS Youth Wave 10"]
  data_merge[, wave_no := 10]
  data_merge[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data_merge[, dataset := "UKHLS"]
  data_merge[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  #########################################
  ######## ADD IN CROSS-WAVE DATA #########
  cat(crayon::red("\tCross-Wave..."))
  data.xwave <- data.table::fread(
    paste0(path, "/xwavedat.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.xwave, names(data.xwave), tolower(names(data.xwave)))
  xwave_vars  <- colnames(data.xwave[ , c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv")])
  data.xwave <- data.xwave[ , xwave_vars, with = F]
  data.table::setnames(data.xwave,
                       # old names
                       c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv"),
                       # new names
                       c("pidp","ethnicity","deceased","deceased_when"))

  ####### Combine - keep all observations in the main data and drop excess xwave observations
  data_merge <- merge(x = data_merge,
                      y = data.xwave,
                      by="pidp",
                      all.x=TRUE,
                      all.y=FALSE)

  youth_data <- rbind(youth_data, data_merge)


  #### Wave 11

  cat(crayon::red("\nWave 11 youth data..."))

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

  id_vars          <- Hmisc::Cs(pidp,pid,k_hidp,k_pno,k_psu,k_strata,k_intdaty_dv,k_intdatm_dv,k_intdatd_dv)
  demographic_vars <- Hmisc::Cs(k_dvage,k_ypsex,k_gor_dv,k_country,k_urban_dv)
  smoke_vars       <- Hmisc::Cs(k_ypevrsmo,k_ypsmofrq
                                ,k_ypevresmo
                                )
  alc_vars         <- Hmisc::Cs(k_ypevralc)
  weight_vars      <- Hmisc::Cs(k_ythscui_xw)

  names <- c(id_vars, demographic_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data_merge <- data_merge[ , names, with = F]

  data.table::setnames(data_merge,

                       c("pidp","pid","k_hidp","k_pno","k_psu","k_strata","k_intdaty_dv","k_intdatm_dv","k_intdatd_dv",
                         ## demographic
                         "k_dvage","k_ypsex","k_gor_dv","k_country","k_urban_dv",
                         ## smoking variables
                         "k_ypevrsmo","k_ypsmofrq",
                         "k_ypevresmo", #e smoke waves 7:12
                         ## alcohol variables
                         "k_ypevralc",
                         ## weights
                         "k_ythscui_xw"
                       ),
                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "age","sex","region","country","area",
                         ## smoking variables
                         "ypevrsmo","ypsmofrq",
                         "ypevresmo",
                         ## alcohol variables
                         "ypevralc",
                         ## weights
                         "weight_xw"
                       ))

  data_merge[, wave := "UKHLS Youth Wave 11"]
  data_merge[, wave_no := 11]
  data_merge[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data_merge[, dataset := "UKHLS"]
  data_merge[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  #########################################
  ######## ADD IN CROSS-WAVE DATA #########
  cat(crayon::red("\tCross-Wave..."))
  data.xwave <- data.table::fread(
    paste0(path, "/xwavedat.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.xwave, names(data.xwave), tolower(names(data.xwave)))
  xwave_vars  <- colnames(data.xwave[ , c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv")])
  data.xwave <- data.xwave[ , xwave_vars, with = F]
  data.table::setnames(data.xwave,
                       # old names
                       c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv"),
                       # new names
                       c("pidp","ethnicity","deceased","deceased_when"))

  ####### Combine - keep all observations in the main data and drop excess xwave observations
  data_merge <- merge(x = data_merge,
                      y = data.xwave,
                      by="pidp",
                      all.x=TRUE,
                      all.y=FALSE)

  youth_data <- rbind(youth_data, data_merge)


  #### Wave 12

  cat(crayon::red("\nWave 12 youth data..."))

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

  id_vars          <- Hmisc::Cs(pidp,pid,l_hidp,l_pno,l_psu,l_strata,l_intdaty_dv,l_intdatm_dv,l_intdatd_dv)
  demographic_vars <- Hmisc::Cs(l_dvage,l_ypsex,l_gor_dv,l_country,l_urban_dv)
  smoke_vars       <- Hmisc::Cs(l_ypevrsmo,l_ypsmofrq
                                ,l_ypevresmo
                                )
  alc_vars         <- Hmisc::Cs(l_ypevralc)
  weight_vars      <- Hmisc::Cs(l_ythscui_xw)

  names <- c(id_vars, demographic_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data_merge <- data_merge[ , names, with = F]

  data.table::setnames(data_merge,

                       c("pidp","pid","l_hidp","l_pno","l_psu","l_strata","l_intdaty_dv","l_intdatm_dv","l_intdatd_dv",
                         ## demographic
                         "l_dvage","l_ypsex","l_gor_dv","l_country","l_urban_dv",
                         ## smoking variables
                         "l_ypevrsmo","l_ypsmofrq",
                         "l_ypevresmo", #e smoke waves 7:12
                         ## alcohol variables
                         "l_ypevralc",
                         ## weights
                         "l_ythscui_xw"
                       ),
                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "age","sex","region","country","area",
                         ## smoking variables
                         "ypevrsmo","ypsmofrq",
                         "ypevresmo",
                         ## alcohol variables
                         "ypevralc",
                         ## weights
                         "weight_xw"
                       ))

  data_merge[, wave := "UKHLS Youth Wave 12"]
  data_merge[, wave_no := 12]
  data_merge[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data_merge[, dataset := "UKHLS"]
  data_merge[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  #########################################
  ######## ADD IN CROSS-WAVE DATA #########
  cat(crayon::red("\tCross-Wave..."))
  data.xwave <- data.table::fread(
    paste0(path, "/xwavedat.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.xwave, names(data.xwave), tolower(names(data.xwave)))
  xwave_vars  <- colnames(data.xwave[ , c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv")])
  data.xwave <- data.xwave[ , xwave_vars, with = F]
  data.table::setnames(data.xwave,
                       # old names
                       c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv"),
                       # new names
                       c("pidp","ethnicity","deceased","deceased_when"))

  ####### Combine - keep all observations in the main data and drop excess xwave observations
  data_merge <- merge(x = data_merge,
                      y = data.xwave,
                      by="pidp",
                      all.x=TRUE,
                      all.y=FALSE)

  youth_data <- rbind(youth_data, data_merge)


  return(youth_data)
}
