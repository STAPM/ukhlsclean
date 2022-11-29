#' Read Understanding Society 2020
#'
#' Reads and cleans the Understanding Society calendar year data for 2020. These data are derived from the main
#' Understanding Society survey but designed to be a representative cross-section for the year 2020. For this data,
#' the function applies the full reading and cleaning proces.
#'
#' UKDS Study Number: \href{https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=8988}{SN8988 Understanding Society: Calendar Year Dataset, 2020}
#'
#' @source University of Essex, Institute for Social and Economic Research. (2022).
#' Understanding Society: Calendar Year Dataset, 2020. [data collection]. UK Data Service. SN: 8988, \href{https://doi.org/10.5255/UKDA-SN-8988-1}{DOI: 10.5255/UKDA-SN-8988-1}
#'
#'
#' @param root Character - the root directory.
#' @param file Character - the file path and name.
#' @param full Logical - TRUE if restricting the sample to full interviews only (excluding proxies)
#' @param ages Integer vector - the ages in single years to retain (defaults to 16 to 89 years).
#' @param keep_vars Character vector - the names of the variables to keep (defaults to NULL - retaining all variables).
#' @param complete_vars Character vector - the names of the variables on which the selection of complete cases will be based (defaults to year, age and sex).
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
ukhlsclean_2020 <- function(
    root = c("X:/"),
    file = "HAR_PR/PR/USoc/Data/Calendar Year Datasets/SN8988_2022_11_29/tab",
    full = TRUE,
    ages = 16:89,
    keep_vars = NULL,
    complete_vars = c("d_age","d_sex")
) {

  cat(crayon::magenta("\tReading UKHLS Calendar Year 2020 datasets"))

  cat(crayon::red("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/jkl_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[jkl_ivfio==1,]
  }


  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, jkl_hidp, jkl_pno, jkl_psu, jkl_strata, jkl_istrtdaty, jkl_istrtdatm, jkl_istrtdatd)
  demographic_vars <- Hmisc::Cs(jkl_sex, jkl_dvage, jkl_birthy, jkl_gor_dv, jkl_urban_dv, jkl_mlstat, jkl_marstat)
  econ_stat_vars   <- Hmisc::Cs(jkl_jbstat, jkl_jbhas, jkl_jboff, jkl_jboffy)
  work_vars        <- Hmisc::Cs(jkl_paygu_dv, jkl_payg_dv, jkl_jbhrs, jkl_fimnlabgrs_dv, jkl_seearngrs_dv, jkl_jbsic07_cc)
  education_vars   <- Hmisc::Cs(jkl_hiqual_dv)
  health_vars      <- Hmisc::Cs(jkl_health, jkl_aidhh, jkl_sclfsat1, jkl_sclfsato, jkl_sf12pcs_dv, jkl_sf12mcs_dv,
                                jkl_scsf1, jkl_scsf2a, jkl_scsf2b, jkl_scsf3a, jkl_scsf3b, jkl_scsf4a, jkl_scsf4b, jkl_scsf5, jkl_scsf6a,
                                jkl_scsf6b, jkl_scsf6c, jkl_scsf7)
  preg_vars        <- Hmisc::Cs(jkl_pregout1, jkl_pregout2)
  smoke_vars       <- Hmisc::Cs(jkl_smoker, jkl_ncigs)
  weight_vars      <- Hmisc::Cs(jkl_indinui_xw)

  s2020_vars       <- Hmisc::Cs(jkl_ethn_dv)

  names <- c(id_vars, demographic_vars, econ_stat_vars, work_vars, education_vars,
             health_vars, preg_vars, smoke_vars, weight_vars, s2020_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","jkl_hidp","jkl_pno","jkl_psu","jkl_strata","jkl_istrtdaty","jkl_istrtdatm","jkl_istrtdatd",
                         ## demographic
                         "jkl_sex","jkl_dvage","jkl_birthy","jkl_gor_dv","jkl_urban_dv","jkl_mlstat","jkl_marstat",
                         ## economic status
                         "jkl_jbstat","jkl_jbhas","jkl_jboff","jkl_jboffy",
                         ## work variables
                         "jkl_paygu_dv","jkl_payg_dv","jkl_jbhrs","jkl_fimnlabgrs_dv","jkl_seearngrs_dv","jkl_jbsic07_cc",
                         ## education variables
                         "jkl_hiqual_dv",
                         ## health variables
                         "jkl_health","jkl_aidhh","jkl_sclfsat1","jkl_sclfsato","jkl_sf12pcs_dv","jkl_sf12mcs_dv",
                         "jkl_scsf1","jkl_scsf2a","jkl_scsf2b","jkl_scsf3a","jkl_scsf3b","jkl_scsf4a","jkl_scsf4b","jkl_scsf5","jkl_scsf6a","jkl_scsf6b","jkl_scsf6c","jkl_scsf7",
                         ## pregnancy variables
                         "jkl_pregout1","jkl_pregout2",
                         ## smoke variables
                         "jkl_smoker", "jkl_ncigs",
                         ## weight
                         "jkl_indinui_xw",
                         ## 2020 specific variables
                         "jkl_ethn_dv"),


                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "sex","age","birth_year","region","urban","mlstat","marstat",
                         ## economic status
                         "econ_stat","jbhas","jboff","jboffy",
                         ## work variables
                         "grss_pay_usual","grss_pay_last","hours","grss_lab_inc","grss_semp","sic07",
                         ## education variables
                         "highest_qual",
                         ## health variables
                         "lt_sick","caring","health_satisf","life_satisf","sf12_pcs","sf12_mcs",
                         "sf1","sf2a","sf2b","sf3a","sf3b","sf4a","sf4b","sf5","sf6a","sf6b","sf6c","sf7",
                         ## pregnancy variables
                         "pregout1","pregout2",
                         ## smoke variables
                         "smoker", "ncigs",
                         ## weight
                         "weight_xw",
                         ## 2020 specific variables
                         "ethn_dv"))

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::red("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/jkl_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(jkl_hidp, jkl_tenure_dv, jkl_nkids_dv, jkl_hhsize, jkl_hhtype_dv,
                                   jkl_nch02_dv, jkl_nch34_dv, jkl_nch511_dv, jkl_nch1215_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("jkl_hidp", "jkl_tenure_dv", "jkl_nkids_dv", "jkl_hhsize","jkl_hhtype_dv",
                         "jkl_nch02_dv", "jkl_nch34_dv", "jkl_nch511_dv", "jkl_nch1215_dv"),
                       # new names
                       c("hidp", "hh_tenure", "hh_numchild", "hh_size", "hh_type",
                         "hh_numchild02", "hh_numchild34", "hh_numchild511", "hh_numchild1215"))

  data_merged <- merge(x = data,
                       y = data.hhold,
                       by="hidp",
                       all.x=TRUE,
                       all.y=FALSE)

  rm(data, data.hhold); gc()

  cat(crayon::magenta("\tdone\n"))

  ####################################
  ### Apply all cleaning functions ###

  data_merged[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data_merged[, wave_no := 2020]
  data_merged[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data_merged[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  cleaned <- ukhls_clean_global(data = data_merged,
                                ages = ages,
                                keep_vars = keep_vars,
                                complete_vars = complete_vars)
  return(cleaned)
}
