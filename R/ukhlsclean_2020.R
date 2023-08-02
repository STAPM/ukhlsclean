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
#' @param country Character - country to produce data for. One of c("UK","england","wales","scotland","northern_ireland"). Defaults to all UK.
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
    country = "UK",
    keep_vars = NULL,
    complete_vars = c("d_age","d_sex","d_country")
) {

  cat(crayon::blue(crayon::underline("\tReading UKHLS Calendar Year 2020 datasets")))

  cat(crayon::green("\tIndividual..."))

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
  benefits_vars    <- Hmisc::Cs(jkl_benbase1, jkl_benbase2, jkl_benbase3, jkl_benbase4, jkl_benbase96,
                                jkl_benctc)
  pension_vars     <- Hmisc::Cs(jkl_benpen1, jkl_benpen2, jkl_benpen3, jkl_benpen4, jkl_benpen5, jkl_benpen6, jkl_benpen7, jkl_benpen8, jkl_benpen96)
  bendis_vars      <- Hmisc::Cs(jkl_bendis1, jkl_bendis2, jkl_bendis3, jkl_bendis4, jkl_bendis5, jkl_bendis12,
                                jkl_bendis7, jkl_bendis8, jkl_bendis10, jkl_bendis97, jkl_bendis96)
  otherben_vars    <- Hmisc::Cs(jkl_benesa,
                                jkl_othben1, jkl_othben2,                           jkl_othben5, jkl_othben6, jkl_othben7, jkl_othben8, jkl_othben9, jkl_othben97, jkl_othben96)
  benincome_vars   <- Hmisc::Cs(jkl_bensta2, jkl_bensta3, jkl_bensta4, jkl_bensta5, jkl_bensta6, jkl_bensta7, jkl_bensta97, jkl_bensta96)
  weight_vars      <- Hmisc::Cs(jkl_indinui_xw)

  s2020_vars       <- Hmisc::Cs(jkl_ethn_dv)

  names <- c(id_vars, demographic_vars, econ_stat_vars, work_vars, education_vars,
             health_vars, preg_vars, smoke_vars,
             benefits_vars, pension_vars, bendis_vars, otherben_vars, benincome_vars,
             weight_vars, s2020_vars)
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
                         ## benefits
                         "jkl_benbase1","jkl_benbase2","jkl_benbase3","jkl_benbase4","jkl_benbase96",
                         "jkl_benctc",
                         ## pensions
                         "jkl_benpen1","jkl_benpen2","jkl_benpen3","jkl_benpen4","jkl_benpen5","jkl_benpen6","jkl_benpen7","jkl_benpen8","jkl_benpen96",
                         ## disability benefits
                         "jkl_bendis1","jkl_bendis2","jkl_bendis3","jkl_bendis4","jkl_bendis5","jkl_bendis12",
                         "jkl_bendis7","jkl_bendis8","jkl_bendis10","jkl_bendis97","jkl_bendis96",
                         ## other benefits
                         "jkl_benesa",
                         "jkl_othben1","jkl_othben2"                            ,"jkl_othben5","jkl_othben6","jkl_othben7","jkl_othben8","jkl_othben9","jkl_othben97","jkl_othben96",
                         ## benefit income variables (formerly receivables)
                         "jkl_bensta2","jkl_bensta3","jkl_bensta4","jkl_bensta5","jkl_bensta6","jkl_bensta7","jkl_bensta97","jkl_bensta96",
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
                         ## benefits
                         "benbase1","benbase2","benbase3","benbase4","benbase96",
                         "benctc",
                         ## pensions
                         "NI.state_pen","employer_pen","spouse.emp_pen","pencred_pen","prvt_pen","widow_pen","parent_pen","benpen8","non_benpen",
                         ## disability benefits
                         "bendis1","bendis2","bendis3","bendis4","bendis5","bendis12",
                         "bendis7","bendis8","bendis10","bendis97","bendis96",
                         ## other benefits
                         "benesa","othben1","othben2","othben5","othben6",
                         "othben7","othben8","othben9","othben97","othben96",
                         ## benefit income variables (formerly receivables)
                         "bensta_edugrant","bensta_tupay","bensta_alimony","bensta_fampay","bensta_rentlodge","bensta_rentother","bensta_other","non_bensta",
                         ## weight
                         "weight_xw",
                         ## 2020 specific variables
                         "ethn_dv"))

  #data[, income_serps := NA]
  #data[, othben3 := NA]
  #data[, othben4 := NA]

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::green("\tHousehold..."))

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

  hhold_merged <- merge(x = data,
                        y = data.hhold,
                        by = "hidp",
                        all.x = TRUE,
                        all.y = FALSE)

  ####################################################
  #### ADD IN THE INDALL DATA ########################

  cat(crayon::green("\tIndall..."))

  data.indall <- data.table::fread(
    paste0(path, "/jkl_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indall_vars  <- colnames(data.indall[ , c("pidp","jkl_hidp",
                                            "jkl_imd2019qe_dv","jkl_imd2017qni_dv",
                                            "jkl_imd2020qs_dv","jkl_imd2019qw_dv")])

  data.indall <- data.indall[ , indall_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","jkl_hidp",
                         "jkl_imd2019qe_dv","jkl_imd2017qni_dv",
                         "jkl_imd2020qs_dv","jkl_imd2019qw_dv"),
                       # new names
                       c("pidp","hidp",
                         "imdq_e","imdq_ni",
                         "imdq_s","imdq_w"))

  ## Combine - keep all observations in the main data and drop excess xwave observations

  data_merged <- merge(x = hhold_merged,
                       y = data.indall,
                       by = c("pidp","hidp"),
                       all.x = TRUE,
                       all.y = FALSE)

  ##########################################################################



  rm(data, data.hhold); gc()

  cat(crayon::blue(crayon::bold("\tdone\n")))

  ####################################
  ### Apply all cleaning functions ###

  data_merged[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data_merged[, wave_no := 12]
  data_merged[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data_merged[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  ## identify if running the calendar year code
  calendar_year <- TRUE

  ## drop small number (643) of 2021 observations
  data_merged <- data_merged[year == 2020,]


  cleaned <- ukhls_clean_global(data = data_merged,
                                ages = ages,
                                country = country,
                                keep_vars = keep_vars,
                                complete_vars = complete_vars,
                                calendar_year = calendar_year)
  return(cleaned)
}
