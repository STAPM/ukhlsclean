#' Read Understanding Society 2021
#'
#' Reads and cleans the Understanding Society calendar year data for 2021. These data are derived from the main
#' Understanding Society survey but designed to be a representative cross-section for the year 2021. For this data,
#' the function applies the full reading and cleaning process.
#'
#' UKDS Study Number: \href{https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=8988}{SN8988 Understanding Society: Calendar Year Dataset, 2020}
#'
#' @source University of Essex, Institute for Social and Economic Research. (2024).
#' Understanding Society: Calendar Year Dataset, 2021 [data collection]. UK Data Service. SN: 9193, \href{https://doi.org/10.5255/UKDA-SN-9193-1}{DOI: 10.5255/UKDA-SN-9193-1}
#'
#'
#' @param root Character - the root directory.
#' @param file Character - the file path and name.
#' @param full Logical - TRUE if restricting the sample to full interviews only (excluding proxies)
#' @param ages Integer vector - the ages in single years to retain (defaults to 16 to 89 years).
#' @param country Character - country to produce data for. One of c("UK","england","wales","scotland","northern_ireland"). Defaults to all UK.
#' @param keep_vars Character vector - the names of the variables to keep (defaults to NULL - retaining all variables).
#' @param complete_vars Character vector - the names of the variables on which the selection of complete cases will be based (defaults to year, age and sex).
#' @param inflation_index Character - one of c("cpih","rpi"). Default option is CPIH.
#'
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
ukhlsclean_2021 <- function(
    root = c("X:/"),
    file = "HAR_PR/PR/USoc/Data/Calendar Year Datasets/SN9193_2024_01_22/tab",
    full = TRUE,
    ages = 16:89,
    country = "UK",
    keep_vars = NULL,
    complete_vars = NULL,
    inflation_index = "cpih"
) {

  cat(crayon::blue(crayon::underline("\tReading UKHLS Calendar Year 2021 datasets")))

  cat(crayon::green("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/klm_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[klm_ivfio==1,]
  }


  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, klm_hidp, klm_pno, klm_psu, klm_strata, klm_istrtdaty, klm_istrtdatm, klm_istrtdatd)
  demographic_vars <- Hmisc::Cs(klm_sex, klm_dvage, klm_birthy, klm_gor_dv, klm_urban_dv, klm_mlstat, klm_marstat)
  econ_stat_vars   <- Hmisc::Cs(klm_jbstat, klm_jbhas, klm_jboff, klm_jboffy)
  work_vars        <- Hmisc::Cs(klm_paygu_dv, klm_payg_dv, klm_jbhrs, klm_fimnlabgrs_dv, klm_seearngrs_dv, klm_jbsic07_cc)
  education_vars   <- Hmisc::Cs(klm_hiqual_dv)
  health_vars      <- Hmisc::Cs(klm_health, klm_aidhh, klm_sclfsat1, klm_sclfsato, klm_sf12pcs_dv, klm_sf12mcs_dv,
                                klm_scsf1, klm_scsf2a, klm_scsf2b, klm_scsf3a, klm_scsf3b, klm_scsf4a, klm_scsf4b, klm_scsf5, klm_scsf6a,
                                klm_scsf6b, klm_scsf6c, klm_scsf7)
  preg_vars        <- Hmisc::Cs(klm_pregout1, klm_pregout2)
  smoke_vars       <- Hmisc::Cs(klm_smoker, klm_ncigs)
  benefits_vars    <- Hmisc::Cs(klm_benbase1, klm_benbase2, klm_benbase3, klm_benbase4, klm_benbase96,
                                klm_benctc)
  pension_vars     <- Hmisc::Cs(klm_benpen1, klm_benpen2, klm_benpen3, klm_benpen4, klm_benpen5, klm_benpen6, klm_benpen7, klm_benpen8, klm_benpen96)
  bendis_vars      <- Hmisc::Cs(klm_bendis1, klm_bendis2, klm_bendis3, klm_bendis4, klm_bendis5, klm_bendis12,
                                klm_bendis7, klm_bendis8, klm_bendis10, klm_bendis97, klm_bendis96)
  otherben_vars    <- Hmisc::Cs(klm_benesa,
                                klm_othben1, klm_othben2,                           klm_othben5, klm_othben6, klm_othben7, klm_othben8, klm_othben9, klm_othben97, klm_othben96)
  benincome_vars   <- Hmisc::Cs(klm_bensta2, klm_bensta3, klm_bensta4, klm_bensta5, klm_bensta6, klm_bensta7, klm_bensta97, klm_bensta96)
  weight_vars      <- Hmisc::Cs(klm_indinui_xw)

  s2020_vars       <- Hmisc::Cs(klm_ethn_dv)

  names <- c(id_vars, demographic_vars, econ_stat_vars, work_vars, education_vars,
             health_vars, preg_vars, smoke_vars,
             benefits_vars, pension_vars, bendis_vars, otherben_vars, benincome_vars,
             weight_vars, s2020_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","klm_hidp","klm_pno","klm_psu","klm_strata","klm_istrtdaty","klm_istrtdatm","klm_istrtdatd",
                         ## demographic
                         "klm_sex","klm_dvage","klm_birthy","klm_gor_dv","klm_urban_dv","klm_mlstat","klm_marstat",
                         ## economic status
                         "klm_jbstat","klm_jbhas","klm_jboff","klm_jboffy",
                         ## work variables
                         "klm_paygu_dv","klm_payg_dv","klm_jbhrs","klm_fimnlabgrs_dv","klm_seearngrs_dv","klm_jbsic07_cc",
                         ## education variables
                         "klm_hiqual_dv",
                         ## health variables
                         "klm_health","klm_aidhh","klm_sclfsat1","klm_sclfsato","klm_sf12pcs_dv","klm_sf12mcs_dv",
                         "klm_scsf1","klm_scsf2a","klm_scsf2b","klm_scsf3a","klm_scsf3b","klm_scsf4a","klm_scsf4b","klm_scsf5","klm_scsf6a","klm_scsf6b","klm_scsf6c","klm_scsf7",
                         ## pregnancy variables
                         "klm_pregout1","klm_pregout2",
                         ## smoke variables
                         "klm_smoker", "klm_ncigs",
                         ## benefits
                         "klm_benbase1","klm_benbase2","klm_benbase3","klm_benbase4","klm_benbase96",
                         "klm_benctc",
                         ## pensions
                         "klm_benpen1","klm_benpen2","klm_benpen3","klm_benpen4","klm_benpen5","klm_benpen6","klm_benpen7","klm_benpen8","klm_benpen96",
                         ## disability benefits
                         "klm_bendis1","klm_bendis2","klm_bendis3","klm_bendis4","klm_bendis5","klm_bendis12",
                         "klm_bendis7","klm_bendis8","klm_bendis10","klm_bendis97","klm_bendis96",
                         ## other benefits
                         "klm_benesa",
                         "klm_othben1","klm_othben2"                            ,"klm_othben5","klm_othben6","klm_othben7","klm_othben8","klm_othben9","klm_othben97","klm_othben96",
                         ## benefit income variables (formerly receivables)
                         "klm_bensta2","klm_bensta3","klm_bensta4","klm_bensta5","klm_bensta6","klm_bensta7","klm_bensta97","klm_bensta96",
                         ## weight
                         "klm_indinui_xw",
                         ## 2020 specific variables
                         "klm_ethn_dv"),


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
    paste0(path, "/klm_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(klm_hidp, klm_tenure_dv, klm_nkids_dv, klm_hhsize, klm_hhtype_dv,
                                   klm_nch02_dv, klm_nch34_dv, klm_nch511_dv, klm_nch1215_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("klm_hidp", "klm_tenure_dv", "klm_nkids_dv", "klm_hhsize","klm_hhtype_dv",
                         "klm_nch02_dv", "klm_nch34_dv", "klm_nch511_dv", "klm_nch1215_dv"),
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
    paste0(path, "/klm_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indall_vars  <- colnames(data.indall[ , c("pidp","klm_hidp")])

  data.indall <- data.indall[ , indall_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","klm_hidp"),
                       # new names
                       c("pidp","hidp"))

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
  data_merged[, wave_no := NA]
  data_merged[, wave := " "]
  data_merged[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data_merged[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  ## identify if running the calendar year code
  calendar_year <- TRUE

  ## drop small number (643) of 2021 observations
  data_merged <- data_merged[year == 2021,]

  if (inflation_index == "cpih"){
    inflation <- ukhlsclean::cpih
  }
  if (inflation_index == "rpi"){
    inflation <- ukhlsclean::rpi
  }

  cleaned <- ukhls_clean_global(data = data_merged,
                                ages = ages,
                                country = country,
                                keep_vars = keep_vars,
                                complete_vars = complete_vars,
                                calendar_year = calendar_year)
  return(cleaned)
}
