#' Read Understanding Society 2022
#'
#' Reads and cleans the Understanding Society calendar year data for 2022. These data are derived from the main
#' Understanding Society survey but designed to be a representative cross-section for the year 2022. For this data,
#' the function applies the full reading and cleaning process.
#'
#' UKDS Study Number: \href{https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=9333}{SN9333 Understanding Society: Calendar Year Dataset, 2020}
#'
#' @source University of Essex, Institute for Social and Economic Research. (2024).
#' Understanding Society: Calendar Year Dataset, 2024 [data collection]. UK Data Service. SN: 9333, \href{https://doi.org/10.5255/UKDA-SN-9333-1}{DOI: 10.5255/UKDA-SN-9333-1}
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
ukhlsclean_2022 <- function(
    root = c("X:/"),
    file = "HAR_PR/PR/USoc/Data/Calendar Year Datasets/SN9333_2025_06_24/tab",
    #root = c("C:/"),
    #file = "Users/damon/OneDrive/Documents/USoc/Datasets/Calendar year data/2022/tab",
    full = FALSE,
    ages = 16:89,
    country = "UK",
    keep_vars = NULL,
    complete_vars = NULL,
    inflation_index = "cpih"
) {

  cat(crayon::blue(crayon::underline("\tReading UKHLS Calendar Year 2022 datasets")))

  cat(crayon::green("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/lmn_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[lmn_ivfio==1,]
  }


  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, lmn_hidp, lmn_pno, lmn_psu, lmn_strata, lmn_istrtdaty, lmn_istrtdatm, lmn_istrtdatd)
  demographic_vars <- Hmisc::Cs(lmn_sex, lmn_dvage, lmn_birthy, lmn_gor_dv, lmn_urban_dv, lmn_mlstat, lmn_marstat)
  econ_stat_vars   <- Hmisc::Cs(lmn_jbstat, lmn_jbhas, lmn_jboff, lmn_jboffy)
  work_vars        <- Hmisc::Cs(lmn_paygu_dv, lmn_payg_dv, lmn_jbhrs, lmn_fimnlabgrs_dv, lmn_seearngrs_dv, lmn_jbsic07_cc)
  education_vars   <- Hmisc::Cs(lmn_hiqual_dv)
  health_vars      <- Hmisc::Cs(lmn_health, lmn_aidhh, lmn_sclfsat1, lmn_sclfsato, lmn_sf12pcs_dv, lmn_sf12mcs_dv,
                                lmn_scsf1, lmn_scsf2a, lmn_scsf2b, lmn_scsf3a, lmn_scsf3b, lmn_scsf4a, lmn_scsf4b, lmn_scsf5, lmn_scsf6a,
                                lmn_scsf6b, lmn_scsf6c, lmn_scsf7)
  health_cond_vars <- Hmisc::Cs(lmn_hconds01, lmn_hconds03, lmn_hconds04, lmn_hconds05, lmn_hconds08,
                                              lmn_hconds11, lmn_hconds12, lmn_hconds15, lmn_hconds16,
                                lmn_hconds21,                                           lmn_hconds26, lmn_hconds27, lmn_hconds28, lmn_hconds29,
                                lmn_hconds30, lmn_hconds31, lmn_hconds32, lmn_hconds33, lmn_hconds34, lmn_hconds35,

                                lmn_hcondns1, lmn_hcondns3, lmn_hcondns4, lmn_hcondns5, lmn_hcondns6, lmn_hcondns7, lmn_hcondns8, lmn_hcondns10)
  preg_vars        <- Hmisc::Cs(lmn_pregout1, lmn_pregout2)
  smoke_vars       <- Hmisc::Cs(lmn_smoker, lmn_ncigs)
  benefits_vars    <- Hmisc::Cs(lmn_benbase1, lmn_benbase2, lmn_benbase3, lmn_benbase4, lmn_benbase96,
                                lmn_benctc)
  pension_vars     <- Hmisc::Cs(lmn_benpen1, lmn_benpen2, lmn_benpen3, lmn_benpen4, lmn_benpen5, lmn_benpen6, lmn_benpen7, lmn_benpen8, lmn_benpen96)
  bendis_vars      <- Hmisc::Cs(lmn_bendis1, lmn_bendis2, lmn_bendis3, lmn_bendis4, lmn_bendis5, lmn_bendis12,
                                lmn_bendis7, lmn_bendis8, lmn_bendis10, lmn_bendis97, lmn_bendis96)
  otherben_vars    <- Hmisc::Cs(lmn_benesa,
                                lmn_othben1, lmn_othben2,                           lmn_othben5, lmn_othben6, lmn_othben7, lmn_othben8, lmn_othben9, lmn_othben97, lmn_othben96)
  benincome_vars   <- Hmisc::Cs(lmn_bensta2, lmn_bensta3, lmn_bensta4, lmn_bensta5, lmn_bensta6, lmn_bensta7, lmn_bensta97, lmn_bensta96)
  weight_vars      <- Hmisc::Cs(lmn_inding2_xw)

  s2020_vars       <- Hmisc::Cs(lmn_ethn_dv)

  names <- c(id_vars, demographic_vars, econ_stat_vars, work_vars, education_vars,
             health_vars, preg_vars, smoke_vars,
             benefits_vars, pension_vars, bendis_vars, otherben_vars, benincome_vars,
             weight_vars, s2020_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","lmn_hidp","lmn_pno","lmn_psu","lmn_strata","lmn_istrtdaty","lmn_istrtdatm","lmn_istrtdatd",
                         ## demographic
                         "lmn_sex","lmn_dvage","lmn_birthy","lmn_gor_dv","lmn_urban_dv","lmn_mlstat","lmn_marstat",
                         ## economic status
                         "lmn_jbstat","lmn_jbhas","lmn_jboff","lmn_jboffy",
                         ## work variables
                         "lmn_paygu_dv","lmn_payg_dv","lmn_jbhrs","lmn_fimnlabgrs_dv","lmn_seearngrs_dv","lmn_jbsic07_cc",
                         ## education variables
                         "lmn_hiqual_dv",
                         ## health variables
                         "lmn_health","lmn_aidhh","lmn_sclfsat1","lmn_sclfsato","lmn_sf12pcs_dv","lmn_sf12mcs_dv",
                         "lmn_scsf1","lmn_scsf2a","lmn_scsf2b","lmn_scsf3a","lmn_scsf3b","lmn_scsf4a","lmn_scsf4b","lmn_scsf5","lmn_scsf6a","lmn_scsf6b","lmn_scsf6c","lmn_scsf7",
                         ## health conditions
                         #"lmn_hconds01", "lmn_hconds03", "lmn_hconds04", "lmn_hconds05", "lmn_hconds08",
                         #                "lmn_hconds11", "lmn_hconds12", "lmn_hconds15", "lmn_hconds16",
                         #"lmn_hconds21"                                                , "lmn_hconds26", "lmn_hconds27", "lmn_hconds28", "lmn_hconds29",
                         #"lmn_hconds30", "lmn_hconds31", "lmn_hconds32", "lmn_hconds33", "lmn_hconds34", "lmn_hconds35",

                         #"lmn_hcondns1", "lmn_hcondns3", "lmn_hcondns4", "lmn_hcondns5", "lmn_hcondns6", "lmn_hcondns7", "lmn_hcondns8",
                         #"lmn_hcondns10",
                         ## pregnancy variables
                         "lmn_pregout1","lmn_pregout2",
                         ## smoke variables
                         "lmn_smoker", "lmn_ncigs",
                         ## benefits
                         "lmn_benbase1","lmn_benbase2","lmn_benbase3","lmn_benbase4","lmn_benbase96",
                         "lmn_benctc",
                         ## pensions
                         "lmn_benpen1","lmn_benpen2","lmn_benpen3","lmn_benpen4","lmn_benpen5","lmn_benpen6","lmn_benpen7","lmn_benpen8","lmn_benpen96",
                         ## disability benefits
                         "lmn_bendis1","lmn_bendis2","lmn_bendis3","lmn_bendis4","lmn_bendis5","lmn_bendis12",
                         "lmn_bendis7","lmn_bendis8","lmn_bendis10","lmn_bendis97","lmn_bendis96",
                         ## other benefits
                         "lmn_benesa",
                         "lmn_othben1","lmn_othben2"                            ,"lmn_othben5","lmn_othben6","lmn_othben7","lmn_othben8","lmn_othben9","lmn_othben97","lmn_othben96",
                         ## benefit income variables (formerly receivables)
                         "lmn_bensta2","lmn_bensta3","lmn_bensta4","lmn_bensta5","lmn_bensta6","lmn_bensta7","lmn_bensta97","lmn_bensta96",
                         ## weight
                         "lmn_inding2_xw",
                         ## 2020 specific variables
                         "lmn_ethn_dv"),


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
                         ## health condition variables
                         #"hconds01", "hconds03", "hconds04", "hconds05", "hconds08",
                         #            "hconds11", "hconds12", "hconds15", "hconds16",
                         #"hconds21",                                     "hconds26", "hconds27", "hconds28", "hconds29",
                         #"hconds30", "hconds31", "hconds32", "hconds33", "hconds34", "hconds35",

                         #"hcondns1", "hcondns3", "hcondns4", "hcondns5", "hcondns6", "hcondns7", "hcondns8", "hcondns10",
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

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::green("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/lmn_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(lmn_hidp, lmn_tenure_dv, lmn_nkids_dv, lmn_hhsize, lmn_hhtype_dv,
                                   lmn_nch02_dv, lmn_nch34_dv, lmn_nch511_dv, lmn_nch1215_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("lmn_hidp", "lmn_tenure_dv", "lmn_nkids_dv", "lmn_hhsize","lmn_hhtype_dv",
                         "lmn_nch02_dv", "lmn_nch34_dv", "lmn_nch511_dv", "lmn_nch1215_dv"),
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
    paste0(path, "/lmn_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indall_vars  <- colnames(data.indall[ , c("pidp","lmn_hidp")])

  data.indall <- data.indall[ , indall_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","lmn_hidp"),
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
  data_merged <- data_merged[year == 2022,]

  if (inflation_index == "cpih"){
    inflation <- ukhlsclean::cpih
  }
  if (inflation_index == "rpi"){
    inflation <- ukhlsclean::rpi
  }

  cleaned <- ukhls_clean_global(data = data_merged,
                                ages = ages,
                                country = country,
                                complete_vars = complete_vars,
                                calendar_year = TRUE,
                                inflation = inflation)
  return(cleaned)
}
