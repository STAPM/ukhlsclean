#' Read Understanding Society Wave 1
#'
#' Reads and performs basic cleaning operations on the UKHLS first wave. Missing values as detailed below are all set to NA.
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
ukhls_read_wave1 <- function(
  root = c("X:/"),
  file = "HAR_PR/PR/USoc/Data/SN6614_2023_11_24/tab/ukhls/",
  full = TRUE
) {

  cat(crayon::blue(crayon::underline("\tReading UKHLS Wave 1 datasets")))

  cat(crayon::green("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/a_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[a_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp,a_hidp,a_pno,a_psu,a_strata,a_istrtdaty,a_istrtdatm,a_istrtdatd)
  demographic_vars <- Hmisc::Cs(a_sex,a_dvage,a_birthy,a_gor_dv,a_urban_dv,a_mlstat, a_marstat)
  econ_stat_vars   <- Hmisc::Cs(a_jbstat,a_jbhas,a_jboff,a_jboffy, a_jbterm1, a_jbterm2, a_jbsemp, a_jbpen, a_jbpenm)
  work_vars        <- Hmisc::Cs(a_paygu_dv,a_payg_dv,a_jbhrs,a_fimnlabgrs_dv,a_seearngrs_dv, a_jbsic07_cc, a_jbot, a_jbotpd,
                                a_jbnssec_dv, a_jbnssec3_dv, a_jbnssec5_dv, a_jbnssec8_dv, a_jbsize)
  employees_vars   <- Hmisc::Cs(a_paygl, a_paynl, a_payu, a_payug, a_ovtpay, a_extnsa, a_extrate, a_extrest, a_basnsa, a_basrate, a_basrest, a_ovtnsa, a_ovtrate, a_ovtrest)
  s.emp_vars       <- Hmisc::Cs(a_jshrs, a_jspayu, a_jspytx, a_jspyni)
  non.emp_vars     <- Hmisc::Cs(a_jbhad)
  job2_vars        <- Hmisc::Cs(a_j2has, a_j2semp, a_j2hrs, a_j2pay)
  benefits_vars    <- Hmisc::Cs(a_btype1, a_btype2, a_btype3, a_btype4, a_btype5, a_btype6, a_btype7, a_btype8, a_btype9, a_btype96,
                                a_benunemp1, a_benunemp2, a_benunemp96, a_bendis1, a_bendis2, a_bendis3, a_bendis4, a_bendis5, a_bendis6, a_bendis7,
                                a_bendis8, a_bendis9, a_bendis10, a_bendis11, a_bendis96)
  pension_vars     <- Hmisc::Cs(a_benpen1, a_benpen2, a_benpen3, a_benpen4, a_benpen5, a_benpen6, a_benpen7, a_benpen8, a_benpen96)
  receivables_vars <- Hmisc::Cs(a_niserps, a_bencb, a_benctc, a_benfam1, a_benfam2, a_benfam3, a_benfam4, a_benfam5,
                                a_benfam96, a_bentax1, a_bentax2, a_bentax3, a_bentax4, a_bentax5, a_bentax96, a_benhou1,
                                a_benhou2, a_benhou3, a_benhou4, a_benhou96, a_bensta1, a_bensta2, a_bensta3, a_bensta4,
                                a_bensta5, a_bensta6, a_bensta7, a_bensta8, a_bensta96)
  hhfinance_vars   <- Hmisc::Cs(a_fiyrdia, a_fiyrdb1, a_fiyrdb2, a_fiyrdb3, a_fiyrdb4, a_fiyrdb5, a_fiyrdb6, a_finnow, a_finfut)
  education_vars   <- Hmisc::Cs(a_hiqual_dv)
  health_vars      <- Hmisc::Cs(a_health,a_aidhh,a_sclfsat1,a_sclfsato,a_sf12pcs_dv,a_sf12mcs_dv,
                                a_sf1,a_sf2a,a_sf2b,a_sf3a,a_sf3b,a_sf4a,a_sf4b,a_sf5,a_sf6a,a_sf6b,a_sf6c,a_sf7)
  weight_vars      <- Hmisc::Cs(a_indinus_xw)


  names <- c(id_vars, demographic_vars, econ_stat_vars, work_vars, employees_vars, s.emp_vars, non.emp_vars, job2_vars, benefits_vars, pension_vars, receivables_vars, hhfinance_vars, education_vars, health_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","a_hidp","a_pno","a_psu","a_strata","a_istrtdaty","a_istrtdatm","a_istrtdatd",
                         ## demographic
                         "a_sex","a_dvage","a_birthy","a_gor_dv","a_urban_dv","a_mlstat","a_marstat",
                         ## economic status
                         "a_jbstat","a_jbhas","a_jboff","a_jboffy","a_jbterm1","a_jbterm2","a_jbsemp","a_jbpen","a_jbpenm",
                         ## work variables
                         "a_paygu_dv","a_payg_dv","a_jbhrs","a_fimnlabgrs_dv","a_seearngrs_dv","a_jbsic07_cc","a_jbot","a_jbotpd",
                         "a_jbnssec_dv","a_jbnssec3_dv","a_jbnssec5_dv","a_jbnssec8_dv","a_jbsize",
                         ## employees
                         "a_paygl","a_paynl","a_payu","a_payug","a_ovtpay","a_extnsa","a_extrate","a_extrest","a_basnsa","a_basrate",
                         "a_basrest","a_ovtnsa","a_ovtrate","a_ovtrest",
                         ## self-employed
                         "a_jshrs","a_jspayu","a_jspytx","a_jspyni",
                         ## non-employed
                         "a_jbhad",
                         ## second job
                         "a_j2has","a_j2semp","a_j2hrs","a_j2pay",
                         ## benefits
                         "a_btype1","a_btype2","a_btype3","a_btype4","a_btype5","a_btype6","a_btype7","a_btype8","a_btype9","a_btype96",
                         "a_benunemp1","a_benunemp2","a_benunemp96","a_bendis1","a_bendis2","a_bendis3","a_bendis4","a_bendis5","a_bendis6","a_bendis7",
                         "a_bendis8","a_bendis9","a_bendis10","a_bendis11","a_bendis96",
                         ## pensions
                         "a_benpen1","a_benpen2","a_benpen3","a_benpen4","a_benpen5","a_benpen6","a_benpen7","a_benpen8","a_benpen96",
                         ## receivables
                         "a_niserps","a_bencb","a_benctc","a_benfam1","a_benfam2","a_benfam3","a_benfam4","a_benfam5",
                         "a_benfam96","a_bentax1","a_bentax2","a_bentax3","a_bentax4","a_bentax5","a_bentax96","a_benhou1",
                         "a_benhou2","a_benhou3","a_benhou4","a_benhou96","a_bensta1","a_bensta2","a_bensta3","a_bensta4",
                         "a_bensta5","a_bensta6","a_bensta7","a_bensta8","a_bensta96",
                         ## household finance variables (interest and dividends)
                         "a_fiyrdia","a_fiyrdb1","a_fiyrdb2","a_fiyrdb3","a_fiyrdb4","a_fiyrdb5","a_fiyrdb6","a_finnow","a_finfut",
                         ## education variables
                         "a_hiqual_dv",
                         ## health variables
                         "a_health","a_aidhh","a_sclfsat1","a_sclfsato","a_sf12pcs_dv","a_sf12mcs_dv",
                         "a_sf1","a_sf2a","a_sf2b","a_sf3a","a_sf3b","a_sf4a","a_sf4b","a_sf5","a_sf6a","a_sf6b","a_sf6c","a_sf7",
                         ## weight
                         "a_indinus_xw"),

                       c("pidp","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "sex","age","birth_year","region","urban","mlstat","marstat",
                         ## economic status
                         "econ_stat","jbhas","jboff","jboffy","jbterm1","jbterm2","jbsemp","jbpen","jbpen_member",
                         ## work variables
                         "grss_pay_usual","grss_pay_last","hours","grss_lab_inc","grss_semp","sic07","ovthours_pw","ovthours_paid",
                         "nssec","nssec_3cat","nssec_5cat","nssec_8cat", "jbsize",
                         ## employees
                         "last_gross_pay","last_net_pay","usual_pay","payug","ovtpay","extnsa","extrate","ext_estimate","baspay_amount","baspay_rate",
                         "baspay_estimate","ovtpay_amount","ovtpay_rate","ovtpay_estimate",
                         ## self-employed
                         "s.emp_hours","s.emp_pay","s.emp_pay_pretax","s.emp_pay_preNI",
                         ## non-employed
                         "jbhad",
                         ## second job
                         "2ndjb","2ndjb_s.emp","2ndjb_hours","2ndjob_pay",
                         ## benefits
                         "btype1","btype2","btype3","btype4","btype5","btype6","btype7","btype8","btype9","btype96",
                         "benunemp1","benunemp2","benunemp96","bendis1","bendis2","bendis3","bendis4","bendis5","bendis6","bendis7",
                         "bendis8","bendis9","bendis10","bendis11","bendis96",
                         ## pensions
                         "NI.state_pen","employer_pen","spouse.emp_pen","pencred_pen","prvt_pen","widow_pen","parent_pen","benpen8","non_benpen",
                         ## receivables
                         "income_serps","ben_childben","ben_childtaxcred","benfam_fosterguard","benfam_mat","benfam_alimony","benfam_lone","benfam_fampay",
                         "non_benfam","bentax_work","bentax_council","bentax_pencred","bentax_childtaxcred","bentax_rtw","non_bentax","benhou_house",
                         "benhou_counciltax","benhou_rentreb","benhou_ratereb","non_benhou","bensta_prvtpen","bensta_edugrant","bensta_tupay","bensta_alimony",
                         "bensta_fampay","bensta_rentlodge","bensta_rentother","bensta_other","non_bensta",
                         ## household finance variables
                         "fiyrdia","fiyrdb1","fiyrdb2","fiyrdb3","fiyrdb4","fiyrdb5","fiyrdb6","finnow","finfut",
                         ## education variables
                         "highest_qual",
                         ## health variables
                         "lt_sick","caring","health_satisf","life_satisf","sf12_pcs","sf12_mcs",
                         "sf1","sf2a","sf2b","sf3a","sf3b","sf4a","sf4b","sf5","sf6a","sf6b","sf6c","sf7",
                         ## weight
                         "weight_xw"))

  data[, wave := "UKHLS Wave 1"]
  data[, wave_no := 1]
  data[, bhps_sample := FALSE]
  data[, dataset := "UKHLS"]
  data[, id := pidp]

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::green("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/a_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(a_hidp, a_tenure_dv, a_numadult, a_numchild, a_hhsize, a_hhtype_dv,
                                   a_nch02_dv, a_nch34_dv, a_nch511_dv, a_nch1215_dv,
                                   a_fihhmngrs1_dv, a_fihhmnlabgrs_dv,
                                   a_fihhmnnet1_dv, a_fihhmnlabnet_dv, a_fihhmnsben_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("a_hidp","a_tenure_dv","a_numadult","a_numchild","a_hhsize","a_hhtype_dv",
                         "a_nch02_dv","a_nch34_dv","a_nch511_dv","a_nch1215_dv",
                         "a_fihhmngrs1_dv", "a_fihhmnlabgrs_dv",
                         "a_fihhmnnet1_dv", "a_fihhmnlabnet_dv", "a_fihhmnsben_dv"),
                       # new names
                       c("hidp","hh_tenure","hh_numadult","hh_numchild","hh_size","hh_type",
                         "hh_numchild02","hh_numchild34","hh_numchild511","hh_numchild1215",
                         "hh_fihhmngrs1_dv", "hh_fihhmnlabgrs_dv",
                         "hh_fihhmnnet1_dv", "hh_fihhmnlabnet_dv", "hh_fihhmnsben_dv"))

  hhold_merged <- merge(x = data,
                        y = data.hhold,
                        by="hidp",
                        all.x=TRUE,
                        all.y=FALSE)

  #########################################
  ######## ADD IN CROSS-WAVE DATA #########

  cat(crayon::green("\tCross-Wave..."))

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
                       c("pidp","ethnicity_raw","deceased","deceased_when"))

  ## Combine - keep all observations in the main data and drop excess xwave observations

  xwave_merged <- merge(x = hhold_merged,
                        y = data.xwave,
                                   by="pidp",
                                   all.x=TRUE,
                                   all.y=FALSE)

  ####################################################
  #### ADD IN THE INDALL DATA ########################

  cat(crayon::green("\tIndall..."))

  data.indall <- data.table::fread(
    paste0(path, "/a_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indall_vars  <- colnames(data.indall[ , c("pidp","a_hidp",
                                           "a_imd2019qe_dv","a_imd2017qni_dv",
                                           "a_imd2020qs_dv","a_imd2019qw_dv")])

  data.indall <- data.indall[ , indall_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","a_hidp",
                         "a_imd2019qe_dv","a_imd2017qni_dv",
                         "a_imd2020qs_dv","a_imd2019qw_dv"),
                       # new names
                       c("pidp","hidp",
                         "imdq_e","imdq_ni",
                         "imdq_s","imdq_w"))

  ## Combine - keep all observations in the main data and drop excess xwave observations

  data_merged <- merge(x = xwave_merged,
                       y = data.indall,
                       by = c("pidp","hidp"),
                       all.x = TRUE,
                       all.y = FALSE)

  ##########################################################################

  cat(crayon::blue(crayon::bold("\tdone\n")))

  return(data_merged)
}
