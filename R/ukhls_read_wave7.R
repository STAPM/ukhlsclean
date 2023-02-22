#' Read Understanding Society Wave 7
#'
#' Reads and performs basic cleaning operations on the UKHLS seventh wave. Missing values as detailed below are all set to NA.
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
ukhls_read_wave7 <- function(
  root = c("C:/"),
  file = "Users/cm1djm/Documents/Datasets/UKHLS/tab/",
  full = TRUE
) {

  cat(crayon::magenta("\tReading UKHLS Wave 7 datasets"))

  cat(crayon::red("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/g_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[g_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, g_hidp, g_pno, g_psu, g_strata, g_istrtdaty, g_istrtdatm, g_istrtdatd)
  demographic_vars <- Hmisc::Cs(g_sex, g_dvage, g_birthy, g_gor_dv, g_urban_dv, g_mlstat, g_marstat)
  prev_wave_vars   <- Hmisc::Cs(g_notempchk, g_empchk)
  econ_stat_vars   <- Hmisc::Cs(g_jbstat, g_jbhas, g_jboff, g_jboffy, g_jbterm1, g_jbterm2, g_jbsemp)
  work_vars        <- Hmisc::Cs(g_paygu_dv, g_payg_dv, g_jbhrs, g_fimnlabgrs_dv, g_seearngrs_dv, g_jbsic07_cc, g_jbot, g_jbotpd)
  employees_vars   <- Hmisc::Cs(g_paygl, g_paynl, g_payu, g_payug, g_paytyp, g_ovtpay, g_pvtpyset, g_extrate, g_extrest, g_basnset, g_basrate, g_basrest, g_ovtnset, g_ovtrate, g_ovtrest)
  s.emp_vars       <- Hmisc::Cs(g_jshrs, g_jspayu, g_jspytx, g_jspyni)
  non.emp_vars     <- Hmisc::Cs(g_jbhad)
  job2_vars        <- Hmisc::Cs(g_j2has, g_j2semp, g_j2hrs, g_j2pay)
  benefits_vars    <- Hmisc::Cs(g_benbase1, g_benbase2, g_benbase3, g_benbase4, g_benbase96,
                                g_benctc)
  pension_vars     <- Hmisc::Cs(g_benpen1, g_benpen2, g_benpen3, g_benpen4, g_benpen5, g_benpen6, g_benpen7, g_benpen8, g_benpen96,
                                g_niserps)
  bendis_vars      <- Hmisc::Cs(g_bendis1, g_bendis2, g_bendis3, g_bendis4, g_bendis5, g_bendis12,
                                g_bendis7, g_bendis8, g_bendis10, g_bendis97, g_bendis96)
  otherben_vars    <- Hmisc::Cs(g_benesa,
                                g_othben1, g_othben2, g_othben3, g_othben4, g_othben5, g_othben6, g_othben7, g_othben8, g_othben9, g_othben97, g_othben96)
  benincome_vars   <- Hmisc::Cs(g_bensta2, g_bensta3, g_bensta4, g_bensta5, g_bensta6, g_bensta7, g_bensta97, g_bensta96)
  hhfinance_vars   <- Hmisc::Cs(g_fiyrdia, g_fiyrdb1, g_fiyrdb2, g_fiyrdb3, g_fiyrdb4, g_fiyrdb5, g_fiyrdb6, g_finnow, g_finfut)
  education_vars   <- Hmisc::Cs(g_hiqual_dv)
  health_vars      <- Hmisc::Cs(g_health, g_aidhh, g_sclfsat1, g_sclfsato, g_sf12pcs_dv, g_sf12mcs_dv,
                                g_scsf1, g_scsf2a, g_scsf2b, g_scsf3a, g_scsf3b, g_scsf4a, g_scsf4b, g_scsf5, g_scsf6a, g_scsf6b, g_scsf6c, g_scsf7)
  preg_vars        <- Hmisc::Cs(g_preg,
                                g_pregout1, g_pregend1, g_pregsmoke1, g_smkmnth11, g_smkmnth21, g_smkmnth31, g_pregsmk11, g_pregsmk21, g_pregsmk31, g_aedrof1, g_aepuwk1, g_aepuda1, g_lchmulti1,
                                g_pregout2, g_pregend2, g_pregsmoke2, g_smkmnth12, g_smkmnth22, g_smkmnth32, g_pregsmk12, g_pregsmk22, g_pregsmk32, g_aedrof2, g_aepuwk2, g_aepuda2, g_lchmulti2,
                                g_pregout3, g_pregend3, g_pregsmoke3, g_smkmnth13, g_smkmnth23, g_smkmnth33, g_pregsmk13, g_pregsmk23, g_pregsmk33, g_aedrof3, g_aepuwk3, g_aepuda3, g_lchmulti3,
                                g_nnewborn)
  smoke_vars       <- Hmisc::Cs(g_smoker, g_ncigs, g_ecigs)
  alc_vars         <- Hmisc::Cs(g_auditc1, g_auditc2, g_auditc3, g_auditc4, g_auditc5)
  weight_vars      <- Hmisc::Cs(g_indinus_lw, g_indinui_xw)


  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars, work_vars, employees_vars, s.emp_vars, non.emp_vars, job2_vars, benefits_vars, pension_vars, bendis_vars, otherben_vars, benincome_vars, hhfinance_vars, education_vars, health_vars, preg_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","g_hidp","g_pno","g_psu","g_strata","g_istrtdaty","g_istrtdatm","g_istrtdatd",
                         ## demographic
                         "g_sex","g_dvage","g_birthy","g_gor_dv","g_urban_dv","g_mlstat","g_marstat",
                         ## previous wave variables
                         "g_notempchk","g_empchk",
                         ## economic status
                         "g_jbstat","g_jbhas","g_jboff","g_jboffy","g_jbterm1","g_jbterm2","g_jbsemp",
                         ## work variables
                         "g_paygu_dv","g_payg_dv","g_jbhrs","g_fimnlabgrs_dv","g_seearngrs_dv","g_jbsic07_cc","g_jbot","g_jbotpd",
                         ## employees
                         "g_paygl","g_paynl","g_payu","g_payug","g_paytyp","g_ovtpay","g_pvtpyset","g_extrate","g_extrest","g_basnset","g_basrate",
                         "g_basrest","g_ovtnset","g_ovtrate","g_ovtrest",
                         ## self-employed
                         "g_jshrs","g_jspayu","g_jspytx","g_jspyni",
                         ## non-employed
                         "g_jbhad",
                         ## second job
                         "g_j2has","g_j2semp","g_j2hrs","g_j2pay",
                         ## benefits
                         "g_benbase1","g_benbase2","g_benbase3","g_benbase4","g_benbase96",
                         "g_benctc",
                         ## pensions
                         "g_benpen1","g_benpen2","g_benpen3","g_benpen4","g_benpen5","g_benpen6","g_benpen7","g_benpen8","g_benpen96",
                         "g_niserps",
                         ## disability benefits
                         "g_bendis1","g_bendis2","g_bendis3","g_bendis4","g_bendis5","g_bendis12",
                         "g_bendis7","g_bendis8","g_bendis10","g_bendis97","g_bendis96",
                         ## other benefits
                         "g_benesa",
                         "g_othben1","g_othben2","g_othben3","g_othben4","g_othben5","g_othben6","g_othben7","g_othben8","g_othben9","g_othben97","g_othben96",
                         ## benefit income variables (formerly receivables)
                         "g_bensta2","g_bensta3","g_bensta4","g_bensta5","g_bensta6","g_bensta7","g_bensta97","g_bensta96",
                         ## household finance variables (interest and dividends)
                         "g_fiyrdia","g_fiyrdb1","g_fiyrdb2","g_fiyrdb3","g_fiyrdb4","g_fiyrdb5","g_fiyrdb6","g_finnow","g_finfut",
                         ## education variables
                         "g_hiqual_dv",
                         ## health variables
                         "g_health","g_aidhh","g_sclfsat1","g_sclfsato","g_sf12pcs_dv","g_sf12mcs_dv",
                         "g_scsf1","g_scsf2a","g_scsf2b","g_scsf3a","g_scsf3b","g_scsf4a","g_scsf4b","g_scsf5","g_scsf6a","g_scsf6b","g_scsf6c","g_scsf7",
                         ## pregnancy variables
                         "g_preg",
                         "g_pregout1","g_pregend1","g_pregsmoke1","g_smkmnth11","g_smkmnth21","g_smkmnth31","g_pregsmk11","g_pregsmk21","g_pregsmk31","g_aedrof1","g_aepuwk1","g_aepuda1","g_lchmulti1",
                         "g_pregout2","g_pregend2","g_pregsmoke2","g_smkmnth12","g_smkmnth22","g_smkmnth32","g_pregsmk12","g_pregsmk22","g_pregsmk32","g_aedrof2","g_aepuwk2","g_aepuda2","g_lchmulti2",
                         "g_pregout3","g_pregend3","g_pregsmoke3","g_smkmnth13","g_smkmnth23","g_smkmnth33","g_pregsmk13","g_pregsmk23","g_pregsmk33","g_aedrof3","g_aepuwk3","g_aepuda3","g_lchmulti3",
                         "g_nnewborn",
                         ## smoking variables
                         "g_smoker", "g_ncigs","g_ecigs",
                         ## alcohol variables
                         "g_auditc1","g_auditc2","g_auditc3","g_auditc4","g_auditc5",
                         ## weight
                         "g_indinus_lw","g_indinui_xw"),

                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "sex","age","birth_year","region","urban","mlstat","marstat",
                         ## previous wave variables
                         "notempchk","empchk",
                         ## economic status
                         "econ_stat","jbhas","jboff","jboffy","jbterm1","jbterm2","jbsemp",
                         ## work variables
                         "grss_pay_usual","grss_pay_last","hours","grss_lab_inc","grss_semp","sic07","ovthours_pw","ovthours_paid",
                         ## employees
                         "last_gross_pay","last_net_pay","usual_pay","payug","pay_type","ovtpay","additional.pay_set","extrate","ext_estimate","basic.pay_set","baspay_rate",
                         "baspay_estimate","ovt.pay_set","ovtpay_rate","ovtpay_estimate",
                         ## self-employed
                         "s.emp_hours","s.emp_pay","s.emp_pay_pretax","s.emp_pay_preNI",
                         ## non-employed
                         "jbhad",
                         ## second job
                         "2ndjb","2ndjb_s.emp","2ndjb_hours","2ndjob_pay",
                         ## benefits
                         "benbase1","benbase2","benbase3","benbase4","benbase96",
                         "benctc",
                         ## pensions
                         "NI.state_pen","employer_pen","spouse.emp_pen","pencred_pen","prvt_pen","widow_pen","parent_pen","benpen8","non_benpen",
                         "income_serps",
                         ## disability benefits
                         "bendis1","bendis2","bendis3","bendis4","bendis5","bendis12",
                         "bendis7","bendis8","bendis10","bendis97","bendis96",
                         ## other benefits
                         "benesa","othben1","othben2","othben3","othben4","othben5","othben6",
                         "othben7","othben8","othben9","othben97","othben96",
                         ## benefit income variables (formerly receivables)
                         "bensta_edugrant","bensta_tupay","bensta_alimony","bensta_fampay","bensta_rentlodge","bensta_rentother","bensta_other","non_bensta",
                         ## household finance variables
                         "fiyrdia","fiyrdb1","fiyrdb2","fiyrdb3","fiyrdb4","fiyrdb5","fiyrdb6","finnow","finfut",
                         ## education variables
                         "highest_qual",
                         ## health variables
                         "lt_sick","caring","health_satisf","life_satisf","sf12_pcs","sf12_mcs",
                         "sf1","sf2a","sf2b","sf3a","sf3b","sf4a","sf4b","sf5","sf6a","sf6b","sf6c","sf7",
                         ## pregnancy variables
                         "preg",
                         "pregout1","pregend1","pregsmoke1","smkmnth11","smkmnth21","smkmnth31","pregsmk_ncigs11","pregsmk_ncigs21","pregsmk_ncigs31","pregdrnk_freq1","pregdrnk_unitpw1","pregdrnk_unit1","lchmulti1",
                         "pregout2","pregend2","pregsmoke2","smkmnth12","smkmnth22","smkmnth32","pregsmk_ncigs12","pregsmk_ncigs22","pregsmk_ncigs32","pregdrnk_freq2","pregdrnk_unitpw2","pregdrnk_unit2","lchmulti2",
                         "pregout3","pregend3","pregsmoke3","smkmnth13","smkmnth23","smkmnth33","pregsmk_ncigs13","pregsmk_ncigs23","pregsmk_ncigs33","pregdrnk_freq3","pregdrnk_unitpw3","pregdrnk_unit3","lchmulti3",
                         "nnewborn",
                         ## smoking variables
                         "smoker","ncigs","ecigs",
                         ## alcohol variables
                         "auditc1","auditc2","auditc3","auditc4","auditc5",
                         ## weight
                         "weight_lw","weight_xw"))

  data[, wave := "UKHLS Wave 7"]
  data[, wave_no := 7]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data[, dataset := "UKHLS"]
  data[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::red("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/g_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(g_hidp, g_tenure_dv, g_nkids_dv, g_hhsize, g_hhtype_dv,
                                   g_nch02_dv, g_nch34_dv, g_nch511_dv, g_nch1215_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("g_hidp","g_tenure_dv","g_nkids_dv","g_hhsize","g_hhtype_dv",
                         "g_nch02_dv","g_nch34_dv","g_nch511_dv","g_nch1215_dv"),
                       # new names
                       c("hidp","hh_tenure","hh_numchild","hh_size","hh_type",
                         "hh_numchild02","hh_numchild34","hh_numchild511","hh_numchild1215"))

  hhold_merged <- merge(x = data,
                        y = data.hhold,
                        by="hidp",
                        all.x=TRUE,
                        all.y=FALSE)

  #########################################
  ######## ADD IN CROSS-WAVE DATA #########

  cat(crayon::red("\tCross-Wave..."))

  data.xwave <- data.table::fread(
    paste0(path, "/xwavedat.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.xwave, names(data.xwave), tolower(names(data.xwave)))

  xwave_vars  <- colnames(data.xwave[ , c(1,34,17,18)])

  data.xwave <- data.xwave[ , xwave_vars, with = F]
  data.table::setnames(data.xwave,
                       # old names
                       c("pidp","racel_dv","dcsedfl_dv","dcsedw_dv"),
                       # new names
                       c("pidp","ethnicity_raw","deceased","deceased_when"))

  ####### Combine - keep all observations in the main data and drop excess xwave observations

  data_merged <- merge(x = hhold_merged,
                       y = data.xwave,
                       by="pidp",
                       all.x=TRUE,
                       all.y=FALSE)

  cat(crayon::magenta("\tdone\n"))

  return(data_merged)
}
