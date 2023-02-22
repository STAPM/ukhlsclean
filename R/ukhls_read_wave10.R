#' Read Understanding Society Wave 10
#'
#' Reads and performs basic cleaning operations on the UKHLS tenth wave. Missing values as detailed below are all set to NA.
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
ukhls_read_wave10 <- function(
  root = c("C:/"),
  file = "Users/cm1djm/Documents/Datasets/UKHLS/tab/",
  full = TRUE
) {

  cat(crayon::magenta("\tReading UKHLS Wave 10 datasets"))

  cat(crayon::red("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/j_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[j_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, j_hidp, j_pno, j_psu, j_strata, j_istrtdaty, j_istrtdatm, j_istrtdatd)
  demographic_vars <- Hmisc::Cs(j_sex, j_dvage, j_birthy, j_gor_dv, j_urban_dv, j_mlstat, j_marstat)
  prev_wave_vars   <- Hmisc::Cs(j_notempchk, j_empchk)
  econ_stat_vars   <- Hmisc::Cs(j_jbstat, j_jbhas, j_jboff, j_jboffy, j_jbterm1, j_jbterm2, j_jbsemp, j_jbpen, j_jbpenm)
  work_vars        <- Hmisc::Cs(j_paygu_dv, j_payg_dv, j_jbhrs, j_fimnlabgrs_dv, j_seearngrs_dv, j_jbsic07_cc, j_jbot, j_jbotpd)
  employees_vars   <- Hmisc::Cs(j_paygl, j_paynl, j_payu, j_payug, j_paytyp, j_ovtpay, j_pvtpyset, j_extrate, j_extrest, j_basnset, j_basrate, j_basrest, j_ovtnset, j_ovtrate, j_ovtrest)
  s.emp_vars       <- Hmisc::Cs(j_jshrs, j_jspayu, j_jspytx, j_jspyni)
  non.emp_vars     <- Hmisc::Cs(j_jbhad)
  job2_vars        <- Hmisc::Cs(j_j2has, j_j2semp, j_j2hrs, j_j2pay)
  benefits_vars    <- Hmisc::Cs(j_benbase1, j_benbase2, j_benbase3, j_benbase4, j_benbase96,
                                j_benctc)
  pension_vars     <- Hmisc::Cs(j_benpen1, j_benpen2, j_benpen3, j_benpen4, j_benpen5, j_benpen6, j_benpen7, j_benpen8, j_benpen96,
                                j_niserps)
  bendis_vars      <- Hmisc::Cs(j_bendis1, j_bendis2, j_bendis3, j_bendis4, j_bendis5, j_bendis12,
                                j_bendis7, j_bendis8, j_bendis10, j_bendis97, j_bendis96)
  otherben_vars    <- Hmisc::Cs(j_benesa,
                                j_othben1, j_othben2, j_othben3, j_othben4, j_othben5, j_othben6, j_othben7, j_othben8, j_othben9, j_othben97, j_othben96)
  benincome_vars   <- Hmisc::Cs(j_bensta2, j_bensta3, j_bensta4, j_bensta5, j_bensta6, j_bensta7, j_bensta97, j_bensta96)
  hhfinance_vars   <- Hmisc::Cs(j_fiyrdia, j_fiyrdb1, j_fiyrdb2, j_fiyrdb3, j_fiyrdb4, j_fiyrdb5, j_fiyrdb6, j_finnow, j_finfut)
  education_vars   <- Hmisc::Cs(j_hiqual_dv)
  health_vars      <- Hmisc::Cs(j_health, j_aidhh, j_sclfsat1, j_sclfsato, j_sf12pcs_dv, j_sf12mcs_dv,
                                j_scsf1, j_scsf2a, j_scsf2b, j_scsf3a, j_scsf3b, j_scsf4a, j_scsf4b, j_scsf5, j_scsf6a, j_scsf6b, j_scsf6c, j_scsf7)
  preg_vars        <- Hmisc::Cs(j_preg,
                                j_pregout1, j_pregend1, j_pregsmoke1, j_smkmnth11, j_smkmnth21, j_smkmnth31, j_pregsmk11, j_pregsmk21, j_pregsmk31, j_aedrof1, j_aepuwk1, j_aepuda1, j_lchmulti1,
                                j_pregout2, j_pregend2, j_pregsmoke2, j_smkmnth12, j_smkmnth22, j_smkmnth32, j_pregsmk12, j_pregsmk22, j_pregsmk32, j_aedrof2, j_aepuwk2, j_aepuda2, j_lchmulti2,
                                j_pregout3, j_pregend3, j_pregsmoke3, j_smkmnth13, j_smkmnth23, j_smkmnth33, j_pregsmk13, j_pregsmk23, j_pregsmk33, j_aedrof3, j_aepuwk3, j_aepuda3, j_lchmulti3,
                                j_pregout4, j_pregend4, j_pregsmoke4, j_smkmnth14, j_smkmnth24, j_smkmnth34, j_pregsmk14, j_pregsmk24, j_pregsmk34, j_aedrof4, j_aepuwk4, j_aepuda4, j_lchmulti4,
                                j_pregout5, j_pregend5, j_pregsmoke5, j_smkmnth15, j_smkmnth25, j_smkmnth35, j_pregsmk15, j_pregsmk25, j_pregsmk35, j_aedrof5, j_aepuwk5, j_aepuda5, j_lchmulti5,
                                j_nnewborn)
  smoke_vars       <- Hmisc::Cs(j_smoker, j_ncigs, j_giveup, j_gvupreas1, j_gvupreas2, j_gvupreas3, j_gvupreas4, j_gvupreas5, j_gvupreas6, j_gvupreas7, j_gvupreas8, j_gvupreas9, j_ecigs1)
  alc_vars         <- Hmisc::Cs(j_dklm, j_drnk4w, j_evralc, j_fivealcdr)
  weight_vars      <- Hmisc::Cs(j_indinus_lw, j_indinui_xw)


  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars, work_vars, employees_vars, s.emp_vars, non.emp_vars, job2_vars, benefits_vars, pension_vars, bendis_vars, otherben_vars, benincome_vars, hhfinance_vars, education_vars, health_vars, preg_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","j_hidp","j_pno","j_psu","j_strata","j_istrtdaty","j_istrtdatm","j_istrtdatd",
                         ## demographic
                         "j_sex","j_dvage","j_birthy","j_gor_dv","j_urban_dv","j_mlstat","j_marstat",
                         ## previous wave variables
                         "j_notempchk","j_empchk",
                         ## economic status
                         "j_jbstat","j_jbhas","j_jboff","j_jboffy","j_jbterm1","j_jbterm2","j_jbsemp","j_jbpen","j_jbpenm",
                         ## work variables
                         "j_paygu_dv","j_payg_dv","j_jbhrs","j_fimnlabgrs_dv","j_seearngrs_dv","j_jbsic07_cc","j_jbot","j_jbotpd",
                         ## employees
                         "j_paygl","j_paynl","j_payu","j_payug","j_paytyp","j_ovtpay","j_pvtpyset","j_extrate","j_extrest","j_basnset","j_basrate",
                         "j_basrest","j_ovtnset","j_ovtrate","j_ovtrest",
                         ## self-employed
                         "j_jshrs","j_jspayu","j_jspytx","j_jspyni",
                         ## non-employed
                         "j_jbhad",
                         ## second job
                         "j_j2has","j_j2semp","j_j2hrs","j_j2pay",
                         ## benefits
                         "j_benbase1","j_benbase2","j_benbase3","j_benbase4","j_benbase96",
                         "j_benctc",
                         ## pensions
                         "j_benpen1","j_benpen2","j_benpen3","j_benpen4","j_benpen5","j_benpen6","j_benpen7","j_benpen8","j_benpen96",
                         "j_niserps",
                         ## disability benefits
                         "j_bendis1","j_bendis2","j_bendis3","j_bendis4","j_bendis5","j_bendis12",
                         "j_bendis7","j_bendis8","j_bendis10","j_bendis97","j_bendis96",
                         ## other benefits
                         "j_benesa",
                         "j_othben1","j_othben2","j_othben3","j_othben4","j_othben5","j_othben6","j_othben7","j_othben8","j_othben9","j_othben97","j_othben96",
                         ## benefit income variables (formerly receivables)
                         "j_bensta2","j_bensta3","j_bensta4","j_bensta5","j_bensta6","j_bensta7","j_bensta97","j_bensta96",
                         ## household finance variables (interest and dividends)
                         "j_fiyrdia","j_fiyrdb1","j_fiyrdb2","j_fiyrdb3","j_fiyrdb4","j_fiyrdb5","j_fiyrdb6","j_finnow","j_finfut",
                         ## education variables
                         "j_hiqual_dv",
                         ## health variables
                         "j_health","j_aidhh","j_sclfsat1","j_sclfsato","j_sf12pcs_dv","j_sf12mcs_dv",
                         "j_scsf1","j_scsf2a","j_scsf2b","j_scsf3a","j_scsf3b","j_scsf4a","j_scsf4b","j_scsf5","j_scsf6a","j_scsf6b","j_scsf6c","j_scsf7",
                         ## pregnancy variables
                         "j_pregout1","j_pregout2","j_pregout3",
                         ## smoke variables
                         "j_smoker", "j_ncigs", "j_giveup", "j_gvupreas1", "j_gvupreas2", "j_gvupreas3",
                         "j_gvupreas4", "j_gvupreas5", "j_gvupreas6", "j_gvupreas7", "j_gvupreas8", "j_gvupreas9", "j_ecigs1",
                         ## alcohol variables
                         "j_dklm","j_drnk4w","j_evralc","j_fivealcdr",
                         ## weight
                         "j_indinus_lw","j_indinui_xw"),

                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "sex","age","birth_year","region","urban","mlstat","marstat",
                         ## previous wave variables
                         "notempchk","empchk",
                         ## economic status
                         "econ_stat","jbhas","jboff","jboffy","jbterm1","jbterm2","jbsemp","jbpen","jbpen_member",
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
                         "pregout1","pregout2","pregout3",
                         ## smoke variables
                         "smoker", "ncigs", "giveup_smk", "gvupsmk_curr_health_prblm", "gvupsmk_general_better_health", "gvupsmk_risk_down_smk_illness",
                         "gvupsmk_public_work_smk_ban", "gvupsmk_fam&frnds", "gvupsmk_finances", "gvupsmk_chld_effect", "gvupsmk_fam_effect", "gvupsmk_other_reason", "ecigs",
                         ## alcohol variables
                         "dklm","drnk4w","evralc","fivealcdr",
                         ## weight
                         "weight_lw","weight_xw"))

  data[, wave := "UKHLS Wave 10"]
  data[, wave_no := 10]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data[, dataset := "UKHLS"]
  data[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::red("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/j_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(j_hidp, j_tenure_dv, j_nkids_dv, j_hhsize, j_hhtype_dv,
                                   j_nch02_dv, j_nch34_dv, j_nch511_dv, j_nch1215_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("j_hidp","j_tenure_dv","j_nkids_dv","j_hhsize","j_hhtype_dv",
                         "j_nch02_dv","j_nch34_dv","j_nch511_dv","j_nch1215_dv"),
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
