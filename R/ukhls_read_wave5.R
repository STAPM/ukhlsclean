#' Read Understanding Society Wave 5
#'
#' Reads and performs basic cleaning operations on the UKHLS fifth wave. Missing values as detailed below are all set to NA.
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
ukhls_read_wave5 <- function(
  root = c("X:/"),
  file = "HAR_PR/PR/USoc/Data/SN6614_2024_12_02/tab/ukhls/",
  full = TRUE
) {

  cat(crayon::blue(crayon::underline("\tReading UKHLS Wave 5 datasets")))

  cat(crayon::green("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/e_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[e_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, e_hidp, e_pno, e_psu, e_strata, e_istrtdaty, e_istrtdatm, e_istrtdatd)
  demographic_vars <- Hmisc::Cs(e_sex, e_dvage, e_birthy, e_gor_dv, e_urban_dv, e_mlstat,  e_marstat)
  prev_wave_vars   <- Hmisc::Cs(e_notempchk, e_empchk)
  econ_stat_vars   <- Hmisc::Cs(e_jbstat, e_jbhas, e_jboff, e_jboffy, e_jbterm1, e_jbterm2, e_jbsemp)
  income_vars      <- Hmisc::Cs(e_fimnnet_dv, e_fimngrs_dv,
                                e_fimnlabnet_dv, e_fimnmisc_dv, e_fimnprben_dv, e_fimninvnet_dv, e_fimnpen_dv, e_fimnsben_dv)
  work_vars        <- Hmisc::Cs(e_paygu_dv, e_payg_dv, e_jbhrs, e_fimnlabgrs_dv, e_seearngrs_dv, e_jbsic07_cc, e_jbot, e_jbotpd,
                                e_jbnssec_dv, e_jbnssec3_dv, e_jbnssec5_dv, e_jbnssec8_dv, e_jbsize)
  employees_vars   <- Hmisc::Cs(e_paygl, e_paynl, e_payu, e_payug, e_ovtpay, e_extnsa, e_extrate, e_extrest, e_basnsa, e_basrate, e_basrest, e_ovtnsa, e_ovtrate, e_ovtrest)
  s.emp_vars       <- Hmisc::Cs(e_jshrs, e_jspayu, e_jspytx, e_jspyni)
  non.emp_vars     <- Hmisc::Cs(e_jbhad)
  job2_vars        <- Hmisc::Cs(e_j2has, e_j2semp, e_j2hrs, e_j2pay)
  benefits_vars    <- Hmisc::Cs(e_btype1, e_btype2, e_btype3, e_btype4, e_btype5, e_btype6, e_btype7, e_btype8, e_btype9, e_btype96,
                                e_benunemp1, e_benunemp2, e_benunemp3, e_benunemp96, e_bendis1, e_bendis11, e_bendis2, e_bendis3, e_bendis4, e_bendis5, e_bendis12,
                                e_bendis6, e_bendis7, e_bendis8, e_bendis9, e_bendis10, e_bendis96, e_bendis97)
  pension_vars     <- Hmisc::Cs(e_benpen1, e_benpen2, e_benpen3, e_benpen4, e_benpen5, e_benpen6, e_benpen7, e_benpen8, e_benpen96)
  receivables_vars <- Hmisc::Cs(e_niserps, e_bencb, e_benctc, e_benfam1, e_benfam2, e_benfam3, e_benfam4, e_benfam5,
                                e_benfam96, e_bentax1, e_bentax2, e_bentax3, e_bentax4, e_bentax5, e_bentax96, e_benhou1,
                                e_benhou2, e_benhou3, e_benhou4, e_benhou5, e_benhou96, e_bensta1, e_bensta2, e_bensta3, e_bensta4,
                                e_bensta5, e_bensta6, e_bensta7, e_bensta96, e_bensta97)
  hhfinance_vars   <- Hmisc::Cs(e_fiyrdia, e_fiyrdb1, e_fiyrdb2, e_fiyrdb3, e_fiyrdb4, e_fiyrdb5, e_fiyrdb6, e_finnow, e_finfut)
  education_vars   <- Hmisc::Cs(e_hiqual_dv)
  health_vars      <- Hmisc::Cs(e_health, e_aidhh, e_sclfsat1, e_sclfsato, e_sf12pcs_dv, e_sf12mcs_dv,
                                e_scsf1, e_scsf2a, e_scsf2b, e_scsf3a, e_scsf3b, e_scsf4a, e_scsf4b, e_scsf5, e_scsf6a, e_scsf6b, e_scsf6c, e_scsf7,
                                e_scghq1_dv,e_scghq2_dv)
  preg_vars        <- Hmisc::Cs(e_preg,
                                e_pregout1, e_pregend1, e_pregsmoke1, e_smkmnth11, e_smkmnth21, e_smkmnth31, e_pregsmk11, e_pregsmk21, e_pregsmk31, e_aedrof1, e_aepuwk1, e_aepuda1, e_lchmulti1,
                                e_pregout2, e_pregend2, e_pregsmoke2, e_smkmnth12, e_smkmnth22, e_smkmnth32, e_pregsmk12, e_pregsmk22, e_pregsmk32, e_aedrof2, e_aepuwk2, e_aepuda2, e_lchmulti2,
                                e_pregout3, e_pregend3, e_pregsmoke3, e_smkmnth13, e_smkmnth23, e_smkmnth33, e_pregsmk13, e_pregsmk23, e_pregsmk33, e_aedrof3, e_aepuwk3, e_aepuda3, e_lchmulti3,
                                e_nnewborn)
  smoke_vars       <- Hmisc::Cs(e_smever, e_smnow, e_ncigs, e_smcigs, e_smncigs, e_aglquit, e_smagbg)
  alc_vars         <- Hmisc::Cs(e_sceverdrnk, e_scfalcdrnk, e_scalcl7d, e_scnalcl7d, e_scnalcpint, e_scnalcshot, e_scnalcwine, e_scnalcpops)
  weight_vars      <- Hmisc::Cs(e_indinus_lw, e_indinub_xw)


  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars, income_vars, work_vars, employees_vars, s.emp_vars, non.emp_vars, job2_vars, benefits_vars, pension_vars, receivables_vars, hhfinance_vars, education_vars, health_vars, preg_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","e_hidp","e_pno","e_psu","e_strata","e_istrtdaty","e_istrtdatm","e_istrtdatd",
                         ## demographic
                         "e_sex","e_dvage","e_birthy","e_gor_dv","e_urban_dv","e_mlstat","e_marstat",
                         ## previous wave variables
                         "e_notempchk","e_empchk",
                         ## economic status
                         "e_jbstat","e_jbhas","e_jboff","e_jboffy","e_jbterm1","e_jbterm2","e_jbsemp",
                         ## income variables
                         "e_fimnnet_dv", "e_fimngrs_dv",
                         "e_fimnlabnet_dv", "e_fimnmisc_dv", "e_fimnprben_dv", "e_fimninvnet_dv", "e_fimnpen_dv", "e_fimnsben_dv",
                         ## work variables
                         "e_paygu_dv","e_payg_dv","e_jbhrs","e_fimnlabgrs_dv","e_seearngrs_dv","e_jbsic07_cc","e_jbot","e_jbotpd",
                         "e_jbnssec_dv","e_jbnssec3_dv","e_jbnssec5_dv","e_jbnssec8_dv", "e_jbsize",
                         ## employees
                         "e_paygl","e_paynl","e_payu","e_payug","e_ovtpay","e_extnsa","e_extrate","e_extrest","e_basnsa","e_basrate",
                         "e_basrest","e_ovtnsa","e_ovtrate","e_ovtrest",
                         ## self-employed
                         "e_jshrs","e_jspayu","e_jspytx","e_jspyni",
                         ## non-employed
                         "e_jbhad",
                         ## second job
                         "e_j2has","e_j2semp","e_j2hrs","e_j2pay",
                         ## benefits
                         "e_btype1","e_btype2","e_btype3","e_btype4","e_btype5","e_btype6","e_btype7","e_btype8","e_btype9","e_btype96",
                         "e_benunemp1","e_benunemp2","e_benunemp3","e_benunemp96",
                         "e_bendis1","e_bendis11","e_bendis2","e_bendis3","e_bendis4","e_bendis5","e_bendis12",
                         "e_bendis6","e_bendis7","e_bendis8","e_bendis9","e_bendis10","e_bendis96","e_bendis97",
                         ## pensions
                         "e_benpen1","e_benpen2","e_benpen3","e_benpen4","e_benpen5","e_benpen6","e_benpen7","e_benpen8","e_benpen96",
                         ## receivables
                         "e_niserps","e_bencb","e_benctc","e_benfam1","e_benfam2","e_benfam3","e_benfam4","e_benfam5",
                         "e_benfam96","e_bentax1","e_bentax2","e_bentax3","e_bentax4","e_bentax5","e_bentax96","e_benhou1",
                         "e_benhou2","e_benhou3","e_benhou4","e_benhou5","e_benhou96","e_bensta1","e_bensta2","e_bensta3","e_bensta4",
                         "e_bensta5","e_bensta6","e_bensta7","e_bensta96","e_bensta97",
                         ## household finance variables (interest and dividends)
                         "e_fiyrdia","e_fiyrdb1","e_fiyrdb2","e_fiyrdb3","e_fiyrdb4","e_fiyrdb5","e_fiyrdb6","e_finnow","e_finfut",
                         ## education variables
                         "e_hiqual_dv",
                         ## health variables
                         "e_health","e_aidhh","e_sclfsat1","e_sclfsato","e_sf12pcs_dv","e_sf12mcs_dv",
                         "e_scsf1","e_scsf2a","e_scsf2b","e_scsf3a","e_scsf3b","e_scsf4a","e_scsf4b","e_scsf5","e_scsf6a","e_scsf6b","e_scsf6c","e_scsf7",
                         "e_scghq1_dv","e_scghq2_dv",
                         ## pregnancy variables
                         "e_preg","e_pregout1","e_pregend1","e_pregsmoke1","e_smkmnth11","e_smkmnth21","e_smkmnth31","e_pregsmk11","e_pregsmk21","e_pregsmk31","e_aedrof1","e_aepuwk1","e_aepuda1","e_lchmulti1",
                         "e_pregout2","e_pregend2","e_pregsmoke2","e_smkmnth12","e_smkmnth22","e_smkmnth32","e_pregsmk12","e_pregsmk22","e_pregsmk32","e_aedrof2","e_aepuwk2","e_aepuda2","e_lchmulti2",
                         "e_pregout3","e_pregend3","e_pregsmoke3","e_smkmnth13","e_smkmnth23","e_smkmnth33","e_pregsmk13","e_pregsmk23","e_pregsmk33","e_aedrof3","e_aepuwk3","e_aepuda3","e_lchmulti3","e_nnewborn",
                         ## smoking variables
                         "e_smever","e_smnow","e_ncigs","e_smcigs","e_smncigs","e_aglquit","e_smagbg",
                         ## alcohol variables
                         "e_sceverdrnk","e_scfalcdrnk", "e_scalcl7d", "e_scnalcl7d", "e_scnalcpint", "e_scnalcshot", "e_scnalcwine", "e_scnalcpops",
                         ## weight
                         "e_indinus_lw","e_indinub_xw"),

                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "sex","age","birth_year","region","urban","mlstat","marstat",
                         ## previous wave variables
                         "notempchk","empchk",
                         ## economic status
                         "econ_stat","jbhas","jboff","jboffy","jbterm1","jbterm2","jbsemp",
                         ## income variables
                         "fimnnet_dv", "fimngrs_dv",
                         "fimnlabnet_dv", "fimnmisc_dv", "fimnprben_dv", "fimninvnet_dv", "fimnpen_dv", "fimnsben_dv",
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
                         "benunemp1","benunemp2","benunemp3","benunemp96",
                         "bendis1","bendis11","bendis2","bendis3","bendis4","bendis5","bendis12",
                         "bendis6","bendis7","bendis8","bendis9","bendis10","bendis96","bendis97",
                         ## pensions
                         "NI.state_pen","employer_pen","spouse.emp_pen","pencred_pen","prvt_pen","widow_pen","parent_pen","benpen8","non_benpen",
                         ## receivables
                         "income_serps","ben_childben","ben_childtaxcred","benfam_fosterguard","benfam_mat","benfam_alimony","benfam_lone","benfam_fampay",
                         "non_benfam","bentax_work","bentax_council","bentax_pencred","bentax_childtaxcred","bentax_rtw","non_bentax","benhou_house",
                         "benhou_counciltax","benhou_rentreb","benhou_ratereb","benhou5","non_benhou","bensta_prvtpen","bensta_edugrant","bensta_tupay","bensta_alimony",
                         "bensta_fampay","bensta_rentlodge","bensta_rentother","non_bensta","bensta_other",
                         ## household finance variables
                         "fiyrdia","fiyrdb1","fiyrdb2","fiyrdb3","fiyrdb4","fiyrdb5","fiyrdb6","finnow","finfut",
                         ## education variables
                         "highest_qual",
                         ## health variables
                         "lt_sick","caring","health_satisf","life_satisf","sf12_pcs","sf12_mcs",
                         "sf1","sf2a","sf2b","sf3a","sf3b","sf4a","sf4b","sf5","sf6a","sf6b","sf6c","sf7",
                         "ghq1","ghq2",
                         ## pregnancy variables
                         "preg",
                         "pregout1","pregend1","pregsmoke1","smkmnth11","smkmnth21","smkmnth31","pregsmk_ncigs11","pregsmk_ncigs21","pregsmk_ncigs31","pregdrnk_freq1","pregdrnk_unitpw1","pregdrnk_unit1","lchmulti1",
                         "pregout2","pregend2","pregsmoke2","smkmnth12","smkmnth22","smkmnth32","pregsmk_ncigs12","pregsmk_ncigs22","pregsmk_ncigs32","pregdrnk_freq2","pregdrnk_unitpw2","pregdrnk_unit2","lchmulti2",
                         "pregout3","pregend3","pregsmoke3","smkmnth13","smkmnth23","smkmnth33","pregsmk_ncigs13","pregsmk_ncigs23","pregsmk_ncigs33","pregdrnk_freq3","pregdrnk_unitpw3","pregdrnk_unit3","lchmulti3",
                         "nnewborn",
                         ## smoking variables
                         "smever","smnow","ncigs","smcigs","smncigs","aglquit","smagbg",
                         ## alcohol variables
                         "sceverdrnk","scfalcdrnk","scalcl7d", "scnalcl7d", "scnalcpint", "scnalcshot", "scnalcwine", "scnalcpops",
                         ## weight
                         "weight_lw","weight_xw"))

  data[, wave := "UKHLS Wave 5"]
  data[, wave_no := 5]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data[, dataset := "UKHLS"]
  data[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::green("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/e_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(e_hidp, e_tenure_dv, e_nkids_dv, e_hhsize, e_hhtype_dv,
                                   e_nch02_dv, e_nch34_dv, e_nch511_dv, e_nch1215_dv,
                                   e_fihhmngrs1_dv, e_fihhmnlabgrs_dv,
                                   e_fihhmnnet1_dv, e_fihhmnlabnet_dv, e_fihhmnsben_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("e_hidp","e_tenure_dv","e_nkids_dv","e_hhsize","e_hhtype_dv",
                         "e_nch02_dv","e_nch34_dv","e_nch511_dv","e_nch1215_dv",
                         "e_fihhmngrs1_dv", "e_fihhmnlabgrs_dv",
                         "e_fihhmnnet1_dv", "e_fihhmnlabnet_dv", "e_fihhmnsben_dv"),
                       # new names
                       c("hidp","hh_tenure","hh_numchild","hh_size","hh_type",
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

  ####### Combine - keep all observations in the main data and drop excess xwave observations

  xwave_merged <- merge(x = hhold_merged,
                        y = data.xwave,
                        by="pidp",
                        all.x=TRUE,
                        all.y=FALSE)

  ####################################################
  #### ADD IN THE INDALL DATA ########################

  cat(crayon::green("\tIndall..."))

  data.indall <- data.table::fread(
    paste0(path, "/e_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indall_vars  <- colnames(data.indall[ , c("pidp","e_hidp",
                                            "e_imd2019qe_dv","e_imd2017qni_dv",
                                            "e_imd2020qs_dv","e_imd2019qw_dv")])

  data.indall <- data.indall[ , indall_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","e_hidp",
                         "e_imd2019qe_dv","e_imd2017qni_dv",
                         "e_imd2020qs_dv","e_imd2019qw_dv"),
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
