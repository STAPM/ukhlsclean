#' Read Understanding Society Wave 3
#'
#' Reads and performs basic cleaning operations on the UKHLS third wave. Missing values as detailed below are all set to NA.
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
ukhls_read_wave3 <- function(
  root = c("X:/"),
  file = "HAR_PR/PR/USoc/Data/SN6614_2024_12_02/tab/ukhls/",
  full = TRUE
) {
  cat(crayon::blue(crayon::underline("\tReading UKHLS Wave 3 datasets")))

  cat(crayon::green("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/c_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[c_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, c_hidp, c_pno, c_psu, c_strata, c_istrtdaty, c_istrtdatm, c_istrtdatd)
  demographic_vars <- Hmisc::Cs(c_sex, c_dvage, c_birthy, c_gor_dv, c_urban_dv, c_mlstat,  c_marstat)
  prev_wave_vars   <- Hmisc::Cs(c_notempchk, c_empchk)
  econ_stat_vars   <- Hmisc::Cs(c_jbstat, c_jbhas, c_jboff, c_jboffy, c_jbterm1, c_jbterm2, c_jbsemp)
  income_vars      <- Hmisc::Cs(c_fimnnet_dv, c_fimngrs_dv,
                                c_fimnlabnet_dv, c_fimnmisc_dv, c_fimnprben_dv, c_fimninvnet_dv, c_fimnpen_dv, c_fimnsben_dv)
  work_vars        <- Hmisc::Cs(c_paygu_dv, c_payg_dv, c_jbhrs, c_fimnlabgrs_dv, c_seearngrs_dv, c_jbsic07_cc, c_jbot, c_jbotpd,
                                c_jbnssec_dv, c_jbnssec3_dv, c_jbnssec5_dv, c_jbnssec8_dv, c_jbsize)
  employees_vars   <- Hmisc::Cs(c_paygl, c_paynl, c_payu, c_payug, c_ovtpay, c_extnsa, c_extrate, c_extrest, c_basnsa, c_basrate, c_basrest, c_ovtnsa, c_ovtrate, c_ovtrest)
  s.emp_vars       <- Hmisc::Cs(c_jshrs, c_jspayu, c_jspytx, c_jspyni)
  non.emp_vars     <- Hmisc::Cs(c_jbhad)
  job2_vars        <- Hmisc::Cs(c_j2has, c_j2semp, c_j2hrs, c_j2pay)
  benefits_vars    <- Hmisc::Cs(c_btype1, c_btype2, c_btype3, c_btype4, c_btype5, c_btype6, c_btype7, c_btype8, c_btype9, c_btype96,
                                c_benunemp1, c_benunemp2, c_benunemp3, c_benunemp96, c_bendis1, c_bendis2, c_bendis3, c_bendis4, c_bendis5, c_bendis6, c_bendis7,
                                c_bendis8, c_bendis9, c_bendis10, c_bendis12, c_bendis96, c_bendis97)
  pension_vars     <- Hmisc::Cs(c_benpen1, c_benpen2, c_benpen3, c_benpen4, c_benpen5, c_benpen6, c_benpen7, c_benpen8, c_benpen96)
  receivables_vars <- Hmisc::Cs(c_niserps, c_bencb, c_benctc, c_benfam1, c_benfam2, c_benfam3, c_benfam4, c_benfam5,
                                c_benfam96, c_bentax1, c_bentax2, c_bentax3, c_bentax4, c_bentax5, c_bentax6, c_bentax96, c_benhou1,
                                c_benhou2, c_benhou3, c_benhou4, c_benhou5, c_benhou96, c_bensta1, c_bensta2, c_bensta3, c_bensta4,
                                c_bensta5, c_bensta6, c_bensta7, c_bensta96, c_bensta97)
  hhfinance_vars   <- Hmisc::Cs(c_fiyrdia, c_fiyrdb1, c_fiyrdb2, c_fiyrdb3, c_fiyrdb4, c_fiyrdb5, c_fiyrdb6, c_finnow, c_finfut)
  education_vars   <- Hmisc::Cs(c_hiqual_dv)
  health_vars      <- Hmisc::Cs(c_health, c_aidhh, c_sclfsat1, c_sclfsato, c_sf12pcs_dv, c_sf12mcs_dv,
                                c_scsf1, c_scsf2a, c_scsf2b, c_scsf3a, c_scsf3b, c_scsf4a, c_scsf4b, c_scsf5, c_scsf6a, c_scsf6b, c_scsf6c, c_scsf7,
                                c_scghq1_dv,c_scghq2_dv)
  preg_vars        <- Hmisc::Cs(c_preg,
                                c_pregout1, c_pregend1, c_pregsmoke1, c_smkmnth11, c_smkmnth21, c_smkmnth31, c_pregsmk11, c_pregsmk21, c_pregsmk31, c_aedrof1, c_aepuwk1, c_aepuda1, c_lchmulti1,
                                c_pregout2, c_pregend2, c_pregsmoke2, c_smkmnth12, c_smkmnth22, c_smkmnth32, c_pregsmk12, c_pregsmk22, c_pregsmk32, c_aedrof2, c_aepuwk2, c_aepuda2, c_lchmulti2,
                                c_pregout3, c_pregend3, c_pregsmoke3, c_smkmnth13, c_smkmnth23, c_smkmnth33, c_pregsmk13, c_pregsmk23, c_pregsmk33, c_aedrof3, c_aepuwk3, c_aepuda3, c_lchmulti3,
                                c_nnewborn)
  smoke_vars       <- Hmisc::Cs(c_evrsmo, c_smofrq)
  alc_vars         <- Hmisc::Cs(c_dklm, c_drnk4w, c_evralc, c_fivealcdr)
  weight_vars      <- Hmisc::Cs(c_indinus_lw, c_indinub_xw)


  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars, income_vars, work_vars, employees_vars, s.emp_vars, non.emp_vars, job2_vars, benefits_vars, pension_vars, receivables_vars, hhfinance_vars, education_vars, health_vars, preg_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","c_hidp","c_pno","c_psu","c_strata","c_istrtdaty","c_istrtdatm","c_istrtdatd",
                         ## demographic
                         "c_sex","c_dvage","c_birthy","c_gor_dv","c_urban_dv","c_mlstat","c_marstat",
                         ## previous wave variables
                         "c_notempchk","c_empchk",
                         ## economic status
                         "c_jbstat","c_jbhas","c_jboff","c_jboffy","c_jbterm1","c_jbterm2","c_jbsemp",
                         ## income variables
                         "c_fimnnet_dv", "c_fimngrs_dv",
                         "c_fimnlabnet_dv", "c_fimnmisc_dv", "c_fimnprben_dv", "c_fimninvnet_dv", "c_fimnpen_dv", "c_fimnsben_dv",
                         ## work variables
                         "c_paygu_dv","c_payg_dv","c_jbhrs","c_fimnlabgrs_dv","c_seearngrs_dv","c_jbsic07_cc","c_jbot","c_jbotpd",
                         "c_jbnssec_dv","c_jbnssec3_dv","c_jbnssec5_dv","c_jbnssec8_dv", "c_jbsize",
                         ## employees
                         "c_paygl","c_paynl","c_payu","c_payug","c_ovtpay","c_extnsa","c_extrate","c_extrest","c_basnsa","c_basrate",
                         "c_basrest","c_ovtnsa","c_ovtrate","c_ovtrest",
                         ## self-employed
                         "c_jshrs","c_jspayu","c_jspytx","c_jspyni",
                         ## non-employed
                         "c_jbhad",
                         ## second job
                         "c_j2has","c_j2semp","c_j2hrs","c_j2pay",
                         ## benefits
                         "c_btype1","c_btype2","c_btype3","c_btype4","c_btype5","c_btype6","c_btype7","c_btype8","c_btype9","c_btype96",
                         "c_benunemp1","c_benunemp2","c_benunemp3","c_benunemp96","c_bendis1","c_bendis2","c_bendis3","c_bendis4","c_bendis5","c_bendis6","c_bendis7",
                         "c_bendis8","c_bendis9","c_bendis10","c_bendis12","c_bendis96","c_bendis97",
                         ## pensions
                         "c_benpen1","c_benpen2","c_benpen3","c_benpen4","c_benpen5","c_benpen6","c_benpen7","c_benpen8","c_benpen96",
                         ## receivables
                         "c_niserps","c_bencb","c_benctc","c_benfam1","c_benfam2","c_benfam3","c_benfam4","c_benfam5",
                         "c_benfam96","c_bentax1","c_bentax2","c_bentax3","c_bentax4","c_bentax5","c_bentax6","c_bentax96","c_benhou1",
                         "c_benhou2","c_benhou3","c_benhou4","c_benhou5","c_benhou96","c_bensta1","c_bensta2","c_bensta3","c_bensta4",
                         "c_bensta5","c_bensta6","c_bensta7","c_bensta96","c_bensta97",
                         ## household finance variables (interest and dividends)
                         "c_fiyrdia","c_fiyrdb1","c_fiyrdb2","c_fiyrdb3","c_fiyrdb4","c_fiyrdb5","c_fiyrdb6","c_finnow","c_finfut",
                         ## education variables
                         "c_hiqual_dv",
                         ## health variables
                         "c_health","c_aidhh","c_sclfsat1","c_sclfsato","c_sf12pcs_dv","c_sf12mcs_dv",
                         "c_scsf1","c_scsf2a","c_scsf2b","c_scsf3a","c_scsf3b","c_scsf4a","c_scsf4b","c_scsf5","c_scsf6a","c_scsf6b","c_scsf6c","c_scsf7",
                         "c_scghq1_dv","c_scghq2_dv",
                         ## pregnancy variables
                         "c_preg","c_pregout1","c_pregend1","c_pregsmoke1","c_smkmnth11","c_smkmnth21","c_smkmnth31","c_pregsmk11","c_pregsmk21","c_pregsmk31","c_aedrof1","c_aepuwk1","c_aepuda1","c_lchmulti1",
                         "c_pregout2","c_pregend2","c_pregsmoke2","c_smkmnth12","c_smkmnth22","c_smkmnth32","c_pregsmk12","c_pregsmk22","c_pregsmk32","c_aedrof2","c_aepuwk2","c_aepuda2","c_lchmulti2",
                         "c_pregout3","c_pregend3","c_pregsmoke3","c_smkmnth13","c_smkmnth23","c_smkmnth33","c_pregsmk13","c_pregsmk23","c_pregsmk33","c_aedrof3","c_aepuwk3","c_aepuda3","c_lchmulti3","c_nnewborn",
                         ## smoking variables
                         "c_evrsmo","c_smofrq",
                         ## alcohol variables
                         "c_dklm","c_drnk4w","c_evralc","c_fivealcdr",
                         ## weight
                         "c_indinus_lw","c_indinub_xw"),

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
                         "benunemp1","benunemp2","benunemp3","benunemp96","bendis1","bendis2","bendis3","bendis4","bendis5","bendis6","bendis7",
                         "bendis8","bendis9","bendis10","bendis12","bendis96","bendis97",
                         ## pensions
                         "NI.state_pen","employer_pen","spouse.emp_pen","pencred_pen","prvt_pen","widow_pen","parent_pen","benpen8","non_benpen",
                         ## receivables
                         "income_serps","ben_childben","ben_childtaxcred","benfam_fosterguard","benfam_mat","benfam_alimony","benfam_lone","benfam_fampay",
                         "non_benfam","bentax_work","bentax_council","bentax_pencred","bentax_childtaxcred","bentax_rtw","bentax6","non_bentax","benhou_house",
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
                         "ever_smoked","smoke_freq",
                         ## alcohol variables
                         "dklm","drnk4w","evralc","fivealcdr",
                         ## weight
                         "weight_lw","weight_xw"))

  data[, wave := "UKHLS Wave 3"]
  data[, wave_no := 3]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data[, dataset := "UKHLS"]
  data[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::green("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/c_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(c_hidp, c_tenure_dv, c_numadult, c_nkids015, c_hhsize, c_hhtype_dv,
                                   c_nch02_dv, c_nch34_dv, c_nch511_dv, c_nch1215_dv,
                                   c_fihhmngrs1_dv, c_fihhmnlabgrs_dv,
                                   c_fihhmnnet1_dv, c_fihhmnlabnet_dv, c_fihhmnsben_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("c_hidp","c_tenure_dv","c_numadult","c_nkids015","c_hhsize","c_hhtype_dv",
                         "c_nch02_dv","c_nch34_dv","c_nch511_dv","c_nch1215_dv",
                         "c_fihhmngrs1_dv", "c_fihhmnlabgrs_dv",
                         "c_fihhmnnet1_dv", "c_fihhmnlabnet_dv", "c_fihhmnsben_dv"),
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
    paste0(path, "/c_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indall_vars  <- colnames(data.indall[ , c("pidp","c_hidp",
                                            "c_imd2019qe_dv","c_imd2017qni_dv",
                                            "c_imd2020qs_dv","c_imd2019qw_dv")])

  data.indall <- data.indall[ , indall_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","c_hidp",
                         "c_imd2019qe_dv","c_imd2017qni_dv",
                         "c_imd2020qs_dv","c_imd2019qw_dv"),
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
