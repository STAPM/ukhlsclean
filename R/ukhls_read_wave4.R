#' Read Understanding Society Wave 4
#'
#' Reads and performs basic cleaning operations on the UKHLS fourth wave. Missing values as detailed below are all set to NA.
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
ukhls_read_wave4 <- function(
  root = c("C:/"),
  file = "Users/cm1djm/Documents/Datasets/UKHLS/tab/",
  full = TRUE
) {

  cat(crayon::blue(crayon::underline("\tReading UKHLS Wave 4 datasets")))

  cat(crayon::green("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/d_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[d_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, d_hidp, d_pno, d_psu, d_strata, d_istrtdaty, d_istrtdatm, d_istrtdatd)
  demographic_vars <- Hmisc::Cs(d_sex, d_dvage, d_birthy, d_gor_dv, d_urban_dv, d_mlstat,  d_marstat)
  prev_wave_vars   <- Hmisc::Cs(d_notempchk, d_empchk)
  econ_stat_vars   <- Hmisc::Cs(d_jbstat, d_jbhas, d_jboff, d_jboffy, d_jbterm1, d_jbterm2, d_jbsemp, d_jbpen, d_jbpenm)
  work_vars        <- Hmisc::Cs(d_paygu_dv, d_payg_dv, d_jbhrs, d_fimnlabgrs_dv, d_seearngrs_dv, d_jbsic07_cc, d_jbot, d_jbotpd,
                                d_jbnssec_dv, d_jbnssec3_dv, d_jbnssec5_dv, d_jbnssec8_dv)
  employees_vars   <- Hmisc::Cs(d_paygl, d_paynl, d_payu, d_payug, d_ovtpay, d_extnsa, d_extrate, d_extrest, d_basnsa, d_basrate, d_basrest, d_ovtnsa, d_ovtrate, d_ovtrest)
  s.emp_vars       <- Hmisc::Cs(d_jshrs, d_jspayu, d_jspytx, d_jspyni)
  non.emp_vars     <- Hmisc::Cs(d_jbhad)
  job2_vars        <- Hmisc::Cs(d_j2has, d_j2semp, d_j2hrs, d_j2pay)
  benefits_vars    <- Hmisc::Cs(d_btype1, d_btype2, d_btype3, d_btype4, d_btype5, d_btype6, d_btype7, d_btype8, d_btype9, d_btype96,
                                d_benunemp1, d_benunemp2, d_benunemp3, d_benunemp96, d_bendis1, d_bendis11, d_bendis2, d_bendis3, d_bendis4, d_bendis5, d_bendis12,
                                d_bendis6, d_bendis7, d_bendis8, d_bendis9, d_bendis10, d_bendis96, d_bendis97)
  pension_vars     <- Hmisc::Cs(d_benpen1, d_benpen2, d_benpen3, d_benpen4, d_benpen5, d_benpen6, d_benpen7, d_benpen8, d_benpen96)
  receivables_vars <- Hmisc::Cs(d_niserps, d_bencb, d_benctc, d_benfam1, d_benfam2, d_benfam3, d_benfam4, d_benfam5,
                                d_benfam96, d_bentax1, d_bentax2, d_bentax3, d_bentax4, d_bentax5, d_bentax6, d_bentax96, d_benhou1,
                                d_benhou2, d_benhou3, d_benhou4, d_benhou5, d_benhou96, d_bensta1, d_bensta2, d_bensta3, d_bensta4,
                                d_bensta5, d_bensta6, d_bensta7, d_bensta96, d_bensta97)
  hhfinance_vars   <- Hmisc::Cs(d_fiyrdia, d_fiyrdb1, d_fiyrdb2, d_fiyrdb3, d_fiyrdb4, d_fiyrdb5, d_fiyrdb6, d_finnow, d_finfut)
  education_vars   <- Hmisc::Cs(d_hiqual_dv)
  health_vars      <- Hmisc::Cs(d_health, d_aidhh, d_sclfsat1, d_sclfsato, d_sf12pcs_dv, d_sf12mcs_dv,
                                d_scsf1, d_scsf2a, d_scsf2b, d_scsf3a, d_scsf3b, d_scsf4a, d_scsf4b, d_scsf5, d_scsf6a, d_scsf6b, d_scsf6c, d_scsf7)
  preg_vars        <- Hmisc::Cs(d_preg,
                                d_pregout1, d_pregend1, d_pregsmoke1, d_smkmnth11, d_smkmnth21, d_smkmnth31, d_pregsmk11, d_pregsmk21, d_pregsmk31, d_aedrof1, d_aepuwk1, d_aepuda1, d_lchmulti1,
                                d_pregout2, d_pregend2, d_pregsmoke2, d_smkmnth12, d_smkmnth22, d_smkmnth32, d_pregsmk12, d_pregsmk22, d_pregsmk32, d_aedrof2, d_aepuwk2, d_aepuda2, d_lchmulti2,
                                d_pregout3, d_pregend3, d_pregsmoke3, d_smkmnth13, d_smkmnth23, d_smkmnth33, d_pregsmk13, d_pregsmk23, d_pregsmk33, d_aedrof3, d_aepuwk3, d_aepuda3, d_lchmulti3,
                                d_pregout4, d_pregend4, d_pregsmoke4, d_smkmnth14, d_smkmnth24, d_smkmnth34, d_pregsmk14, d_pregsmk24, d_pregsmk34, d_aedrof4, d_aepuwk4, d_aepuda4, d_lchmulti4,
                                d_nnewborn)
  smoke_vars       <- Hmisc::Cs(d_evrsmo, d_smofrq)
  alc_vars         <- Hmisc::Cs(d_dklm, d_drnk4w, d_evralc, d_fivealcdr)
  weight_vars      <- Hmisc::Cs(d_indinus_lw, d_indinub_xw)


  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars, work_vars, employees_vars, s.emp_vars, non.emp_vars, job2_vars, benefits_vars, pension_vars, receivables_vars, hhfinance_vars, education_vars, health_vars, preg_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","d_hidp","d_pno","d_psu","d_strata","d_istrtdaty","d_istrtdatm","d_istrtdatd",
                         ## demographic
                         "d_sex","d_dvage","d_birthy","d_gor_dv","d_urban_dv","d_mlstat","d_marstat",
                         ## previous wave variables
                         "d_notempchk","d_empchk",
                         ## economic status
                         "d_jbstat","d_jbhas","d_jboff","d_jboffy","d_jbterm1","d_jbterm2","d_jbsemp","d_jbpen","d_jbpenm",
                         ## work variables
                         "d_paygu_dv","d_payg_dv","d_jbhrs","d_fimnlabgrs_dv","d_seearngrs_dv","d_jbsic07_cc","d_jbot","d_jbotpd",
                         "d_jbnssec_dv","d_jbnssec3_dv","d_jbnssec5_dv","d_jbnssec8_dv",
                         ## employees
                         "d_paygl","d_paynl","d_payu","d_payug","d_ovtpay","d_extnsa","d_extrate","d_extrest","d_basnsa","d_basrate",
                         "d_basrest","d_ovtnsa","d_ovtrate","d_ovtrest",
                         ## self-employed
                         "d_jshrs","d_jspayu","d_jspytx","d_jspyni",
                         ## non-employed
                         "d_jbhad",
                         ## second job
                         "d_j2has","d_j2semp","d_j2hrs","d_j2pay",
                         ## benefits
                         "d_btype1","d_btype2","d_btype3","d_btype4","d_btype5","d_btype6","d_btype7","d_btype8","d_btype9","d_btype96",
                         "d_benunemp1","d_benunemp2","d_benunemp3","d_benunemp96","d_bendis1","d_bendis11","d_bendis2","d_bendis3","d_bendis4","d_bendis5","d_bendis12",
                         "d_bendis6","d_bendis7","d_bendis8","d_bendis9","d_bendis10","d_bendis96","d_bendis97",
                         ## pensions
                         "d_benpen1","d_benpen2","d_benpen3","d_benpen4","d_benpen5","d_benpen6","d_benpen7","d_benpen8","d_benpen96",
                         ## receivables
                         "d_niserps","d_bencb","d_benctc","d_benfam1","d_benfam2","d_benfam3","d_benfam4","d_benfam5",
                         "d_benfam96","d_bentax1","d_bentax2","d_bentax3","d_bentax4","d_bentax5","d_bentax6","d_bentax96","d_benhou1",
                         "d_benhou2","d_benhou3","d_benhou4","d_benhou5","d_benhou96","d_bensta1","d_bensta2","d_bensta3","d_bensta4",
                         "d_bensta5","d_bensta6","d_bensta7","d_bensta96","d_bensta97",
                         ## household finance variables (interest and dividends)
                         "d_fiyrdia","d_fiyrdb1","d_fiyrdb2","d_fiyrdb3","d_fiyrdb4","d_fiyrdb5","d_fiyrdb6","d_finnow","d_finfut",
                         ## education variables
                         "d_hiqual_dv",
                         ## health variables
                         "d_health","d_aidhh","d_sclfsat1","d_sclfsato","d_sf12pcs_dv","d_sf12mcs_dv",
                         "d_scsf1","d_scsf2a","d_scsf2b","d_scsf3a","d_scsf3b","d_scsf4a","d_scsf4b","d_scsf5","d_scsf6a","d_scsf6b","d_scsf6c","d_scsf7",
                         ## pregnancy variables
                         "d_preg",
                         "d_pregout1","d_pregend1","d_pregsmoke1","d_smkmnth11","d_smkmnth21","d_smkmnth31","d_pregsmk11","d_pregsmk21","d_pregsmk31","d_aedrof1","d_aepuwk1","d_aepuda1","d_lchmulti1",
                         "d_pregout2","d_pregend2","d_pregsmoke2","d_smkmnth12","d_smkmnth22","d_smkmnth32","d_pregsmk12","d_pregsmk22","d_pregsmk32","d_aedrof2","d_aepuwk2","d_aepuda2","d_lchmulti2",
                         "d_pregout3","d_pregend3","d_pregsmoke3","d_smkmnth13","d_smkmnth23","d_smkmnth33","d_pregsmk13","d_pregsmk23","d_pregsmk33","d_aedrof3","d_aepuwk3","d_aepuda3","d_lchmulti3",
                         "d_pregout4","d_pregend4","d_pregsmoke4","d_smkmnth14","d_smkmnth24","d_smkmnth34","d_pregsmk14","d_pregsmk24","d_pregsmk34","d_aedrof4","d_aepuwk4","d_aepuda4","d_lchmulti4",
                         "d_nnewborn",
                         ## smoking variables
                         "d_evrsmo","d_smofrq",
                         ## alcohol variables
                         "d_dklm","d_drnk4w","d_evralc","d_fivealcdr",
                         ## weight
                         "d_indinus_lw","d_indinub_xw"),

                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "sex","age","birth_year","region","urban","mlstat","marstat",
                         ## previous wave variables
                         "notempchk","empchk",
                         ## economic status
                         "econ_stat","jbhas","jboff","jboffy","jbterm1","jbterm2","jbsemp","jbpen","jbpen_member",
                         ## work variables
                         "grss_pay_usual","grss_pay_last","hours","grss_lab_inc","grss_semp","sic07","ovthours_pw","ovthours_paid",
                         "nssec","nssec_3cat","nssec_5cat","nssec_8cat",
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
                         ## pregnancy variables
                         "preg",
                         "pregout1","pregend1","pregsmoke1","smkmnth11","smkmnth21","smkmnth31","pregsmk_ncigs11","pregsmk_ncigs21","pregsmk_ncigs31","pregdrnk_freq1","pregdrnk_unitpw1","pregdrnk_unit1","lchmulti1",
                         "pregout2","pregend2","pregsmoke2","smkmnth12","smkmnth22","smkmnth32","pregsmk_ncigs12","pregsmk_ncigs22","pregsmk_ncigs32","pregdrnk_freq2","pregdrnk_unitpw2","pregdrnk_unit2","lchmulti2",
                         "pregout3","pregend3","pregsmoke3","smkmnth13","smkmnth23","smkmnth33","pregsmk_ncigs13","pregsmk_ncigs23","pregsmk_ncigs33","pregdrnk_freq3","pregdrnk_unitpw3","pregdrnk_unit3","lchmulti3",
                         "pregout4","pregend4","pregsmoke4","smkmnth14","smkmnth24","smkmnth34","pregsmk_ncigs14","pregsmk_ncigs24","pregsmk_ncigs34","pregdrnk_freq4","pregdrnk_unitpw4","pregdrnk_unit4","lchmulti4",
                         "nnewborn",
                         ## smoking variables
                         "ever_smoked","smoke_freq",
                         ## alcohol variables
                         "dklm","drnk4w","evralc","fivealcdr",
                         ## weight
                         "weight_lw","weight_xw"))

  data[, wave := "UKHLS Wave 4"]
  data[, wave_no := 4]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data[, dataset := "UKHLS"]
  data[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::green("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/d_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(d_hidp, d_tenure_dv, d_numadult, d_nkids015, d_hhsize, d_hhtype_dv,
                                   d_nch02_dv, d_nch34_dv, d_nch511_dv, d_nch1215_dv,
                                   d_fihhmngrs1_dv, d_fihhmnlabgrs_dv,
                                   d_fihhmnnet1_dv, d_fihhmnlabnet_dv, d_fihhmnsben_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("d_hidp","d_tenure_dv","d_numadult","d_nkids015","d_hhsize","d_hhtype_dv",
                         "d_nch02_dv","d_nch34_dv","d_nch511_dv","d_nch1215_dv",
                         "d_fihhmngrs1_dv", "d_fihhmnlabgrs_dv",
                         "d_fihhmnnet1_dv", "d_fihhmnlabnet_dv", "d_fihhmnsben_dv"),
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
    paste0(path, "/d_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indall_vars  <- colnames(data.indall[ , c("pidp","d_hidp",
                                            "d_imd2019qe_dv","d_imd2017qni_dv",
                                            "d_imd2020qs_dv","d_imd2019qw_dv")])

  data.indall <- data.indall[ , indall_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","d_hidp",
                         "d_imd2019qe_dv","d_imd2017qni_dv",
                         "d_imd2020qs_dv","d_imd2019qw_dv"),
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
