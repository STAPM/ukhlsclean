#' Read Understanding Society Wave 2
#'
#' Reads and does basic cleaning on the UKHLS second wave.
#'
#' MISSING VALUES
#'
#' \itemize{
#' \item -1 Don't know.
#' \item -2 Refused: Used only for variables on the nurse schedules, this code indicates that a
#' respondent refused a particular measurement or test or the measurement was attempted but not
#' obtained or not attempted.
#' \item -8 Not applicable: Used to signify that a particular variable did not apply to a given respondent
#' usually because of internal routing. For example, men in women only questions.
#' \item -9 Missing
#' }
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
ukhls_read_wave2 <- function(
  root = c("C:/"),
  file = "Users/cm1djm/Documents/Datasets/UKHLS/tab/",
  full = TRUE
) {

  cat(crayon::magenta("\tReading UKHLS Wave 2 datasets"))

  cat(crayon::red("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/b_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[b_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, b_hidp, b_pno, b_psu, b_strata, b_istrtdaty, b_istrtdatm, b_istrtdatd)
  demographic_vars <- Hmisc::Cs(b_sex, b_dvage, b_birthy, b_gor_dv, b_urban_dv, b_mlstat, b_marstat)
  prev_wave_vars   <- Hmisc::Cs(b_notempchk, b_empchk)
  econ_stat_vars   <- Hmisc::Cs(b_jbstat, b_jbhas, b_jboff, b_jboffy, b_jbterm1, b_jbterm2, b_jbsemp, b_jbpen, b_jbpenm)
  work_vars        <- Hmisc::Cs(b_paygu_dv, b_payg_dv, b_jbhrs, b_fimnlabgrs_dv, b_seearngrs_dv, b_jbsic07_cc, b_jbot, b_jbotpd)
  employees_vars   <- Hmisc::Cs(b_paygl, b_paynl, b_payu, b_payug, b_ovtpay, b_extnsa, b_extrate, b_extrest, b_basnsa, b_basrate, b_basrest, b_ovtnsa, b_ovtrate, b_ovtrest)
  s.emp_vars       <- Hmisc::Cs(b_jshrs, b_jspayu, b_jspytx, b_jspyni)
  non.emp_vars     <- Hmisc::Cs(b_jbhad)
  job2_vars        <- Hmisc::Cs(b_j2has, b_j2semp, b_j2hrs, b_j2pay)
  benefits_vars    <- Hmisc::Cs(b_btype1, b_btype2, b_btype3, b_btype4, b_btype5, b_btype6, b_btype7, b_btype8, b_btype9, b_btype96,
                                b_benunemp1, b_benunemp2, b_benunemp96, b_bendis1, b_bendis2, b_bendis3, b_bendis4, b_bendis5, b_bendis6, b_bendis7,
                                b_bendis8, b_bendis9, b_bendis10, b_bendis11, b_bendis96)
  pension_vars     <- Hmisc::Cs(b_benpen1, b_benpen2, b_benpen3, b_benpen4, b_benpen5, b_benpen6, b_benpen7, b_benpen8, b_benpen96)
  receivables_vars <- Hmisc::Cs(b_niserps, b_bencb, b_benctc, b_benfam1, b_benfam2, b_benfam3, b_benfam4, b_benfam5,
                                b_benfam96, b_bentax1, b_bentax2, b_bentax3, b_bentax4, b_bentax5, b_bentax96, b_benhou1,
                                b_benhou2, b_benhou3, b_benhou4, b_benhou96, b_bensta1, b_bensta2, b_bensta3, b_bensta4,
                                b_bensta5, b_bensta6, b_bensta7, b_bensta8, b_bensta96)
  hhfinance_vars   <- Hmisc::Cs(b_fiyrdia, b_fiyrdb1, b_fiyrdb2, b_fiyrdb3, b_fiyrdb4, b_fiyrdb5, b_fiyrdb6, b_finnow, b_finfut)
  education_vars   <- Hmisc::Cs(b_hiqual_dv)
  health_vars      <- Hmisc::Cs(b_health, b_aidhh, b_sclfsat1, b_sclfsato, b_sf12pcs_dv, b_sf12mcs_dv,
                                b_scsf1, b_scsf2a, b_scsf2b, b_scsf3a, b_scsf3b, b_scsf4a, b_scsf4b, b_scsf5, b_scsf6a, b_scsf6b, b_scsf6c, b_scsf7)
  preg_vars        <- Hmisc::Cs(b_preg, b_pregout1, b_pregend1, b_pregsmoke1, b_smkmnth11, b_smkmnth21, b_smkmnth31, b_pregsmk11, b_pregsmk21, b_pregsmk31, b_pregdrink1, b_lchmulti1,
                                b_pregout2, b_pregend2, b_pregsmoke2, b_smkmnth12, b_smkmnth22, b_smkmnth32, b_pregsmk12, b_pregsmk22, b_pregsmk32, b_pregdrink2, b_lchmulti2,
                                b_pregout3, b_pregend3, b_pregsmoke3, b_smkmnth13, b_smkmnth23, b_smkmnth33, b_pregsmk13, b_pregsmk23, b_pregsmk33, b_pregdrink3, b_lchmulti3,
                                b_pregout4, b_pregend4, b_pregsmoke4, b_smkmnth14, b_smkmnth24, b_smkmnth34, b_pregsmk14, b_pregsmk24, b_pregsmk34, b_pregdrink4, b_lchmulti4,
                                b_pregout5, b_pregend5, b_pregsmoke5, b_smkmnth15, b_smkmnth25, b_smkmnth35, b_pregsmk15, b_pregsmk25, b_pregsmk35, b_pregdrink5, b_lchmulti5, b_nnewborn)
  smoke_vars       <- Hmisc::Cs(b_smever, b_smnow, b_ncigs, b_smcigs, b_smncigs, b_aglquit, b_smagbg)
  alc_vars         <- Hmisc::Cs(b_sceverdrnk, b_scfalcdrnk, b_scalcl7d, b_scnalcl7d, b_scnalcpint, b_scnalcshot, b_scnalcwine, b_scnalcpops)
  weight_vars      <- Hmisc::Cs(b_indinus_lw, b_indinub_xw)



  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars, work_vars, employees_vars, s.emp_vars, non.emp_vars, job2_vars, benefits_vars, pension_vars, receivables_vars, hhfinance_vars, education_vars, health_vars, preg_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","b_hidp","b_pno","b_psu","b_strata","b_istrtdaty","b_istrtdatm","b_istrtdatd",
                         ## demographic
                         "b_sex","b_dvage","b_birthy","b_gor_dv","b_urban_dv","b_mlstat","b_marstat",
                         ## previous wave variables
                         "b_notempchk","b_empchk",
                         ## economic status
                         "b_jbstat","b_jbhas","b_jboff","b_jboffy","b_jbterm1","b_jbterm2","b_jbsemp","b_jbpen","b_jbpenm",
                         ## work variables
                         "b_paygu_dv","b_payg_dv","b_jbhrs","b_fimnlabgrs_dv","b_seearngrs_dv","b_jbsic07_cc","b_jbot","b_jbotpd",
                         ## employees
                         "b_paygl","b_paynl","b_payu","b_payug","b_ovtpay","b_extnsa","b_extrate","b_extrest","b_basnsa","b_basrate",
                         "b_basrest","b_ovtnsa","b_ovtrate","b_ovtrest",
                         ## self-employed
                         "b_jshrs","b_jspayu","b_jspytx","b_jspyni",
                         ## non-employed
                         "b_jbhad",
                         ## second job
                         "b_j2has","b_j2semp","b_j2hrs","b_j2pay",
                         ## benefits
                         "b_btype1","b_btype2","b_btype3","b_btype4","b_btype5","b_btype6","b_btype7","b_btype8","b_btype9","b_btype96",
                         "b_benunemp1","b_benunemp2","b_benunemp96","b_bendis1","b_bendis2","b_bendis3","b_bendis4","b_bendis5","b_bendis6","b_bendis7",
                         "b_bendis8","b_bendis9","b_bendis10","b_bendis11","b_bendis96",
                         ## pensions
                         "b_benpen1","b_benpen2","b_benpen3","b_benpen4","b_benpen5","b_benpen6","b_benpen7","b_benpen8","b_benpen96",
                         ## receivables
                         "b_niserps","b_bencb","b_benctc","b_benfam1","b_benfam2","b_benfam3","b_benfam4","b_benfam5",
                         "b_benfam96","b_bentax1","b_bentax2","b_bentax3","b_bentax4","b_bentax5","b_bentax96","b_benhou1",
                         "b_benhou2","b_benhou3","b_benhou4","b_benhou96","b_bensta1","b_bensta2","b_bensta3","b_bensta4",
                         "b_bensta5","b_bensta6","b_bensta7","b_bensta8","b_bensta96",
                         ## household finance variables (interest and dividends)
                         "b_fiyrdia","b_fiyrdb1","b_fiyrdb2","b_fiyrdb3","b_fiyrdb4","b_fiyrdb5","b_fiyrdb6","b_finnow","b_finfut",
                         ## education variables
                         "b_hiqual_dv",
                         ## health variables
                         "b_health","b_aidhh","b_sclfsat1","b_sclfsato","b_sf12pcs_dv","b_sf12mcs_dv",
                         "b_scsf1","b_scsf2a","b_scsf2b","b_scsf3a","b_scsf3b","b_scsf4a","b_scsf4b","b_scsf5","b_scsf6a","b_scsf6b","b_scsf6c","b_scsf7",
                         ## pregnancy variables
                         "b_preg","b_pregout1","b_pregend1","b_pregsmoke1","b_smkmnth11","b_smkmnth21","b_smkmnth31","b_pregsmk11","b_pregsmk21","b_pregsmk31","b_pregdrink1","b_lchmulti1",
                         "b_pregout2","b_pregend2","b_pregsmoke2","b_smkmnth12","b_smkmnth22","b_smkmnth32","b_pregsmk12","b_pregsmk22","b_pregsmk32","b_pregdrink2","b_lchmulti2",
                         "b_pregout3","b_pregend3","b_pregsmoke3","b_smkmnth13","b_smkmnth23","b_smkmnth33","b_pregsmk13","b_pregsmk23","b_pregsmk33","b_pregdrink3","b_lchmulti3",
                         "b_pregout4","b_pregend4","b_pregsmoke4","b_smkmnth14","b_smkmnth24","b_smkmnth34","b_pregsmk14","b_pregsmk24","b_pregsmk34","b_pregdrink4","b_lchmulti4",
                         "b_pregout5","b_pregend5","b_pregsmoke5","b_smkmnth15","b_smkmnth25","b_smkmnth35","b_pregsmk15","b_pregsmk25","b_pregsmk35","b_pregdrink5","b_lchmulti5","b_nnewborn",
                         ## smoking variables
                         "b_smever","b_smnow","b_ncigs","b_smcigs","b_smncigs","b_aglquit","b_smagbg",
                         ## alcohol variables
                         "b_sceverdrnk","b_scfalcdrnk", "b_scalcl7d", "b_scnalcl7d", "b_scnalcpint", "b_scnalcshot", "b_scnalcwine", "b_scnalcpops",
                         ## weight
                         "b_indinus_lw","b_indinub_xw"),

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
                         "last_gross_pay","last_net_pay","usual_pay","payug","ovtpay","extnsa","extrate","ext_estimate","baspay_amount","baspay_rate",
                         "baspay_estimate","ovtpay_amount","ovtpay_rate","ovtpay_estimate",
                         ## self-employed
                         "s.emp_hours","s.emp_pay","s.emp_pay_pretax","s.emp_pay_preNI",
                         ## non-employed
                         "jbhad",
                         ## second job
                         "2ndjb","2ndjb_s.emp","2ndjb_hours","2ndjob_pay",
                         ## benefits
                         "unemp_ben","incomesupp_ben","sickdis_ben","pension_ben","child_ben","taxcred_ben","family_ben","counciltax_ben","otherstate_ben","no_ben",
                         "jbseek_allowance","NI_credits","non_btype1","incap_ben","empsupport_allowance","severedisab_allowance","carers_allowance","disliving_allowance","RTW_credit","attend_allowance",
                         "injury_ben","war_pension","sick.accident_insurance","otherdis_pay","non_bendis",
                         ## pensions
                         "NI.state_pen","employer_pen","spouse.emp_pen","pencred_pen","prvt_pen","widow_pen","parent_pen","war_pen","non_benpen",
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
                         ## pregnancy variables
                         "preg","pregout1","pregend1","pregsmoke1","smkmnth11","smkmnth21","smkmnth31","pregsmk_ncigs11","pregsmk_ncigs21","pregsmk_ncigs31","pregdrink1","lchmulti1",
                         "pregout2","pregend2","pregsmoke2","smkmnth12","smkmnth22","smkmnth32","pregsmk_ncigs12","pregsmk_ncigs22","pregsmk_ncigs32","pregdrink2","lchmulti2",
                         "pregout3","pregend3","pregsmoke3","smkmnth13","smkmnth23","smkmnth33","pregsmk_ncigs13","pregsmk_ncigs23","pregsmk_ncigs33","pregdrink3","lchmulti3",
                         "pregout4","pregend4","pregsmoke4","smkmnth14","smkmnth24","smkmnth34","pregsmk_ncigs14","pregsmk_ncigs24","pregsmk_ncigs34","pregdrink4","lchmulti4",
                         "pregout5","pregend5","pregsmoke5","smkmnth15","smkmnth25","smkmnth35","pregsmk_ncigs15","pregsmk_ncigs25","pregsmk_ncigs35","pregdrink5","lchmulti5","nnewborn",
                         ## smoking variables
                         "smever","smnow","ncigs","smcigs","smncigs","aglquit","smagbg",
                         ## alcohol variables
                         "sceverdrnk","scfalcdrnk","scalcl7d", "scnalcl7d", "scnalcpint", "scnalcshot", "scnalcwine", "scnalcpops",
                         ## weight
                         "weight_lw","weight_xw"))

  data[, wave := "UKHLS Wave 2"]
  data[, wave_no := 2]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data[, dataset := "UKHLS"]
  data[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::red("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/b_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(b_hidp, b_tenure_dv, b_numadult, b_nkids015, b_hhsize, b_hhtype_dv,
                                   b_nch02_dv, b_nch34_dv, b_nch511_dv, b_nch1215_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("b_hidp","b_tenure_dv","b_numadult","b_nkids015","b_hhsize","b_hhtype_dv",
                         "b_nch02_dv","b_nch34_dv","b_nch511_dv","b_nch1215_dv"),
                       # new names
                       c("hidp","hh_tenure","hh_numadult","hh_numchild","hh_size","hh_type",
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
