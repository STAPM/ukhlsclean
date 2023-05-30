#' Clean Benefit Variables
#'
#' Clean all variables related to benefits -
#'
#' @param data Data table. Understanding Society data produced using the read functions.
#'
#' @export

ukhls_clean_benefit <- function(data = NULL) {

  data <- data[order(id,wave_no),]

  ######################################################
  #### Setting benefit respondents to "non_receipt" ####

  ### W1-5
  if ("btype96" %in% colnames(data)) {
    data[wave_no %in% c(1:5) & ! is.na(btype96), income_support := "non_receipt"]
    data[wave_no %in% c(1:5) & ! is.na(btype96), jbseek_allowance := "non_receipt"]
    data[wave_no %in% c(1:5) & ! is.na(btype96), national_pen_cred := "non_receipt"]
    data[wave_no %in% c(1:5) & ! is.na(btype96), child_benefit := "non_receipt"]
    data[wave_no %in% c(1:5) & ! is.na(btype96), child_tax_credit := "non_receipt"]
    data[wave_no %in% c(1:5) & ! is.na(btype96), other_family_benefit := "non_receipt"] ### legacy category, keep (?)
    data[wave_no %in% c(1:5) & ! is.na(btype96), housing_council_rent_rate := "non_receipt"] ### legacy category, keep (?)
    data[wave_no %in% c(1:5) & ! is.na(btype96), other_ben_cred := "non_receipt"]

    data[wave_no %in% c(1:5) & ! is.na(btype96), foster_allowance := "non_receipt"]
    data[wave_no %in% c(1:5) & ! is.na(btype96), maternity_allowance := "non_receipt"]
    data[wave_no %in% c(1:5) & ! is.na(btype96), maintenance_alimony := "non_receipt"]
    data[wave_no %in% c(1:5) & ! is.na(btype96), family_payments := "non_receipt"]

    data[wave_no %in% c(1:5) & ! is.na(btype96), NI_state_pension := "non_receipt"]
    data[wave_no %in% c(1:5) & ! is.na(btype96), pension_credit := "non_receipt"]
    data[wave_no %in% c(1:5) & ! is.na(btype96), NI_credit := "non_receipt"]

    data[wave_no %in% c(1:5) & ! is.na(btype96), council_tax_benefit := "non_receipt"]
    data[wave_no %in% c(1:5) & ! is.na(btype96), housing_benefit := "non_receipt"]
    data[wave_no %in% c(1:5) & ! is.na(btype96), rate_rebate := "non_receipt"]
    data[wave_no %in% c(1:5) & ! is.na(btype96), rent_rebate := "non_receipt"]
    data[wave_no %in% c(1:5) & ! is.na(btype96), universal_credit := "non_receipt"]

    data[wave_no %in% c(1:5) & ! is.na(btype96), iw_lone_cred := "non_receipt"]
    data[wave_no %in% c(1:5) & ! is.na(btype96), work_tax_cred := "non_receipt"]
    data[wave_no %in% c(1:5) & ! is.na(btype96), RTW_cred := "non_receipt"]

    data[wave_no %in% c(1:5) & ! is.na(btype96), war_pen := "non_receipt"]
    data[wave_no %in% c(1:5) & ! is.na(btype96), incap_ben := "non_receipt"]
    data[wave_no %in% c(1:5) & ! is.na(btype96), ES_Allowance := "non_receipt"]
    data[wave_no %in% c(1:5) & ! is.na(btype96), sev_disab_allowance := "non_receipt"]
    data[wave_no %in% c(1:5) & ! is.na(btype96), carers_allowance := "non_receipt"]
    data[wave_no %in% c(1:5) & ! is.na(btype96), disliving_allowance := "non_receipt"]
    data[wave_no %in% c(1:5) & ! is.na(btype96), attend_allowance := "non_receipt"]
    data[wave_no %in% c(1:5) & ! is.na(btype96), industry_inj_ben := "non_receipt"]
    data[wave_no %in% c(1:5) & ! is.na(btype96), sick.accident_insurance := "non_receipt"]

    if ("bendis12" %in% colnames(data)) { # W3-12[3:5]
      data[wave_no %in% c(3:5) & ! is.na(btype96), pers.indep_pay := "non_receipt"]
      data[wave_no %in% c(3:5) & ! is.na(btype96), oth_disab_ben := "non_receipt"]
    }
    if (! "bendis12" %in% colnames(data)) {
      data[wave_no %in% c(1,2), pers.indep_pay := NA]
      data[wave_no %in% c(1,2), oth_disab_ben := NA]
    }

    if ("income_serps" %in% colnames(data)) { # W1-11[1:5]
      data[wave_no %in% c(1:5) & ! is.na(btype96), iw_lone_cred := "non_receipt"]
      data[wave_no %in% c(1:5) & ! is.na(btype96), RTW_cred := "non_receipt"]
      data[wave_no %in% c(1:5) & ! is.na(btype96), SERPs := "non_receipt"]
    }
  }
  ### W6-12
  if ("benbase96" %in% colnames(data)) {
    data[wave_no %in% c(6:12) & ! is.na(benbase96), income_support := "non_receipt"]
    data[wave_no %in% c(6:12) & ! is.na(benbase96), jbseek_allowance := "non_receipt"]
    data[wave_no %in% c(6:12) & ! is.na(benbase96), national_pen_cred := "non_receipt"] ##### need to create checkfor others
    data[wave_no %in% c(6:12) & ! is.na(benbase96), child_benefit := "non_receipt"]
    data[wave_no %in% c(6:12) & ! is.na(benbase96), child_tax_credit := "non_receipt"]
    data[wave_no %in% c(6:12) & ! is.na(benbase96), other_family_benefit := "non_receipt"] ### legacy category, keep (?)
    data[wave_no %in% c(6:12) & ! is.na(benbase96), housing_council_rent_rate := "non_receipt"] ### legacy category, keep (?)
    data[wave_no %in% c(6:12) & ! is.na(benbase96), other_ben_cred := "non_receipt"]

    data[wave_no %in% c(6:12) & ! is.na(benbase96), foster_allowance := "non_receipt"]
    data[wave_no %in% c(6:12) & ! is.na(benbase96), maternity_allowance := "non_receipt"]
    data[wave_no %in% c(6:12) & ! is.na(benbase96), maintenance_alimony := "non_receipt"]
    data[wave_no %in% c(6:12) & ! is.na(benbase96), family_payments := "non_receipt"]

    data[wave_no %in% c(6:12) & ! is.na(benbase96), NI_state_pension := "non_receipt"]
    data[wave_no %in% c(6:12) & ! is.na(benbase96), pension_credit := "non_receipt"]
    data[wave_no %in% c(6:12) & ! is.na(benbase96), NI_credits := "non_receipt"]

    data[wave_no %in% c(6:12) & ! is.na(benbase96), council_tax_benefit := "non_receipt"]
    data[wave_no %in% c(6:12) & ! is.na(benbase96), housing_benefit := "non_receipt"]
    data[wave_no %in% c(6:12) & ! is.na(benbase96), rate_rebate := "non_receipt"]
    data[wave_no %in% c(6:12) & ! is.na(benbase96), rent_rebate := "non_receipt"]
    data[wave_no %in% c(6:12) & ! is.na(benbase96), universal_credit := "non_receipt"]

    data[wave_no %in% c(6:12) & ! is.na(benbase96), work_tax_cred := "non_receipt"]

    data[wave_no %in% c(6:12) & ! is.na(benbase96), war_pen := "non_receipt"]
    data[wave_no %in% c(6:12) & ! is.na(benbase96), incap_ben := "non_receipt"]
    data[wave_no %in% c(6:12) & ! is.na(benbase96), ES_Allowance := "non_receipt"]
    data[wave_no %in% c(6:12) & ! is.na(benbase96), sev_disab_allowance := "non_receipt"]
    data[wave_no %in% c(6:12) & ! is.na(benbase96), carers_allowance := "non_receipt"]
    data[wave_no %in% c(6:12) & ! is.na(benbase96), disliving_allowance := "non_receipt"]
    data[wave_no %in% c(6:12) & ! is.na(benbase96), pers.indep_pay := "non_receipt"]
    data[wave_no %in% c(6:12) & ! is.na(benbase96), attend_allowance := "non_receipt"]
    data[wave_no %in% c(6:12) & ! is.na(benbase96), industry_inj_ben := "non_receipt"]
    data[wave_no %in% c(6:12) & ! is.na(benbase96), sick.accident_insurance := "non_receipt"]
    data[wave_no %in% c(6:12) & ! is.na(benbase96), oth_disab_ben := "non_receipt"]

    if ("bendis12" %in% colnames(data)) { # W3-12[6:12]
      data[wave_no %in% c(6:12) & ! is.na(benbase96), pers.indep_pay := "non_receipt"]
      data[wave_no %in% c(6:12) & ! is.na(benbase96), oth_disab_ben := "non_receipt"]
    }

    if ("income_serps" %in% colnames(data)) { # W1-11[6:11]
      data[wave_no %in% c(6:11) & ! is.na(benbase96), iw_lone_cred := "non_receipt"]
      data[wave_no %in% c(6:11) & ! is.na(benbase96), RTW_cred := "non_receipt"]
      data[wave_no %in% c(6:11) & ! is.na(benbase96), SERPs := "non_receipt"]
    }
    if (! "income_serps" %in% colnames(data)) { # W1-11[6:11]
      data[wave_no %in% c(12) & ! is.na(benbase96), iw_lone_cred := NA]
      data[wave_no %in% c(12) & ! is.na(benbase96), RTW_cred := NA]
      data[wave_no %in% c(12) & ! is.na(benbase96), SERPs := NA]
    }
  }


  ######################################################
  #### Waves 1 - 5 initial and transition variables ####


  if ("btype1" %in% colnames(data)) {

    ### Legacy benefits ###
    data[wave_no %in% c(1:5) & btype2 == 1, income_support := "receipt"]
    data[wave_no %in% c(1:5) & btype3 == 1, jbseek_allowance := "receipt"]
    data[wave_no %in% c(1:5) & btype4 == 1, national_pen_cred := "receipt"]
    data[wave_no %in% c(1:5) & (btype5 == 1 | ben_childben == 1 | bentax_childtaxcred == 1), child_benefit := "receipt"]
    data[wave_no %in% c(1:5) & ben_childtaxcred == 1, child_tax_credit := "receipt"]
    # data[wave_no %in% c(1:5) & btype6 == 1, tax_credit := "receipt"]
    data[wave_no %in% c(1:5) & btype7 == 1, other_family_benefit := "receipt"]
    data[wave_no %in% c(1:5) & btype8 == 1, housing_council_rent_rate := "receipt"] ####### different markers
    data[wave_no %in% c(1:5) & btype9 == 1, other_ben_cred := "receipt"] # question doesnt actually mention credits

    ### Allowances ###
    data[wave_no %in% c(1:5) & benfam_fosterguard == 1, foster_allowance := "receipt"] #under btype7 (other_family_benefit)
    data[wave_no %in% c(1:5) & benfam_mat == 1, maternity_allowance := "receipt"]
    data[wave_no %in% c(1:5) & (benfam_alimony == 1 | bensta_alimony == 1), maintenance_alimony := "receipt"]
    data[wave_no %in% c(1:5) & (benfam_fampay == 1 | bensta_fampay == 1), family_payments := "receipt"]

    ### National Insurance ###
    data[wave_no %in% c(1:5) & NI.state_pen == 1, NI_state_pension := "receipt"]
    data[wave_no %in% c(1:5) & (pencred_pen == 1 | bentax_pencred == 1), pension_credit := "receipt"]

    ### Housing benefits + Council tax ###
    data[wave_no %in% c(1:5) & bentax_council == 1, council_tax_benefit := "receipt"]
    data[wave_no %in% c(1:5) & benhou_counciltax == 1, council_tax_benefit := "receipt"] # GB only
    data[wave_no %in% c(1:5) & benhou_house == 1, housing_benefit := "receipt"] # GB only
    data[wave_no %in% c(1:5) & benhou_ratereb == 1, rate_rebate := "receipt"] # NI only
    data[wave_no %in% c(1:5) & benhou_rentreb == 1, rent_rebate := "receipt"] # NI only
    data[wave_no %in% c(1:5) & (benhou_ratereb == 1 | rate_rebate == 1) & benhou_counciltax == 0, council_tax_benefit := NA]
    data[wave_no %in% c(1:5) & (benhou_ratereb == 1 | rate_rebate == 1) & benhou_house == 0, housing_benefit := NA]
    data[wave_no %in% c(1:5) & (benhou_counciltax == 1 | benhou_house == 1) & benhou_ratereb == 0, rate_rebate := NA]
    data[wave_no %in% c(1:5) & (benhou_counciltax == 1 | benhou_house == 1) & benhou_rentreb == 0, rent_rebate := NA]

    ### Universal Credit ###
    if ("benunemp3" %in% colnames(data)) {
      data[wave_no %in% c(3:5) & benunemp3 == 1, universal_credit := "receipt"] # (btype1 == 1 | econ_stat == 3) &
    }
    if ("bendis11" %in% colnames(data)) {
      data[wave_no %in% c(1,2,4,5) & bendis11 == 1, universal_credit := "receipt"] # (btype3 == 1 | ((econ_stat == 8 | lt_sick == 1) & btype3 != 1)) &
    }
    if ("bentax6" %in% colnames(data)) {
      data[wave_no %in% c(3,4) & bentax6 == 1, universal_credit := "receipt"] # btype6 == 1 &
    }
    if ("benhou5" %in% colnames(data)) { # running separately because it crashes in waves without benhou5 variable
      data[wave_no %in% c(3:5) & benhou5 == 1, universal_credit := "receipt"] # btype8 == 1 | (btype8 != 1 & (btype2 == 1 | benunemp1 == 1 | benunemp2 == 1 | bendis1 == 1 | bendis2 == 1 | pencred_pen == 1 | bentax_pencred == 1)) &
    }

    ### Other credits ###
    data[wave_no %in% c(1:5) & benfam_lone == 1, iw_lone_cred := "receipt"]
    data[wave_no %in% c(1:5) & bentax_work == 1, work_tax_cred := "receipt"]
    # national insurance credits?
    data[wave_no %in% c(1:5) & benunemp2 == 1, NI_credit := "receipt"]

  }


  #################################################
  #### Waves 6 - 12 updated benefits variables ####

  if ("benbase1" %in% colnames(data)) {

    ### Legacy benefits ###
    data[wave_no %in% c(6:12) & benbase1 == 1, income_support := "receipt"]
    data[wave_no %in% c(6:12) & benbase2 == 1, jbseek_allowance := "receipt"]
    data[wave_no %in% c(6:12) & benbase3 == 1, child_benefit := "receipt"]
    data[wave_no %in% c(6:12) & benbase3 == 1 & benbase4 != 1 & benctc == 1, child_tax_credit := "receipt"]
    data[wave_no %in% c(6:12) & othben97 == 1, other_ben_cred := "receipt"]
    ## Legacy categories: national_pension; other_family_benefit; housing_council_rent_rate
    data[wave_no %in% c(6:12) & (NI.state_pen == 1 | pencred_pen == 1), national_pension := "receipt"]
    if ("othben3" %in% colnames(data)) {
      data[wave_no %in% c(6:12) & (othben1 == 1 | othben2 == 1 | bensta_alimony == 1 | othben3 == 1 | bensta_fampay == 1), other_family_benefit := "receipt"]
    }
    if (! "othben3" %in% colnames(data)) {
      data[wave_no %in% c(6:12) & (othben1 == 1 | othben2 == 1 | bensta_alimony == 1 | bensta_fampay == 1), other_family_benefit := "receipt"]
    }
    data[wave_no %in% c(6:12) & (othben6 == 1 | othben7 == 1 | othben8 == 1 | othben9 == 1), housing_council_rent_rate := "receipt"]

    ### Allowances ###
    data[wave_no %in% c(6:12) & othben1 == 1, foster_allowance := "receipt"]
    data[wave_no %in% c(6:12) & othben2 == 1, maternity_allowance := "receipt"]
    data[wave_no %in% c(6:12) & bensta_alimony == 1, maintenance_alimony := "receipt"]
    data[wave_no %in% c(1:5) & bensta_fampay == 1, family_payments := "receipt"]

    ### National Insurance ###
    data[wave_no %in% c(1:5) & NI.state_pen == 1, NI_state_pension := "receipt"]
    data[wave_no %in% c(1:5) & pencred_pen == 1, pension_credit := "receipt"]

    ### Housing benefits + Council tax ###
    data[wave_no %in% c(6:12) & othben6 == 1, council_tax_benefit := "receipt"] # GB only
    data[wave_no %in% c(6:12) & othben7 == 1, rate_rebate := "receipt"] # NI only
    data[wave_no %in% c(6:12) & othben8 == 1, housing_benefit := "receipt"] # GB only
    data[wave_no %in% c(6:12) & othben9 == 1, rent_rebate := "receipt"] # NI only
    data[wave_no %in% c(6:12) & (othben7 == 1 | othben9 == 1) & othben6 == 0, council_tax_benefit := NA]
    data[wave_no %in% c(6:12) & (othben7 == 1 | othben9 == 1) & othben8 == 0, housing_benefit := NA]
    data[wave_no %in% c(6:12) & (othben6 == 1 | othben8 == 1) & othben7 == 0, rate_rebate := NA]
    data[wave_no %in% c(6:12) & (othben6 == 1 | othben8 == 1) & othben9 == 0, rent_rebate := NA]

    ### Universal Credit ###
    data[wave_no %in% c(6:12) & benbase4 == 1, universal_credit := "receipt"]

    ### Other credits ###
    if ("othben3" %in% colnames(data)) {
      data[wave_no %in% c(6:11) & othben3 == 1, iw_lone_cred := "receipt"] ## benfam_lone
    }
    if ("othben4" %in% colnames(data)) {
      data[wave_no %in% c(6:11) & othben4 == 1, RTW_cred := "receipt"] ## bentax_rtw # bendis6
    }
    data[wave_no %in% c(6:12) & othben5 == 1, work_tax_cred := "receipt"]
    # National insurance credits?
    data[wave_no %in% c(6:12), NI_credit := NA] ### What is this function meant to do?

  }


  #########################################################
  #### Waves 1 - 12 Disability benefits, Pensions + NI ####

  ### Disability ###
  data[wave_no %in% c(1:12) & bendis1 == 1, incap_ben := "receipt"]
  data[wave_no %in% c(1:12) & bendis10 == 1, sick.accident_insurance := "receipt"]
  data[wave_no %in% c(1:12) & bendis2 == 1, ES_Allowance := "receipt"]
  data[wave_no %in% c(1:12) & bendis3 == 1, sev_disab_allowance := "receipt"]
  data[wave_no %in% c(1:12) & bendis4 == 1, carers_allowance := "receipt"]
  data[wave_no %in% c(1:12) & bendis5 == 1, disliving_allowance := "receipt"]
  data[wave_no %in% c(1:12) & bendis7 == 1, attend_allowance := "receipt"]
  data[wave_no %in% c(1:12) & bendis8 == 1, industry_inj_ben := "receipt"]
  if ("bendis11" %in% colnames(data)) {
    data[wave_no %in% c(1,2,4,5) & bendis11 == 1, universal_credit := "receipt"]
  }
  if ("bendis6" %in% colnames(data)) {
    data[wave_no %in% c(1:5) & (bendis6 == 1 | bentax_rtw == 1), RTW_cred := "receipt"]
    data[wave_no %in% c(1:5) & (bendis9 == 1 | benpen8 == 1), war_pen := "receipt"]
  }
  if ("bendis12" %in% colnames(data)) {
    data[wave_no %in% c(3:12) & bendis12 == 1, pers.indep_pay := "receipt"]
    data[wave_no %in% c(3:12) & bendis97 == 1, oth_disab_ben := "receipt"]
  }

  ### National Insurance ###
  data[wave_no %in% c(1:12) & NI.state_pen == 1, NI_state_pension := "receipt"]
  data[wave_no %in% c(1:12) & pencred_pen == 1, pension_credit := "receipt"]


  ###################################
  #### Waves 1 - 11 income SERPS ####

  if ("income_serps" %in% colnames(data)) {
    data[wave_no %in% c(1:11) & income_serps == 1, SERPs := "receipt"]
  }

  ######################################
  #### Universal Credit - All waves ####

  ### Non-receipt of Universal Credit
  data[universal_credit == "non_receipt", UniCred := "non_receipt"]
  data[income_support == "non_receipt", UniCred := "non_receipt"]
  data[child_tax_credit == "non_receipt", UniCred := "non_receipt"]
  data[work_tax_cred == "non_receipt", UniCred := "non_receipt"]
  data[ES_Allowance == "non_receipt", UniCred := "non_receipt"]
  data[jbseek_allowance == "non_receipt", UniCred := "non_receipt"]
  data[housing_benefit == "non_receipt", UniCred := "non_receipt"]
  data[rate_rebate == "non_receipt", UniCred := "non_receipt"]
  data[rent_rebate == "non_receipt", UniCred := "non_receipt"]

  # data[, UniCred := "non_receipt"]
  data[universal_credit == "receipt", UniCred := "receipt"]
  data[income_support == "receipt", UniCred := "receipt"]
  data[child_tax_credit == "receipt", UniCred := "receipt"]
  data[work_tax_cred == "receipt", UniCred := "receipt"]
  data[ES_Allowance == "receipt", UniCred := "receipt"] #### https://www.gov.uk/employment-support-allowance/eligibility ## income vs support ? ### only UC when conditions: not claiming UC (or any qualifying benefit) AND not income related (contributory style)
  # UniCred := "receipt" ~ ESA == "receipt" & benesa == "income related"
  # UniCred := "receipt" ~ ESA == "receipt" & benesa == "support related" & legacyBenefit == "receipt" - might be worth creating dummy variable to show if ANY legacy benefit is in receipt
  data[jbseek_allowance == "receipt", UniCred := "receipt"]
  # # jbseek_allowance == "receipt" & (income_support == "receipt" | universal_credit == "recept") := UniCred == "receipt"
  # jbseek_allowance == "receipt" & (legacyBenefit == "receipt" | income_support == "receipt"):= UniCred == "receipt"
  # jbseek_allowance == "receipt" & legacyBenefit != "receipt" := UniCred == NA
  data[housing_benefit == "receipt", UniCred := "receipt"] ## Only housing benefit falls under Universal Credit in GB, council tax benefit does not
  data[rate_rebate == "receipt", UniCred := "receipt"] ## Rate and rent rebate in NI falls under Universal Credit
  data[rent_rebate == "receipt", UniCred := "receipt"]

  # legacyBenefit
  # income_support == "receipt" | child_tax_credit == "receipt" | work_tax_cred == "receipt" | benesa == "income-related" !! | jbseek_allowance == "income-related" ?? | housing_benefit == "receipt" | rent_rebate == "receipt" | rate_rebate == "receipt"

  # data[, ]


  ##################me_
  ## RETAIN THE CLEANED VARIABLES

  legacy    <- c("income_support","jbseek_allowance","national_pen_cred","child_benefit",
                 "child_tax_credit","other_family_benefit","housing_council_rent_rate","other_ben_cred")
  bendis    <- c("incap_ben","ES_Allowance","sev_disab_allowance","carers_allowance","disliving_allowance",
                 "pers.indep_pay", "attend_allowance","industry_inj_ben","sick.accident_insurance","oth_disab_ben")
  pension   <- c("war_pen","SERPs")
  national  <- c("NI_state_pension","pension_credit","NI_credit")
  housing   <- c("council_tax_benefit","housing_benefit","rate_rebate","rent_rebate")
  credit    <- c("iw_lone_cred","RTW_cred","work_tax_cred")

  var_names <- c(legacy,
                 bendis,
                 pension,
                 national,
                 housing,
                 credit,
                 "universal_credit",
                 "UniCred"
                 )
  # var_names <- c("income_support","jbseek_allowance","child_benefit","universal_credit","council_tax_benefit","housing_benefit","rate_rebate","rent_rebate","war_pen","RTW_cred")

  ### Factorise variables ###
  #
  # data[, income_support := as.factor(income_support)]
  # data[, jbseek_allowance := as.factor(jbseek_allowance)]
  # data[, child_benefit := as.factor(child_benefit)]
  # data[, universal_credit := as.factor(universal_credit)]
  # data[, council_tax_benefit := as.factor(council_tax_benefit)]
  # data[sapply(data, is.character)]
  #
  data[, (paste0(var_names)) := lapply(.SD, as.factor), .SDcols = paste0(var_names)]

  final_data <- data[, c("pidp", "id", "hidp", "wave_no", ..var_names)]


  setnames(final_data, var_names, paste0("b_", var_names))

  return(final_data)

  }
