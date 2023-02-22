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
  #### Waves 1 - 5 initial and transition variables ####

  if ("btype1" %in% colnames(data)) {

    ### Legacy benefits ###
    data[wave_no %in% c(1:5) & btype2 == 1, income_support := "receipt"]
    data[wave_no %in% c(1:5) & btype3 == 1, jbseek_allowance := "receipt"]
    # data[wave_no %in% c(1:5) & btype4 == 1, pension := "receipt"]
    data[wave_no %in% c(1:5) & (btype5 == 1 | ben_childben == 1), child_benefit := "receipt"]
    # data[wave_no %in% c(1:5) & btype6 == 1, tax_credit := "receipt"]
    data[wave_no %in% c(1:5) & btype7 == 1, other_family_benefit := "receipt"]
    data[wave_no %in% c(1:5) & btype8 == 1, housing_council_rent_rate := "receipt"] ####### different markers
    data[wave_no %in% c(1:5) & btype9 == 1, other_ben_cred := "receipt"] # question doesnt actually mention credits

    data[wave_no %in% c(1:5) & btype96 == 1, income_support := "non_receipt"]
    data[wave_no %in% c(1:5) & btype96 == 1, jbseek_allowance := "non_receipt"]
    data[wave_no %in% c(1:5) & btype96 == 1, child_benefit := "non_receipt"]

    ### Housing benefits + Council tax ###
    data[wave_no %in% c(1:5) & bentax_council == 1, council_tax_benefit := "receipt"]
    data[wave_no %in% c(1:5) & benhou_counciltax == 1, council_tax_benefit := "receipt"] # GB only
    data[wave_no %in% c(1:5) & benhou_house == 1, housing_benefit := "receipt"] # GB only
    data[wave_no %in% c(1:5) & benhou_ratereb == 1, rate_rebate := "receipt"] # NI only
    data[wave_no %in% c(1:5) & benhou_rentreb == 1, rent_rebate := "receipt"] # NI only
    data[wave_no %in% c(1:5) & btype8 == 1 & benhou_counciltax == 0, council_tax_benefit := "non_receipt"]
    data[wave_no %in% c(1:5) & btype8 == 1 & benhou_house == 0, housing_benefit := "non_receipt"]
    data[wave_no %in% c(1:5) & btype8 == 1 & benhou_ratereb == 0, rate_rebate := "non_receipt"]
    data[wave_no %in% c(1:5) & btype8 == 1 & benhou_rentreb == 0, rent_rebate := "non_receipt"]
    data[wave_no %in% c(1:5) & (benhou_ratereb == 1 | rate_rebate == 1) & benhou_counciltax == 0, council_tax_benefit := NA]
    data[wave_no %in% c(1:5) & (benhou_ratereb == 1 | rate_rebate == 1) & benhou_house == 0, housing_benefit := NA]
    data[wave_no %in% c(1:5) & (benhou_counciltax == 1 | benhou_house == 1) & benhou_ratereb == 0, rate_rebate := NA]
    data[wave_no %in% c(1:5) & (benhou_counciltax == 1 | benhou_house == 1) & benhou_rentreb == 0, rent_rebate := NA]

    # data[wave_no %in% c(6:12) & othben97 == 1, other_ben_cred := "receipt"] ######## whats the equivalent to this one ?

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
    data[wave_no %in% c(1:5) & ((btype7 == 1 & benfam_lone != 1) | benfam_lone == 0), iw_lone_cred := "non_receipt"]
    data[wave_no %in% c(1:5) & ((btype6 == 1 & bentax_work != 1) | bentax_work == 0), work_tax_cred := "non_receipt"]

    # non receipt notes
    # eg for bentax1 (working tax credit) if not in receipt of tax credit at all, btype6 == 0 -> NA
    # but for cases were btype6 == 1 and bentax1 == 0 -> "not_receipt" (indicate they are in receipt of credits, just not this particular credit)


    data[, income_support := as.factor(income_support)]
    data[, jbseek_allowance := as.factor(jbseek_allowance)]
    data[, child_benefit := as.factor(child_benefit)]
    data[, universal_credit := as.factor(universal_credit)]
    data[, council_tax_benefit := as.factor(council_tax_benefit)]

  }






  #################################################
  #### Waves 6 - 12 updated benefits variables ####

  if ("benbase1" %in% colnames(data)) {

    ### Legacy benefits ###
    data[wave_no %in% c(6:12) & benbase1 == 1, income_support := "receipt"]
    data[wave_no %in% c(6:12) & benbase2 == 1, jbseek_allowance := "receipt"]
    data[wave_no %in% c(6:12) & benbase3 == 1, child_benefit := "receipt"]
    data[wave_no %in% c(6:12) & benbase3 == 1 & benbase4 != 1 & benctc == 1, child_tax_credit := "receipt"]

    data[wave_no %in% c(6:12) & benbase96 == 1, income_support := "non_receipt"]
    data[wave_no %in% c(6:12) & benbase96 == 1, jbseek_allowance := "non_receipt"]
    data[wave_no %in% c(6:12) & benbase96 == 1, child_benefit := "non_receipt"]

    ### Housing benefits + Council tax ###
    data[wave_no %in% c(6:12) & othben6 == 1, council_tax_benefit := "receipt"] # GB only
    data[wave_no %in% c(6:12) & othben7 == 1, rate_rebate := "receipt"] # NI only
    data[wave_no %in% c(6:12) & othben8 == 1, housing_benefit := "receipt"] # GB only
    data[wave_no %in% c(6:12) & othben9 == 1, rent_rebate := "receipt"] # NI only
    data[wave_no %in% c(6:12) & othben6 == 0, council_tax_benefit := "non_receipt"]
    data[wave_no %in% c(6:12) & othben7 == 0, rate_rebate := "non_receipt"]
    data[wave_no %in% c(6:12) & othben8 == 0, housing_benefit := "non_receipt"]
    data[wave_no %in% c(6:12) & othben9 == 0, rent_rebate := "non_receipt"]
    data[wave_no %in% c(6:12) & (othben7 == 1 | othben9 == 1) & othben6 == 0, council_tax_benefit := NA]
    data[wave_no %in% c(6:12) & (othben7 == 1 | othben9 == 1) & othben8 == 0, housing_benefit := NA]
    data[wave_no %in% c(6:12) & (othben6 == 1 | othben8 == 1) & othben7 == 0, rate_rebate := NA]
    data[wave_no %in% c(6:12) & (othben6 == 1 | othben8 == 1) & othben9 == 0, rent_rebate := NA]


    ### Universal Credit ###
    data[wave_no %in% c(6:12) & benbase4 == 1, universal_credit := "receipt"]
    data[wave_no %in% c(6:12) & benbase96 == 1, universal_credit := "non_receipt"]

    ### Other credits ###
    if ("othben3" %in% colnames(data)) {
      data[wave_no %in% c(6:11) & othben3 == 1, iw_lone_cred := "receipt"] ## benfam_lone
    }
    if ("othben4" %in% colnames(data)) {
      data[wave_no %in% c(6:11) & othben4 == 1, RTW_cred := "receipt"] ## bentax_rtw # bendis6
    }
    data[wave_no %in% c(6:12) & othben5 == 1, work_tax_cred := "receipt"]
    data[wave_no %in% c(6:12) & othben97 == 1, other_ben_cred := "receipt"]

    data[wave_no %in% c(6:12) & othben1 == 8, housing_benefit := "receipt"] # benhou1
    # data[wave_no %in% c(6:12) & othben96 == 1, income_support := "receipt"]



  }


  #####################################################
  #### Waves 1 - 12 Disability benefits + Pensions ####

  ### Disability ###

  if ("bendis11" %in% colnames(data)) {
    data[wave_no %in% c(1,2,4,5) & bendis11 == 1, universal_credit := "receipt"]
  }
  if ("bendis6" %in% colnames(data)) {
    data[wave_no %in% c(1:5) & bendis6 == 1, RTW_cred := "receipt"]
    data[wave_no %in% c(1:5) & bendis6 != 1 & bentax_rtw == 1, RTW_cred := "receipt"]
    data[wave_no %in% c(1:5) & btype6 == 1 & (bendis6 == 0 | bentax_rtw == 0), RTW_cred := "non_receipt"] #btype6 check to ensure only people who checked receiving credits of some form receive non_receipt output
    data[wave_no %in% c(1:5) & bendis9 == 1, war_pen := "receipt"]
    data[wave_no %in% c(1:5) & bendis9 != 1 & benpen8 == 1, war_pen := "receipt"]
    data[wave_no %in% c(1:5) & btype4 == 1 & (bendis9 == 0 | benpen8 == 0), war_pen := "non_receipt"]
  }
  # variable check in wave 1:11
  if (! "income_serps" %in% colnames(data)) {
    data[wave_no %in% c(12), RTW_cred := NA]
    data[wave_no %in% c(12), iw_lone_cred := NA]
  }
  # if (! data$wave_no %in% c(1:11)) {
  #   print(TRUE)
  # }
  # testdata <- data.table(NULL)
  # testdata <- copy(data)
  # ifelse(data$wave_no != c(1:11),
  #        testdata[ , c("wave_no")],
  #        print("nothing"))

  data[wave_no %in% c(1:12), oth_disab_ben := as.character(NA)]
  if ("bendis12" %in% colnames(data)) {
    data[wave_no %in% c(3:12) & bendis12 == 1, pers.indep_pay := "receipt"]
    data[wave_no %in% c(3:12) & bendis97 == 1, oth_disab_ben := "receipt"]
  }

  if ("bendis1" %in% colnames(data)) {
    data[wave_no %in% c(1:12) & bendis1 == 1, incap_ben := "receipt"]
    data[wave_no %in% c(1:12) & bendis10 == 1, sick.accident_insurance := "receipt"]
    data[wave_no %in% c(1:12) & bendis2 == 1, ES_Allowance := "receipt"]
    data[wave_no %in% c(1:12) & bendis3 == 1, sev_disab_allowance := "receipt"]
    data[wave_no %in% c(1:12) & bendis4 == 1, carers_allowance := "receipt"]
    data[wave_no %in% c(1:12) & bendis5 == 1, disliving_allowance := "receipt"]
    data[wave_no %in% c(1:12) & bendis7 == 1, attend_allowance := "receipt"]
    data[wave_no %in% c(1:12) & bendis8 == 1, industry_inj_ben := "receipt"]
    # non-receipt
    {
      data[wave_no %in% c(1:12) & bendis96 == 1, incap_ben := "non_receipt"]
      data[wave_no %in% c(1:12) & bendis96 == 1, ES_Allowance := "non_receipt"]
      data[wave_no %in% c(1:12) & bendis96 == 1, sev_disab_allowance := "non_receipt"]
      data[wave_no %in% c(1:12) & bendis96 == 1, carers_allowance := "non_receipt"]
      data[wave_no %in% c(1:12) & bendis96 == 1, disliving_allowance := "non_receipt"]
      data[wave_no %in% c(3:12) & bendis96 == 1, pers.indep_pay := "non_receipt"]
      data[wave_no %in% c(1:12) & bendis96 == 1, attend_allowance := "non_receipt"]
      data[wave_no %in% c(1:12) & bendis96 == 1, industry_inj_ben := "non_receipt"]
      data[wave_no %in% c(1:12) & bendis96 == 1, sick.accident_insurance := "non_receipt"]
      }
    }

  ### Pensions ###
  # if ("bendis6" %in%)
  if ("benpen8" %in% colnames(data)) {
    data[wave_no %in% c(6:12) & benpen8 == 1, war_pen := "receipt"]
    data[wave_no %in% c(6:12) & benpen8 == 0, war_pen := "non_receipt"]
  }


  ### Credits ###


  ###################################
  #### Waves 1 - 11 income SERPS ####

  if ("income_serps" %in% colnames(data)) {
    data[wave_no %in% c(1:11) & income_serps == 1, SERPS := "receipt"]
  }


  ##################
  ## RETAIN THE CLEANED VARIABLES

  legacy <- c("income_support","jbseek_allowance","other_ben_cred")
  bendis <- c("incap_ben","ES_Allowance","sev_disab_allowance","carers_allowance","disliving_allowance","pers.indep_pay",
              "attend_allowance","industry_inj_ben","sick.accident_insurance","oth_disab_ben")
  housing <- c("council_tax_benefit","housing_benefit","rate_rebate","rent_rebate")
  credit <- c("iw_lone_cred","RTW_cred","work_tax_cred")

  var_names <- c(legacy,
                 bendis,
                 housing,
                 credit,
                 "universal_credit"
                 )
  # var_names <- c("income_support","jbseek_allowance","child_benefit","universal_credit","council_tax_benefit","housing_benefit","rate_rebate","rent_rebate","war_pen","RTW_cred")

  final_data <- data[, c("id", "hidp", "wave_no", ..var_names)]


  setnames(final_data, var_names, paste0("b_", var_names))

  return(final_data)

  }
