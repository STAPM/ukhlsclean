#' Post process of Benefits variables
#'
#' Produce benefits variables specifically for Unemployment (unemployed), In-work (employed) and Disability (inactive)
#'
#' @param data Data table. Understanding Society data produced as part of the ukhls_clean_global function
#'
#' @export


ukhls_post_clean_benefit <- function(data = NULL) {

  data <- data[order(id,wave_no),]

  ### Unemployment benefits
  data[b_jbseek_allowance == "receipt" | (b_UniCred == "receipt" & l_econ_stat_3cat == "unemployed"), unemployment_benefits := "receipt"]
  data[b_jbseek_allowance == "non_receipt" & b_UniCred == "non_receipt", unemployment_benefits := "non_receipt"]
  data[b_jbseek_allowance == "non_receipt" & b_UniCred == "receipt" & l_econ_stat_3cat != "unemployed", unemployment_benefits := "non_receipt"]

  ### In-Work benefits
  data[(b_iw_lone_cred == "receipt" | b_RTW_cred == "receipt" | b_work_tax_cred == "receipt" | b_council_tax == "receipt" | b_rate_rebate == "receipt" | b_housing_benefit == "receipt" | b_rent_rebate == "receipt") | (b_UniCred == "receipt" & l_econ_stat_3cat == "employed"), in_work_benefits := "receipt"]
  data[b_iw_lone_cred != "receipt" & b_RTW_cred != "receipt" & b_work_tax_cred != "receipt" | b_council_tax != "receipt" | b_rate_rebate != "receipt" | b_housing_benefit != "receipt" | b_rent_rebate != "receipt" & b_UniCred == "non_receipt", in_work_benefits := "non_receipt"]
  data[(b_iw_lone_cred != "receipt" & b_RTW_cred != "receipt" & b_work_tax_cred != "receipt" | b_council_tax != "receipt" | b_rate_rebate != "receipt" | b_housing_benefit != "receipt" | b_rent_rebate != "receipt") & b_UniCred == "receipt" & l_econ_stat_3cat != "employed", in_work_benefits := "non_receipt"]
  data[is.na(b_iw_lone_cred) & is.na(b_iw_lone_cred) & is.na(b_work_tax_cred) & is.na(b_council_tax) & is.na(b_rate_rebate) & is.na(b_housing_benefit) & is.na(b_rent_rebate) & is.na(b_UniCred), in_work_benefits := NA]

  ### Disability benefits
  data[(b_income_support == "receipt" | b_ES_Allowance == "receipt" | b_incap_ben == "receipt" | b_sev_disab_allowance == "receipt") | (b_UniCred == "receipt" & l_econ_stat_3cat == "inactive"), econ_inactive_benefits := "receipt"]
  data[(b_income_support == "non_receipt" & b_ES_Allowance == "non_receipt" & b_incap_ben == "non_receipt" & b_sev_disab_allowance == "non_receipt") & b_UniCred == "non_receipt", econ_inactive_benefits := "non_receipt"]
  data[(b_income_support == "non_receipt" & b_ES_Allowance == "non_receipt" & b_incap_ben == "non_receipt" & b_sev_disab_allowance == "non_receipt") & b_UniCred == "receipt" & l_econ_stat_3cat != "inactive", econ_inactive_benefits := "non_receipt"]


}
