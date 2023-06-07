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
  data[b_jbseek_allowance == "receipt" | (b_UniCred == "receipt" & l_econ_stat_3cat == "unemployed"), unemployment_benefits := "receipt"] # conditions for setting receipt
  data[b_jbseek_allowance != "receipt" & b_UniCred != "receipt", unemployment_benefits := "non_receipt"] # not in receipt conditions
  data[b_jbseek_allowance != "receipt" & b_UniCred == "receipt" & l_econ_stat_3cat != "unemployed", unemployment_benefits := "non_receipt"] # receipt of UniCred but not the relevant employment status
  data[is.na(b_jbseek_allowance) & is.na(b_UniCred), unemployment_benefits := NA] # generate NA values if ALL are missing

  ### In-Work benefits (can use b_housing_council_rent_rate instead)
  data[(b_iw_lone_cred == "receipt" | b_RTW_cred == "receipt" | b_work_tax_cred == "receipt" | b_council_tax_benefit == "receipt" | b_rate_rebate == "receipt" | b_housing_benefit == "receipt" | b_rent_rebate == "receipt") | (b_UniCred == "receipt" & l_econ_stat_3cat == "employed"), in_work_benefits := "receipt"]
  data[b_iw_lone_cred != "receipt" & b_RTW_cred != "receipt" & b_work_tax_cred != "receipt" | b_council_tax_benefit != "receipt" | b_rate_rebate != "receipt" | b_housing_benefit != "receipt" | b_rent_rebate != "receipt" & b_UniCred == "non_receipt", in_work_benefits := "non_receipt"]
  data[(b_iw_lone_cred != "receipt" & b_RTW_cred != "receipt" & b_work_tax_cred != "receipt" | b_council_tax_benefit != "receipt" | b_rate_rebate != "receipt" | b_housing_benefit != "receipt" | b_rent_rebate != "receipt") & b_UniCred == "receipt" & l_econ_stat_3cat != "employed", in_work_benefits := "non_receipt"]
  data[is.na(b_iw_lone_cred) & is.na(b_iw_lone_cred) & is.na(b_work_tax_cred) & is.na(b_council_tax_benefit) & is.na(b_rate_rebate) & is.na(b_housing_benefit) & is.na(b_rent_rebate) & is.na(b_UniCred), in_work_benefits := NA]

  ### Disability benefits
  data[(b_income_support == "receipt" | b_ES_Allowance == "receipt" | b_incap_ben == "receipt" | b_sev_disab_allowance == "receipt") | (b_UniCred == "receipt" & l_econ_stat_3cat == "inactive"), econ_inactive_benefits := "receipt"]
  data[(b_income_support != "receipt" & b_ES_Allowance != "receipt" & b_incap_ben != "receipt" & b_sev_disab_allowance != "receipt") & b_UniCred != "receipt", econ_inactive_benefits := "non_receipt"]
  data[(b_income_support != "receipt" & b_ES_Allowance != "receipt" & b_incap_ben != "receipt" & b_sev_disab_allowance != "receipt") & b_UniCred == "receipt" & l_econ_stat_3cat != "inactive", econ_inactive_benefits := "non_receipt"]
  data[is.na(b_income_support) & is.na(b_ES_Allowance) & is.na(b_incap_ben) & is.na(b_sev_disab_allowance) & is.na(b_UniCred), econ_inactive_benefits := NA]

  return(data)
}
