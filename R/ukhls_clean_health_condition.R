#' Clean Health and Caring Variables
#'
#' Produce clean versions of indicator variables for health conditions.
#'
#' @param data Data table. Understanding Society data produced using the read functions.
#'
#' @export
ukhls_clean_health_condition <- function(data = NULL) {

  if("hconds01" %in% colnames(data)) {

  ###################################################################
  ### Create dummies for health conditions (still experiencing) #####

  data[, hcond1_asthma := 0]
  data[hconds01 == 1 | hcondns1 == 1, hcond1_asthma := 1]

  data[, hcond3_congestive_heart_failure := 0]
  data[hconds03 == 1 | hcondns3 == 1, hcond3_congestive_heart_failure := 1]

  data[, hcond4_coronary_heart_disease := 0]
  data[hconds04 == 1 | hcondns4 == 1, hcond4_coronary_heart_disease := 1]

  data[, hcond5_angina := 0]
  data[hconds05 == 1 | hcondns5 == 1, hcond5_angina := 1]

  data[, hcond6_heart_attack := 0]
  data[hcondns6 == 1, hcond6_heart_attack := 1]

  data[, hcond7_stroke := 0]
  data[hcondns7 == 1, hcond7_stroke := 1]

  data[, hcond8_emphysema := 0]
  data[hconds08 == 1 | hcondns8 == 1, hcond8_emphysema := 1]

  data[, hcond10_hypothyroidism := 0]
  data[hconds10 == 1 | hcondns10 == 1, hcond10_hypothyroidism := 1]

  data[, hcond11_chronic_bronchitis := 0]
  data[hconds11 == 1 | hcondns11 == 1, hcond11_chronic_bronchitis := 1]

  data[, hcond12_liver_conditions := 0]
  data[hconds12 == 1 | hcondns12 == 1, hcond12_liver_conditions := 1]

  data[, hcond15_epilepsy := 0]
  data[hconds15 == 1 | hcondns15 == 1, hcond15_epilepsy := 1]

  data[, hcond16_high_blood_pressure := 0]
  data[hconds16 == 1 | hcondns16 == 1, hcond16_high_blood_pressure := 1]

  data[, hcond19_multiple_sclerosis := 0]
  data[hcondns19 == 1, hcond19_multiple_sclerosis := 1]

  data[, hcond21_copd := 0]
  data[hconds21 == 1 | hcondns21 == 1, hcond21_copd := 1]

  data[, hcond23_osteoarthritis := 0]
  data[hconds23 == 1 | hcondns23 == 1, hcond23_osteoarthritis := 1]

  data[, hcond24_rheumatoid_arthritis := 0]
  data[hconds24 == 1 | hcondns24 == 1, hcond24_rheumatoid_arthritis := 1]

  data[, hcond25_other_arthritis := 0]
  data[hconds25 == 1, hcond25_other_arthritis := 1]

  data[, hcond26_cancer_bowel_colorectal := 0]
  data[hconds26 == 1 | hcondns26 == 1, hcond26_cancer_bowel_colorectal := 1]

  data[, hcond27_cancer_lung := 0]
  data[hconds27 == 1 | hcondns27 == 1, hcond27_cancer_lung := 1]

  data[, hcond28_cancer_breast := 0]
  data[hconds28 == 1 | hcondns28 == 1, hcond28_cancer_breast := 1]

  data[, hcond29_cancer_prostate := 0]
  data[hconds29 == 1 | hcondns29 == 1, hcond29_cancer_prostate := 1]

  data[, hcond30_cancer_liver := 0]
  data[hconds30 == 1 | hcondns30 == 1, hcond30_cancer_liver := 1]

  data[, hcond31_cancer_skin := 0]
  data[hconds31 == 1 | hcondns31 == 1, hcond31_cancer_skin := 1]

  data[, hcond32_cancer_other := 0]
  data[hconds32 == 1 , hcond32_cancer_other := 1]

  data[, hcond33_diabetes_type1 := 0]
  data[hconds33 == 1 | hcondns33 == 1, hcond33_diabetes_type1 := 1]

  data[, hcond34_diabetes_type2 := 0]
  data[hconds34 == 1 | hcondns34 == 1, hcond34_diabetes_type2 := 1]

  data[, hcond35_diabetes_gestational := 0]
  data[hconds35 == 1 | hcondns35 == 1, hcond35_diabetes_gestational := 1]

  data[, hcond36_diabetes_other := 0]
  data[hconds36 == 1, hcond36_diabetes_other := 1]

  data[, hcond37_anxiety := 0]
  data[hconds37 == 1 | hcondns37 == 1, hcond37_anxiety := 1]

  data[, hcond38_depression := 0]
  data[hconds38 == 1 | hcondns38 == 1, hcond38_depression := 1]

  data[, hcond39_psychosis_schiz := 0]
  data[hconds39 == 1 | hcondns39 == 1, hcond39_psychosis_schiz := 1]

  data[, hcond40_bipolar_manic_depression := 0]
  data[hconds40 == 1 | hcondns40 == 1, hcond40_bipolar_manic_depression := 1]

  data[, hcond41_eating_disorder := 0]
  data[hconds41 == 1 | hcondns41 == 1, hcond41_eating_disorder := 1]

  data[, hcond42_ptsd := 0]
  data[hconds42 == 1 | hconds42 == 1, hcond42_ptsd := 1]

  data[, hcond43_mental_health_other := 0]
  data[hconds43 == 1, hcond43_mental_health_other := 1]

  #############################################
  ### Aggregate into more detailed groups #####

  ## (1) Respiratory conditions
  data[, condition1_respiratory := 0]
  data[hcond1_asthma == 1 | hcond8_emphysema == 1 |
         hcond11_chronic_bronchitis == 1 | hcond21_copd == 1, condition1_respiratory := 1]

  ## (2) Heart and circulatory conditions
  data[, condition2_circulatory := 0]
  data[hcond3_congestive_heart_failure == 1 | hcond4_coronary_heart_disease == 1 | hcond5_angina == 1 |
         hcond6_heart_attack == 1 | hcond7_stroke == 1 | hcond16_high_blood_pressure == 1, condition2_circulatory := 1]

  ## (3) Musculoskeletal
  data[, condition3_musculoskeletal := 0]
  data[hcond23_osteoarthritis == 1 | hcond24_rheumatoid_arthritis == 1 |
         hcond25_other_arthritis == 1, condition3_musculoskeletal := 1]

  ## (4) Mental health and behavioural
  data[, condition4_mental := 0]
  data[hcond37_anxiety == 1 | hcond38_depression == 1 | hcond39_psychosis_schiz == 1 | hcond40_bipolar_manic_depression == 1 |
         hcond41_eating_disorder == 1 | hcond42_ptsd == 1 | hcond43_mental_health_other == 1, condition4_mental := 1]

  ## (5) Diabetes
  data[, condition5_diabetes := 0]
  data[hcond33_diabetes_type1 == 1 | hcond34_diabetes_type2 == 1 |
         hcond35_diabetes_gestational == 1 | hcond36_diabetes_other == 1, condition5_diabetes := 1]

  ## (6) Cancer
  data[, condition6_cancer := 0]
  data[hcond26_cancer_bowel_colorectal == 1 | hcond27_cancer_lung == 1 | hcond28_cancer_breast == 1 | hcond29_cancer_prostate == 1 |
         hcond30_cancer_liver == 1 | hcond31_cancer_skin == 1 | hcond32_cancer_other == 1, condition6_cancer := 1]

  ## (7) Liver conditions
  data[, condition7_liver := 0]
  data[hcond12_liver_conditions == 1, condition7_liver := 1]

  ## (8) Nervous system
  data[, condition8_nerve := 0]
  data[hcond15_epilepsy == 1 | hcond19_multiple_sclerosis == 1, condition8_nerve := 1]

  ## (9) Endocrine, nutritional, and metabolic
  data[, condition9_metabolic := 0]
  data[hcond10_hypothyroidism == 1, condition9_metabolic := 1]

  } else {

  data[, hcond1_asthma := NA]
  data[, hcond3_congestive_heart_failure := NA]
  data[, hcond4_coronary_heart_disease := NA]
  data[, hcond5_angina := NA]
  data[, hcond6_heart_attack := NA]
  data[, hcond7_stroke := NA]
  data[, hcond8_emphysema := NA]
  data[, hcond10_hypothyroidism := NA]
  data[, hcond11_chronic_bronchitis := NA]
  data[, hcond12_liver_conditions := NA]
  data[, hcond15_epilepsy := NA]
  data[, hcond16_high_blood_pressure := NA]
  data[, hcond19_multiple_sclerosis := NA]
  data[, hcond21_copd := NA]
  data[, hcond23_osteoarthritis := NA]
  data[, hcond24_rheumatoid_arthritis := NA]
  data[, hcond25_other_arthritis := NA]
  data[, hcond26_cancer_bowel_colorectal := NA]
  data[, hcond27_cancer_lung := NA]
  data[, hcond28_cancer_breast := NA]
  data[, hcond29_cancer_prostate := NA]
  data[, hcond30_cancer_liver := NA]
  data[, hcond31_cancer_skin := NA]
  data[, hcond32_cancer_other := NA]
  data[, hcond33_diabetes_type1 := NA]
  data[, hcond34_diabetes_type2 := NA]
  data[, hcond35_diabetes_gestational := NA]
  data[, hcond36_diabetes_other := NA]
  data[, hcond37_anxiety := NA]
  data[, hcond38_depression := NA]
  data[, hcond39_psychosis_schiz := NA]
  data[, hcond40_bipolar_manic_depression := NA]
  data[, hcond41_eating_disorder := NA]
  data[, hcond42_ptsd := NA]
  data[, hcond43_mental_health_other := NA]
  }


  ##################
  ## RETAIN THE CLEANED VARIABLES

  final_data <- data[, c("pidp", "id", "hidp", "wave_no",
                                ## broad conditions
                                "condition1_respiratory","condition2_circulatory","condition3_musculoskeletal",
                                "condition4_mental","condition5_diabetes","condition6_cancer",
                                "condition7_liver","condition8_nerve","condition9_metabolic",
                                ## more detailed conditions
                                "hcond1_asthma","hcond3_congestive_heart_failure","hcond4_coronary_heart_disease",
                                "hcond5_angina","hcond6_heart_attack","hcond7_stroke","hcond8_emphysema","hcond10_hypothyroidism",
                                "hcond11_chronic_bronchitis","hcond12_liver_conditions","hcond15_epilepsy","hcond16_high_blood_pressure",
                                "hcond19_multiple_sclerosis","hcond21_copd","hcond23_osteoarthritis","hcond24_rheumatoid_arthritis","hcond25_other_arthritis",
                                "hcond26_cancer_bowel_colorectal","hcond27_cancer_lung","hcond28_cancer_breast","hcond29_cancer_prostate","hcond30_cancer_liver",
                                "hcond31_cancer_skin","hcond32_cancer_other","hcond33_diabetes_type1","hcond34_diabetes_type2","hcond35_diabetes_gestational",
                                "hcond36_diabetes_other","hcond37_anxiety","hcond38_depression","hcond39_psychosis_schiz","hcond40_bipolar_manic_depression",
                                "hcond41_eating_disorder","hcond42_ptsd","hcond43_mental_health_other")]

  var_names <- c(
    ## broad conditions
    "condition1_respiratory","condition2_circulatory","condition3_musculoskeletal",
    "condition4_mental","condition5_diabetes","condition6_cancer",
    "condition7_liver","condition8_nerve","condition9_metabolic",
    ## more detailed conditions
    "hcond1_asthma","hcond3_congestive_heart_failure","hcond4_coronary_heart_disease",
    "hcond5_angina","hcond6_heart_attack","hcond7_stroke","hcond8_emphysema","hcond10_hypothyroidism",
    "hcond11_chronic_bronchitis","hcond12_liver_conditions","hcond15_epilepsy","hcond16_high_blood_pressure",
    "hcond19_multiple_sclerosis","hcond21_copd","hcond23_osteoarthritis","hcond24_rheumatoid_arthritis","hcond25_other_arthritis",
    "hcond26_cancer_bowel_colorectal","hcond27_cancer_lung","hcond28_cancer_breast","hcond29_cancer_prostate","hcond30_cancer_liver",
    "hcond31_cancer_skin","hcond32_cancer_other","hcond33_diabetes_type1","hcond34_diabetes_type2","hcond35_diabetes_gestational",
    "hcond36_diabetes_other","hcond37_anxiety","hcond38_depression","hcond39_psychosis_schiz","hcond40_bipolar_manic_depression",
    "hcond41_eating_disorder","hcond42_ptsd","hcond43_mental_health_other")

  setnames(final_data, var_names, paste0("h_", var_names))


  return(final_data)

}
