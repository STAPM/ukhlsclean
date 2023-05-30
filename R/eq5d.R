#' EQ-5D Algorithm
#'
#' Apply the algorithm developed by Gray A, Rivero-Arias O, Clarke P (2006) to estimate
#' EQ-5D utility values from SF-12 responses. Algorithm materials obtained from
#' \href{https://www.herc.ox.ac.uk/downloads/downloads-supporting-material-1/sf-12-responses-and-eq-5d-utility-values}{SF-12 Responses and EQ-5D Utility Values}
#'
#' @param data Data table. UKHLS data containing SF-12 variables.
#' @param matrix Matrix. Matrix of coefficients to use in the algorithm
#' @param seed Integer. Random number seed
#'
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
#'
eq5d <- function(data,
                 matrix = ukhlsclean::CoefficientMatrix,
                 seed = 0) {

  health_util_data <- copy(data)

  ##############################################
  ### Retain only complete cases of SF-12 inputs

  complete_vars = c("sfstat", "sfmode", "sfstaira", "sfless", "sflimit", "sflesse",
                    "sfcarful", "sfpainb", "sfcalm", "sfener", "sflow", "sfvisit")

  for(cv in complete_vars) {

    health_util_data <- health_util_data[!is.na(get(cv))]

  }

  ##############################################
  ### Prepare dummy variables

  health_util_data[, sfstat1 := ifelse(sfstat == 1, 1, 0)]
  health_util_data[, sfstat2 := ifelse(sfstat == 2, 1, 0)]
  health_util_data[, sfstat3 := ifelse(sfstat == 3, 1, 0)]
  health_util_data[, sfstat4 := ifelse(sfstat == 4, 1, 0)]

  health_util_data[, sfmode1 := ifelse(sfmode == 1, 1, 0)]
  health_util_data[, sfmode2 := ifelse(sfmode == 2, 1, 0)]

  health_util_data[, sfstaira1 := ifelse(sfstaira == 1, 1, 0)]
  health_util_data[, sfstaira2 := ifelse(sfstaira == 2, 1, 0)]

  health_util_data[, sfless1 := ifelse(sfless == 1, 1, 0)]

  health_util_data[, sflimit1 := ifelse(sflimit == 1, 1, 0)]

  health_util_data[, sflesse1 := ifelse(sflesse == 1, 1, 0)]

  health_util_data[, sfcarful1 := ifelse(sfcarful == 1, 1, 0)]

  health_util_data[, sfpainb1 := ifelse(sfpainb == 1, 1, 0)]
  health_util_data[, sfpainb2 := ifelse(sfpainb == 2, 1, 0)]
  health_util_data[, sfpainb3 := ifelse(sfpainb == 3, 1, 0)]
  health_util_data[, sfpainb4 := ifelse(sfpainb == 4, 1, 0)]

  health_util_data[, sfcalm1 := ifelse(sfcalm == 1, 1, 0)]
  health_util_data[, sfcalm2 := ifelse(sfcalm == 2, 1, 0)]
  health_util_data[, sfcalm3 := ifelse(sfcalm == 3, 1, 0)]
  health_util_data[, sfcalm4 := ifelse(sfcalm == 4, 1, 0)]
  health_util_data[, sfcalm5 := ifelse(sfcalm == 5, 1, 0)]

  health_util_data[, sfener1 := ifelse(sfener == 1, 1, 0)]
  health_util_data[, sfener2 := ifelse(sfener == 2, 1, 0)]
  health_util_data[, sfener3 := ifelse(sfener == 3, 1, 0)]
  health_util_data[, sfener4 := ifelse(sfener == 4, 1, 0)]
  health_util_data[, sfener5 := ifelse(sfener == 5, 1, 0)]

  health_util_data[, sflow1 := ifelse(sflow == 1, 1, 0)]
  health_util_data[, sflow2 := ifelse(sflow == 2, 1, 0)]
  health_util_data[, sflow3 := ifelse(sflow == 3, 1, 0)]
  health_util_data[, sflow4 := ifelse(sflow == 4, 1, 0)]
  health_util_data[, sflow5 := ifelse(sflow == 5, 1, 0)]

  health_util_data[, sfvisit1 := ifelse(sfvisit == 1, 1, 0)]
  health_util_data[, sfvisit2 := ifelse(sfvisit == 2, 1, 0)]
  health_util_data[, sfvisit3 := ifelse(sfvisit == 3, 1, 0)]
  health_util_data[, sfvisit4 := ifelse(sfvisit == 4, 1, 0)]

  ##########################################################
  ##### Generate probabilities of being in each EQ5D domain

  set.seed(seed)
  m <- copy(matrix)

  ###############
  ## Mobility ###

  health_util_data[, M2XB := (m[1,"m2"]*sfstat1   + m[2,"m2"]*sfstat2 + m[3,"m2"]*sfstat3 + m[4,"m2"]*sfstat4 +
                                m[5,"m2"]*sfmode1   + m[6,"m2"]*sfmode2 +
                                m[7,"m2"]*sfstaira1 + m[8,"m2"]*sfstaira2 +
                                m[9,"m2"]*sfless1 +
                                m[10,"m2"]*sflimit1 +
                                m[11,"m2"]*sflesse1 +
                                m[12,"m2"]*sfcarful1 +
                                m[13,"m2"]*sfpainb1  + m[14,"m2"]*sfpainb2 + m[15,"m2"]*sfpainb3 + m[16,"m2"]*sfpainb4 +
                                m[17,"m2"]*sfcalm1   + m[18,"m2"]*sfcalm2  + m[19,"m2"]*sfcalm3  + m[20,"m2"]*sfcalm4  + m[21,"m2"]*sfcalm5 +
                                m[22,"m2"]*sfener1   + m[23,"m2"]*sfener2  + m[24,"m2"]*sfener3  + m[25,"m2"]*sfener4  + m[26,"m2"]*sfener5 +
                                m[27,"m2"]*sflow1    + m[28,"m2"]*sflow2   + m[29,"m2"]*sflow3   + m[30,"m2"]*sflow4   + m[31,"m2"]*sflow5 +
                                m[32,"m2"]*sfvisit1  + m[33,"m2"]*sfvisit2 + m[34,"m2"]*sfvisit3 + m[35,"m2"]*sfvisit4 +
                                m[36,"m2"]) ]

  health_util_data[, M3XB := (m[1,"m3"]*sfstat1   + m[2,"m3"]*sfstat2 + m[3,"m3"]*sfstat3 + m[4,"m3"]*sfstat4 +
                                m[5,"m3"]*sfmode1   + m[6,"m3"]*sfmode2 +
                                m[7,"m3"]*sfstaira1 + m[8,"m3"]*sfstaira2 +
                                m[9,"m3"]*sfless1 +
                                m[10,"m3"]*sflimit1 +
                                m[11,"m3"]*sflesse1 +
                                m[12,"m3"]*sfcarful1 +
                                m[13,"m3"]*sfpainb1  + m[14,"m3"]*sfpainb2 + m[15,"m3"]*sfpainb3 + m[16,"m3"]*sfpainb4 +
                                m[17,"m3"]*sfcalm1   + m[18,"m3"]*sfcalm2  + m[19,"m3"]*sfcalm3  + m[20,"m3"]*sfcalm4  + m[21,"m3"]*sfcalm5 +
                                m[22,"m3"]*sfener1   + m[23,"m3"]*sfener2  + m[24,"m3"]*sfener3  + m[25,"m3"]*sfener4  + m[26,"m3"]*sfener5 +
                                m[27,"m3"]*sflow1    + m[28,"m3"]*sflow2   + m[29,"m3"]*sflow3   + m[30,"m3"]*sflow4   + m[31,"m3"]*sflow5 +
                                m[32,"m3"]*sfvisit1  + m[33,"m3"]*sfvisit2 + m[34,"m3"]*sfvisit3 + m[35,"m3"]*sfvisit4 +
                                m[36,"m3"]) ]

  health_util_data[, prob_mob1 := 1/(1 + exp(M2XB) + exp(M3XB))]
  health_util_data[, prob_mob2 := exp(M2XB)/(1 + exp(M2XB) + exp(M3XB))]
  health_util_data[, prob_mob3 := exp(M3XB)/(1 + exp(M2XB) + exp(M3XB))]

  health_util_data[, rand_E1 := runif(.N)]

  health_util_data[, eqmob_est := 2]
  health_util_data[rand_E1 < prob_mob1, eqmob_est := 1]
  health_util_data[rand_E1 > 1 - prob_mob3, eqmob_est := 3]

  #################
  ### Self Care ###

  health_util_data[, C2XB := (m[1,"c2"]*sfstat1   + m[2,"c2"]*sfstat2 + m[3,"c2"]*sfstat3 + m[4,"c2"]*sfstat4 +
                                m[5,"c2"]*sfmode1   + m[6,"c2"]*sfmode2 +
                                m[7,"c2"]*sfstaira1 + m[8,"c2"]*sfstaira2 +
                                m[9,"c2"]*sfless1 +
                                m[10,"c2"]*sflimit1 +
                                m[11,"c2"]*sflesse1 +
                                m[12,"c2"]*sfcarful1 +
                                m[13,"c2"]*sfpainb1  + m[14,"c2"]*sfpainb2 + m[15,"c2"]*sfpainb3 + m[16,"c2"]*sfpainb4 +
                                m[17,"c2"]*sfcalm1   + m[18,"c2"]*sfcalm2  + m[19,"c2"]*sfcalm3  + m[20,"c2"]*sfcalm4  + m[21,"c2"]*sfcalm5 +
                                m[22,"c2"]*sfener1   + m[23,"c2"]*sfener2  + m[24,"c2"]*sfener3  + m[25,"c2"]*sfener4  + m[26,"c2"]*sfener5 +
                                m[27,"c2"]*sflow1    + m[28,"c2"]*sflow2   + m[29,"c2"]*sflow3   + m[30,"c2"]*sflow4   + m[31,"c2"]*sflow5 +
                                m[32,"c2"]*sfvisit1  + m[33,"c2"]*sfvisit2 + m[34,"c2"]*sfvisit3 + m[35,"c2"]*sfvisit4 +
                                m[36,"c2"]) ]


  health_util_data[, C3XB := (m[1,"c3"]*sfstat1   + m[2,"c3"]*sfstat2 + m[3,"c3"]*sfstat3 + m[4,"c3"]*sfstat4 +
                                m[5,"c3"]*sfmode1   + m[6,"c3"]*sfmode2 +
                                m[7,"c3"]*sfstaira1 + m[8,"c3"]*sfstaira2 +
                                m[9,"c3"]*sfless1 +
                                m[10,"c3"]*sflimit1 +
                                m[11,"c3"]*sflesse1 +
                                m[12,"c3"]*sfcarful1 +
                                m[13,"c3"]*sfpainb1  + m[14,"c3"]*sfpainb2 + m[15,"c3"]*sfpainb3 + m[16,"c3"]*sfpainb4 +
                                m[17,"c3"]*sfcalm1   + m[18,"c3"]*sfcalm2  + m[19,"c3"]*sfcalm3  + m[20,"c3"]*sfcalm4  + m[21,"c3"]*sfcalm5 +
                                m[22,"c3"]*sfener1   + m[23,"c3"]*sfener2  + m[24,"c3"]*sfener3  + m[25,"c3"]*sfener4  + m[26,"c3"]*sfener5 +
                                m[27,"c3"]*sflow1    + m[28,"c3"]*sflow2   + m[29,"c3"]*sflow3   + m[30,"c3"]*sflow4   + m[31,"c3"]*sflow5 +
                                m[32,"c3"]*sfvisit1  + m[33,"c3"]*sfvisit2 + m[34,"c3"]*sfvisit3 + m[35,"c3"]*sfvisit4 +
                                m[36,"c3"]) ]


  health_util_data[, prob_care1 := 1/(1 + exp(C2XB) + exp(C3XB))]
  health_util_data[, prob_care2 := exp(C2XB)/(1 + exp(C2XB) + exp(C3XB))]
  health_util_data[, prob_care3 := exp(C3XB)/(1 + exp(C2XB) + exp(C3XB))]

  health_util_data[, rand_E2 := runif(.N)]

  health_util_data[, eqcare_est := 2]
  health_util_data[rand_E2 < prob_care1, eqcare_est := 1]
  health_util_data[rand_E2 > 1 - prob_care3, eqcare_est := 3]

  ########################
  ### Usual activities ###

  health_util_data[, U2XB := (m[1,"u2"]*sfstat1   + m[2,"u2"]*sfstat2 + m[3,"u2"]*sfstat3 + m[4,"u2"]*sfstat4 +
                                m[5,"u2"]*sfmode1   + m[6,"u2"]*sfmode2 +
                                m[7,"u2"]*sfstaira1 + m[8,"u2"]*sfstaira2 +
                                m[9,"u2"]*sfless1 +
                                m[10,"u2"]*sflimit1 +
                                m[11,"u2"]*sflesse1 +
                                m[12,"u2"]*sfcarful1 +
                                m[13,"u2"]*sfpainb1  + m[14,"u2"]*sfpainb2 + m[15,"u2"]*sfpainb3 + m[16,"u2"]*sfpainb4 +
                                m[17,"u2"]*sfcalm1   + m[18,"u2"]*sfcalm2  + m[19,"u2"]*sfcalm3  + m[20,"u2"]*sfcalm4  + m[21,"u2"]*sfcalm5 +
                                m[22,"u2"]*sfener1   + m[23,"u2"]*sfener2  + m[24,"u2"]*sfener3  + m[25,"u2"]*sfener4  + m[26,"u2"]*sfener5 +
                                m[27,"u2"]*sflow1    + m[28,"u2"]*sflow2   + m[29,"u2"]*sflow3   + m[30,"u2"]*sflow4   + m[31,"u2"]*sflow5 +
                                m[32,"u2"]*sfvisit1  + m[33,"u2"]*sfvisit2 + m[34,"u2"]*sfvisit3 + m[35,"u2"]*sfvisit4 +
                                m[36,"u2"]) ]

  health_util_data[, U3XB := (m[1,"u3"]*sfstat1   + m[2,"u3"]*sfstat2 + m[3,"u3"]*sfstat3 + m[4,"u3"]*sfstat4 +
                                m[5,"u3"]*sfmode1   + m[6,"u3"]*sfmode2 +
                                m[7,"u3"]*sfstaira1 + m[8,"u3"]*sfstaira2 +
                                m[9,"u3"]*sfless1 +
                                m[10,"u3"]*sflimit1 +
                                m[11,"u3"]*sflesse1 +
                                m[12,"u3"]*sfcarful1 +
                                m[13,"u3"]*sfpainb1  + m[14,"u3"]*sfpainb2 + m[15,"u3"]*sfpainb3 + m[16,"u3"]*sfpainb4 +
                                m[17,"u3"]*sfcalm1   + m[18,"u3"]*sfcalm2  + m[19,"u3"]*sfcalm3  + m[20,"u3"]*sfcalm4  + m[21,"u3"]*sfcalm5 +
                                m[22,"u3"]*sfener1   + m[23,"u3"]*sfener2  + m[24,"u3"]*sfener3  + m[25,"u3"]*sfener4  + m[26,"u3"]*sfener5 +
                                m[27,"u3"]*sflow1    + m[28,"u3"]*sflow2   + m[29,"u3"]*sflow3   + m[30,"u3"]*sflow4   + m[31,"u3"]*sflow5 +
                                m[32,"u3"]*sfvisit1  + m[33,"u3"]*sfvisit2 + m[34,"u3"]*sfvisit3 + m[35,"u3"]*sfvisit4 +
                                m[36,"u3"]) ]

  health_util_data[, prob_uact1 := 1/(1 + exp(U2XB) + exp(U3XB))]
  health_util_data[, prob_uact2 := exp(U2XB)/(1 + exp(U2XB) + exp(U3XB))]
  health_util_data[, prob_uact3 := exp(U3XB)/(1 + exp(U2XB) + exp(U3XB))]

  health_util_data[, rand_E3 := runif(.N)]

  health_util_data[, equact_est := 2]
  health_util_data[rand_E3 < prob_uact1, equact_est := 1]
  health_util_data[rand_E3 > 1 - prob_uact3, equact_est := 3]

  ############
  ### Pain ###

  health_util_data[, P2XB := (m[1,"p2"]*sfstat1   + m[2,"p2"]*sfstat2 + m[3,"p2"]*sfstat3 + m[4,"p2"]*sfstat4 +
                                m[5,"p2"]*sfmode1   + m[6,"p2"]*sfmode2 +
                                m[7,"p2"]*sfstaira1 + m[8,"p2"]*sfstaira2 +
                                m[9,"p2"]*sfless1 +
                                m[10,"p2"]*sflimit1 +
                                m[11,"p2"]*sflesse1 +
                                m[12,"p2"]*sfcarful1 +
                                m[13,"p2"]*sfpainb1  + m[14,"p2"]*sfpainb2 + m[15,"p2"]*sfpainb3 + m[16,"p2"]*sfpainb4 +
                                m[17,"p2"]*sfcalm1   + m[18,"p2"]*sfcalm2  + m[19,"p2"]*sfcalm3  + m[20,"p2"]*sfcalm4  + m[21,"p2"]*sfcalm5 +
                                m[22,"p2"]*sfener1   + m[23,"p2"]*sfener2  + m[24,"p2"]*sfener3  + m[25,"p2"]*sfener4  + m[26,"p2"]*sfener5 +
                                m[27,"p2"]*sflow1    + m[28,"p2"]*sflow2   + m[29,"p2"]*sflow3   + m[30,"p2"]*sflow4   + m[31,"p2"]*sflow5 +
                                m[32,"p2"]*sfvisit1  + m[33,"p2"]*sfvisit2 + m[34,"p2"]*sfvisit3 + m[35,"p2"]*sfvisit4 +
                                m[36,"p2"]) ]

  health_util_data[, P3XB := (m[1,"p3"]*sfstat1   + m[2,"p3"]*sfstat2 + m[3,"p3"]*sfstat3 + m[4,"p3"]*sfstat4 +
                                m[5,"p3"]*sfmode1   + m[6,"p3"]*sfmode2 +
                                m[7,"p3"]*sfstaira1 + m[8,"p3"]*sfstaira2 +
                                m[9,"p3"]*sfless1 +
                                m[10,"p3"]*sflimit1 +
                                m[11,"p3"]*sflesse1 +
                                m[12,"p3"]*sfcarful1 +
                                m[13,"p3"]*sfpainb1  + m[14,"p3"]*sfpainb2 + m[15,"p3"]*sfpainb3 + m[16,"p3"]*sfpainb4 +
                                m[17,"p3"]*sfcalm1   + m[18,"p3"]*sfcalm2  + m[19,"p3"]*sfcalm3  + m[20,"p3"]*sfcalm4  + m[21,"p3"]*sfcalm5 +
                                m[22,"p3"]*sfener1   + m[23,"p3"]*sfener2  + m[24,"p3"]*sfener3  + m[25,"p3"]*sfener4  + m[26,"p3"]*sfener5 +
                                m[27,"p3"]*sflow1    + m[28,"p3"]*sflow2   + m[29,"p3"]*sflow3   + m[30,"p3"]*sflow4   + m[31,"p3"]*sflow5 +
                                m[32,"p3"]*sfvisit1  + m[33,"p3"]*sfvisit2 + m[34,"p3"]*sfvisit3 + m[35,"p3"]*sfvisit4 +
                                m[36,"p3"]) ]

  health_util_data[, prob_pain1 := 1/(1 + exp(P2XB) + exp(P3XB))]
  health_util_data[, prob_pain2 := exp(P2XB)/(1 + exp(P2XB) + exp(P3XB))]
  health_util_data[, prob_pain3 := exp(P3XB)/(1 + exp(P2XB) + exp(P3XB))]

  health_util_data[, rand_E4 := runif(.N)]

  health_util_data[, eqpain_est := 2]
  health_util_data[rand_E4 < prob_pain1, eqpain_est := 1]
  health_util_data[rand_E4 > 1 - prob_pain3, eqpain_est := 3]

  ##########################
  ### Anxiety/Depression ###

  health_util_data[, A2XB := (m[1,"a2"]*sfstat1   + m[2,"a2"]*sfstat2 + m[3,"a2"]*sfstat3 + m[4,"a2"]*sfstat4 +
                                m[5,"a2"]*sfmode1   + m[6,"a2"]*sfmode2 +
                                m[7,"a2"]*sfstaira1 + m[8,"a2"]*sfstaira2 +
                                m[9,"a2"]*sfless1 +
                                m[10,"a2"]*sflimit1 +
                                m[11,"a2"]*sflesse1 +
                                m[12,"a2"]*sfcarful1 +
                                m[13,"a2"]*sfpainb1  + m[14,"a2"]*sfpainb2 + m[15,"a2"]*sfpainb3 + m[16,"a2"]*sfpainb4 +
                                m[17,"a2"]*sfcalm1   + m[18,"a2"]*sfcalm2  + m[19,"a2"]*sfcalm3  + m[20,"a2"]*sfcalm4  + m[21,"a2"]*sfcalm5 +
                                m[22,"a2"]*sfener1   + m[23,"a2"]*sfener2  + m[24,"a2"]*sfener3  + m[25,"a2"]*sfener4  + m[26,"a2"]*sfener5 +
                                m[27,"a2"]*sflow1    + m[28,"a2"]*sflow2   + m[29,"a2"]*sflow3   + m[30,"a2"]*sflow4   + m[31,"a2"]*sflow5 +
                                m[32,"a2"]*sfvisit1  + m[33,"a2"]*sfvisit2 + m[34,"a2"]*sfvisit3 + m[35,"a2"]*sfvisit4 +
                                m[36,"a2"]) ]

  health_util_data[, A3XB := (m[1,"a3"]*sfstat1   + m[2,"a3"]*sfstat2 + m[3,"a3"]*sfstat3 + m[4,"a3"]*sfstat4 +
                                m[5,"a3"]*sfmode1   + m[6,"a3"]*sfmode2 +
                                m[7,"a3"]*sfstaira1 + m[8,"a3"]*sfstaira2 +
                                m[9,"a3"]*sfless1 +
                                m[10,"a3"]*sflimit1 +
                                m[11,"a3"]*sflesse1 +
                                m[12,"a3"]*sfcarful1 +
                                m[13,"a3"]*sfpainb1  + m[14,"a3"]*sfpainb2 + m[15,"a3"]*sfpainb3 + m[16,"a3"]*sfpainb4 +
                                m[17,"a3"]*sfcalm1   + m[18,"a3"]*sfcalm2  + m[19,"a3"]*sfcalm3  + m[20,"a3"]*sfcalm4  + m[21,"a3"]*sfcalm5 +
                                m[22,"a3"]*sfener1   + m[23,"a3"]*sfener2  + m[24,"a3"]*sfener3  + m[25,"a3"]*sfener4  + m[26,"a3"]*sfener5 +
                                m[27,"a3"]*sflow1    + m[28,"a3"]*sflow2   + m[29,"a3"]*sflow3   + m[30,"a3"]*sflow4   + m[31,"a3"]*sflow5 +
                                m[32,"a3"]*sfvisit1  + m[33,"a3"]*sfvisit2 + m[34,"a3"]*sfvisit3 + m[35,"a3"]*sfvisit4 +
                                m[36,"a3"]) ]

  health_util_data[, prob_anx1 := 1/(1 + exp(A2XB) + exp(A3XB))]
  health_util_data[, prob_anx2 := exp(A2XB)/(1 + exp(A2XB) + exp(A3XB))]
  health_util_data[, prob_anx3 := exp(A3XB)/(1 + exp(A2XB) + exp(A3XB))]

  health_util_data[, rand_E5 := runif(.N)]

  health_util_data[, eqanx_est := 2]
  health_util_data[rand_E5 < prob_anx1, eqanx_est := 1]
  health_util_data[rand_E5 > 1 - prob_anx3, eqanx_est := 3]

  ##########################
  ### Apply EQ-5D Tariff ###

  health_util_data[, eq5d_score := 1]

  health_util_data[eqmob_est == 2, eq5d_score := eq5d_score - 0.069]
  health_util_data[eqmob_est == 3, eq5d_score := eq5d_score - 0.341]

  health_util_data[eqcare_est == 2, eq5d_score := eq5d_score - 0.104]
  health_util_data[eqcare_est == 3, eq5d_score := eq5d_score - 0.214]

  health_util_data[equact_est == 2, eq5d_score := eq5d_score - 0.036]
  health_util_data[equact_est == 3, eq5d_score := eq5d_score - 0.094]

  health_util_data[eqpain_est == 2, eq5d_score := eq5d_score - 0.123]
  health_util_data[eqpain_est == 3, eq5d_score := eq5d_score - 0.386]

  health_util_data[eqanx_est == 2, eq5d_score := eq5d_score - 0.071]
  health_util_data[eqanx_est == 3, eq5d_score := eq5d_score - 0.236]

  health_util_data[eqmob_est != 1 | eqcare_est != 1 | equact_est != 1 | eqpain_est != 1 | eqanx_est != 1, eq5d_score := eq5d_score - 0.081]
  health_util_data[eqmob_est == 3 | eqcare_est == 3 | equact_est == 3 | eqpain_est == 3 | eqanx_est == 3, eq5d_score := eq5d_score - 0.269]

  out <- health_util_data[, c("pidp", "id","wave_no","eq5d_score")]

  return(out)

}
