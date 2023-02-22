#' Read British Household Panel Survey - all waves
#'
#' Reads and performs basic cleaning on the full harmonised British Household Panel Survey (BHPS).
#' data. Note that this function currently only cleans smoker status and number of cigarettes variables
#' to produce full smoking histories for BHPS participants in the Understanding Society survey
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
bhps_read_all <- function(
    root = c("X:/"),
    file = "HAR_PR/PR/USoc/Data/SN6614_2022_11_29/tab/bhps",
    full = TRUE
    ) {

      cat(crayon::magenta("\tReading BHPS datasets Waves 1 - 18: "))

      path <- here::here(paste0(root, file))

      ##############################
      ### LOOP OVER WAVES 1 - 18

      wave_list <- c("a","b","c","d","e","f","g","h","i",
                     "j","k","l","m","n","o","p","q","r")

      for (wave in 1:length(wave_list)){

      cat(crayon::red(paste0(wave," ")))


      data <- data.table::fread(
        paste0(path, "/b", wave_list[wave], "_indresp.tab"),
        showProgress = FALSE,
        na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
      )

      if (wave == 9){
        data[, bi_smoker := NA]
      }

      keep_vars <- c("pid",
                     paste0("b",wave_list[wave],"_age_dv"),
                     paste0("b",wave_list[wave],"_smoker"),
                     paste0("b",wave_list[wave],"_ncigs"))

      clean_data <- data[, ..keep_vars, with = F]

      setnames(clean_data, names(clean_data), c("id","age","smoker","ncigs"))

      clean_data[, wave := paste0("BHPS Wave ",wave)]

      if (wave == 1){
        data_out <- copy(clean_data)
      } else {
        data_out <- rbindlist(list(data_out, clean_data), fill = TRUE)
      }

    }

      ##############################
      ### Clean the smoking variables

      #### Current smoking status

      if ("smoker" %in% colnames(data_out)) {

        data_out[smoker == 1, current_smoker := "smoker"]
        data_out[smoker == 2, current_smoker := "non_smoker"]

        data_out[, current_smoker := as.factor(current_smoker)]

      } else {

        data_out[, current_smoker := NA]
      }

      #### Number of cigarettes smoked

      if ("ncigs" %in% colnames(data_out)) {

        data_out[, ncigs := ncigs]

      } else {

        data_out[, ncigs := NA]
      }



      cat(crayon::magenta("\tdone\n"))


      ##############################
      ### Save out the data

      final_data <- data_out[, c("id", "wave",
                             "age","current_smoker", "ncigs")]

      final_data[, dataset := "BHPS"]


      return(final_data)
    }

