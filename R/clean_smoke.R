
#' Clean Smoking Variables
#'
#' Reads and does basic cleaning on the UKHLS first wave.
#'
#' A sample of the population living in private households. All persons living in the house, including those
#' under 2 years were eligible for inclusion. At addresses where there were more than two children under 16,
#' two children were selected at random. Information was obtained directly from persons aged 13 and
#' over. Information about children aged 0-12 was obtained from a parent, with the child present.
#'
#' @export
clean_smoke <- function(data = NULL) {

# ensure data is sorted
data <- data[order(pidp,wave),]
# backfill smoking behaviour to waves 1-4 ---------

data[, StartAge:=mean(smagbg,na.rm=TRUE), by=pidp] # starting age
data[, QuitAge:=mean(aglquit,na.rm=TRUE), by=pidp] # ending age
data[is.nan(QuitAge) , QuitAge:=Inf, by=pidp]
data[, MeanCigs:=mean(smncigs,na.rm=TRUE), by=pidp]  # umber of cigs smoked by former smokers
data[, MeanCurrCigs:=mean(ncigs,na.rm=TRUE), by=pidp] # number of cigs smoked by current smokers

data[wave < 5 & age >= StartAge & age < QuitAge, current_smoker := TRUE] # backfill current smoker status if in prior waves was within the smoking age range
data[wave < 5 & is.na(current_smoker), current_smoker := FALSE]

# for those who smoked in waves 1-4, fill in ncigs as the ncigs "used to smoke" stated in wave 5
# if the individual identifies as a former smoker in wave 5. if a current smoker in wave 5, fill in with
# mean cigs smoked in waves 5-9 [REPLACE THIS WITH JUST WAVE 5 AT SOME POINT]

data[wave < 5 & age >= StartAge & age < QuitAge, ncigs := ifelse(!is.nan(MeanCigs),MeanCigs,MeanCurrCigs ) ]

## identify current smokers ------------------
data[(wave == 5 & smnow == 1) |
       (wave > 5 & smoker == 1) |
       (is.na(smoker) & ncigs > 0), current_smoker := TRUE, by=pidp]
data[is.na(current_smoker), current_smoker := FALSE]

## identify former smokers ------------------
data[wave == 5 & smever == 1 & current_smoker == FALSE, former_smoker := TRUE]
# flag when individual who once identified as a smoker no longer does
data[current_smoker == FALSE & shift(current_smoker) == TRUE, former_smoker := TRUE, by=pidp]
data[current_smoker == FALSE & shift(former_smoker,n=1 ) == TRUE, former_smoker := TRUE, by=pidp]
data[current_smoker == FALSE & shift(former_smoker,n=2 ) == TRUE, former_smoker := TRUE, by=pidp]
data[current_smoker == FALSE & shift(former_smoker,n=3 ) == TRUE, former_smoker := TRUE, by=pidp]
data[current_smoker == FALSE & shift(former_smoker,n=4 ) == TRUE, former_smoker := TRUE, by=pidp]
data[current_smoker == FALSE & shift(former_smoker,n=5 ) == TRUE, former_smoker := TRUE, by=pidp]
data[current_smoker == FALSE & shift(former_smoker,n=6 ) == TRUE, former_smoker := TRUE, by=pidp]
data[current_smoker == FALSE & shift(former_smoker,n=7 ) == TRUE, former_smoker := TRUE, by=pidp]
data[current_smoker == FALSE & shift(former_smoker,n=8 ) == TRUE, former_smoker := TRUE, by=pidp]
data[is.na(former_smoker), former_smoker := FALSE]

## identify never smokers ------------------
data[, never_smoker := FALSE]
data[current_smoker == FALSE & former_smoker == FALSE, never_smoker := TRUE]

## replace NA if individual not observed in relevant waves to backfill information

data[, max:=max(wave), by=pidp] # starting age
data[max < 5, current_smoker := NA]
data[max < 5, former_smoker := NA]
data[max < 5, never_smoker := NA]
data[max < 5, ncigs := NA]

# remove the raw data
data <- subset(data,select = -c(smever,smnow,smcigs,smncigs,aglquit,smagbg,smoker,ecigs,giveup,ecigs1,
                                StartAge,QuitAge,MeanCigs,MeanCurrCigs,max))



}

#library(dplyr)

#data_test <- saved_data %>%
 # filter(pidp == 22445) %>%
  #select(pidp,wave,age,ncigs,smever,smnow,smcigs,smncigs,aglquit,smagbg,smoker,ecigs,giveup,ecigs1)
