#### Read in inflation data from the ONS and forecast inflation from
#### the Office for Budget Responsibility (OBR)

library(data.table)
library(readxl)
library(curl)
library(magrittr)

###################################
#### Get ONS data for 2020 ########

url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland/mid2020/ukpopestimatesmid2020on2021geography.xls"
temp <- tempfile()
temp <- curl_download(url = url, destfile = temp, quiet = FALSE, mode = "wb")

#######
## read in the data (males)

data <- readxl::read_excel(temp, sheet = "MYE2 - Males", range = "B8:CQ428") %>% setDT

data <- data[Geography == "Country",]
data <- data[Name %in% c("ENGLAND","WALES","SCOTLAND","NORTHERN IRELAND"),]
data[, c("Geography","All ages","90+") := NULL]
setnames(data, "Name", "d_country")

data <- melt(data, id.vars = "d_country", variable.name = "d_age", value.name = "pop_count")
data[, d_age := as.numeric(as.character(d_age)) ]
data[, d_sex := "male"]

male <- copy(data)

#######
## read in the data (females)

data <- readxl::read_excel(temp, sheet = "MYE2 - Females", range = "B8:CQ428") %>% setDT

data <- data[Geography == "Country",]
data <- data[Name %in% c("ENGLAND","WALES","SCOTLAND","NORTHERN IRELAND"),]
data[, c("Geography","All ages","90+") := NULL]
setnames(data, "Name", "d_country")

data <- melt(data, id.vars = "d_country", variable.name = "d_age", value.name = "pop_count")
data[, d_age := as.numeric(as.character(d_age)) ]
data[, d_sex := "female"]

female <- copy(data)

#######
## combine data, align merging variables with those generated in the main data

data <- rbindlist(list(male,female))
data[, year := 2020]

data[, d_sex := factor(d_sex, levels = c("male","female"))]
data[, d_country := factor(d_country,
                           levels = c("ENGLAND","WALES","SCOTLAND","NORTHERN IRELAND"),
                           labels = c("england","wales","scotland","northern_ireland"))]

###########################
### Save out to package

PopulationCounts <- copy(data)

usethis::use_data(PopulationCounts, overwrite = TRUE)
