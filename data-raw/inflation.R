library(curl)
library(readxl)
library(data.table)
library(magrittr)
library(lubridate)

base <- c(1,2023)

#######################################
###### CPIH Monthly Inflation #########

### download data direct from ONS

temp <- tempfile()
url <- "https://www.ons.gov.uk/generator?format=xls&uri=/economy/inflationandpriceindices/timeseries/l522/mm23"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

### load in the data and clean

data <- read_excel(temp, range="A9:B2000", col_names = FALSE) %>% setDT()

setnames(data, names(data), c("time","index"))

data[, year := as.numeric(substr(time,1,4))]
data[, month := substr(time,6,8)]

### keep only monthly data and recode to numeric

data <- data[!(month %in% c(NA,"","Q1","Q2","Q3","Q4")),]

data[.(month = c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"),
       to = c(1:12)), on = "month", month := i.to]

data[, month := as.numeric(month)]

cpih <- data[, c("year","month","index")]

#### Rebase the index

base_infl <- as.numeric(data[month == base[1] & year == base[2], "index"]  )

cpih[, index := 100*(index/base_infl)]

usethis::use_data(cpih, overwrite=TRUE)

#######################################
###### RPI Monthly Inflation #########

### download data direct from ONS

temp <- tempfile()
url <- "https://www.ons.gov.uk/generator?format=xls&uri=/economy/inflationandpriceindices/timeseries/cdko/mm23"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

### load in the data and clean

data <- read_excel(temp, range="A9:B2000", col_names = FALSE) %>% setDT()

setnames(data, names(data), c("time","index"))

data[, year := as.numeric(substr(time,1,4))]
data[, month := substr(time,6,8)]

### keep only monthly data and recode to numeric

data <- data[!(month %in% c(NA,"","Q1","Q2","Q3","Q4")),]

data[.(month = c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"),
       to = c(1:12)), on = "month", month := i.to]

data[, month := as.numeric(month)]

rpi <- data[, c("year","month","index")]

#### Rebase the index

base_infl <- as.numeric(data[month == base[1] & year == base[2], "index"]  )

rpi[, index := 100*(index/base_infl)]

usethis::use_data(rpi, overwrite=TRUE)
