library(readxl)
library(data.table)

## read in the downloaded cpi index data

data <- read_excel(path = paste0("data-raw/","CPI Index 02-03-2022",".xls"),
                   sheet = "data",
                   range = "B43:B414",
                   col_names = FALSE)

cpi_value <- as.vector(as.matrix(data))
cpi_value <- as.numeric(cpi_value)

### create a year and month indicator from Jan 2004 to December of the
### current year of APS data

year <- c(rep(1991:2021,each=12))

month <- c(rep(seq(1:12),2021-1991+1))

cpi <- matrix(c(year,month,cpi_value),
              byrow=FALSE,
              ncol=3, dimnames = list(NULL,
                                      c("year","month","cpi_value")))

# rebase to the most recent month of data

cpi[,"cpi_value"] <- 100*(cpi[,"cpi_value"]/(cpi[length(cpi[,"cpi_value"]),"cpi_value"]))
print(paste0("Base Year = ",cpi[length(cpi[,"year"]),"year"]))
print(paste0("Base Month = ",cpi[length(cpi[,"month"]),"month"]))

cpi <- as.data.table(cpi)

usethis::use_data(cpi, overwrite=TRUE)

rm(data, cpi, cpi_value, month, year)
