library(foreign)
library(data.table)

CoefficientMatrix <- as.matrix( read.dta("data-raw/Coefficient_Matrix.dta") )

usethis::use_data(CoefficientMatrix, overwrite = TRUE)
