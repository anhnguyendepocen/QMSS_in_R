library(devtools)
library(roxygen2)

setwd("/Users/jgabry/Desktop/COLUMBIA/Stuff_for_Greg/QMSS/Package")
# create("QMSS")


setwd("./QMSS")
document()

setwd("..")
install("QMSS")

library(QMSS)
?rho
?Tab
?ReverseThis
?tab
?propOddsTest
?stdCoef
?ovtest
?firstD
data(GSS_2010)


