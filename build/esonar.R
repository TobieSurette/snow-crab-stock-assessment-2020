library(gulf.utils)
library(gulf.data)


source("C:/Users/SuretteTJ/Desktop/gulf.data/R/esonar.R")

files <- locate.esonar(year = 2020)

locate.esonar(year = 2020)

locate.esonar(path = "W:/Crab/Offshore Crab Common/Fishing Year 2020/Trawl Data/South Western Gulf/ESonar/Summary")



file.copy(from = files, to = "/data/esonar", overwrite = TRUE, copy.date = TRUE)
