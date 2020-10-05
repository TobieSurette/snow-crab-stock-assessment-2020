library(gulf.utils)
library(gulf.data)

source("C:/Users/SuretteTJ/Desktop/gulf.data/R/esonar.R")

files <- locate.esonar(path = "W:/Crab/Offshore Crab Common/Fishing Year 2020/Trawl Data/South Western Gulf/ESonar/Summary", remove = FALSE)

file.copy(from = files, to = "data/esonar", overwrite = FALSE, copy.date = TRUE)
