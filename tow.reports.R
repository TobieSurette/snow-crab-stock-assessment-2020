library(gulf.data)
library(gulf.graphics)
library(gulf.spatial)

# Load tow data:
x <- read.scsset(2020, valid = 1)

# Plot tow data:
tow.id <- "GP237F"
clg(); plot(x[which(x$tow.id == "GP098F"), ])

clg(); plot(x[which(x$tow.id == "GP002F"), ])

clg(); plot(x, wingspread = FALSE, headline = FALSE, tilt = FALSE)


read.esonar(x[which(x$tow.id == "GP276F"), ])

clg()
x <- read.scsset(2020, tow.id = "GP002F")
plot(x, pdf = TRUE, path = paste0(getwd(), "/results/tow reports"))

