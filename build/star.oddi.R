library(gulf.data)




#files <- locate.star.oddi(path = "W:/Crab/Offshore Crab Common/Fishing Year 2020/Trawl Data/South Western Gulf/Star Oddi", keywords = "headline")
tow.id <- substr(unlist(lapply(strsplit(files, "/"), function(x) x[length(x)-1])), 6, 15)
file.copy(from = files, to = paste0("data/star oddi/headline/", tow.id, ".dat"), overwrite = TRUE, copy.date = TRUE)

files <- locate(path = paste0(options()$gulf.path$snow.crab,
                              "/Offshore Crab Common/Fishing Year 2020/Trawl Data/South Western Gulf/Star Oddi/Tilt"),
                pattern = "*.DAT", keywords = "star oddi")

tow.id <- substr(unlist(lapply(strsplit(files, "/"), function(x) x[length(x)-1])), 6, 15)
file.copy(from = files, to = paste0("data/star oddi/footrope/", tow.id, ".dat"), overwrite = TRUE, copy.date = TRUE)


files <- locate(path = paste0(options()$gulf.path$snow.crab,
                              "/Offshore Crab Common/Fishing Year 2020/Trawl Data/South Western Gulf/Star Oddi/Tilt"),
                pattern = "*.DAT", keywords = "star oddi")

