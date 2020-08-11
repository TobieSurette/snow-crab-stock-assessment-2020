
# Header files:
files <- star.oddi.file.str(year = 2020)
tows <- unlist(lapply(strsplit(toupper(files), "GP"), function(x) x[length(x)]))
tows <- unlist(lapply(strsplit(tows, "/"), function(x) x[1]))
tows <- unlist(lapply(strsplit(tows, " "), function(x) x[1]))
tows <- paste0("GP", tows)

names <- unlist(lapply(strsplit(toupper(files), "/"), function(x) x[length(x)]))

path <- "U:/Snow Crab/Stock Assessment 2020/data/star oddi/headline"
for (i in 1:length(files)){
   file.copy(from = files[i], to = path, overwrite = TRUE, copy.date = TRUE)
   names <- unlist(lapply(strsplit(toupper(files[i]), "/"), function(x) x[length(x)]))
   file.rename(from = paste0(path, "/", names), to = paste0(path, "/", tows[i], ".DAT"))
}

# Footrope files:
files <- star.oddi.file.str(year = 2020, type = "tilt")
tows <- unlist(lapply(strsplit(toupper(files), "GP"), function(x) x[length(x)]))
tows <- unlist(lapply(strsplit(tows, "/"), function(x) x[1]))
tows <- unlist(lapply(strsplit(tows, " "), function(x) x[1]))
tows <- paste0("GP", tows)

names <- unlist(lapply(strsplit(toupper(files), "/"), function(x) x[length(x)]))

path <- "U:/Snow Crab/Stock Assessment 2020/data/star oddi/footrope"
for (i in 1:length(files)){
   file.copy(from = files[i], to = path, overwrite = TRUE, copy.date = TRUE)
   names <- unlist(lapply(strsplit(toupper(files[i]), "/"), function(x) x[length(x)]))
   file.rename(from = paste0(path, "/", names), to = paste0(path, "/", tows[i], ".DAT"))
}

