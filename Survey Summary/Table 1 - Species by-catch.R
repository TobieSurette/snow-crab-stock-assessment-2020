
x <- read.scset(year = 2019, valid = 1)
cat <- read.sccat(year = 2019)
len <- read.csv("W:/Crab/Offshore Crab Common/Fishing Year 2019/Trawl Data/South Western Gulf/By-catch/SCS2019L.csv", header = TRUE, stringsAsFactors = FALSE)
len$length[len$length.unit == "mm"] <- len$length[len$length.unit == "mm"] / 10
len$tow.id <- toupper(len$tow.id)

tmp <- aggregate(list(number.caught = len$length), by = len[c("species", "tow.id")], length)
index <- match(tmp[c("species", "tow.id")], cat[c("species", "tow.id")])
tmp <- tmp[is.na(index), ]
tmp$year <- 2019
tmp$month <- NA
tmp$day <- NA
tmp$tow.number <- NA
tmp$weight.caught <- NA
tmp$presence <- 1
tmp$comment <- NA
tmp <- tmp[names(cat)]
cat <- rbind(cat, tmp)

cat <- cat[substr(cat$tow.id,2,2) != "C", ]
#cat <- cat[cat$tow.id %in% x$tow.id[x$groundfish.sample == 1], ]

res <- aggregate(list(count = cat$number.caught), cat["species"], sum, na.rm = TRUE)
res$grids <- aggregate(list(x = cat$tow.id), cat["species"], length)$x
res$weight <- aggregate(list(x = cat$weight.caught), cat["species"], sum, na.rm = TRUE)$x
res$species.name <- species.str(res$species, source = "gulf")
res$species.latin <- species.str(res$species, source = "gulf", language = "latin")

res <- res[c("species", "species.name", "species.latin", "count", "grids", "weight")]


