# Files to treat:
x <- read.csv("data/raw/scs.cat.2020.csv", header = TRUE, stringsAsFactors = FALSE)
names(x) <- tolower(names(x))

# Parse date:
x$tow.id <- toupper(x$gpnum)
x$comment <- gsub("\n", ", ", x$comment)
x$comment[x$comment == ", "] <- ""

# Isolate catch variables:
vars <- names(x)[grep("[cwp][0-9]+", names(x))]
vars <- vars[!apply(x[vars], 2, function(x) all(is.na(x)))]  # Remove species with only NA values.

# Reformat table:
species <- sort(as.numeric(unique(substr(vars, 2, nchar(vars)))))
y <- NULL
for (i in 1:length(species)){
   str <- vars[which(as.numeric(substr(vars, 2, nchar(vars))) %in% species[i])]
   if (length(grep("^p", str)) > 0) p <-  x[,str[grep("p", str)]] else p <- NA
   if (length(grep("^w", str)) > 0) cw <- x[,str[grep("w", str)]] else cw <- NA
   if (length(grep("^c", str)) > 0) nc <- x[,str[grep("c", str)]] else nc <- NA
   tmp <- cbind(x[c("date", "tow.id")], data.frame(species = species[i], weight.caught = cw, number.caught = nc, presence = p))
   y <- rbind(y, tmp)
}
y$number.caught[which(y$number.caught == 0)] <- NA
y$weight.caught[which(y$weight.caught == 0)] <- NA
y$comment <- ""

# Remove empty catches:
y <- y[!apply(y[c("weight.caught", "number.caught", "presence")], 1, function(x) all(is.na(x))), ]

# Fix missing presences:
y$presence[is.na(y$presence) & (!is.na(y$weight.caught) | !is.na(y$number.caught))] <- 1

# Read set of valid tows:
s <- read.scsset(year = 2020)

# Check that tow IDs exist:
index <- match(y$tow.id, s$tow.id)
y[is.na(index), ]

# Check that dates match:
all(y$month == s$month[index] & y$day == s$day[index])

# Import tow number:
y$tow.number <- s$tow.number[index]

# Re-order columns:
y <- y[c("date", "tow.number", "tow.id", "species", "number.caught", "weight.caught", "presence", "comment")]

# Spot corrections:
#y$number.caught[which(y$tow.id == "GP354F" & y$species == 2527)] <- 3
#y$weight.caught[which(y$tow.id == "GP354F" & y$species == 2527)] <- 0.43

#==============================================================================================================================

#y$species[y$tow.id == "GP354F" & y$species == 41] <- 40
#y$number.caught[y$tow.id == "GP350F" & y$species == 40] <- 172
#y$weight.caught[y$tow.id == "GP019F" & y$species == 42] <- 0.278
#y$weight.caught[y$tow.id == "GP027F" & y$species == 64] <- 0.02
#y$weight.caught[y$tow.id == "GP130F" & y$species == 340] <- 0.008
#y$number.caught[y$tow.id == "GP100F" & y$species == 1510] <- 1
#y$species[y$tow.id == "GP231F" & y$species == 2524] <- 2513
#y$number.caught[y$tow.id == "GP320F" & y$species == 23] <- 1169
#y$weight.caught[y$tow.id == "GP320F" & y$species == 23] <- 153.79
#y$number.caught[y$tow.id == "GP282F" & y$species == 23] <- 188
#y$weight.caught[y$tow.id == "GP128A2" & y$species == 6400] <- 3.36
#y$species[y$tow.id == "GP107F" & y$species == 8300] <- 8600
#y$number.caught[y$tow.id == "GP350F" & y$species == 201] <- 30
#y$number.caught[y$tow.id == "GP084A1" & y$species == 300] <- 2
#y$number.caught[y$tow.id == "GP236A1" & y$species == 302] <- 2
#y$number.caught[y$tow.id == "GP057F" & y$species == 340] <- 2

# Check individual species entries:
i = 1
species(species[i])
tmp <- y[y$species == species[i], ]
tmp$ratio <- round(tmp$weight.caught / tmp$number.caught, 3)
tmp[order(tmp$weight.caught), ]
y[y$tow.id == "GP231F", ]

y <- sort(y, by = c("date", "tow.number", "tow.id", "species"))
rownames(y) <- NULL

# Write by-catch table:
write.table(y, file = "data/by-catch/scs.cat.2020.csv", sep = ",", row.names = FALSE)
