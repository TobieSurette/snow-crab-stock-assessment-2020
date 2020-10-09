# Files to treat:
x <- read.table("W:/Crab/Offshore Crab Common/Fishing Year 2019/Trawl Data/South Western Gulf/By-catch/Raw Data/SCS2019 By-catch.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
names(x) <- tolower(names(x))
str <- names(x)
tmp <- read.table("W:/Crab/Offshore Crab Common/Fishing Year 2019/Trawl Data/South Western Gulf/By-catch/Raw Data/SCS2019 Comparative By-catch.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
names(tmp) <- tolower(names(tmp))

# Combine data sets:
x <- rbind(x, tmp)

# Parse date:
x$year   <- as.numeric(unlist(lapply(strsplit(x$date, "/"), function(x) x[3])))
x$month  <- as.numeric(unlist(lapply(strsplit(x$date, "/"), function(x) x[1])))
x$day    <- as.numeric(unlist(lapply(strsplit(x$date, "/"), function(x) x[2])))
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
   if (length(grep("p", str)) > 0) p <- x[,str[grep("p", str)]] else p <- NA
   if (length(grep("w", str)) > 0) cw <- x[,str[grep("w", str)]] else cw <- NA
   if (length(grep("c", str)) > 0) nc <- x[,str[grep("c", str)]] else nc <- NA
   tmp <- cbind(x[c("year", "month", "day", "tow.id")], data.frame(species = species[i], weight.caught = cw, number.caught = nc, presence = p))
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
s <- read.scset(year = 2019, valid = 1)

# Check that tow IDs exist:
index <- match(y$tow.id, s$tow.id)
y[is.na(index), ]

# Check that dates match:
all(y$month == s$month[index] & y$day == s$day[index])

# Import tow number:
y$tow.number <- s$tow.number[index]

# Re-order columns:
y <- y[c("year", "month", "day", "tow.number", "tow.id", "species", "number.caught", "weight.caught", "presence", "comment")]

# Spot corrections:
y$number.caught[which(y$tow.id == "GP354F" & y$species == 2527)] <- 3
y$weight.caught[which(y$tow.id == "GP354F" & y$species == 2527)] <- 0.43


#==============================================================================================================================



y$species[y$tow.id == "GP354F" & y$species == 41] <- 40      
y$number.caught[y$tow.id == "GP350F" & y$species == 40] <- 172
y$weight.caught[y$tow.id == "GP019F" & y$species == 42] <- 0.278
y$weight.caught[y$tow.id == "GP027F" & y$species == 64] <- 0.02
y$weight.caught[y$tow.id == "GP130F" & y$species == 340] <- 0.008                                              
y$number.caught[y$tow.id == "GP100F" & y$species == 1510] <- 1 
y$species[y$tow.id == "GP231F" & y$species == 2524] <- 2513
y$number.caught[y$tow.id == "GP320F" & y$species == 23] <- 1169
y$weight.caught[y$tow.id == "GP320F" & y$species == 23] <- 153.79
y$number.caught[y$tow.id == "GP282F" & y$species == 23] <- 188
y$weight.caught[y$tow.id == "GP128A2" & y$species == 6400] <- 3.36
y$species[y$tow.id == "GP107F" & y$species == 8300] <- 8600
y$number.caught[y$tow.id == "GP350F" & y$species == 201] <- 30
y$number.caught[y$tow.id == "GP084A1" & y$species == 300] <- 2
y$number.caught[y$tow.id == "GP236A1" & y$species == 302] <- 2
y$number.caught[y$tow.id == "GP057F" & y$species == 340] <- 2

# Added entries:
tmp <- rbind(data.frame(year = 2018, month = 8, day = 16, tow.number = 6, tow.id = "GP064F", species = 9996, number.caught = 24, weight.caught = 0.21, presence = 1, 
                        comment = "Haminoe solitaria (see picture)"),
             data.frame(year = 2018, month = 7, day = 23, tow.number = 3, tow.id = "GP081F", species = 9996, number.caught = 1, weight.caught = 0.2, presence = 1, 
                        comment = "Halipteris finmarchica"), 
             data.frame(year = 2018, month = 7, day = 23, tow.number = 3, tow.id = "GP081F", species = 9997, number.caught = 1, weight.caught = 0.5, presence = 1, 
                        comment = "Pennutula aceleata"), 
             data.frame(year = 2018, month = 8, day = 28, tow.number = 7, tow.id = "GP224F", species = 8600, number.caught = 1, weight.caught = 0.5, presence = 1, 
                        comment = "Halicorna sp."),
             data.frame(year = 2018, month = 8, day = 16, tow.number = 5, tow.id = "GP054F", species = 8324, number.caught = 1, weight.caught = 0.4, presence = 1, 
                        comment = ""), 
             data.frame(year = 2018, month = 8, day = 18, tow.number = 5, tow.id = "GP060A1", species = 8324, number.caught = 1, weight.caught = 0.1, presence = 1, 
                        comment = ""),                     
             data.frame(year = 2018, month = 8, day = 16, tow.number = 7, tow.id = "GP078F", species = 9996, number.caught = 1, weight.caught = 0.3, presence = 1, 
                        comment = "Duva florida"))
                        
y <- rbind(y, tmp)

# Attach sponge and coral data:
other <- read.csv("W:/Crab/Offshore Crab Common/Fishing Year 2018/Trawl Data/South Western Gulf/By-catch/Sponge and Coral Catches 2018.csv", stringsAsFactors = FALSE)
index <- match(other[c("tow.id", "species")], y[c("tow.id", "species")])
y$weight.caught[index[which(!is.na(index))]] <- other$weight.caught[!is.na(index)]
other <- other[-which(!is.na(index)), ]
s <- read.scset(year = 2018, valid = 1)
index <- match(other$tow.id, s$tow.id)
other$year  <- s$year[index]
other$month <- s$month[index]
other$day   <- s$day[index]
other$tow.number <- s$tow.number[index]
other$presence <- 1
other$number.caught <- NA
other <- other[names(y)]

# Update sponge entries from database:
yy <- y[y$species == 8600, ]  # Isolate sponges.
tmp <- aggregate(other["weight.caught"], by = other["tow.id"], sum)
index <- match(yy$tow.id, tmp$tow.id)
yy$total <- NA
yy$total <- tmp$weight.caught[index]
y <- y[y$species != 8600, ]  # Remove all previous sponges entries.
yy <- yy[-which(!is.na(yy$total)), ]
yy <- yy[, -grep("total", names(yy))]
y <- rbind(y, yy)    # Re-add entries hich are to be kept as is.
y <- rbind(y, other) # Add sponge entries which are to be updated.

# Check individual species entries:
i = 102
species.str(species[i])
tmp <- y[y$species == species[i], ]
tmp$ratio <- round(tmp$weight.caught / tmp$number.caught, 3)
tmp[order(tmp$weight.caught), ]
y[y$tow.id == "GP231F", ]

y <- sort(y, by = c("year", "month", "day", "tow.number", "tow.id", "species"))
rownames(y) <- NULL



# Read length-frequency file:
z <- read.table("W:/Crab/Offshore Crab Common/Fishing Year 2019/Trawl Data/South Western Gulf/By-catch/SCS2019L.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

# Check for measurements with no by-catch data:
index <- match(z[c("tow.id", "species")], y[c("tow.id", "species")])
z[is.na(index), ]

# Check for mis-matches between counts:
tmp <- aggregate(list(n = z$length), by = z[c("tow.id", "species")], length)
index <- match(tmp[c("tow.id", "species")], y[c("tow.id", "species")])
tmp$number.caught <- y$number.caught[index]
tmp[which((tmp$n != tmp$number.caught) & tmp$n < 100), ]

# Write by-catch table:
write.table(y, file = "W:/Crab/Offshore Crab Common/Fishing Year 2019/Trawl Data/South Western Gulf/By-Catch/SCS2019C.csv", sep = ",", row.names = FALSE)
