library(gulf)

x <- read.table("U:/Snow Crab/Stock Assessment 2020/data/raw/Biological Data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
names(x) <- tolower(names(x))
x$tow.id <- toupper(x$gpnumber)
x$tow.number <- x$trawl.number 
x$egg.colour <- x$egg.color
x$eggs.remaining <- x$percent.eggs
x$gonad.colour <- x$gonade

# Extract time stamp:
x$time <- unlist(lapply(strsplit(x$timestamp, " "), function(x) x[2]))
              
x$missing.legs <- apply(x[, c(paste0("l", 1:5), paste0("l", 1:5))], 1, paste0, collapse = "")
x$year <- as.numeric(substr(x$date, 1, 4))
x$month <- as.numeric(substr(x$date, 6, 7))
x$day <- as.numeric(substr(x$date, 9, 10))

# Delete irrelevant data:
vars <- c("year", "month", "day", "time", "tow.id", "tow.number", "crab.number", "sex", "carapace.width", "chela.height", "shell.condition", "missing.legs", 
          "gonad.colour", "egg.colour", "eggs.remaining", "samplers", "comment")  
x <- x[vars]


x$shell.condition.mossy <- "*"
x$shell.condition.mossy[substr(toupper(x$shell.condition), 2, 2) == "M"] <- "M"
x$shell.condition <- substr(x$shell.condition, 1, 1)
x$tag.number <- "00000000"
x$position.type <- "LL" 
x$weight <- "*****"
x$depth <- "***"
x$durometer <- "***"
x$trap.code <- "****"
x$abdomen.width <- "*****"    

# Remove irrelevant data:
x <- x[x$month >= 7, ]
x <- sort(x, by = c("year", "month", "day", "tow.number", "crab.number"))
index <- is.na(x$carapace.width) & (x$shell.condition == "*") & (x$gonad.colour == "*") & (x$egg.colour == "*") & (x$eggs.remaining == "*")
x <- x[!index, ]
x <- x[x$sex %in% c("1", "2"), ]

                                                                                      
# Fix comments:
x$comment <- gsub("Ž", "e", x$comment)
x$comment <- gsub("ã©", "e", x$comment)
x$comment <- paste0(toupper(substr(x$comment, 1, 1)), tolower(substr(x$comment, 2, nchar(x$comment))))

# Check missing legs:
table(unlist((strsplit(x$missing.legs, ""))))

# Check for inconsistencies is sex measurements:
which(x$sex == "2" & !is.na(x$chela.height))
index <- which((x$sex == "1") & ((x$gonad.colour != "*") | (x$egg.colour != "*") | (x$eggs.remaining != "*")))

# Missing eggs remaining:
index <- which((x$egg.colour == "4") & (x$eggs.remaining == "*"))
x$eggs.remaining[index] <- "0"

# Fix tow exceeding 1000 crab:
x$crab.number[x$tow.id == "GP101A1"] <- 999 + x$crab.number[x$tow.id == "GP101A1"]  
x$tow.number[x$tow.id == "GP101A1"] <- 4 
x$tow.id[x$tow.id == "GP101A1"] <- "GP101F"
x$tow.number[x$month == 8 & x$day == 23 & x$tow.number > 4] <- x$tow.number[x$month == 8 & x$day == 23 & x$tow.number > 4] - 1

# Fix samplers:
x$samplers[x$samplers == "Yves garlo Larocque"] <- "Yves Larocque"

x$carapace.width[x$carapace.width == 0] <- NA
x$carapace.width[x$tow.id == "GC330F" & x$crab.number == 29] <- 94.28 # Expected value.           

# Chela height fixes (expected value):
x$chela.height[x$tow.id == "GP197F" & x$crab.number == 51] <- 29.47
x$chela.height[x$tow.id == "GP066F" & x$crab.number == 85] <- 29.47
x$chela.height[x$tow.id == "GP325F" & x$crab.number == 209] <- NA  
x$chela.height[x$tow.id == "GP163F" & x$crab.number == 113] <- NA  
x$chela.height[x$tow.id == "GP053F" & x$crab.number == 30] <- NA   
x$chela.height[x$tow.id == "GP148F" & x$crab.number == 19] <- NA    
x$chela.height[x$tow.id == "GC297F" & x$crab.number == 27] <- NA  

# IMPORTANT - Check number of crab with missing chela height measurements:
x[which(x$carapace.width >= 95 & is.na(x$chela.height)), ]

# Load tow data: 
y <- read.scset(year = 2019)

# Remove fake crab:
x <- x[-which(x$tow.id == "GC330F" & x$crab.number == 1), ]

# Check for data with non-valid tows:
index <- match(x$tow.id, y$tow.id)
table(y$valid[index])

# Check that dates match:
which((y$year[index] != x$year) | (y$month[index] != x$month) | (y$day[index] != x$day))

# Check that tow numbers match:
which(y$tow.number[match(x$tow.id, y$tow.id)] != x$tow.number)

# Check that crab numbers are sequential within tows:
x <- sort(x, by = c("year", "month", "day", "tow.number", "crab.number"))
aggregate(x$crab.number, by = x["tow.id"], function(x) if (length(x) == 1) return(1) else return( unique(diff(x))))

# Check indexing variables:
tmp <- aggregate(x$tow.id, by = cbind(substr(x$tow.id, 2, 2), x[c("month", "day", "tow.number")]), function(x) length(unique(x)))
dim(unique(cbind(substr(x$tow.id, 2, 2), x[c("month", "day", "tow.number")])))[1] == length(unique(x$tow.id))

# Convert to 'scbio' object:
xx <- scbio(x)

tows <- unique(x[c("month", "day", "tow.number", "tow.id")])
for (i in 1:nrow(tows)){
   index <- gsub(" ", "", xx$tow.id) == tows$tow.id[i]
   #file <- paste0("GCR", substr(x$year[i], 3, 4), convert.vector(i, to = "a3", fill = "0"), ".txt")      # Old file name format.
   file <- paste0(tows$tow.id[i], ".txt")
   print(paste0("U:/Snow Crab/Stock Assessment 2019/", file))
   write(xx[index, ], file = paste0("U:/Snow Crab/Stock Assessment 2019/", file)) 
}

# Update R and CSV file versions:
update.scbio(year = 2019)

