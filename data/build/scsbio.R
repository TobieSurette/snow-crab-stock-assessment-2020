library(gulf.data)

x <- read.table("data/raw/scs.bio.2020.csv", header = TRUE, sep =",", stringsAsFactors = FALSE, fileEncoding = "Windows-1252")
names(x)          <- gsub("_", ".", tolower(names(x)))
x$tow.id          <- deblank(toupper(x$gpnumber))
x$tow.number      <- x$trawl.number
x$sex             <- as.numeric(gsub("[*]", "", x$sex))
x$egg.colour      <- as.numeric(gsub("[*]", "", x$egg.color))
x$eggs.remaining  <- as.numeric(gsub("[*]", "", x$percent.eggs))
x$gonad.colour    <- as.numeric(gsub("[*]", "", x$gonade))
x$shell.condition <- deblank(gsub("[*]", "", x$shell.condition))

# Extract time stamp:
x$time <- unlist(lapply(strsplit(x$timestamp, " "), function(x) x[2]))

x$missing.legs <- apply(x[, c(paste0("l", 1:5), paste0("l", 1:5))], 1, paste0, collapse = "")
x$year  <- as.numeric(substr(x$date, 1, 4))
x$month <- as.numeric(substr(x$date, 6, 7))
x$day   <- as.numeric(substr(x$date, 9, 10))
x$date  <- as.character(date(x))

x$shell.condition.mossy <- ""
x$shell.condition.mossy[substr(toupper(x$shell.condition), 2, 2) == "M"] <- "M"
x$shell.condition <- as.numeric(substr(x$shell.condition, 1, 1))
if (!("tag.number" %in% names(x)))    x$tag.number <- NA
if (!("weight" %in% names(x)))        x$weight <- NA
if (!("durometer" %in% names(x)))     x$durometer <- NA
if (!("trap.code" %in% names(x)))     x$trap.code <- NA
if (!("abdomen.width" %in% names(x))) x$abdomen.width <- NA

# Remove irrelevant data:
x <- x[order(time(x)), ]
index <- is.na(x$carapace.width) & is.na(x$shell.condition) & is.na(x$gonad.colour) & is.na(x$egg.colour) & is.na(x$eggs.remaining)
x <- x[which(!index), ]
x <- x[x$sex %in% c(1:2), ]

# Fix comments:
x$comment <- paste0(toupper(substr(x$comment, 1, 1)), tolower(substr(x$comment, 2, nchar(x$comment))))
x$comment[x$comment == "Vien de mu‚"] <- "Vient de muer"
x$comment[x$comment == "Pattes bris‚es"] <- "Pattes brisees"

# Check missing legs:
table(unlist((strsplit(x$missing.legs, ""))))

# Chela height corrections:
x$chela.height[x$chela.height == "."] <- ""
x$chela.height <- as.numeric(x$chela.height)

# Check for inconsistencies is sex measurements:
which(x$sex == 2 & !is.na(x$chela.height))
index <- which((x$sex == 1) & ((!is.na(x$gonad.colour)) | (!is.na(x$egg.colour)) | (!is.na(x$eggs.remaining))))

# Missing eggs remaining:
x$eggs.remaining[which((x$egg.colour == 4) & is.na(x$eggs.remaining))] <- 0

# Carapace width corrections:
x$carapace.width <- abs(x$carapace.width)

# Female size corrections:
x$carapace.width[which(x$sex == 2 & is.mature.scsbio(x) & x$carapace.width < 30)] <- NA


# Delete irrelevant data and re-order variables:
vars <- c("date", "time", "tow.id", "tow.number", "crab.number", "sex",
          "carapace.width", "chela.height", "abdomen.width", "durometer", "weight",
          "shell.condition", "missing.legs", "shell.condition.mossy",
          "gonad.colour", "egg.colour", "eggs.remaining", "samplers", "tag.number", "trap.code", "comment")
x <- x[vars]

# Date fix:
x$date[(x$date == "2020-07-12") & (x$tow.id == "GP354F")] <- "2020-07-13"

# Tow number fixes:
x$tow.number[(x$tow.id == "GP049A1")] <- 2
x$tow.number[(x$tow.id == "GP026F")] <- 3
x$tow.number[(x$tow.id == "GP024F")] <- 4
x$tow.number[(x$tow.id == "GP036F")] <- 5
x$tow.number[(x$tow.id == "GP046F")] <- 6
x$tow.number[(x$tow.id == "GP067F")] <- 7
x$tow.number[(x$tow.id == "GP091A1")] <- 9
x$tow.number[(x$tow.id == "GP066A1")] <- 11
x$tow.number[(x$tow.id == "GP048F")] <- 12

# Write to file:
write.csv(x, file = "data/scs.bio.2020.csv", row.names = FALSE)

# Update gulf.data repo:
if (file.exists("C:/Users/SuretteTJ/Desktop/gulf.data")){
   write.csv(x, file = "C:/Users/SuretteTJ/Desktop/gulf.data/inst/extdata/scs.bio.2020.csv", row.names = FALSE)
}

# Update gulf.data repo:
if (file.exists("/Users/crustacean/Desktop/gulf.data")){
   write.csv(x, file = "/Users/crustacean/Desktop/gulf.data/inst/extdata/scs.bio.2020.csv", row.names = FALSE)
}
