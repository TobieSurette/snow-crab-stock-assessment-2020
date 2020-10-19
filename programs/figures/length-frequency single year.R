library(gulf)
library(gulf.spatial)

# Load survey data:
year <- 2020
category <- c("MI", "MM")
survey <- "regular"
jpeg <- FALSE
language <- "english"

# Load set data:
s <- read.scsset(year = year, valid = 1)
s <- s[survey(s) == survey, ]

# Load biological data:
b <- read.scsbio(year = year, valid = 1)


for (i in 1:length(category)){
   ss <- s
   bb <- b[is.category(b, category = category[i]), ]
   import(ss, fill = 0) <- freq(bb, by = key(s))

   print(years[i])
   index <- which(b$sex == sex & b$year == years[i])
   vars <- c("year", "tow.id", "grid")
   f <- freq(b[index, ], category = cvars, by = vars)
   f <- f[!is.na(f$year), ]
   fvars <- setdiff(names(f), c(vars, "category"))

   fi <- f[f$category == cvars[1], ]
   fm <- f[f$category == cvars[2], ]

   # Merge frequencies:
   si <- s[s$year == years[i], ]
   si[fvars] <- 0
   sm <- si
   index <- match(fi[vars], si[vars])
   si[index, fvars] <- fi[fvars]
   index <- match(fm[vars], sm[vars])
   sm[index, fvars] <- fm[fvars]

   # Calculate densities:
   si[, fvars] <- 1000000 * si[, fvars] / repvec(si$swept.area, ncol = length(fvars))
   sm[, fvars] <- 1000000 * sm[, fvars] / repvec(sm$swept.area, ncol = length(fvars))

   # Disaggregate data using 10x10 minute grid:
   if (disaggregate & (years[i] <= 2010)){
      si <- aggregate(si[, fvars], by = si[c("year", "grid")], mean)
      sm <- aggregate(sm[, fvars], by = sm[c("year", "grid")], mean)
   }

   # Calculate mean densities:
   ti <- apply(si[fvars], 2, mean)
   tm <- apply(sm[fvars], 2, mean)


