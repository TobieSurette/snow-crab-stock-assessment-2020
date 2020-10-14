rm(list=ls())
graphics.off()
library("gulf")

setwd("U:/Snow Crab/Stock Assessment 2019")
years <- 2017:2019

language <- "english"
language <- tolower(language)

tab <- read.csv("U:/Snow Crab/Trawl Survey/2017/Generate Survey Stations 2017/MIF table.csv", header = TRUE)
tab$tow.id <- substr(tab$tow.id, 3, 5)

# Load survey polygon:
data(survey.polygons)
p <- subset(p, label = "sc.survey")
poly <- data.frame(longitude = p[[1]]$x, latitude = p[[1]]$y)
      
# Check that grid identifiers are unique:
#if (any(duplicated(stations$tow.id))) print("There are duplicate tow ID numbers.")

# Read grid scheme from previous year:
mif <- read.mif("U:/Snow Crab/Trawl Survey/2017/Generate Survey Stations 2017/grids2017.mif", mid = FALSE)
for (i in 1:length(mif)){
   tmp <- km2deg(mif[[i]]$x, mif[[i]]$y)
   mif[[i]]$longitude <- tmp$longitude
   mif[[i]]$latitude <- tmp$latitude
}
n <- length(mif) # Number of stations, which defines the sampling grid scheme.]

tow.id <- c('GP002','GP001','GP004','GP003','GP006','GP005','GP008','GP007','GP009','GP011','GP010','GP012','GP013','GP016','GP019','GP018','GP015','GP017',
            'GP014','GP026','GP024','GP025','GP023','GP028','GP022','GP030','GP031','GP021','GP020','GP027','GP036','GP044','GP032','GP040','GP042','GP041',
            'GP039','GP043','GP037','GP029','GP034','GP033','GP038','GP035','GP049','GP046','GP048','GP050','GP047','GP052','GP051','GP045','GP053','GP055',
            'GP058','GP059','GP057','GP056','GP054','GP077','GP068','GP061','GP076','GP067','GP066','GP070','GP063','GP069','GP062','GP060','GP071','GP073',
            'GP065','GP074','GP064','GP072','GP094','GP084','GP088','GP080','GP090','GP079','GP091','GP093','GP095','GP092','GP087','GP082','GP075','GP089',
            'GP086','GP083','GP078','GP085','GP081','GP099','GP113','GP106','GP105','GP097','GP104','GP108','GP102','GP111','GP110','GP100','GP101','GP114',
            'GP103','GP112','GP096','GP107','GP109','GP098','GP133','GP128','GP123','GP127','GP125','GP124','GP116','GP117','GP132','GP119','GP131','GP135',
            'GP129','GP118','GP126','GP130','GP115','GP134','GP121','GP122','GP120','GP154','GP141','GP147','GP149','GP150','GP151','GP146','GP140','GP148',
            'GP137','GP145','GP136','GP144','GP143','GP153','GP152','GP138','GP139','GP142','GP162','GP170','GP159','GP158','GP163','GP164','GP161','GP160',
            'GP167','GP171','GP155','GP168','GP172','GP157','GP169','GP165','GP166','GP156','GP189','GP174','GP184','GP176','GP179','GP177','GP188','GP182',
            'GP186','GP173','GP175','GP178','GP183','GP185','GP180','GP181','GP187','GP192','GP197','GP202','GP198','GP191','GP190','GP196','GP199','GP194',
            'GP205','GP195','GP193','GP204','GP200','GP201','GP203','GP214','GP220','GP206','GP215','GP210','GP221','GP212','GP219','GP211','GP213','GP216',
            'GP209','GP218','GP217','GP207','GP208','GP231','GP234','GP226','GP233','GP229','GP225','GP232','GP222','GP224','GP228','GP223','GP236','GP230',
            'GP227','GP237','GP235','GP238','GP241','GP240','GP248','GP245','GP246','GP244','GP239','GP242','GP247','GP243','GP249','GP251','GP250','GP262',
            'GP264','GP254','GP256','GP265','GP259','GP263','GP252','GP255','GP253','GP258','GP257','GP261','GP260','GP269','GP271','GP270','GP266','GP276',
            'GP267','GP272','GP274','GP268','GP275','GP279','GP281','GP278','GP273','GP282','GP277','GP284','GP280','GP289','GP286','GP283','GP288','GP291',
            'GP285','GP287','GP290','GP293','GP294','GP301','GP298','GP292','GP299','GP302','GP296','GP297','GP300','GP303','GP295','GP305','GP306','GP304',
            'GP309','GP307','GP311','GP308','GP310','GP315','GP313','GP312','GP316','GP319','GP318','GP322','GP327','GP317','GP314','GP321','GP328','GP323',
            'GP325','GP326','GP324','GP320','GP330','GP336','GP332','GP329','GP333','GP337','GP338','GP334','GP335','GP331','GP340','GP339','GP343','GP345',
            'GP346','GP342','GP344','GP341','GP349','GP348','GP347','GP351','GP350','GP352','GP355','GP354','GP353')
for (i in 1:length(mif)) mif[[i]]$tow.id <- tow.id[i]

windows(width = 8.5, height = 8.5)
file <- paste0("Survey Map Summary ", language, " ", min(years), "-", max(years), ".jpg")
jpeg(file, width = 8*480, height = 8*480, res = 8*75)

m <- kronecker(t(matrix(1:4, ncol = 2)), matrix(1, ncol = 5, nrow = 5))
m <- rbind(0, cbind(0, m, 0), 0)
layout(m)
par(mar = c(0,0,0,0))  #   c(bottom, left, top, right) 

for (year in years){

   s <- read.scset(year = year, valid = 1)
   s <- s[substr(s$tow.id, 2, 2) != "C", ]
   s$station <- ""
   s$station[substr(s$tow.id, 6, 9) %in% c("F", "FR")] <- "P"
   s$station[substr(s$tow.id, 6, 9) %in% c("A1")] <- "A1"
   s$station[substr(s$tow.id, 6, 9) %in% c("A2")] <- "A2"
   s$station[substr(s$tow.id, 6, 9) %in% c("A3")] <- "A3"

   tab <- data.frame(grid = 1:length(mif), tow.id = unlist(lapply(mif, function(x) x$tow.id)))
   tab$tow.id <- as.character(tab$tow.id)
   tab$type <- NA
   tab$type[match(substr(s$tow.id, 1, 5), tab$tow.id)] <- s$station
   tab$lon[match(substr(s$tow.id, 1, 5), tab$tow.id)] <- longitude(s)
   tab$lat[match(substr(s$tow.id, 1, 5), tab$tow.id)] <- latitude(s) 
   tab$type[!(tab$tow.id %in% substr(s$tow.id, 1, 5))] <- "Abandonned"
   tab$misplaced <- FALSE

   # Get complete survey grid scheme:
   for (i in 1:length(mif)){   
      # Assign survey stations to grids:
      tab$clon[i] <- mean(mif[[i]]$longitude[1:4])
      tab$clat[i] <- mean(mif[[i]]$latitude[1:4])
      p <- as.polygon(mif[[i]]$longitude, mif[[i]]$latitude)
      index <- which(in.polygon(p, longitude(s), latitude(s)))
      if (length(index) > 0){
         if (is.na(tab$type[i]) | (tab$type[i] != "Abandonned")) tab$misplaced[i] <- TRUE
      }
   }
   tab$misplaced <- !tab$misplaced
   tab$misplaced[which(tab$type == "Abandonned")] <- FALSE

   if (year == years[1]) gulf.map(axis.side = c(2,3))
   if (year == years[2]) gulf.map(axis.side = c(3,4))
   if (year == years[3]) gulf.map(axis.side = c(1,2))
   cols <- c("grey85", "green", "yellow", "red", "black")
   for (i in 1:length(mif)){
      border <- NA
      col <- cols[match(tab$type[i], c("P", "A1", "A2", "A3", "Abandonned"))  ]
      plot(as.polygon(mif[[i]]$longitude, mif[[i]]$latitude), col = col)
   }
   for (i in 1:length(mif)){
      if (tab$misplaced[i]){
         lines(c(tab$lon[i], tab$clon[i]), c(tab$lat[i], tab$clat[i]), lwd = 1)
         points(tab$lon[i], tab$lat[i], pch = 21, bg = "grey20")
      }
   } 
   
   text(par("usr")[1] + 0.85 * diff(par("usr"))[1:2],
        par("usr")[3] + 0.9 * diff(par("usr"))[3:4],
         year, cex = 1.5)
   # Draw fishing zone lines without drawing the shoreline:
   data("fishing.zone.polygons")
   p <- subset(fishing.zone.polygons, species = 2526)
   for (i in 1:length(p)){
      for (j in 1:length(p[[i]])){
         if ("x" %in% names(p[[i]][[j]])){
            xx <- p[[i]][[j]]$x
            yy <- p[[i]][[j]]$y
            dd <- depth(xx, yy)
            index <- which(depth(xx, yy) < -30) # Threshold depth for crab zone lines.
            if (length(index) > 0){
               for (k in 1:length(index)){
                  if (index[k] > 1) lines(xx[(index[k]-1):index[k]], yy[(index[k]-1):index[k]], col = "grey10", lwd = 1.5)
                  if (index[k] < length(xx)) lines(xx[index[k]:(index[k]+1)], yy[index[k]:(index[k]+1)], col = "grey10", lwd = 1.5)
               }
            }
         }
      }
   }
   
   print(year)
   print(tab$tow.id[tab$type == "Abandonned"])
   print(tab$tow.id[tab$misplaced])
}
plot.new()
if (language == "french") str <- c("Primaire", "Alternative 1", "Alternative 2", "Alternative 3", "Abandonnée", "Dehors du quadrilatère visé")
if (language == "english") str <- c("Primary", "Alternate 1", "Alternate 2", "Alternate 3", "Abandoned", "Outside target grid")
legend("center", 
       legend = str,
       pch = c(rep(22, length(cols)), 21),
       pt.bg = c(cols, "grey10"),
       pt.cex = c(rep(3.5, length(cols)), 1), cex = 1.5, 
       lwd = c(rep(NA, length(cols)), 1))
     
dev.off()
dev.off()
