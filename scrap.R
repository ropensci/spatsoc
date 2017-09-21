updatePackageVersion <- function(packageLocation ="."){
  ## Read DESCRIPTION file
  desc <- readLines(file.path(packageLocation, "DESCRIPTION"))

  ## Find the line where the version is defined
  vLine <- grep("^Version\\:", desc)

  ## Extract version number
  vNumber <- gsub("^Version\\:\\s*", "", desc[vLine])

  ## Split the version number into two; a piece to keep, a piece to increment
  versionNumber <- strsplit(vNumber, "\\.")[[1]]
  versionParts <- length(versionNumber)
  vNumberKeep <- paste(versionNumber[1:(versionParts-1)], sep= "", collapse= ".")
  vNumberUpdate <- versionNumber[versionParts]

  ## Replace old version number with new one (increment by 1)
  oldVersion <- as.numeric(vNumberUpdate)
  newVersion <- oldVersion + 1

  ## Build final version number
  vFinal <- paste(vNumberKeep, newVersion, sep = ".")

  ## Update DESCRIPTION file (in R)
  desc[vLine] <- paste0("Version: ", vFinal )

  ## Update the actual DESCRIPTION file
  writeLines(desc, file.path(packageLocation, "DESCRIPTION"))

  ## Return the updated version number to screen
  return(vFinal)
}
updatePackageVersion()

data(locs)



utm <- '+proj=utm +zone=21 ellps=WGS84'
BuildHRs(hrType = 'mcp', hrParams = list(percent = 50), DT = locs, projection = utm)
BuildHRs(hrType = 'kernel', DT = locs, projection = utm)

hrprms <- list(percent = 95)
do.call(adehabitatHR::mcp, list(xy = BuildPts(locs, projection = utm), hrprms))

locs[, datetime:= as.POSIXct(paste(idate, itime))]

GroupTimes(locs, 'datetime', '2 hours')

#####HOURLY
timeTh <- '2 hours'

# nTime <- data.table::as.ITime(strsplit(roundUnit, ' hour')[[1]], format = '%H')
nTime <- unlist(data.table::tstrsplit(timeTh, ' ', keep = 1, type.convert = TRUE)) * 60 * 60
nHour <- unlist(data.table::tstrsplit(timeTh, ' ', keep = 1, type.convert = TRUE))

locs[]

hyenas <- data.table::fread('input/Striped hyenas Carmel Israel.csv')

GroupTimes(hyenas, 'timestamp', '30 minutes')[]#[, .(timestamp, newtime)]

hyenas[1:1000, .(timestamp, data.table::minute(timestamp), data.table::minute(timestamp) %% 30,
                data.table::minute(timestamp) %% 30 > 15)]


DT[, {new <- ifelse((data.table::minute(get(timeField)) %% nTime) > (nTime / 2),
                (as.POSIXct(get(timeField)) +
                   (nTime - (data.table::minute(get(timeField)) %% nTime)) * 60),
                as.POSIXct(get(timeField)) -
                  ((data.table::minute(get(timeField)) %% (nTime)) * 60) -
                  data.table::second(get(timeField)))
          class(new) <- c("POSIXct", "POSIXt")
          new}]


hyenas[(data.table::minute(timestamp) %% 30) > (30 / 2),
       .(timestamp,
         sec = data.table::second(timestamp),
         sub = (as.POSIXct(timestamp) +
                  ((30 - (data.table::minute(timestamp) %% 30)) * 60) -
                  (data.table::second(timestamp))),
         woSec = as.POSIXct(timestamp) - data.table::second(timestamp))]

hyenas[is.na(newtime),
   newtime :=
     as.POSIXct(timestamp) -
     ((data.table::minute(timestamp) %% (30)) * 60) -
     data.table::second(timestamp)]






# locs[, .(itime, even = itime %% nTime, odd = ((itime - (60*60)) %% nTime))]
nHour
nTime

locs[data.table::hour(itime) %% nHour != 0]
locs[data.table::hour(datetime) %% nHour,
     newTime := datetime + ((nHour/2) * 60 * 60)]

locs[data.table::hour(datetime) %% nHour == 0,
     newdate := datetime]
locs[data.table::hour(datetime) %% nHour != 0,
     newdate := datetime + ((nHour / 2) * 60 * 60)]

#[(itime %% nTime) > (nTime / 2)]

#######MINUTE
timeTh <- '30 minutes'

nTime <- unlist(data.table::tstrsplit(timeTh, ' ', keep = 1, type.convert = TRUE))
timeField <- 'timestamp'

hyenas[data.table::minute(timestamp) %% 30 > 15, newtime := as.POSIXct(timestamp) + 15 * 60]
hyenas[order(newtime), .(newtime, timestamp)]

hyenas[(data.table::minute(timestamp) %% nTime) > (nTime / 2),
       .(minutes = data.table::minute(timestamp),
         mod30 = data.table::minute(timestamp) %% nTime,
         timestamp,
         as.POSIXct(timestamp) + ((30 - (data.table::minute(timestamp) %% nTime)) * 60),
         data.table::minute(timestamp) + (30 - data.table::minute(timestamp) %% nTime))]


# if minute
# DT[(data.table::minute(timestamp) %% nTime) > (nTime / 2),
#    as.POSIXct(timestamp) + ((30 - (data.table::minute(timestamp) %% nTime)) * 60)]

# if 1 hour
timeTh <- '1 hour'
nTime <- unlist(data.table::tstrsplit(timeTh, ' ', keep = 1, type.convert = TRUE))

hyenas[minute(timestamp) %% (1 * 60) > 30,
       as]


# DT[(data.table::minute(timestamp) %% nTime) > (nTime / 2),
#    as.POSIXct(timestamp) + ((30 - (data.table::minute(timestamp) %% nTime)) * 60)]


locs[order(-idate), .(itime, minutes = itime %% nTime)][order(-minutes)][1:100]





data.table::as.ITime(unlist(data.table::tstrsplit(timeTh, ' ', type.convert = TRUE, keep = 1)),
                     format = '%H')


library(data.table)

x.col <- 'X_COORD'
y.col <- 'Y_COORD'
id.field <- 'ID'
buffer.width <- 50
utm <- '+proj=utm +zone=21 ellps=WGS84'
proj.fields <- c("EASTING", "NORTHING")
data(locs)

utm21N <- '+proj=utm +zone=21 ellps=WGS84'
locs[, c('EASTING', 'NORTHING') := data.table::as.data.table(rgdal::project(cbind(X_COORD, Y_COORD), utm21N))]

locs[, ihour := data.table::hour(itime)]

locs[, timeGroup := paste(.BY[1], .BY[2], sep = '_'), by = .(idate, ihour)]
# group...
locs[, c('group') := .(a$group)]

range(locs[, uniqueN(ID), by = group]$V1)

fake <- readRDS('/home/alecr/Documents/Local-git/implementations/CAH and RDH/output/cleaned-locs.Rds')
system.time(
f <- spatsoc::GroupPts(fake, idField = "ANIMAL_ID", bufferWidth = 50,
                          timeField = "roundtime",
                          projection = '+proj=utm +zone=21 ellps=WGS84')
)

e <- spatsoc::Randomizations()

colnames(fake)

f

########### TIME ROUNDING
# watch the >= <= < >

# roundUnit = data.table::as.ITime('00:05:00')
roundUnit = data.table::as.ITime('01:00:00')
locs[(data.table::minute(itime) %% data.table::minute(roundUnit)) >
       (data.table::minute(roundUnit) / 2),
     .(ID, idate, itime,
       itime + roundUnit)]
minute(roundUnit)

locs[minute(itime) > 30, .(itime, hour = hour(itime))]
                           # timePlus30 = itime + as.ITime('00:30:00'),
                           # hourPlus30 = hour(itime + as.ITime('00:30:00')))]

# if there are no rows, then it returns the origin must be supplied error..

########### TIME ROUNDING




##############
d <- data.table(dates =  unique(locs[ID == 'mr2009a31', idate]))[,
                rands :=  sample(1:length(dates), length(dates))]
locs[ID == 'mr2009a31', randDate := d[rands == .GRP, dates], by = idate]
z <- locs[ID == 'mr2009a31', .(ID, idate, randDate)]
##############

x <- locs[, {d <- data.table(dates =  unique(idate))[,
                        rands :=  sample(1:length(dates), length(dates))]
        .SD[, .(randDate = d[rands == .GRP, dates]), by = idate]
        },
     by = ID]
id.field <- 'ID'
date.field <- 'idate'
v <- merge(locs, x, by = c(id.field, date.field))#[, .(ID, idate, randDate)]
v

a[, sID := sample(id)]
# but this just samples within group
a[, bsID := sample(id), by = group]

# so alternatively, we can sample from all possible IDs
ls.ids <- unique(a$id)
a[, lsID := sample(ls.ids, size = .N), by = group]

unique.counts <- data.table(id = a[, uniqueN(id), by = group]$V1,
                            sID = a[, uniqueN(sID), by = group]$V1,
                            bsID = a[, uniqueN(bsID), by = group]$V1,
                            lsID = a[, uniqueN(lsID), by = group]$V1,
                            group = a[, 1, by = group]$group)


# the sample across the entire dataset returns different group size
unique.counts[id != sID]
# the group replacement only samples from within group but group suze is good
unique.counts[id != bsID]
# group replacement from separate dict/list of ids returns same group size and any ids
unique.counts[id != lsID]

z <- data.table(time = c(1,1,1,2,2,2,3,3,3,4,4,4),
                group = c(1,2,3,4,4,4,5,6,7,8,8,9),
                id = seq(1:3),
                day = c(1,1,1,1,1,1,2,2,2,2,2,2))
ls.n <- unique(z$id)

z[, sample(ls.n, 1), by = id]


########### functional....
cj <- CJ(t = z$day, i = z$id, unique = TRUE)

for(u in seq(1:nrow(cj))){
  intr <- intersect(which(z$day == cj[u, t]), which(z$id == cj[u, i]))
  set(z, i = intr,
      j = 's', value = sample(ls.n, 1))
}
z
##################

cj <- CJ(t = locs$idate, i = locs$ID, unique = TRUE)
ls.ids <- unique(locs$ID)
for(u in seq(1:nrow(cj))){
  intr <- intersect(which(locs$idate == cj[u, t]), which(locs$ID == cj[u, i]))
  set(locs, i = intr,
      j = 's', value = sample(ls.ids, 1))
}
locs


z <- locs[ihour < 15, .(idate, ID, group)]

#### THIS?
z[, s := sample(ls.ids, 1), by = .(idate, ID)]
#####

b <- spatsoc::Randomizations(locs, 'ID', 'group', 'spiegel', 'idate')
b
system.time(
for(i in 1:100){
spatsoc::Randomizations(locs, 'ID', 'group', 'hourly')
}
)


empty <- locs[NA][, .(seq(1:100), ID, group)]
d <- locs[rep(1:.N, 100)][,Indx:=1:.N,by=ID]
d
# mapview::mapview(BuildPts(l, crs = utm, coordFields = c("EASTING", "NORTHING"),
#                           idField = id.field))

## NEAREST ##
a <- Nearest(locs, coordFields = c("EASTING", "NORTHING"), idField = id.field)
a

## sub in roundtime here
a <- Nearest(locs, 'idate',
                    coordFields = c("EASTING", "NORTHING"), idField = id.field)
a

a <- Nearest(locs, 'idate', 'group',
             coordFields = c("EASTING", "NORTHING"), idField = id.field)
a

## PTS ##
a <- spatsoc::BuildPts(locs, projection = utm,
                       coordFields = proj.fields, idField = id.field)
a
a <- spatsoc::GroupPts(locs, 50, 'timeGroup', projection = utm,
                       coordFields = proj.fields, idField = id.field)
a
### LINES ###############
a <- spatsoc::BuildLines(locs, projection = utm, coordFields = proj.fields,
                idField = id.field)
a
a <- spatsoc::GroupLines(locs, projection = utm, coordFields = proj.fields, idField = id.field)
a

a <- spatsoc::GroupLines(locs, 100, coordFields = proj.fields, projection = utm, idField = id.field)
a
mapview::mapview(a)
a <- spatsoc::GroupLines(locs, 100, coordFields = proj.fields, timeField = 'idate',
                         projection = utm, idField = id.field)
a

##################

a <- BuildPts(locs, projection = utm, coordFields = proj.fields,
               idField = id.field)
a


a <- BuildHRs('mcp', locs, projection = utm, coordFields = proj.fields,
               idField = id.field)
a
a <- GroupPts(locs, 50, timeField = 'date',
              projection = utm, idField = id.field)
a


a <- GroupHRs('mcp', locs, projection = utm, idField = id.field)
a

mapview::mapview(a)

a <- PairwiseDist(locs, 'date', idField = id.field)
a



spatsoc::grp_pts()
a <- spatsoc::grp_lines(buffer.width = 10, sp.lines = build_lines(locs, utm, idField = 'ANIMAL_ID'))
a

mapview::mapview(build_lines(locs, utm, idField = 'ANIMAL_ID'))

a <- grp_lines(locs, 50, utm, idField = 'ANIMAL_ID')
a





Mrlocs[, c("meanDistance", "distID") :=
         MeanPairwiseDists(.SD), by = timeGroup,
       .SDcols = c(id.col, east.col, north.col)]

Mrlocs[, c("meanDistance", "distID") :=
         MeanPairwiseDists(.SD), by = timeGroup,
       .SDcols = c(id.col, east.col, north.col)]


spatsoc::mean_pairwise_dist(locs, 'date', 'ANIMAL_ID')

z <- locs[, .(
  timeGroup, idate, itime, E = `utm-easting`, N = `utm-northing`,
  ID = `individual-local-identifier`, group)]


locs <- data.table::fread('input/Striped hyenas Carmel Israel.csv')
locs <- locs[!is.na(`utm-easting`)]
locs[, idate := data.table::as.IDate(timestamp)]
locs[, ihour := data.table::hour(data.table::as.ITime(timestamp, format = '%F %T'))]

id.field <- 'individual-local-identifier'
buffer.width <- 50
utm <- '+init=epsg:32636'
proj.fields <- c("utm-easting", "utm-northing")


locs[, itime := (data.table::as.ITime(timestamp, format = '%F %T'))]
locs[, round(itime, 'hours')]
locs[minute(itime) > 30, .(itime, hour = hour(itime),
                           timePlus30 = itime + as.ITime('00:30:00'),
                           hourPlus30 = hour(itime + as.ITime('00:30:00')))]
