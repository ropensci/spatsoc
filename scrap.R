
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

library(spatsoc)
data(locs)

utm <- '+proj=utm +zone=21 ellps=WGS84'


# randomizations
# x <- GroupTimes(locs, 'datetime', '2 hours')
z <- GroupPts(locs, 100, 'datetime', '2 hours', projection = utm)
z[, .N, by = group][order(-N)]

z
# function(DT, idField, iterations, groupField, randomType, dateField = NULL) {
# Iterations(z, 'ID', 10, 'group', 'hourly', 'datetime')
aa = Randomizations(z, 'ID', 'group', randomType = 'spiegel', dateField = 'datetime', 2)
aa[, GroupPts(.SD, 100, 'randomDateTime', '2 hours', projection = utm),
   by = iter]
GroupPts(aa, 100, 'randomDateTime', '2 hours', projection = utm)
aa
aa[, .(datetime, yday, randomYday, randomDateTime)]

aa[, as.POSIXct(
  paste0(year(datetime), "-",
        month(datetime), "-",
        randomYday, " ",
        as.ITime(datetime)))
  ]


aa[, difday := randomYday - yday]

aa[, .(datetime,
       dtPlus = datetime + (86400 * difday),
       ydayPlus = yday(datetime + (86400 * difday)),
       yday,
       randomYday)]



aa[, as.ITime(datetime)]

ggplot(aa[order(randomYday)]) +
  geom_path(aes(EASTING, NORTHING, color = yday, group = ID)) #+ guides(color = FALSE) +
  facet_wrap(~ID)
  #theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(),
  #      legend.position = c(0, 1))

aa[, .(range(yday), range(randomYday)), by = ID]


# between ids (hourly)
# essentially just swapping the id of each point

z[, uniqueN(ID), by = timegroup]
z[, .(ID, randomID = sample(ID)), by = timegroup]
fwrite(z[, .(ID, randomID = sample(ID)), by = timegroup],
       'betweenIDsHourly.csv')

Randomizations(z, 'ID', 'group', 'hourly', 'timegroup')

# between ids (daily)
# swap all IDs in the day with a randomly sampled other ID

z[, uniqueN(ID), by = yday(datetime)]
v <- z[, .(ID = unique(ID)), by = yday(datetime)]
v[, randomID := sample(ID), by = yday]
v[, uniqueN(ID), by = yday]

z[, yday := yday(datetime)]
s <- merge(z, v, on = 'yday')
s[, uniqueN(randomID), by = .(yday, ID)]

j <- Randomizations(z, 'ID', 'group', 'daily', 'ID')
j[, uniqueN(randomID), by = .(yday, ID)]#[, max(V1)]

fwrite(j, 'betweenIDsDaily.csv')
#

## Speigel
z
z[, yday := yday(datetime)]
v <- z[, .(yday = unique(yday)), by = ID]
v[, randomYday := sample(yday)]
v

s <- merge(z, v, on = c('yday', 'ID'))
s[, .(uniqueN(yday), uniqueN(randomYday)), by = ID]

s[, uniqueN(randomYday) ,by = .(ID, yday)]

fwrite(Randomizations(z, 'ID', 'group', 'spiegel', 'datetime'),
       'spiegel.csv')




if(randomType == 'hourly'){
  # TODO: this isn't really 'hourly' it's just not 'daily'
  if(!is.null(dateField)) warning('dateField ignored since randomType is hourly')

  lsIDs <- unique(DT[[idField]])
  randIDs <- sample(lsIDs)

  DT[, randomID := randIDs[.GRP], by = idField]

} else if(randomType == 'daily'){
  if(is.null(dateField)) stop('must provide a dateField if daily randomType chosen')
  listIDs <- unique(DT[[idField]])
  # TODO: is it just daily or should this flex to specified time?

  # sample 1 id from the list and repeat it for the number of rows
  # so the dimensions input are the same returned
  DT[, .(randomID = rep(sample(listIDs, 1), .N), group = get(groupField)),
     by = c(dateField, idField)]
} else if(randomType == 'spiegel'){
  randomDatesDT <- DT[, {d <- data.table(dates =  unique(get(dateField)))
  d[, randomN :=  sample(1:length(dates), length(dates))]
  .SD[, .(randomDate = rep(d[randomN == .GRP, dates], .N),
          group = get(groupField)),
      by = dateField]
  },
  by = idField]
  # DT[randomDatesDT, on = c(idField, dateField)]

} else {
  stop('must provide either hourly or daily for randomType')
}
}

# TODO: work on var names
# TODO: remove old ID once we are satisfied?
# TODO: change 'randomDatesDT'
# TODO: optional N random iterations?
# TODO: optionally return the original ID for checking?














### grouptime
locs[, datetime := as.POSIXct(datetime, tz = 'UTC')]
utm <- '+proj=utm +zone=21 ellps=WGS84'
# locs[, datetime := datetime + (runif(.N, 0, 60) * 60)]
# locs[, datetime := datetime + (runif(.N, 0, 3) * 3600)]
GroupTimes(locs[order(datetime)], 'datetime',
           '1 hour')[1:40]
GroupTimes(locs[order(datetime)], 'datetime',
           '60 minutes')[1:40]

z <- GroupTimes(locs[order(datetime)], 'datetime',
           '1 hour')

GroupTimes(locs, 'datetime', '2 hours')[1:30]

## 15 minute interval, following new dtm method
dtm <- locs[, IDateTime(datetime)]
dtm[, itime := itime + (runif(.N, 0, 60) * 60)]

nMins <- 20
if(!is.integer(nMins)) nMins <- as.integer(nMins)
dtm[, c('grp', 'hr') := NULL]

dtm[, .(minute(itime),
        nMins,
        mod = minute(itime) %% nMins,
        isModLtHalf = minute(itime) %% nMins < (nMins / 2))][sample(.N, 5)]

dtm[minute(itime) %% nMins < (nMins / 2) ,
    mint := nMins * (minute(itime) %/% nMins)]
dtm[minute(itime) %% nMins >= (nMins / 2),
    mint := nMins * ((minute(itime) %/% nMins) + 1L)]

dtm[, .(itime, .GRP), by = .(mint, hour(itime), idate)]


# ? hour interval
nHours <- 3

z[, .(hr = hour(new),
      nHours = nHours,
      clean = hour(new) %% nHours == 0,
      moddiv = unclass(as.ITime(new)) %/% (nHours * 3600L),
      mult = nHours * ((unclass(as.ITime(new)) %/% (nHours * 3600L))))]

z
dtm <- z[, IDateTime(datetime)]
dtm[, itime := itime + (runif(.N, 0, 60) * 60)]
dtm[, itime := itime + (runif(.N, 0, 3) * 3600)]

# if 1 hour
dtm[minute(itime) > 30, hr := hour(itime) + 1L]
dtm[minute(itime) < 30, hr := hour(itime)]
dtm[, grp := .GRP, by = .(idate, hr)]

# if 2 hour
nHours <- 12
if(!is.integer(nHours)) nHours <- as.integer(nHours)
# ONLY THESE MAKE SENSE 1, 2, 3, 4, 6, 8, 12, 24
dtm[, c('grp', 'hr') := NULL]

# case 1 - Where minute floored, and hour is on the interval, return the hour
# dtm[minute(itime) < 30 & hour(itime) %% nHours == 0, hr4 := hour(itime)]#[order(-hr)]
dtm[minute(itime) < 30 & hour(itime) %% nHours < (nHours / 2),
    hr4 := nHours * (hour(itime) %/% nHours)]#hour(itime)]#[order(-hr)]
# case 2 - Where minute is > 30, but the hour is still less than half the nhours, return the hour
dtm[minute(itime) >= 30 & hour(itime) %% nHours < (nHours / 2) ,
    hr4 := nHours * (hour(itime) %/% nHours)]#hour(itime)]#[order(-hr)]
# case 3 - where minute is floored but hours is greater than half the nhours
dtm[minute(itime) < 30 & hour(itime) %% nHours >= (nHours / 2),
    hr4 := nHours * ((hour(itime) %/% nHours) + 1L)]
# case 4 - where is >30 and
dtm[minute(itime) >= 30 & hour(itime) %% nHours >= (nHours / 2), #hour(itime) %% nHours != 0,
    hr4 := nHours * ((hour(itime) %/% nHours) + 1L)]

# ARE THEY MUTUALLY EXCLUSIVE?
dtm[hour(itime) %% nHours < (nHours / 2) ,
    hr := nHours * (hour(itime) %/% nHours)]#[order(-hr)]
dtm[hour(itime) %% nHours >= (nHours / 2),
    hr := nHours * ((hour(itime) %/% nHours) + 1L)]

dtm[, .(itime, hour = hour(itime),
        hr, hr4,
        mod = hour(itime) %% nHours,
        nHours,
        modDivHalf = hour(itime) %% nHours < (nHours / 2))][1:50]
dtm[hr != hr4]

dtm[1:50]
dtm[is.na(hr)]
# group em
dtm[, grp := .GRP, by = .(idate, hr)]


structure


z[hour(new) %% nHours == 0 & minute(new) < 30, n := hour(new)]
z[hour(new) %% nHours == 0 & minute(new) > 30, n := hour(new) + 1]
z[hour(new) %% nHours < (nHours / 2) & minute(new) < 30, n := hour(new)]
z[hour(new) %% nHours > (nHours / 2) & minute(new) < 30, n := hour(new)]
z[hour(new) %% nHours < (nHours / 2) & minute(new) < 30, n := hour(new)]

z[, .(hour = hour(new), ModMinHalf = hour(new) %% nHours > (nHours / 2),
      Mod = hour(new) %% nHours, HalfN = nHours / 2)]
z[, m := {m <- ifelse(hour(new) %% nHours > (nHours / 2),
                      as.POSIXct(new) + ((hour(new) %/% nHours) + 1) * nHours,
                  ((hour(new) %/% nHours)) * nHours)
     class(m) <- c('POSIXct', 'POSIXct')
     m}]

z[, m := {m <- ifelse(hour(new) %% nHours > (nHours / 2),
                      as.POSIXct(as.IDate(new),
                                 as.ITime(
                                   paste0(
                                     ((hour(new) %/% nHours) + 1) * nHours,
                                     ": 00")
                                   )
                                 ),
                      as.POSIXct(as.IDate(new), as.ITime(paste0(((hour(new) %/% nHours)) * nHours, ": 00"))))
class(m) <- c('POSIXct', 'POSIXct')
m}]





z
# as.POSIXct(as.IDate(new), as.ITime(paste0(((hour(new) %/% nHours) + 1) * nHours, ": 00"),


z[, .(hour(new), nHours, hour(new) %% nHours > (nHours / 2),
      sub = nHours - hour(new),
      mod = hour(new) %% nHours,
      moddiv = hour(new) %/% nHours,
      ((hour(new) %/% nHours) + 1) * nHours,
      ((hour(new) %/% nHours)) * nHours,
      )]




z[, hour(new) <-((hour(new) %/% nHours) + 1) * nHours]
(20 %/% 3 * 3) + 1

((20 %/% 3) + 1) * 3
(1 + (20 %/% 3)) * 3

# 20 -> 21



z[, .(m, timegrp, as.numeric(new))]
z[, .(hour = hour(new), ModMinHalf = hour(new) %% nHours > (nHours / 2),
      Mod = hour(new) %% nHours,
      (nHours - hour(new)),
      new,
      as.POSIXct(new) + (hour(new) %% nHours * 3600),
      as.POSIXct(new) - ((hour(new) %% nHours) * 3600))]
(3 - 2) * 3600

z[, as.POSIXct(new) + (nHours - hour(new) * 3600)]
z[, .(hour(new), (nHours - hour(new) * 3600))]
z[, (hour(new) %% nHours * 3600)]
z[, as.POSIXct(new) - (hour(new) %% nHours * 3600)]
z[, timegrp := .GRP, by = m]
z
data.table::yday

`z[, data.table::second(new) ]
5 + (3 - 2)
10 - 1

newdates <- DT[, .(new =
{new <- ifelse((data.table::minute(get(timeField)) %% nTime) > (nTime / 2),
               (as.POSIXct(get(timeField)) +
                  (nTime - (data.table::minute(get(timeField)) %% nTime)) * 60) -
                 data.table::second(get(timeField)),
               as.POSIXct(get(timeField)) -
                 ((data.table::minute(get(timeField)) %% (nTime)) * 60) -
                 data.table::second(get(timeField)))
class(new) <- c("POSIXct", "POSIXct")
new})]


glins <- GroupPts(locs, bufferWidth = 50, projection = utm, timeField = 'datetime',
                    timeThreshold = '1 day')
ls.ids <- unique(glins[['ID']])
rand.ids <- sample(ls.ids)

glins[, {lsIDs <- unique(ID)
         randIDs <- sample(lsIDs)
         data.table(ID)[, rand := randIDs[.GRP], by = ID]

         # randIDs[.GRP]
         },
      by = timegroup][1:100]
glins[1:100]




locs[, {
  lsdays <- unique(yday(datetime))
  randays <- sample(lsdays)

  return(yday(datetime), ), by = ID]


DT[, .(randomID = rep(sample(listIDs, 1), .N), group = get(groupField)),
   by = c(dateField, idField)]


# hourly.. but its daily
glins[, .(randomID = sample(ls.ids, .N),
          ID),
     by = group]

listIDs <- unique(DT[[idField]])
# TODO: is it just daily or should this flex to specified time?

# sample 1 id from the list and repeat it for the number of rows
# so the dimensions input are the same returned
glins[, .(randomID = rep(sample(ls.ids, 1), .N), group),
   by = .(datetime, ID)]

# sample 1 id from the list and repeat it for the number of rows
# so the dimensions input are the same returned
locs[, .(randomID = rep(sample(ls.ids, 1), .N), group = get(groupField)),
   by = c(dateField, idField)]


locs




locs[, day := data.table::yday(datetime)]
l <- GroupLines(locs, projection = utm, timeField = 'day')
l[, groupN := .N, by = group]

v <- merge(locs, l)

ggplot(v[order(datetime)]) +
  geom_path(aes(EASTING, NORTHING, color = ID, group = ID)) + guides(color = FALSE) +
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(),
        legend.position = c(0, 1))

library(ggplot2)
ggplot(v[order(datetime)]) +
  # geom_path(aes(EASTING, NORTHING,
  #               group = ID), color = 'orange',
  #           data = v[groupN > 1][order(datetime)]) +
  geom_path(aes(EASTING, NORTHING,
                group = ID, color = groupN)) +
  guides(color = FALSE)

utm <- '+proj=utm +zone=21 ellps=WGS84'
b <- BuildHRs(hrType = 'mcp', hrParams = list(percent = 50), DT = locs, projection = utm)
b

GroupPolys(TRUE, spPolys = b)


inters <- rgeos::gIntersection(b, b, byid = TRUE)

data.table::data.table(area = lapply(b@polygons, FUN=function(x) {slot(x, 'area')}))[,
                       c('ID1', 'ID2') := data.table::tstrsplit(
                         sapply(b@polygons, FUN=function(x) {slot(x, 'ID')}), ' ',
                         type.convert = TRUE)]


pi <- rgeos::gIntersection(b, b, byid = TRUE)
# Extract areas from polygon objects then attach as attribute
areas <- data.frame(area=sapply(pi@polygons, FUN=function(x) {slot(x, 'area')}))
# row.names(areas) <-
library(data.table)
library(rgeos)


data.table::data.table(area=lapply(pi@polygons, FUN=function(x) {slot(x, 'area')}),
data.table::tstrsplit(
  sapply(pi@polygons, FUN=function(x) {slot(x, 'ID')}), ' ')
)

# Combine attributes info and areas
attArea <- sp::spCbind(pi, areas)

# For each field, get area per soil type
aggregate(area~field + soil, data=attArea, FUN=sum)


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

locs[, timegroup := paste(.BY[1], .BY[2], sep = '_'), by = .(idate, ihour)]
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
a <- spatsoc::GroupPts(locs, 50, 'timegroup', projection = utm,
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
         MeanPairwiseDists(.SD), by = timegroup,
       .SDcols = c(id.col, east.col, north.col)]

Mrlocs[, c("meanDistance", "distID") :=
         MeanPairwiseDists(.SD), by = timegroup,
       .SDcols = c(id.col, east.col, north.col)]


spatsoc::mean_pairwise_dist(locs, 'date', 'ANIMAL_ID')

z <- locs[, .(
  timegroup, idate, itime, E = `utm-easting`, N = `utm-northing`,
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
