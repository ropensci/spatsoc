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
library(ggplot2)
# data(locs)

utm <- '+proj=utm +zone=21 ellps=WGS84'
l <- data.table(locs)
# l[, yr := data.table::year(data.table::as.IDate(datetime))]



## BUFFALO ========
DT <- fread('input/Buffalo.csv')
DT[, idate := as.IDate(timestamp)]
DT[, posix := as.POSIXct(timestamp)]
DT[, ID := `individual-local-identifier`]
DT[, X := `utm-easting`]
DT[, Y := `utm-northing`]
# DT[, .SD, .SDcols = cF]
qplot(X, Y, data = DT, color = ID)
GroupTimes(DT, 'posix', '10 minutes')
DT[, uniqueN(posix)]
DT <- DT[1:1000]
GroupPts(DT, 100, time = 'timegroup', coordFields = c('X', 'Y'),
         idField = 'individual-local-identifier')
DT[, timegroup := NULL]

fwrite(DT[, .(X, Y, ID, posix)], 'tests/testdata/buffalo.csv')

## DAILY ====...


## DAILY ========
DT <- fread('input/Daily')
DT[, idate := as.IDate(timestamp)]
DT[, posix := as.POSIXct(timestamp)]
DT[, grp := tstrsplit(`individual-local-identifier`, '_')[[1]]]
DT[, X := `location-long`]
DT[, Y := `location-lat`]

DT <- DT[grp == 'ngelleehon']

GroupTimes(DT, 'posix', '10 minutes')
DT[, uniqueN(posix)]
GroupPts(DT, 1, time = 'timegroup', coordFields = c('X', 'Y'),
         idField = 'individual-local-identifier')
## DAILY ====...

############# SF SF SF SF ################
library(sf)
DT <- l[EASTING < 644180.4 & NORTHING < 5297469]
DT <- l
## SF
# pts <- st_multipoint(as.matrix(
#   DT[, .(EASTING, NORTHING)],
#   ncol = 2
#   ))
# system.time({
pts <- st_as_sf(DT, coords = c('EASTING', 'NORTHING'))

st_sf(DT[, (coords)])
st_crs(pts) <- utm
bufs <- st_buffer(pts, 50)
un <- st_cast(st_union(bufs), 'POLYGON')
int <- st_intersects(pts, un)
OUT <- data.table::data.table(DT$ID,
                              unlist(int))
# })

profvis::profvis({
GroupPts(l, 100, timeField = 'timegroup', projection = utm)
GroupPtsSF(l, 100, timeField = 'timegroup', projection = utm)
GroupPtsIGRAPH(l, 100, timeField = 'timegroup', projection = utm)
})

l

coordFields <- c('EASTING', 'NORTHING')
st_sf(DT[, coordFields, with=FALSE])

l[, withinGroup := {
  pts <- sf::st_as_sf(.SD, coords = coordFields)
  sf::st_crs(pts) <- utm
  bufs <- sf::st_buffer(pts, 50)
  un <- sf::st_cast(st_union(bufs), 'POLYGON')
  # int <- sf::st_intersects(pts, un)
  unlist(sf::st_intersects(pts, un))
  # data.table::setnames(
    # data.table::data.table(get(idField), unlist(int, FALSE, FALSE))#,
  #   c(idField, 'withinGroup'))
},
by = timegroup, .SDcols = coordFields]
l
l[, withinGroup := NULL]
microbenchmark::microbenchmark(
GroupPts(l, 100, projection = utm)
, times = 15)
microbenchmark::microbenchmark(
GroupPtsSF(l, 100, projection = utm)
, times = 15)

system.time(
  GroupPts(l, 100, projection = utm)
)
system.time(
  GroupPtsSF(l, 100, projection = utm)
)


int
un
bufs
st_cast(sf::st_union(bufs, by_feature = TRUE), 'MULTIPOLYGON')

plot(st_geometry(pts))
plot(st_geometry(bufs))
plot(st_geometry(un))
st_distinct

int <- st_intersects(pts, un)

sapply(st_intersects(pts, bufs),
       function(z) if (length(z)==0) NA_integer_ else z[1])
st_cast(bufs)
bufs <- st_buffer(pts, 50)
plot(bufs)
st_geometry(bufs)
point(pts)
## SP
system.time({
spPts <- BuildPts(DT, utm)
buffers <- rgeos::gBuffer(spPts, width = 50, byid = FALSE)
ovr <- sp::over(spPts, sp::disaggregate(buffers))
OUT <- data.table::data.table(DT$ID,
                       unlist(ovr))
})

data.table::setnames(
  data.table::data.table(get(idField),
                         unlist(ovr)),
  c(idField, 'withinGroup'))


DT[ovrDT, withinGroup := withinGroup, on = c(idField, byFields)]
DT[, group := .GRP, by = c(byFields, 'withinGroup')][, withinGroup := NULL][]
################# SF SFS FSF SF SF ########################

