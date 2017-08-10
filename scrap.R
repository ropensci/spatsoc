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

locs <- data.table::fread('input/mock-locs.csv')
locs <- locs[X_COORD != 0 & Y_COORD != 0]

x.col <- 'X_COORD'
y.col <- 'Y_COORD'
id.field <- 'ANIMAL_ID'
buffer.width <- 500
utm21N <- '+proj=utm +zone=21 ellps=WGS84'

# locs[, round(FIX_TIME)]
# locs[, floor(data.table::as.ITime(FIX_TIME))]

proj.fields <- c('EASTING', 'NORTHING')
locs[, (proj.fields) := data.table::as.data.table(rgdal::project(cbind(get(x.col), get(y.col)),
                                            utm21N))]

# ____________


locs[, roundtime := lubridate::round_date(as.POSIXct(paste(idate, itime)), 'hour')]

l <- locs[(FIX_DATE == '2010-05-01' | FIX_DATE == '2010-05-02')& FIX_TIME < '00:01:30']
l <- locs[FIX_DATE == '2010-05-01' & FIX_TIME < '00:01:30']

b <- locs[, Nearest(.SD, 'roundtime', TRUE,
            coordFields = c("EASTING", "NORTHING"), idField = id.field)]
b
Nearest(l, coordFields = c("EASTING", "NORTHING"), idField = id.field)

b[ID == neighbor]
b[, .N, by =ID]

b <- l[, Nearest(.SD,
                    coordFields = c("EASTING", "NORTHING"), idField = id.field)]
b


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!             b[ID == neighbor]
b

b[ID == neighbor]

mapview::mapview(BuildPts(l, crs = utm21N, coordFields = c("EASTING", "NORTHING"),
                          idField = id.field))

timeField <- 'roundtime'
b[, nTime := data.table::uniqueN(get(timeField)), by = ID]
b[, nTimeAll :=data.table::uniqueN(get(timeField))]

b[nTime == nTimeAll]

b[, .N, by = .(ID, neighbor)]
b[, .N, by = roundtime]

c <- unique(b[, .(prop = .N / nTime), by = .(ID, neighbor)])



a <- locs[, Nearest(.SD, 'FIX_DATE',
                    coordFields = c("EASTING", "NORTHING"), idField = id.field)]
a

a <- spatsoc::GroupPts(locs, 50, 'FIX_DATE', crs = utm21N,
              coordFields = c("EASTING", "NORTHING"), idField = id.field)
a

a <- spatsoc::GroupLines(locs, 50, 'FIX_DATE', crs = utm21N, idField = id.field)
a

locs[, {spLines <- BuildLines(.SD, utm21N, proj.fields, id.field)
        spLines
}, by = FIX_DATE]
a

locs[, {spLines <- BuildLines(.SD, utm21N, proj.fields, id.field)
        buffers <- rgeos::gBuffer(spLines, width = 50, byid = FALSE)
        # ovr <- sp::over(spLines, sp::disaggregate(buffers), returnList = T)
        length(spLines)
        },
     by = FIX_DATE]

buffers <- rgeos::gBuffer(spLines, width = bufferWidth, byid = FALSE)
ovr <- sp::over(spLines, sp::disaggregate(buffers), returnList = T)
list(id = names(ovr), group = unlist(ovr))

locs[, {spLines <- BuildLines(.SD, utm21N, proj.fields, id.field); spLines@bbox}, by = FIX_DATE]




a <- BuildPts(locs, crs = utm21N, coordFields = c("EASTING", "NORTHING"),
               idField = id.field)
a
a <- BuildLines(locs, crs = utm21N, coordFields = c("EASTING", "NORTHING"),
                 idField = id.field)

b <- rgeos::gBuffer(a, width = 50, byid = FALSE)
o <- sp::over(a, sp::disaggregate(b), returnList = T)
o


a <- BuildHRs('mcp', locs, crs = utm21N, coordFields = c("EASTING", "NORTHING"),
               idField = id.field)
a
a <- GroupPts(locs, 50, timeField = 'FIX_DATE',
              crs = utm21N, idField = id.field)
a
a <- GroupLines(locs, 50, crs = utm21N, idField = id.field)
a
a <- GroupHRs('mcp', locs, crs = utm21N, idField = id.field)
a

mapview::mapview(a)

a <- PairwiseDist(locs, 'FIX_DATE', idField = id.field)
a



spatsoc::grp_pts()
a <- spatsoc::grp_lines(buffer.width = 10, sp.lines = build_lines(locs, utm21N, idField = 'ANIMAL_ID'))
a

mapview::mapview(build_lines(locs, utm21N, idField = 'ANIMAL_ID'))

a <- grp_lines(locs, 50, utm21N, idField = 'ANIMAL_ID')
a





Mrlocs[, c("meanDistance", "distID") :=
         MeanPairwiseDists(.SD), by = timeGroup,
       .SDcols = c(id.col, east.col, north.col)]

Mrlocs[, c("meanDistance", "distID") :=
         MeanPairwiseDists(.SD), by = timeGroup,
       .SDcols = c(id.col, east.col, north.col)]


spatsoc::mean_pairwise_dist(locs, 'FIX_DATE', 'ANIMAL_ID')


locs <- locs[NORTHING > 5360000 & EASTING < 650000]

l <- spatsoc::grp.lines(locs[, list(EASTING, NORTHING, ID = ANIMAL_ID)],
                   crs = utm21N)
l


lst <- data.table:::split.data.table(locs[, list(EASTING, NORTHING)],
                                     locs[, list(ANIMAL_ID)])

`%do%` <- foreach::`%do%`

# Make each list of an individuals locs into a spatial lines [sp, mapview, foreach]
sp.lines <- foreach::foreach(i = lst, id = names(lst), .combine = sp::rbind.SpatialLines) %do% {
  mapview::coords2Lines(
    matrix(c(i[['EASTING']], i[['NORTHING']]), ncol = 2),
                        ID = id,
                        proj4string = sp::CRS(utm21N))
}
sp.lines

data.table::setorder(locs, FIX_DATE, FIX_TIME)
locs

in.dt <- locs[, list(EASTING, NORTHING, ID = ANIMAL_ID)]
lst <- data.table:::split.data.table(in.dt[, list(EASTING, NORTHING)],
                                     in.dt[, list(ID)])
#  ------------------------------------------------------------------------

as.matrix(c(lst[1]$EASTING, lst[1]$NORTHING))
lst[1][['EASTING']]

d <- lst[1]
d


`%do%` <- foreach::`%do%`
lst <- lst[1]
# Make each list of an individuals locs into a spatial lines [sp, mapview, foreach]
sp.lines <- foreach::foreach(i = lst, id = names(lst), .combine = rbind) %do% {
  mapview::coords2Lines(matrix(c(i[['EASTING']], i[['NORTHING']]), ncol = 2),
                        ID = id,
                        proj4string = sp::CRS(crs))
}
sp.lines

# #### grps lines
#
# # Split up the data.table by collar ID into lists
# lst <- data.table:::split.data.table(locs[, .(EASTING, NORTHING)], list(locs[[id.field]]))
#
# `%do%` <- foreach::`%do%`
#
# # Make each list of an individuals locs into a spatial lines [sp, mapview, foreach]
# sp.lines <- foreach::foreach(i = lst, id = names(lst), .combine = rbind) %do% {
# mapview::coords2Lines(as.matrix(i[, get(proj.fields[1]), get(proj.fields[2])]), ID = id,
#                proj4string = sp::CRS(utm21N))
# }
#
#
# # Buffer those lines by a preassigned buffer width
# buffers <- rgeos::gBuffer(sp.lines, width = buffer.width, byid = FALSE)
#
# # Find which buffers overlap each other and return as a list
# ovr <- sp::over(sp.lines, sp::disaggregate(buffers), returnList = T)
# dt <- data.table::data.table(id = names(ovr), group = ovr)
# dt
#

#
#
#
#
#
#
#
#
#
#
#
#
#
# #### grps pts
# # ##### this is not important for the package.... (prepping our own data)
# sp.pts <- sp::SpatialPointsDataFrame(locs[, ..proj.fields],
#                                  proj4string = sp::CRS(utm21N),
#                                  data = locs[, .(id = get(id.field))])
#
# #
# # spatsoc::grp.pts(sp.pts, 50)
# buffers <- rgeos::gBuffer(sp.pts, width = buffer.width, byid = TRUE)
#
#
# disbuf <- rbind(sp::disaggregate(buffers))
# # b <- SpatialPolygonsDataFrame(disbuf, data.frame(1:29))
#
# # ggplot(dt) + geom_point(aes(EASTING, NORTHING),
# #                         color = o) +
# #   guides(color = guide_colorbar()) +
# #   # polygon calls fortify, and the default x y are long lat with id for grouping
# #   geom_polygon(data = b, aes(long, lat, group = id, color = id), alpha = 0.2)
