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


x.col <- 'X_COORD'
y.col <- 'Y_COORD'
id.field <- 'ID'
buffer.width <- 50
utm21N <- '+proj=utm +zone=21 ellps=WGS84'
proj.fields <- c("EASTING", "NORTHING")
data(locs)
# locs[, roundtime := lubridate::round_date(as.POSIXct(paste(idate, itime)), 'hour')]


# ____________

# mapview::mapview(BuildPts(l, crs = utm21N, coordFields = c("EASTING", "NORTHING"),
#                           idField = id.field))

## NEAREST ##
a <- locs[, Nearest(.SD, coordFields = c("EASTING", "NORTHING"), idField = id.field)]
a

a <- locs[, Nearest(.SD, 'date',
                    coordFields = c("EASTING", "NORTHING"), idField = id.field)]
a

a <- locs[, Nearest(.SD, 'date', 'group',
                    coordFields = c("EASTING", "NORTHING"), idField = id.field)]
a

## PTS ##
a <- spatsoc::BuildPts(locs, projection = utm21N,
                       coordFields = c("EASTING", "NORTHING"), idField = id.field)
a
a <- spatsoc::GroupPts(locs, 50, 'date', projection = utm21N,
              coordFields = c("EASTING", "NORTHING"), idField = id.field)
a
### LINES ###############
a <- BuildLines(locs, projection = utm21N, coordFields = c("EASTING", "NORTHING"),
                idField = id.field)
a

a <- spatsoc::GroupLines(locs, projection = utm21N, idField = id.field)
a

a <- spatsoc::GroupLines(locs, 100, projection = utm21N, idField = id.field)
a

a <- spatsoc::GroupLines(locs, 100, timeField = 'date', projection = utm21N, idField = id.field)
a

##################

a <- BuildPts(locs, projection = utm21N, coordFields = c("EASTING", "NORTHING"),
               idField = id.field)
a


a <- BuildHRs('mcp', locs, projection = utm21N, coordFields = c("EASTING", "NORTHING"),
               idField = id.field)
a
a <- GroupPts(locs, 50, timeField = 'date',
              projection = utm21N, idField = id.field)
a


a <- GroupHRs('mcp', locs, projection = utm21N, idField = id.field)
a

mapview::mapview(a)

a <- PairwiseDist(locs, 'date', idField = id.field)
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


spatsoc::mean_pairwise_dist(locs, 'date', 'ANIMAL_ID')
