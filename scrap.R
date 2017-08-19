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
utm <- '+proj=utm +zone=21 ellps=WGS84'
proj.fields <- c("EASTING", "NORTHING")
data(locs)
# locs[, roundtime := lubridate::round_date(as.POSIXct(paste(idate, itime)), 'hour')]


locs <- data.table::fread('input/Striped hyenas Carmel Israel.csv')
id.field <- 'individual-local-identifier'
buffer.width <- 50

utm <- '+init=epsg:32636'
proj.fields <- c("utm-easting", "utm-northing")
locs <- locs[!is.na(`utm-easting`)]
locs[, idate := data.table::as.IDate(timestamp)]

# ____________

# mapview::mapview(BuildPts(l, crs = utm, coordFields = c("EASTING", "NORTHING"),
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
a <- spatsoc::BuildPts(locs, projection = utm,
                       coordFields = proj.fields, idField = id.field)
a
a <- spatsoc::GroupPts(locs, 50, 'idate', projection = utm,
              coordFields = proj.fields, idField = id.field)
a
### LINES ###############
a <- spatsoc::BuildLines(locs, projection = utm, coordFields = proj.fields,
                idField = id.field)
a
# with new data:
# unable to find an inherited method for function ‘is.projected’ for signature ‘"data.table"’
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
