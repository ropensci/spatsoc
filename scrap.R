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


library(data.table)

x.col <- 'X_COORD'
y.col <- 'Y_COORD'
id.field <- 'ID'
buffer.width <- 50
utm <- '+proj=utm +zone=21 ellps=WGS84'
proj.fields <- c("EASTING", "NORTHING")
data(locs)

utm21N <- '+proj=utm +zone=21 ellps=WGS84'
locs[, c('EASTING', 'NORTHING') := as.data.table(rgdal::project(cbind(X_COORD, Y_COORD), utm21N))]

locs[, ihour := hour(itime)]

locs[, timeGroup := paste(.BY[1], .BY[2], sep = '_'), by = .(idate, ihour)]
# group...
locs[, c('group') := .(a$group)]

range(locs[, uniqueN(ID), by = group]$V1)

rand_fake_spiegel = Randomizations(locs, idField = "ID", groupField = "group",
               randomType = "spiegel", dateField = "idate")



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
v <- merge(locs, x, by = c('ID', 'idate'))[, .(ID, idate, randDate)]


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

b <- spatsoc::Randomizations(locs, 'ID', 'group', 'daily', 'idate')
spatsoc::Randomizations(locs, 'ID', 'group', 'hourly')



# mapview::mapview(BuildPts(l, crs = utm, coordFields = c("EASTING", "NORTHING"),
#                           idField = id.field))

## NEAREST ##
a <- Nearest(locs, coordFields = c("EASTING", "NORTHING"), idField = id.field)
a

a <- Nearest(locs, 'date',
                    coordFields = c("EASTING", "NORTHING"), idField = id.field)
a

a <- Nearest(locs, 'date', 'group',
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
