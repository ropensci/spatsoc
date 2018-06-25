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

group_times(Dt, datetime = 'datetime', threshold = '6 hours')
group_pts(Dt, threshold = 600000000, id = 'id', timegroup = 'timegroup',
          coords = c('X', 'Y'))
Dt <- Dt[order(-datetime)]
group_pts(DT = Dt, threshold = 1000, id = 'id', timegroup = 'timegroup',
          coords = c('X', 'Y'))
x <- x + 1
Dt[, (paste0('groupSample', x)) := group]
Dt
library(ggplot2)
x <- x + 1
Dt[, .N, get(paste0('groupSample', x))][, qplot(N)]



d <- data.table::dcast(df, formula = group ~ get(idField), fun.aggregate = length,
                       value.var = 'group')

gbi_df <- data.matrix(d[, !'group', with=FALSE])

rownames(gbi_df) <- d$group

gbi.net_df <- get_network(gbi_df, data_format="GBI",association_index="SRI")



Dt[, .N, groupNoOrder][, qplot(groupNoOrder, N)]
Dt[, .N, groupReverse][, qplot(groupReverse, N)]


Dt[, qplot(groupRever)]
Dt[, .N, groupRever]
Dt[, .N, groupNoOrder]


group_times(Dt, datetime = 'datetime', threshold = '40 days')


group_polys(Dt, area = FALSE, hrParams = list(percent =96), hrType = 'mcp',
            projection = utm, id = 'id',
            coords = c('X', 'Y'),
            splitBy = 'timegroup')

## BUFFALO ========
# Dt <- fread('input/Buffalo.csv')
Dt <- fread('tests/testdata/buffalo.csv')
utm <- '+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
# Dt[, datetime := gsub('T', ' ', gsub('Z', '', timestamp))]
# # Dt[, idate := as.IDate(timestamp)]
# Dt[, datetime := as.POSIXct(timestamp)]
Dt[, datetime := as.POSIXct(datetime)]
# Dt[, ID := `individual-local-identifier`]
# Dt[, X := `utm-easting`]
# Dt[, Y := `utm-northing`]
# fwrite(Dt[sample(.N, 2000), .(X, Y, ID, datetime)],
#        'tests/testdata/buffalo.csv')
# Dt[, jul := yday(datetime)]
# Dt[, yr := year(datetime)]
# Dt[, potato := ID]
Dt[, id := ID]
group_times(Dt, datetime = 'datetime', threshold = '30 days')

Randomizations(Dt, id= 'ID', groupField = 'group',
               randomType = 'spiegel', dateField = 'datetime',
               splitBy = 'yr', iterations = 10)
group_lines(Dt, threshold = 0,
           projection = utm,
           coords = c('X', 'Y'), id = 'ID')

group_times(Dt, datetime = 'datetime', threshold = '15 minutes')
group_lines(Dt, threshold = 50, projection = utm, id = 'id',
            coords = c('X', 'Y'),
            timegroup = 'timegroup')

# if (as.list(sys.call(-5))[[1]] != 'group_lines') {

## DAILY ========
Dt <- fread('input/Daily')
Dt[, idate := as.IDate(timestamp)]
Dt[, posix := as.POSIXct(timestamp)]
Dt[, grp := tstrsplit(`individual-local-identifier`, '_')[[1]]]
Dt[, X := `location-long`]
Dt[, Y := `location-lat`]

Dt <- Dt[grp == 'ngelleehon']

group_times(Dt, 'posix', '10 minutes')
Dt[, uniqueN(posix)]
group_pts(Dt, 100, time = 'timegroup', coords = c('X', 'Y'),
         id = 'ID')
## DAILY ====...

############# SF SF SF SF ################
library(sf)
Dt <- l[EASTING < 644180.4 & NORTHING < 5297469]
Dt <- l
## SF
# pts <- st_multipoint(as.matrix(
#   Dt[, .(EASTING, NORTHING)],
#   ncol = 2
#   ))
# system.time({
pts <- st_as_sf(Dt, coords = c('EASTING', 'NORTHING'))

st_sf(Dt[, (coords)])
st_crs(pts) <- utm
bufs <- st_buffer(pts, 50)
un <- st_cast(st_union(bufs), 'POLYGON')
int <- st_intersects(pts, un)
OUT <- data.table::data.table(Dt$ID,
                              unlist(int))
# })

profvis::profvis({
  group_pts(l, 100, datetime = 'timegroup', projection = utm)
  group_ptsSF(l, 100, datetime = 'timegroup', projection = utm)
  group_ptsIGRAPH(l, 100, datetime = 'timegroup', projection = utm)
})

l

coords <- c('EASTING', 'NORTHING')
st_sf(Dt[, coords, with=FALSE])

l[, withinGroup := {
  pts <- sf::st_as_sf(.SD, coords = coords)
  sf::st_crs(pts) <- utm
  bufs <- sf::st_buffer(pts, 50)
  un <- sf::st_cast(st_union(bufs), 'POLYGON')
  # int <- sf::st_intersects(pts, un)
  unlist(sf::st_intersects(pts, un))
  # data.table::setnames(
  # data.table::data.table(get(id), unlist(int, FALSE, FALSE))#,
  #   c(id, 'withinGroup'))
},
by = timegroup, .SDcols = coords]
l
l[, withinGroup := NULL]
microbenchmark::microbenchmark(
  group_pts(l, 100, projection = utm)
  , times = 15)
microbenchmark::microbenchmark(
  group_ptsSF(l, 100, projection = utm)
  , times = 15)

system.time(
  group_pts(l, 100, projection = utm)
)
system.time(
  group_ptsSF(l, 100, projection = utm)
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
  spPts <- BuildPts(Dt, utm)
  buffers <- rgeos::gBuffer(spPts, width = 50, byid = FALSE)
  ovr <- sp::over(spPts, sp::disaggregate(buffers))
  OUT <- data.table::data.table(Dt$ID,
                                unlist(ovr))
})

data.table::setnames(
  data.table::data.table(get(id),
                         unlist(ovr)),
  c(id, 'withinGroup'))


Dt[ovrDt, withinGroup := withinGroup, on = c(id, splitBy)]
Dt[, group := .GRP, by = c(splitBy, 'withinGroup')][, withinGroup := NULL][]
################# SF SFS FSF SF SF ########################


library(magrittr)
DT %>%
  group_times(datetime = 'date', threshold = '15 minutes') %>%
  group_pts(threshold = 50, id = 'id', coords = c('x', 'y'),
            timegroup = 'timegroup')


utm <- '+proj=utm +zone=21 ellps=WGS84'
group_times(DT = DT, datetime = 'date', threshold = '1 day')
group_lines(DT, threshold = 50, projection = utm,
            id = 'id', coords = c('x', 'y'),
            timegroup = 'timegroup')


pander::pander(
  group_pts(DT,
            threshold = 50, id = 'id', coords = c('x', 'y'),
            timegroup = 'timegroup')[order(group)][
              1:nRows, .(id, x, y, timegroup, group)])




# from vignette ----
library(ggplot2); library(data.table); library(spatsoc);
library(gridExtra); library(ggthemes)
data(DT)
subDT <- DT[EASTING > 6e+05]
subDT[, day := yday(datetime)]

utm <- '+proj=utm +zone=21 ellps=WGS84'

pt.grpsgroup_pts(DT, threshold =50, id = 'ID', coords = c('EASTING', 'NORTHING'))
pt.grps[, groupN := .N, by = group]
pt.grps[ID != 'A'][sample(5), groupN := 2]

hrs <- build_polys(hrType = 'mcp', DT = subDT, projection = utm)
hr.grps <- group_polys(area = FALSE, hrType = 'mcp', DT = subDT, projection = utm)
hrs@data$grps <- hr.grps$group
hr <- merge(as.data.table(hrs@data), as.data.table(broom::tidy(hrs)),
            by = 'id')

l <- group_lines(subDT, projection = utm, datetime = 'day')
l[, groupN := .N, by = group]

v <- merge(subDT, l)

g1 <- ggplot(subDT) +
  geom_point(aes(EASTING, NORTHING, color = factor(ID), group = ID), alpha = 0.8) +
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(),
        legend.position = c(0, 1), legend.justification = c(0, 1)) +
  labs(color = 'ID') +
  scale_color_brewer(type='qual', palette='Dark2')

g2 <- ggplot(subDT) +
  geom_path(aes(EASTING, NORTHING, color = ID, group = ID)) + guides(color = FALSE) +
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(),
        legend.position = c(0, 1)) +
  scale_color_brewer(type='qual', palette='Dark2')

g3 <- ggplot(broom::tidy(hrs)) +
  geom_polygon(aes(long, lat, group = id, color = id, fill = factor(id)), alpha = 0.4) +
  guides(color = FALSE, fill = FALSE ) + labs(fill = 'ID') +
  theme(axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank()) +
  scale_color_brewer(type='qual', palette = 'Dark2') +
  scale_fill_brewer(type='qual', palette = 'Dark2')

g4 <- ggplot() +
  geom_point(aes(EASTING, NORTHING, shape = ID), alpha = 0.4, data = pt.grps) +
  scale_shape_manual(values = c(21, 22, 23)) +
  geom_point(aes(EASTING, NORTHING, color = factor(groupN)),  pt.grps[groupN > 1]) +
  guides(fill = FALSE, shape = FALSE, color = FALSE) +
  theme(axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank()) +
  labs(color = 'Group') +
  scale_color_manual(values = c('1' = '#b5b3b8', '2' = '#ff3333'))

g5 <- ggplot(v[order(datetime)]) +
  geom_path(aes(EASTING, NORTHING, group = ID, color = factor(groupN))) +
  guides(color = FALSE) +
  theme(axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position = c(0, 1)) +
  scale_color_manual(values = c('1' = '#6a696b', '2' = '#ff3333'))

g6 <- ggplot(hr) +
  geom_polygon(aes(long, lat, group = group, fill = factor(grps)),color = 'grey') + labs(fill = 'Group', color = 'ID')  + guides(fill = FALSE, color = FALSE) +
  theme(axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank()) +
  scale_fill_manual(values = c('1' = '#6a696b', '2' = '#ff3333'))

gridExtra::grid.arrange(g1, g2, g3, g4, g5, g6, nrow = 2,
                        top = 'Point                                                       Line                                                       Polygon')
