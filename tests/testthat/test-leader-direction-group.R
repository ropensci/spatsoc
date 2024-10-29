# Test leader_direction_group
context('test leader_direction_group')

library(spatsoc)
library(units)

DT <- fread('../testdata/DT.csv')
id <- 'ID'
datetime <- 'datetime'
timethreshold <- '20 minutes'
threshold <- 50
coords <- c('X', 'Y')
timegroup <- 'timegroup'
group <- 'group'
projection <- 32736

DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
group_times(DT, datetime = datetime, timethreshold)
group_pts(DT, threshold = threshold, id = id,
          coords = coords, timegroup = timegroup)
centroid_group(DT, coords = coords, group = group, na.rm = TRUE)
direction_step(DT = DT, id = id, coords = coords, projection = projection)
direction_group(DT, coords = coords)


