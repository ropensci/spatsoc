# Test distance_to_leader
context('test distance_to_leader')

library(spatsoc)

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
direction_group(DT)
leader_direction_group(DT, coords = coords, group = group, return_rank = TRUE)

# Removing group with missing leader
DT <- copy(DT)[group != 868]

clean_DT <- copy(DT)

test_that('DT is required', {
  expect_error(distance_to_leader(DT = NULL))
})

test_that('arguments required, otherwise error detected', {
  expect_error(distance_to_leader(DT, coords = NULL, group = group),
               'coords req')
  expect_error(distance_to_leader(DT, coords = coords, group = NULL),
               'group column name required')
})

