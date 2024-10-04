# Test direction_to_centroid
context('test direction_to_centroid')

library(spatsoc)

DT <- fread('../testdata/DT.csv')
id <- 'ID'
datetime <- 'datetime'
timethreshold <- '20 minutes'
threshold <- 50
coords <- c('X', 'Y')
timegroup <- 'timegroup'
group <- 'group'

DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
group_times(DT, datetime = datetime, timethreshold)
group_pts(DT, threshold = threshold, id = id,
          coords = coords, timegroup = timegroup)
centroid_group(DT, coords = coords, group = group, na.rm = TRUE)

clean_DT <- copy(DT)

test_that('DT is required', {
  expect_error(direction_to_centroid(DT = NULL))
})

test_that('arguments required, otherwise error detected', {
  expect_error(direction_to_centroid(DT, coords = NULL),
               'coords req')
})

test_that('column names must exist in DT', {
  expect_error(direction_to_centroid(DT, coords = rep('potato', 2)),
               'potato field')
})

test_that('coords are correctly provided or error detected', {
  expect_error(direction_to_centroid(DT, coords = c('X', NULL)),
               'coords requires a vector')
  expect_error(direction_to_centroid(DT, coords = c('X', 'ID')),
               'coords must be numeric')
})
