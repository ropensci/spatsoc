# Test direction_to_centroid
context('test direction_to_centroid')

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
  copy_DT <- copy(DT)
  setnames(copy_DT, 'centroid_X', 'potato_X')
  expect_error(direction_to_centroid(copy_DT, coords = coords),
               'centroid_')
})

test_that('coords are correctly provided or error detected', {
  expect_error(direction_to_centroid(DT, coords = c('X', NULL)),
               'coords requires a vector')
  expect_error(direction_to_centroid(DT, coords = c('X', 'ID')),
               'coords must be numeric')
})

test_that('direction_centroid column succesfully detected', {
  copyDT <- copy(clean_DT)[, direction_centroid := 1]
  expect_message(
    direction_to_centroid(copyDT, coords = coords),
    'direction_centroid column will be overwritten'
  )
})

test_that('no rows are added to the result DT', {
  copyDT <- copy(clean_DT)

  expect_equal(nrow(copyDT),
               nrow(direction_to_centroid(copyDT, coords = coords)))
})

test_that('one column added to the result DT', {
  copyDT <- copy(clean_DT)

  expect_equal(ncol(copyDT) + 1,
               ncol(direction_to_centroid(DT, coords = coords)))
})

test_that('column added to the result DT is a double with units rad', {
  expect_type(direction_to_centroid(DT, coords = coords)$direction_centroid,
              'double')
  expect_equal(
    units(
      direction_to_centroid(DT, coords = coords)$direction_centroid)$numerator,
    'rad')
})

test_that('returns a data.table', {
  expect_s3_class(direction_to_centroid(DT, coords = coords), 'data.table')
})

