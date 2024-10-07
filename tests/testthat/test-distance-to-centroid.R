# Test distance_to_centroid
context('test distance_to_centroid')

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
  expect_error(distance_to_centroid(DT = NULL))
})

test_that('arguments required, otherwise error detected', {
  expect_error(distance_to_centroid(DT, coords = NULL),
               'coords req')
  expect_error(distance_to_centroid(DT, coords = coords, group = NULL,
                                    return_rank = TRUE),
               'group column name required')
  expect_error(distance_to_centroid(DT, coords = coords, group = group,
                                    return_rank = NULL),
               'return_rank')
})

test_that('column names must exist in DT', {
  expect_error(distance_to_centroid(DT, coords = rep('potato', 2)),
               'potato field')
  expect_error(distance_to_centroid(DT, coords = coords, group = 'potato',
                                    return_rank = TRUE),
               'group column')
  copy_DT <- copy(DT)
  setnames(copy_DT, 'centroid_X', 'potato_X')
  expect_error(distance_to_centroid(copy_DT, coords = coords),
               'centroid_')
})

test_that('coords are correctly provided or error detected', {
  expect_error(distance_to_centroid(DT, coords = c('X', NULL)),
               'coords requires a vector')
  copy_DT <- copy(DT)[, X := as.character(X)]
  expect_error(distance_to_centroid(copy_DT, coords = coords),
               'coords must be numeric')
})

test_that('distance_centroid column succesfully detected', {
  copyDT <- copy(clean_DT)[, distance_centroid := 1]
  expect_message(
    distance_to_centroid(copyDT, coords = coords),
    'distance_centroid column will be overwritten'
  )
})

test_that('no rows are added to the result DT', {
  copyDT <- copy(clean_DT)

  expect_equal(nrow(copyDT),
               nrow(distance_to_centroid(copyDT, coords = coords)))
})

test_that('one column added to the result DT', {
  copyDT <- copy(clean_DT)

  expect_equal(ncol(copyDT) + 1,
               ncol(distance_to_centroid(DT, coords = coords)))
})

test_that('column added to the result DT is a double', {
  expect_type(distance_to_centroid(DT, coords = coords)$distance_centroid,
              'double')
})

test_that('returns a data.table', {
  expect_s3_class(distance_to_centroid(DT, coords = coords), 'data.table')
})

