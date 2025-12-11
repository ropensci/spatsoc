# Test centroid_group
context('test centroid_group')

library(spatsoc)

DT <- fread('../testdata/DT.csv')
id <- 'ID'
datetime <- 'datetime'
timethreshold <- '20 minutes'
threshold <- 50
coords <- c('X', 'Y')
timegroup <- 'timegroup'
group <- 'group'
utm <- 32736


DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
group_times(DT, datetime = datetime, threshold = timethreshold)
group_pts(DT, threshold = threshold, id = id,
          coords = coords, timegroup = timegroup)

clean_DT <- copy(DT)

test_that('DT is required', {
  expect_error(centroid_group(DT = NULL))
})

test_that('arguments required, otherwise error detected', {
  expect_error(centroid_group(DT, group = group, coords = 'X'),
               'coords must be length 2')
  expect_error(centroid_group(DT, coords = coords, group = NULL),
               'group must be provided')
})

test_that('column names must exist in DT', {
  expect_error(centroid_group(DT, coords = rep('potato', 2)),
               'potato field')
  expect_error(centroid_group(DT, coords = coords, group = 'potato'),
               'potato field')
})

test_that('coords are correctly provided or error detected', {
  expect_error(centroid_group(DT, coords = c('X', NULL)),
               'coords must be length 2')
  expect_error(centroid_group(DT, coords = c('X', 'ID')),
               'coords must be of class numeric')
})

test_that('centroid column succesfully detected', {
  copyDT <- copy(clean_DT)[, centroid_X := 1]
  expect_message(
    centroid_group(copyDT, coords = coords),
    'centroid_X column will be overwritten'
  )
})

test_that('no rows are added to the result DT', {
  copyDT <- copy(clean_DT)

  expect_equal(nrow(copyDT),
               nrow(centroid_group(copyDT, coords = coords)))
})

test_that('two columns added to the result DT', {
  copyDT <- copy(clean_DT)

  expect_equal(ncol(copyDT) + 2,
               ncol(centroid_group(DT, coords = coords)))
})

test_that('two columns added to the result DT are doubles', {
  expect_type(centroid_group(DT, coords = coords)$centroid_X, 'double')
  expect_type(centroid_group(DT, coords = coords)$centroid_Y, 'double')
})

test_that('returns a data.table', {
  expect_s3_class(centroid_group(DT, coords = coords), 'data.table')
})


DT <- data.table(
  group = c(1, 1, 2, 2),
  X = c(10, 20, 10, NA),
  Y = c(10, 20, 10, 20)
)

test_that('xy results are expected', {
  expect_equal(
    centroid_group(copy(DT), coords)[
      group == 1, unique(centroid_X)],
    15
  )

  expect_equal(
    centroid_group(copy(DT), coords)[
      group == 1, unique(centroid_X)],
    15
  )

  expect_equal(
    centroid_group(copy(DT), coords)[
      group == 2, unique(centroid_X)],
    10
  )
})

DT <- data.table(
  group = c(1, 1, 2, 2),
  X = c(10, 20, 10, NA),
  Y = c(10, 20, 10, 20)
)

get_geometry(DT, c('X', 'Y'), 32736, output_crs = FALSE)

test_that('geometry results are expected', {
  expect_equal(
    centroid_group(copy(DT))[
      group == 1, sf::st_coordinates(unique(centroid))[,1]],
      setNames(15, 'X')
  )

  expect_equal(
    centroid_group(copy(DT))[
      group == 1, sf::st_coordinates(unique(centroid))[,2]],
    setNames(15, 'Y')
  )

  expect_equal(
    centroid_group(copy(DT))[
      group == 2, sf::st_coordinates(unique(centroid))[,1]],
    setNames(10, 'X')
  )

})


test_that('sfc interface', {
  expect_message(centroid_group(copy(DT), crs = utm))
})
