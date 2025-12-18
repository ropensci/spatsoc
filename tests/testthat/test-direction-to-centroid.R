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
utm <- 32736

DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
group_times(DT, datetime = datetime, timethreshold)
group_pts(DT, threshold = threshold, id = id,
          coords = coords, timegroup = timegroup)

DT_without_centroid <- copy(DT)

centroid_group(DT, coords = coords, group = group)

clean_DT <- copy(DT)

test_that('DT is required', {
  expect_error(direction_to_centroid(DT = NULL))
})

test_that('column names must exist in DT', {
  expect_error(direction_to_centroid(DT, coords = rep('potato', 2),
                                     crs = utm),
               'potato field')
  copy_DT <- copy(clean_DT)
  setnames(copy_DT, 'centroid_X', 'potato_X')
  expect_error(direction_to_centroid(copy_DT, coords = coords,
                                     crs = utm),
               'did you run centroid_group')
})

test_that('coords are correctly provided or error detected', {
  expect_error(direction_to_centroid(DT, coords = c('X', NULL), crs = utm),
               'coords must be length 2')
  expect_error(direction_to_centroid(DT, coords = c('X', 'ID'), crs = utm),
               'coords must be of class numeric')
  copy_DT <- copy(clean_DT)[, X := as.character(X)]
  expect_error(direction_to_centroid(copy_DT, coords = coords, crs = utm),
               'coords must be of class numeric')
  copy_DT <- copy(clean_DT)[, centroid_X := as.character(centroid_X)]
  expect_error(direction_to_centroid(copy_DT, coords = coords, crs = utm),
               'coords_centroid must be of class numeric')
})

test_that('direction_centroid column succesfully detected', {
  copy_DT <- copy(clean_DT)[, direction_centroid := 1]
  expect_message(
    direction_to_centroid(copy_DT, coords = coords, crs = utm),
    'direction_centroid column will be overwritten'
  )
})

test_that('no rows are added to the result DT', {
  copy_DT <- copy(clean_DT)

  expect_equal(nrow(copy_DT),
               nrow(direction_to_centroid(copy_DT, coords = coords, crs = utm)))
})

test_that('one column added to the result DT', {
  copy_DT <- copy(clean_DT)

  expect_equal(ncol(copy_DT) + 1,
               ncol(direction_to_centroid(copy_DT, coords = coords, crs = utm)))
})

test_that('column added to the result DT is a double with units rad', {
  expect_type(direction_to_centroid(DT, coords = coords, crs = utm)$direction_centroid,
              'double')
  expect_equal(
    units(
      direction_to_centroid(DT, coords = coords, crs = utm)$direction_centroid)$numerator,
    'rad')
})

test_that('returns a data.table', {
  expect_s3_class(direction_to_centroid(DT, coords = coords, crs = utm), 'data.table')
})

test_that('use_transform errors if crs not provided', {
  expect_error(
    direction_to_centroid(DT, coords = coords, crs = NA),
    'ensure crs is provided'
  )

  copyDT <- copy(DT)
  get_geometry(copyDT, coords = coords, crs = utm)
  st_crs(copyDT$geometry) <- NA
  get_geometry(copyDT, coords = paste0('centroid_', coords), crs = utm,
               geometry_colname = 'centroid')

  expect_error(
    direction_to_centroid(
      DT = copyDT
    ),
    'ensure crs is provided'
  )
})


# sfc interface
test_that('if coords null, geometry required', {
  expect_error(direction_to_centroid(DT, crs = utm),
               'get_geometry?')

  get_geometry(DT, coords = coords, crs = utm)
  expect_error(direction_to_centroid(DT, crs = utm),
               'centroid_group?')

  copy_DT <- copy(DT)
  copy_DT[, centroid := 42]
  expect_error(direction_to_centroid(copy_DT, crs = utm),
               'sfc_POINT')

})

test_that('message if overwrite', {
  copy_DT <- copy(clean_DT)[, direction_centroid := 1]
  get_geometry(copy_DT, coords = coords, crs = utm)
  centroid_group(copy_DT)
  expect_message(
    direction_to_centroid(copy_DT),
    'direction_centroid column will be overwritten'
  )
})

test_that('NAs in coordinates return NA', {
  copyDT <- copy(DT)
  copyDT[sample(.N, 100), X := NA]

  expect_equal(
    copyDT[is.na(X), .N],
    direction_to_centroid(copyDT, coords = coords,
                          crs = utm)[is.na(X)][is.na(direction_centroid), .N]
  )

  copyDT <- copy(DT)
  copyDT[sample(.N, 100), Y := NA]

  expect_equal(
    copyDT[is.na(Y), .N],
    direction_to_centroid(copyDT, coords = coords,
                          crs = utm)[is.na(Y)][is.na(direction_centroid), .N]
  )

  copyDT <- copy(DT)
  copyDT[sample(.N, 100), X := NA]
  copyDT[sample(.N, 100), centroid_X := NA]
  get_geometry(copyDT, coords, crs = utm)
  get_geometry(copyDT, c('centroid_X', 'centroid_Y'), crs = utm,
               geometry_colname = 'centroid')

  expect_equal(
    copyDT[is.na(Y), .N],
    direction_to_centroid(copyDT)[is.na(Y)][is.na(direction_centroid), .N]
  )

  copyDT <- copy(DT)
  copyDT[sample(.N, 100), Y := NA]
  copyDT[sample(.N, 100), centroid_Y := NA]
  get_geometry(copyDT, coords, crs = utm)
  get_geometry(copyDT, c('centroid_X', 'centroid_Y'), crs = utm,
               geometry_colname = 'centroid')

  expect_equal(
    copyDT[is.na(Y), .N],
    direction_to_centroid(copyDT)[is.na(Y)][is.na(direction_centroid), .N]
  )

})
