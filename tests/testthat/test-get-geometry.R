# Test get_geometry
context('test get_geometry')

library(sf)

DT <- fread('../testdata/DT.csv')

coords <- c('X', 'Y')
crs <- 32736

test_that('DT is required', {
  expect_error(get_geometry(
    DT = NULL
  ),
  'DT must be provided')
})

test_that('coords provided correctly, else error', {
  expect_error(
    get_geometry(
      DT,
      coords = 'X'
    ),
    'coords must be length 2'
  )

  expect_error(
    get_geometry(
      DT,
      coords = c('potatoX', 'Y')
    ),
    'not present in input'
  )

  expect_error(
    get_geometry(
      DT,
      coords = c('X', 'potatoY')
    ),
    'not present in input'
  )

  expect_error(
    get_geometry(
      DT,
      coords = c('ID', 'ID')
    ),
    'coords must be of class numeric'
  )

})

test_that('crs provided correctly, else error', {
  expect_error(
    get_geometry(
      DT,
      coords = coords,
      crs = NULL
    ),
    'crs must be provided'
  )
})

test_that('group column succesfully detected', {
  copyDT <- copy(DT)[, geometry := 1]
  expect_message(
    get_geometry(
      copyDT,
      coords = coords,
      crs = crs
    ),
    'geometry column will be overwritten'
  )
})

test_that('geometry column returned is sfc, as expected', {
  copyDT <- copy(DT)
  get_geometry(copyDT, coords = coords, crs = crs)

  expect_s3_class(copyDT$geometry, 'sfc_POINT')
  expect_s3_class(copyDT$geometry, 'sfc')

  copyDT <- copy(DT)
  get_geometry(copyDT, coords = coords, crs = crs)

  expect_equal(st_coordinates(copyDT$geometry),
               copyDT[, as.matrix(.SD), .SDcols = coords])

  copyDT <- copy(DT)
  get_geometry(copyDT, coords = coords, crs = crs)

  expect_equal(
    mean(st_coordinates(copyDT$geometry) -
      copyDT[, as.matrix(.SD), .SDcols = coords]),
    0
  )

  copyDT <- copy(DT)[sample(seq.int(.N), 10), c(coords) := NA]
  get_geometry(copyDT, coords = coords, crs = crs)
  expect_equal(copyDT[is.na(X), .N], copyDT[st_is_empty(geometry), .N])
})
