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
  'input DT required')
})

test_that('coords provided correctly, else error', {
  expect_error(
    get_geometry(
      DT,
      coords = 'X'
    ),
    'coords requires a vector'
  )

  expect_error(
    get_geometry(
      DT,
      coords = c('potatoX', 'Y')
    ),
    'not present in input DT'
  )

  expect_error(
    get_geometry(
      DT,
      coords = c('X', 'potatoY')
    ),
    'not present in input DT'
  )

  expect_error(
    get_geometry(
      DT,
      coords = c('ID', 'ID')
    ),
    'coords must be numeric'
  )

})

test_that('crs provided correctly, else error', {
  expect_error(
    get_geometry(
      DT,
      coords = coords,
      crs = NULL
    ),
    'input crs required'
  )
})

