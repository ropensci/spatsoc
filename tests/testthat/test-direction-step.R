# Test direction_step
context('test direction_step')

library(spatsoc)

DT <- fread('../testdata/DT.csv')

DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
setorder(DT, datetime)

id <- 'ID'
coords <- c('X', 'Y')
projection <- 32736

clean_DT <- copy(DT)

test_that('DT is required', {
  expect_error(direction_step(DT = NULL), 'input DT required')
})

test_that('args required else error', {
  expect_error(direction_step(DT,  id = NULL), 'id column')

  expect_error(direction_step(DT, id = id, coords = NULL), 'coords requir')

  expect_error(direction_step(DT,  id = id, coords = coords, projection = NULL),
               'projection required')

  expect_error(direction_step(DT, id = id, coords = 'X'),
               'coords requires a vector')
})

