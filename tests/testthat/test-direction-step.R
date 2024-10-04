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


test_that('column names must exist in DT', {
  expect_error(direction_step(DT, id = 'potato', coords = coords,
                              projection = projection),
               'not present')

  expect_error(direction_step(DT, id = id, coords = c('potato', 'potato'),
                              projection = projection),
               'not present')

  expect_error(direction_step(DT, id = id, coords = coords,
                              projection = projection, splitBy = 'potato'),
               'not present')
})

test_that('coords are correctly provided or error detected', {
  expect_error(direction_step(DT, id = id, coords = c('X', NULL),
                              projection = projection),
               'requires a vector')

  expect_error(direction_step(DT, id = id, coords = c('X', 'ID'),
                              projection = projection),
               'numeric')
})

test_that('dimensions returned expected', {

  expect_equal(
    ncol(clean_DT) + 1,
    ncol(direction_step(copy(clean_DT), id = id,
                        coords = coords, projection = projection))
  )

  expect_equal(
    nrow(clean_DT),
    nrow(direction_step(copy(clean_DT), id = id,
                        coords = coords, projection = projection))
  )

  expect_true('direction' %in% colnames(
    direction_step(
      copy(clean_DT),
      id = id,
      coords = coords,
      projection = projection
    )
  ))

})


test_that('direction column succesfully detected', {
  copyDT <- copy(clean_DT)[, direction := 1]
  expect_message(
    direction_step(
      copyDT,
      id = id,
      coords = coords,
      projection = projection
    ),
    'direction column will be overwritten'
  )
})
