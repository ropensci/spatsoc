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


test_that('returns a data.table', {
  expect_s3_class(direction_step(DT, id = id, coords = coords,
                                 projection = projection),
                  'data.table')
})


test_that('splitBy returns expected', {
  DT[, split := sample(seq.int(5), .N, replace = TRUE)]
  expect_gte(
    direction_step(DT, id = id, coords = coords,
                   projection = projection,
                   splitBy = 'split')[is.na(direction), .N],
    direction_step(DT, id = id, coords = coords,
                   projection = projection)[is.na(direction), .N]

  )
})


DT_A <- data.table(
  x = c(-5, -5, 0, 14, 10, 0),
  y = c(5, 3, 1, 1, 11, 11),
  id =  'A'
)[, timegroup := seq.int(.N)]

# Related to: PR 92
test_that('longlat NA radian returned', {
  expect_equal(
    direction_step(DT_A, id = 'id', coords = c('x', 'y'),
                   projection = 4326)[]$direction |> class(),
    'units'
  )
  expect_gte(
    sum(is.na(direction_step(DT_A, id = 'id', coords = c('x', 'y'),
                   projection = 4326)[]$direction)),
    1
  )
})
