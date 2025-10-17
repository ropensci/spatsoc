# Test get_gbi
context('test get_gbi')

library(spatsoc)

DT <- fread('../testdata/DT.csv')

# Cast the character column to POSIXct
DT[, datetime := as.POSIXct(datetime)]
DT[, yr := year(datetime)]

utm <- 'EPSG:32736'

group_times(DT, 'datetime', '5 minutes')
group_pts(DT, 50, timegroup = 'timegroup',
          id = 'ID', coords = c('X', 'Y'), splitBy = 'yr')

test_that('DT is required', {
  expect_error(get_gbi(
    DT = NULL,
    group = 'group',
    id = 'ID'
  ),
  'DT must be provided')
})


test_that('ID and group column names provided', {
  expect_error(get_gbi(
    DT = DT,
    group = NULL,
    id = 'ID'
  ),
  'group must be')

  expect_error(get_gbi(
    DT = DT,
    group = 'group',
    id = NULL
  ),
  'id must be')

})


test_that('columns in DT', {
  expect_error(get_gbi(
    DT = DT,
    group = 'potato',
    id = 'ID'
  ),
  'potato field provided')

  expect_error(get_gbi(
    DT = DT,
    group = 'group',
    id = 'potato'
  ),
  'potato field provided')

})



test_that('matrix returned and type integer', {
  expect_type(
    get_gbi(
      DT = DT,
      group = 'group',
      id = 'ID'
    ),
    'integer'
  )

  expect_is(
    get_gbi(
      DT = DT,
      group = 'group',
      id = 'ID'
    ),
    'matrix'
  )

})


test_that('gbi length returned makes sense', {
  expect_equal(sum(get_gbi(
    DT = DT,
    group = 'group',
    id = 'ID'
  )),
  unique(DT[, .(ID, group)])[, .N])
})

test_that('NAs detected', {
  copyDT <- copy(DT)[1, group := NA]

  expect_warning(get_gbi(
    DT = copyDT,
    group = 'group',
    id = 'ID'
  ),
  'DT contains NA', fixed = FALSE)
})
