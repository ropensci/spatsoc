# Test GBI
context('test get_gbi')
library(spatsoc)

DT <- fread('../testdata/DT.csv')

test_that('DT is required', {
  expect_error(get_gbi(
    DT = NULL,
    group = 'group',
    id = 'ID',
    type = 'point'
  ),
  'input DT required')
})


test_that('ID and group column names provided', {
  expect_error(get_gbi(
    DT = DT,
    group = NULL,
    id = 'ID',
    type = 'point'
  ),
  'group field required')

  expect_error(get_gbi(
    DT = DT,
    group = 'group',
    id = NULL,
    type = 'point'
  ),
  'ID field required')

})


test_that('type provided, and correctly', {
  expect_error(get_gbi(
    DT = DT,
    group = 'group',
    id = 'ID',
    type = NULL
  ),
  'type required')

  expect_error(get_gbi(
    DT = DT,
    group = 'group',
    id = 'ID',
    type = 'potato'
  ),
  'type must be one of', fixed = FALSE)

})
