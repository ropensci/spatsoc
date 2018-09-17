# Test GBI
context('test get_gbi')
library(spatsoc)

DT <- fread('../testdata/DT.csv')

test_that('DT is required', {
  expect_error(get_gbi(
    DT = NULL,
    threshold = 10,
    id = 'ID'
  ),
  'input DT required')
})
