# Test Lines
context('test Lines')
library(spatsoc)

DT <- fread('../testdata/buffalo.csv')

test_that('DT is required', {
  expect_error(GroupLines(DT = NULL, distance = 10, idField = 'ID'),
               'input DT required')
})
