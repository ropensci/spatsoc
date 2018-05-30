# Test GroupPts
context('test GroupPts')
library(spatsoc)

DT <- fread('../testdata/buffalo.csv')
# DT <- fread('input/Buffalo.csv')

ls.params <- list(DT = DT,
                  coordFields = c('X', 'Y'),
                  idField = 'ID',
                  time = 'posix')


test_that("column names must be provided", {
  expect_error(GroupPts(DT, coordFields = NULL))
  expect_error(GroupPts(DT, idField = NULL))
  expect_error(GroupPts(DT, time = NULL))
  expect_error(GroupPts(DT, coordFields = NULL, time = NULL))
  expect_error(GroupPts(DT, idField = NULL, time = NULL))
  expect_error(GroupPts(DT, idField = NULL, coordFields = NULL))
  expect_error(GroupPts(DT, idField = NULL, coordFields = NULL, time = NULL))
})

test_that("column names must exist in DT", {
  expect_error(GroupPts(
    DT,
    idField = 'ID',
    time = 'potato',
    coordFields = c('X', 'Y')
  ))
  # expect_error(GroupPts(DT, idField = 'potato'))
  # expect_error(GroupPts(DT, time = 'potato'))
  # expect_error(GroupPts(DT, coordFields = NULL, time = NULL))
  # expect_error(GroupPts(DT, idField = NULL, time = NULL))
  # expect_error(GroupPts(DT, idField = NULL, coordFields = NULL))
  # expect_error(GroupPts(DT, idField = NULL, coordFields = NULL, time = NULL))
})
