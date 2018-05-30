# Test GroupPts
context('test GroupPts')
library(spatsoc)

DT <- fread('../testdata/buffalo.csv')
# DT <- fread('input/Buffalo.csv')

ls.params <- list(DT = DT,
                  coordFields = c('X', 'Y'),
                  idField = 'ID',
                  time = 'posix')


test_that('column names must be provided', {
  expect_error(GroupPts(DT, distance = 10),
               'ID field required', fixed = FALSE)

  expect_error(GroupPts(DT, distance = 10),
               'ID field required', fixed = FALSE)
  expect_error(GroupPts(DT, distance = 10, coordFields = NULL, time = NULL),
               'some fields', fixed = FALSE)
  expect_error(GroupPts(DT, distance = 10, idField = NULL, time = NULL),
               'some fields', fixed = FALSE)
  expect_error(GroupPts(DT, distance = 10, idField = NULL, coordFields = NULL),
               'some fields', fixed = FALSE)
  expect_error(GroupPts(DT, distance = 10, idField = NULL, coordFields = NULL,
                        time = NULL), 'some fields', fixed = FALSE)
})

# test_that('column names must exist in DT', {
#   expect_silent(GroupPts(
#     DT,
#     idField = 'ID',
#     time = 'potato',
#     coordFields = c('X', 'Y')
#   ))
#   # expect_error(GroupPts(DT, idField = 'potato'))
#   # expect_error(GroupPts(DT, time = 'potato'))
#   # expect_error(GroupPts(DT, coordFields = NULL, time = NULL))
#   # expect_error(GroupPts(DT, idField = NULL, time = NULL))
#   # expect_error(GroupPts(DT, idField = NULL, coordFields = NULL))
#   # expect_error(GroupPts(DT, idField = NULL, coordFields = NULL, time = NULL))
# })
