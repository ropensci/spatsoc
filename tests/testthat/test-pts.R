# Test GroupPts
context('test GroupPts')
library(spatsoc)

DT <- fread('../testdata/buffalo.csv')
# DT <- fread('input/Buffalo.csv')

ls.params <- list(DT = DT,
                  coordFields = c('X', 'Y'),
                  idField = 'ID',
                  time = 'posix')


test_that('ID and distance column names must be provided', {
  expect_error(GroupPts(DT, distance = 10, idField = NULL),
               'ID field required')
  expect_error(GroupPts(DT, distance = NULL, idField = 'ID'),
               'distance threshold required')
})

test_that('column names must exist in DT', {
  # where ID field doesn't exist in DT
  expect_error(GroupPts(DT, distance = 10, idField = 'potato',
                        coordFields = c('X', 'Y')),
               'some fields', fixed = FALSE)

  # where coordFields don't exist
  expect_error(GroupPts(DT, distance = 10, idField = 'ID',
                        coordFields = c('potatoX', 'potatoY')),
               'some fields', fixed = FALSE)

  # where time field doesn't exist
  expect_error(GroupPts(DT, distance = 10, idField = 'ID',
                        coordFields = c('X', 'Y'),
                        groupFields = 'potato'),
               'some fields', fixed = FALSE)

  # expect_error(GroupPts(DT, distance = 10, idField = NULL, coordFields = NULL,
  #                       time = NULL), 'some fields', fixed = FALSE)
  # expect_silent(GroupPts(
  #   DT,
  #   idField = 'ID',
  #   time = 'potato',
  #   coordFields = c('X', 'Y')
  # ))
  # expect_error(GroupPts(DT, idField = 'potato'))
  # expect_error(GroupPts(DT, time = 'potato'))
  # expect_error(GroupPts(DT, coordFields = NULL, time = NULL))
  # expect_error(GroupPts(DT, idField = NULL, time = NULL))
  # expect_error(GroupPts(DT, idField = NULL, coordFields = NULL))
  # expect_error(GroupPts(DT, idField = NULL, coordFields = NULL, time = NULL))
})
