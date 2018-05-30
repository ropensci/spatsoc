# Test GroupPts
context('test GroupPts')
library(spatsoc)

DT <- fread('../testdata/buffalo.csv')

ls.params <- list(DT = DT,
                  coordFields = c('X', 'Y'),
                  idField = 'ID',
                  timeGroup = 'posix')


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

  # where group fields doesn't exist
  expect_error(GroupPts(DT, distance = 10, idField = 'ID',
                        coordFields = c('X', 'Y'),
                        groupFields = 'potato'),
               'some fields', fixed = FALSE)

  # where timeGroup field doesn't exist
  expect_error(GroupPts(DT, distance = 10, idField = 'ID',
                        coordFields = c('X', 'Y'),
                        timeGroup = 'potato'),
               'some fields', fixed = FALSE)

})


test_that('threshold correctly provided or error detected', {
  expect_silent(GroupPts(DT, distance = 10, idField = 'ID',
                         coordFields = c('X', 'Y')))

  expect_error(GroupPts(DT, distance = -10, idField = 'ID'),
               'distance must be greater than 0')

  expect_error(GroupPts(DT, distance = 0, idField = 'ID'),
               'distance must be greater than 0')


})


test_that('coordFields are correctly provided or error detected', {
  expect_error(GroupPts(DT, distance = 10, idField = 'ID',
                         coordFields = c('X', NULL)),
               'coordFields requires a vector')

  expect_error(GroupPts(DT, distance = 10, idField = 'ID',
                        coordFields = c('X', 'ID')),
               'coordFields must be numeric')
})


test_that('group column succesfully detected', {
  copyDT <- copy(DT)[, group := 1]
  expect_warning(GroupPts(copyDT,distance = 10, idField = 'ID',
                          coordFields = c('X', 'Y')),
                 '`group` column will be overwritten')
})


test_that('two column DT returned if timeGroup, group fields not provided', {
  expect_equal(ncol(GroupPts(DT, distance = 10, idField = 'ID',
                          coordFields = c('X', 'Y'))),
               2)
})


test_that('withinGroup is not returned to the user', {
  copyDT <- copy(DT)
  expect_false('withinGroup' %in% colnames(
    GroupPts(copyDT, distance = 10, idField = 'ID',
             coordFields = c('X', 'Y'), timeGroup = 'posix')))
})

test_that('no rows are added to the result DT', {
  copyDT <- copy(DT)
  expect_equal(nrow(copyDT),
               nrow(GroupPts(copyDT, distance = 10, idField = 'ID',
                              coordFields = c('X', 'Y'), timeGroup = 'posix')))
})

test_that('only one column added to the result DT', {
  copyDT <- copy(DT)
  expect_equal(ncol(copyDT) + 1,
               ncol(GroupPts(copyDT, distance = 10, idField = 'ID',
                             coordFields = c('X', 'Y'), timeGroup = 'posix')))
})
