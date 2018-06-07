# Test Build HRs
context('test BuildHRs')
library(spatsoc)

DT <- fread('../testdata/buffalo.csv')
utm <- '+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'


test_that('DT is required', {
  expect_error(BuildHRs(DT = NULL, spPts = NULL),
               'input DT or spPts required')
})



test_that('coordFields, idField, projection must be provided and proper format', {
  expect_error(BuildHRs(DT = DT,
                        projection = utm,
                        hrType = 'mcp',
                        coordFields = c('X', 'Y'),
                        idField = NULL),
               'idField must be provided')

  expect_error(BuildHRs(DT = DT,
                        projection = NULL,
                        hrType = 'mcp',
                        coordFields = c('X', 'Y'),
                        idField = 'ID'),
               'projection must be provided')

  expect_error(BuildHRs(DT = DT,
                        projection = utm,
                        hrType = 'mcp',
                        coordFields = NULL,
                        idField = 'ID'),
               'coordFields must be provided')

  expect_error(BuildHRs(DT = DT,
                        projection = utm,
                        hrType = 'mcp',
                        coordFields = 'X',
                        idField = 'ID'),
               'coordFields requires a vector of', fixed = FALSE)

  copyDT <- copy(DT)
  copyDT[, X := as.character(X)]
  expect_error(BuildHRs(DT = copyDT,
                        projection = utm,
                        hrType = 'mcp',
                        coordFields = c('X', 'Y'),
                        idField = 'ID'),
               'coordFields must be numeric')

  expect_error(BuildHRs(DT = DT,
                        projection = utm,
                        hrType = NULL,
                        coordFields = c('X', 'Y'),
                        idField = 'ID'),
               'hrType must be provided')

})


test_that('column names must exist in DT', {
  expect_error(BuildHRs(DT = DT,
                        projection = utm,
                        hrType = 'mcp',
                        coordFields = c('X', 'Y'),
                        idField = 'potato'),
               'not present in input DT', fixed = FALSE)

  expect_error(BuildHRs(DT = DT,
                        projection = utm,
                        hrType = 'mcp',
                        coordFields = c('potatoX', 'potatoY'),
                        idField = 'ID'),
               'not present in input DT', fixed = FALSE)
})

test_that('hrParams returns error if params do not match function params', {
  expect_error(
    BuildHRs(
      DT = DT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95, potato = TRUE),
      coordFields = c('X', 'Y'),
      idField = 'ID'
    ),
    'hrParams provided do not match function parameters', fixed = FALSE)

  expect_error(
    BuildHRs(
      DT = DT,
      projection = utm,
      hrType = 'kernel',
      hrParams = list(grid = 60, potato = TRUE),
      coordFields = c('X', 'Y'),
      idField = 'ID'
    ),
    'hrParams provided do not match function parameters', fixed = FALSE)
})

test_that('if hrParams NULL, warngs', {
  expect_warning(
    BuildHRs(
      DT = DT,
      projection = utm,
      hrType = 'mcp',
      hrParams = NULL,
      coordFields = c('X', 'Y'),
      idField = 'ID'
    ),
    'hrParams is not provided, using defaults')

})

