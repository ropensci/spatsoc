# Test GroupPolys
context('test GroupPolys')
library(spatsoc)

DT <- fread('../testdata/buffalo.csv')
utm <- '+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

test_that('DT or spPts are required but not both', {
  expect_error(GroupPolys(DT = NULL, spPolys = NULL, area = FALSE),
               'must provide either DT or spPolys')

  expect_error(GroupPolys(DT = DT, spPolys = 10, area = FALSE),
               'cannot provide both DT and spPolys')
})

test_that('area provided and logical, or error', {
 expect_error(GroupPolys(
   DT = DT,
   projection = utm,
   hrType = 'mcp',
   area = NULL,
   coordFields = c('X', 'Y'),
   idField = 'ID'
 ), 'area must be provided', fixed = TRUE)

  expect_error(GroupPolys(
    DT = DT,
    projection = utm,
    hrType = 'mcp',
    area = 'potato',
    coordFields = c('X', 'Y'),
    idField = 'ID'
  ), 'area must be provided', fixed = TRUE)

  expect_error(GroupPolys(
    DT = DT,
    projection = utm,
    hrType = 'mcp',
    area = 10,
    coordFields = c('X', 'Y'),
    idField = 'ID'
  ), 'area must be provided', fixed = TRUE)
})


# if DT spPolys both, neither provided
test_that('DT or spPolys, but not both', {
  expect_error(
    GroupPolys(
      DT = NULL,
      area = FALSE,
      spPolys = NULL
    ),
    'must provide either DT or spPolys')

  expect_error(
    GroupPolys(
      DT = DT,
      area = FALSE,
      spPolys = 10
    ),
    'cannot provide both DT and spPolys')
})

test_that('column names must exist in DT', {
  expect_error(
    GroupPolys(
      DT = DT,
      projection = utm,
      hrType = 'mcp',
      area = FALSE,
      coordFields = c('X', 'Y'),
      idField = 'potato'
    ),
    'not present in input DT',
    fixed = FALSE
  )

  expect_error(
    GroupPolys(
      DT = DT,
      projection = utm,
      hrType = 'mcp',
      area = FALSE,
      coordFields = c('potatoX', 'potatoY'),
      idField = 'ID'
    ),
    'not present in input DT',
    fixed = FALSE
  )
})


# if byfields, not also spPolys
# if area, returns different length
# if not area, returns same length, appended group column

test_that('ID field is alphanumeric and does not have spaces', {
  copyDT <- copy(DT)[, ID := gsub('e', ' ', ID)]
  expect_error(
    GroupPolys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = TRUE,
      coordFields = c('X', 'Y'),
      idField = 'ID'
    ),
    'please ensure IDs are alphanumeric and do not contain spaces'
  )
})

# GroupPolys(
#   DT = DT,
#   projection = utm,
#   hrType = 'mcp',
#   hrParams = NULL,
#   area = FALSE,
#   coordFields = c('X', 'Y'),
#   idField = 'ID',
#   byFields = NULL,
#   spPolys = NULL
# )
