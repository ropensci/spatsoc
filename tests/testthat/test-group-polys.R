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
# checks for - or spaces in idField

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
