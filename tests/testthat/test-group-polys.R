# Test GroupPolys
context('test GroupPolys')
library(spatsoc)

DT <- fread('../testdata/buffalo.csv')
utm <- '+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

test_that('DT or spPts are required but not both', {
  expect_error(GroupPolys(DT = NULL, spPolys = NULL, area = FALSE),
               'input DT or spPts required')

  expect_error(GroupPolys(DT = DT, spPolys = 10, area = FALSE),
               'cannot provide both DT and spPts')
})

test_that('area provided and logical, or error', {
 expect_error(GroupPolys(
   DT = DT,
   projection = utm,
   hrType = 'mcp',
   area = NULL,
   coordFields = c('X', 'Y'),
   idField = 'ID',
 ), 'must provide TRUE or FALSE for area parameter')

  expect_error(GroupPolys(
    DT = DT,
    projection = utm,
    hrType = 'mcp',
    area = 'potato',
    coordFields = c('X', 'Y'),
    idField = 'ID',
  ), 'must provide TRUE or FALSE for area parameter')

  expect_error(GroupPolys(
    DT = DT,
    projection = utm,
    hrType = 'mcp',
    area = 10,
    coordFields = c('X', 'Y'),
    idField = 'ID',
  ), 'must provide TRUE or FALSE for area parameter')
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
