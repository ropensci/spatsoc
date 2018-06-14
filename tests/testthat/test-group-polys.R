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

test_that('projection provided or error', {
  expect_error(
    GroupPolys(
      DT = DT,
      projection = NULL,
      hrType = 'mcp',
      area = FALSE,
      coordFields = c('X', 'Y'),
      idField = 'ID'
    ),
    'projection must be provided')
})

test_that('mising hrParams warns default used', {
  copyDT <- copy(DT)
  expect_warning(
    GroupPolys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      area = FALSE,
      coordFields = c('X', 'Y'),
      idField = 'ID'
    ),
    'hrParams is not provided, using defaults')
})

test_that('missing hrType fails', {
  expect_error(
    GroupPolys(
      DT = DT,
      projection = utm,
      hrType = NULL,
      area = FALSE,
      coordFields = c('X', 'Y'),
      idField = 'ID'
    ),
    'hrType must be provided')
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



test_that('byFields and spPolys are not both provided', {
  expect_error(
    GroupPolys(
      byFields = 'yr',
      spPolys = 10,
      area = TRUE
    ),
    'cannot provide spPolys if providing byFields')
})


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

test_that('column and row lengths returned make sense', {
  expect_lte(nrow(
    GroupPolys(
      DT = DT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = TRUE,
      coordFields = c('X', 'Y'),
      idField = 'ID'
    )
  ),
  nrow(expand.grid(DT[, unique(ID)], DT[, unique(ID)])))

  copyDT <- copy(DT)[, yr := year(datetime)]
  expect_equal(nrow(
    GroupPolys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = FALSE,
      coordFields = c('X', 'Y'),
      idField = 'ID',
      byFields = 'yr'
    )
  ),
  nrow(copyDT))

  copyDT <- copy(DT)
  copyDT[, family := sample(c(1, 2, 3, 4), .N, replace = TRUE)]
  expect_equal(ncol(copyDT) + 1,
               ncol(
                 GroupPolys(
                   DT = copyDT,
                   projection = utm,
                   hrType = 'mcp',
                   hrParams = list(percent = 95),
                   area = FALSE,
                   coordFields = c('X', 'Y'),
                   idField = 'ID',
                   byFields = 'family'
                 )
               ))
})


test_that('withinGroup is not returned to the user', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, timeField = 'datetime', threshold = '14 days')
  copyDT[, N := .N, by = .(ID, block)]
  expect_false('withinGroup' %in% colnames(
    GroupPolys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = FALSE,
      coordFields = c('X', 'Y'),
      idField = 'ID'
    )))
})


test_that('group column succesfully detected', {
  copyDT <- copy(DT)
  copyDT[, group := 1][, mnth := month(datetime)]

  expect_warning(
    GroupPolys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = FALSE,
      coordFields = c('X', 'Y'),
      idField = 'ID',
      byFields = 'mnth'
    ),
    'group column will be overwritten'
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
