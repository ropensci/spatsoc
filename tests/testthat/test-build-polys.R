# Test build_polys
context('test build_polys')
library(spatsoc)

DT <- fread('../testdata/DT.csv')

utm <- '+init=epsg:32736'


test_that('DT or spPts are required but not both', {
  expect_error(build_polys(DT = NULL, spPts = NULL),
               'input DT or spPts required')

  expect_error(build_polys(DT = DT, spPts = 10),
               'cannot provide both DT and spPts')
})



test_that('coords, id, projection must be provided and (splitBy) proper format',
          {
            expect_error(
              build_polys(
                DT = DT,
                projection = utm,
                hrType = 'mcp',
                coords = c('X', 'Y'),
                id = NULL
              ),
              'id must be provided'
            )

            expect_error(
              build_polys(
                DT = DT,
                projection = NULL,
                hrType = 'mcp',
                coords = c('X', 'Y'),
                id = 'ID'
              ),
              'projection must be provided'
            )

            expect_error(
              build_polys(
                DT = DT,
                projection = utm,
                hrType = 'mcp',
                coords = NULL,
                id = 'ID'
              ),
              'coords must be provided'
            )

            expect_error(
              build_polys(
                DT = DT,
                projection = utm,
                hrType = 'mcp',
                coords = 'X',
                id = 'ID'
              ),
              'coords requires a vector of',
              fixed = FALSE
            )

            copyDT <- copy(DT)
            copyDT[, X := as.character(X)]
            expect_error(
              build_polys(
                DT = copyDT,
                projection = utm,
                hrType = 'mcp',
                coords = c('X', 'Y'),
                id = 'ID'
              ),
              'coords must be numeric'
            )

            expect_error(
              build_polys(
                DT = DT,
                projection = utm,
                hrType = NULL,
                coords = c('X', 'Y'),
                id = 'ID'
              ),
              'hrType must be provided'
            )

            copyDT <- copy(DT)
            copyDT[, s := factor(1)]
            expect_error(
              build_polys(
                DT = copyDT,
                projection = utm,
                hrType = NULL,
                coords = c('X', 'Y'),
                id = 'ID',
                splitBy = 's'
              ),
              'and splitBy when provided', fixed = FALSE
            )
          })


test_that('column names must exist in DT', {
  expect_error(
    build_polys(
      DT = DT,
      projection = utm,
      hrType = 'mcp',
      coords = c('X', 'Y'),
      id = 'potato'
    ),
    'not present in input DT',
    fixed = FALSE
  )

  expect_error(
    build_polys(
      DT = DT,
      projection = utm,
      hrType = 'mcp',
      coords = c('potatoX', 'potatoY'),
      id = 'ID'
    ),
    'not present in input DT',
    fixed = FALSE
  )

  expect_error(
    build_polys(
      DT = DT,
      projection = utm,
      hrType = 'mcp',
      coords = c('X', 'Y'),
      id = 'ID',
      splitBy = 'potato'
    ),
    'not present in input DT',
    fixed = FALSE
  )
})

test_that('hrParams returns error if params do not match function params', {
  expect_error(
    build_polys(
      DT = DT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95, potato = TRUE),
      coords = c('X', 'Y'),
      id = 'ID'
    ),
    'hrParams provided do not match function parameters',
    fixed = FALSE
  )

  expect_error(
    build_polys(
      DT = DT,
      projection = utm,
      hrType = 'kernel',
      hrParams = list(grid = 60, potato = TRUE),
      coords = c('X', 'Y'),
      id = 'ID'
    ),
    'hrParams provided do not match function parameters',
    fixed = FALSE
  )

  s4promise <- evaluate_promise(build_polys(
    DT = DT,
    projection = utm,
    hrType = 'mcp',
    hrParams = list(percent = 95),
    coords = c('X', 'Y'),
    id = 'ID'
  ))

  expect_s4_class(
    s4promise$result,
    'SpatialPolygonsDataFrame'
  )
})

test_that('if hrParams NULL, warns', {
  paramspromise <- evaluate_promise(
    build_polys(
      DT = DT,
      projection = utm,
      hrType = 'mcp',
      hrParams = NULL,
      coords = c('X', 'Y'),
      id = 'ID'
    )
  )

  expect_match(
    paramspromise$messages,
    'hrParams is not provided, using defaults'
  )

})


test_that('build_polys returns SpatialPolygons', {
  polpromise <- evaluate_promise(
    build_polys(
      DT = DT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      coords = c('X', 'Y'),
      id = 'ID'
    )
  )

  expect_s4_class(
    polpromise$result,
    'SpatialPolygonsDataFrame'
  )

  polpromise2 <- evaluate_promise(
    build_polys(
      DT = DT,
      projection = utm,
      hrType = 'kernel',
      hrParams = list(grid = 60),
      coords = c('X', 'Y'),
      id = 'ID'
    )
  )

  expect_s4_class(
    polpromise2$result,
    'SpatialPolygonsDataFrame'
  )
})


test_that('hrParams can have both vertices and kernel args', {
  bothparamspromise <- evaluate_promise(
    build_polys(
      DT = DT,
      projection = utm,
      hrType = 'kernel',
      hrParams = list(percent = 95, grid = 60),
      coords = c('X', 'Y'),
      id = 'ID'
    )
  )
  expect_s4_class(
    bothparamspromise$result,
    'SpatialPolygonsDataFrame'
  )
})
