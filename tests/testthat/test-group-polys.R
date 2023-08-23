# Test group_polys
context('test group_polys')
library(spatsoc)

DT <- fread('../testdata/DT.csv')

DT[, datetime := as.POSIXct(datetime)]
DT[, family := sample(c(1, 2, 3, 4), .N, replace = TRUE)]
DT[, jul := data.table::yday(datetime)]

utm <- 32736

test_that('DT or spPts are required but not both', {
  expect_error(group_polys(
    DT = NULL,
    sfPolys = NULL,
    area = FALSE
  ),
  'must provide either DT or sfPolys')

  expect_error(group_polys(
    DT = DT,
    sfPolys = 10,
    area = FALSE
  ),
  'cannot provide both DT and sfPolys')
})

test_that('area provided and logical, or error', {
  expect_error(
    group_polys(
      DT = DT,
      projection = utm,
      hrType = 'mcp',
      area = NULL,
      coords = c('X', 'Y'),
      id = 'ID'
    ),
    'area must be provided',
    fixed = TRUE
  )

  expect_error(
    group_polys(
      DT = DT,
      projection = utm,
      hrType = 'mcp',
      area = 'potato',
      coords = c('X', 'Y'),
      id = 'ID'
    ),
    'area must be provided',
    fixed = TRUE
  )

  expect_error(
    group_polys(
      DT = DT,
      projection = utm,
      hrType = 'mcp',
      area = 10,
      coords = c('X', 'Y'),
      id = 'ID'
    ),
    'area must be provided',
    fixed = TRUE
  )
})

test_that('id provided', {
  expect_error(
    group_polys(
      DT = DT,
      projection = utm,
      hrType = 'mcp',
      area = TRUE,
      coords = c('X', 'Y'),
      id = NULL
    ),
    'id must be provided',
    fixed = TRUE
  )
})

test_that('projection provided or error', {
  expect_error(
    group_polys(
      DT = DT,
      projection = NULL,
      hrType = 'mcp',
      area = FALSE,
      coords = c('X', 'Y'),
      id = 'ID'
    ),
    'projection must be provided'
  )
})

test_that('mising hrParams warns default used', {
  copyDT <- copy(DT)
  missingpromise <- evaluate_promise(
    group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      area = FALSE,
      coords = c('X', 'Y'),
      id = 'ID'
    )
  )
  expect_match(
    missingpromise$messages,
    'hrParams is not provided, using defaults'
  )
})

test_that('missing hrType fails', {
  expect_error(
    group_polys(
      DT = DT,
      projection = utm,
      hrType = NULL,
      area = FALSE,
      coords = c('X', 'Y'),
      id = 'ID'
    ),
    'hrType must be provided'
  )
})

test_that('column names must exist in DT', {
  expect_error(
    group_polys(
      DT = DT,
      projection = utm,
      hrType = 'mcp',
      area = FALSE,
      coords = c('X', 'Y'),
      id = 'potato'
    ),
    'not present in input DT',
    fixed = FALSE
  )

  expect_error(
    group_polys(
      DT = DT,
      projection = utm,
      hrType = 'mcp',
      area = FALSE,
      coords = c('potatoX', 'potatoY'),
      id = 'ID'
    ),
    'not present in input DT',
    fixed = FALSE
  )

  expect_error(
    group_polys(
      DT = DT,
      projection = utm,
      hrType = 'mcp',
      area = FALSE,
      coords = c('X', 'Y'),
      id = 'ID',
      splitBy = 'potato'
    ),
    'not present in input DT',
    fixed = FALSE
  )
})

test_that('splitBy and sfPolys are not both provided', {
  expect_error(
    group_polys(
      splitBy = 'yr',
      sfPolys = 10,
      area = TRUE
    ),
    'cannot provide sfPolys if providing splitBy'
  )
})


test_that('ID field does not have spaces', {
  copyDT <- copy(DT)[ID == unique(ID)[1], ID := paste(ID, ID)]
  idpromise <- evaluate_promise(
    expect_error(group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = TRUE,
      coords = c('X', 'Y'),
      id = 'ID'
    ))
  )
  expect_match(
    as.character(idpromise$result),
    'please ensure IDs do not contain spaces'
  )

  copyDT[, population := 1]
  expect_error(
    group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = TRUE,
      coords = c('X', 'Y'),
      id = 'ID',
      splitBy = 'population'
    ),
    'please ensure IDs do not contain spaces'
  )
})

test_that('column and row lengths returned make sense', {
  copyDT <- copy(DT)
  group_polys_mcp <- group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = TRUE,
      coords = c('X', 'Y'),
      id = 'ID'
  )
  expect_lte(nrow(group_polys_mcp),
             nrow(expand.grid(DT[, unique(ID)], DT[, unique(ID)])))

  copyDT <- copy(DT)[, yr := year(datetime)]

  group_polys_mcp_split <- group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = FALSE,
      coords = c('X', 'Y'),
      id = 'ID',
      splitBy = 'yr'
    )

  expect_equal(nrow(group_polys_mcp_split), nrow(copyDT))

  group_sf_polys <- group_polys(
      id = 'ID',
      area = FALSE,
      sfPolys = build_polys(
        DT = DT,
        projection = utm,
        hrType = 'mcp',
        hrParams = list(percent = 95),
        coords = c('X', 'Y'),
        id = 'ID'
    )
  )
  group_sf_polys_area <- group_polys(
    id = 'ID',
    area = TRUE,
    sfPolys = build_polys(
      DT = DT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      coords = c('X', 'Y'),
      id = 'ID'
    )
  )
  expect_equal(nrow(group_sf_polys), length(unique(DT$ID)))
  expect_equal(nrow(group_sf_polys_area), length(unique(DT$ID)) ^ 2)
})


test_that('withinGroup is not returned to the user', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  # to avoid block length warning
  suppressWarnings(
    group_times(copyDT, datetime = 'datetime', threshold = '14 days')
  )
  copyDT[, N := .N, by = .(ID, block)]
  within_group <- group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = FALSE,
      coords = c('X', 'Y'),
      id = 'ID'
  )
  expect_false('withinGroup' %in% colnames(within_group))
})


test_that('group column succesfully detected', {
  copyDT <- copy(DT)[, group := 1][, mnth := month(datetime)]
  copyDT <- copyDT[, nBy := .N, by = .(mnth, ID)][nBy > 30]
  groupdelpromise <- evaluate_promise(
    group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = FALSE,
      coords = c('X', 'Y'),
      id = 'ID',
      splitBy = 'mnth'
    )
  )
  expect_match(groupdelpromise$messages, 'group column will be overwritten')

  copyDT <- copy(DT)[, group := 1][, mnth := month(datetime)]
  copyDT <- copyDT[, nBy := .N, by = .(mnth, ID)][nBy > 30]
  overwritepromise <- evaluate_promise(
    group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = FALSE,
      coords = c('X', 'Y'),
      id = 'ID'
    )
  )
  expect_match(
    overwritepromise$messages,
    'group column will be overwritten'
  )
})


test_that('area provided with splitBy does not return errors', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  copyDT[, yr := year(datetime)]
  within_split <- group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = TRUE,
      coords = c('X', 'Y'),
      id = 'ID',
      splitBy = 'yr'
  )
  expect_false('withinGroup' %in% colnames(within_split))

  area <- group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = TRUE,
      coords = c('X', 'Y'),
      id = 'ID',
      splitBy = 'yr'
  )
  expect_true('area' %in% colnames(area))

  prop <- group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = TRUE,
      coords = c('X', 'Y'),
      id = 'ID',
      splitBy = 'yr'
  )
  expect_true('proportion' %in% colnames(prop))
})


test_that('less than 5 locs returns NAs and warning', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  copyDT[, yr := year(datetime)]
  copyDT[1, yr := 2020]
  expect_warning(
    group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = TRUE,
      coords = c('X', 'Y'),
      id = 'ID',
      splitBy = 'yr'
    ),
    'build_polys failed for some rows', fixed = FALSE
  )

  expect_warning(
    group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = FALSE,
      coords = c('X', 'Y'),
      id = 'ID',
      splitBy = 'yr'
    ),
    'build_polys failed for some rows', fixed = FALSE
  )

  expect_true(
    suppressWarnings(group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = FALSE,
      coords = c('X', 'Y'),
      id = 'ID',
      splitBy = 'yr'
    )[is.na(group), .N != 0]
  ))

  expect_true(
    suppressWarnings(group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = TRUE,
      coords = c('X', 'Y'),
      id = 'ID',
      splitBy = 'yr'
    )[is.na(area), .N != 0]
  ))


})



test_that('splitBy argument doesnt use splitBy column', {
  copyDT <- copy(DT)

  copyDT[, splitBy := family]

  utm <- 32736

  expect_true(
    suppressWarnings(group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = FALSE,
      coords = c('X', 'Y'),
      id = 'ID',
      splitBy = 'jul'
    ))[, uniqueN(jul), group][, all(V1 == 1)]
  )

  expect_true(
    ! 'splitBy' %in%
      suppressWarnings(group_polys(
        DT = copyDT,
        projection = utm,
        hrType = 'mcp',
        hrParams = list(percent = 95),
        area = TRUE,
        coords = c('X', 'Y'),
        id = 'ID',
        splitBy = 'jul'
      ))
  )

})


test_that('proportion within 0-100, area > 0', {

  out_mcp <- suppressWarnings(group_polys(
    DT = DT,
    projection = utm,
    hrType = 'mcp',
    hrParams = list(percent = 95),
    area = TRUE,
    coords = c('X', 'Y'),
    id = 'ID'
  ))

  expect_gte(min(out_mcp$proportion),
             units::as_units(0, 'percent'))
  expect_lte(max(out_mcp$proportion),
             units::as_units(100.01, 'percent'))
  expect_gte(min(out_mcp$area),
             units::as_units(0, 'm^2'))


  out_kernel <- suppressWarnings(group_polys(
    DT = DT,
    projection = utm,
    hrType = 'kernel',
    hrParams = list(percent = 95),
    area = TRUE,
    coords = c('X', 'Y'),
    id = 'ID'
  ))

  expect_gte(min(out_kernel$proportion),
             units::as_units(0, 'percent'))
  expect_lte(max(out_kernel$proportion),
             units::as_units(100.01, 'percent'))
  expect_gte(min(out_kernel$area),
             units::as_units(0, 'm^2'))

})


test_that('sfPolys has area column', {
  sfPolys <- build_polys(DT, projection = utm, hrType = 'kernel',
                         hrParams = list(grid = 60, percent = 95),
                         id = 'ID', coords = c('X', 'Y'))

  sfPolys$area <- NULL

  expect_error(
    group_polys(sfPolys = sfPolys,
                area = TRUE,
                id = 'id'),
    'please ensure column "area"',
    fixed = TRUE
  )
})
