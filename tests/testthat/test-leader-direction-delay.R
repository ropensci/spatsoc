# Test leader_direction_delay
context('test leader_direction_delay')

library(spatsoc)

DT <- fread('../testdata/DT.csv')
id <- 'ID'
datetime <- 'datetime'
timethreshold <- '20 minutes'
threshold <- 50
coords <- c('X', 'Y')
timegroup <- 'timegroup'
group <- 'group'
utm <- 32736
window <- 3


DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
group_times(DT, datetime = datetime, threshold = timethreshold)
direction_step(DT, id, coords, utm)
edges <- edge_dist(
  DT,
  threshold = threshold,
  id = id,
  coords = coords,
  timegroup = timegroup,
  returnDist = TRUE,
  fillNA = FALSE
)
dyad_id(edges, id1 = 'ID1', id2 = 'ID2')
fusion_id(edges, threshold = threshold)
leader_delay <- leader_direction_delay(
  edges = edges,
  DT = DT,
  window = 3,
  id = 'ID'
)

clean_edges <- copy(edges)
clean_DT <- copy(DT)

test_that('edges, DT are required', {
  expect_error(leader_direction_delay(edges, DT = NULL))
  expect_error(leader_direction_delay(edges = NULL, DT))
})

test_that('arguments required, otherwise error detected', {
  expect_error(
    leader_direction_delay(edges, DT, id = NULL),
    'id must be provided'
  )
  expect_error(
    leader_direction_delay(edges, DT, id = id, window = NULL),
    'window must be provided'
  )
})

test_that('window is numeric, timegroup is integer', {
  expect_error(
    leader_direction_delay(edges, DT, id = id, window = 'potato'),
    'numeric'
  )
  copy_edges <- copy(clean_edges)
  copy_edges[, timegroup := as.character(timegroup)]
  expect_error(
    leader_direction_delay(copy_edges, DT, id = id, window = 2),
    'integer'
  )

  copy_DT <- copy(clean_DT)
  copy_DT[, timegroup := as.character(timegroup)]
  expect_error(
    leader_direction_delay(edges, copy_DT, id = id, window = 2),
    'integer'
  )
})

test_that('column names must exist in DT', {
  expect_error(leader_direction_delay(edges, DT, id = 'potato'), 'potato field')

  copy_edges <- copy(clean_edges)
  copy_edges[, timegroup := NULL]
  expect_error(
    leader_direction_delay(copy_edges, DT, id = id, window = window),
    'timegroup field'
  )

  copy_edges <- copy(clean_edges)
  copy_edges[, fusionID := NULL]
  expect_error(
    leader_direction_delay(copy_edges, DT, id = id, window = window),
    'fusionID field'
  )

  copy_edges <- copy(clean_edges)
  copy_edges[, dyadID := NULL]
  expect_error(
    leader_direction_delay(copy_edges, DT, id = id, window = window),
    'dyadID field'
  )

  expect_error(
    leader_direction_delay(
      edges,
      DT,
      id = id,
      window = window,
      direction = 'potato'
    ),
    'potato field'
  )

  copy_DT <- copy(clean_DT)
  copy_DT[, timegroup := NULL]
  expect_error(
    leader_direction_delay(edges, copy_DT, id = id, window = window),
    'timegroup field'
  )
})

test_that('output length as expected', {
  # nrow(edges) is less than nrow(leader_delay) since leader_delay is aggregate
  expect_gt(nrow(edges), nrow(leader_delay))

  # number of ID1, ID2 combinations * the length of seq_tau / only ID1-ID2 in output
  expect_equal(
    nrow(edges[, .GRP, .(ID1, ID2)]) * length(-window:window) / 2,
    nrow(leader_delay)
  )
})

test_that('columns added to the result DT', {
  expected_cols <- c(
    'ID1',
    'ID2',
    'dyadID',
    'direction_delay',
    'direction_cor'
  )

  expect_setequal(
    expected_cols,
    colnames(leader_direction_delay(edges, DT, id = id, window = window))
  )
  expect_length(
    expected_cols,
    ncol(leader_direction_delay(edges, DT, id = id, window = window))
  )
})

test_that('columns in result match expected type', {
  out <- leader_direction_delay(edges, DT, id = id, window = window)
  expect_type(out$direction_delay, 'integer')
  expect_type(out$direction_cor, 'double')
})

test_that('returns a data.table', {
  expect_s3_class(
    leader_direction_delay(edges, DT, id = id, window = window),
    'data.table'
  )
})

test_that('ID1 matches first col from tstrsplit of dyadID', {
  expect_equal(
    leader_direction_delay(edges, DT, id = id, window = window)$ID1,
    leader_direction_delay(edges, DT, id = id, window = window)[, tstrsplit(
      dyadID,
      '-'
    )[[1]]]
  )
})

N_id <- 5
N_seq <- 10
seq_xy <- c(
  seq(0, 5, length.out = N_seq / 2),
  seq(5.1, 0, length.out = N_seq / 2)
)
DT_expect <- data.table(
  X = rep(seq_xy, each = N_id),
  Y = rep(seq_xy, each = N_id),
  ID = LETTERS[seq.int(N_id)]
)
DT_expect[, timegroup := seq.int(.GRP)[.GRP] + seq.int(.N), by = ID]
setorder(DT_expect, timegroup)
direction_step(DT_expect, id, coords, crs = 4326)

edge_expect <- edge_dist(
  DT_expect,
  threshold = 100,
  id,
  coords,
  timegroup,
  returnDist = TRUE
)
dyad_id(edge_expect, 'ID1', 'ID2')
fusion_id(edge_expect, threshold = 50)

window <- 5
delay_expect <- direction_delay(
  edge_expect,
  DT_expect,
  window = window,
  id = id
)
leader_expect <- leader_direction_delay(delay_expect)

test_that('expected results are returned', {
  expect_lte(nrow(leader_expect), delay_expect[, uniqueN(dyadID) * 2])

  expect_equal(
    leader_expect[, uniqueN(abs(mean_direction_delay_dyad)), by = dyadID],
    leader_expect[, 1, by = dyadID]
  )

  expect_equal(
    leader_expect[, mean(mean_direction_delay_dyad)],
    0,
    tolerance = 2
  )
  expect_equal(leader_expect[, mean(mean_direction_delay)], 0, tolerance = 2)

  expect_equal(
    leader_expect[
      mean_direction_delay == min(mean_direction_delay),
      unique(ID1)
    ],
    LETTERS[N_id]
  )
  expect_equal(
    leader_expect[
      mean_direction_delay == median(mean_direction_delay),
      unique(ID1)
    ],
    LETTERS[ceiling(N_id / 2)]
  )
  expect_equal(
    leader_expect[
      mean_direction_delay == max(mean_direction_delay),
      unique(ID1)
    ],
    LETTERS[1]
  )
})
