# Test edge_delay
context('test edge_delay')

library(spatsoc)

DT <- fread('../testdata/DT.csv')
id <- 'ID'
datetime <- 'datetime'
timethreshold <- '20 minutes'
threshold <- 50
coords <- c('X', 'Y')
timegroup <- 'timegroup'
group <- 'group'
projection <- 32736
window <- 3


DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
group_times(DT, datetime = datetime, threshold = timethreshold)
direction_step(DT, id, coords, projection)
edges <- edge_dist(DT, threshold = threshold, id = id,
                   coords = coords, timegroup = timegroup,
                   returnDist = TRUE, fillNA = FALSE)
dyad_id(edges, id1 = 'ID1', id2 = 'ID2')
fusion_id(edges, threshold = threshold)

clean_DT <- copy(DT)
clean_edges <- copy(edges)

# edge_delay(edges = edges, DT = DT, id = id, window = window)

test_that('edges, DT are required', {
  expect_error(edge_delay(edges, DT = NULL))
  expect_error(edge_delay(edges = NULL, DT))
})

test_that('arguments required, otherwise error detected', {
  expect_error(edge_delay(edges, DT, id = NULL),
               'id column name required')
  expect_error(edge_delay(edges, DT, id = id, window = NULL),
               'window is required')
})

test_that('window is numeric, timegroup is integer', {
  expect_error(edge_delay(edges, DT, id = id, window = 'potato'),
               'numeric')
  copy_edges <- copy(clean_edges)
  copy_edges[, timegroup := as.character(timegroup)]
  expect_error(edge_delay(copy_edges, DT, id = id, window = 2),
               'integer')

  copy_DT <- copy(clean_DT)
  copy_DT[, timegroup := as.character(timegroup)]
  expect_error(edge_delay(edges, copy_DT, id = id, window = 2),
               'integer')
})

test_that('column names must exist in DT', {
  expect_error(edge_delay(edges, DT, id = 'potato'),
               'potato field')

  copy_edges <- copy(clean_edges)
  copy_edges[, timegroup := NULL]
  expect_error(edge_delay(copy_edges, DT, id = id, window = window),
               'timegroup field')

  copy_edges <- copy(clean_edges)
  copy_edges[, fusionID := NULL]
  expect_error(edge_delay(copy_edges, DT, id = id, window = window),
               'fusionID field')

  copy_edges <- copy(clean_edges)
  copy_edges[, dyadID := NULL]
  expect_error(edge_delay(copy_edges, DT, id = id, window = window),
               'dyadID field')

  expect_error(edge_delay(edges, DT, id = id, window = window,
                          direction = 'potato'),
               'potato field')

  copy_DT <- copy(clean_DT)
  copy_DT[, timegroup := NULL]
  expect_error(edge_delay(edges, copy_DT, id = id, window = window),
               'timegroup field')
})

test_that('no rows are added to the result edges', {
  expect_equal(nrow(edges),
               nrow(edge_delay(edges, DT, id = id, window = window)))
})

test_that('column added to the result DT', {
  copyEdges <- copy(edges)

  expected_cols <- c(colnames(copyEdges), 'direction_delay', 'direction_diff')

  expect_setequal(expected_cols,
                  colnames(edge_delay(edges, DT, id = id, window = window)))
  expect_equal(ncol(edge_delay(edges, DT, id = id, window = window)),
               length(expected_cols))

})

test_that('column added to the result DT is integer', {
  expect_type(edge_delay(edges, DT, id = id,
                         window = window)$direction_delay, 'integer')
})

test_that('returns a data.table', {
  expect_s3_class(edge_delay(edges, DT, id = id, window = window), 'data.table')
})

test_that('direction colname can be different than default', {
  copyDT <- copy(clean_DT)
  setnames(copyDT, 'direction', 'potato')
  expect_s3_class(edge_delay(edges, copyDT, id = id, window = window,
                             direction = 'potato'),
                  'data.table')
})

test_that('window column in edge, DT does not influence results', {
  copyDT <- copy(clean_DT)
  copyDT[, window := 42]

  copyEdges <- copy(clean_edges)
  copyEdges[, window := 42]

  expect_equal(
    edge_delay(edges, copyDT, id = id, window = window),
    edge_delay(edges, DT, id = id, window = window)
  )

  expect_equal(
    edge_delay(copyEdges, DT, id = id,
               window = window)[, .SD, .SDcols = -'window'],
    edge_delay(edges, DT, id = id, window = window)
  )

  expect_equal(
    edge_delay(copyEdges, copyDT, id = id,
               window = window)[, .SD, .SDcols = -'window'],
    edge_delay(edges, DT, id = id, window = window)
  )

})

test_that('rows with NAs in ID1, ID2, dyadID, fusionID are dropped', {
  n_dropped <- 42
  na_edges <- copy(clean_edges)
  na_edges[seq.int(n_dropped), ID1 := NA]

  expect_equal(
    edge_delay(edges = na_edges, DT = DT, window, id)[, .N + n_dropped],
    edge_delay(edges = edges, DT = DT, window, id)[, .N]
  )

  na_edges <- copy(clean_edges)
  na_edges[seq.int(n_dropped), ID2 := NA]

  expect_equal(
    edge_delay(edges = na_edges, DT = DT, window, id)[, .N + n_dropped],
    edge_delay(edges = edges, DT = DT, window, id)[, .N]
  )

  na_edges <- copy(clean_edges)
  na_edges[seq.int(n_dropped), dyadID := NA]

  expect_equal(
    edge_delay(edges = na_edges, DT = DT, window, id)[, .N + n_dropped],
    edge_delay(edges = edges, DT = DT, window, id)[, .N]
  )
  na_edges <- copy(clean_edges)
  na_edges[seq.int(n_dropped), fusionID := NA]

  expect_equal(
    edge_delay(edges = na_edges, DT = DT, window, id)[, .N + n_dropped],
    edge_delay(edges = edges, DT = DT, window, id)[, .N]
  )

})

test_that('setorder doesnt impact results', {
  reorder_edges <- copy(edges)
  setorder(reorder_edges, ID1)
  expect_equal(
    edge_delay(edges = edges, DT, window, id)[, sort(direction_delay)],
    edge_delay(edges = reorder_edges, DT, window, id)[, sort(direction_delay)]
  )

  reorder_DT <- copy(DT)
  setorder(reorder_DT, ID)
  expect_equal(
    edge_delay(edges = edges, DT, window, id)[, sort(direction_delay)],
    edge_delay(edges = edges, reorder_DT, window, id)[, sort(direction_delay)]
  )
})

N_id <- 5
N_seq <- 10
seq_xy <- c(seq(0, 5, length.out = N_seq / 2),
            seq(5.1, 0, length.out = N_seq / 2))
DT_expect <- data.table(
  X = rep(seq_xy, each = N_id),
  Y = rep(seq_xy, each = N_id),
  ID = LETTERS[seq.int(N_id)]
)
DT_expect[, timegroup := seq.int(.GRP)[.GRP] + seq.int(.N), by = ID]
setorder(DT_expect, timegroup)
direction_step(DT_expect, id, coords, projection = 4326)

edge_expect <- edge_dist(DT_expect, threshold = 100, id, coords, timegroup,
                         returnDist = TRUE)
dyad_id(edge_expect, 'ID1', 'ID2')
fusion_id(edge_expect)

window <- 5
delay_expect <- edge_delay(edge_expect, DT_expect, window = window, id = id)

leader_expect <- leader_edge_delay(delay_expect)

test_that('expected results are returned', {
  expect_lte(nrow(delay_expect), nrow(edge_expect))
  expect_lte(nrow(DT_expect), nrow(delay_expect))

  mean_delays <- delay_expect[, mean(direction_delay, na.rm = TRUE), by = ID1]

  expect_equal(mean_delays[V1 == min(V1), ID1], LETTERS[N_id])
  expect_equal(mean_delays[V1 == median(V1), ID1], LETTERS[ceiling(N_id / 2)])
  expect_equal(mean_delays[V1 == max(V1), ID1], LETTERS[1])

  mean_delays_wrt_A <- delay_expect[
    ID1 == 'A', mean(direction_delay, na.rm = TRUE), by = ID2]
  expect_equal(mean_delays_wrt_A[V1 == max(V1), ID2], LETTERS[N_id])
})

test_that('exaggerated window size returns the same',  {
  expect_equal(
    edge_delay(edge_expect, DT_expect, window = window, id = id),
    edge_delay(edge_expect, DT_expect, window = window + 10, id = id)
  )

  expect_equal(
    edge_delay(edge_expect, DT_expect, window = window, id = id),
    edge_delay(edge_expect, DT_expect, window = window + 1000, id = id)
  )
})
