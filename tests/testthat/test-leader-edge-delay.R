# Test leader_edge_delay
context('test leader_edge_delay')

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
delay <- edge_delay(edges, DT, id = id, window = window)
leader_delay <- leader_edge_delay(delay, threshold = 0.5)

# leader_edge_delay(edges = edges, threshold = 0.5, splitBy = 'population')

clean_delay <- copy(delay)

test_that('edges required', {
  expect_error(leader_edge_delay(edges = NULL))
})

test_that('threshold is numeric', {
  expect_error(leader_edge_delay(delay, threshold = 'potato'),
               'numeric')
})

test_that('column names must exist in DT', {
  missing_delay <- copy(clean_delay)[, direction_diff := NULL]
  expect_error(leader_edge_delay(missing_delay),
               'direction_diff')

  missing_delay <- copy(clean_delay)[, direction_delay := NULL]
  expect_error(leader_edge_delay(missing_delay),
               'direction_delay')

  missing_delay <- copy(clean_delay)[, ID1 := NULL]
  expect_error(leader_edge_delay(missing_delay),
               'ID1')

  missing_delay <- copy(clean_delay)[, ID2 := NULL]
  expect_error(leader_edge_delay(missing_delay),
               'ID2')

})

test_that('non-numeric cols passed as direction_diff and _delay error', {
  char_delay <- copy(delay)[, direction_diff := as.character(direction_diff)]
  expect_error(
    leader_edge_delay(char_delay, threshold = 0.5),
    "must be numeric"
  )
  char_delay <- copy(delay)[, direction_delay := as.character(direction_delay)]
  expect_error(
    leader_edge_delay(char_delay, threshold = 0.5),
    "must be numeric"
  )
})

test_that('output length as expected', {
  # nrow(delay) is less than nrow(leader_delay) since leader_delay is aggregate
  expect_gt(nrow(delay), nrow(leader_delay))

  # nrow(delay) is less than when threshold is 1e-2 since some dyads will drop
  expect_gt(
    nrow(leader_edge_delay(delay)),
    nrow(leader_edge_delay(delay, threshold = 1e-2))
  )
  # nrow(delay) is the same when threshold is 1e3 since no rows will drop
  expect_equal(
    nrow(leader_edge_delay(delay)),
    nrow(leader_edge_delay(delay, threshold = 1e3))
  )

  # nrow(delay) with tiny threshold is 0 since all rows will drop
  expect_equal(nrow(leader_edge_delay(delay, threshold = 1e-3)), 0)
})

test_that('columns added to the result DT', {
  expected_cols <- c('ID1', 'ID2', 'dyadID',
                     'mean_direction_delay_dyad', 'mean_direction_delay')

  expect_setequal(expected_cols, colnames(leader_edge_delay(delay)))
  expect_length(expected_cols, ncol(leader_edge_delay(delay)))
})

test_that('column added to the result DT is double', {
  expect_type(leader_edge_delay(delay)$mean_direction_delay_dyad, 'double')
  expect_type(leader_edge_delay(delay)$mean_direction_delay, 'double')
})

test_that('returns a data.table', {
  expect_s3_class(leader_edge_delay(delay), 'data.table')
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
direction_step(DT_expect, id, coords, crs = 4326)

edge_expect <- edge_dist(DT_expect, threshold = 100, id, coords, timegroup,
                          returnDist = TRUE)
dyad_id(edge_expect, 'ID1', 'ID2')
fusion_id(edge_expect)

window <- 5
delay_expect <- edge_delay(edge_expect, DT_expect, window = window, id = id)
leader_expect <- leader_edge_delay(delay_expect)

test_that('expected results are returned', {
  expect_lte(nrow(leader_expect), delay_expect[, uniqueN(dyadID) * 2])

  expect_equal(leader_expect[, uniqueN(abs(mean_direction_delay_dyad)),
                             by = dyadID],
               leader_expect[, 1, by = dyadID])

  expect_equal(leader_expect[, mean(mean_direction_delay_dyad)], 0,
               tolerance = 2)
  expect_equal(leader_expect[, mean(mean_direction_delay)], 0,
               tolerance = 2)

  expect_equal(
    leader_expect[mean_direction_delay == min(mean_direction_delay),
                  unique(ID1)],
    LETTERS[N_id]
  )
  expect_equal(
    leader_expect[mean_direction_delay == median(mean_direction_delay),
                  unique(ID1)],
    LETTERS[ceiling(N_id / 2)]
  )
  expect_equal(
    leader_expect[mean_direction_delay == max(mean_direction_delay),
                  unique(ID1)],
    LETTERS[1]
  )
})
