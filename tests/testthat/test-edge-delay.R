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

clean_DT <- copy(DT)
clean_edges <- copy(edges)

# edge_delay(edges = edges, DT = DT, id = id, window = window)

test_that('edges, DT are required', {
  expect_error(edge_delay(edges, DT = NULL))
  expect_error(edge_delay(edges = NULL, DT))
})

test_that('arguments required, otherwise error detected', {
  expect_error(edge_delay(edges, DT, id = NULL), 'id must be provided')
  expect_error(
    edge_delay(edges, DT, id = id, window = NULL),
    'window must be provided'
  )
})

test_that('window is numeric, timegroup is integer', {
  expect_error(edge_delay(edges, DT, id = id, window = 'potato'), 'numeric')
  copy_edges <- copy(clean_edges)
  copy_edges[, timegroup := as.character(timegroup)]
  expect_error(edge_delay(copy_edges, DT, id = id, window = 2), 'integer')

  copy_DT <- copy(clean_DT)
  copy_DT[, timegroup := as.character(timegroup)]
  expect_error(edge_delay(edges, copy_DT, id = id, window = 2), 'integer')
})

test_that('column names must exist in DT', {
  expect_error(edge_delay(edges, DT, id = 'potato'), 'potato field')

  copy_edges <- copy(clean_edges)
  copy_edges[, timegroup := NULL]
  expect_error(
    edge_delay(copy_edges, DT, id = id, window = window),
    'timegroup field'
  )

  copy_edges <- copy(clean_edges)
  copy_edges[, fusionID := NULL]
  expect_error(
    edge_delay(copy_edges, DT, id = id, window = window),
    'fusionID field'
  )

  copy_edges <- copy(clean_edges)
  copy_edges[, dyadID := NULL]
  expect_error(
    edge_delay(copy_edges, DT, id = id, window = window),
    'dyadID field'
  )

  expect_error(
    edge_delay(edges, DT, id = id, window = window, direction = 'potato'),
    'potato field'
  )

  copy_DT <- copy(clean_DT)
  copy_DT[, timegroup := NULL]
  expect_error(
    edge_delay(edges, copy_DT, id = id, window = window),
    'timegroup field'
  )
})

test_that('no rows are added to the result edges', {
  expect_equal(
    nrow(edges),
    nrow(edge_delay(edges, DT, id = id, window = window))
  )
})

test_that('column added to the result DT', {
  copyEdges <- copy(edges)

  expected_cols <- c(colnames(copyEdges), 'direction_delay', 'direction_cor')

  expect_setequal(
    expected_cols,
    colnames(edge_delay(edges, DT, id = id, window = window))
  )
  expect_length(
    expected_cols,
    ncol(edge_delay(edges, DT, id = id, window = window))
  )
})

test_that('column added to the result DT is integer', {
  expect_type(
    edge_delay(edges, DT, id = id, window = window)$direction_delay,
    'integer'
  )
})

test_that('returns a data.table', {
  expect_s3_class(edge_delay(edges, DT, id = id, window = window), 'data.table')
})

test_that('direction colname can be different than default', {
  copyDT <- copy(clean_DT)
  setnames(copyDT, 'direction', 'potato')
  expect_s3_class(
    edge_delay(edges, copyDT, id = id, window = window, direction = 'potato'),
    'data.table'
  )
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
    edge_delay(copyEdges, DT, id = id, window = window),
    edge_delay(edges, DT, id = id, window = window)
  )

  expect_equal(
    edge_delay(copyEdges, copyDT, id = id, window = window),
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

test_that('equality Cij(t, tau) == Cji(t + tau, -tau) holds', {
  copy_edges <- copy(edges)
  copy_DT <- copy(DT)

  # Taking from inside of edge_delay, since intermediate matrices are not returned
  drop_nas <- copy_edges[
    !(is.na(fusionID) | is.na(ID1) | is.na(ID2) | is.na(dyadID))
  ]

  forward <- drop_nas[ID1 == tstrsplit(dyadID, '-')[[1L]]]

  tg_matching <- forward[,
    {
      tg <- seq(min(timegroup), max(timegroup))
      list(
        timegroup = tg,
        ID1 = rep(ID1, length.out = length(tg)),
        ID2 = rep(ID2, length.out = length(tg))
      )
    },
    by = .(dyadID, fusionID)
  ]

  direction <- 'direction'
  tg_matching[
    copy_DT,
    direction_ID1 := units::drop_units(DT_direction_column),
    on = c(paste0('ID1 == ', id), 'timegroup == timegroup'),
    env = list(DT_direction_column = paste0('i.', direction))
  ]
  tg_matching[
    copy_DT,
    direction_ID2 := units::drop_units(DT_direction_column),
    on = c(paste0('ID2 == ', id), 'timegroup == timegroup'),
    env = list(DT_direction_column = paste0('i.', direction))
  ]

  seq_tau <- -window:window

  tg_matching[, N_by_fusion := .N, by = fusionID]
  check_equality <- tg_matching[
    N_by_fusion > 1,
    {
      shift_dir_ID2 <- do.call(
        cbind,
        shift(direction_ID2, seq_tau, type = 'lead')
      )
      dif_dir_ID1 <- cos(direction_ID1 - shift_dir_ID2)

      shift_dir_ID1 <- do.call(
        cbind,
        shift(direction_ID1, seq_tau, type = 'lead')
      )
      dif_dir_ID2 <- cos(direction_ID2 - shift_dir_ID1)

      # Indices for non-NA values
      ind <- data.table(which(!is.na(dif_dir_ID1), arr.ind = TRUE))

      # Rename to match equality: rows are t, cols are the column representing tau
      setnames(ind, c('row', 'col'), c('t_val', 'tau_col'))

      # Get each Cij(t, tau) correlation value
      ind[, Cij_t_tau := dif_dir_ID1[t_val, tau_col], by = .(t_val, tau_col)]

      # Match the tau col to the corresponding tau value
      ind[, tau_val := seq_tau[tau_col]]

      # Calculate t + tau
      ind[, t_plus_tau := t_val + tau_val]

      # Calculate -tau then match the corresponding column
      ind[, neg_tau_col := which(seq_tau == -tau_val), by = tau_val]

      # Get each Cji(t + tau, -tau) correlation value
      ind[,
        Cji_t_plus_tau_neg_tau := dif_dir_ID2[t_plus_tau, neg_tau_col],
        by = .(t_plus_tau, neg_tau_col)
      ]

      # Check they are all equal
      list(check_cor = ind[, all(Cij_t_tau == Cji_t_plus_tau_neg_tau)])
    },
    by = fusionID
  ]

  expect_all_true(check_equality$check_cor)
})
