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
  # Edges: missing
  copy_edges <- copy(clean_edges)
  copy_edges[, timegroup := NULL]
  expect_error(
    leader_direction_delay(copy_edges, DT, id = id, window = window),
    'timegroup field'
  )

  copy_edges <- copy(clean_edges)
  copy_edges[, ID1 := NULL]
  expect_error(
    leader_direction_delay(copy_edges, DT, id = id, window = window),
    'ID1 field'
  )

  copy_edges <- copy(clean_edges)
  copy_edges[, ID2 := NULL]
  expect_error(
    leader_direction_delay(copy_edges, DT, id = id, window = window),
    'ID2 field'
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

  # Edges: colname not found in edges
  expect_error(leader_direction_delay(edges, DT, id = 'potato'), 'potato field')

  expect_error(
    leader_direction_delay(
      edges,
      DT,
      id = id,
      window = window,
      splitBy = 'potato'
    ),
    'potato field'
  )

  # DT: missing
  copy_DT <- copy(clean_DT)
  copy_DT[, c(id) := NULL]
  expect_error(
    leader_direction_delay(edges, copy_DT, id = id, window = window),
    paste0(id, ' field')
  )

  copy_DT <- copy(clean_DT)
  copy_DT[, 'timegroup' := NULL]
  expect_error(
    leader_direction_delay(edges, copy_DT, id = id, window = window),
    'timegroup field'
  )

  copy_DT <- copy(clean_DT)
  copy_DT[, 'direction' := NULL]
  expect_error(
    leader_direction_delay(edges, copy_DT, id = id, window = window),
    'direction field'
  )

  # DT: colname not found in DT
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
})

test_that('output length as expected', {
  expect_lt(nrow(leader_delay), nrow(edges))

  expect_lte(
    nrow(leader_delay),
    edges[, uniqueN(dyadID)] * length(-window:window),
  )
})

test_that('n unique matches ID1, ID2 and optionally splitBy', {
  lead_delay <- leader_direction_delay(edges, DT, id = id, window = window)

  edges[, split_tg := timegroup < median(timegroup)]
  lead_delay_split <- leader_direction_delay(
    edges,
    DT,
    id = id,
    window = window,
    splitBy = 'split_tg'
  )

  expect_lte(
    lead_delay[, uniqueN(direction_cor)],
    edges[, uniqueN(dyadID)] * length(-window:window)
  )

  expect_in(lead_delay[, unique(dyadID)], edges[, unique(dyadID)])

  expect_lte(
    lead_delay_split[, uniqueN(direction_cor)],
    edges[, uniqueN(dyadID)] *
      length(-window:window) *
      edges[, uniqueN(split_tg)]
  )

  expect_in(lead_delay_split[, unique(dyadID)], edges[, unique(dyadID)])

  expect_lte(
    lead_delay[, uniqueN(direction_cor)],
    lead_delay_split[, uniqueN(direction_cor)]
  )
})

test_that('columns added to the result DT', {
  expected_cols <- c(
    'ID1',
    'ID2',
    'dyadID',
    'direction_delay',
    'direction_cor',
    'max_direction_cor'
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
  expect_type(out$max_direction_cor, 'logical')
})

test_that('splitBy argument doesnt use splitBy column', {
  copy_edges <- copy(clean_edges)
  copy_edges[, splitBy := timegroup]

  expect_lte(
    nrow(leader_direction_delay(
      copy_edges,
      DT,
      id = id,
      window = window
    )),
    edges[, uniqueN(dyadID)] * length(-window:window)
  )

  delay_split_tg <- leader_direction_delay(
    copy_edges,
    DT,
    id = id,
    window = window,
    splitBy = 'timegroup'
  )

  expect_all_true(delay_split_tg[direction_delay != 0, is.na(direction_cor)])
})

test_that('returns a data.table', {
  expect_s3_class(
    leader_direction_delay(edges, DT, id = id, window = window),
    'data.table'
  )
})

test_that('direction colname can be different than default', {
  copyDT <- copy(clean_DT)
  setnames(copyDT, 'direction', 'potato')
  expect_s3_class(
    leader_direction_delay(
      edges,
      copyDT,
      id = id,
      window = window,
      direction = 'potato'
    ),
    'data.table'
  )
})

test_that('window column in edge, DT does not influence results', {
  copyDT <- copy(clean_DT)
  copyDT[, window := 42]

  copyEdges <- copy(clean_edges)
  copyEdges[, window := 42]

  expect_equal(
    leader_direction_delay(edges, copyDT, id = id, window = window),
    leader_direction_delay(edges, DT, id = id, window = window)
  )

  expect_equal(
    leader_direction_delay(copyEdges, DT, id = id, window = window),
    leader_direction_delay(edges, DT, id = id, window = window)
  )

  expect_equal(
    leader_direction_delay(copyEdges, copyDT, id = id, window = window),
    leader_direction_delay(edges, DT, id = id, window = window)
  )
})

test_that('setorder doesnt impact results', {
  reorder_edges <- copy(edges)
  setorder(reorder_edges, ID1)
  expect_equal(
    leader_direction_delay(edges = edges, DT, window, id)[, sort(
      direction_delay
    )],
    leader_direction_delay(edges = reorder_edges, DT, window, id)[, sort(
      direction_delay
    )]
  )

  reorder_DT <- copy(DT)
  setorder(reorder_DT, ID)
  expect_equal(
    leader_direction_delay(edges = edges, DT, window, id)[, sort(
      direction_delay
    )],
    leader_direction_delay(edges = edges, reorder_DT, window, id)[, sort(
      direction_delay
    )]
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

test_that('increasing min nobs required increases NAs', {
  min_nobs_0 <- leader_direction_delay(
    edges,
    DT,
    id = id,
    window = window,
    min_nobs = 0
  )

  min_nobs_5 <- leader_direction_delay(
    edges,
    DT,
    id = id,
    window = window,
    min_nobs = 5
  )

  expect_lt(
    min_nobs_0[, sum(is.na(direction_cor))],
    min_nobs_5[, sum(is.na(direction_cor))]
  )
})

test_that('equality Cij(tau) = Cji(-tau) holds', {
  # With strict fusion_id settings
  copy_edges <- copy(edges)
  copy_edges[, fusionID := NULL]
  fusion_id(copy_edges, threshold = threshold)

  # Calculate direction correlation and delay from ij direction
  # in spatsoc, this is dyadID is used to assign i to the first ID from the
  # dyadID and j to the second.
  # For example, if dyadID is 'A-B', i: A and j:B
  out <- leader_direction_delay(copy_edges, DT, id = id, window = window)

  # Next, we reverse this relationship to calculate Cji, with these IDs reversed
  copy_edges[,
    dyadID := do.call(paste, c(rev(tstrsplit(dyadID, '-')), sep = '-'))
  ]

  out_rev <- leader_direction_delay(copy_edges, DT, id = id, window = window)

  # Preserve the delay and correlation from the ID2 / ji perspective
  out_rev[, direction_delay_ID2 := direction_delay]
  out_rev[, direction_cor_ID2 := direction_cor]

  # Revert the reversed dyadID
  out_rev[, dyadID := do.call(paste, c(rev(tstrsplit(dyadID, '-')), sep = '-'))]

  # Multiple the Cji delay by -1 for the merge
  out_rev[, direction_delay := direction_delay * -1]

  # Merge based on dyadID and direction
  m <- out[out_rev, on = .(dyadID, direction_delay)]

  expect_all_true(
    m[, ID1 != i.ID1]
  )
  expect_all_true(
    m[, ID2 != i.ID2]
  )
  expect_all_true(
    m[, direction_delay == -direction_delay_ID2]
  )

  expect_all_true(
    m[
      !is.na(direction_cor) & !is.na(direction_cor_ID2),
      direction_cor == direction_cor_ID2
    ]
  )

  expect_equal(m[, .N, is.na(direction_cor)], m[, .N, is.na(direction_cor_ID2)])

  # With loose fusion_id settings
  copy_edges <- copy(edges)
  copy_edges[, fusionID := NULL]
  fusion_id(
    copy_edges,
    threshold = threshold,
    n_max_missing = 1,
    allow_split = TRUE
  )

  # Calculate direction correlation and delay from ij direction
  # in spatsoc, this is dyadID is used to assign i to the first ID from the
  # dyadID and j to the second.
  # For example, if dyadID is 'A-B', i: A and j:B
  out <- leader_direction_delay(copy_edges, DT, id = id, window = window)

  # Next, we reverse this relationship to calculate Cji, with these IDs reversed
  copy_edges[,
    dyadID := do.call(paste, c(rev(tstrsplit(dyadID, '-')), sep = '-'))
  ]

  out_rev <- leader_direction_delay(copy_edges, DT, id = id, window = window)

  # Preserve the delay and correlation from the ID2 / ji perspective
  out_rev[, direction_delay_ID2 := direction_delay]
  out_rev[, direction_cor_ID2 := direction_cor]

  # Revert the reversed dyadID
  out_rev[, dyadID := do.call(paste, c(rev(tstrsplit(dyadID, '-')), sep = '-'))]

  # Multiple the Cji delay by -1 for the merge
  out_rev[, direction_delay := direction_delay * -1]

  # Merge based on dyadID and direction
  m <- out[out_rev, on = .(dyadID, direction_delay)]

  expect_all_true(
    m[, ID1 != i.ID1]
  )
  expect_all_true(
    m[, ID2 != i.ID2]
  )
  expect_all_true(
    m[, direction_delay == -direction_delay_ID2]
  )

  expect_all_true(
    m[
      !is.na(direction_cor) & !is.na(direction_cor_ID2),
      direction_cor == direction_cor_ID2
    ]
  )

  expect_equal(m[, .N, is.na(direction_cor)], m[, .N, is.na(direction_cor_ID2)])
})
