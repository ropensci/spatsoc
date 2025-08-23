#' Direction based edge-lists
#' @family Edge-list generation
edge_direction <- function(
    edges = NULL,
    DT = NULL,
    id = NULL,
    coords = NULL,
    projection = NULL,
    timegroup = 'timegroup') {

  if (is.null(DT)) {
    stop('input DT required')
  }

  if (is.null(edges)) {
    stop('input edges required')
  }

  if (is.null(id)) {
    stop('id column name required')
  }

  if (length(coords) != 2) {
    stop('coords requires a vector of column names for coordinates X and Y')
  }

  if (is.null(timegroup)) {
    stop('timegroup required')
  }

  if (any(!(
    c('dyadID', timegroup) %in% colnames(edges)
  ))) {
    stop(paste0(
      as.character(paste(setdiff(
        c('dyadID', timegroup),
        colnames(edges)
      ), collapse = ', ')),
      ' field(s) provided are not present in input DT'
    ))
  }

  if (any(!(
    c(id, coords, timegroup) %in% colnames(DT)
  ))) {
    stop(paste0(
      as.character(paste(setdiff(
        c(id, coords, timegroup),
        colnames(DT)
      ), collapse = ', ')),
      ' field(s) provided are not present in input DT'
    ))
  }

  if (any(!(DT[, vapply(.SD, is.numeric, TRUE), .SDcols = coords]))) {
    stop('coords must be numeric')
  }

  }



  if (is.null(threshold)) {
    edges <- DT[, {

      distMatrix <-
        as.matrix(stats::dist(.SD[, 2:3], method = 'euclidean'))
      diag(distMatrix) <- NA

      # TODO: fix use delta_rad
      # azMatrix <-
      #   as.matrix(stats::dist(.SD[, 4], method = 'manhattan'))
      # azMatrix <- (azMatrix + pi) %% (2 * pi) - pi
      diag(azMatrix) <- NA

      if (returnDist) {
        l <- data.table::data.table(
          ID1 = .SD[[1]][rep(seq_len(nrow(distMatrix)), ncol(distMatrix))],
          ID2 = .SD[[1]][rep(seq_len(ncol(distMatrix)), each = nrow(distMatrix))],
          distance = c(distMatrix),
          diff_az = c(azMatrix)
        )[ID1 != ID2]
      } else {
        l <- data.table::data.table(
          ID1 = .SD[[1]][rep(seq_len(nrow(distMatrix)), ncol(distMatrix))],
          ID2 = .SD[[1]][rep(seq_len(ncol(distMatrix)), each = nrow(distMatrix))],
          diff_az = c(azMatrix)
        )[ID1 != ID2]
      }
      l
    },
    by = splitBy, .SDcols = c(id, coords, 'bearing')]
  } else {
    edges <- DT[, {

      distMatrix <-
        as.matrix(stats::dist(.SD[, 2:3], method = 'euclidean'))
      diag(distMatrix) <- NA

      azMatrix <-
        as.matrix(stats::dist(.SD[, 4], method = 'euclidean'))
      # TODO: can it actually be > 2 * pi?
      # azMatrix[azMatrix > (2 * pi)] <- azMatrix[azMatrix > (2 * pi)] - (2 * pi)
      diag(azMatrix) <- NA

      w <- which(distMatrix < threshold, arr.ind = TRUE)

      # TODO: use which for azMatrix
      if (returnDist) {
        l <- list(ID1 = .SD[[1]][w[, 1]],
                  ID2 = .SD[[1]][w[, 2]],
                  distance = distMatrix[w],
                  diff_az = c(azMatrix))
      } else {
        l <- list(ID1 = .SD[[1]][w[, 1]],
                  ID2 = .SD[[1]][w[, 2]],
                  diff_az = c(azMatrix))
      }
      l
    },
    by = splitBy, .SDcols = c(id, coords, 'bearing')]
  }


  if (fillNA) {
    merge(edges,
          unique(DT[, .SD, .SDcols = c(splitBy, id)]),
          by.x = c(splitBy, 'ID1'),
          by.y = c(splitBy, id),
          all = TRUE)
  } else {
    return(edges)
  }
}
