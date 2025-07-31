#' Bearing difference based edge lists
#' @family Edge-list generation
edge_direction <- function(DT = NULL,
                    threshold,
                    id = NULL,
                    coords = NULL,
                    timegroup,
                    splitBy = NULL,
                    returnDist = FALSE,
                    fillNA = TRUE) {

  # due to NSE notes in R CMD check
  N <- ID1 <- ID2 <- value <- . <- NULL

  if (is.null(DT)) {
    stop('input DT required')
  }

  if (missing(threshold)) {
    stop('threshold required')
  }

  if (!is.null(threshold) & !is.numeric(threshold)) {
    stop('threshold must be numeric or NULL')
  }

  if (is.numeric(threshold) && threshold <= 0) {
    stop('threshold must be greater than 0')
  }

  if (is.null(id)) {
    stop('ID field required')
  }

  if (length(coords) != 2) {
    stop('coords requires a vector of column names for coordinates X and Y')
  }

  if (missing(timegroup)) {
    stop('timegroup required')
  }

  if (any(!(
    c(timegroup, id, coords, splitBy) %in% colnames(DT)
  ))) {
    stop(paste0(
      as.character(paste(setdiff(
        c(timegroup, id, coords, splitBy),
        colnames(DT)
      ), collapse = ', ')),
      ' field(s) provided are not present in input DT'
    ))
  }

  if (any(!(DT[, vapply(.SD, is.numeric, TRUE), .SDcols = coords]))) {
    stop('coords must be numeric')
  }

  if (!is.null(timegroup)) {
    if (any(unlist(lapply(DT[, .SD, .SDcols = timegroup], class)) %in%
            c('POSIXct', 'POSIXlt', 'Date', 'IDate', 'ITime', 'character'))) {
      warning(
        strwrap(
          prefix = " ",
          initial = "",
          x = 'timegroup provided is a date/time
          or character type, did you use group_times?'
        )
      )
    }
  }

  if (is.null(timegroup) && is.null(splitBy)) {
    splitBy <- NULL
  } else {
    splitBy <- c(splitBy, timegroup)
    if (DT[, .N, by = c(id, splitBy, timegroup)][N > 1, sum(N)] != 0) {
      warning(
        strwrap(
          prefix = " ",
          initial = "",
          x = 'found duplicate id in a
          timegroup and/or splitBy -
          does your group_times threshold match the fix rate?'
        )
      )
    }
  }

  if ('splitBy' %in% colnames(DT)) {
    warning(
      strwrap(x = 'a column named "splitBy" was found in your data.table,
              renamed to "split_by" to avoid confusion with the argument
              "splitBy"')
    )
    data.table::setnames(DT, 'splitBy', 'split_by')
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
