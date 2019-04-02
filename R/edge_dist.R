#' Distance based edge lists
#'
#' \code{edge_dist} returns edge lists defined by a spatial distance within the user defined threshold. The function accepts a \code{data.table} with relocation data, individual identifiers and a threshold argument. The threshold argument is used to specify the criteria for distance between points which defines a group. Relocation data should be in two columns representing the X and Y coordinates.
#'
#'
#' @inheritParams group_pts
#'
#' @export
#'
#' @family Edge-list generation
#'
#' @examples
#' # Load data.table
#' library(data.table)
#'
#' # Read example data
#' DT <- fread(system.file("extdata", "DT.csv", package = "spatsoc"))
#'
#' # Cast the character column to POSIXct
#' DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
#'
#' # Temporal grouping
#' group_times(DT, datetime = 'datetime', threshold = '20 minutes')
#'
#' # Edge list generation
#' group_pts(DT, threshold = 50, id = 'ID',
#'           coords = c('X', 'Y'), timegroup = 'timegroup')
#'
edge_dist <- function(DT = NULL,
                      threshold = NULL,
                      id = NULL,
                      coords = NULL,
                      timegroup = NULL,
                      splitBy = NULL) {
  # due to NSE notes in R CMD check
  N <- withinGroup <- ..id <- ..coords <- NULL

  if (is.null(DT)) {
    stop('input DT required')
  }

  if (is.null(threshold)) {
    stop('threshold required')
  }

  if (!is.numeric(threshold)) {
    stop('threshold must be numeric')
  }

  if (threshold <= 0) {
    stop('threshold must be greater than 0')
  }

  if (is.null(id)) {
    stop('ID field required')
  }

  if (length(coords) != 2) {
    stop('coords requires a vector of column names for coordinates X and Y')
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

  DT[, {
    distMatrix <-
      as.matrix(stats::dist(.SD[, 2:3], method = 'euclidean'))
    colnames(distMatrix) <- rownames(distMatrix) <- .SD[[1]]
    lt <- data.table(melt(distMatrix < threshold))
    lt[Var1 != Var2 & (value), .(leftID = Var1, rightID = Var2)]
  },
  by = splitBy, .SDcols = c(id, coords)]
}
