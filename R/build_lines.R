#' Build Lines
#'
#'
#' `build_lines` generates a simple feature collection with LINESTRINGs from a
#' `data.table`. The function accepts a `data.table` with relocation data,
#' individual identifiers, a sorting column and a `projection`. The relocation
#' data is transformed into LINESTRINGs for each individual and, optionally,
#' combination of columns listed in `splitBy`. Relocation data should be in two
#' columns representing the X and Y coordinates.
#'
#' ## R-spatial evolution
#'
#'  Please note, spatsoc has followed updates from R spatial, GDAL and PROJ
#'  for handling projections, see more at
#'  \url{https://r-spatial.org/r/2020/03/17/wkt.html}.
#'
#' In addition, `build_lines` previously used [sp::SpatialLines] but has been
#' updated to use [sf::st_as_sf] and [sf::st_linestring] according to the
#' R-spatial evolution, see more at
#' \url{https://r-spatial.org/r/2022/04/12/evolution.html}.
#'
#' ## Notes on arguments
#'
#' The `projection` argument expects a numeric or character defining the
#' coordinate reference system.
#' For example, for UTM zone 36N (EPSG 32736), the projection argument is either
#'  `projection = 'EPSG:32736'` or `projection = 32736`.
#'  See details in [`sf::st_crs()`] and \url{https://spatialreference.org}
#'  for a list of EPSG codes.
#'
#' The `sortBy` argument is used to order the input `DT` when creating
#' sf LINESTRINGs. It must a column in the input `DT` of type
#' POSIXct to ensure the rows are sorted by date time.
#'
#' The `splitBy` argument offers further control building LINESTRINGs.
#' If in your input `DT`, you have multiple temporal groups (e.g.: years) for
#' example, you can provide the name of the column which identifies them and
#' build LINESTRINGs for each individual in each year.
#'
#' `build_lines` is used by `group_lines` for grouping overlapping
#' lines generated from relocations.
#'
#' @return `build_lines` returns an sf LINESTRING object with a line
#' for each individual (and optionally `splitBy` combination).
#'
#' Individuals (or combinations of individuals and `splitBy`) with less than two
#'  relocations are dropped since it requires at least two relocations to
#'  build a line.
#'
#' @inheritParams group_lines
#' @inheritParams build_polys
#'
#' @export
#'
#' @family Build functions
#' @seealso \code{\link{group_lines}}
#'
#' @import data.table
#'
#' @examples
#' # Load data.table
#' library(data.table)
#' \dontshow{data.table::setDTthreads(1)}
#'
#' # Read example data
#' DT <- fread(system.file("extdata", "DT.csv", package = "spatsoc"))
#'
#' # Cast the character column to POSIXct
#' DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
#'
#' # EPSG code for example data
#' utm <- 32736
#'
#' # Build lines for each individual
#' lines <- build_lines(DT, projection = utm, id = 'ID', coords = c('X', 'Y'),
#'             sortBy = 'datetime')
#'
#' # Build lines for each individual by year
#' DT[, yr := year(datetime)]
#' lines <- build_lines(DT, projection = utm, id = 'ID', coords = c('X', 'Y'),
#'             sortBy = 'datetime', splitBy = 'yr')
build_lines <-
  function(DT = NULL,
           projection = NULL,
           id = NULL,
           coords = NULL,
           sortBy = NULL,
           splitBy = NULL) {

    # due to NSE notes in R CMD check
    dropped <- . <- NULL

    if (is.null(DT)) {
      stop('input DT required')
    }

    if (is.null(coords)) {
      stop('coords must be provided')
    }

    if (is.null(id)) {
      stop('id must be provided')
    }

    if (is.null(projection)) {
      stop('projection must be provided')
    }

    if (is.null(sortBy)) {
      stop('sortBy must be provided')
    }

    if (length(coords) != 2) {
      stop('coords requires a vector of column names for coordinates X and Y')
    }

    if (any(!(c(id, coords, splitBy, sortBy) %in% colnames(DT)))) {
      stop(paste0(
        as.character(paste(setdiff(
          c(id, coords, splitBy, sortBy), colnames(DT)
        ),
        collapse = ', ')),
        ' field(s) provided are not present in input DT'
      ))
    }

    if (any(!(DT[, vapply(.SD, is.numeric, TRUE), .SDcols = coords]))) {
      stop('coords must be numeric')
    }

    if (is.null(splitBy)) {
      splitBy <- id
    } else {
      splitBy <- c(id, splitBy)
    }
    if (any(!(DT[, lapply(.SD, FUN = function(x) {
        is.numeric(x) | is.character(x) | is.integer(x)
      }
    ), .SDcols = splitBy]))) {
      stop(
        strwrap(prefix = " ", initial = "",
          x = 'id (and splitBy when provided)
          must be character, numeric or integer type'
        )
      )
    }

    if (!('POSIXct' %in%
          unlist(lapply(DT[, .SD, .SDcols = c(sortBy)], class)))) {
      stop('sortBy provided must be 1 column of type POSIXct')
    }


    dropRows <- DT[, .(dropped = .N < 2), by = c(splitBy)]

    if (dropRows[(dropped), .N] > 0) {
      warning('some rows dropped, cannot build lines with less than two points')
    }

    wo_drop <- DT[dropRows, on = splitBy][!(dropped)]

    data.table::setorderv(wo_drop, sortBy)

    lines <- sf::st_as_sf(
      wo_drop[, .(geometry = sf::st_sfc(sf::st_linestring(as.matrix(.SD)))),
         by = c(splitBy), .SDcols = coords],
      crs = sf::st_crs(projection)
    )
  }
