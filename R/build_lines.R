#' Build Lines
#'
#'
#' \code{build_lines} creates a \code{SpatialLines} object from a \code{data.table}.
#' The function accepts a \code{data.table} with relocation data, individual
#' identifiers a sorting column and a \code{projection}. The relocation data
#' is transformed into \code{SpatialLines} for each individual and optionally,
#' each \code{splitBy}. Relocation data should be in two columns representing
#' the X and Y coordinates.
#'
#' The \code{projection} argument expects a character string defining the EPSG
#' code. For example, for UTM zone 36N (EPSG 32736), the projection argument is
#'  'EPSG:32736'. See \url{https://spatialreference.org} for a list of
#'  EPSG codes. Please note, R spatial has followed updates to GDAL and PROJ
#'  for handling projections, see more at
#'  \url{https://www.r-spatial.org/r/2020/03/17/wkt.html}.
#'
#' The \code{sortBy} is used to order the input \code{data.table} when creating
#' \code{SpatialLines}. It must a \code{POSIXct} to ensure the rows are sorted
#' by date time.
#'
#' The \code{splitBy} argument offers further control building \code{SpatialLines}.
#' If in your \code{DT}, you have multiple temporal groups (e.g.: years) for
#' example, you can provide the name of the column which identifies them and
#' build \code{SpatialLines} for each individual in each year.
#'
#' \code{build_lines} is used by \code{group_lines} for grouping overlapping
#' lines created from relocations.
#'
#' @return \code{build_lines} returns a \code{SpatialLines} object with a line
#' for each individual (and optionally \code{splitBy} combination).
#'
#' An error is returned when an individual has less than 2 relocations, making
#' it impossible to build a line.
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
#'
#' # Read example data
#' DT <- fread(system.file("extdata", "DT.csv", package = "spatsoc"))
#'
#' # Cast the character column to POSIXct
#' DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
#'
#' # EPSG code for example data
#' utm <- 'EPSG:32736'
#'
#' # Build lines for each individual
#' lines <- build_lines(DT, projection = utm, id = 'ID', coords = c('X', 'Y'),
#'             sortBy = 'datetime')
#'
#' # Build lines for each individual by year
#' DT[, yr := year(datetime)]
#' lines <- build_lines(DT, projection = utm, id = 'ID', coords = c('X', 'Y'),
#'             sortBy = 'datetime', splitBy = 'yr')
#'
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
          unlist(lapply(DT[, .SD, .SDcols = sortBy], class)))) {
      stop('sortBy provided must be 1 column of type POSIXct')
    }


    dropRows <- DT[, .(dropped = .N < 2), by = splitBy]

    if (dropRows[(dropped), .N] > 0) {
      warning('some rows dropped, cannot build lines with less than two points')
    }

    lst <- split(DT[dropRows, on = splitBy][!(dropped)][order(get(sortBy))],
                 by = splitBy, sorted = TRUE)

    if (length(lst) == 0) {
      return(NULL)
    } else {
      proj4string <- sp::CRS(projection)
      l <- lapply(seq_along(lst), function(i) {
        sp::SpatialLines(list(sp::Lines(sp::Line(
          cbind(lst[[i]][[coords[1]]],
                lst[[i]][[coords[2]]])
        ),
        names(lst)[[i]])),
        proj4string = proj4string)
      })
      return(do.call(sp::rbind.SpatialLines, l))
    }
  }
