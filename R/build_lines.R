#' Build Lines
#'
#' @inheritParams BuildPts
#'
#' @return SpatialLines for each ID provided
#' @export
#'
#' @import data.table
build_lines <-
  function(DT = NULL,
           projection = NULL,
           id = NULL,
           coords = NULL,
           groupFields = NULL) {
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

    if (length(coords) != 2) {
      stop('coords requires a vector of column names for coordinates X and Y')
    }

    if (any(!(c(id, coords, groupFields) %in% colnames(DT)))) {
      stop(paste0(
        as.character(paste(setdiff(
          c(id, coords, groupFields), colnames(DT)
        ),
        collapse = ', ')),
        ' field(s) provided are not present in input DT'
      ))
    }

    if (any(!(DT[, vapply(.SD, is.numeric, TRUE), .SDcols = coords]))) {
      stop('coords must be numeric')
    }

    if (is.null(groupFields)) {
      groupFields <- id
    } else {
      groupFields <- c(id, groupFields)
    }
    if (any(!(DT[, lapply(.SD, FUN = function(x) {
        is.numeric(x) | is.character(x) | is.integer(x)
      }
    ), .SDcols = groupFields]))) {
      stop('id (and groupFields when provided) must be character, numeric or integer type')
    }

    dropRows <- DT[, .(dropped = .N < 2), by = groupFields]

    if (dropRows[(dropped), .N] > 0) {
      warning('some rows dropped, cannot build lines with less than two points')
    }

    lst <- data.table:::split.data.table(DT[dropRows, on = groupFields][!(dropped)],
                                  by = groupFields)

    if (length(lst) == 0) {
      return(NULL)
    } else {
      l <- lapply(seq_along(lst), function(i) {
        sp::SpatialLines(list(sp::Lines(sp::Line(
          cbind(lst[[i]][[coords[1]]],
                lst[[i]][[coords[2]]])
        ),
        names(lst)[[i]])),
        proj4string = sp::CRS(projection))
      })
      return(do.call(sp::rbind.SpatialLines, l))
    }
  }
