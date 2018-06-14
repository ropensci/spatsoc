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
           idField = NULL,
           coordFields = NULL,
           groupFields = NULL) {
    if (is.null(DT)) {
      stop('input DT required')
    }

    if (is.null(coordFields)) {
      stop('coordFields must be provided')
    }

    if (is.null(idField)) {
      stop('idField must be provided')
    }

    if (is.null(projection)) {
      stop('projection must be provided')
    }

    if (length(coordFields) != 2) {
      stop('coordFields requires a vector of column names for coordinates X and Y')
    }

    if (any(!(c(idField, coordFields, groupFields) %in% colnames(DT)))) {
      stop(paste0(
        as.character(paste(setdiff(
          c(idField, coordFields, groupFields), colnames(DT)
        ),
        collapse = ', ')),
        ' field(s) provided are not present in input DT'
      ))
    }

    if (any(!(DT[, vapply(.SD, is.numeric, TRUE), .SDcols = coordFields]))) {
      stop('coordFields must be numeric')
    }

    if (is.null(groupFields)) {
      groupFields <- idField
    } else {
      groupFields <- c(idField, groupFields)
    }
    if (any(!(DT[, lapply(.SD, FUN = function(x) {
        is.numeric(x) | is.character(x) | is.integer(x)
      }
    ), .SDcols = groupFields]))) {
      stop('idField (and groupFields when provided) must be character, numeric or integer type')
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
          cbind(lst[[i]][[coordFields[1]]],
                lst[[i]][[coordFields[2]]])
        ),
        names(lst)[[i]])),
        proj4string = sp::CRS(projection))
      })
      return(do.call(sp::rbind.SpatialLines, l))
    }
  }
