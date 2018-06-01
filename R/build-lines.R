#' Build Lines
#'
#' @inheritParams BuildPts
#'
#' @return SpatialLines for each ID provided
#' @export
#'
#' @import data.table
BuildLines <-
  function(DT = NULL,
           projection = NULL,
           coordFields = NULL,
           idField = NULL,
           byFields = NULL) {
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

    if (any(!(c(idField, coordFields) %in% colnames(DT)))) {
      stop(paste0(
        as.character(paste(setdiff(
          c(idField, coordFields), colnames(DT)
        ),
        collapse = ', ')),
        ' field(s) provided are not present in input DT'
      ))
    }

    if (any(!(DT[, vapply(.SD, is.numeric, TRUE), .SDcols = coordFields]))) {
      stop('coordFields must be numeric')
    }

    if (!is.null(byFields)) {

      byFields <- c(idField, byFields)
    } else {
      byFields <- idField
    }

    if (sum(c('character', 'numeric', 'integer') %in%
            unlist(lapply(DT[, .SD, .SDcols = c('ID', 'jul')], class))) == 2) {
      stop('idField and/or byFields provided must be
           character, numeric or integer type')
    }

    dropRows <- DT[, .(dropped = .N < 2), by = byFields]

    if(dropRows[(dropped), .N] > 0) {
      warning('some rows dropped, cannot build lines with less than two points')}

    lst <- data.table:::split.data.table(DT[dropRows, on = byFields][!(dropped)],
                                  by = byFields)

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
      do.call(sp::rbind.SpatialLines, l)
    }
  }
