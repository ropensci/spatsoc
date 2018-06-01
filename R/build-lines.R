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
           idField = NULL) {
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

    # less than 2 warning (not in loop)
    # TODO: one single warning with count of how many dropped
    # TODO: should this be flexible to time?

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

    # Find any ids with only one loc (rgeos requires at least 2 locs for a line)
    dropRows <- DT[, .(dropped = .N < 2), by = idField]

    # if(dropRows[(dropped), .N] > 0) {
    #   message('some rows dropped, cannot build lines with less than two points')}

    # Split up the data.table by collar ID into lists
    lst <-
      data.table:::split.data.table(DT[get(idField) %in% dropRows[!(dropped), get(idField)],
                                       ..coordFields],
                                    DT[get(idField) %in% dropRows[!(dropped), get(idField)],
                                       .(get(idField))])
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
