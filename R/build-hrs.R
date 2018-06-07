#' Build Home Ranges for Individuals
#'
#' @param hrType Type of HR estimation either 'mcp' or 'kernel'
#' @param hrParams List of named arguments to be passed to adehabitatHR functions as determined by hrType
#' @inheritParams BuildPts
#'
#' @return Home range polygons for each ID
#' @export
BuildHRs <- function(DT = NULL,
                     projection = NULL,
                     hrType = NULL,
                     hrParams = NULL,
                     coordFields = NULL,
                     idField = NULL,
                     byFields = NULL,
                     spPts = NULL) {
  if (is.null(DT) && is.null(spPts)) {
    stop('input DT or spPts required')
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

  if (is.null(hrType)) {
    stop('hrType must be provided')
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


  if (is.null(byFields)) {
    byFields <- idField
  } else {
    byFields <- c(idField, byFields)
  }

  if (any(!(DT[, lapply(.SD, FUN = function(x) {
    is.numeric(x) | is.character(x) | is.integer(x)
  }
  ), .SDcols = byFields]))) {
    stop('idField (and byFields when provided) must be character, numeric or integer type')
  }

  DT[, bys := paste0(.BY, collapse = '-'), by = byFields]
  if (is.null(spPts)) {
    spPts <- sp::SpatialPointsDataFrame(DT[, ..coordFields],
                                        proj4string = sp::CRS(projection),
                                        data = DT[, .(id = bys)])
  }
  set(DT, j = 'bys', value = NULL)
  hrParams$xy <- spPts
  if (hrType == 'mcp') {
    return(do.call(adehabitatHR::mcp, hrParams))
  } else if (hrType == 'kernel') {
    return(adehabitatHR::getverticeshr(do.call(adehabitatHR::kernelUD, hrParams)))
  }
}


