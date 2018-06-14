#' Build Home Ranges for Individuals
#'
#' @param hrType Type of HR estimation either 'mcp' or 'kernel'
#' @param hrParams List of named arguments to be passed to adehabitatHR functions as determined by hrType
#' @inheritParams BuildPts
#'
#' @return Home range polygons for each ID
#' @export
build_polys <- function(DT = NULL,
                     projection = NULL,
                     hrType = NULL,
                     hrParams = NULL,
                     coordFields = NULL,
                     idField = NULL,
                     groupFields = NULL,
                     spPts = NULL) {
  if (is.null(DT) && is.null(spPts)) {
    stop('input DT or spPts required')
  }

  if (!is.null(DT) && !is.null(spPts)) {
    stop('cannot provide both DT and spPts')
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


  if (is.null(hrParams)) {
    warning('hrParams is not provided, using defaults')
  }

  if (is.null(groupFields)) {
    groupFields <- idField
  } else {
    groupFields <- c(idField, groupFields)
  }

  if (any(!(DT[, lapply(
    .SD,
    FUN = function(x) {
      is.numeric(x) | is.character(x) | is.integer(x)
    }
  ), .SDcols = groupFields]))) {
    stop('idField (and groupFields when provided) must be character, numeric or integer type')
  }

  # DT[, bys := paste0(.BY, collapse = '-'), by = groupFields]

  if (is.null(spPts)) {
    spPts <- sp::SpatialPointsDataFrame(
      DT[, ..coordFields],
      proj4string = sp::CRS(projection),
      data = DT[, .(ID = do.call(paste,
                                 c(.SD, sep = '-'))),
                .SDcols = groupFields])
  }

  # set(DT, j = 'bys', value = NULL)

  hrParams$xy <- spPts

  if (hrType == 'mcp') {
    functionParams <- formals(adehabitatHR::mcp)
    if (all(names(hrParams) %in% names(functionParams))) {
      if (!('unout' %in% names(hrParams))) {
        hrParams$unout <- 'm2'
      }
      return(do.call(adehabitatHR::mcp, hrParams))
    } else {
      stop('hrParams provided do not match function parameters, see ?adehabitatHR::mcp')
    }
  } else if (hrType == 'kernel') {
    functionParams <- formals(adehabitatHR::kernelUD)
    if (all(names(hrParams) %in% names(functionParams))) {
      return(adehabitatHR::getverticeshr(
        do.call(adehabitatHR::kernelUD, hrParams)),
        unout = 'm2')
    } else {
      stop(
        'hrParams provided do not match function parameters, see ?adehabitatHR::kernelUD'
      )
    }
  }
}


