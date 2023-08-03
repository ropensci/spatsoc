#' Build Polygons (deprecated version of function with retired spatial packages)
#'
#' @inheritParams build_polys
#' @export
#'
#' @family Build functions
#' @seealso \code{\link{group_polys}}
build_polys_sp <- function(DT = NULL,
                        projection = NULL,
                        hrType = NULL,
                        hrParams = NULL,
                        id = NULL,
                        coords = NULL,
                        splitBy = NULL,
                        spPts = NULL) {
  .Deprecated(msg = 'build_polys has been updated to use modern spatial R packages, removing dependencies on rgdal, rgeos, maptools in favor of sf. This version will be preserved until September 2023 for testing and user transition.')

  # due to NSE notes in R CMD check
  . <- NULL

  if (is.null(DT) && is.null(spPts)) {
    stop('input DT or spPts required')
  }

  if (!is.null(DT) && !is.null(spPts)) {
    stop('cannot provide both DT and spPts')
  }

  if (!is.null(DT) && is.null(spPts)) {
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

    if (is.null(splitBy)) {
      splitBy <- id
    } else {
      splitBy <- c(id, splitBy)
    }

    if (any(!(c(splitBy, coords) %in% colnames(DT)))) {
      stop(paste0(
        as.character(paste(setdiff(
          c(id, coords), colnames(DT)
        ),
        collapse = ', ')),
        ' field(s) provided are not present in input DT'
      ))
    }

    if (any(!(DT[, vapply(.SD, is.numeric, TRUE),
                 .SDcols = coords]))) {
      stop('coords must be numeric')
    }

    if (any(!(DT[, lapply(
      .SD,
      FUN = function(x) {
        is.numeric(x) | is.character(x) | is.integer(x)
      }
    ), .SDcols = splitBy]))) {
      stop(
        strwrap(
          prefix = " ",
          initial = "",
          x = 'id (and splitBy when provided)
          must be character, numeric or integer type'
        )
      )
    }

  }


  if (is.null(hrType)) {
    stop('hrType must be provided')
  }

  if (is.null(hrParams)) {
    message('hrParams is not provided, using defaults')
  }

  if (is.null(spPts)) {
    spPts <- sp::SpatialPointsDataFrame(
      DT[, .SD, .SDcols = eval.parent(coords, n = 1)],
      proj4string = sp::CRS(projection),
      data = DT[, .(ID = do.call(paste,
                                 c(.SD, sep = '-'))),
                .SDcols = splitBy])
  }

  hrParams$xy <- spPts

  if (hrType == 'mcp') {
    functionParams <- formals(adehabitatHR::mcp)
    if (all(names(hrParams) %in% names(functionParams))) {
      if (!('unout' %in% names(hrParams))) {
        hrParams$unout <- 'm2'
      }
      return(do.call(adehabitatHR::mcp, hrParams))
    } else {
      stop(
        strwrap(
          prefix = " ",
          initial = "",
          x = 'hrParams provided do not match function parameters,
               see ?adehabitatHR::mcp'
        )
      )
    }
  } else if (hrType == 'kernel') {
    kernelParam <- formals(adehabitatHR::kernelUD)
    verticesParam <- formals(adehabitatHR::getverticeshr.estUD)

    if (all(names(hrParams) %in% c(names(kernelParam), names(verticesParam)))) {
      if (!('unout' %in% names(hrParams))) {
        hrParams$unout <- 'm2'
      }
      kern <- do.call(adehabitatHR::kernelUD,
                      hrParams[intersect(names(hrParams), names(kernelParam))])
      return(do.call(adehabitatHR::getverticeshr,
                     c(x = list(kern),
                       hrParams[intersect(names(hrParams),
                                          names(verticesParam))])))
    } else {
      stop(
        strwrap(
          prefix = " ",
          initial = "",
          x = 'hrParams provided do not match
          function parameters, see ?adehabitatHR::kernelUD
          and ?adehabitatHR::getverticeshr'
        )
      )
    }
  }
}


