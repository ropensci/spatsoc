#' Build Polygons
#'
#' `build_polys` generates a simple feature collection with POLYGONs from a
#' `data.table`. The function accepts a `data.table` with
#' relocation data, individual identifiers, a projection,
#' home range type and parameters. The relocation
#' data is transformed into POLYGONs using either [adehabitatHR::mcp] or
#' [adehabitatHR::kernelUD] for each individual and, optionally,
#' combination of columns listed in `splitBy`. Relocation data should be in two
#' columns representing the X and Y coordinates.
#'
#' [group_polys] uses `build_polys` for grouping overlapping
#' polygons created from relocations.
#'
#' ## R-spatial evolution
#'
#' Please note, spatsoc has followed updates from R spatial, GDAL and PROJ for
#' handling projections, see more below and  details at
#' <https://r-spatial.org/r/2020/03/17/wkt.html>.
#'
#' In addition, `build_polys` previously used [sp::SpatialPoints] but has been
#' updated to use [sf::st_as_sf] according to the R-spatial evolution, see more
#' at <https://r-spatial.org/r/2022/04/12/evolution.html>.
#'
#' ## Notes on arguments
#'
#' The `DT` must be a `data.table`. If your data is a `data.frame`, you can
#' convert it by reference using [data.table::setDT].
#'
#' The `id`, `coords` (and optional `splitBy`) arguments
#' expect the names of respective columns in `DT` which correspond
#' to the individual identifier, X and Y coordinates, and additional
#' grouping columns.
#'
#' The `projection` argument expects a character string or numeric
#' defining the coordinate reference system to be passed to [sf::st_crs].
#' For example, for UTM zone 36S (EPSG 32736), the projection
#' argument is `projection = "EPSG:32736"` or `projection = 32736`.
#' See <https://spatialreference.org>
#' for a list of EPSG codes.
#'
#' The `hrType` must be either one of "kernel" or "mcp". The
#' `hrParams` must be a named list of arguments matching those
#' of [adehabitatHR::kernelUD] and [adehabitatHR::getverticeshr]
#' or [adehabitatHR::mcp].
#'
#' The `splitBy` argument offers further control building
#' POLYGONs. If in your `DT`, you have multiple
#' temporal groups (e.g.: years) for example, you can provide the
#' name of the column which identifies them and build POLYGONs
#' for each individual in each year.
#'
#'
#' @return `build_polys` returns a simple feature collection with POLYGONs
#' for each individual (and optionally `splitBy` combination).
#'
#' An error is returned when `hrParams` do not match the arguments
#' of the respective `hrType` `adehabitatHR` function.
#'
#'
#' @inheritParams group_polys
#' @param spPts alternatively, provide solely a SpatialPointsDataFrame with one
#' column representing the ID of each point, as specified by [adehabitatHR::mcp]
#' or [adehabitatHR::kernelUD]
#' @param projection numeric or character defining the coordinate reference
#'   system to be passed to [sf::st_crs]. For example, either
#'   `projection = "EPSG:32736"` or `projection = 32736`.
#' @export
#'
#' @family Build functions
#' @seealso [group_polys]
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
#' # Build polygons for each individual using kernelUD and getverticeshr
#' build_polys(DT, projection = utm, hrType = 'kernel',
#'             hrParams = list(grid = 60, percent = 95),
#'             id = 'ID', coords = c('X', 'Y'))
#'
#' # Build polygons for each individual by year
#' DT[, yr := year(datetime)]
#' build_polys(DT, projection = utm, hrType = 'mcp',
#'             hrParams = list(percent = 95),
#'             id = 'ID', coords = c('X', 'Y'), splitBy = 'yr')
build_polys <- function(DT = NULL,
                        projection = NULL,
                        hrType = NULL,
                        hrParams = NULL,
                        id = NULL,
                        coords = NULL,
                        splitBy = NULL,
                        spPts = NULL) {
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
    ), .SDcols = c(splitBy)]))) {
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
    ade_id <- DT[, do.call(function(...) paste(..., sep = '-'), .SD),
                           .SDcols = c(splitBy)]

    spPts <- sf::as_Spatial(
      sf::st_as_sf(
        DT[, cbind(.SD, ade_id), .SDcols = c(coords)],
        coords = coords,
        crs = sf::st_crs(projection)
      ),
      IDs = ade_id
    )
  }

  hrParams$xy <- spPts

  if (hrType == 'mcp') {
    functionParams <- formals(adehabitatHR::mcp)
    if (all(names(hrParams) %in% names(functionParams))) {
      if (!('unout' %in% names(hrParams))) {
        hrParams$unout <- 'm2'
      }
      out <- do.call(adehabitatHR::mcp, hrParams)
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
      out <- do.call(adehabitatHR::getverticeshr,
                     c(x = list(kern),
                       hrParams[intersect(names(hrParams),
                                          names(verticesParam))]))
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
  } else {
    stop('hrType not one of "kernel" or "mcp"')
  }

  out_sf <- sf::st_as_sf(out)
  colnames(out_sf) <- gsub('id', id, colnames(out_sf))
  return(out_sf)
}


