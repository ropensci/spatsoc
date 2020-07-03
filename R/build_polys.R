#' Build Polygons
#'
#' \code{build_polys} creates a \code{SpatialPolygons} object from a \code{data.table}. The function accepts a \code{data.table} with relocation data, individual identifiers, a \code{projection}, \code{hrType} and \code{hrParams}. The relocation data is transformed into \code{SpatialPolygons} for each individual and optionally, each \code{splitBy}. Relocation data should be in two columns representing the X and Y coordinates.
#'
#' The \code{DT} must be a \code{data.table}. If your data is a \code{data.frame}, you can convert it by reference using \code{\link[data.table:setDT]{data.table::setDT}}.
#'
#' The \code{id}, \code{coords} (and optional \code{splitBy}) arguments expect the names of respective columns in \code{DT} which correspond to the individual identifier, X and Y coordinates, and additional grouping columns.
#'
#' The \code{projection} argument expects a character string defining the EPSG code. For example, for UTM zone 21N (EPSG 32736), the projection argument is "+init=epsg:32736". See \url{https://spatialreference.org} for a list of EPSG codes. Please note, R spatial has followed updates to GDAL and PROJ for handling projections, see more at \url{https://www.r-spatial.org/r/2020/03/17/wkt.html}.
#'
#' The \code{hrType} must be either one of "kernel" or "mcp". The \code{hrParams} must be a named list of arguments matching those of \code{adehabitatHR::kernelUD} and \code{adehabitatHR::getverticeshr} or \code{adehabitatHR::mcp}.
#'
#' The \code{splitBy} argument offers further control building \code{SpatialPolygons}. If in your \code{DT}, you have multiple temporal groups (e.g.: years) for example, you can provide the name of the column which identifies them and build \code{SpatialPolygons} for each individual in each year.
#'
#' \code{group_polys} uses \code{build_polys} for grouping overlapping polygons created from relocations.
#'
#' @return \code{build_polys} returns a \code{SpatialPolygons} object with a polyon for each individual (and optionally \code{splitBy} combination).
#'
#' An error is returned when \code{hrParams} do not match the arguments of the \code{hrType} \code{adehabitatHR} function.
#'
#'
#' @inheritParams group_polys
#' @param spPts alternatively, provide solely a SpatialPointsDataFrame with one column representing the ID of each point.
#' @param projection character string defining the EPSG code. For example, for UTM zone 21N (EPSG 32736), the projection argument is "+init=epsg:32736". See details.
#' @export
#'
#' @family Build functions
#' @seealso \code{\link{group_polys}}
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
#' utm <- '+init=epsg:32736'
#'
#' # Build polygons for each individual using kernelUD and getverticeshr
#' build_polys(DT, projection = utm, hrType = 'kernel',
#'             hrParams = list(grid = 60, percent = 95),
#'             id = 'ID', coords = c('X', 'Y'))
#'
#' # Build polygons for each individual by year
#' DT[, yr := year(datetime)]
#' build_polys(DT, projection = utm, hrType = 'mcp', hrParams = list(percent = 95),
#'             id = 'ID', coords = c('X', 'Y'), splitBy = 'yr')
#'
#' # Build polygons from SpatialPointsDataFrame
#' library(sp)
#' pts <- SpatialPointsDataFrame(coords = DT[, .(X, Y)],
#'                               proj4string = CRS(utm),
#'                               data = DT[, .(ID)]
#' )
#'
#' build_polys(spPts = pts, hrType = 'mcp', hrParams = list(percent = 95))
#'
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
    verticesParam <- formals(adehabitatHR::getverticeshr)

    if (all(names(hrParams) %in% c(names(kernelParam), names(verticesParam)))) {

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


