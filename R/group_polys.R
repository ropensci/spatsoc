#' Group Polygons
#'
#' \code{group_polys} groups rows into spatial groups by overlapping polygons
#' (home ranges). The function accepts a \code{data.table} with relocation data,
#' individual identifiers and an \code{area} argument.  The relocation data is
#' transformed into home range \code{SpatialPolygons}. If the \code{area}
#' argument is \code{FALSE}, \code{group_polys} returns grouping calculated by
#' overlap. If the \code{area} argument is \code{TRUE}, the area and proportion
#' of overlap is calculated. Relocation data should be in two columns
#' representing the X and Y coordinates.
#'
#' The \code{DT} must be a \code{data.table}. If your data is a
#' \code{data.frame}, you can convert it by reference using
#' \code{\link[data.table:setDT]{data.table::setDT}}.
#'
#' The \code{id}, \code{coords} (and optional \code{splitBy}) arguments expect
#' the names of respective columns in \code{DT} which correspond to the
#' individual identifier, X and Y coordinates, and additional grouping columns.
#'
#' The \code{projection} argument expects a character string defining the EPSG
#' code. For example, for UTM zone 36N (EPSG 32736), the projection argument is
#'  'EPSG:32736'. See \url{https://spatialreference.org} for a list of EPSG
#'  codes. Please note, R spatial has followed updates to GDAL and PROJ for
#'  handling projections, see more at
#'  \url{https://www.r-spatial.org/r/2020/03/17/wkt.html}. It is likely
#' that \code{build_polys} will return "Warning in proj4string(xy) :
#' CRS object has comment, which is lost in output" due to these changes.
#'
#' The \code{hrType} must be either one of "kernel" or "mcp". The
#' \code{hrParams} must be a named list of arguments matching those of
#' \code{adehabitatHR::kernelUD} or \code{adehabitatHR::mcp}.
#'
#' The \code{splitBy} argument offers further control over grouping. If within
#' your \code{DT}, you have multiple populations, subgroups or other distinct
#' parts, you can provide the name of the column which identifies them to
#' \code{splitBy}. The grouping performed by \code{group_polys} will only
#' consider rows within each \code{splitBy} subgroup.
#'
#' @return When \code{area} is \code{FALSE}, \code{group_polys} returns the
#'   input \code{DT} appended with a \code{group} column. As with the other
#'   grouping functions,  the actual value of \code{group} is arbitrary and
#'   represents the identity of a given group where 1 or more individuals are
#'   assigned to a group. If the data was reordered, the \code{group} may
#'   change, but the contents of each group would not. When \code{area} is
#'   \code{TRUE}, \code{group_polys} returns a proportional area overlap
#'   \code{data.table}. In this case, ID refers to the focal individual of which
#'   the total area is compared against the overlapping area of ID2.
#'
#'   If \code{area} is \code{FALSE}, a message is returned when a column named
#'   \code{group} already exists in the input \code{DT}, because it will be
#'   overwritten.
#'
#' @inheritParams group_pts
#' @inheritParams group_lines
#' @inheritParams build_polys
#' @param area boolean indicating either overlap group (when \code{FALSE}) or
#'   area and proportion of overlap (when \code{TRUE})
#' @param hrType type of HR estimation, either 'mcp' or 'kernel'
#' @param hrParams a named list of parameters for `adehabitatHR` functions
#' @param spPolys Alternatively, provide solely a SpatialPolygons object
#'
#' @export
#'
#' @family Spatial grouping
#' @seealso \code{\link{build_polys}} \code{\link{group_times}}
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
#' group_polys(DT, area = FALSE, hrType = 'mcp',
#'             hrParams = list(percent = 95), projection = utm,
#'             id = 'ID', coords = c('X', 'Y'))
#'
#' areaDT <- group_polys(DT, area = TRUE, hrType = 'mcp',
#'                       hrParams = list(percent = 95), projection = utm,
#'                       id = 'ID', coords = c('X', 'Y'))
group_polys <-
  function(DT = NULL,
           area = NULL,
           hrType = NULL,
           hrParams = NULL,
           projection = NULL,
           id = NULL,
           coords = NULL,
           splitBy = NULL,
           spPolys = NULL) {
    # due to NSE notes in R CMD check
    nBy <- ..coords <- ..id <- withinGroup <- group <- outGroup <- NULL

    if (is.null(area) | !is.logical(area)) {
      stop('area must be provided (TRUE or FALSE)')
    }

    if (is.null(DT) && is.null(spPolys)) {
      stop('must provide either DT or spPolys')
    } else if (!is.null(DT) && !is.null(spPolys)) {
      stop('cannot provide both DT and spPolys')
    }

    if (is.null(splitBy)) {
      if (!is.null(DT) && is.null(spPolys)) {
        spPolys <-
          build_polys(
            DT = DT,
            projection = projection,
            hrType = hrType,
            hrParams = hrParams,
            coords = coords,
            id = id,
            splitBy = NULL,
            spPts = NULL
          )
      }


      if (!area) {
        if ('group' %in% colnames(DT)) {
          message('group column will be overwritten by this function')
          set(DT, j = 'group', value = NULL)
        }
        inter <- rgeos::gIntersects(spPolys, spPolys, byid = TRUE)
        g <- igraph::graph_from_adjacency_matrix(inter)
        ovr <- igraph::clusters(g)$membership
        out <- data.table::data.table(names(ovr),
                                      as.integer(unlist(ovr)))
        data.table::setnames(out, c(id, 'outGroup'))
        DT[out, group := outGroup, on = id]
        return(DT[])
      } else if (area) {
        if (!is.null(DT)) {
          if (any(DT[, grepl('[^A-z0-9]', .SD[[1]]), .SDcols = id])) {
            stop('please ensure IDs are alphanumeric and do not contain spaces')
          }
        }
        inters <-
          rgeos::gIntersection(spPolys, spPolys, byid = TRUE)
        out <- data.table::data.table(
          area = vapply(inters@polygons, methods::slot, 'area', FUN.VALUE = 1),
          IDs = vapply(inters@polygons, methods::slot, 'ID', FUN.VALUE = "")
        )

        set(out, j = 'ID1', value = tstrsplit(out[['IDs']], ' ', keep = 1))
        set(out, j = 'ID2', value = tstrsplit(out[['IDs']], ' ', keep = 2))

        out <- merge(
          out,
          data.table(spPolys@data),
          by.x = 'ID1',
          by.y = 'id',
          suffixes = c('', 'Total')
        )
        set(out, j = 'proportion', value = out[['area']] / out[['areaTotal']])
        set(out, j = c('IDs', 'areaTotal'),  value = NULL)
        data.table::setcolorder(out, c('ID1', 'ID2', 'area', 'proportion'))
        return(out[])
      }
    } else if (!is.null(splitBy)) {
      if (!is.null(spPolys)) {
        stop('cannot provide spPolys if providing splitBy')
      }

      if (any(!(c(id, splitBy) %in% colnames(DT)))) {
        stop(paste0(
          as.character(paste(setdiff(
            c(id, splitBy), colnames(DT)
          ),
          collapse = ', ')),
          ' field(s) provided are not present in input DT'
        ))
      }

      DT[, nBy := .N, c(splitBy, id)]

      if (!area) {
        if ('group' %in% colnames(DT)) {
          message('group column will be overwritten by this function')
          set(DT, j = 'group', value = NULL)
        }
        ovrDT <-
          DT[nBy > 5, {
            try(
              spPolys <-
                build_polys(
                  DT = .SD,
                  projection = projection,
                  hrType = hrType,
                  hrParams = hrParams,
                  coords = ..coords,
                  id = ..id,
                  splitBy = NULL,
                  spPts = NULL
                ),
              silent = TRUE
            )
            if (!is.null(spPolys)) {
              inter <- rgeos::gIntersects(spPolys, spPolys, byid = TRUE)
              g <- igraph::graph_from_adjacency_matrix(inter)
              ovr <- igraph::clusters(g)$membership
              out <- data.table::data.table(names(ovr),
                                            unlist(ovr))
              data.table::setnames(out, c(..id, 'withinGroup'))
            } else {
              data.table(ID = get(..id),
                         withinGroup = as.integer(NA))
            }
          }, by = splitBy, .SDcols = c(coords, id)]
        DT[ovrDT, withinGroup := withinGroup, on = c(id, splitBy)]
        DT[, group := ifelse(is.na(withinGroup), as.integer(NA), .GRP),
           by = c(splitBy, 'withinGroup')]
        set(DT, j = c('withinGroup', 'nBy'), value = NULL)
        if (DT[is.na(group), .N] > 0) {
          warning(
            strwrap(
              prefix = " ",
              initial = "",
              x = 'build_polys failed for some rows,
              check `DT[is.na(group)]` and choice of hrParams'
            )
          )
        }
        return(DT[])
      } else if (area) {
        if (any(DT[, grepl('[^A-z0-9]', .SD[[1]]), .SDcols = id])) {
          stop('please ensure IDs are alphanumeric and do not contain spaces')
        }
        outDT <-
          DT[nBy > 5, {
            try(
              spPolys <-
                build_polys(
                  DT = .SD,
                  projection = projection,
                  hrType = hrType,
                  hrParams = hrParams,
                  id = ..id,
                  coords = ..coords,
                  splitBy = NULL,
                  spPts = NULL
                ),
              silent = TRUE
            )
            if (!is.null(spPolys)) {
              inters <- rgeos::gIntersection(spPolys, spPolys, byid = TRUE)
              areas <- rgeos::gArea(inters, byid = TRUE)
              areaID <-
                data.table::data.table(
                  area = areas,
                  IDs = names(areas))

              set(areaID,
                  j = ..id,
                  value = tstrsplit(areaID[['IDs']], ' ', keep = 1))

              set(areaID,
                  j = paste0(..id, '2'),
                  value = tstrsplit(areaID[['IDs']], ' ', keep = 2))

              out <- merge(
                x = data.table(spPolys@data),
                y = areaID,
                by.x = 'id',
                by.y = ..id,
                suffixes = c('Total', '')
              )
              set(out, j = 'proportion',
                  value = out[['area']] / out[['areaTotal']])
              set(out, j = c('IDs', 'areaTotal'),  value = NULL)
              setnames(out, 'id', ..id)
              setcolorder(out, c(..id, paste0(..id, '2'),
                                 'area', 'proportion'))
              out
            } else {
              out <- data.table(ID = get(..id),
                                ID2 = as.character(NA),
                                as.numeric(NA),
                                as.numeric(NA))
              setnames(out, c(..id, paste0(..id, '2'),
                              'area', 'proportion'))
              out
            }
          }, by = splitBy, .SDcols = c(coords, id)]
        dropped <-
          unique(DT[nBy <= 5, .SD, .SDcols = c(splitBy, id)])
        out <- rbindlist(list(dropped, outDT), fill = TRUE)
        if (out[is.na(area), .N] > 0) {
          warning(
            strwrap(
              prefix = " ",
              initial = "",
              x = 'build_polys failed for some rows,
              check `DT[is.na(group)]` and choice of hrParams'
            )
          )
        }
        return(out)
      }
    }
  }
