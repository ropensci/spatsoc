#' Group polygons
#'
#' `group_polys` groups rows into spatial groups by overlapping polygons (home
#' ranges). The function expects a `data.table` with relocation data, individual
#' identifiers and an `area` argument.  The relocation data is transformed into
#' home range POLYGONs using [build_polys()] with [adehabitatHR::mcp] or
#' [adehabitatHR::kernelUD]. If the `area` argument is `FALSE`, `group_polys`
#' returns grouping calculated by spatial overlap. If the `area` argument is
#' `TRUE`, `group_polys` returns the area area and proportion of overlap.
#' Relocation data should be in two columns representing the X and Y
#' coordinates.
#'
#' ## R-spatial evolution
#'
#' Please note, spatsoc has followed updates from R spatial, GDAL and PROJ for
#' handling coordinate reference systems, see more below and  details at
#' <https://r-spatial.org/r/2020/03/17/wkt.html>.
#'
#' In addition, `group_polys` previously used rgeos::gIntersection,
#' rgeos::gIntersects and rgeos::gArea but has been
#' updated to use [sf::st_intersects], [sf::st_intersection] and [sf::st_area]
#' according to the R-spatial evolution, see more
#' at <https://r-spatial.org/r/2022/04/12/evolution.html>.
#'
#' ## Notes on arguments
#'
#' The `DT` must be a `data.table`. If your data is a
#' `data.frame`, you can convert it by reference using
#' [data.table::setDT()].
#'
#' The `id`, `coords` (and optional `splitBy`) arguments expect
#' the names of respective columns in `DT` which correspond to the
#' individual identifier, X and Y coordinates, and additional grouping columns.
#'
#' The `crs` argument expects a character string or numeric defining the
#' coordinate reference system to be passed to [sf::st_crs]. For example, for
#' UTM zone 36S (EPSG 32736), the crs argument is `crs = "EPSG:32736"` or `crs =
#' 32736`. See <https://spatialreference.org> for a list of EPSG codes.
#'
#' The `hrType` must be either one of "kernel" or "mcp". The
#' `hrParams` must be a named list of arguments matching those of
#' [adehabitatHR::kernelUD()] or [adehabitatHR::mcp()].
#'
#' The `splitBy` argument offers further control over grouping. If within
#' your `DT`, you have multiple populations, subgroups or other distinct
#' parts, you can provide the name of the column which identifies them to
#' `splitBy`. The grouping performed by `group_polys` will only
#' consider rows within each `splitBy` subgroup.
#'
#' @return When `area` is `FALSE`, `group_polys` returns the input `DT` appended
#'   with a `group` column. As with the other grouping functions,  the actual
#'   value of `group` is arbitrary and represents the identity of a given group
#'   where 1 or more individuals are assigned to a group. If the data was
#'   reordered, the `group` may change, but the contents of each group would
#'   not. When `area` is `TRUE`, `group_polys` returns a proportional area
#'   overlap `data.table`. In this case, ID refers to the focal individual of
#'   which the total area is compared against the overlapping area of ID2.
#'
#'   If `area` is `FALSE`, a message is returned when a column named `group`
#'   already exists in the input `DT`, because it will be overwritten.
#'
#'   Along with changes to follow the R-spatial evolution, `group_polys` also
#'   now returns area and proportion of overlap with units explicitly specified
#'   through the `units` package.
#'
#'  Note: if area is TRUE, the output of `group_polys` needs to be
#'  reassigned. See details in
#'  [FAQ](https://docs.ropensci.org/spatsoc/articles/faq.html).
#'
#'
#' @inheritParams group_pts
#' @inheritParams group_lines
#' @inheritParams build_polys
#' @param area logical indicating either overlap group (when `FALSE`) or
#'   area and proportion of overlap (when `TRUE`)
#' @param hrType type of HR estimation, either 'mcp' or 'kernel'
#' @param hrParams a named list of parameters for `adehabitatHR` functions
#' @param sfPolys Alternatively, provide solely a simple features object with
#' POLYGONs or MULTIPOLYGONs. If sfPolys are provided, id is required and
#' splitBy cannot be used.
#'
#' @export
#'
#' @family Spatial grouping
#' @seealso [build_polys()] [group_times()]
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
#' group_polys(DT, area = FALSE, hrType = 'mcp',
#'             hrParams = list(percent = 95), crs = utm,
#'             id = 'ID', coords = c('X', 'Y'))
#'
#' areaDT <- group_polys(DT, area = TRUE, hrType = 'mcp',
#'                       hrParams = list(percent = 95), crs = utm,
#'                       id = 'ID', coords = c('X', 'Y'))
#' print(areaDT)
group_polys <- function(
    DT = NULL,
    area = NULL,
    hrType = NULL,
    hrParams = NULL,
    crs = NULL,
    id = NULL,
    coords = NULL,
    splitBy = NULL,
    sfPolys = NULL,
    projection = NULL) {
  # due to NSE notes in R CMD check
  nBy <- ..coords <- ..id <- withinGroup <- group <- outGroup <- NULL

  if (!is.null(projection)) {
    warning('projection argument is deprecated, setting crs = projection')
    crs <- projection
  }

  assert_not_null(area)
  assert_inherits(area, 'logical')

  if (is.null(DT) && is.null(sfPolys)) {
    stop('must provide either DT or sfPolys')
  } else if (!is.null(DT) && !is.null(sfPolys)) {
    stop('cannot provide both DT and sfPolys')
  }

  if (is.null(splitBy)) {
    if (is.null(DT) && !is.null(sfPolys)) {
      input <- 'sfPolys'
    } else if (!is.null(DT) && is.null(sfPolys)) {
      sfPolys <-
        build_polys(
          DT = DT,
          crs = crs,
          hrType = hrType,
          hrParams = hrParams,
          coords = coords,
          id = id,
          splitBy = NULL,
          spPts = NULL
        )
      input <- 'DT'
    }

    assert_not_null(id)

    if (!area) {
      if ('group' %in% colnames(DT)) {
        message('group column will be overwritten by this function')
        data.table::set(DT, j = 'group', value = NULL)
      }
      inter <- sf::st_intersects(sfPolys, sfPolys, sparse = FALSE)
      dimnames(inter) <- list(sfPolys[[id]], sfPolys[[id]])
      g <- igraph::graph_from_adjacency_matrix(inter)
      ovr <- igraph::components(g)$membership
      out <- data.table::data.table(names(ovr),
                                    as.integer(unlist(ovr)))
      data.table::setnames(out, c(id, 'group'))
      if (input == 'DT') {
        DT[out, group := group, on = c(id)]
        return(DT)
      } else if (input == 'sfPolys'){
        return(out)
      }
    } else if (area) {
      if (any(grepl(' ', sfPolys[[id]]))) {
        stop('please ensure IDs do not contain spaces')
      }
      if (! 'area' %in% colnames(sfPolys)) {
        stop('please ensure column "area" present in input DT or sfPolys')
      }
      sf::st_agr(sfPolys) <- 'constant'
      inter <- sf::st_intersection(sfPolys, sfPolys)
      areas <- sf::st_area(inter)
      out_inter <- data.table::data.table(
        ID1 = inter[[id]],
        ID2 = inter[[paste0(id, '.1')]],
        area = areas,
        area_ID1 = units::as_units(inter[['area']], units(areas),
                                   set_units_mode = 'standard')
      )
      data.table::set(out_inter, j = 'proportion',
          value = units::set_units(
            out_inter[['area']] / out_inter[['area_ID1']],
            'percent')
      )
      data.table::set(out_inter, j = 'area_ID1',  value = NULL)

      disjointed <- data.frame(sf::st_disjoint(sfPolys))
      out_disjointed <- data.frame(
        ID1 = sfPolys[[id]][disjointed$row.id],
        ID2 = sfPolys[[id]][disjointed$col.id],
        area = rep(units::as_units(0, units(out_inter$area)),
                   nrow(disjointed)),
        proportion = rep(units::set_units(0, 'percent'), nrow(disjointed))
      )
      out <- rbind(out_inter, out_disjointed)

      data.table::setcolorder(out, c('ID1', 'ID2', 'area', 'proportion'))
      return(out[])
    }
  } else if (!is.null(splitBy)) {
    if (!is.null(sfPolys)) {
      stop('cannot provide sfPolys if providing splitBy')
    }
    assert_are_colnames(DT, c(id, splitBy))

    DT[, nBy := .N, c(splitBy, id)]

    if (!area) {
      if ('group' %in% colnames(DT)) {
        message('group column will be overwritten by this function')
        data.table::set(DT, j = 'group', value = NULL)
      }
      ovrDT <-
        DT[nBy > 5, {
          try(
            sfPolys <-
              build_polys(
                DT = .SD,
                crs = crs,
                hrType = hrType,
                hrParams = hrParams,
                coords = ..coords,
                id = ..id,
                splitBy = NULL,
                spPts = NULL
              ),
            silent = TRUE
          )
          if (!is.null(sfPolys)) {
            inter <- sf::st_intersects(sfPolys, sfPolys, sparse = FALSE)
            dimnames(inter) <- list(sfPolys[[..id]], sfPolys[[..id]])
            g <- igraph::graph_from_adjacency_matrix(inter)
            ovr <- igraph::components(g)$membership
            out <- data.table::data.table(names(ovr),
                                          as.integer(unlist(ovr)))
            data.table::setnames(out, c(id, 'withinGroup'))
            out
          } else {
            data.table(ID = get(..id),
                       withinGroup = NA_integer_)
          }
        }, by = c(splitBy), .SDcols = c(coords, id)]
      DT[ovrDT, withinGroup := withinGroup, on = c(id, splitBy)]
      DT[, group := ifelse(is.na(withinGroup), NA_integer_, .GRP),
         by = c(splitBy, 'withinGroup')]
      data.table::set(DT, j = c('withinGroup', 'nBy'), value = NULL)
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
      if (any(DT[, grepl(' ', .SD[[1]]), .SDcols = id])) {
        stop('please ensure IDs do not contain spaces')
      }
      outDT <-
        DT[nBy > 5, {
          try(
            sfPolys <-
              build_polys(
                DT = .SD,
                crs = crs,
                hrType = hrType,
                hrParams = hrParams,
                id = ..id,
                coords = ..coords,
                splitBy = NULL,
                spPts = NULL
              ),
            silent = TRUE
          )
          if (!is.null(sfPolys)) {
            sf::st_agr(sfPolys) <- 'constant'
            inter <- sf::st_intersection(sfPolys, sfPolys)
            areas <- sf::st_area(inter)
            out_inter <- data.table::data.table(
              ID1 = inter[[id]],
              ID2 = inter[[paste0(id, '.1')]],
              area = areas,
              area_ID1 = units::as_units(inter[['area']], units(areas),
                                         set_units_mode = 'standard')
            )
            data.table::set(out_inter, j = 'proportion',
                value = units::set_units(
                  out_inter[['area']] / out_inter[['area_ID1']],
                  'percent')
            )
            data.table::set(out_inter, j = 'area_ID1',  value = NULL)
            data.table::set(out_inter, j = 'proportion',
                i = which(unclass(out_inter$proportion) > 100),
                value = units::set_units(100, 'percent'))

            disjointed <- data.frame(sf::st_disjoint(sfPolys))
            out_disjointed <- data.frame(
              ID1 = sfPolys[[id]][disjointed$row.id],
              ID2 = sfPolys[[id]][disjointed$col.id],
              area = rep(
                units::as_units(0, units(out_inter$area)),
                nrow(disjointed)
              ),
              proportion = rep(
                units::set_units(0, 'percent'),
                nrow(disjointed)
              )
            )
            out <- rbind(out_inter, out_disjointed)

            data.table::setcolorder(out,
                                    c('ID1', 'ID2', 'area', 'proportion'))
            out
          } else {
            out <- data.table(ID = get(..id),
                              ID2 = NA_character_,
                              NA_real_,
                              NA_real_)
            data.table::setnames(out, c(..id, paste0(..id, '2'),
                            'area', 'proportion'))
            out
          }
        }, by = c(splitBy), .SDcols = c(coords, id)]
      dropped <-
        unique(DT[nBy <= 5, .SD, .SDcols = c(splitBy, id)])
      out <- data.table::rbindlist(list(dropped, outDT), fill = TRUE)
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
