#' Group Polygons
#'
#' Group individuals by polygon (eg: home ranges) overlap
#'
#'
#' @inheritParams group_pts
#' @inheritParams group_lines
#' @param area boolean indicating either returning area and proportion of overlap or group
#' @param hrType type of HR estimation, of 'mcp' or 'kernel'
#' @param hrParams parameters for adehabitatHR functions, a named list passed to do.call
#' @param spPolys Alternatively, provide solely a SpatialPolygons object.
#'
#' @return If area is FALSE, a DT is returned with ID and spatialtemporal group. If area is TRUE, a DT is returned with ID and a proportional overlap. ID refers to the focal individual of which the total area is compared against the overlapping area of ID2.
#'
#'
#' @export
#'
#' @importFrom methods slot
#'
#' @examples
#' library(data.table)
#' DT <- fread(system.file("extdata", "DT.csv", package = "spatsoc"))
#'
#' utm <- '+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
#'
#' group_polys(DT, area = FALSE, 'mcp', list(percent = 95),
#'             projection = utm,
#'             id = 'ID', coords = c('X', 'Y'))
#'
#' areaDT <- group_polys(DT, area = TRUE, 'mcp', list(percent = 95),
#'                       projection = utm,
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
        if ('group' %in% colnames(DT)) {
          warning('group column will be overwritten by this function')
          set(DT, j = 'group', value = NULL)
        }
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
          area = vapply(inters@polygons, slot, 'area', FUN.VALUE = 1),
          IDs = vapply(inters@polygons, slot, 'ID', FUN.VALUE = "")
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

      if ('group' %in% colnames(DT)) {
        warning('group column will be overwritten by this function')
        set(DT, j = 'group', value = NULL)
      }

      DT[, nBy := .N, c(splitBy, id)]

      if (!area) {
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
