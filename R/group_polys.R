#' Group Overlapping Polygons
#'
#' Group individuals by polygon (eg: home ranges) overlap
#'
#' @inheritParams BuildPts
#' @inheritParams group_pts
#' @param area boolean indicating either returning area and proportion of overlap or group
#' @param hrType type of HR estimation, of 'mcp' or 'kernel'
#' @param hrParams parameters for adehabitatHR functions, a named list passed to do.call
#' @param spPolys Alternatively, provide solely a SpatialPolygons object.
#'
#' @return Group by ID (by time) data.table
#' @export
#'
#' @examples
#' groups <- group_polys(locs, area = FALSE, 'mcp', list(percent = 95),
#'                       projection = utm,
#'                       id = 'ID', coords = c('X', 'Y'))
#'
#' areaDT <- group_polys(locs, area = TRUE, 'mcp', list(percent = 95),
#'                       projection = utm,
#'                       id = 'ID', coords = c('X', 'Y'))
group_polys <-
  function(DT = NULL,
           area = NULL,
           hrType = NULL,
           hrParams = NULL,
           projection = NULL,
           coords = NULL,
           id = NULL,
           splitBy = NULL,
           spPolys = NULL) {
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
        inter <- rgeos::gIntersects(spPolys, spPolys, byid = TRUE)
        g <- igraph::graph_from_adjacency_matrix(inter)
        ovr <- igraph::clusters(g)$membership
        out <- data.table::data.table(names(ovr),
                                      unlist(ovr))
        data.table::setnames(out, c('ID', 'group'))
        return(out[])
      } else if (area) {
        if (!is.null(DT)) {
          if (any(DT[, grepl('[^A-z0-9]', .SD[[1]]), .SDcols = id])) {
            stop('please ensure IDs are alphanumeric and do not contain spaces')
          }
        }
        inters <-
          rgeos::gIntersection(spPolys, spPolys, byid = TRUE)
        out <- data.table::data.table(
          area = sapply(inters@polygons, slot, 'area'),
          IDs = sapply(inters@polygons, slot, 'ID')
        )

        set(out, j = 'ID1', value = tstrsplit(out[['IDs']], ' ', keep = 1))
        set(out, j = 'ID2', value = tstrsplit(out[['IDs']], ' ', keep = 2))

        out <- data.table:::merge.data.table(
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

      if ('group' %in% colnames(DT)) {
        warning('group column will be overwritten by this function')
        set(DT, j = 'group', value = NULL)
      }

      if (!area) {
        ovrDT <-
          DT[, {
            suppressWarnings(
              spPolys <-
                build_polys(
                  DT = .SD,
                  projection = projection,
                  hrType = hrType,
                  hrParams = hrParams,
                  coords = coords,
                  id = id,
                  splitBy = NULL,
                  spPts = NULL
                )
            )
            if (!is.null(spPolys)) {
              inter <- rgeos::gIntersects(spPolys, spPolys, byid = TRUE)
              g <- igraph::graph_from_adjacency_matrix(inter)
              ovr <- igraph::clusters(g)$membership
              out <- data.table::data.table(names(ovr),
                                            unlist(ovr))
              data.table::setnames(out, c('ID', 'withinGroup'))
            } else {
              data.table(ID = get(id),
                         withinGroup = as.integer(NA))
            }
          }, by = splitBy, .SDcols = c(coords, id)]
        DT[ovrDT, withinGroup := withinGroup, on = c(id, splitBy)]
        DT[, group := ifelse(is.na(withinGroup), as.integer(NA), .GRP),
           by = c(splitBy, 'withinGroup')]
        set(DT, j = 'withinGroup', value = NULL)
        return(DT[])
      } else if (area) {
        if (any(DT[, grepl('[^A-z0-9]', .SD[[1]]), .SDcols = id])) {
          stop('please ensure IDs are alphanumeric and do not contain spaces')
        }
        Dt[, nBy := .N, c(splitBy, id)]
        outDT <-
          Dt[nBy > 5, {
            suppressWarnings(
              spPolys <-
                build_polys(
                  DT = .SD,
                  projection = projection,
                  hrType = hrType,
                  hrParams = hrParams,
                  coords = coords,
                  id = id,
                  splitBy = NULL,
                  spPts = NULL
                )
            )
            inters <-
              rgeos::gIntersection(spPolys, spPolys, byid = TRUE)
            areaID <-
              data.table::data.table(area = sapply(inters@polygons, slot, 'area'),
                                     IDs = as.character(sapply(inters@polygons, slot, 'ID')))

            set(areaID, j = id, value = tstrsplit(areaID[['IDs']], ' ', keep = 1))
            set(areaID,
                j = paste0(id, '2'),
                value = tstrsplit(areaID[['IDs']], ' ', keep = 2))

            out <- data.table:::merge.data.table(
              x = data.table(spPolys@data),
              y = areaID,
              by.y = id,
              by.x = 'id',
              suffixes = c('Total', '')
            )
            set(out, j = 'proportion', value = out[['area']] / out[['areaTotal']])
            set(out, j = c('IDs', 'areaTotal'),  value = NULL)
            setnames(out, 'id', id)
            setcolorder(out, c(id, paste0(id, '2'), 'area', 'proportion'))
            out
          }, by = splitBy, .SDcols = c(coords, id)]

        dropped <-
          unique(DT[nBy <= 5, .SD, .SDcols = c(splitBy, id)])
        return(rbindlist(list(dropped, outDT), fill = TRUE))
      }
    }
  }
