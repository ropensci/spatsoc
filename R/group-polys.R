#' Group Overlapping Polygons
#'
#' Group individuals by polygon (eg: home ranges) overlap
#'
#' @inheritParams BuildPts
#' @inheritParams GroupPts
#' @param area boolean indicating either returning area of overlap
#'                   of polygons or simply group
#' @param hrType Type of HR estimation, defaults to 'mcp'
#' @param spPolys Alternatively, provide a SpatialPolygons object.
#'
#' @return Group by ID (by time) data.table
#' @export
#'
#' @examples
#' # Build the HRs for a set of locs by individuals using mcp,
#' # kernel density estimation...
#' data(locs)
#'
#' groups <- GroupPolys('mcp', locs, 50, projection = '+proj=utm +zone=21 ellps=WGS84',
#'                    idField = 'ID')
#'
#' # If you'd like to simply compare proportion or overlap of a set of polygons,
#' # ...
#' data(locsPolys)
#'
#' groups <- GroupPolys(spPolys = locsPolys)
GroupPolys <-
  function(DT = NULL,
           hrType = NULL,
           hrParams = NULL,
           area = NULL,
           projection = NULL,
           coordFields = NULL,
           idField = NULL,
           byFields = NULL,
           spPolys = NULL) {
    if (is.null(area) | !is.logical(area)) {
      stop('area must be provided (TRUE or FALSE)')
    }

    if (is.null(DT) && is.null(spPolys)) {
      stop('must provide either DT or spPolys')
    } else if (!is.null(DT) && !is.null(spPolys)) {
      stop('cannot provide both DT and spPolys')
    }

    if (is.null(byFields)) {
      if (!is.null(DT) && is.null(spPolys)) {
        spPolys <-
          BuildHRs(
            DT = DT,
            projection = projection,
            hrType = hrType,
            hrParams = hrParams,
            coordFields = coordFields,
            idField = idField,
            byFields = NULL,
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
          if (any(DT[, grepl('[^A-z0-9]', get(idField))])) {
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
        set(out, j = 'proportion', value = dd[['area']] / dd[['areaTotal']])
        set(out, j = c('IDs', 'areaTotal'),  value = NULL)
        data.table::setcolorder(out, c('ID1', 'ID2', 'area', 'proportion'))
        return(out[])
      }
    } else if (!is.null(byFields)) {
      if (!is.null(spPolys)) {
        stop('cannot provide spPolys if providing byFields')
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
                BuildHRs(
                  DT = .SD,
                  projection = projection,
                  hrType = hrType,
                  hrParams = hrParams,
                  coordFields = coordFields,
                  idField = idField,
                  byFields = NULL,
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
              data.table(ID = get(idField),
                         withinGroup = as.integer(NA))
            }
          }, by = byFields, .SDcols = c(coordFields, idField)]
        DT[ovrDT, withinGroup := withinGroup, on = c(idField, byFields)]
        DT[, group := ifelse(is.na(withinGroup), as.integer(NA), .GRP),
           by = c(byFields, 'withinGroup')]
        set(DT, j = 'withinGroup', value = NULL)
        return(DT[])
      } else if (area){
        if (any(DT[, grepl('[^A-z0-9]', get(idField))])) {
          stop('please ensure IDs are alphanumeric and do not contain spaces')
        }
        outDT <-
          DT[, {
            suppressWarnings(
              spPolys <-
                BuildHRs(
                  DT = .SD,
                  projection = projection,
                  hrType = hrType,
                  hrParams = hrParams,
                  coordFields = coordFields,
                  idField = idField,
                  byFields = NULL,
                  spPts = NULL
                )
            )
            if (!is.null(spPolys)) {
              inters <- rgeos::gIntersection(spPolys, spPolys, byid = TRUE)
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
              # set(out, j = 'proportion', value = dd[['area']] / dd[['areaTotal']])
              # set(out, j = c('IDs', 'areaTotal'),  value = NULL)
              # out
            } else {
              data.table(ID1 = get(idField),
                         ID2 = as.character(NA),
                         area = as.numeric(NA),
                         proportion = as.numeric(NA))
            }
            # inters <- rgeos::gIntersection(spPolys, spPolys, byid = TRUE)
            #
            # outDT <- data.table::data.table(area = sapply(
            #   inters@polygons,
            #   FUN = function(x) {
            #     slot(x, 'area')
            #   }
            # ))[, c('ID1', 'ID2') := data.table::tstrsplit(sapply(
            #   inters@polygons,
            #   FUN = function(x) {
            #     slot(x, 'ID')
            #   }
            # ),
            # ' ',
            # type.convert = TRUE)]
          }, by = byFields, .SDcols = c(coordFields, idField)]

        return(outDT[])
      }
    }
  }

