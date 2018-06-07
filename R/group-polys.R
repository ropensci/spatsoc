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


    if(is.null(byFields)) {
      if(!is.null(DT) && is.null(spPolys)) {
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
        unionPolys <- rgeos::gUnaryUnion(spPolys)
        ovr <- sp::over(spPolys, sp::disaggregate(unionPolys))
        ovrDT <- data.table::data.table(names(ovr),
                                        ovr)
        data.table::setnames(ovrDT, c('ID', 'group'))
        # check if null byfields, if it isnt null split the -
        return(ovrDT[])
      } else if (area) {
        inters <- rgeos::gIntersection(spPolys, spPolys, byid = TRUE)
        data.table::data.table(area = sapply(
          inters@polygons,
          FUN = function(x) {
            slot(x, 'area')
          }
        )/1e6)[, # cant actually use 1e6, but how do we standardize units?
           # this is susceptible to error if ID field provided has spaces
           c('ID1', 'ID2') := data.table::tstrsplit(sapply(
             inters@polygons,
             FUN = function(x) {
               slot(x, 'ID')
             }
           ),
           ' ',
           type.convert = TRUE)][]
      }
    } else if (byFields) {
      if(!is.null(spPolys)) {
        stop('cannot provide spPolys if providing byFields')
      }

      DT[, {

      }, by = byFields]

      # ovr vor WIHTIN WIHTI
    }
}

  }
