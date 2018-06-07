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
  function(area = NULL,
           DT = NULL,
           projection = NULL,
           hrType = NULL,
           hrParams = NULL,
           coordFields = NULL,
           idField = NULL,
           byFields = NULL,
           spPolys = NULL) {

    if (is.null(area) | !is.logical(area)) {
      stop('must provide TRUE or FALSE for area parameter')
    }


    if (is.null(DT) && !is.null(spPolys)) {
      unionPolys <- rgeos::gUnaryUnion(spPolys)
      ovr <- sp::over(spPolys, sp::disaggregate(unionPolys))
      data.table::setnames(data.table::data.table(names(ovr),
                                                  ovr),
                           c(idField, 'group'))[]
    } else if (!is.null(DT) && is.null(spPolys)) {
      if (any(!(c(idField, coordFields) %in% colnames(DT)))) {
        stop('some fields provided are not present in data.table provided/colnames(DT)')
      }
      spPolys <-
        BuildHRs(hrType, hrParams, DT, projection, coordFields, idField)
    }

    if (area) {
      inters <- rgeos::gIntersection(spPolys, spPolys, byid = TRUE)

      data.table::data.table(area = sapply(
        inters@polygons,
        FUN = function(x) {
          slot(x, 'area')
        }
      ))[,
         c('ID1', 'ID2') := data.table::tstrsplit(sapply(
           inters@polygons,
           FUN = function(x) {
             slot(x, 'ID')
           }
         ),
         ' ',
         type.convert = TRUE)][]
      # this is susceptible to error if ID field provided has spaces
    } else if (is.null(DT) && is.null(spPolys)) {
      stop('must provide either DT or spLines')
    } else (!is.null(DT) && !is.null(spPolys)) {
      stop('cannot provide both DT and spLines')
    }


  }

# TODO: optional proportion overlap
# TODO: by year
# TODO: add adehabitat to depends
