#' Groups From Spatial Lines
#'
#' @inheritParams BuildPts
#' @inheritParams GroupPts
#' @param bufferWidth The width of the buffer around the geometry in the units
#'   of the projection. Optionally, exclude argument or supply 0 to compare
#'   SpatialLines intersection
#' @param spLines Alternatively, provide a SpatialLines object created with the sp
#'   package. If a spLines object is provided, groups cannot be calculated by a
#'   timeGroup
#' @return Group by ID (by time) data.table
#' @export
#'
#' @examples
#' data(locs)
#' groups <- GroupLines(locs, 50)
#'
#' groups <- GroupLines(locs, 50, timeGroup = 'FIX_DATE',
#'         projection = '+proj=utm +zone=21 ellps=WGS84',
#'         idField = 'ID')
#'
#'
#' data(locsLines)
#'
#' groups <- GroupLines(spLines = locsLines)
GroupLines <-
  function(DT = NULL,
           bufferWidth = NULL,
           projection = NULL,
           coordFields = NULL,
           idField = NULL,
           timeGroup = NULL,
           groupFields = NULL,
           spLines = NULL) {
    if (is.null(spLines) & !is.null(DT)) {
      if (is.null(projection)) {
        stop('projection must be provided when DT is')
      }

      if (is.null(coordFields)) {
        stop('coordFields must be provided')
      }

      if (is.null(idField)) {
        stop('idField must be provided')
      }

      if (any(!(c(idField, coordFields) %in% colnames(DT)))) {
        stop(paste0(
          as.character(paste(setdiff(
            c(idField, coordFields), colnames(DT)
          ),
          collapse = ', ')),
          ' field(s) provided are not present in input DT'
        ))
      }

      if ('group' %in% colnames(DT)) {
        warning('`group` column will be overwritten by this function')
        set(DT, j = 'group', value = NULL)
      }
    } else if (!is.null(spLines) & is.null(DT)) {
        if(!('SpatialLines' %in% class(spLines) && isS4(spLines))) {
          stop('spLines provided must be a SpatialLines object')
        }
    } else if (!is.null(spLines) & !is.null(DT)) {
      stop('cannot provide both DT and spLines')
    } else {
      stop('must provide either DT or spLines')
    }

    if (is.null(bufferWidth)) {
      warning('buffer width missing, using 0 by default')
      bufferWidth <- 0
    } else if (bufferWidth < 0) {
      stop('cannot provide a negative bufferWidth')
    }


    if (is.null(timeGroup)) {
      if (is.null(spLines)) {
        spLines <- BuildLines(DT, projection, coordFields, idField)
      }
      if (bufferWidth == 0) {
        merged <- rgeos::gBuffer(spLines, width = 0.0001, byid = F)
      } else {
        merged <- rgeos::gBuffer(spLines, width = bufferWidth, byid = FALSE)
      }
      ovr <-
        sp::over(spLines, sp::disaggregate(merged), returnList = T)
      ovrDT <-
        data.table::setnames(data.table::data.table(names(ovr),
                                                    unlist(ovr)),
                             c(idField, 'group'))
      DT[ovrDT, group := group, on = idField][]
    } else {
      if (!is.null(spLines)) {
        stop("if providing a spLines, cannot provide a time field")
      }

      if (is.null(groupFields))
        byFields <- timeGroup
      else
        byFields <- c(groupFields, timeGroup)

      ovrDT <-
        DT[, {
          spLines <- BuildLines(.SD, projection, coordFields, idField)
          if (is.null(spLines)) {
            message('some rows are dropped - unable to build lines with <3 locs')
          } else {
            if (bufferWidth == 0) {
              merged <- rgeos::gBuffer(spLines, width = 0.0001, byid = FALSE)
            } else {
              merged <- rgeos::gBuffer(spLines, width = bufferWidth, byid = FALSE)
            }
            ovr <-
              sp::over(spLines, sp::disaggregate(merged), returnList = TRUE)
            data.table::setnames(data.table::data.table(names(ovr),
                                                        unlist(ovr)),
                                 c(idField, 'withinGroup'))
          }
        }, by = byFields, .SDcols = c(coordFields, idField)]

      DT[ovrDT, withinGroup := withinGroup, on = c(idField, byFields)]
      DT[, group := .GRP, by = c(byFields, 'withinGroup')][, withinGroup := NULL][]
    }
  }
