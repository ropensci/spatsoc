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
           idField = NULL,
           coordFields = NULL,
           timeGroup = NULL,
           groupFields = NULL,
           spLines = NULL) {

    if (is.null(bufferWidth)) {
      warning('buffer width missing, using 0 by default')
      bufferWidth <- 0
    } else if (bufferWidth < 0) {
      stop('cannot provide a negative bufferWidth')
    }

    if (!is.null(spLines) && !is.null(DT)) {
      stop('cannot provide both DT and spLines')
    } else if (is.null(spLines) && is.null(DT)) {
      stop('must provide either DT or spLines')
    } else if (is.null(spLines) & !is.null(DT)) {
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
        warning('group column will be overwritten by this function')
        set(DT, j = 'group', value = NULL)
      }
    } else if (!is.null(spLines) && is.null(DT)) {
      if (!('SpatialLines' %in% class(spLines) && isS4(spLines))) {
        stop('spLines provided must be a SpatialLines object')
      }

      if (bufferWidth == 0) {
        inter <- rgeos::gIntersects(spLines, spLines, byid=TRUE)
      } else {
        buffered <- rgeos::gBuffer(spLines, width = bufferWidth, byid = TRUE)
        inter <- rgeos::gIntersects(spLines, buffered, byid=TRUE)
      }
      g <- igraph::graph_from_adjacency_matrix(inter)
      ovr <- igraph::clusters(g)$membership
      out <- data.table::data.table(names(ovr),
                                      unlist(ovr))
      data.table::setnames(outDT, c('ID', 'group'))
      return(outDT)
    }
    if (is.null(timeGroup)) {
      suppressWarnings(
        spLines <- BuildLines(
          DT,
          projection = projection,
          coordFields = coordFields,
          idField = idField
        )
      )
      if (!is.null(spLines)) {
        if (bufferWidth == 0) {
          inter <- rgeos::gIntersects(spLines, spLines, byid=TRUE)
        } else {
          buffered <- rgeos::gBuffer(spLines, width = bufferWidth, byid = TRUE)
          inter <- rgeos::gIntersects(spLines, buffered, byid = TRUE)
        }
        g <- igraph::graph_from_adjacency_matrix(inter)
        ovr <- igraph::clusters(g)$membership
        ovrDT <- data.table::data.table(ID = names(ovr), group = unlist(ovr))
      } else {
        ovrDT <- data.table::data.table(ID = get(idField), group = as.integer(NA))
      }

      data.table::setnames(ovrDT, c(idField, 'group'))
      DT[ovrDT, group := group, on = idField]
      if (DT[is.na(group), .N] > 0) {
        warning('some rows were dropped, cannot build a line with < 2 points. in this case, group set to NA.')
      }
      return(DT[])
    } else {

      ### CHECK THAT TIMEGROUP IS IN DT
      if (is.null(groupFields)) {
        byFields <- timeGroup
      }
      else {
        byFields <- c(groupFields, timeGroup)
      }
      ovrDT <-
        DT[, {
          suppressWarnings(
            spLines <- BuildLines(
              DT = .SD,
              projection = projection,
              coordFields = coordFields,
              idField = idField
            )
          )
          if (!is.null(spLines)) {
            if (bufferWidth == 0) {
              inter <- rgeos::gIntersects(spLines, spLines, byid=TRUE)
            } else {
              buffered <- rgeos::gBuffer(spLines, width = bufferWidth,
                                         byid = TRUE)
              inter <- rgeos::gIntersects(spLines, buffered, byid=TRUE)

            }
            g <- igraph::graph_from_adjacency_matrix(inter)
            ovr <- igraph::clusters(g)$membership
            out <- data.table::data.table(names(ovr),
                                          unlist(ovr))
            # DROP THE SETNAMES AND JUST KEEP WITHINGROUP?
            data.table::setnames(out, c(idField, 'withinGroup'))
          } else {
            # why is a double??
            out <- data.table(get(idField), withinGroup = as.double(NA))
            data.table::setnames(out, c(idField, 'withinGroup'))

          }
        }, by = byFields, .SDcols = c(coordFields, idField)]

      DT[ovrDT, withinGroup := withinGroup, on = c(idField, byFields)]
      DT[, group := ifelse(is.na(withinGroup), as.integer(NA), .GRP),
         by = c(byFields, 'withinGroup')]
      # DT[withinGroup == -999L, group := NA]
      set(DT, j = 'withinGroup', value = NULL)
      if (DT[is.na(group), .N] > 0) {
        warning('some rows were dropped, cannot build a line with < 2 points. in this case, group set to NA.')
      }
      return(DT[])
    }
  }
