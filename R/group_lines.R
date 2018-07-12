#' Groups Lines
#'
#' @inheritParams group_pts
#' @param projection PROJ.4 character string
#' @param threshold The width of the buffer around the lines in the units
#'   of the projection. Alternatively, supply 0 to compare intersection without buffering.
#' @param spLines Alternatively, provide a SpatialLines object created with the sp
#'   package. If a spLines object is provided, groups cannot be calculated by a
#'   timegroup
#' @return Group by ID (by time) data.table
#' @export
#'
#' @examples
#' library(data.table)
#' DT <- fread(system.file("extdata", "DT.csv", package = "spatsoc"))
#' DT[, datetime := as.POSIXct(datetime,
#'                             tz = 'UTC')]
#'
#' utm <- '+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
#'
#' group_lines(DT, threshold = 50, projection = utm,
#'             id = 'ID', coords = c('X', 'Y'))
#'
#' # Daily movement tracks
#' group_times(DT, datetime = 'datetime', threshold = '1 day')
#' group_lines(DT, threshold = 50, projection = utm,
#'             id = 'ID', coords = c('X', 'Y'),
#'             timegroup = 'timegroup')
#'
#' # Daily movement tracks by year
#' DT[, yr := year(datetime)]
#' group_lines(DT, threshold = 50, projection = utm,
#'             id = 'ID', coords = c('X', 'Y'),
#'             timegroup = 'timegroup',
#'             splitBy = 'yr')
group_lines <-
  function(DT = NULL,
           threshold = NULL,
           projection = NULL,
           id = NULL,
           coords = NULL,
           timegroup = NULL,
           splitBy = NULL,
           spLines = NULL) {

    # due to NSE notes in R CMD check
    group <- ..coords <- ..id <- withinGroup <- NULL

    if (is.null(threshold)) {
      warning('threshold missing, using 0 by default')
      threshold <- 0
    } else if (threshold < 0) {
      stop('cannot provide a negative threshold')
    }

    if (!is.null(spLines) && !is.null(DT)) {
      stop('cannot provide both DT and spLines')
    } else if (is.null(spLines) && is.null(DT)) {
      stop('must provide either DT or spLines')
    } else if (is.null(spLines) && !is.null(DT)) {
      if (is.null(projection)) {
        stop('projection must be provided when DT is')
      }

      if (is.null(coords)) {
        stop('coords must be provided')
      }

      if (is.null(id)) {
        stop('id must be provided')
      }

      if (any(!(c(id, coords, timegroup) %in% colnames(DT)))) {
        stop(paste0(
          as.character(paste(setdiff(
            c(id, coords), colnames(DT)
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

      if (threshold == 0) {
        inter <- rgeos::gIntersects(spLines, spLines, byid = TRUE)
      } else {
        buffered <- rgeos::gBuffer(spLines, width = threshold,
                                   byid = TRUE)
        inter <- rgeos::gIntersects(spLines, buffered, byid = TRUE)
      }
      g <- igraph::graph_from_adjacency_matrix(inter)
      ovr <- igraph::clusters(g)$membership
      out <- data.table::data.table(names(ovr),
                                      unlist(ovr))
      data.table::setnames(out, c('ID', 'group'))
      return(out[])
    }

    if (is.null(timegroup)) {
      suppressWarnings(
        spLines <- build_lines(
          DT = DT,
          projection = projection,
          coords = coords,
          id = id
        )
      )
      if (!is.null(spLines)) {
        if (threshold == 0) {
          inter <- rgeos::gIntersects(spLines, spLines, byid=TRUE)
        } else {
          buffered <- rgeos::gBuffer(spLines, width = threshold, byid = TRUE)
          inter <- rgeos::gIntersects(spLines, buffered, byid = TRUE)
        }
        g <- igraph::graph_from_adjacency_matrix(inter)
        ovr <- igraph::clusters(g)$membership
        ovrDT <- data.table::data.table(ID = names(ovr),
                                        group = unlist(ovr))
      } else {
        ovrDT <- data.table::data.table(ID = DT[[id]], group = as.integer(NA))
      }

      data.table::setnames(ovrDT, c(id, 'group'))
      DT[ovrDT, group := group, on = id]
      if (DT[is.na(group), .N] > 0) {
        warning(
          strwrap(
            prefix = " ",
            initial = "",
            x = 'some rows were dropped,
            cannot build a line with < 2 points.
            in this case, group set to NA.'
          )
        )
      }
      return(DT[])
    } else {

      if (is.null(splitBy)) {
        splitBy <- timegroup
      }
      else {
        splitBy <- c(splitBy, timegroup)
      }
      ovrDT <-
        DT[, {
          suppressWarnings(
            spLines <- build_lines(
              DT = .SD,
              projection = projection,
              coords = ..coords,
              id = ..id
            )
          )
          if (!is.null(spLines)) {
            if (threshold == 0) {
              inter <- rgeos::gIntersects(spLines, spLines, byid=TRUE)
            } else {
              buffered <- rgeos::gBuffer(spLines, width = threshold,
                                         byid = TRUE)
              inter <- rgeos::gIntersects(spLines, buffered, byid=TRUE)

            }
            g <- igraph::graph_from_adjacency_matrix(inter)
            ovr <- igraph::clusters(g)$membership
            out <- data.table::data.table(names(ovr),
                                          unlist(ovr))
            data.table::setnames(out, c(..id, 'withinGroup'))
          } else {
            out <- data.table(get(..id), withinGroup = as.double(NA))
            data.table::setnames(out, c(..id, 'withinGroup'))

          }
        }, by = splitBy, .SDcols = c(coords, id)]

      DT[ovrDT, withinGroup := withinGroup, on = c(id, splitBy)]
      DT[, group := ifelse(is.na(withinGroup), as.integer(NA), .GRP),
         by = c(splitBy, 'withinGroup')]
      set(DT, j = 'withinGroup', value = NULL)
      if (DT[is.na(group), .N] > 0) {
        warning(
          strwrap(
            prefix = " ",
            initial = "",
            x = 'some rows were dropped,
            cannot build a line with < 2 points.
            in this case, group set to NA.'
          )
        )
      }
      return(DT[])
    }
  }
