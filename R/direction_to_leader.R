#' Direction to group leader
#'
#' `direction_to_leader` calculates the direction to the leader of each
#' spatiotemporal group. The function expects a `data.table` with relocation
#' data appended with a `rank_position_group_direction` column indicating the
#' ranked position along the group direction generated with
#' `leader_direction_group(return_rank = TRUE)`. Relocation data should be in
#' two columns representing the X and Y coordinates, or in a geometry column
#' prepared by the helper function [get_geometry()].
#'
#' The `DT` must be a `data.table`. If your data is a `data.frame`, you can
#' convert it by reference using [data.table::setDT()] or by reassigning using
#' [data.table::data.table()].
#'
#' This function expects a `rank_position_group_direction` column generated with
#' `leader_direction_group(return_rank = TRUE)`, a `group` column generated with
#' the `group_pts` function. The `group` argument expects the name of the column
#' in `DT` which correspond to the group column.
#'
#' See below under "Interface" for details on providing coordinates and under
#' "Direction function" for details on the underlying direction function used.
#'
#' @inheritSection direction_step Interface
#' @inheritSection direction_step Direction function
#'
#' @inheritParams distance_to_leader
#' @inheritParams direction_step
#'
#' @return `direction_to_leader` returns the input `DT` appended with a
#'   `direction_leader` column indicating the direction to the group leader in
#'   radians. A value of NaN is returned when the coordinates of the focal
#'   individual equal the coordinates of the leader.
#'
#'   An error is returned if there are any missing values in coordinates for
#'   the focal individual or the group leader, as the underlying direction
#'   function ([lwgeom::st_geod_azimuth()]) does not accept missing values.
#'
#'   A message is returned when the `direction_leader` column already
#'   exist in the input `DT` because it will be overwritten.
#'
#'   See details for appending outputs using modify-by-reference in the
#'   [FAQ](https://docs.ropensci.org/spatsoc/articles/faq.html).
#'
#' @export
#' @family Direction functions
#' @family Leadership functions
#' @seealso [distance_to_leader], [leader_direction_group], [group_pts],
#'   [lwgeom::st_geod_azimuth()]
#' @references
#'
#' See examples of using direction to leader and position within group:
#'  * \doi{doi:10.1016/j.anbehav.2023.09.009}
#'  * \doi{doi:10.1016/j.beproc.2013.10.007}
#'  * \doi{doi:10.1371/journal.pone.0036567}
#'
#' @examples
#' # Load data.table
#' library(data.table)
#' \dontshow{data.table::setDTthreads(1)}
#'
#' # Read example data
#' DT <- fread(system.file("extdata", "DT.csv", package = "spatsoc"))
#'
#' # (Subset example data to reduce example run time)
#' DT <- DT[year(datetime) == 2016]
#'
#' # Cast the character column to POSIXct
#' DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
#'
#' # Temporal grouping
#' group_times(DT, datetime = 'datetime', threshold = '20 minutes')
#'
#' # Spatial grouping with timegroup
#' group_pts(DT, threshold = 50, id = 'ID',
#'           coords = c('X', 'Y'), timegroup = 'timegroup')
#'
#' # Calculate direction at each step
#' direction_step(
#'   DT = DT,
#'   id = 'ID',
#'   coords = c('X', 'Y'),
#'   crs = 32736
#' )
#'
#' # Calculate group centroid
#' centroid_group(DT, coords = c('X', 'Y'))
#'
#' # Calculate group direction
#' direction_group(DT)
#'
#' # Calculate leader in terms of position along group direction
#' leader_direction_group(
#'   DT,
#'   coords = c('X', 'Y'),
#'   return_rank = TRUE
#' )
#'
#' # Calculate direction to leader
#' direction_to_leader(DT, coords = c('X', 'Y'), crs = 32736)
direction_to_leader <- function(
    DT = NULL,
    coords = NULL,
    group = 'group',
    crs = NULL,
    geometry = 'geometry') {

  # Due to NSE notes
  geo <- lead <- x <- y <- x_leader <- y_leader <- . <-
    rank_position_group_direction <- has_leader <- direction_leader <- NULL

  assert_not_null(DT)
  assert_is_data_table(DT)
  assert_not_null(group)
  assert_are_colnames(DT, group)

  leader_col <- 'rank_position_group_direction'
  assert_are_colnames(
    DT, leader_col,
    ', did you run leader_direction_group(return_rank = TRUE)?'
  )
  assert_col_inherits(DT, leader_col, 'numeric')

  check_leaderless <- DT[, .(
    has_leader = any(rank_position_group_direction == 1)),
    by = c(group)][!(has_leader)]

  out_col <- 'direction_leader'

  if (is.null(coords)) {
    if (!is.null(crs)) {
      message('crs argument is ignored when coords are null, using geometry')
    }

    assert_are_colnames(DT, geometry, ', did you run get_geometry()?')
    assert_col_inherits(DT, geometry, 'sfc_POINT')

    zzz_geometry_leader <- 'zzz_geometry_leader'
    DT[, c(zzz_geometry_leader) :=
      sf::st_sf(rep(geo[which(rank_position_group_direction == 1)], .N)),
       env = list(geo = geometry),
       by = c(group)]

    if (check_leaderless[, .N > 0]) {
      warning(
        'groups found missing leader (rank_position_group_direction == 1): \n',
        check_leaderless[, paste(group, collapse = ', ')]
      )
    }

    if (out_col %in% colnames(DT)) {
      message(
        paste0(out_col, ' column will be overwritten by this function')
      )
      data.table::set(DT, j = out_col, value = NULL)
    }

    crs <- sf::st_crs(DT[[geometry]])
    use_transform <- !sf::st_is_longlat(crs)

    if (is.na(use_transform)) {
      rlang::abort(paste0('sf::st_is_longlat(crs) is ', use_transform, ', ensure crs is provided for direction functions'))
    }

    DT[!group %in% check_leaderless$group &
         !sf::st_is_empty(geo) &
         !sf::st_is_empty(lead),
      direction_leader := calc_direction(
        geometry_a = geo,
        geometry_b = lead,
        use_transform = use_transform
      ),
      env = list(
        geo = geometry, lead = zzz_geometry_leader
      )
    ]

    data.table::set(DT, j = zzz_geometry_leader, value = NULL)

  } else {
    assert_are_colnames(DT, coords)
    assert_length(coords, 2)
    assert_col_inherits(DT, coords, 'numeric')
    assert_not_null(crs)

    xcol <- data.table::first(coords)
    ycol <- data.table::last(coords)
    pre <- 'zzz_leader_'
    zzz_xcol_leader <- paste0(pre, xcol)
    zzz_ycol_leader <- paste0(pre, ycol)
    zzz_coords_leader  <- c(zzz_xcol_leader, zzz_ycol_leader)

    DT[, c(zzz_coords_leader) :=
         .SD[which(rank_position_group_direction == 1)],
       .SDcols = c(coords),
       by = c(group)]

    if (check_leaderless[, .N > 0]) {
      warning(
        'groups found missing leader (rank_position_group_direction == 1): \n',
        check_leaderless[, paste(group, collapse = ', ')]
      )
    }

    if (out_col %in% colnames(DT)) {
      message(
        paste0(out_col, ' column will be overwritten by this function')
      )
      data.table::set(DT, j = out_col, value = NULL)
    }

    use_transform <- !sf::st_is_longlat(crs)

    if (is.na(use_transform)) {
      rlang::abort(paste0('sf::st_is_longlat(crs) is ', use_transform,
                          ', ensure crs is provided for direction functions'))
    }
    DT[!group %in% check_leaderless$group &
        !is.na(x) & !is.na(y) & !is.na(x_leader) & !is.na(y_leader),
      direction_leader := calc_direction(
        x_a = x,
        y_a = y,
        x_b = x_leader,
        y_b = y_leader,
        crs = crs,
        use_transform = use_transform
      ),
      env = list(
        x = xcol, y = ycol, x_leader = zzz_xcol_leader, y_leader = zzz_ycol_leader
      )
    ]

    data.table::set(DT, j = zzz_coords_leader, value = NULL)

  }

  return(DT[])
}
