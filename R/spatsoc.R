#' spatsoc
#'
#' spatsoc is an R package for detecting spatial and temporal groups in GPS
#' relocations and measuring intragroup social dynamics. It can be used to convert GPS relocations to gambit-of-the-group
#' format to build proximity-based social networks, identify nearest neighbours distances and direction, and measure individual's position with respect to the group centroid or group leader. See all available functions below, in the manual or on the acommpanying documentation website at https://docs.ropensci.org/spatsoc/.
#'
#' Temporal grouping:
#'
#' - [`group_times()`]
#'
#' Spatial grouping:
#'
#' - [`group_lines()`]
#' - [`group_polys()`]
#' - [`group_pts()`]
#'
#' Edge-list generation:
#'
#' - [`edge_dist()`]
#' - [`edge_nn()`]
#' - [`edge_delay()`]
#' - [`edge_alignment()`]
#' - [`edge_direction()`]
#' - [`edge_zones()`]
#'
#' Social network tools:
#'
#' - [`randomizations()`]
#' - [`get_gbi()`]
#'
#' Dyad functions
#'
#' - [`dyad_id()`]
#' - [`fusion_id()`]
#'
#' Centroid functions
#'
#' - [`centroid_group()`]
#' - [`centroid_dyad()`]
#' - [`centroid_fusion()`]
#'
#' Direction functions
#'
#' - [`direction_step()`]
#' - [`direction_to_centroid()`]
#' - [`direction_to_leader()`]
#' - [`direction_group()`]
#' - [`direction_polarization()`]
#'
#' Distance functions
#'
#' - [`distance_to_centroid()`]
#' - [`distance_to_leader()`]
#'
#' Leadership functions
#'
#' - [`leader_direction_group()`]
#' - [`leader_edge_delay()`]
#'
#' Geometry interface functions
#'
#' - [`get_geometry()`]
#'
#' Build functions
#' - [`build_lines()`]
#' - [`build_polys()`]
#'
#' @docType package
#' @name spatsoc
#' @aliases spatsoc-package
#' @keywords internal
"_PACKAGE"
