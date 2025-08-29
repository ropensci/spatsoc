# Behavioural zones

#' @export
#' TODO: @references (Couzin 2002, used in eg. Lukeman 2010, Klamser 2021, Klamser 2021)
#' TODO: @inheritParams centroid_dyad
edge_zones <- function(
    edges = NULL,
    zone_thresholds = NULL,
    zone_labels = NULL,
    blind_volume = NULL) {

  # TODO: check if
  #  - direction
  #  - direction_dyad
  #  - zones
  #    - zone repulsion < orientation < attraction
  #  - blind_volume

  edges[, zones := cut(distance, breaks = c(0, zone_thresholds),
                       labels = c(zone_labels))]

  if (!is.null(blind_volume)) {
    edges[, direction_dyad_relative :=
            diff_rad(direction, direction_dyad, signed = TRUE)]

    edges[abs(direction_dyad_relative) > blind_volume & !is.na(zones),
          zones := 'blind']
  }

}
