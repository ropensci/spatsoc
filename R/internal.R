assert_are_colnames <- function(x, nms, ...) {
  if (length(nms)) {
    for (nm in nms) {
      if(!nm %in% colnames(x)) {
        rlang::abort(paste0(nm, ' field provided is not present in input DT',
                            ...),
                     call = rlang::caller_env())
      } else {
        invisible(NULL)
      }
    }
  }
}

assert_col_inherits <- function(x, cols, classes, ...) {
  if (length(cols)) {
    for (col in cols) {
      if(!inherits(x[[col]], classes)) {
        rlang::abort(paste0(rlang::caller_arg(cols), ' must be of class ',
                            paste0(classes, collapse = '/'),
                            ...),
                     call = rlang::caller_env())
      } else {
        invisible(NULL)
      }
    }
  }
}

assert_col_radians <- function(x, col, ...) {
  if(!identical(units(x[[col]])$numerator, 'rad')) {
    rlang::abort(paste0(rlang::caller_arg(col), ' must be of units radians', ...),
                 call = rlang::caller_env())
  } else {
    invisible(NULL)
  }
}

assert_col_typeof <- function(x, cols, type, ...) {
  if (length(cols)) {
    for (col in cols) {
      if(!identical(typeof(x[[col]]), type)) {
        rlang::abort(paste0(rlang::caller_arg(cols), ' must be of type ',
                            paste0(type, collapse = '/'),
                            ...),
                     call = rlang::caller_env())
      } else {
        invisible(NULL)
      }
    }
  }
}

assert_inherits <- function(x, classes, ...) {
  if(!inherits(x, classes)) {
    rlang::abort(paste0(rlang::caller_arg(x), ' must be of class ',
                        paste0(classes, collapse = '/'),
                        ...),
                 call = rlang::caller_env())
  } else {
    invisible(NULL)
  }
}

assert_is_data_table <- function(x) {
  if (!data.table::is.data.table(x)) {
    rlang::abort(paste0('input ', rlang::caller_arg(x), ' must be a data.table'),
                 call = rlang::caller_env())
  }
  if (nrow(x) == 0L) {
    rlang::abort(paste0('input ', rlang::caller_arg(x), ' has zero rows'),
                 call = rlang::caller_env())
  }
  return(invisible(NULL))
}

assert_length <- function(x, len) {
  if (length(x) != len) {
    rlang::abort(paste0(rlang::caller_arg(x), ' must be length ', len),
                 call = rlang::caller_env())
  } else {
    return(invisible(NULL))
  }
}

assert_not_missing <- function(x) {
  if (missing(x)) {
    rlang::abort(paste0(rlang::caller_arg(x), ' must be provided'),
                 call = rlang::caller_env())
  }
  return(invisible(NULL))
}

assert_not_null <- function(x, ...) {
  if (is.null(x)) {
    rlang::abort(paste0(rlang::caller_arg(x), ' must be provided', ...),
                 call = rlang::caller_env())
  }
  return(invisible(NULL))
}

assert_relation <- function(x, fun, y, ..., n = 1) {
  if (!(fun(x, y))) {
    rlang::abort(
      paste0(
        rlang::caller_arg(x),
        ' must be ',
        rlang::caller_arg(fun),
        ' ',
        rlang::caller_arg(y),
        ...
      ),
      call = rlang::caller_env(n = n)
    )
  }
  return(invisible(NULL))
}

assert_threshold <- function(threshold = NULL, crs = NULL) {
  if (inherits(threshold, 'units')) {
    if (any(is.null(crs), is.na(crs))) {
      assert_relation(threshold, `>`, units::as_units(0, units(threshold)))
    } else {
      assert_units_match(threshold, sf::st_crs(crs)$SemiMajor, n = 2)
      assert_relation(threshold, `>`, units::as_units(0, units(threshold)))
    }
  } else {
    if (any(is.null(crs), is.na(crs))) {
      assert_relation(threshold, `>`, 0)
    } else {
      assert_relation(units::as_units(threshold, units(sf::st_crs(crs)$SemiMajor)),
                      `>`,
                      units::as_units(0, units(sf::st_crs(crs)$SemiMajor)),
                      n = 2)
    }
    return(invisible(NULL))
  }
}

assert_units_match <- function(x, y, n = 1) {
  if (isFALSE(identical(units(x), units(y)))) {
    rlang::abort(
      paste0(
        'units of ',
        rlang::caller_arg(x),
        ' (', units(x), ')',
        ' do not match units of ',
        rlang::caller_arg(y),
        ' (', units(y), ')'
      ),
      call = rlang::caller_env(n = n)
    )
  }
  return(invisible(NULL))
}

#' Calculate centroid
#'
#' **Internal function** - not developed to be used outside of spatsoc functions
#'
#' Calculate centroid using [sf::st_centroid()] for one of:
#' - geometry
#' - the points in x, y
#' - the pairwise points in geometry_a and geometry_b
#' - the pairwise points in x_a, y_a and x_b, y_b
#' @param geometry sfc (simple feature geometry list column) from [get_geometry()]
#' @param x X coordinate column, numeric
#' @param y Y coordinate column, numeric
#' @param crs crs for x, y coordinates, ignored for geometry argument
#'
#' @returns
#'
#' Centroid of the geometry or coordinates in x,y provided
#'
#' @keywords internal
#' @examples
#' # Load data.table
#' library(data.table)
#' \dontshow{
#' data.table::setDTthreads(1)
#' }
#' # Read example data
#' DT <- fread(system.file("extdata", "DT.csv", package = "spatsoc"))
#'
#' DT[, spatsoc:::calc_centroid(x = X, y = Y, crs = 32736)]
calc_centroid <- function(geometry, x, y, crs) {
  if (!missing(geometry) && missing(x) && missing(y)) {
    sf::st_centroid(sf::st_combine(geometry))
  } else if (missing(geometry) && !missing(x) && !missing(y)) {
    sf::st_centroid(sf::st_combine(
      sf::st_as_sf(
        data.frame(x, y),
        crs = crs,
        coords = seq.int(2),
        na.fail = FALSE
      )
    ))
  } else {
    rlang::abort(c(
      'arguments incorrectly provided, use one of the following combinations:',
      '1. geometry',
      '2. x, y'
    ))
  }
}

#' Calculate direction
#'
#' **Internal function** - not developed to be used outside of spatsoc functions
#'
#' Calculate direction using [lwgeom::st_geod_azimuth()] between one of:
#' - the sequence of points in geometry_a
#' - the sequence of points in x_a, y_a
#' - the pairwise points in geometry_a and geometry_b
#' - the pairwise points in x_a, y_a and x_b, y_b
#'
#' Requirements:
#' - matching length between a and b objects if b provided
#' - crs is longlat, check with [sf::st_is_longlat()]
#' - no missing values in coordinates
#'
#' @param geometry_a,geometry_b sfc (simple feature geometry list column) from [get_geometry()]
#' @param x_a,x_b X coordinate column, numeric
#' @param y_a,y_b Y coordinate column, numeric
#' @param crs crs for x_a, y_a (and if provided, x_b, y_b) coordinates,
#' ignored for geometry_a and geometry_b arguments
#'
#' @returns
#'
#' Direction in units of radians, in range of pi, -pi where North = 0.
#' @keywords internal
#' @examples
#' # Load data.table
#' library(data.table)
#' \dontshow{data.table::setDTthreads(1)}
#'
#' # Example result for East, North, West, South directions
#' example <- data.table(
#'   X = c(0, 5, 5, 0, 0),
#'   Y = c(0, 0, 5, 5, 0)
#' )
#' # E, N, W, S
#' example[, spatsoc:::calc_direction(x_a = X, y_a = Y, crs = 4326)]
calc_direction <- function(
    geometry_a, geometry_b,
    x_a, y_a,
    x_b, y_b,
    crs) {
  if (!missing(geometry_a) && missing(x_a) && missing(y_a)
      && missing(x_b) && missing(y_b)) {
    if(any(rowSums(is.na(st_coordinates(geometry_a))) == 2)) {
      rlang::abort('missing values in coordinates')
    }
    if (!missing(geometry_b)) {
      if(any(rowSums(is.na(st_coordinates(geometry_b))) == 2)) {
        rlang::abort('missing values in coordinates')
      }
      lwgeom::st_geod_azimuth(geometry_a, geometry_b)
    } else {
      lwgeom::st_geod_azimuth(geometry_a)
    }
  } else if (missing(geometry_a) && !missing(x_a) && !missing(y_a)) {
    if (!missing(x_b) && !missing(y_b)) {
      lwgeom::st_geod_azimuth(
        x = sf::st_as_sf(data.frame(x_a, y_a), crs = crs, coords = seq.int(2),
                         na.fail = TRUE),
        y = sf::st_as_sf(data.frame(x_b, y_b), crs = crs, coords = seq.int(2),
                         na.fail = TRUE)
      )
    } else {
      lwgeom::st_geod_azimuth(
        x = sf::st_as_sf(data.frame(x_a, y_a), crs = crs, coords = seq.int(2),
                         na.fail = TRUE)
      )
    }
  } else {
    rlang::abort(c(
      'arguments incorrectly provided, use one of the following combinations:',
      '1. geometry_a',
      '2. geometry_a and geometry_b',
      '3. x_a, y_a',
      '4. x_a, y_a, and x_b, y_b'
    ))
  }
}

#' Calculate distance
#'
#' **Internal function** - not developed to be used outside of spatsoc functions
#'
#' Calculate distance using [sf::st_distance()] for one of the following combinations:
#' - the distance matrix of points in geometry_a
#' - the distance matrix of points in x_a, y_a
#' - the pairwise distance between points in geometry_a and geometry_b
#' - the pairwise distance between points in x_a, y_a and x_b, y_b
#'
#' Requirements:
#' - matching length between a and b objects if b provided
#'
#' @param geometry_a,geometry_b sfc (simple feature geometry list column)
#' from [get_geometry()]
#' @param x_a,x_b X coordinate column, numeric
#' @param y_a,y_b Y coordinate column, numeric
#' @param crs crs for x_a, y_a (and if provided, x_b, y_b) coordinates,
#' ignored for geometry_a and geometry_b arguments
#'
#' @returns
#'
#' Distance with unit of measurement, see details in [sf::st_distance()]
#'
#' @keywords internal
#' @examples
#' # Load data.table
#' library(data.table)
#' \dontshow{data.table::setDTthreads(1)}
#'
#' # Example points
#' example <- data.table(
#'   X = c(0, 5, 5, 0, 0, NA_real_, 0,        NA_real_),
#'   Y = c(0, 0, 5, 5, 0, 0,        NA_real_, NA_real_)
#' )
#' # E, N, W, S
#' example[, spatsoc:::calc_distance(x_a = X, y_a = Y, crs = 4326)]
calc_distance <- function(
  geometry_a, geometry_b,
  x_a, y_a,
  x_b, y_b,
  crs,
  use_dist) {
 if (!missing(geometry_a) && missing(x_a) && missing(y_a) &&
     missing(x_b) && missing(y_b)) {
   if (!missing(geometry_b)) {
     # Pairwise
     sf::st_distance(geometry_a, geometry_b, by_element = TRUE)
   } else {
     # Matrix
     sf::st_distance(geometry_a, by_element = FALSE)
   }
 } else if (missing(geometry_a) && !missing(x_a) && !missing(y_a)) {
   if (!missing(x_b) && !missing(y_b)) {
     # Pairwise
     sf::st_distance(
       x = sf::st_as_sf(data.frame(x_a, y_a), crs = crs, coords = seq.int(2),
                        na.fail = FALSE),
       y = sf::st_as_sf(data.frame(x_b, y_b), crs = crs, coords = seq.int(2),
                        na.fail = FALSE),
       by_element = TRUE
     )
   } else {
     # Matrix
     sf::st_distance(
       x = sf::st_as_sf(data.frame(x_a, y_a), crs = crs, coords = seq.int(2),
                        na.fail = FALSE),
       by_element = FALSE
     )
   }
 } else {
   rlang::abort(c(
     'arguments incorrectly provided, use one of the following combinations:',
     '1. geometry_a',
     '2. geometry_a and geometry_b',
     '3. x_a, y_a',
     '4. x_a, y_a, and x_b, y_b'
   ))
 }
}

pairwise_dist <- function(geometry_a, geometry_b,
                          x_a, y_a,
                          x_b, y_b) {
  if (!missing(geometry_a) && missing(x_a) && missing(y_a) &&
      !missing(geometry_b)) {
    a <- sf::st_coordinates(geometry_a)
    b <- sf::st_coordinates(geometry_b)
    sqrt((a[, 1] - b[, 1]) ^ 2 + (a[, 2] - b[, 2]) ^ 2)
  } else if (missing(geometry_a) && !missing(x_a) && !missing(y_a) &&
             !missing(x_b) && !missing(y_b)) {
    sqrt((x_a - x_b) ^ 2 + (y_a - y_b) ^ 2)
  }
}
#' Difference of two angles measured in radians
#'
#' **Internal function** - not developed to be used outside of spatsoc functions
#'
#' @param x angle in radians
#' @param y angle in radians
#' @param signed logical if signed difference should be returned, default FALSE
#' @param return_units return difference with units = 'rad'
#'
#' @return Difference between x and y in radians. If signed is TRUE, the signed
#'   difference is returned. If signed is FALSE, the absolute difference is
#'   returned. Note: The difference is the smallest difference, eg. the
#'   difference between 2 rad and -2.5 rad is 1.78.
#' @references adapted from https://stackoverflow.com/a/7869457
#' @keywords internal
#' @examples
#' # Load data.table, units
#' library(data.table)
#' library(units)
#' \dontshow{data.table::setDTthreads(1)}
#'
#' # Read example data
#' DT <- fread(system.file("extdata", "DT.csv", package = "spatsoc"))
#'
#' # Cast the character column to POSIXct
#' DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
#'
#' # Set order using data.table::setorder
#' setorder(DT, datetime)
#'
#' # Calculate direction
#' direction_step(
#'   DT = DT,
#'   id = 'ID',
#'   coords = c('X', 'Y'),
#'   crs = 32736
#' )
#'
#' # Differences
#' spatsoc:::diff_rad(DT[1, direction], DT[2, direction])
#'
#' # Note smallest difference returned
#' spatsoc:::diff_rad(as_units(2, 'rad'), as_units(-2.5, 'rad'))
diff_rad <- function(x, y, signed = FALSE, return_units = FALSE) {
  if (!inherits(x, 'units') || units(x)$numerator != 'rad') {
    stop('units(x) is not radians')
  }
  if (!inherits(y, 'units') || units(y)$numerator != 'rad') {
    stop('units(y) is not radians')
  }

  d <- units::drop_units(y) - units::drop_units(x)
  d <- ((d + pi) %% (2 * pi)) - pi

  if (signed) {
    out <- d
  } else {
    out <- abs(d)
  }

  if (return_units) {
    return(units::as_units(out, 'rad'))
  } else {
    return(out)
  }
}
