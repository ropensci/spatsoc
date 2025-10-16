assert_not_null <- function(x) {
  if (is.null(x)) {
    rlang::abort(paste0(rlang::caller_arg(x), ' must be provided'),
                 call = rlang::caller_env())
  }
  return(invisible(NULL))
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

assert_are_colnames <- function(x, nms) {
  if (length(nms)) {
    for (nm in nms) {
      if(!nm %in% colnames(x)) {
        rlang::abort(paste0(nm, ' field provided is not present in input DT'),
                     call = rlang::caller_env())
      } else {
        invisible(NULL)
      }
    }
  }
}

assert_inherits <- function(x, cols, classes) {
  if (length(cols)) {
    for (col in cols) {
      if(!inherits(x[[col]], classes)) {
        rlang::abort(paste0(rlang::caller_arg(cols), ' must be ',
                            paste0(classes, collapse = '/')),
                     call = rlang::caller_env())
      } else {
        invisible(NULL)
      }
    }
  }
}

assert_length <- function(x, len) {
  if (length(x) != len) {
    rlang::abort(paste0(rlang::caller_arg(x), ' must be length ', len),
                 call = rlang::caller_env())
  } else {
    return(invisible(NULL))
  }
}

assert_relation <- function(x, fun, y) {
  if (!(fun(x, y))) {
    rlang::abort(
      paste(
        rlang::caller_arg(x),
        'must be',
        rlang::caller_arg(fun),
        rlang::caller_arg(y)
      ),
      call = rlang::caller_env()
    )
  } else {
    return(invisible(NULL))
  }
}

assert_not_missing <- function(x) {
  if (is.missing(x)) {
    rlang::abort(paste0(rlang::caller_arg(x), ' must be provided'),
                 call = rlang::caller_env())
  }
  return(invisible(NULL))
}
