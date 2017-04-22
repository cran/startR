# Parameter 'file_selectos' expects a named character vector of single
# file dimension selectors.
# Parameter 'inner_indices' expects a named list of numeric vectors.
NcDataReader <- function(file_path = NULL, file_object = NULL, 
                         file_selectors = NULL, inner_indices = NULL,
                         synonims) {
  if (!is.null(file_object)) {
    file_to_read <- file_object
    file_path <- file_object$filename
  } else if (!is.null(file_path)) {
    file_to_read <- NcOpener(file_path)
  } else {
    stop("Either 'file_path' or 'file_object' must be provided.")
  }

  if (is.null(file_to_read)) {
    return(NULL)
  }

  var_requested <- is.null(inner_indices)

  drop_var_dim <- FALSE
  if (any(c('var', 'variable') %in% names(file_selectors))) {
    if (!any(c('var', 'variable') %in% names(inner_indices))) {
      inner_indices <- c(inner_indices,
                         list(var = file_selectors[[which(names(file_selectors) %in% 
                                                    c('var', 'variable'))[1]]]))
      drop_var_dim <- TRUE
    }
  }

  vars_in_file <- easyNCDF::NcReadVarNames(file_to_read)
  if (any(names(inner_indices) %in% c('var', 'variable'))) {
    position_of_var <- which(names(inner_indices) %in% c('var', 'variable'))[1]
  } else if (length(vars_in_file) == 1) {
    inner_indices <- c(inner_indices,
                       list(var = vars_in_file))
    drop_var_dim <- TRUE
    position_of_var <- length(inner_indices)
  } else {
    stop("A 'var'/'variable' file dimension or inner dimension must be ",
         "requested for NcDataReader() to read NetCDF files.")
  }

  inner_indices[[position_of_var]] <- sapply(inner_indices[[position_of_var]],
    function(x) {
      if (x %in% names(synonims)) {
        x_in_file <- which(synonims[[x]] %in% vars_in_file)
        if (length(x_in_file) < 1) {
          stop("Could not find variable '", x, "' (or its synonims if ",
               "specified) in the file ", file_path)
        }
        if (length(x_in_file) > 1) {
          stop("Found more than one matches for the synonims of the ",
               "variable '", x, "' in the file ", file_path)
        }
        synonims[[x]][x_in_file]
      } else {
        if (is.character(x) && !(x %in% c('all', 'first', 'last'))) {
          if (!(x %in% vars_in_file)) {
            stop("Could not find variable '", x, "' (or its synonims if ",
                 "specified) in the file ", file_path)
          }
        }
        x
      }
    })
  #inner_indices[[position_of_var]] <- SelectorChecker(inner_indices[[position_of_var]], vars_in_file)
  dims_in_file <- NcDimReader(NULL, file_to_read, NULL, 
                              inner_indices[position_of_var], synonims)
  names(inner_indices) <- sapply(names(inner_indices), 
    function(x) {
      if (x %in% names(synonims)) {
        x_in_file <- which(synonims[[x]] %in% names(dims_in_file))
        if (length(x_in_file) < 1) {
          stop("Could not find dimension '", x, "' (or its synonims if ",
               "specified) in the file ", file_path)
        }
        if (length(x_in_file) > 1) {
          stop("Found more than one matches for the synonims of the ",
               "dimension '", x, "' in the file ", file_path)
        }
        synonims[[x]][x_in_file]
      } else {
        if (!(x %in% names(dims_in_file))) {
          stop("Could not find dimension '", x, "' (or its synonims if ",
               "specified) in the file ", file_path)
        }
        x
      }
    })
  if (drop_var_dim) {
    dims_in_file <- dims_in_file[-which(names(dims_in_file) %in% c('var', 'variable'))]
  }
  singleton_unspecified_dims <- which((dims_in_file == 1) & 
                                      !(names(dims_in_file) %in% names(inner_indices)))
  if (length(singleton_unspecified_dims) > 0) {
    dims_in_file <- dims_in_file[-singleton_unspecified_dims]
  }
  if (var_requested) {
    result <- easyNCDF::NcToArray(file_to_read, inner_indices, drop_var_dim = drop_var_dim,
                                  expect_all_indices = FALSE, allow_out_of_range = TRUE)
  } else {
    if (any(!(names(dims_in_file) %in% names(inner_indices)))) {
      expected_dim_names <- names(inner_indices)
      if (drop_var_dim) {
        expected_dim_names <- expected_dim_names[-position_of_var]
      }
      stop("Unexpected extra dimensions (of length > 1) in the file.\nExpected: ",
           paste(expected_dim_names, collapse = ', '), "\n",
           "Found: ", paste(names(dims_in_file), collapse = ', '), "\n",
           file_path)
    }
    result <- easyNCDF::NcToArray(file_to_read, inner_indices, drop_var_dim = drop_var_dim,
                                  expect_all_indices = TRUE, allow_out_of_range = TRUE)
  }
  names(dim(result)) <- sapply(names(dim(result)),
    function(x) {
      which_entry <- which(sapply(synonims, function(y) x %in% y))
      if (length(which_entry) > 0) {
        names(synonims)[which_entry]
      } else {
        x
      }
    })
  names(attr(result, 'variables')) <- sapply(names(attr(result, 'variables')),
    function(x) {
      which_entry <- which(sapply(synonims, function(y) x %in% y))
      if (length(which_entry) > 0) {
        names(synonims)[which_entry]
      } else {
        x
      }
    })
  if (length(names(attr(result, 'variables'))) == 1) {
    var_name <- names(attr(result, 'variables'))
    units <- attr(result, 'variables')[[var_name]][['units']]
    if (units %in% c('seconds', 'minutes', 'hours', 'days', 'weeks', 'months', 'years')) {
      if (units == 'seconds') {
        units <- 'secs'
      } else if (units == 'minutes') {
        units <- 'mins'
      }
      result[] <- paste(result[], units)
    } else if (grepl(' since ', units)) {
      parts <- strsplit(units, ' since ')[[1]]
      units <- parts[1]
      if (units == 'seconds') {
        units <- 'secs'
      } else if (units == 'minutes') {
        units <- 'mins'
      }
      new_array <- seq(as.POSIXct(parts[2]), length = max(result, na.rm = TRUE) + 1, by = units)[result[] + 1]
      dim(new_array) <- dim(result)
      attr(new_array, 'variables') <- attr(result, 'variables')
      result <- new_array
    }
  }
  result
}
