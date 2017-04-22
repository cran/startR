# Parameter 'file_selectors' expects a named character vector of single 
# file dimension selectors.
# Parameter 'inner_indices' expects a named list of numeric or 
# character string vectors.
NcDimReader <- function(file_path = NULL, file_object = NULL, 
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

  vars_in_file <- easyNCDF::NcReadVarNames(file_to_read)
  if (any(c('var', 'variable') %in% names(inner_indices))) {
    vars_to_read <- inner_indices[[which(names(inner_indices) %in% c('var', 'variable'))[1]]]
    var_tag <- names(inner_indices)[[which(names(inner_indices) %in% c('var', 'variable'))[1]]]
  } else if (any(c('var', 'variable') %in% names(file_selectors))) {
    vars_to_read <- file_selectors[[which(names(file_selectors) %in% c('var', 'variable'))[1]]]
    var_tag <- names(file_selectors)[[which(names(file_selectors) %in% c('var', 'variable'))[1]]]
  } else if (length(vars_in_file) == 1) {
    vars_to_read <- vars_in_file
    file_selectors <- c(file_selectors, list(var = vars_in_file))
    var_tag <- 'var'
  } else {
    stop("NcDimReader expected to find a requested 'var' or 'variable' in 'file_selectors'.")
  }

  if ((length(vars_to_read) == 1) && (vars_to_read[1] == 'var_names')) {
    setNames(length(vars_in_file), var_tag)
  } else {
    vars_to_read <- sapply(vars_to_read, 
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
          if (is.character(x) && !(x %in% c('all', 'last', 'first'))) {
            if (!(x %in% vars_in_file)) {
              stop("Could not find variable '", x, "' (or its synonims if ",
                   "specified) in the file ", file_path)
            }
          }
          x
        }
      })
    vars_to_read <- SelectorChecker(vars_to_read, vars_in_file, 
                                    return_indices = FALSE)
    read_dims <- easyNCDF::NcReadDims(file_to_read, vars_to_read)
    if (any(c('var', 'variable') %in% names(inner_indices))) {
      names(read_dims)[which(names(read_dims) == 'var')] <- var_tag
      read_dims[var_tag] <- length(vars_in_file)
    } else {
      read_dims <- read_dims[-which(names(read_dims) == 'var')]
    }
    read_dims
  }
}
