NcVarReader <- function(file_path = NULL, file_object = NULL, 
                        file_selectors = NULL, var_name = NULL,
                        synonims) {
  if (!is.null(file_object)) {
    file_to_read <- file_object
  } else if (!is.null(file_path)) {
    file_to_read <- file_path
  } else {
    stop("Either 'file_path' or 'file_object' must be provided.")
  }
  if (var_name %in% c('var_names')) {
    vars_in_file <- easyNCDF::NcReadVarNames(file_to_read)
    vars_in_file <- sapply(vars_in_file,
      function(x) {
        which_entry <- which(sapply(synonims, function(y) x %in% y))
        if (length(which_entry) > 0) {
          names(synonims)[which_entry]
        } else {
          x
        }
      })
    vars_in_file
  } else {
    NcDataReader(file_path, file_object, list(var = var_name), NULL, synonims)
  }
}
