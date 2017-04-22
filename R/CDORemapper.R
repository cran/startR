CDORemapper <- function(data_array, variables, file_selectors = NULL, ...) {
  file_dims <- names(file_selectors)
  known_lon_names <- s2dverification:::.KnownLonNames()
  known_lat_names <- s2dverification:::.KnownLatNames()
  if (!any(known_lon_names %in% names(variables)) ||
      !any(known_lat_names %in% names(variables))) {
    stop("The longitude and latitude variables must be requested in ",
         "'return_vars' and specified in 'transform_vars' for the ",
         "CDORemapper to work.")
  }
  lon_name <- names(variables)[which(names(variables) %in% known_lon_names)[1]]
  lons <- variables[[lon_name]]
  if (!is.null(dim(lons))) {
    dims_to_subset <- which(names(dim(lons)) %in% file_dims)
    if (length(dims_to_subset) > 0) {
      lons_to_use <- as.list(rep(TRUE, length(dim(lons))))
      names(lons_to_use) <- names(dim(lons))
      lons_to_use[dims_to_subset] <- as.list(rep(1, length(dims_to_subset)))
      attr_bk <- attributes(lons)
      lons <- do.call('[', c(list(x = lons), lons_to_use, list(drop = TRUE)))
      attributes(lons) <- attr_bk
    }
  }
  lat_name <- names(variables)[which(names(variables) %in% known_lat_names)[1]]
  lats <- variables[[lat_name]]
  if (!is.null(dim(lats))) {
    dims_to_subset <- which(names(dim(lats)) %in% file_dims)
    if (length(dims_to_subset) > 0) {
      lats_to_use <- as.list(rep(TRUE, length(dim(lats))))
      names(lats_to_use) <- names(dim(lats))
      lats_to_use[dims_to_subset] <- as.list(rep(1, length(dims_to_subset)))
      attr_bk <- attributes(lons)
      lats <- do.call('[', c(list(x = lats), lats_to_use, list(drop = TRUE)))
      attributes(lats) <- attr_bk
    }
  }
  extra_params <- list(...)
  if (!all(c('grid', 'method') %in% names(extra_params))) {
    stop("Parameters 'grid' and 'method' must be specified for the ",
         "CDORemapper, via the 'transform_params' argument.")
  }
  result <- s2dverification::CDORemap(data_array, lons, lats, ...)
  return_variables <- list(result$lons, result$lats)
  names(return_variables) <- c(lon_name, lat_name)
  list(data_array = result$data_array, variables = return_variables)
}
