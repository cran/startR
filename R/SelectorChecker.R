SelectorChecker <- function(selectors, var = NULL, return_indices = TRUE,
                            tolerance = NULL) {
  if (length(selectors) == 0) {
    stop("No selectors provided in 'selectors'.")
  }
  if (return_indices) {
    if (is.list(selectors)) {
      if (length(selectors) != 2) {
        stop("'selectors' provided in a wrong format.")
      }
      for (i in 1:length(selectors)) {
        if (is.null(var)) {
          if (!is.numeric(selectors[[i]])) {
            stop("No selector values provided in 'var'.")
          } else {
            selectors[[i]] <- round(selectors[[i]])
          }
        } else if (is.na(selectors[[i]])) {
          if (i == 1) {
            selectors[[i]] <- 1
          } else {
            selectors[[i]] <- length(var)
          }
        } else if (is.character(selectors[[i]])) {
          if (is.character(var)) {
            candidate <- which(var == selectors[[i]])
            if (length(candidate) > 0) {
              selectors[[i]] <- candidate[1]
            } else {
              stop("Selector value not found in 'var'.")
            }
          } else {
            stop("Character selectors provided but possible values in 'var' are not character.")
          }
        } else if (is.numeric(selectors[[i]])) {
          if (is.numeric(var)) {
            #if (sum(var == selectors[[i]], na.rm = TRUE) > 0) {
            #  selectors[[i]] <- which(var == selectors[[i]])[1]
            #} else {
            #  .warning(paste0("A numeric selector has been ",
            #           "provided for a dimension defined along a ",
            #           "numeric variable, but no exact match ",
            #           "found. Taking the index of the nearest ",
            #           "value."))
            if (i == 1) {
              candidates <- which(var >= selectors[[i]])
            } else {
              candidates <- which(var <= selectors[[i]])
            }
            selectors[[i]] <- candidates[which.min(abs(var[candidates] - selectors[[i]]))]
            #}
          } else {
            stop("Numeric selectors provided but possible values in 'var' are not numeric.")
          }
        }
      }
      # The checker is returning a list of two indices.
      ##selectors[[1]]:selectors[[2]]
      selectors
    } else if (is.numeric(selectors)) {
      if (is.null(var)) {
        ## TODO: Crash if negative indices?
        round(selectors)
      } else {
        if (is.numeric(var)) {
          if (!all(selectors %in% var)) {
            .warning(paste0("Numeric selectors have been ",
                     "provided for a dimension defined along a ",
                     "numeric variable, but no exact match ",
                     "found for all the selectors. Taking the index of the ",
                     "nearest values."))
          }
          if (!is.null(tolerance)) {
            if (!any(class(tolerance) %in% 'numeric')) {
              stop("Expected a numeric *_tolerance.")
            }
          }
          sapply(selectors, function(x) {
                              dif <- abs(var - x)
                              res <- which.min(dif)[1]
                              if (!is.null(tolerance)) {
                                if (dif[res] > tolerance) {
                                  stop("Could not find a value in 'var' close ",
                                       "enough to one of the 'selectors', ",
                                       "according to 'tolerance'.")
                                }
                              }
                              res
                            })
        } else {
          stop("Numeric selectors provided but possible values in 'var' are not numeric.")
        }
      }
    } else if (any(c('POSIXct', 'POSIXlt', 'POSIXt', 'Date') %in% class(selectors))) {
      if (is.null(var)) {
        stop("Numeric selectors have been provided for a dimension ",
             "defined along a date variable, but no possible values ",
             "provided in 'var'.")
      }
      if (!all(selectors %in% var)) {
        .warning(paste0("Date selectors have been ",
                 "provided for a dimension defined along a ",
                 "date variable, but no exact match ",
                 "found for all the selectors. Taking the index of the ",
                 "nearest values."))
      }
      if (!is.null(tolerance)) {
        if (!any(class(tolerance) %in% 'difftime')) {
          stop("Expected a difftime *_tolerance.")
        }
      }
      sapply(selectors, function(x) {
                          dif <- abs(var - x)
                          res <- which.min(dif)[1]
                          if (!is.null(tolerance)) {
                            if (dif[res] > tolerance) {
                              stop("Could not find a value in 'var' close ",
                                   "enough to one of the 'selectors', ",
                                   "according to 'tolerance'.")
                            }
                          }
                          res
                        })
    } else {
      if (is.null(var)) {
        stop("No selector values provided in 'var'.")
      } else {
        if ((length(selectors) == 1) && 
            (selectors %in% c('all', 'first', 'last'))) {
          if (selectors == 'all') {
            1:length(var)
          } else if (selectors == 'first') {
            1
          } else {
            length(var)
          }
        } else {
          if (!identical(class(var), class(selectors))) {
            stop("Class of provided selectors does not match class of 'var'.")
          }
          candidates <- which(as.vector(var) == as.vector(selectors))
          if (length(candidates) == 0) {
            stop("Selectors do not match values in 'var'.")
          } else if (length(candidates) != length(selectors)) {
            stop("Some selectors do not match values in 'var'.")
          }
          candidates
        }
      }
    }
  } else {
    if (!is.null(var)) {
      if (is.list(selectors)) {
        if (length(selectors != 2)) {
          stop("'selectors' provided in a wrong format.")
        } else {
          var[selectors[[1]]:selectors[[2]]]
        }
      } else if (is.numeric(selectors)) {
        if (length(selectors) > 0) {
          var[selectors]
        } else {
          stop("No selectors provided.")
        }
      } else {
        if ((length(selectors) == 1) && 
            (selectors %in% c('all', 'first', 'last'))) {
          if (selectors == 'all') {
            var
          } else if (selectors == 'first') {
            head(var, 1)
          } else {
            tail(var, 1)
          }
        } else {
          selectors
        }
      }
    } else {
      selectors
    }
  }
}
