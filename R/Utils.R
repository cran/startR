indices <- function(x) {
  attr(x, 'indices') <- TRUE
  x
}

.ReplaceVariablesInString <- function(string, replace_values, allow_undefined_key_vars = FALSE) {
  # This function replaces all the occurrences of a variable in a string by 
  # their corresponding string stored in the replace_values.
  if (length(strsplit(string, "\\$")[[1]]) > 1) {
    parts <- strsplit(string, "\\$")[[1]]
    output <- ""
    i <- 0
    for (part in parts) {
      if (i %% 2 == 0) {
        output <- paste(output, part, sep = "")
      } else {
        if (part %in% names(replace_values)) {
          output <- paste(output, .ReplaceVariablesInString(replace_values[[part]], replace_values, allow_undefined_key_vars), sep = "")
        } else if (allow_undefined_key_vars) {
          output <- paste0(output, "$", part, "$")
        } else {
          stop(paste('Error: The variable $', part, '$ was not defined in the configuration file.', sep = ''))
        }
      }
      i <- i + 1
    }
    output
  } else {
    string
  }
}

.ReplaceGlobExpressions <- function(path_with_globs, actual_path, 
                                    replace_values, tags_to_keep, 
                                    dataset_name, permissive) {
  # The goal of this function is to replace the shell globbing expressions in
  # a path pattern (that may contain shell globbing expressions and Load() 
  # tags) by the corresponding part of the real existing path.
  # What is done actually is to replace all the values of the tags in the 
  # actual path by the corresponding $TAG$
  #
  # It takes mainly two inputs. The path with expressions and tags, e.g.:
  #   /data/experiments/*/$EXP_NAME$/$VAR_NAME$/$VAR_NAME$_*$START_DATE$*.nc
  # and a complete known path to one of the matching files, e.g.:
  #   /data/experiments/ecearth/i00k/tos/tos_fc0-1_19901101_199011-199110.nc
  # and it returns the path pattern but without shell globbing expressions:
  #   /data/experiments/ecearth/$EXP_NAME$/$VAR_NAME$/$VAR_NAME$_fc0-1_$START_DATE$_199011-199110.nc
  #
  # To do that, it needs also as inputs the list of replace values (the 
  # association of each tag to their value).
  #
  # All the tags not present in the parameter tags_to_keep will be repalced.
  #
  # Not all cases can be resolved with the implemented algorithm. In an
  # unsolvable case a warning is given and one possible guess is returned.
  #
  # In some cases it is interesting to replace only the expressions in the
  # path to the file, but not the ones in the file name itself. To keep the
  # expressions in the file name, the parameter permissive can be set to 
  # TRUE. To replace all the expressions it can be set to FALSE.
  clean <- function(x) {
    if (nchar(x) > 0) {
      x <- gsub('\\\\', '', x)
      x <- gsub('\\^', '', x)
      x <- gsub('\\$', '', x)
      x <- unname(sapply(strsplit(x, '[',fixed = TRUE)[[1]], function(y) gsub('.*]', '.', y)))
      do.call(paste0, as.list(x))
    } else {
      x
    }
  }

  strReverse <- function(x) sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")

  if (permissive) {
    actual_path_chunks <- strsplit(actual_path, '/')[[1]]
    actual_path <- paste(actual_path_chunks[-length(actual_path_chunks)], collapse = '/')
    file_name <- tail(actual_path_chunks, 1)
    if (length(actual_path_chunks) > 1) {
      file_name <- paste0('/', file_name)
    }
    path_with_globs_chunks <- strsplit(path_with_globs, '/')[[1]]
    path_with_globs <- paste(path_with_globs_chunks[-length(path_with_globs_chunks)], 
                             collapse = '/')
    path_with_globs <- .ReplaceVariablesInString(path_with_globs, replace_values)
    file_name_with_globs <- tail(path_with_globs_chunks, 1)
    if (length(path_with_globs_chunks) > 1) {
      file_name_with_globs <- paste0('/', file_name_with_globs)
    }
    right_known <- head(strsplit(file_name_with_globs, '*', fixed = TRUE)[[1]], 1)
    right_known_no_tags <- .ReplaceVariablesInString(right_known, replace_values)
    path_with_globs_rx <- utils::glob2rx(paste0(path_with_globs, right_known_no_tags))
    match <- regexpr(gsub('$', '', path_with_globs_rx, fixed = TRUE), paste0(actual_path, file_name))
    if (match != 1) {
      stop("Incorrect parameters to replace glob expressions. The path with expressions does not match the actual path.")
    }
    if (attr(match, 'match.length') - nchar(right_known_no_tags) < nchar(actual_path)) {
      path_with_globs <- paste0(path_with_globs, right_known_no_tags, '*')
      file_name_with_globs <- sub(right_known, '/*', file_name_with_globs)
    } 
  }
  path_with_globs_rx <- utils::glob2rx(path_with_globs)
  values_to_replace <- c()
  tags_to_replace_starts <- c()
  tags_to_replace_ends <- c()
  give_warning <- FALSE
  for (tag in tags_to_keep) {
    matches <- gregexpr(paste0('$', tag, '$'), path_with_globs_rx, fixed = TRUE)[[1]]
    lengths <- attr(matches, 'match.length')
    if (!(length(matches) == 1 && matches[1] == -1)) {
      for (i in 1:length(matches)) {
        left <- NULL
        if (matches[i] > 1) {
          left <- .ReplaceVariablesInString(substr(path_with_globs_rx, 1, matches[i] - 1), replace_values)
          left_known <- strReverse(head(strsplit(strReverse(left), strReverse('.*'), fixed = TRUE)[[1]], 1))
        }
        right <- NULL
        if ((matches[i] + lengths[i] - 1) < nchar(path_with_globs_rx)) {
          right <- .ReplaceVariablesInString(substr(path_with_globs_rx, matches[i] + lengths[i], nchar(path_with_globs_rx)), replace_values)
          right_known <- head(strsplit(right, '.*', fixed = TRUE)[[1]], 1)
        }
        final_match <- NULL
        match_limits <- NULL
        if (!is.null(left)) {
          left_match <- regexpr(paste0(left, replace_values[[tag]], right_known), actual_path)
          match_len <- attr(left_match, 'match.length')
          left_match_limits <- c(left_match + match_len - 1 - nchar(clean(right_known)) - nchar(replace_values[[tag]]) + 1, 
                                 left_match + match_len - 1 - nchar(clean(right_known)))
          if (!(left_match < 1)) {
            match_limits <- left_match_limits
          }
        }
        right_match <- NULL
        if (!is.null(right)) {
          right_match <- regexpr(paste0(left_known, replace_values[[tag]], right), actual_path)
          match_len <- attr(right_match, 'match.length')
          right_match_limits <- c(right_match + nchar(clean(left_known)),  
                                  right_match + nchar(clean(left_known)) + nchar(replace_values[[tag]]) - 1)
          if (is.null(match_limits) && !(right_match < 1)) {
            match_limits <- right_match_limits
          }
        }
        if (!is.null(right_match) && !is.null(left_match)) {
          if (!identical(right_match_limits, left_match_limits)) {
            give_warning <- TRUE
          }
        }
        if (is.null(match_limits)) {
          stop("Too complex path pattern specified for ", dataset_name,
               ". Specify a simpler path pattern for this dataset.")
        }
        values_to_replace <- c(values_to_replace, tag)
        tags_to_replace_starts <- c(tags_to_replace_starts, match_limits[1])
        tags_to_replace_ends <- c(tags_to_replace_ends, match_limits[2])
      }
    }
  }

  if (length(tags_to_replace_starts) > 0) {
    reorder <- sort(tags_to_replace_starts, index.return = TRUE)
    tags_to_replace_starts <- reorder$x
    values_to_replace <- values_to_replace[reorder$ix]
    tags_to_replace_ends <- tags_to_replace_ends[reorder$ix]
    while (length(values_to_replace) > 0) {
      actual_path <- paste0(substr(actual_path, 1, head(tags_to_replace_starts, 1) - 1),
                           '$', head(values_to_replace, 1), '$',
                           substr(actual_path, head(tags_to_replace_ends, 1) + 1, nchar(actual_path)))
      extra_chars <- nchar(head(values_to_replace, 1)) + 2 - (head(tags_to_replace_ends, 1) - head(tags_to_replace_starts, 1) + 1)
      values_to_replace <- values_to_replace[-1]
      tags_to_replace_starts <- tags_to_replace_starts[-1]
      tags_to_replace_ends <- tags_to_replace_ends[-1]
      tags_to_replace_starts <- tags_to_replace_starts + extra_chars
      tags_to_replace_ends <- tags_to_replace_ends + extra_chars
    }
  }

  if (give_warning) {
    .warning(paste0("Too complex path pattern specified for ", dataset_name, 
                    ". Double check carefully the '$Files' fetched for this dataset or specify a simpler path pattern."))
  }

  if (permissive) {
    paste0(actual_path, file_name_with_globs)
  } else {
    actual_path
  }
}

.FindTagValue <- function(path_with_globs_and_tag, actual_path, tag) {
  if (!all(sapply(c(path_with_globs_and_tag, actual_path, tag), is.character))) {
    stop("All 'path_with_globs_and_tag', 'actual_path' and 'tag' must be character strings.")
  }

  if (grepl('$', tag, fixed = TRUE)) {
    stop("The provided 'tag' must not contain '$' symbols.")
  }
  full_tag <- paste0('$', tag, '$')

  if (!grepl(full_tag, path_with_globs_and_tag, fixed = TRUE)) {
    stop("The provided 'path_with_globs_and_tag' must contain the tag in 'tag' surrounded by '$' symbols.")
  }

  parts <- strsplit(path_with_globs_and_tag, full_tag, fixed = TRUE)[[1]]
  if (length(parts) == 1) {
    parts <- c(parts, '')
  }
  parts[1] <- paste0('^', parts[1])
  parts[length(parts)] <- paste0(parts[length(parts)], '$')

  # Group the parts in 2 groups, in a way that both groups have a number
  # of characters as similar as possible.
  part_lengths <- sapply(parts, nchar)
  group_len_diffs <- sapply(1:(length(parts) - 1), 
    function(x) {
      sum(part_lengths[(x + 1):length(parts)]) - sum(part_lengths[1:x])
    }
  )
  clp <- chosen_left_part <- which.min(group_len_diffs)[1]

  left_expr <- paste(parts[1:clp], collapse = full_tag)
  left_expr <- gsub('?', '.', left_expr, fixed = TRUE)
  # The .*? will force lazy evaluation (find the shortest match from the 
  # beginning of the actual_path).
  left_expr <- gsub('*', '.*?', left_expr, fixed = TRUE)
  left_expr <- gsub(full_tag, '.*?', left_expr, fixed = TRUE)
  left_match <- regexec(left_expr, actual_path)[[1]]
  if (left_match < 0) {
    stop("Unexpected error in .FindTagValue.")
  }

  right_expr <- paste(parts[(clp + 1):(length(parts))], collapse = full_tag)
  right_expr <- gsub('?', '.', right_expr, fixed = TRUE)
  # For lazy evaulation to work, pattern and string have to be reversed.
  right_expr <- gsub('*', '.*?', right_expr, fixed = TRUE)
  right_expr <- gsub(full_tag, '.*?', right_expr, fixed = TRUE)
  right_expr <- gsub('$', '^', right_expr, fixed = TRUE)
  rev_str <- function(s) {
    paste(rev(strsplit(s, NULL)[[1]]), collapse = '')
  }
  right_expr <- rev_str(right_expr)
  right_expr <- gsub('?*.', '.*?', right_expr, fixed = TRUE)
  right_match <- regexec(right_expr, rev_str(actual_path))[[1]]
  if (right_match < 0) {
    stop("Unexpected error in .FindTagValue.")
  }
  right_match[] <- nchar(actual_path) - 
                   (right_match[] + attr(right_match, 'match.length') - 1) + 1

  if ((left_match + attr(left_match, 'match.length')) > 
      (right_match - 1)) {
    NULL
  } else {
    substr(actual_path, left_match + attr(left_match, 'match.length'),
                        right_match - 1)
  }
}

.message <- function(...) {
  # Function to use the 'message' R function with our custom settings
  # Default: new line at end of message, indent to 0, exdent to 3, 
  #  collapse to \n*
  args <- list(...)

  ## In case we need to specify message arguments
  if (!is.null(args[["appendLF"]])) {
    appendLF <- args[["appendLF"]]
  } else {
    ## Default value in message function
    appendLF <- TRUE
  } 
  if (!is.null(args[["domain"]])) {
    domain <- args[["domain"]]
  } else {
    ## Default value in message function
    domain <- NULL
  }
  args[["appendLF"]] <- NULL
  args[["domain"]] <- NULL

  ## To modify strwrap indent and exdent arguments
  if (!is.null(args[["indent"]])) {
    indent <- args[["indent"]]
  } else {
    indent <- 0
  }
  if (!is.null(args[["exdent"]])) {
    exdent <- args[["exdent"]]
  } else {
    exdent <- 3
  }
  args[["indent"]] <- NULL
  args[["exdent"]] <- NULL

  ## To modify paste collapse argument
  if (!is.null(args[["collapse"]])) {
    collapse <- args[["collapse"]]
  } else {
    collapse <- "\n*"
  }
  args[["collapse"]] <- NULL

  ## Message tag
  if (!is.null(args[["tag"]])) {
    tag <- args[["tag"]]
  } else {
    tag <- "* "
  }
  args[["tag"]] <- NULL

  message(paste0(tag, paste(strwrap(
    args, indent = indent, exdent = exdent
    ), collapse = collapse)), appendLF = appendLF, domain = domain)
}

.warning <- function(...) {
  # Function to use the 'warning' R function with our custom settings
  # Default: no call information, indent to 0, exdent to 3, 
  #  collapse to \n
  args <- list(...)

  ## In case we need to specify warning arguments
  if (!is.null(args[["call."]])) {
    call <- args[["call."]]
  } else {
    ## Default: don't show info about the call where the warning came up
    call <- FALSE
  }
  if (!is.null(args[["immediate."]])) {
    immediate <- args[["immediate."]]
  } else {
    ## Default value in warning function
    immediate <- FALSE
  }
  if (!is.null(args[["noBreaks."]])) {
    noBreaks <- args[["noBreaks."]]
  } else {
    ## Default value warning function
    noBreaks <- FALSE
  }
  if (!is.null(args[["domain"]])) {
    domain <- args[["domain"]]
  } else {
    ## Default value warning function
    domain <- NULL
  }
  args[["call."]] <- NULL
  args[["immediate."]] <- NULL
  args[["noBreaks."]] <- NULL
  args[["domain"]] <- NULL

  ## To modify strwrap indent and exdent arguments
  if (!is.null(args[["indent"]])) {
    indent <- args[["indent"]]
  } else {
    indent <- 0
  }
  if (!is.null(args[["exdent"]])) {
    exdent <- args[["exdent"]]
  } else {
    exdent <- 3
  }
  args[["indent"]] <- NULL
  args[["exdent"]] <- NULL

  ## To modify paste collapse argument
  if (!is.null(args[["collapse"]])) {
    collapse <- args[["collapse"]]
  } else {
    collapse <- "\n!"
  }
  args[["collapse"]] <- NULL

  ## Warning tag
  if (!is.null(args[["tag"]])) {
    tag <- args[["tag"]]
  } else {
    tag <- "! Warning: "
  }
  args[["tag"]] <- NULL

  warning(paste0(tag, paste(strwrap(
    args, indent = indent, exdent = exdent
    ), collapse = collapse)),  call. = call, immediate. = immediate, 
    noBreaks. = noBreaks, domain = domain)
}

# Function to permute arrays of non-atomic elements (e.g. POSIXct)
.aperm2 <- function(x, new_order) {
  y <- array(1:length(x), dim = dim(x))
  y <- aperm(y, new_order)
  old_dims <- dim(x)
  x <- x[as.vector(y)]
  dim(x) <- old_dims[new_order]
  x
}

# This function is a helper for the function .MergeArrays.
# It expects as inputs two named numeric vectors, and it extends them
# with dimensions of length 1 until an ordered common dimension
# format is reached.
.MergeArrayDims <- function(dims1, dims2) {
  new_dims1 <- c()
  new_dims2 <- c()
  while (length(dims1) > 0) {
    if (names(dims1)[1] %in% names(dims2)) {
      pos <- which(names(dims2) == names(dims1)[1])
      dims_to_add <- rep(1, pos - 1)
      if (length(dims_to_add) > 0) {
        names(dims_to_add) <- names(dims2[1:(pos - 1)])
      }
      new_dims1 <- c(new_dims1, dims_to_add, dims1[1])
      new_dims2 <- c(new_dims2, dims2[1:pos])
      dims1 <- dims1[-1]
      dims2 <- dims2[-c(1:pos)]
    } else {
      new_dims1 <- c(new_dims1, dims1[1])
      new_dims2 <- c(new_dims2, 1)
      names(new_dims2)[length(new_dims2)] <- names(dims1)[1]
      dims1 <- dims1[-1]
    }
  }
  if (length(dims2) > 0) {
    dims_to_add <- rep(1, length(dims2))
    names(dims_to_add) <- names(dims2)
    new_dims1 <- c(new_dims1, dims_to_add)
    new_dims2 <- c(new_dims2, dims2)
  }
  list(new_dims1, new_dims2)
}

# This function takes two named arrays and merges them, filling with
# NA where needed.
# dim(array1)
#          'b'   'c'         'e'   'f'
#           1     3           7     9
# dim(array2)
#    'a'   'b'         'd'         'f'   'g'
#     2     3           5           9     11
# dim(.MergeArrays(array1, array2, 'b'))
#    'a'   'b'   'c'   'e'   'd'   'f'   'g'
#     2     4     3     7     5     9     11
.MergeArrays <- function(array1, array2, along) {
  if (!(identical(names(dim(array1)), names(dim(array2))) &&
      identical(dim(array1)[-which(names(dim(array1)) == along)],
                dim(array2)[-which(names(dim(array2)) == along)]))) {
    new_dims <- .MergeArrayDims(dim(array1), dim(array2))
    dim(array1) <- new_dims[[1]]
    dim(array2) <- new_dims[[2]]
    for (j in 1:length(dim(array1))) {
      if (names(dim(array1))[j] != along) {
        if (dim(array1)[j] != dim(array2)[j]) {
          if (which.max(c(dim(array1)[j], dim(array2)[j])) == 1) {
            na_array_dims <- dim(array2)
            na_array_dims[j] <- dim(array1)[j] - dim(array2)[j]
            na_array <- array(dim = na_array_dims)
            array2 <- abind(array2, na_array, along = j)
            names(dim(array2)) <- names(na_array_dims)
          } else {
            na_array_dims <- dim(array1)
            na_array_dims[j] <- dim(array2)[j] - dim(array1)[j]
            na_array <- array(dim = na_array_dims)
            array1 <- abind(array1, na_array, along = j)
            names(dim(array1)) <- names(na_array_dims)
          }
        }
      }
    }
  }
  array1 <- abind(array1, array2, along = which(names(dim(array1)) == along))
  names(dim(array1)) <- names(dim(array2))
  array1
}
