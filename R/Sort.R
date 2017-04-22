Sort <- function(...) {
  r <- function(x) {
    dim_bk <- dim(x)
    x <- sort(x, index.return = TRUE, ...)
    dim(x$x) <- dim_bk
    dim(x$ix) <- dim_bk
    x
  }
  attr(r, 'circular') <- FALSE
  r
}

CircularSort <- function(start, end, ...) {
  r <- function (x) {
    dim_bk <- dim(x)
    x <- sort((x - start) %% (end - start) + start, index.return = TRUE, ...)
    dim(x$x) <- dim_bk
    dim(x$ix) <- dim_bk
    x
  }
  attr(r, 'circular') <- TRUE
  r
}
