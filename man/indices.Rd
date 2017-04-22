\name{indices}
\alias{indices}
\title{Mark Dimension Selectors as Indices}
\description{
Helper function intended for use in a call to the function \code{Start} in the package 'startR', to explicitly mark that a set of provided indices to subset one of the requested dimensions are actually indices and not values to be matched against a coordinate variable. See details in the documentation of the parameter \dots of the function \code{Start()}.
}
\usage{
indices(x)
}
\arguments{
  \item{x}{
Numeric vector or list with two numeric elements.
  }
}
\value{
The same as the input, but with an additional attribute 'indices' with the value TRUE, marking the indices as numeric indices.
}
\author{
See details in the documentation of the parameter \code{transform} of the function \code{Start()}.
}
\examples{
## Check https://earth.bsc.es/gitlab/es/startR for step-by-step examples 
## of Start() that use indices().
}
\keyword{IO}
\keyword{array}
\keyword{manip}
