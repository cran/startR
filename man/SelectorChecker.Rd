\name{SelectorChecker}
\alias{SelectorChecker}
\title{Default Selector Checker for 'startR'}
\description{
This is a selector checker function intended for use as parameter \code{selector_checker} in a call to the function \code{Start()} in the package 'startR'. This function complies with the input/output interface required by \code{Start()} defined in the documentation for the parameter \code{selector_checker} of that function.
}
\usage{
SelectorChecker(selectors, var = NULL, return_indices = TRUE, tolerance = NULL)
}
\arguments{
  \item{selectors}{
Numeric indices or variable values to be retrieved for a dimension, automatically provided by \code{Start()}. See details in the documentation of the parameters \code{selector_checker} and \dots of the function \code{Start()}. The indices or values can be provided in the form of a vector or in the form of a list with two elements. 
  }
  \item{var}{
Vector of values of a coordinate variable where to search matches with the provided indices or values in the parameter \code{selectors}, automatically provided by \code{Start()}. See details in the documentation of the parameters \code{selector_checker} and \dots of the function \code{Start()}. The parameter \code{var} is optional. When not specified, \code{SelectorChecker} simply returns the input indices.
  }
  \item{return_indices}{
Boolean flag, automatically configured by \code{Start()}, telling whether to return numeric indices or coordinate variable values after doing the matching.
  }
  \item{tolerance}{
Numeric value with a tolerance value to be used in the matching of the \code{selectors} and \code{var}. See documentation on \code{<dim_name>_tolerance} in \dots, in the documentation of the function \code{Start()}.
  }
}
\value{
A vector of either the indices of the matching values (if \code{return_indices = TRUE}) or the matching values themselves (if \code{return_indices = FALSE}).
}
\author{
History:\cr
0.0  -  2017-04  (N. Manubens, \email{nicolau.manubens at bsc.es})  -  Original code
}
\examples{
## Check https://earth.bsc.es/gitlab/es/startR for step-by-step examples 
## of Start() that use SelectorChecker().
}
\keyword{IO}
\keyword{array}
\keyword{manip}
