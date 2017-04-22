\name{CircularSort}
\alias{CircularSort}
\title{Circular Sort Dimension Reorder for 'startR'}
\description{
This is a function that generates a reorder function intended for use as parameter \code{<dim_name>_reorder} in a call to the function \code{Start()} in the package 'startR'. This function complies with the input/output interface required by \code{Start()} defined in the documentation for the parameter \dots of that function.
\cr\cr
The applied reordering consists of a circular sort of the coordinate variable values, where any values beyond the limits specified in the parameters \code{start} and \code{end} is applied a modulus to fall in the specified range. This is useful for circular dimensions such as the Earth longitudes.
}
\usage{
CircularSort(start, end, ...)
}
\arguments{
  \item{start}{
Numeric lower bound of the circular range.
  }
  \item{end}{
Numeric upper bound of the circular range.
  }
  \item{\dots}{
Additional parameters to adjust the reorderig (sent internally to the function \code{sort()}).
  }
}
\value{
List with the reordered values in the component \code{$x} and the permutation indices in the component \code{$ix}. See details in the documentation of the parameter \dots of the function \code{Start()}.
}
\author{
History:\cr
0.0  -  2017-04  (N. Manubens, \email{nicolau.manubens at bsc.es})  -  Original code
}
\seealso{
\code{\link[startR]{Sort}}
}
\examples{
## Check https://earth.bsc.es/gitlab/es/startR for step-by-step examples 
## of Start() that use CircularSort().
}
\keyword{array}
\keyword{manip}
