\name{Sort}
\alias{Sort}
\title{Sort Dimension Reorder for 'startR'}
\description{
This is a reorder function intended for use as parameter \code{<dim_name>_reorder} in a call to the function \code{Start()} in the package 'startR'. This function complies with the input/output interface required by \code{Start()} defined in the documentation for the parameter \dots of that function.
\cr\cr
The applied reordering consists of an increasing sort of the coordinate variable values.
}
\usage{
Sort(...)
}
\arguments{
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
\code{\link[startR]{CircularSort}}
}
\examples{
## Check https://earth.bsc.es/gitlab/es/startR for step-by-step examples 
## of Start() that use Sort().
}
\keyword{IO}
\keyword{array}
\keyword{manip}
