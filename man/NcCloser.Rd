\name{NcCloser}
\alias{NcCloser}
\title{NetCDF File Closer for 'startR'}
\description{
This is a file closer function for NetCDF files, intended for use as parameter \code{file_closer} in a call to the function \code{Start()} in the package 'startR'. This function complies with the input/output interface required by \code{Start()} defined in the documentation for the parameter \code{file_closer} of that function.
\cr\cr
This function uses the function \code{NcClose()} in the package 'easyNCDF', which in turn uses \code{nc_close()} in the package 'ncdf4'.
}
\usage{
NcCloser(file_object)
}
\arguments{
  \item{file_object}{
Open connection to a NetCDF file, optionally with additional header information. See details in the documentation of the parameter \code{file_closer} of the function \code{Start()}.
  }
}
\value{
This function returns \code{NULL}.
}
\author{
History:\cr
0.0  -  2017-04  (N. Manubens, \email{nicolau.manubens at bsc.es})  -  Original code
}
\seealso{
\code{\link{NcOpener}}, \code{\link{NcDimReader}}, \code{\link{NcDataReader}}, \code{\link{NcVarReader}} 
}
\examples{
## Check https://earth.bsc.es/gitlab/es/startR for step-by-step examples 
## of Start() that use NcCloser().
}
\keyword{IO}
\keyword{array}
\keyword{manip}
