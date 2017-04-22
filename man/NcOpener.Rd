\name{NcOpener}
\alias{NcOpener}
\title{NetCDF File Opener for 'startR'}
\description{
This is a file closer function for NetCDF files, intended for use as parameter \code{file_opener} in a call to the function \code{Start()} in the package 'startR'. This function complies with the input/output interface required by \code{Start()} defined in the documentation for the parameter \code{file_opener} of that function.
\cr\cr
This function uses the function \code{NcOpen()} in the package 'easyNCDF', which in turn uses \code{nc_open()} in the package 'ncdf4'.
}
\usage{
NcOpener(file_path)
}
\arguments{
  \item{file_path}{
Character string with the path to the data file to read. See details in the documentation of the parameter \code{file_opener} of the function \code{Start()}.
  }
}
\value{
An open connection to a NetCDF file, with additional header information, as returned by \code{nc_open} in the package 'ncdf4'. See details in the documentation of the parameter \code{file_opener} of the function \code{Start()}.
}
\author{
History:\cr
0.0  -  2017-04  (N. Manubens, \email{nicolau.manubens at bsc.es})  -  Original code
}
\seealso{
\code{\link{NcCloser}}, \code{\link{NcDataReader}}, \code{\link{NcDimReader}}, \code{\link{NcVarReader}}
}
\examples{
## Check https://earth.bsc.es/gitlab/es/startR for step-by-step examples 
## of Start() that use NcOpener().
}
\keyword{IO}
\keyword{array}
\keyword{manip}
