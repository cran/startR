\name{NcDimReader}
\alias{NcDimReader}
\title{NetCDF Dimension Reader for 'startR'}
\description{
This is a dimension reader function for NetCDF files, intended for use as parameter \code{file_dim_reader} in a call to the function \code{Start()} in the package 'startR'. This function complies with the input/output interface required by \code{Start()} defined in the documentation for the parameter \code{file_dim_reader} of that function.
\cr\cr
This function uses the function \code{NcReadDims()} in the package 'easyNCDF'.
}
\usage{
NcDimReader(file_path = NULL, file_object = NULL, file_selectors = NULL, 
            inner_indices = NULL, synonims)
}
\arguments{
  \item{file_path}{
Character string with the path to the data file to read the dimensions for. See details in the documentation of the parameter \code{file_dim_reader} of the function \code{Start()}.
  }
  \item{file_object}{
Open connection to a NetCDF file, optionally with additional header information. See details in the documentation of the parameter \code{file_dim_reader} of the function \code{Start()}.
  }
  \item{file_selectors}{
Information on the path of the file to read data from. See details in the documentation of the parameter \code{file_dim_reader} of the function \code{Start()}.
  }
  \item{inner_indices}{
Named list with the numeric indices to take from each of the inner dimensions in the requested file. Used only in advanced configurations. See details in the documentation of the parameter \code{file_dim_reader} of the function \code{Start()}.
  }
  \item{synonims}{
Named list with synonims for the dimension names to look for in the requested file, exactly as provided in the parameter \code{synonims} in a call to the function \code{Start()}. See details in the documentation of the parameter \code{file_dim_reader} of the function \code{Start()}.
  }
}
\value{
Named numeric vector with the names and sizes of the dimensions of the requested file.
}
\author{
History:\cr
0.0  -  2017-04  (N. Manubens, \email{nicolau.manubens at bsc.es})  -  Original code
}
\seealso{
\code{\link{NcOpener}}, \code{\link{NcCloser}}, \code{\link{NcDataReader}}, \code{\link{NcVarReader}}
}
\examples{
## Check https://earth.bsc.es/gitlab/es/startR for step-by-step examples 
## of Start() that use NcDimReader().
}
\keyword{IO}
\keyword{array}
\keyword{manip}
