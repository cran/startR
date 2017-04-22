\name{NcVarReader}
\alias{NcVarReader}
\title{NetCDF Variable Reader for 'startR'}
\description{
This is an auxiliary variable reader function for NetCDF files, intended for use as parameter \code{file_var_reader} in a call to the function \code{Start()} in the package 'startR'. This function complies with the input/output interface required by \code{Start()} defined in the documentation for the parameter \code{file_var_reader} of that function.
\cr\cr
This function uses the function \code{NcDataReader()} in the package 'startR', which in turn uses \code{NcToArray()} in the package 'easyNCDF', which in turn uses \code{nc_var_get()} in the package 'ncdf4'.
}
\usage{
NcVarReader(file_path = NULL, file_object = NULL, file_selectors = NULL, 
            var_name = NULL, synonims)
}
\arguments{
  \item{file_path}{
Character string with the path to the data file to read the variable from. See details in the documentation of the parameter \code{file_var_reader} of the function \code{Start()}.
  }
  \item{file_object}{
Open connection to a NetCDF file, optionally with additional header information. See details in the documentation of the parameter \code{file_var_reader} of the function \code{Start()}.
  }
  \item{file_selectors}{
Information on the path of the file to read data from. See details in the documentation of the parameter \code{file_var_reader} of the function \code{Start()}.
  }
  \item{var_name}{
Character string with the name of the variable to be read.
  }
  \item{synonims}{
Named list with synonims for the variable names to look for in the requested file, exactly as provided in the parameter \code{synonims} in a call to the function \code{Start()}. See details in the documentation of the parameter \code{file_var_reader} of the function \code{Start()}.
  }
}
\value{
A multidimensional data array with the named dimensions, potentially with the attribute 'variables' with additional auxiliary data. See details in the documentation of the parameter \code{file_var_reader} of the function \code{Start()}.
}
\author{
History:\cr
0.0  -  2017-04  (N. Manubens, \email{nicolau.manubens at bsc.es})  -  Original code
}
\seealso{
\code{\link{NcOpener}}, \code{\link{NcCloser}}, \code{\link{NcDataReader}}, \code{\link{NcDimReader}}
}
\examples{
## Check https://earth.bsc.es/gitlab/es/startR for step-by-step examples 
## of Start() that use NcVarReader().
}
\keyword{IO}
\keyword{array}
\keyword{manip}
