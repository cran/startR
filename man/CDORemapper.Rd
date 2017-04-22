\name{CDORemapper}
\alias{CDORemapper}
\title{CDO Remap Data Transformation for 'startR'}
\description{
This is a transform function that uses CDO software to remap longitude-latitude data subsets onto a specified target grid, intended for use as parameter \code{transform} in a call to the function \code{Start()} in the package 'startR'. This function complies with the input/output interface required by \code{Start()} defined in the documentation for the parameter \code{transform} of that function.
\cr\cr
This function uses the function \code{CDORemap()} in the package 's2dverification' to perform the interpolations and hence requires having CDO installed in the machine.
}
\usage{
CDORemapper(data_array, variables, file_selectors, ...)
}
\arguments{
  \item{data_array}{
Input data array to be transformed. See details in the documentation of the parameter \code{transform} of the function \code{Start()}.
  }
  \item{variables}{
Auxiliary variables required for the transformation, automatically provided by \code{Start()}. See details in the documentation of the parameter \code{transform} of the function \code{Start()}.
  }
  \item{file_selectors}{
Information on the path of the file the input data array comes from. See details in the documentation of the parameter \code{transform} of the function \code{Start()}.
  }
  \item{\dots}{
Additional parameters to adjust the transform process, as provided in the parameter \code{transform_params} in a call to the function \code{Start()}. See details in the documentation of the parameter \code{transform} of the function \code{Start()}.
  }
}
\value{
An array with the same amount of dimensions as the input data array, potentially with different sizes, and potentially with the attribute 'variables' with additional auxiliary data. See details in the documentation of the parameter \code{transform} of the function \code{Start()}.
}
\author{
History:\cr
0.0  -  2017-04  (N. Manubens, \email{nicolau.manubens at bsc.es})  -  Original code
}
\seealso{
\code{\link[s2dverification]{CDORemap}}
}
\examples{
## Check https://earth.bsc.es/gitlab/es/startR for step-by-step examples 
## of Start() that use CDORemapper().
}
\keyword{array}
\keyword{manip}
