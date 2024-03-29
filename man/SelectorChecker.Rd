% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SelectorChecker.R
\name{SelectorChecker}
\alias{SelectorChecker}
\title{Translate a set of selectors into a set of numeric indices}
\usage{
SelectorChecker(selectors, var = NULL, return_indices = TRUE, tolerance = NULL)
}
\arguments{
\item{selectors}{A vector or a list of two of numeric indices or variable 
values to be retrieved for a dimension, automatically provided by Start(). 
See details in the documentation of the parameters 'selector_checker' and 
'\dots' of the function Start().}

\item{var}{A vector of values of a coordinate variable for which to search 
matches with the provided indices or values in the parameter 'selectors', 
automatically provided by Start(). See details in the documentation of the 
parameters 'selector_checker' and '\dots' of the function Start(). The 
default value is NULL. When not specified, SelectorChecker() simply returns
the input indices.}

\item{return_indices}{A logical value automatically configured by Start(), 
telling whether to return the numeric indices or coordinate variable values 
after the matching. The default value is TRUE.}

\item{tolerance}{A numeric value indicating a tolerance value to be used in 
the matching of 'selectors' and 'var'. See documentation on 
'<dim_name>_tolerance' in \code{\dots} in the documentation of the function
Start(). The default value is NULL.}
}
\value{
A vector of either the indices of the matching values (if 
 return_indices = TRUE) or the matching values themselves (if return_indices
 = FALSE).
}
\description{
This is a selector checker function intended for use as parameter 
'selector_checker' in a Start() call. It translates a set of selectors which
is the value for one dimension into a set of numeric indices corresponding to
the coordinate variable. The function complies with the input/output interface 
required by Start() defined in the documentation for the parameter 
'selector_checker' of Start().
}
\examples{
# Get the latitudes from 10 to 20 degree
sub_array_of_selectors <- list(10, 20)
# The latitude values from original file
sub_array_of_values <- seq(90, -90, length.out = 258)[2:257]
SelectorChecker(sub_array_of_selectors, sub_array_of_values)

}
