\name{Start}
\alias{Start}
\title{Automatically Retrieve Multidimensional Distributed Data Sets}
\description{
See the \href{https://earth.bsc.es/gitlab/es/startR}{\code{startR} documentation and tutorial} for a step-by-step explanation on how to use \code{Start()}.
\cr\cr
Data retrieval and alignment is the first step in data analysis in any field and is often highly complex and time-consuming, especially nowadays in the era of Big Data, where large multidimensional data sets from diverse sources need to be combined and processed. Taking subsets of these datasets (Divide) to then be processed efficiently (and Conquer) becomes an indispensable technique.
\cr\cr
\code{startR} (Subset, Transform, Arrange and ReTrieve multidimensional subsets in R) is an R project started at BSC with the aim to develop a tool that allows the user to automatically retrieve, homogenize and align subsets of multidimensional distributed data sets. It is an open source project that is open to external collaboration and funding, and will continuously evolve to support as many data set formats as possible while maximizing its efficiency.
\cr\cr
\code{startR}, through its main function \code{Start()}, provides an interface that allows to perceive and access one or a collection of data sets as if they all were part of a large multidimensional array. Indices or bounds can be specified for each of the dimensions in order to crop the whole array into a smaller sub-array. \code{Start()} will perform the required operations to fetch the corresponding regions of the corresponding files (potentially distributed over various remote servers) and arrange them into a local R multidimensional array. By default, as many cores as available locally are used in this procedure.
\cr\cr
Usually, in retrieval processes previous to multidimensional data analysis, it is needed to apply a set of common transformations, pre-processes or reorderings to the data as it comes in. \code{Start()} accepts user-defined transformation or reordering functions to be applied for such purposes.
\cr\cr
\code{Start()} is not bound to a specific file format. Interface functions to custom file formats can be provided for \code{Start()} to read them. As of April 2017 \code{startR} includes interface functions to the following file formats:
\cr
\itemize{
  \item{
NetCDF
  }
}
Metadata and auxilliary data is also preserved and arranged by \code{Start()} in the measure that it is retrieved by the interface functions for a specific file format.
}
\usage{
Start(..., 
      return_vars = NULL, 
      synonims = NULL, 
      file_opener = NcOpener, 
      file_var_reader = NcVarReader, 
      file_dim_reader = NcDimReader, 
      file_data_reader = NcDataReader, 
      file_closer = NcCloser, 
      transform = NULL, 
      transform_params = NULL, 
      transform_vars = NULL, 
      transform_extra_cells = 0, 
      apply_indices_after_transform = FALSE, 
      pattern_dims = NULL, 
      metadata_dims = NULL, 
      selector_checker = SelectorChecker, 
      num_procs = NULL, 
      silent = FALSE, 
      debug = FALSE)
}
\arguments{
  \item{\dots}{
When willing to retrieve data from one or a collection of data sets, the involved data can be perceived as belonging to a large multi-dimensional array. For instance, let us consider an example case. We want to retrieve data from a source, which contains data for the number of monthly sales of various items, and also for their retail price each month. The data on source is stored as follows:
\cr
\command{
\cr #  /data/
\cr #    |-> sales/
\cr #    |    |-> electronics
\cr #    |    |    |-> item_a.data
\cr #    |    |    |-> item_b.data
\cr #    |    |    |-> item_c.data
\cr #    |    |-> clothing
\cr #    |         |-> item_d.data
\cr #    |         |-> idem_e.data
\cr #    |         |-> idem_f.data
\cr #    |-> prices/
\cr #         |-> electronics
\cr #         |    |-> item_a.data
\cr #         |    |-> item_b.data
\cr #         |    |-> item_c.data
\cr #         |-> clothing
\cr #              |-> item_d.data
\cr #              |-> item_e.data
\cr #              |-> item_f.data
}
\cr\cr
Each item file contains data, stored in whichever format, for the sales or prices over a time period, e.g. for the past 24 months, registered at 100 different stores over the world. Whichever the format it is stored in, each file can be perceived as a container of a data array of 2 dimensions, time and store. Let us assume the '.data' format allows to keep a name for each of these dimensions, and the actual names are 'time' and 'store'.
\cr\cr
The different item files for sales or prices can be perceived as belonging to an 'item' dimension of length 3, and the two groups of three items to a 'section' dimension of length 2, and the two groups of two sections (one with the sales and the other with the prices) can be perceived as belonging also to another dimension 'variable' of length 2. Even the source can be perceived as belonging to a dimension 'source' of length 1.
\cr\cr
All in all, in this example, the whole data could be perceived as belonging to a multidimensional 'large array' of dimensions
\cr
\command{
\cr #  source variable  section      item    store    month
\cr #       1        2        2         3      100       24
}
\cr\cr
The dimensions of this 'large array' can be classified in two types. The ones that group actual files (the file dimensions) and the ones that group data values inside the files (the inner dimensions). In the example, the file dimensions are 'source', 'variable', 'section' and 'item', whereas the inner dimensions are 'store' and 'month'.
\cr\cr
Having the dimensions of our target sources in mind, the parameter \dots expects to receive information on:
    \itemize{
      \item{
The names of the expected dimensions of the 'large dataset' we want to retrieve data from
      }
      \item{
The indices to take from each dimension (and other constraints)
      }
      \item{
How to reorder the dimension if needed
      }
      \item{
The location and organization of the files of the data sets
      }
    }
For each dimension, the 3 first information items can be specified with a set of parameters to be provided through \dots. For a given dimension 'dimname', six parameters can be specified:
\cr
\command{
\cr # dimname = <indices_to_take>,  # 'all' / 'first' / 'last' /
\cr #                               # indices(c(1, 10, 20)) / 
\cr #                               # indices(c(1:20)) /
\cr #                               # indices(list(1, 20)) /
\cr #                               # c(1, 10, 20) / c(1:20) /
\cr #                               # list(1, 20)
\cr # dimname_var = <name_of_associated_coordinate_variable>,
\cr # dimname_tolerance = <tolerance_value>,
\cr # dimname_reorder = <reorder_function>,
\cr # dimname_depends = <name_of_another_dimension>,
\cr # dimname_across = <name_of_another_dimension>
}
\cr\cr
The \bold{indices to take} can be specified in three possible formats (see code comments above for examples). The first format consists in using character tags, such as 'all' (take all the indices available for that dimension), 'first' (take only the first) and 'last' (only the last). The second format consists in using numeric indices, which have to be wrapped in a call to the \code{indices()} helper function. For the second format, either a vector of numeric indices can be provided, or a list with two numeric indices can be provided to take all the indices in the range between the two specified indices (both extremes inclusive). The third format consists in providing a vector character strings (for file dimensions) or of values of whichever type (for inner dimensions). For the file dimensions, the provided character strings in the third format will be used as components to build up the final path to the files (read further). For inner dimensions, the provided values in the third format will be compared to the values of an associated coordinate variable (must be specified in \code{dimname_reorder}, read further), and the indices of the closest values will be retrieved. When using the third format, a list with two values can also be provided to take all the indices of the values within the specified range.
\cr\cr
The \bold{name of the associated coordinate variable} must be a character string with the name of an associated coordinate variable to be found in the data files (in all* of them). For this to work, a \code{file_var_reader} function must be specified when calling \code{Start()} (see parameter 'file_var_reader'). The coordinate variable must also be requested in the parameter \code{return_vars} (see its section for details). This feature only works for inner dimensions.
\cr\cr
The \bold{tolerance value} is useful when indices for an inner dimension are specified in the third format (values of whichever type). In that case, the indices of the closest values in the coordinate variable are seeked. However the closest value might be too distant and we would want to consider no real match exists for such provided value. This is possible via the tolerance. which allows to specify a threshold beyond which not to seek for matching values and mark that index as missing value.
\cr\cr
The \bold{reorder_function} is useful when indices for an inner dimension are specified in the third fromat, and the retrieved indices need to be reordered in function of their provided associated variable values. A function can be provided, which receives as input a vector of values, and returns as outputs a list with the components \code{x} with the reordered values, and \code{ix} with the permutation indices. Two reordering functions are included in \code{startR}, the \code{Sort()} and the \code{CircularSort()}.
\cr\cr
The \bold{name of another dimension} to be specified in \code{dimname_depends}, only available for file dimensions, must be a character string with the name of another requested \bold{file dimension} in \dots, and will make \code{Start()} aware that the path components of a file dimension can vary in function of the path component of another file dimension. For instance, in the example above, specifying \code{item_depends = 'section'} will make \code{Start()} aware that the item names vary in function of the section, i.e. section 'electronics' has items 'a', 'b' and 'c' but section 'clothing' has items 'd', 'e', 'f'. Otherwise \code{Start()} would expect to find the same item names in all the sections.
\cr\cr
The \bold{name of another dimension} to be specified in \code{dimname_across}, only available for inner dimensions, must be a character string with the name of another requested \bold{inner dimension} in \dots, and will make \code{Start()} aware that an inner dimension extends along multiple files. For instance, let us imagine that in the example above, the records for each item are so large that it becomes necessary to split them in multiple files each one containing the registers for a different period of time, e.g. in 10 files with 100 months each ('item_a_period1.data', 'item_a_period2.data', and so on). In that case, the data can be perceived as having an extra file dimension, the 'period' dimension. The inner dimension 'month' would extend across multiple files, and providing the parameter \code{month = indices(1, 300)} would make \code{Start()} crash because it would perceive we have made a request out of bounds (each file contains 100 'month' indices, but we requested 1 to 300). This can be solved by specifying the parameter \code{month_across = period} (along with the full specification of the dimension 'period').
\cr\cr
\bold{Defining the path pattern}
\cr
As mentioned above, the parameter \dots also expects to receive information with the location of the data files. In order to do this, a special dimension must be defined. In that special dimension, in place of specifying indices to take, a path pattern must be provided. The path pattern is a character string that encodes the way the files are organized in their source. It must be a path to one of the data set files in an accessible local or remote file system, or a URL to one of the files provided by a local or remote server. The regions of this path that vary across files (along the file dimensions) must be replaced by wildcards. The wildcards must match any of the defined file dimensions in the call to \code{Start()} and must be delimited with heading and trailing '$'. Shell globbing expressions can be used in the path pattern. See the next code snippet for an example of a path pattern.
\cr\cr
All in all, the call to \code{Start()} to load the entire data set in the example of store item sales, would look as follows:
\cr
\command{
\cr # data <- Start(source = paste0('/data/$variable$/',
\cr #                               '$section$/$item$.data'),
\cr #               variable = 'all',
\cr #               section = 'all',
\cr #               item = 'all',
\cr #               item_depends = 'section',
\cr #               store = 'all',
\cr #               month = 'all')
}
\cr\cr
Note that in this example it would still be pending to properly define the parameters \code{file_opener}, \code{file_closer}, \code{file_dim_reader}, \code{file_var_reader} and \code{file_data_reader} for the '.data' file format (see the corresponding sections).
\cr\cr
The call to \code{Start()} will return a multidimensional R array with the following dimensions:
\cr
\command{
\cr #  source variable  section      item    store    month
\cr #       1        2        2         3      100       24
}
\cr
The dimension specifications in the \dots do not have to follow any particular order. The returned array will have the dimensions in the same order as they have been specified in the call. For example, the following call:
\cr
\command{
\cr # data <- Start(source = paste0('/data/$variable$/',
\cr #                               '$section$/$item$.data'),
\cr #               month = 'all',
\cr #               store = 'all',
\cr #               item = 'all',
\cr #               item_depends = 'section',
\cr #               section = 'all',
\cr #               variable = 'all')
}
\cr\cr
would return an array with the following dimensions:
\cr
\command{
\cr #  source    month    store      item  section variable
\cr #       1       24      100         3        2        2
}
\cr\cr
Next, a more advanced example to retrieve data for only the sales records, for the first section ('electronics'), for the 1st and 3rd items and for the stores located in Barcelona (assuming the files contain the variable 'store_location' with the name of the city each of the 100 stores are located at):
\cr
\command{
\cr # data <- Start(source = paste0('/data/$variable$/',
\cr #                               '$section$/$item$.data'),
\cr #               variable = 'sales',
\cr #               section = 'first',
\cr #               item = indices(c(1, 3)),
\cr #               item_depends = 'section',
\cr #               store = 'Barcelona',
\cr #               store_var = 'store_location',
\cr #               month = 'all',
\cr #               return_vars = list(store_location = NULL))
}
\cr\cr
The defined names for the dimensions do not necessarily have to match the names of the dimensions inside the file. Lists of alternative names to be seeked can be defined in the parameter \code{synonims}.
\cr\cr
If data from multiple sources (not necessarily following the same structure) has to be retrieved, it can be done by providing a vector of character strings with path pattern specifications, or, in the extended form, by providing a list of lists with the components 'name' and 'path', and the name of the dataset and path pattern as values, respectively. For example:
\cr
\command{
\cr # data <- Start(source = list(
\cr #                 list(name = 'sourceA', 
\cr #                      path = paste0('/sourceA/$variable$/',
\cr #                                    '$section$/$item$.data')),
\cr #                 list(name = 'sourceB', 
\cr #                      path = paste0('/sourceB/$section$/',
\cr #                                    '$variable$/$item$.data'))
\cr #               ),
\cr #               variable = 'sales',
\cr #               section = 'first',
\cr #               item = indices(c(1, 3)),
\cr #               item_depends = 'section',
\cr #               store = 'Barcelona',
\cr #               store_var = 'store_location',
\cr #               month = 'all',
\cr #               return_vars = list(store_location = NULL))
}
\cr
  }
  \item{return_vars}{
Apart from retrieving a multidimensional data array, retrieving auxiliary variables inside the files can also be needed. The parameter \code{return_vars} allows for requesting such variables, as long as a \code{file_var_reader} function is also specified in the call to \code{Start()} (see documentation on the corresponding parameter). 
\cr\cr
This parameter expects to receive a named list where the names are the names of the variables to be fetched in the files, and the values are vectors of character strings with the names of the file dimension which to retrieve each variable for, or \code{NULL} if the variable has to be retrieved only once from any (the first) of the involved files. In the case of the the item sales example (see documentation on parameter \dots), the store location variable is requested with the parameter \cr \code{return_vars = list(store_location = NULL)}. This will cause \code{Start()} to fetch once the variable 'store_location' and return it in the component \cr \code{$Variables$common$store_location}, and will be an array of character strings with the location names, with the dimensions \code{c('store' = 100)}. Although useless in this example, we could ask \code{Start()} to fetch and return such variable for each file along the items dimension as follows: \cr \code{return_vars = list(store_location = c('item'))}. In that case, the variable will be fetched once from a file of each of the items, and will be returned as an array with the dimensions \code{c('item' = 3, 'store' = 100)}.
\cr\cr
If a variable is requested along a file dimension that contains path pattern specifications ('source' in the example), the fetched variable values will be returned in the component \code{$Variables$<dataset_name>$<variable_name>}. For example:
\cr
\command{
\cr # data <- Start(source = list(
\cr #                 list(name = 'sourceA', 
\cr #                      path = paste0('/sourceA/$variable$/',
\cr #                                    '$section$/$item$.data')),
\cr #                 list(name = 'sourceB', 
\cr #                      path = paste0('/sourceB/$section$/',
\cr #                                    '$variable$/$item$.data'))
\cr #               ),
\cr #               variable = 'sales',
\cr #               section = 'first',
\cr #               item = indices(c(1, 3)),
\cr #               item_depends = 'section',
\cr #               store = 'Barcelona',
\cr #               store_var = 'store_location',
\cr #               month = 'all',
\cr #               return_vars = list(store_location = c('source', 
\cr #                                                     'item')))
\cr # # Checking the structure of the returned variables
\cr # str(found_data$Variables)
\cr # Named list
\cr # ..$common: NULL
\cr # ..$sourceA: Named list
\cr # .. ..$store_location: char[1:18(3d)] 'Barcelona' 'Barcelona' ...
\cr # ..$sourceB: Named list
\cr # .. ..$store_location: char[1:18(3d)] 'Barcelona' 'Barcelona' ...
\cr # # Checking the dimensions of the returned variable 
\cr # # for the source A
\cr # dim(found_data$Variables$sourceA)
\cr #     item   store
\cr #        3       3
}
\cr\cr
The names of the requested variables do not necessarily have to match the actual variable names inside the files. A list of alternative names to be seeked can be specified via the parameter \code{synonims}.
  }
  \item{synonims}{
In some requests, data from different sources may follow different naming conventions for the dimensions or variables, or even files in the same source could have varying names. In order for \code{Start()} to properly identify the dimensions or variables with different names, the parameter \code{synonims} can be specified as a named list where the names are requested variable or dimension names, and the values are vectors of character strings with alternative names to seek for such dimension or variable.
\cr\cr
In the example used in parameter \code{return_vars}, it may be the case that the two involved data sources follow slightly different naming conventions. For example, source A uses 'sect' as name for the sections dimension, whereas source B uses 'section'; source A uses 'store_loc' as variable name for the store locations, whereas source B uses 'store_location'. This can be taken into account as follows:
\cr
\command{
\cr # data <- Start(source = list(
\cr #                 list(name = 'sourceA', 
\cr #                      path = paste0('/sourceA/$variable$/',
\cr #                                    '$section$/$item$.data')),
\cr #                 list(name = 'sourceB', 
\cr #                      path = paste0('/sourceB/$section$/',
\cr #                                    '$variable$/$item$.data'))
\cr #               ),
\cr #               variable = 'sales',
\cr #               section = 'first',
\cr #               item = indices(c(1, 3)),
\cr #               item_depends = 'section',
\cr #               store = 'Barcelona',
\cr #               store_var = 'store_location',
\cr #               month = 'all',
\cr #               return_vars = list(store_location = c('source', 
\cr #                                                     'item')),
\cr #               synonims = list(
\cr #                 section = c('sec', 'section'),
\cr #                 store_location = c('store_loc', 
\cr #                                    'store_location')
\cr #               ))
}
\cr
  }
  \item{file_opener}{
A function that receives as a single parameter (\code{file_path}) a character string with the path to a file to be opened, and returns an object with an open connection to the file (optionally with header information) on success, or returns \code{NULL} on failure.
\cr\cr
This parameter takes by default \code{NcOpener} (an opener function for NetCDF files).
\cr\cr
See \code{NcOpener} for a template to build a file opener for your own file format.
  }
  \item{file_var_reader}{
A function with the header \code{file_path = NULL}, \code{file_object = NULL}, \code{file_selectors = NULL}, \code{var_name}, \code{synonims} that returns an array with auxiliary data (i.e. data from a variable) inside a file. \code{Start()} will provide automatically either a \code{file_path} or a \code{file_object} to the \code{file_var_reader} function (the function has to be ready to work whichever of these two is provided). The parameter \code{file_selectors} will also be provided automatically to the variable reader, containing a named list where the names are the names of the file dimensions of the queried data set (see documentation on \dots) and the values are single character strings with the components used to build the path to the file being read (the one provided in \code{file_path} or \code{file_object}). The parameter \code{var_name} will be filled in automatically by \code{Start()} also, with the name of one of the variales to be read. The parameter \code{synonims} will be filled in with exactly the same value as provided in the parameter \code{synonims} in the call to \code{Start()}, and has to be used in the code of the variable reader to check for alternative variable names inside the target file. The \code{file_var_reader} must return a (multi)dimensional array with named dimensions, and optionally with the attribute 'variales' with other additional metadata on the retrieved variable.
\cr\cr
Usually, the \code{file_var_reader} should be a degenerate case of the \code{file_data_reader} (see documentation on the corresponding parameter), so it is recommended to code the \code{file_data_reder} in first place.
\cr\cr
This parameter takes by default \code{NcVarReader} (a variable reader function for NetCDF files).
\cr\cr
See \code{NcVarReader} for a template to build a variale reader for your own file format.
  }
  \item{file_dim_reader}{
A function with the header \code{file_path = NULL}, \code{file_object = NULL}, \code{file_selectors = NULL}, \code{synonims} that returns a named numeric vector where the names are the names of the dimensions of the multidimensional data array in the file and the values are the sizes of such dimensions. \code{Start()} will provide automatically either a \code{file_path} or a \code{file_object} to the \code{file_dim_reader} function (the function has to be ready to work whichever of these two is provided). The parameter \code{file_selectors} will also be provided automatically to the dimension reader, containing a named list where the names are the names of the file dimensions of the queried data set (see documentation on \dots) and the values are single character strings with the components used to build the path to the file being read (the one provided in \code{file_path} or \code{file_object}). The parameter \code{synonims} will be filled in with exactly the same value as provided in the parameter \code{synonims} in the call to \code{Start()}, and can optionally be used in advanced configurations.
\cr\cr
This parameter takes by default \code{NcDimReader} (a dimension reader function for NetCDF files).
\cr\cr
See \code{NcDimReader} for a(n advanced) template to build a dimension reader for your own file format.
  }
  \item{file_data_reader}{
A function with the header \code{file_path = NULL}, \code{file_object = NULL}, \code{file_selectors = NULL}, \code{inner_indices = NULL}, \code{synonims} that returns a subset of the multidimensional data array inside a file (even if internally it is not an array). \code{Start()} will provide automatically either a \code{file_path} or a \code{file_object} to the \code{file_data_reader} function (the function has to be ready to work whichever of these two is provided). The parameter \code{file_selectors} will also be provided automatically to the data reader, containing a named list where the names are the names of the file dimensions of the queried data set (see documentation on \dots) and the values are single character strings with the components used to build the path to the file being read (the one provided in \code{file_path} or \code{file_object}). The parameter \code{inner_indices} will be filled in automatically by \code{Start()} also, with a named list of numeric vectors, where the names are the names of all the expected inner dimensions in a file to be read, and the numeric vectors are the indices to be taken from the corresponding dimension (the indices may not be consecutive nor in order). The parameter \code{synonims} will be filled in with exactly the same value as provided in the parameter \code{synonims} in the call to \code{Start()}, and has to be used in the code of the data reader to check for alternative dimension names inside the target file. The \code{file_data_reader} must return a (multi)dimensional array with named dimensions, and optionally with the attribute 'variales' with other additional metadata on the retrieved data.
\cr\cr
Usually, the \code{file_data_reader} should use the \code{file_dim_reader} (see documentation on the corresponding parameter), so it is recommended to code the \code{file_dim_reder} in first place.
\cr\cr
This parameter takes by default \code{NcDataReader} (a data reader function for NetCDF files).
\cr\cr
See \code{NcDataReader} for a template to build a data reader for your own file format.
  }
  \item{file_closer}{
A function that receives as a single parameter (\code{file_object}) an open connection (as returned by \code{file_opener}) to one of the files to be read, optionally with header information, and closes the open connection. Always returns \code{NULL}.
\cr\cr
This parameter takes by default \code{NcCloser} (a closer function for NetCDF files).
\cr\cr
See \code{NcCloser} for a template to build a file closer for your own file format.
  }
  \item{transform}{
A function with the header \code{dara_array}, \code{variables}, \code{file_selectors = NULL}, \code{\dots}. It receives as input, through the parameter \code{data_array}, a subset of a multidimensional array (as returned by \code{file_data_reader}), applies a transformation to it and returns it, preserving the amount of dimensions but potentially modifying their size. This transformation may require data from other auxiliary variables, automatically provided to \code{transform} through the parameter \code{variables}, in the form of a named list where the names are the variable names and the values are (multi)dimensional arrays. Which variables need to be sent to \code{transform} can be specified with the parameter \code{transform_vars} in \code{Start()}. The parameter \code{file_selectors} will also be provided automatically to \code{transform}, containing a named list where the names are the names of the file dimensions of the queried data set (see documentation on \dots) and the values are single character strings with the components used to build the path to the file the subset being processed belongs to. The parameter \dots will be filled in with other additional parameters to adjust the transformation, exactly as provided in the call to \code{Start()} via the parameter \code{transform_params}.
  }
  \item{transform_params}{
Named list with additional parameters to be sent to the \code{transform} function (if specified). See documentation on \code{transform} for details.
  }
  \item{transform_vars}{
Vector of character strings with the names of auxiliary variables to be sent to the \code{transform} function (if specified). All the variables to be sent to \code{transform} must also have been requested as return variables in the parameter \code{return_vars} of \code{Start()}.
  }
  \item{transform_extra_cells}{
Number of extra indices to retrieve from the data set, beyond the requested indices in \dots, in order for \code{transform} to dispose of additional information to properly apply whichever transformation (if needed). As many as \code{transform_extra_cells} will be retrieved beyond each of the limits for each of those inner dimensions associated to a coordinate variable and sent to \code{transform} (i.e. present in \code{transform_vars}). After \code{transform} has finished, \code{Start()} will take again and return a subset of the result, for the returned data to fall within the specified bounds in \dots. 
  }
  \item{apply_indices_after_transform}{
When a \code{transform} is specified in \code{Start()} and numeric indices are provided for any of the inner dimensions that depend on coordinate variables, these numeric indices can be made effective (retrieved) before applying the transformation or after. The boolean flab \code{apply_indices_after_transform} allows to adjust this behaviour. It takes \code{FALSE} by default (numeric indices are applied before sending data to \code{transform}).
  }
  \item{pattern_dims}{
Name of the dimension with path pattern specifications (see \dots for details). If not specified, \code{Start()} assumes the first provided dimension is the pattern dimension, with a warning.
  }
  \item{metadata_dims}{
It expects to receive a vector of character strings with the names of the file dimensions which to return metadata for. As noted in \code{file_data_reader}, the data reader can optionally return auxiliary data via the attribute 'variables' of the returned array. \code{Start()} by default returns the auxiliary data read for only the first file of each source (or data set) in the pattern dimension (see \dots for info on what the pattern dimension is). However it can be configured to return the metadata for all the files along any set of file dimensions. The parameter \code{metadata_dims} allows to configure this level of granularity of the returned metadata. 
  }
  \item{selector_checker}{
Function used internaly by \code{Start()} to translate a set of selectors (values for a dimension associated to a coordinate variable) into a set of numeric indices. It takes by default \code{SelectorChecker} and, in principle, it should not be required to change it for customized file formats. The option to replace it is left open for more versatility. See the code of \code{SelectorChecker} for details on the inputs, functioning and outputs of a selector checker. 
  }
  \item{num_procs}{
Number of processes to be created for the parallel execution of the retrieval / transformation / arrangement of the multiple involved files in a call to \code{Start()}. Takes by default the number of available cores (as detected by \code{detectCores()} in the package 'future').
  }
  \item{silent}{
Boolean flag, whether to display progress messages (FALSE; default) or not (TRUE).
  }
  \item{debug}{
Whether to return detailed messages on the progress and operations in a \code{Start()} call (TRUE) or not (FALSE; default).
  }
}
\details{
Check \href{https://earth.bsc.es/gitlab/es/startR}{the startR website} for more information.
}
\value{
  \item{Data}{
Multidimensional data array with named dimensions, with the data values requested via \dots and other parameters. This array can potentially contain metadata in the attribute 'variables'.
  }
  \item{Variales}{
Named list of 1 + N components, containing lists of retrieved variables (as requested in \code{return_vars}) common to all the data sources (in the 1st component, \code{$common}), and for each of the N dara sources (named after the source name, as specified in \dots, or, if not specified, \code{$dat1}, \code{$dat2}, ..., \code{$datN}). Each of the variables are contained in a multidimensional array with named dimensions, and potentially with the attribute 'variables' with additional auxiliary data.
  }
  \item{Files}{
Multidimensonal character string array with named dimensions. Its dimensions are the file dimensions (as requested in \dots). Each cell in this array contains a path to a retrieved file, or \code{NULL} if the corresponding file was not found.
  }
  \item{NotFoundFiles}{
Array with the same shape as \code{$Files} but with \code{NULL} in the positions for which the corresponding file was found, and a path to the expected file in the positions for which the corresponding file was not found.
  }
  \item{FileSelectors}{
Multidimensional character string array with named dimensions, with the same shape as \code{$Files} and \code{$NotFoundFiles}, which contains the components used to build up the paths to each of the files in the data sources.
  }
}
\author{
History:\cr
0.0  -  2017-04  (N. Manubens, \email{nicolau.manubens at bsc.es})  -  Original code
}
\examples{
## Check https://earth.bsc.es/gitlab/es/startR for step-by-step examples 
## of Start().
}
\keyword{IO}
\keyword{array}
\keyword{manip}
