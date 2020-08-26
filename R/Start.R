#'Declare, discover, subset and retrieve multidimensional distributed data sets
#'
#'See the \href{https://earth.bsc.es/gitlab/es/startR}{startR documentation and
#'tutorial} for a step-by-step explanation on how to use Start().\cr\cr
#'Nowadays in the era of big data, large multidimensional data sets from 
#'diverse sources need to be combined and processed. Analysis of big data in any
#'field is often highly complex and time-consuming. Taking subsets of these data
#'sets and processing them efficiently become an indispensable practice. This 
#'technique is also known as Domain Decomposition, Map Reduce or, more commonly,
#''chunking'.\cr\cr
#'startR (Subset, TrAnsform, ReTrieve, arrange and process large 
#'multidimensional data sets in R) is an R project started at BSC with the aim 
#'to develop a tool that allows the user to automatically process large 
#'multidimensional distributed data sets. It is an open source project that is 
#'open to external collaboration and funding, and will continuously evolve to 
#'support as many data set formats as possible while maximizing its efficiency.\cr\cr
#'startR provides a framework under which a data set (collection of one 
#'or multiple data files, potentially distributed over various remote servers) 
#'are perceived as if they all were part of a single large multidimensional 
#'array. Once such multidimensional array is declared, any user-defined function
#'can be applied to the data in a \code{apply}-like fashion, where startR
#'transparently implements the Map Reduce paradigm. The steps to follow in order
#'to process a collection of big data sets are as follows:\cr
#'\itemize{
#'  \item{
#'Declaring the data set, i.e. declaring the distribution of the data files 
#'involved, the dimensions and shape of the multidimensional array, and the 
#'boundaries of the target data. This step can be performed with the 
#'Start() function. Numeric indices or coordinate values can be used when
#'fixing the boundaries. It is common having the need to apply transformations, 
#'pre-processing or reordering to the data. Start() accepts user-defined 
#'transformation or reordering functions to be applied for such purposes. Once a
#'data set is declared, a list of involved files, dimension lengths, memory size
#'and other metadata is made available. Optionally, the data set can be 
#'retrieved and loaded onto the current R session if it is small enough. 
#'  }
#'  \item{
#'Declaring the workflow of operations to perform on the involved data set(s).
#'This step can be performed with the Step() and AddStep() functions.
#'  }
#'  \item{
#'Defining the computation settings. The mandatory settings include a) how many
#'subsets to divide the data sets into and along which dimensions; b) which 
#'platform to perform the workflow of operations on (local machine or remote 
#'machine/HPC?), how to communicate with it (unidirectional or bidirectional 
#'connection? shared or separate file systems?), which queuing system it uses 
#'(slurm, PBS, LSF, none?); and c) how many parallel jobs and execution threads
#'per job to use when running the calculations. This step can be performed when 
#'building up the call to the Compute() function.
#'  }
#'  \item{
#'Running the computation. startR transparently implements the Map Reduce 
#'paradigm, according to the settings in the previous steps. The progress can 
#'optionally be monitored with the EC-Flow workflow management tool. When the 
#'computation ends, a report of performance timings is displayed. This step can 
#'be triggered with the Compute() function.
#'  }
#'}
#'startR is not bound to a specific file format. Interface functions to
#'custom file formats can be provided for Start() to read them. As this
#'version, startR includes interface functions to the following file formats:
#'\itemize{
#'  \item{
#'NetCDF
#'  }
#'}
#'Metadata and auxilliary data is also preserved and arranged by Start()
#'in the measure that it is retrieved by the interface functions for a specific 
#'file format.
#'
#'@param \dots A selection of custemized parameters depending on the data 
#'format. When we retrieve data from one or a collection of data sets, 
#'the involved data can be perceived as belonging to a large multi-dimensional 
#'array. For instance, let us consider an example case. We want to retrieve data
#'from a source, which contains data for the number of monthly sales of various 
#'items, and also for their retail price each month. The data on source is 
#'stored as follows:\cr\cr
#'\command{
#'\cr #  /data/
#'\cr #    |-> sales/
#'\cr #    |    |-> electronics
#'\cr #    |    |    |-> item_a.data
#'\cr #    |    |    |-> item_b.data
#'\cr #    |    |    |-> item_c.data
#'\cr #    |    |-> clothing
#'\cr #    |         |-> item_d.data
#'\cr #    |         |-> idem_e.data
#'\cr #    |         |-> idem_f.data
#'\cr #    |-> prices/
#'\cr #         |-> electronics
#'\cr #         |    |-> item_a.data
#'\cr #         |    |-> item_b.data
#'\cr #         |    |-> item_c.data
#'\cr #         |-> clothing
#'\cr #              |-> item_d.data
#'\cr #              |-> item_e.data
#'\cr #              |-> item_f.data
#'}\cr\cr
#'Each item file contains data, stored in whichever format, for the sales or 
#'prices over a time period, e.g. for the past 24 months, registered at 100 
#'different stores over the world. Whichever the format it is stored in, each 
#'file can be perceived as a container of a data array of 2 dimensions, time and
#'store. Let us assume the '.data' format allows to keep a name for each of 
#'these dimensions, and the actual names are 'time' and 'store'.\cr\cr
#'The different item files for sales or prices can be perceived as belonging to 
#'an 'item' dimension of length 3, and the two groups of three items to a 
#''section' dimension of length 2, and the two groups of two sections (one with
#'the sales and the other with the prices) can be perceived as belonging also to
#'another dimension 'variable' of length 2. Even the source can be perceived as 
#'belonging to a dimension 'source' of length 1.\cr\cr
#'All in all, in this example, the whole data could be perceived as belonging to
#'a multidimensional 'large array' of dimensions\cr
#'\command{
#'\cr #  source variable  section      item    store    month
#'\cr #       1        2        2         3      100       24
#'}
#'\cr\cr
#'The dimensions of this 'large array' can be classified in two types. The ones 
#'that group actual files (the file dimensions) and the ones that group data 
#'values inside the files (the inner dimensions). In the example, the file 
#'dimensions are 'source', 'variable', 'section' and 'item', whereas the inner 
#'dimensions are 'store' and 'month'.
#'\cr\cr
#'Having the dimensions of our target sources in mind, the parameter \code{\dots} 
#'expects to receive information on:
#'    \itemize{
#'      \item{
#'The names of the expected dimensions of the 'large dataset' we want to 
#'retrieve data from
#'      }
#'      \item{
#'The indices to take from each dimension (and other constraints)
#'      }
#'      \item{
#'How to reorder the dimension if needed
#'      }
#'      \item{
#'The location and organization of the files of the data sets
#'      }
#'    }
#'For each dimension, the 3 first information items can be specified with a set
#'of parameters to be provided through \code{\dots}. For a given dimension 
#''dimname', six parameters can be specified:\cr
#'\command{
#'\cr # dimname = <indices_to_take>,  # 'all' / 'first' / 'last' /
#'\cr #                               # indices(c(1, 10, 20)) /
#'\cr #                               # indices(c(1:20)) /
#'\cr #                               # indices(list(1, 20)) /
#'\cr #                               # c(1, 10, 20) / c(1:20) /
#'\cr #                               # list(1, 20)
#'\cr # dimname_var = <name_of_associated_coordinate_variable>,
#'\cr # dimname_tolerance = <tolerance_value>,
#'\cr # dimname_reorder = <reorder_function>,
#'\cr # dimname_depends = <name_of_another_dimension>,
#'\cr # dimname_across = <name_of_another_dimension>
#'}
#'\cr\cr
#'The \bold{indices to take} can be specified in three possible formats (see 
#'code comments above for examples). The first format consists in using 
#'character tags, such as 'all' (take all the indices available for that 
#'dimension), 'first' (take only the first) and 'last' (only the last). The 
#'second format consists in using numeric indices, which have to be wrapped in a
#'call to the indices() helper function. For the second format, either a
#'vector of numeric indices can be provided, or a list with two numeric indices 
#'can be provided to take all the indices in the range between the two specified
#'indices (both extremes inclusive). The third format consists in providing a 
#'vector character strings (for file dimensions) or of values of whichever type
#'(for inner dimensions). For the file dimensions, the provided character 
#'strings in the third format will be used as components to build up the final 
#'path to the files (read further). For inner dimensions, the provided values in
#'the third format will be compared to the values of an associated coordinate 
#'variable (must be specified in '<dimname>_reorder', read further), and the 
#'indices of the closest values will be retrieved. When using the third format, 
#'a list with two values can also be provided to take all the indices of the 
#'values within the specified range.
#'\cr\cr
#'The \bold{name of the associated coordinate variable} must be a character 
#'string with the name of an associated coordinate variable to be found in the 
#'data files (in all* of them). For this to work, a 'file_var_reader' 
#'function must be specified when calling Start() (see parameter 
#''file_var_reader'). The coordinate variable must also be requested in the 
#'parameter 'return_vars' (see its section for details). This feature only 
#'works for inner dimensions.
#'\cr\cr
#'The \bold{tolerance value} is useful when indices for an inner dimension are 
#'specified in the third format (values of whichever type). In that case, the 
#'indices of the closest values in the coordinate variable are seeked. However 
#'the closest value might be too distant and we would want to consider no real 
#'match exists for such provided value. This is possible via the tolerance,
#'which allows to specify a threshold beyond which not to seek for matching 
#'values and mark that index as missing value.
#'\cr\cr
#'The \bold{reorder_function} is useful when indices for an inner dimension are
#'specified in the third fromat, and the retrieved indices need to be reordered 
#'in function of their provided associated variable values. A function can be 
#'provided, which receives as input a vector of values, and returns as outputs a
#'list with the components \code{$x} with the reordered values, and \code{$ix} 
#'with the permutation indices. Two reordering functions are included in 
#'startR, the Sort() and the CircularSort().
#'\cr\cr
#'The \bold{name of another dimension} to be specified in <dimname>_depends,
#'only available for file dimensions, must be a character string with the name 
#'of another requested \bold{file dimension} in \code{\dots}, and will make 
#'Start() aware that the path components of a file dimension can vary in
#'function of the path component of another file dimension. For instance, in the
#'example above, specifying \code{item_depends = 'section'} will make 
#'Start() aware that the item names vary in function of the section, i.e.
#'section 'electronics' has items 'a', 'b' and 'c' but section 'clothing' has 
#'items 'd', 'e', 'f'. Otherwise Start() would expect to find the same 
#'item names in all the sections.
#'\cr\cr
#'The \bold{name of another dimension} to be specified in '<dimname>_across',
#'only available for inner dimensions, must be a character string with the name 
#'of another requested \bold{inner dimension} in \code{\dots}, and will make 
#'Start() aware that an inner dimension extends along multiple files. For
#'instance, let us imagine that in the example above, the records for each item 
#'are so large that it becomes necessary to split them in multiple files each 
#'one containing the registers for a different period of time, e.g. in 10 files 
#'with 100 months each ('item_a_period1.data', 'item_a_period2.data', and so on).
#'In that case, the data can be perceived as having an extra file dimension, the 
#''period' dimension. The inner dimension 'month' would extend across multiple 
#'files, and providing the parameter \code{month = indices(1, 300)} would make 
#'Start() crash because it would perceive we have made a request out of 
#'bounds (each file contains 100 'month' indices, but we requested 1 to 300). 
#'This can be solved by specifying the parameter \code{month_across = period} (a
#'long with the full specification of the dimension 'period').
#'\cr\cr
#'\bold{Defining the path pattern}
#'\cr
#'As mentioned above, the parameter \dots also expects to receive information 
#'with the location of the data files. In order to do this, a special dimension 
#'must be defined. In that special dimension, in place of specifying indices to 
#'take, a path pattern must be provided. The path pattern is a character string 
#'that encodes the way the files are organized in their source. It must be a 
#'path to one of the data set files in an accessible local or remote file system,
#'or a URL to one of the files provided by a local or remote server. The regions
#'of this path that vary across files (along the file dimensions) must be 
#'replaced by wildcards. The wildcards must match any of the defined file 
#'dimensions in the call to Start() and must be delimited with heading 
#'and trailing '$'. Shell globbing expressions can be used in the path pattern. 
#'See the next code snippet for an example of a path pattern.
#'\cr\cr
#'All in all, the call to Start() to load the entire data set in the 
#'example of store item sales, would look as follows:
#'\cr
#'\command{
#'\cr # data <- Start(source = paste0('/data/$variable$/',
#'\cr #                               '$section$/$item$.data'),
#'\cr #               variable = 'all',
#'\cr #               section = 'all',
#'\cr #               item = 'all',
#'\cr #               item_depends = 'section',
#'\cr #               store = 'all',
#'\cr #               month = 'all')
#'}
#'\cr\cr
#'Note that in this example it would still be pending to properly define the 
#'parameters 'file_opener', 'file_closer', 'file_dim_reader', 
#''file_var_reader' and 'file_data_reader' for the '.data' file format
#'(see the corresponding sections).
#'\cr\cr
#'The call to Start() will return a multidimensional R array with the 
#'following dimensions:
#'\cr
#'\command{
#'\cr #  source variable  section      item    store    month
#'\cr #       1        2        2         3      100       24
#'}
#'\cr
#'The dimension specifications in the \code{\dots} do not have to follow any 
#'particular order. The returned array will have the dimensions in the same order
#'as they have been specified in the call. For example, the following call:
#'\cr
#'\command{
#'\cr # data <- Start(source = paste0('/data/$variable$/',
#'\cr #                               '$section$/$item$.data'),
#'\cr #               month = 'all',
#'\cr #               store = 'all',
#'\cr #               item = 'all',
#'\cr #               item_depends = 'section',
#'\cr #               section = 'all',
#'\cr #               variable = 'all')
#'}
#'\cr\cr
#'would return an array with the following dimensions:
#'\cr
#'\command{
#'\cr #  source    month    store      item  section variable
#'\cr #       1       24      100         3        2        2
#'}
#'\cr\cr
#'Next, a more advanced example to retrieve data for only the sales records, for
#'the first section ('electronics'), for the 1st and 3rd items and for the 
#'stores located in Barcelona (assuming the files contain the variable 
#''store_location' with the name of the city each of the 100 stores are located 
#'at):
#'\cr
#'\command{
#'\cr # data <- Start(source = paste0('/data/$variable$/',
#'\cr #                               '$section$/$item$.data'),
#'\cr #               variable = 'sales',
#'\cr #               section = 'first',
#'\cr #               item = indices(c(1, 3)),
#'\cr #               item_depends = 'section',
#'\cr #               store = 'Barcelona',
#'\cr #               store_var = 'store_location',
#'\cr #               month = 'all',
#'\cr #               return_vars = list(store_location = NULL))
#'}
#'\cr\cr
#'The defined names for the dimensions do not necessarily have to match the 
#'names of the dimensions inside the file. Lists of alternative names to be 
#'seeked can be defined in the parameter 'synonims'.
#'\cr\cr
#'If data from multiple sources (not necessarily following the same structure) 
#'has to be retrieved, it can be done by providing a vector of character strings
#'with path pattern specifications, or, in the extended form, by providing a 
#'list of lists with the components 'name' and 'path', and the name of the 
#'dataset and path pattern as values, respectively. For example:
#'\cr
#'\command{
#'\cr # data <- Start(source = list(
#'\cr #                 list(name = 'sourceA',
#'\cr #                      path = paste0('/sourceA/$variable$/',
#'\cr #                                    '$section$/$item$.data')),
#'\cr #                 list(name = 'sourceB',
#'\cr #                      path = paste0('/sourceB/$section$/',
#'\cr #                                    '$variable$/$item$.data'))
#'\cr #               ),
#'\cr #               variable = 'sales',
#'\cr #               section = 'first',
#'\cr #               item = indices(c(1, 3)),
#'\cr #               item_depends = 'section',
#'\cr #               store = 'Barcelona',
#'\cr #               store_var = 'store_location',
#'\cr #               month = 'all',
#'\cr #               return_vars = list(store_location = NULL))
#'}
#'\cr
#'
#'@param return_vars A named list where the names are the names of the 
#'variables to be fetched in the files, and the values are vectors of 
#'character strings with the names of the file dimension which to retrieve each
#'variable for, or NULL if the variable has to be retrieved only once 
#'from any (the first) of the involved files.\cr\cr
#'Apart from retrieving a multidimensional data array, retrieving auxiliary 
#'variables inside the files can also be needed. The parameter 
#''return_vars' allows for requesting such variables, as long as a 
#''file_var_reader' function is also specified in the call to 
#'Start() (see documentation on the corresponding parameter). 
#'\cr\cr
#'In the case of the the item sales example (see documentation on parameter 
#'\code{\dots)}, the store location variable is requested with the parameter\cr 
#'\code{return_vars = list(store_location = NULL)}.\cr This will cause 
#'Start() to fetch once the variable 'store_location' and return it in 
#'the component\cr \code{$Variables$common$store_location},\cr and will be an 
#'array of character strings with the location names, with the dimensions 
#'\code{c('store' = 100)}. Although useless in this example, we could ask 
#'Start() to fetch and return such variable for each file along the 
#'items dimension as follows: \cr 
#'\code{return_vars = list(store_location = c('item'))}.\cr In that case, the 
#'variable will be fetched once from a file of each of the items, and will be 
#'returned as an array with the dimensions \code{c('item' = 3, 'store' = 100)}.
#'\cr\cr
#'If a variable is requested along a file dimension that contains path pattern 
#'specifications ('source' in the example), the fetched variable values will be 
#'returned in the component\cr \code{$Variables$<dataset_name>$<variable_name>}.\cr 
#'For example:
#'\cr
#'\command{
#'\cr # data <- Start(source = list(
#'\cr #                 list(name = 'sourceA',
#'\cr #                      path = paste0('/sourceA/$variable$/',
#'\cr #                                    '$section$/$item$.data')),
#'\cr #                 list(name = 'sourceB',
#'\cr #                      path = paste0('/sourceB/$section$/',
#'\cr #                                    '$variable$/$item$.data'))
#'\cr #               ),
#'\cr #               variable = 'sales',
#'\cr #               section = 'first',
#'\cr #               item = indices(c(1, 3)),
#'\cr #               item_depends = 'section',
#'\cr #               store = 'Barcelona',
#'\cr #               store_var = 'store_location',
#'\cr #               month = 'all',
#'\cr #               return_vars = list(store_location = c('source',
#'\cr #                                                     'item')))
#'\cr # # Checking the structure of the returned variables
#'\cr # str(found_data$Variables)
#'\cr # Named list
#'\cr # ..$common: NULL
#'\cr # ..$sourceA: Named list
#'\cr # .. ..$store_location: char[1:18(3d)] 'Barcelona' 'Barcelona' ...
#'\cr # ..$sourceB: Named list
#'\cr # .. ..$store_location: char[1:18(3d)] 'Barcelona' 'Barcelona' ...
#'\cr # # Checking the dimensions of the returned variable
#'\cr # # for the source A
#'\cr # dim(found_data$Variables$sourceA)
#'\cr #     item   store
#'\cr #        3       3
#'}
#'\cr\cr
#'The names of the requested variables do not necessarily have to match the 
#'actual variable names inside the files. A list of alternative names to be 
#'seeked can be specified via the parameter 'synonims'.
#'
#'@param synonims A named list where the names are the requested variable or 
#'dimension names, and the values are vectors of character strings with 
#'alternative names to seek for such dimension or variable.\cr\cr
#'In some requests, data from different sources may follow different naming 
#'conventions for the dimensions or variables, or even files in the same source
#'could have varying names. This parameter is in order for Start() to 
#'properly identify the dimensions or variables with different names.
#'\cr\cr
#'In the example used in parameter 'return_vars', it may be the case that 
#'the two involved data sources follow slightly different naming conventions. 
#'For example, source A uses 'sect' as name for the sections dimension, whereas 
#'source B uses 'section'; source A uses 'store_loc' as variable name for the 
#'store locations, whereas source B uses 'store_location'. This can be taken 
#'into account as follows:
#'\cr
#'\command{
#'\cr # data <- Start(source = list(
#'\cr #                 list(name = 'sourceA',
#'\cr #                      path = paste0('/sourceA/$variable$/',
#'\cr #                                    '$section$/$item$.data')),
#'\cr #                 list(name = 'sourceB',
#'\cr #                      path = paste0('/sourceB/$section$/',
#'\cr #                                    '$variable$/$item$.data'))
#'\cr #               ),
#'\cr #               variable = 'sales',
#'\cr #               section = 'first',
#'\cr #               item = indices(c(1, 3)),
#'\cr #               item_depends = 'section',
#'\cr #               store = 'Barcelona',
#'\cr #               store_var = 'store_location',
#'\cr #               month = 'all',
#'\cr #               return_vars = list(store_location = c('source',
#'\cr #                                                     'item')),
#'\cr #               synonims = list(
#'\cr #                 section = c('sec', 'section'),
#'\cr #                 store_location = c('store_loc',
#'\cr #                                    'store_location')
#'\cr #               ))
#'}
#'\cr
#'
#'@param file_opener A function that receives as a single parameter 
#'  'file_path' a character string with the path to a file to be opened, 
#'  and returns an object with an open connection to the file (optionally with 
#'  header information) on success, or returns NULL on failure.
#'\cr\cr
#'This parameter takes by default NcOpener() (an opener function for NetCDF
#'files).
#'\cr\cr
#'See NcOpener() for a template to build a file opener for your own file 
#'format.
#'
#'@param file_var_reader A function with the header \code{file_path = NULL}, 
#'  \code{file_object = NULL}, \code{file_selectors = NULL}, \code{var_name}, 
#'  \code{synonims} that returns an array with auxiliary data (i.e. data from a
#'  variable) inside a file. Start() will provide automatically either a 
#'  'file_path' or a 'file_object' to the 'file_var_reader'
#'  function (the function has to be ready to work whichever of these two is 
#'  provided). The parameter 'file_selectors' will also be provided 
#'  automatically to the variable reader, containing a named list where the 
#'  names are the names of the file dimensions of the queried data set (see 
#'  documentation on \code{\dots}) and the values are single character strings 
#'  with the components used to build the path to the file being read (the one 
#'  provided in 'file_path' or 'file_object'). The parameter 'var_name'
#'  will be filled in automatically by Start() also, with the name of one
#'  of the variales to be read. The parameter 'synonims' will be filled in 
#'  with exactly the same value as provided in the parameter 'synonims' in 
#'  the call to Start(), and has to be used in the code of the variable 
#'  reader to check for alternative variable names inside the target file. The 
#'  'file_var_reader' must return a (multi)dimensional array with named 
#'  dimensions, and optionally with the attribute 'variales' with other 
#'  additional metadata on the retrieved variable.
#'\cr\cr
#'Usually, the 'file_var_reader' should be a degenerate case of the 
#''file_data_reader' (see documentation on the corresponding parameter), 
#'so it is recommended to code the 'file_data_reder' in first place.
#'\cr\cr
#'This parameter takes by default NcVarReader() (a variable reader function
#'for NetCDF files).
#'\cr\cr
#'See NcVarReader() for a template to build a variale reader for your own 
#'file format.
#'
#'@param file_dim_reader A function with the header \code{file_path = NULL}, 
#'  \code{file_object = NULL}, \code{file_selectors = NULL}, \code{synonims} 
#'  that returns a named numeric vector where the names are the names of the 
#'  dimensions of the multidimensional data array in the file and the values are
#'  the sizes of such dimensions. Start() will provide automatically 
#'  either a 'file_path' or a 'file_object' to the 
#'  'file_dim_reader' function (the function has to be ready to work 
#'  whichever of these two is provided). The parameter 'file_selectors'
#'  will also be provided automatically to the dimension reader, containing a
#'  named list where the names are the names of the file dimensions of the 
#'  queried data set (see documentation on \code{\dots}) and the values are 
#'  single character strings with the components used to build the path to the 
#'  file being read (the one provided in 'file_path' or 'file_object'). 
#'  The parameter 'synonims' will be filled in with exactly the same value 
#'  as provided in the parameter 'synonims' in the call to Start(), 
#'  and can optionally be used in advanced configurations.
#'\cr\cr
#'This parameter takes by default NcDimReader() (a dimension reader 
#'function for NetCDF files).
#'\cr\cr
#'See NcDimReader() for (an advanced) template to build a dimension reader
#'for your own file format.
#'
#'@param file_data_reader A function with the header \code{file_path = NULL}, 
#'  \code{file_object = NULL}, \code{file_selectors = NULL}, 
#'  \code{inner_indices = NULL}, \code{synonims} that returns a subset of the 
#'  multidimensional data array inside a file (even if internally it is not an 
#'  array). Start() will provide automatically either a 'file_path'
#'  or a 'file_object' to the 'file_data_reader' function (the 
#'  function has to be ready to work whichever of these two is provided). The
#'  parameter 'file_selectors' will also be provided automatically to the
#'  data reader, containing a named list where the names are the names of the
#'  file dimensions of the queried data set (see documentation on \code{\dots})
#'  and the values are single character strings with the components used to 
#'  build the path to the file being read (the one provided in 'file_path' or 
#'  'file_object'). The parameter 'inner_indices' will be filled in 
#'  automatically by Start() also, with a named list of numeric vectors, 
#'  where the names are the names of all the expected inner dimensions in a file
#'  to be read, and the numeric vectors are the indices to be taken from the 
#'  corresponding dimension (the indices may not be consecutive nor in order).
#'  The parameter 'synonims' will be filled in with exactly the same value 
#'  as provided in the parameter 'synonims' in the call to Start(), 
#'  and has to be used in the code of the data reader to check for alternative 
#'  dimension names inside the target file. The 'file_data_reader' must 
#'  return a (multi)dimensional array with named dimensions, and optionally with
#'  the attribute 'variables' with other additional metadata on the retrieved 
#'  data.
#'\cr\cr
#'Usually, 'file_data_reader' should use 'file_dim_reader'
#'(see documentation on the corresponding parameter), so it is recommended to 
#'code 'file_dim_reder' in first place.
#'\cr\cr
#'This parameter takes by default NcDataReader() (a data reader function 
#'for NetCDF files).
#'\cr\cr
#'See NcDataReader() for a template to build a data reader for your own 
#'file format.
#'
#'@param file_closer A function that receives as a single parameter 
#'  'file_object' an open connection (as returned by 'file_opener') 
#'  to one of the files to be read, optionally with header information, and 
#'  closes the open connection. Always returns NULL.
#'\cr\cr
#'This parameter takes by default NcCloser() (a closer function for NetCDF 
#'files).
#'\cr\cr
#'See NcCloser() for a template to build a file closer for your own file 
#'format.
#'
#'@param transform A function with the header \code{dara_array}, 
#'  \code{variables}, \code{file_selectors = NULL}, \code{\dots}. It receives as
#'  input, through the parameter \code{data_array}, a subset of a 
#'  multidimensional array (as returned by 'file_data_reader'), applies a 
#'  transformation to it and returns it, preserving the amount of dimensions but
#'  potentially modifying their size. This transformation may require data from 
#'  other auxiliary variables, automatically provided to 'transform' 
#'  through the parameter 'variables', in the form of a named list where
#'  the names are the variable names and the values are (multi)dimensional
#'  arrays. Which variables need to be sent to 'transform' can be specified
#'  with the parameter 'transform_vars' in Start(). The parameter 
#'  'file_selectors' will also be provided automatically to 
#'  'transform', containing a named list where the names are the names of 
#'  the file dimensions of the queried data set (see documentation on 
#'  \code{\dots}) and the values are single character strings with the 
#'  components used to build the path to the file the subset being processed 
#'  belongs to. The parameter \code{\dots} will be filled in with other 
#'  additional parameters to adjust the transformation, exactly as provided in 
#'  the call to Start() via the parameter 'transform_params'.
#'@param transform_params A named list with additional parameters to be sent to 
#'  the 'transform' function (if specified). See documentation on parameter
#'  'transform' for details.
#'@param transform_vars A vector of character strings with the names of 
#'  auxiliary variables to be sent to the 'transform' function (if 
#'  specified). All the variables to be sent to 'transform' must also 
#'  have been requested as return variables in the parameter 'return_vars' 
#'  of Start().
#'@param transform_extra_cells An integer of extra indices to retrieve from the 
#'  data set, beyond the requested indices in \code{\dots}, in order for 
#'  'transform' to dispose of additional information to properly apply 
#'  whichever transformation (if needed). As many as 
#'  'transform_extra_cells' will be retrieved beyond each of the limits for
#'  each of those inner dimensions associated to a coordinate variable and sent 
#'  to 'transform' (i.e. present in 'transform_vars'). After 
#'  'transform' has finished, Start() will take again and return a 
#'  subset of the result, for the returned data to fall within the specified 
#'  bounds in \code{\dots}. The default value is 2.
#'@param apply_indices_after_transform A logical value indicating when a 
#'  'transform' is specified in Start() and numeric indices are 
#'  provided for any of the inner dimensions that depend on coordinate variables,
#'  these numeric indices can be made effective (retrieved) before applying the 
#'  transformation or after. The boolean flag allows to adjust this behaviour. 
#'  It takes FALSE by default (numeric indices are applied before sending
#'  data to 'transform').
#'@param pattern_dims A character string indicating the name of the dimension 
#'  with path pattern specifications (see \code{\dots} for details). If not  
#'  specified, Start() assumes the first provided dimension is the pattern 
#'  dimension, with a warning.
#'@param metadata_dims A vector of character strings with the names of the file 
#'  dimensions which to return metadata for. As noted in 'file_data_reader', 
#'  the data reader can optionally return auxiliary data via the attribute 
#'  'variables' of the returned array. Start() by default returns the 
#'  auxiliary data read for only the first file of each source (or data set) in 
#'  the pattern dimension (see \code{\dots} for info on what the pattern 
#'  dimension is). However it can be configured to return the metadata for all 
#'  the files along any set of file dimensions. The default value is NULL, and
#'  it will be assigned automatically as parameter 'pattern_dims'.
#'@param selector_checker A function used internaly by Start() to 
#'  translate a set of selectors (values for a dimension associated to a 
#'  coordinate variable) into a set of numeric indices. It takes by default 
#'  SelectorChecker() and, in principle, it should not be required to 
#'  change it for customized file formats. The option to replace it is left open
#'  for more versatility. See the code of SelectorChecker() for details on
#'  the inputs, functioning and outputs of a selector checker.
#'@param merge_across_dims A logical value indicating whether to merge 
#'  dimensions across which another dimension extends (according to the 
#'  '<dimname>_across' parameters). Takes the value FALSE by default. For 
#'  example, if the dimension 'time' extends across the dimension 'chunk' and 
#'  \code{merge_across_dims = TRUE}, the resulting data array will only contain
#'  only the dimension 'time' as long as all the chunks together.
#'@param merge_across_dims_narm A logical value indicating whether to remove
#'  the additional NAs from data when parameter 'merge_across_dims' is TRUE.
#'  It is helpful when the length of the to-be-merged dimension is different 
#'  across another dimension. For example, if the dimension 'time' extends 
#'  across dimension 'chunk', and the time length along the first chunk is 2 
#'  while along the second chunk is 10. Setting this parameter as TRUE can 
#'  remove the additional 8 NAs at position 3 to 10. The default value is FALSE.
#'@param split_multiselected_dims A logical value indicating whether to split a 
#'  dimension that has been selected with a multidimensional array of selectors
#'  into as many dimensions as present in the selector array. The default value
#'  is FALSE.
#'@param path_glob_permissive A logical value or an integer specifying how many
#'  folder levels in the path pattern, beginning from the end, the shell glob
#'  expressions must be preserved and worked out for each file. The default 
#'  value is FALSE, which is equivalent to 0. TRUE is equivalent to 1.\cr\cr
#'When specifying a path pattern for a dataset, it might contain shell glob 
#'experissions. For each dataset, the first file matching the path pattern is 
#'found, and the found file is used to work out fixed values for the glob 
#'expressions that will be used for all the files of the dataset. However, in 
#'some cases, the values of the shell glob expressions may not be constant for 
#'all files in a dataset, and they need to be worked out for each file 
#'involved.\cr\cr
#'For example, a path pattern could be as follows: \cr
#'\code{'/path/to/dataset/$var$_*/$date$_*_foo.nc'}. \cr Leaving 
#'\code{path_glob_permissive = FALSE} will trigger automatic seek of the 
#'  contents to replace the asterisks (e.g. the first asterisk matches with 
#'  \code{'bar'} and the second with \code{'baz'}. The found contents will be 
#'  used for all files in the dataset (in the example, the path pattern will be
#'  fixed to\cr \code{'/path/to/dataset/$var$_bar/$date$_baz_foo.nc'}. However, if
#'  any of the files in the dataset have other contents in the position of the
#'  asterisks, Start() will not find them (in the example, a file like \cr
#'  \code{'/path/to/dataset/precipitation_bar/19901101_bin_foo.nc'} would not be
#'  found). Setting \code{path_glob_permissive = 1} would preserve global
#'  expressions in the latest level (in the example, the fixed path pattern
#'  would be\cr \code{'/path/to/dataset/$var$_bar/$date$_*_foo.nc'}, and the
#'  problematic file mentioned before would be found), but of course this would
#'  slow down the Start() call if the dataset involves a large number of
#'  files. Setting \code{path_glob_permissive = 2} would leave the original path
#'  pattern with the original glob expressions in the 1st and 2nd levels (in the
#'  example, both asterisks would be preserved, thus would allow Start()
#'  to recognize files such as \cr
#'  \code{'/path/to/dataset/precipitation_zzz/19901101_yyy_foo.nc'}).\cr\cr
#'Note that each glob expression can only represent one possibility (Start() 
#'chooses the first). Because /code{*} is not the tag, which means it cannot
#'be a dimension of the output array. Therefore, only one possibility can be
#'adopted. For example, if \cr
#'\code{'/path/to/dataset/precipitation_*/19901101_*_foo.nc'}\cr
#'has two matches:\cr
#'\code{'/path/to/dataset/precipitation_xxx/19901101_yyy_foo.nc'} and\cr
#'\code{'/path/to/dataset/precipitation_zzz/19901101_yyy_foo.nc'},\cr
#'only the first found file will be used.
#'@param retrieve A logical value indicating whether to retrieve the data
#'  defined in the Start() call or to explore only its dimension lengths 
#'  and names, and the values for the file and inner dimensions. The default
#'  value is FALSE.
#'@param num_procs An integer of number of processes to be created for the
#'  parallel execution of the retrieval/transformation/arrangement of the
#'  multiple involved files in a call to Start(). If set to NULL,
#'  takes the number of available cores (as detected by detectCores() in 
#'  the package 'future'). The default value is 1 (no parallel execution).
#'@param silent A logical value of whether to display progress messages (FALSE)
#'   or not (TRUE). The default value is FALSE.
#'@param debug A logical value of whether to return detailed messages on the
#'  progress and operations in a Start() call (TRUE) or not (FALSE). The
#'  default value is FALSE.
#'
#'@return If \code{retrieve = TRUE} the involved data is loaded into RAM memory
#'  and an object of the class 'startR_cube' with the following components is
#'  returned:\cr
#'  \item{Data}{
#'  Multidimensional data array with named dimensions, with the data values
#'  requested via \code{\dots} and other parameters. This array can potentially 
#'  contain metadata in the attribute 'variables'.
#'  }
#'  \item{Variables}{
#'  Named list of 1 + N components, containing lists of retrieved variables (as
#'  requested in 'return_vars') common to all the data sources (in the 1st
#'  component, \code{$common}), and for each of the N dara sources (named after 
#'  the source name, as specified in \dots, or, if not specified, \code{$dat1},
#'  \code{$dat2}, ..., \code{$datN}). Each of the variables are contained in a
#'  multidimensional array with named dimensions, and potentially with the
#'  attribute 'variables' with additional auxiliary data.
#'  }
#'  \item{Files}{
#'  Multidimensonal character string array with named dimensions. Its dimensions
#'  are the file dimensions (as requested in \code{\dots}). Each cell in this
#'  array contains a path to a retrieved file, or NULL if the corresponding
#'  file was not found.
#'  }
#'  \item{NotFoundFiles}{
#'  Array with the same shape as \code{$Files} but with NULL in the
#'  positions for which the corresponding file was found, and a path to the
#'  expected file in the positions for which the corresponding file was not
#'  found.
#'  }
#'  \item{FileSelectors}{
#'  Multidimensional character string array with named dimensions, with the same
#'  shape as \code{$Files} and \code{$NotFoundFiles}, which contains the
#'  components used to build up the paths to each of the files in the data
#'  sources.
#'  }
#'If \code{retrieve = FALSE} the involved data is not loaded into RAM memory and
#'an object of the class 'startR_header' with the following components is
#' returned:\cr
#'  \item{Dimensions}{
#'  Named vector with the dimension lengths and names of the data involved in
#'  the Start() call.
#'  }
#'  \item{Variables}{
#'  Named list of 1 + N components, containing lists of retrieved variables (as
#'  requested in 'return_vars') common to all the data sources (in the 1st
#'  component, \code{$common}), and for each of the N dara sources (named after
#'  the source name, as specified in \dots, or, if not specified, \code{$dat1},
#'  \code{$dat2}, ..., \code{$datN}). Each of the variables are contained in a
#'  multidimensional array with named dimensions, and potentially with the
#'  attribute 'variables' with additional auxiliary data.
#'  }
#'  \item{Files}{
#'  Multidimensonal character string array with named dimensions. Its dimensions
#'  are the file dimensions (as requested in \dots). Each cell in this array
#'  contains a path to a file to be retrieved (which may exist or not).
#'  }
#'  \item{FileSelectors}{
#'  Multidimensional character string array with named dimensions, with the same
#'  shape as \code{$Files} and \code{$NotFoundFiles}, which contains the
#'  components used to build up the paths to each of the files in the data
#'  sources.
#'  }
#'  \item{StartRCall}{
#'  List of parameters sent to the Start() call, with the parameter
#'  'retrieve' set to TRUE. Intended for calling in order to
#'  retrieve the associated data a posteriori with a call to do.call().
#'  }
#'
#'@examples
#'  data_path <- system.file('extdata', package = 'startR')
#'  path_obs <- file.path(data_path, 'obs/monthly_mean/$var$/$var$_$sdate$.nc')
#'  sdates <- c('200011', '200012')
#'  data <- Start(dat = list(list(path = path_obs)),
#'                var = 'tos',
#'                sdate = sdates,
#'                time = 'all',
#'                latitude = 'all',
#'                longitude = 'all',
#'                return_vars = list(latitude = 'dat', 
#'                                   longitude = 'dat', 
#'                                   time = 'sdate'),
#'                retrieve = FALSE)
#'
#'@import bigmemory multiApply parallel abind future
#'@importFrom utils str
#'@importFrom stats na.omit setNames 
#'@importFrom ClimProjDiags Subset
#'@export
Start <- function(..., # dim = indices/selectors, 
                  # dim_var = 'var', 
                  # dim_reorder = Sort/CircularSort, 
                  # dim_tolerance = number, 
                  # dim_depends = 'file_dim', 
                  # dim_across = 'file_dim', 
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
                  transform_extra_cells = 2, 
                  apply_indices_after_transform = FALSE, 
                  pattern_dims = NULL,
                  metadata_dims = NULL, 
                  selector_checker = SelectorChecker,
                  merge_across_dims = FALSE,
                  merge_across_dims_narm = FALSE,
                  split_multiselected_dims = FALSE,
                  path_glob_permissive = FALSE,
                  retrieve = FALSE, 
                  num_procs = 1, 
                  silent = FALSE, debug = FALSE) {
  #, config_file = NULL
  #dictionary_dim_names = ,
  #dictionary_var_names =  

  # Specify Subset() is from ClimProjDiags
  Subset <- ClimProjDiags::Subset

  dim_params <- list(...)
  
  # Take *_var parameters apart
  var_params_ind <- grep('_var$', names(dim_params))
  var_params <- dim_params[var_params_ind]
  # Check all *_var are NULL or vectors of character strings, and 
  # that they all have a matching dimension param.
  i <- 1
  for (var_param in var_params) {
    if (!is.character(var_param)) {
      stop("All '*_var' parameters must be character strings.")
    } else if (!any(grepl(paste0('^', strsplit(names(var_params)[i], 
                                               '_var$')[[1]][1], '$'),
                          names(dim_params)))) {
      stop(paste0("All '*_var' parameters must be associated to a dimension parameter. Found parameter '", 
                  names(var_params)[i], "' but no parameter '", 
                  strsplit(names(var_params)[i], '_var$')[[1]][1], "'."))
    }
    i <- i + 1
  }
  # Make the keys of 'var_params' to be the name of 
  # the corresponding dimension.
  if (length(var_params) < 1) {
    var_params <- NULL
  } else {
    names(var_params) <- gsub('_var$', '', names(var_params))
  }
  
  # Take *_reorder parameters apart
  dim_reorder_params_ind <- grep('_reorder$', names(dim_params))
  dim_reorder_params <- dim_params[dim_reorder_params_ind]
  # Make the keys of 'dim_reorder_params' to be the name of 
  # the corresponding dimension.
  if (length(dim_reorder_params) < 1) {
    dim_reorder_params <- NULL
  } else {
    names(dim_reorder_params) <- gsub('_reorder$', '', names(dim_reorder_params))
  }
  
  # Take *_tolerance parameters apart
  tolerance_params_ind <- grep('_tolerance$', names(dim_params))
  tolerance_params <- dim_params[tolerance_params_ind]
  
  # Take *_depends parameters apart
  depends_params_ind <- grep('_depends$', names(dim_params))
  depends_params <- dim_params[depends_params_ind]
  # Check all *_depends are NULL or vectors of character strings, and 
  # that they all have a matching dimension param.
  i <- 1
  for (depends_param in depends_params) {
    if (!is.character(depends_param) || (length(depends_param) > 1)) {
      stop("All '*_depends' parameters must be single character strings.")
    } else if (!any(grepl(paste0('^', strsplit(names(depends_params)[i], 
                                               '_depends$')[[1]][1], '$'),
                          names(dim_params)))) {
      stop(paste0("All '*_depends' parameters must be associated to a dimension parameter. Found parameter '", 
                  names(depends_params)[i], "' but no parameter '", 
                  strsplit(names(depends_params)[i], '_depends$')[[1]][1], "'."))
    }
    i <- i + 1
  }
  # Make the keys of 'depends_params' to be the name of 
  # the corresponding dimension.
  if (length(depends_params) < 1) {
    depends_params <- NULL
  } else {
    names(depends_params) <- gsub('_depends$', '', names(depends_params))
  }
  # Change name to depending_file_dims
  depending_file_dims <- depends_params
  
  # Take *_across parameters apart
  across_params_ind <- grep('_across$', names(dim_params))
  across_params <- dim_params[across_params_ind]
  # Check all *_across are NULL or vectors of character strings, and 
  # that they all have a matching dimension param.
  i <- 1
  for (across_param in across_params) {
    if (!is.character(across_param) || (length(across_param) > 1)) {
      stop("All '*_across' parameters must be single character strings.")
    } else if (!any(grepl(paste0('^', strsplit(names(across_params)[i], 
                                               '_across$')[[1]][1], '$'),
                          names(dim_params)))) {
      stop(paste0("All '*_across' parameters must be associated to a dimension parameter. Found parameter '", 
                  names(across_params)[i], "' but no parameter '", 
                  strsplit(names(across_params)[i], '_across$')[[1]][1], "'."))
    }
    i <- i + 1
  }
  # Make the keys of 'across_params' to be the name of 
  # the corresponding dimension.
  if (length(across_params) < 1) {
    across_params <- NULL
  } else {
    names(across_params) <- gsub('_across$', '', names(across_params))
  }
  # Change name to inner_dims_across_files
  inner_dims_across_files <- across_params
  
  # Check merge_across_dims
  if (!is.logical(merge_across_dims)) {
    stop("Parameter 'merge_across_dims' must be TRUE or FALSE.")
  }
  
  # Check merge_across_dims_narm
  if (!is.logical(merge_across_dims_narm)) {
    stop("Parameter 'merge_across_dims_narm' must be TRUE or FALSE.")
  }
  if (!merge_across_dims & merge_across_dims_narm) {
    merge_across_dims_narm <- FALSE
    warning(paste0("Parameter 'merge_across_dims_narm' can only be TRUE when ",
                   "'merge_across_dims' is TRUE. Set 'merge_across_dims_narm'",
                   " to FALSE."))
  }
  
  # Leave alone the dimension parameters in the variable dim_params
  if (length(c(var_params_ind, dim_reorder_params_ind, tolerance_params_ind,
               depends_params_ind, across_params_ind)) > 0) {
    dim_params <- dim_params[-c(var_params_ind, dim_reorder_params_ind,
                                tolerance_params_ind, depends_params_ind,
                                across_params_ind)]
    # Reallocating pairs of across file and inner dimensions if they have
    # to be merged. They are put one next to the other to ease merge later.
    if (merge_across_dims) {
      for (inner_dim_across in names(inner_dims_across_files)) {
        inner_dim_pos <- which(names(dim_params) == inner_dim_across)
        file_dim_pos <- which(names(dim_params) == inner_dims_across_files[[inner_dim_across]])
        new_pos <- inner_dim_pos
        if (file_dim_pos < inner_dim_pos) {
          new_pos <- new_pos - 1
        }
        dim_params_to_move <- dim_params[c(inner_dim_pos, file_dim_pos)]
        dim_params <- dim_params[-c(inner_dim_pos, file_dim_pos)]
        new_dim_params <- list()
        if (new_pos > 1) {
          new_dim_params <- c(new_dim_params, dim_params[1:(new_pos - 1)])
        }
        new_dim_params <- c(new_dim_params, dim_params_to_move)
        if (length(dim_params) >= new_pos) {
          new_dim_params <- c(new_dim_params, dim_params[new_pos:length(dim_params)])
        }
        dim_params <- new_dim_params
      }
    }
  }
  dim_names <- names(dim_params)
  if (is.null(dim_names)) {
    stop("At least one pattern dim must be specified.")
  }
  
  # Look for chunked dims
  chunks <- vector('list', length(dim_names))
  names(chunks) <- dim_names
  for (dim_name in dim_names) {
    if (!is.null(attr(dim_params[[dim_name]], 'chunk'))) {
      chunks[[dim_name]] <- attr(dim_params[[dim_name]], 'chunk')
      attributes(dim_params[[dim_name]]) <- attributes(dim_params[[dim_name]])[-which(names(attributes(dim_params[[dim_name]])) == 'chunk')]
    } else {
      chunks[[dim_name]] <- c(chunk = 1, n_chunks = 1)
    }
  }
  # This is a helper function to compute the chunk indices to take once the total
  # number of indices for a dimension has been discovered.
  chunk_indices <- function(n_indices, chunk, n_chunks, dim_name) {
    if (n_chunks > n_indices) {
      stop("Requested to divide dimension '", dim_name, "' of length ", 
           n_indices, " in ", n_chunks, " chunks, which is not possible.")
    }
    chunk_sizes <- rep(floor(n_indices / n_chunks), n_chunks)
    chunks_to_extend <- n_indices - chunk_sizes[1] * n_chunks
    if (chunks_to_extend > 0) {
      chunk_sizes[1:chunks_to_extend] <- chunk_sizes[1:chunks_to_extend] + 1
    }
    chunk_size <- chunk_sizes[chunk]
    offset <- 0
    if (chunk > 1) {
      offset <- sum(chunk_sizes[1:(chunk - 1)])
    }
    indices <- 1:chunk_sizes[chunk] + offset
    array(indices, dim = setNames(length(indices), dim_name))
  }
  
  # Check pattern_dims
  if (is.null(pattern_dims)) {
    .warning(paste0("Parameter 'pattern_dims' not specified. Taking the first dimension, '", 
                    dim_names[1], "' as 'pattern_dims'."))
    pattern_dims <- dim_names[1]
  } else if (is.character(pattern_dims) && (length(pattern_dims) > 0)) {
    pattern_dims <- unique(pattern_dims)
  } else {
    stop("Parameter 'pattern_dims' must be a vector of character strings.")
  }
  if (any(names(var_params) %in% pattern_dims)) {
    stop("'*_var' parameters specified for pattern dimensions. Remove or fix them.")
  }
  # Find the pattern dimension with the pattern specifications
  found_pattern_dim <- NULL
  for (pattern_dim in pattern_dims) {
    # Check all specifications in pattern_dim are valid
    dat <- datasets <- dim_params[[pattern_dim]]
    if (is.null(dat) || !(is.character(dat) && all(nchar(dat) > 0)) && !is.list(dat)) {
      stop(paste0("Parameter '", pattern_dim, 
                  "' must be a list of lists with pattern specifications or a vector of character strings."))
    }
    if (!is.null(dim_reorder_params[[pattern_dim]])) {
      .warning(paste0("A reorder for the selectors of '", pattern_dim, 
                      "' has been specified, but it is a pattern dimension and the reorder will be ignored."))
    }
    if (is.list(dat) || any(sapply(dat, is.list))) {
      if (is.null(found_pattern_dim)) {
        found_pattern_dim <- pattern_dim
      } else {
        stop("Found more than one pattern dim with pattern specifications (list of lists). One and only one pattern dim must contain pattern specifications.")
      }
    }
  }
  if (is.null(found_pattern_dim)) {
    .warning(paste0("Could not find any pattern dim with explicit data set descriptions (in the form of list of lists). Taking the first pattern dim, '", pattern_dims[1], "', as dimension with pattern specifications."))
    found_pattern_dim <- pattern_dims[1]
  }
  
  # Check all *_reorder are NULL or functions, and that they all have 
  # a matching dimension param.
  i <- 1
  for (dim_reorder_param in dim_reorder_params) {
    if (!is.function(dim_reorder_param)) {
      stop("All '*_reorder' parameters must be functions.")
    } else if (!any(grepl(paste0('^', strsplit(names(dim_reorder_params)[i], 
                                               '_reorder$')[[1]][1], '$'),
                          names(dim_params)))) {
      stop(paste0("All '*_reorder' parameters must be associated to a dimension parameter. Found parameter '", 
                  names(dim_reorder_params)[i], "' but no parameter '", 
                  strsplit(names(dim_reorder_params)[i], '_reorder$')[[1]][1], "'."))
      #} else if (!any(grepl(paste0('^', strsplit(names(dim_reorder_params)[i], 
      #                                           '_reorder$')[[1]][1], '$'),
      #                      names(var_params)))) {
      #  stop(paste0("All '*_reorder' parameters must be associated to a dimension parameter associated to a ",
      #              "variable. Found parameter '", names(dim_reorder_params)[i], "' and dimension parameter '", 
      #              strsplit(names(dim_reorder_params)[i], '_reorder$')[[1]][1], "' but did not find variable ",
      #              "parameter '", strsplit(names(dim_reorder_params)[i], '_reorder$')[[1]][1], "_var'."))
    }
    i <- i + 1
  }
  
  # Check all *_tolerance are NULL or vectors of character strings, and 
  # that they all have a matching dimension param.
  i <- 1
  for (tolerance_param in tolerance_params) {
    if (!any(grepl(paste0('^', strsplit(names(tolerance_params)[i], 
                                        '_tolerance$')[[1]][1], '$'),
                   names(dim_params)))) {
      stop(paste0("All '*_tolerance' parameters must be associated to a dimension parameter. Found parameter '", 
                  names(tolerance_params)[i], "' but no parameter '", 
                  strsplit(names(tolerance_params)[i], '_tolerance$')[[1]][1], "'."))
      #} else if (!any(grepl(paste0('^', strsplit(names(tolerance_params)[i], 
      #                                           '_tolerance$')[[1]][1], '$'),
      #                      names(var_params)))) {
      #  stop(paste0("All '*_tolerance' parameters must be associated to a dimension parameter associated to a ",
      #              "variable. Found parameter '", names(tolerance_params)[i], "' and dimension parameter '", 
      #              strsplit(names(tolerance_params)[i], '_tolerance$')[[1]][1], "' but did not find variable ",
      #              "parameter '", strsplit(names(tolerance_params)[i], '_tolerance$')[[1]][1], "_var'."))
    }
    i <- i + 1
  }
  # Make the keys of 'tolerance_params' to be the name of 
  # the corresponding dimension.
  if (length(tolerance_params) < 1) {
    tolerance_params <- NULL
  } else {
    names(tolerance_params) <- gsub('_tolerance$', '', names(tolerance_params))
  }
  
  # Check metadata_dims
  if (!is.null(metadata_dims)) {
    if (any(is.na(metadata_dims))) {
      metadata_dims <- NULL
    } else if (!is.character(metadata_dims) || (length(metadata_dims) < 1)) {
      stop("Parameter 'metadata' dims must be a vector of at least one character string.")
    }
  } else {
    metadata_dims <- pattern_dims
  }
  
  # Check if pattern_dims is the first item in metadata_dims
  if ((pattern_dims %in% metadata_dims) & metadata_dims[1] != pattern_dims) {
    metadata_dims <- c(metadata_dims[-1], metadata_dims[1])
  }
  # Check if metadata_dims has more than 2 elements
  if ((metadata_dims[1] == pattern_dims & length(metadata_dims) > 2)) {
  .warning(paste0("Parameter 'metadata_dims' has too many elements which serve repetitive ",
                  "function. Keep '", metadata_dims[1], "' and '", metadata_dims[2], "' only."))
  metadata_dims <- metadata_dims[1:2]
  } else if (!(pattern_dims %in% metadata_dims) & length(metadata_dims) > 1) {
  .warning(paste0("Parameter 'metadata_dims' has too many elements which serve repetitive ",
                  "function. Keep '", metadata_dims[1], "' only."))
  metadata_dims <- metadata_dims[1]
  }

  # Once the pattern dimension with dataset specifications is found,
  # the variable 'dat' is mounted with the information of each
  # dataset.
  # Take only the datasets for the requested chunk
  dats_to_take <- chunk_indices(length(dim_params[[found_pattern_dim]]), 
                                chunks[[found_pattern_dim]]['chunk'], 
                                chunks[[found_pattern_dim]]['n_chunks'],
                                found_pattern_dim)
  dim_params[[found_pattern_dim]] <- dim_params[[found_pattern_dim]][dats_to_take]
  dat <- datasets <- dim_params[[found_pattern_dim]]
  dat_info_names <- c('name', 'path')#, 'nc_var_name', 'suffix', 'var_min', 'var_max', 'dimnames')
  dat_to_fetch <- c()
  dat_names <- c()
  if (!is.list(dat)) {
    dat <- as.list(dat)
  } else {
    if (!any(sapply(dat, is.list))) {
      dat <- list(dat)
    }
  }
  for (i in 1:length(dat)) {
    if (is.character(dat[[i]]) && length(dat[[i]]) == 1 && nchar(dat[[i]]) > 0) {
      if (grepl('^(\\./|\\.\\./|/.*/|~/)', dat[[i]])) {
        dat[[i]] <- list(path = dat[[i]])
      } else {
        dat[[i]] <- list(name = dat[[i]])
      }
    } else if (!is.list(dat[[i]])) {
      stop(paste0("Parameter '", pattern_dim, 
                  "' is incorrect. It must be a list of lists or character strings."))
    }
    #if (!(all(names(dat[[i]]) %in% dat_info_names))) {
    #  stop("Error: parameter 'dat' is incorrect. There are unrecognized components in the information of some of the datasets. Check 'dat' in ?Load for details.")
    #}
    if (!('name' %in% names(dat[[i]]))) {
      dat[[i]][['name']] <- paste0('dat', i)
      if (!('path' %in% names(dat[[i]]))) {
        stop(paste0("Parameter '", found_pattern_dim, 
                    "' is incorrect. A 'path' should be provided for each dataset if no 'name' is provided."))
      }
    } else if (!('path' %in% names(dat[[i]]))) {
      dat_to_fetch <- c(dat_to_fetch, i)
    }
    #if ('path' %in% names(dat[[i]])) {
    #  if (!('nc_var_name' %in% names(dat[[i]]))) {
    #    dat[[i]][['nc_var_name']] <- '$var_name$'
    #  }
    #  if (!('suffix' %in% names(dat[[i]]))) {
    #    dat[[i]][['suffix']] <- ''
    #  }
    #  if (!('var_min' %in% names(dat[[i]]))) {
    #    dat[[i]][['var_min']] <- ''
    #  }
    #  if (!('var_max' %in% names(dat[[i]]))) {
    #    dat[[i]][['var_max']] <- ''
    #  }
    #}
    dat_names <- c(dat_names, dat[[i]][['name']])
  }
  if ((length(dat_to_fetch) > 0) && (length(dat_to_fetch) < length(dat))) {
    .warning("'path' has been provided for some datasets. Any information in the configuration file related to these will be ignored.")
  }
  if (length(dat_to_fetch) > 0) {
    stop("Specified only the name for some data sets, but not the path ",
         "pattern. This option has not been yet implemented.")
  }
  
  # Reorder inner_dims_across_files (to make the keys be the file dimensions,
  # and the values to be the inner dimensions that go across it).
  if (!is.null(inner_dims_across_files)) {
    # Reorder: example, convert list(ftime = 'chunk', ensemble = 'member', xx = 'chunk')
    #                        to list(chunk = c('ftime', 'xx'), member = 'ensemble')
    new_idaf <- list()
    for (i in names(inner_dims_across_files)) {
      if (!(inner_dims_across_files[[i]] %in% names(new_idaf))) {
        new_idaf[[inner_dims_across_files[[i]]]] <- i
      } else {
        new_idaf[[inner_dims_across_files[[i]]]] <- c(new_idaf[[inner_dims_across_files[[i]]]], i)
      }
    }
    inner_dims_across_files <- new_idaf
  }
  
  # Check return_vars
  if (is.null(return_vars)) {
    return_vars <- list()
    #    if (length(var_params) > 0) {
    #      return_vars <- as.list(paste0(names(var_params), '_var'))
    #    } else {
    #      return_vars <- list()
    #    }
  }
  if (!is.list(return_vars)) {
    stop("Parameter 'return_vars' must be a list or NULL.")
  }
  if (length(return_vars) > 0 && is.null(names(return_vars))) {
    #    names(return_vars) <- rep('', length(return_vars))
    stop("Parameter 'return_vars' must be a named list.")
  }
  i <- 1
  while (i <= length(return_vars)) {
    #    if (names(return_vars)[i] == '') {
    #      if (!(is.character(return_vars[[i]]) && (length(return_vars[[i]]) == 1))) {
    #        stop("The ", i, "th specification in 'return_vars' is malformed.")
    #      } 
    #      if (!grepl('_var$', return_vars[[i]])) {
    #        stop("The ", i, "th specification in 'return_vars' is malformed.")
    #      }
    #      dim_name <- strsplit(return_vars[[i]], '_var$')[[1]][1]
    #      if (!(dim_name %in% names(var_params))) {
    #        stop("'", dim_name, "_var' requested in 'return_vars' but ",
    #             "no '", dim_name, "_var' specified in the .Load call.")
    #      }
    #      names(return_vars)[i] <- var_params[[dim_name]]
    #      return_vars[[i]] <- found_pattern_dim
    #    } else
    if (length(return_vars[[i]]) > 0) { 
      if (!is.character(return_vars[[i]])) {
        stop("The ", i, "th specification in 'return_vars' is malformed. It ",
             "must be a vector of character strings of valid file dimension ",
             "names.")
      }
    }
    i <- i + 1
  }
  
  # Check synonims
  if (!is.null(synonims)) {
    error <- FALSE
    if (!is.list(synonims)) {
      error <- TRUE
    }
    for (synonim_entry in names(synonims)) {
      if (!(synonim_entry %in% names(dim_params)) &&
          !(synonim_entry %in% names(return_vars))) {
        error <- TRUE
      }
      if (!is.character(synonims[[synonim_entry]]) ||
          length(synonims[[synonim_entry]]) < 1) {
        error <- TRUE
      }
    }
    if (error) {
      stop("Parameter 'synonims' must be a named list, where the names are ",
           "a name of a requested dimension or variable and the values are ",
           "vectors of character strings with at least one alternative name ",
           " for each dimension or variable in 'synonims'.")
    }
  }
  if (length(unique(names(synonims))) < length(names(synonims))) {
    stop("There must not be repeated entries in 'synonims'.")
  }
  if (length(unique(unlist(synonims))) < length(unlist(synonims))) {
    stop("There must not be repeated values in 'synonims'.")
  }
  # Make that all dims and vars have an entry in synonims, even if only dim_name = dim_name
  dim_entries_to_add <- which(!(names(dim_params) %in% names(synonims)))
  if (length(dim_entries_to_add) > 0) {
    synonims[names(dim_params)[dim_entries_to_add]] <- as.list(names(dim_params)[dim_entries_to_add])
  }
  var_entries_to_add <- which(!(names(var_params) %in% names(synonims)))
  if (length(var_entries_to_add) > 0) {
    synonims[names(var_params)[var_entries_to_add]] <- as.list(names(var_params)[var_entries_to_add])
  }
  
  # Check selector_checker
  if (is.null(selector_checker) || !is.function(selector_checker)) {
    stop("Parameter 'selector_checker' must be a function.")
  }
  
  # Check file_opener
  if (is.null(file_opener) || !is.function(file_opener)) {
    stop("Parameter 'file_opener' must be a function.")
  }
  
  # Check file_var_reader
  if (!is.null(file_var_reader) && !is.function(file_var_reader)) {
    stop("Parameter 'file_var_reader' must be a function.")
  }
  
  # Check file_dim_reader
  if (!is.null(file_dim_reader) && !is.function(file_dim_reader)) {
    stop("Parameter 'file_dim_reader' must be a function.")
  }
  
  # Check file_data_reader
  if (is.null(file_data_reader) || !is.function(file_data_reader)) {
    stop("Parameter 'file_data_reader' must be a function.")
  }
  
  # Check file_closer
  if (is.null(file_closer) || !is.function(file_closer)) {
    stop("Parameter 'file_closer' must be a function.")
  }
  
  # Check transform
  if (!is.null(transform)) {
    if (!is.function(transform)) {
      stop("Parameter 'transform' must be a function.")
    }
  }
  
  # Check transform_params
  if (!is.null(transform_params)) {
    if (!is.list(transform_params)) {
      stop("Parameter 'transform_params' must be a list.")
    }
    if (is.null(names(transform_params))) {
      stop("Parameter 'transform_params' must be a named list.")
    }
  }
  
  # Check transform_vars
  if (!is.null(transform_vars)) {
    if (!is.character(transform_vars)) {
      stop("Parameter 'transform_vars' must be a vector of character strings.")
    }
  }
  if (any(!(transform_vars %in% names(return_vars)))) {
    stop("All the variables specified in 'transform_vars' must also be specified in 'return_vars'.")
  }
  
  # Check apply_indices_after_transform
  if (!is.logical(apply_indices_after_transform)) {
    stop("Parameter 'apply_indices_after_transform' must be either TRUE or FALSE.")
  }
  aiat <- apply_indices_after_transform
  
  # Check transform_extra_cells
  if (!is.numeric(transform_extra_cells)) {
    stop("Parameter 'transform_extra_cells' must be numeric.")
  }
  transform_extra_cells <- round(transform_extra_cells)
  
  # Check split_multiselected_dims
  if (!is.logical(split_multiselected_dims)) {
    stop("Parameter 'split_multiselected_dims' must be TRUE or FALSE.")
  }
  
  # Check path_glob_permissive
  if (!is.numeric(path_glob_permissive) && !is.logical(path_glob_permissive)) {
    stop("Parameter 'path_glob_permissive' must be TRUE, FALSE or an integer.")
  }
  if (length(path_glob_permissive) != 1) {
    stop("Parameter 'path_glob_permissive' must be of length 1.")
  }
  
  # Check retrieve
  if (!is.logical(retrieve)) {
    stop("Parameter 'retrieve' must be TRUE or FALSE.")
  }
  
  # Check num_procs
  if (!is.null(num_procs)) {
    if (!is.numeric(num_procs)) {
      stop("Parameter 'num_procs' must be numeric.")
    } else {
      num_procs <- round(num_procs)
    }
  }
  
  # Check silent
  if (!is.logical(silent)) {
    stop("Parameter 'silent' must be logical.")
  }
  
  dim_params[[found_pattern_dim]] <- dat_names
  
  if (!silent) {
    .message(paste0("Exploring files... This will take a variable amount ",
                    "of time depending on the issued request and the ",
                    "performance of the file server..."))
  }
  
  if (!is.character(debug)) {
    dims_to_check <- c('time')
  } else {
    dims_to_check <- debug
    debug <- TRUE
  }
  
  ############################## READING FILE DIMS ############################
  # Check that no unrecognized variables are present in the path patterns
  # and also that no file dimensions are requested to THREDDs catalogs.
  # And in the mean time, build all the work pieces and look for the 
  # first available file of each dataset.
  array_of_files_to_load <- NULL
  array_of_not_found_files <- NULL
  indices_of_first_files_with_data <- vector('list', length(dat))
  selectors_of_first_files_with_data <- vector('list', length(dat))
  dataset_has_files <- rep(FALSE, length(dat))
  found_file_dims <- vector('list', length(dat))
  expected_inner_dims <- vector('list', length(dat))
  
  #print("A")
  for (i in 1:length(dat)) {
    #print("B")
    dat_selectors <- dim_params
    dat_selectors[[found_pattern_dim]] <- dat_selectors[[found_pattern_dim]][i]
    dim_vars <- paste0('$', dim_names, '$')
    file_dims <- which(sapply(dim_vars, grepl, dat[[i]][['path']], fixed = TRUE))
    if (length(file_dims) > 0) {
      file_dims <- dim_names[file_dims]
    }
    file_dims <- unique(c(pattern_dims, file_dims))
    found_file_dims[[i]] <- file_dims
    expected_inner_dims[[i]] <- dim_names[which(!(dim_names %in% file_dims))]
    # (Check the depending_file_dims).
    if (any(c(names(depending_file_dims), unlist(depending_file_dims)) %in% 
            expected_inner_dims[[i]])) {
      stop(paste0("The dimension dependancies specified in ",
                  "'depending_file_dims' can only be between file ",
                  "dimensions, but some inner dimensions found in ",
                  "dependancies for '", dat[[i]][['name']], "', which ",
                  "has the following file dimensions: ", 
                  paste(paste0("'", file_dims, "'"), collapse = ', '), ".")) 
    } else {
      a <- names(depending_file_dims) %in% file_dims
      b <- unlist(depending_file_dims) %in% file_dims
      ab <- a & b
      if (any(!ab)) {
        .warning(paste0("Detected some dependancies in 'depending_file_dims' with ",
                        "non-existing dimension names. These will be disregarded."))
        depending_file_dims <- depending_file_dims[-which(!ab)]
      }
      if (any(names(depending_file_dims) == unlist(depending_file_dims))) {
        depending_file_dims <- depending_file_dims[-which(names(depending_file_dims) == unlist(depending_file_dims))]
      }
    }
    # (Check the inner_dims_across_files).
    if (any(!(names(inner_dims_across_files) %in% file_dims)) ||
        any(!(unlist(inner_dims_across_files) %in% expected_inner_dims[[i]]))) {
      stop(paste0("All relationships specified in ",
                  "'_across' parameters must be between a inner ",
                  "dimension and a file dimension. Found wrong ",
                  "specification for '", dat[[i]][['name']], "', which ",
                  "has the following file dimensions: ", 
                  paste(paste0("'", file_dims, "'"), collapse = ', '),
                  ", and the following inner dimensions: ", 
                  paste(paste0("'", expected_inner_dims[[i]], "'"), 
                        collapse = ', '), "."))
    }
    # (Check the return_vars).
    j <- 1
    while (j <= length(return_vars)) {
      if (any(!(return_vars[[j]] %in% file_dims))) {
        if (any(return_vars[[j]] %in% expected_inner_dims[[i]])) {
          stop("Found variables in 'return_vars' requested ",
               "for some inner dimensions (for dataset '",
               dat[[i]][['name']], "'), but variables can only be ",
               "requested for file dimensions.")
        } else {
          stop("Found variables in 'return_vars' requested ",
               "for non-existing dimensions.")
        }
      }
      j <- j + 1
    }
    # (Check the metadata_dims).
    if (!is.null(metadata_dims)) {
      if (any(!(metadata_dims %in% file_dims))) {
        stop("All dimensions in 'metadata_dims' must be file dimensions.")
      }
    }
    ## Look for _var params that should be requested automatically.
    for (dim_name in dim_names) {
      if (!(dim_name %in% pattern_dims)) {
        if (is.null(attr(dat_selectors[[dim_name]], 'values')) ||
            is.null(attr(dat_selectors[[dim_name]], 'indices'))) {
          flag <- ((any(dat_selectors[[dim_name]] %in% c('all', 'first', 'last'))) || 
                     (is.numeric(unlist(dat_selectors[[dim_name]]))))
          attr(dat_selectors[[dim_name]], 'values') <- !flag
          attr(dat_selectors[[dim_name]], 'indices') <- flag
        }
        ## The following code 'rewrites' var_params for all datasets. If providing different
        ## path pattern repositories with different file/inner dimensions, var_params might
        ## have to be handled for each dataset separately.
        if ((attr(dat_selectors[[dim_name]], 'values') || (dim_name %in% c('var', 'variable'))) &&
            !(dim_name %in% names(var_params)) && !(dim_name %in% file_dims))  {
          if (dim_name %in% c('var', 'variable')) {
            var_params <- c(var_params, setNames(list('var_names'), dim_name))
            .warning(paste0("Found specified values for dimension '", dim_name, "' but no '", 
                            dim_name, "_var' requested. ", '"', dim_name, "_var = '", 
                            'var_names', "'", '"', " has been automatically added to ",
                            "the Start call."))
          } else {
            var_params <- c(var_params, setNames(list(dim_name), dim_name))
            .warning(paste0("Found specified values for dimension '", dim_name, "' but no '", 
                            dim_name, "_var' requested. ", '"', dim_name, "_var = '", 
                            dim_name, "'", '"', " has been automatically added to ",
                            "the Start call."))
          }
        }
      }
    }
    ## (Check the *_var parameters).
    if (any(!(unlist(var_params) %in% names(return_vars)))) {
      vars_to_add <- which(!(unlist(var_params) %in% names(return_vars)))
      new_return_vars <- vector('list', length(vars_to_add))
      names(new_return_vars) <- unlist(var_params)[vars_to_add]
      return_vars <- c(return_vars, new_return_vars)
      .warning(paste0("All '*_var' params must associate a dimension to one of the ",
                      "requested variables in 'return_vars'. The following variables",
                      " have been added to 'return_vars': ", 
                      paste(paste0("'", unlist(var_params), "'"), collapse = ', ')))
    }
    
    replace_values <- vector('list', length = length(file_dims))
    names(replace_values) <- file_dims
    # Take the first selector for all possible file dimensions
    for (file_dim in file_dims) {
      if (file_dim %in% names(var_params)) {
        .warning(paste0("The '", file_dim, "_var' param will be ignored since '", 
                        file_dim, "' is a file dimension (for the dataset with pattern ", 
                        dat[[i]][['path']], ")."))
      }
      if (!is.list(dat_selectors[[file_dim]]) || 
          (is.list(dat_selectors[[file_dim]]) && 
           length(dat_selectors[[file_dim]]) == 2 &&
           is.null(names(dat_selectors[[file_dim]])))) {
        dat_selectors[[file_dim]] <- list(dat_selectors[[file_dim]])
      }
      first_class <- class(dat_selectors[[file_dim]][[1]])
      first_length <- length(dat_selectors[[file_dim]][[1]])
      for (j in 1:length(dat_selectors[[file_dim]])) {
        sv <- selector_vector <- dat_selectors[[file_dim]][[j]]
        if (!identical(first_class, class(sv)) ||
            !identical(first_length, length(sv))) {
          stop("All provided selectors for depending dimensions must ",
               "be vectors of the same length and of the same class.")
        }
        if (is.character(sv) && !((length(sv) == 1) && (sv[1] %in% c('all', 'first', 'last')))) {
          dat_selectors[[file_dim]][[j]] <- selector_checker(selectors = sv,
                                                             return_indices = FALSE)
          # Take chunk if needed
          dat_selectors[[file_dim]][[j]] <- dat_selectors[[file_dim]][[j]][chunk_indices(length(dat_selectors[[file_dim]][[j]]),
                                                                                         chunks[[file_dim]]['chunk'],
                                                                                         chunks[[file_dim]]['n_chunks'], 
                                                                                         file_dim)]
        } else if (!(is.numeric(sv) || 
                     (is.character(sv) && (length(sv) == 1) && (sv %in% c('all', 'first', 'last'))) || 
                     (is.list(sv) && (length(sv) == 2) && (all(sapply(sv, is.character)) || 
                                                           all(sapply(sv, is.numeric)))))) {
          stop("All explicitly provided selectors for file dimensions must be character strings.")
        } 
      }
      sv <- dat_selectors[[file_dim]][[1]]
      if (is.character(sv) && !((length(sv) == 1) && (sv[1] %in% c('all', 'first', 'last')))) {
        replace_values[[file_dim]] <- dat_selectors[[file_dim]][[1]][1]
      }
    }
    #print("C")
    # Now we know which dimensions whose selectors are provided non-explicitly.
    undefined_file_dims <- file_dims[which(sapply(replace_values, is.null))]
    defined_file_dims <- file_dims[which(!(file_dims %in% undefined_file_dims))]
    # Quickly check if the depending dimensions are provided properly.
    for (file_dim in file_dims) {
      if (file_dim %in% names(depending_file_dims)) {
        ## TODO: Detect multi-dependancies and forbid.
        if (all(c(file_dim, depending_file_dims[[file_dim]]) %in% defined_file_dims)) {
          if (length(dat_selectors[[file_dim]]) != length(dat_selectors[[depending_file_dims[[file_dim]]]][[1]])) {
            stop(paste0("If providing selectors for the depending ",
                        "dimension '", file_dim, "', a ",
                        "vector of selectors must be provided for ",
                        "each selector of the dimension it depends on, '",
                        depending_file_dims[[file_dim]], "'."))
          } else if (!all(names(dat_selectors[[file_dim]]) == dat_selectors[[depending_file_dims[[file_dim]]]][[1]])) {
            stop(paste0("If providing selectors for the depending ",
                        "dimension '", file_dim, "', the name of the ",
                        "provided vectors of selectors must match ",
                        "exactly the selectors of the dimension it ",
                        "depends on, '", depending_file_dims[[file_dim]], "'."))
          }
        }
      }
    }
    # Find the possible values for the selectors that are provided as
    # indices. If the requested file is on server, impossible operation.
    if (length(grep("^http", dat[[i]][['path']])) > 0) {
      if (length(undefined_file_dims) > 0) {
        stop(paste0("All selectors for the file dimensions must be ",
                    "character strings if requesting data to a remote ",
                    "server. Found invalid selectors for the file dimensions ",
                    paste(paste0("'", undefined_file_dims, "'"), collapse = ', '), "."))
      }
      dataset_has_files[i] <- TRUE
    } else {
      dat[[i]][['path']] <- path.expand(dat[[i]][['path']])
      # Iterate over the known dimensions to find the first existing file.
      # The path to the first existing file will be used to find the 
      # values for the non explicitly defined selectors.
      first_file <- NULL
      first_file_selectors <- NULL
      if (length(undefined_file_dims) > 0) {
        replace_values[undefined_file_dims] <- '*'
      }
      ## TODO: What if length of defined_file_dims is 0? code might crash (in practice it worked for an example case)
      files_to_check <- sapply(dat_selectors[defined_file_dims], function(x) length(x[[1]]))
      sub_array_of_files_to_check <- array(1:prod(files_to_check), dim = files_to_check)
      j <- 1
      #print("D")
      while (j <= prod(files_to_check) && is.null(first_file)) {
        selector_indices <- which(sub_array_of_files_to_check == j, arr.ind = TRUE)[1, ]
        selectors <- sapply(1:length(defined_file_dims), 
                            function (x) {
                              vector_to_pick <- 1
                              if (defined_file_dims[x] %in% names(depending_file_dims)) {
                                vector_to_pick <- selector_indices[which(defined_file_dims == depending_file_dims[[defined_file_dims[x]]])]
                              }
                              dat_selectors[defined_file_dims][[x]][[vector_to_pick]][selector_indices[x]]
                            })
        replace_values[defined_file_dims] <- selectors
        file_path <- .ReplaceVariablesInString(dat[[i]][['path']], replace_values)
        file_path <- Sys.glob(file_path)
        if (length(file_path) > 0) {
          first_file <- file_path[1]
          first_file_selectors <- selectors
        }
        j <- j + 1
      }
      #print("E")
      # Start looking for values for the non-explicitly defined selectors.
      if (is.null(first_file)) {
        .warning(paste0("No found files for the datset '", dat[[i]][['name']], 
                        "'. Provide existing selectors for the file dimensions ",
                        " or check and correct its path pattern: ", dat[[i]][['path']]))
      } else {
        dataset_has_files[i] <- TRUE
        ## TODO: Improve message here if no variable found:
        if (length(undefined_file_dims) > 0) {
          # Looking for the first values, parsed from first_file.
          first_values <- vector('list', length = length(undefined_file_dims))
          names(first_values) <- undefined_file_dims
          found_values <- 0
          stop <- FALSE
          try_dim <- 1
          last_success <- 1
          while ((found_values < length(undefined_file_dims)) && !stop) {
            u_file_dim <- undefined_file_dims[try_dim]
            if (is.null(first_values[[u_file_dim]])) {
              path_with_globs_and_tag <- .ReplaceVariablesInString(dat[[i]][['path']], 
                                                                   replace_values[-which(file_dims == u_file_dim)],
                                                                   allow_undefined_key_vars = TRUE)
              found_value <- .FindTagValue(path_with_globs_and_tag, 
                                           first_file, u_file_dim)
              if (!is.null(found_value)) {
                found_values <- found_values + 1
                last_success <- try_dim
                first_values[[u_file_dim]] <- found_value
                replace_values[[u_file_dim]] <- found_value
              }
            }
            try_dim <- (try_dim %% length(undefined_file_dims)) + 1
            if (try_dim == last_success) {
              stop <- TRUE
            }
          }
          if (found_values < length(undefined_file_dims)) {
            stop(paste0("Path pattern of dataset '", dat[[i]][['name']], 
                        "' is too complex. Could not automatically ",
                        "detect values for all non-explicitly defined ",
                        "indices. Check its pattern: ", dat[[i]][['path']]))
          }
          ## TODO: Replace ReplaceGlobExpressions by looped call to FindTagValue? As done above
          ##       Maybe it can solve more cases actually. I got warnings in ReplGlobExp with a typical
          ##       cmor case, requesting all members and chunks for fixed var and sdate. Not fixing
          ##       sdate raised 'too complex' error.
          # Replace shell globs in path pattern and keep the file_dims as tags
          dat[[i]][['path']] <- .ReplaceGlobExpressions(dat[[i]][['path']], first_file, replace_values, 
                                                        file_dims, dat[[i]][['name']], path_glob_permissive)
          # Now time to look for the available values for the non 
          # explicitly defined selectors for the file dimensions.
          #print("H")
          # Check first the ones that do not depend on others.
          ufd <- c(undefined_file_dims[which(!(undefined_file_dims %in% names(depending_file_dims)))],
                   undefined_file_dims[which(undefined_file_dims %in% names(depending_file_dims))])
          
          for (u_file_dim in ufd) {
            replace_values[undefined_file_dims] <- first_values
            replace_values[[u_file_dim]] <- '*'
            depended_dim <- NULL
            depended_dim_values <- NA

#NOTE: Here 'selectors' is always 1. Is it supposed to be like this?
            selectors <- dat_selectors[[u_file_dim]][[1]]
            if (u_file_dim %in% names(depending_file_dims)) {
              depended_dim <- depending_file_dims[[u_file_dim]]
              depended_dim_values <- dat_selectors[[depended_dim]][[1]]
              dat_selectors[[u_file_dim]] <- vector('list', length = length(depended_dim_values))
              names(dat_selectors[[u_file_dim]]) <- depended_dim_values
            } else {
              dat_selectors[[u_file_dim]] <- list()
            }
            if (u_file_dim %in% unlist(depending_file_dims)) {
              depending_dims <- names(depending_file_dims)[which(sapply(depending_file_dims, function(x) u_file_dim %in% x))]
              replace_values[depending_dims] <- rep('*', length(depending_dims))
            }
            for (j in 1:length(depended_dim_values)) {
              parsed_values <- c()
              if (!is.null(depended_dim)) {
                replace_values[[depended_dim]] <- depended_dim_values[j]
              }
              path_with_globs <- .ReplaceVariablesInString(dat[[i]][['path']], replace_values)
              found_files <- Sys.glob(path_with_globs)
              ## TODO: Enhance this error message, or change by warning.
              ##       Raises if a wrong sdate is specified, for example.
              if (length(found_files) == 0) {
                .warning(paste0("Could not find files for any '", u_file_dim, 
                                "' for '", depended_dim, "' = '", 
                                depended_dim_values[j], "'."))
                dat_selectors[[u_file_dim]][[j]] <- NA
              } else {
                for (found_file in found_files) {
                  path_with_globs_and_tag <- .ReplaceVariablesInString(dat[[i]][['path']],
                                                                       replace_values[-which(file_dims == u_file_dim)],
                                                                       allow_undefined_key_vars = TRUE)
                  parsed_values <- c(parsed_values, 
                                     .FindTagValue(path_with_globs_and_tag, found_file, 
                                                   u_file_dim))
                }
                dat_selectors[[u_file_dim]][[j]] <- selector_checker(selectors = selectors,
                                                                     var = unique(parsed_values),
                                                                     return_indices = FALSE)
                # Take chunk if needed
                dat_selectors[[u_file_dim]][[j]] <- dat_selectors[[u_file_dim]][[j]][chunk_indices(length(dat_selectors[[u_file_dim]][[j]]),
                                                                                                   chunks[[u_file_dim]]['chunk'],
                                                                                                   chunks[[u_file_dim]]['n_chunks'], 
                                                                                                   u_file_dim)]
              }
            }
          }
          #print("I")
        } else {
          dat[[i]][['path']] <- .ReplaceGlobExpressions(dat[[i]][['path']], first_file, replace_values, 
                                                        defined_file_dims, dat[[i]][['name']], path_glob_permissive)
        }
      }
    }
    # Now fetch for the first available file
    if (dataset_has_files[i]) {
      known_dims <- file_dims
    } else {
      known_dims <- defined_file_dims
    }
    replace_values <- vector('list', length = length(known_dims))
    names(replace_values) <- known_dims
    files_to_load <- sapply(dat_selectors[known_dims], function(x) length(x[[1]]))
    files_to_load[found_pattern_dim] <- 1
    sub_array_of_files_to_load <- array(1:prod(files_to_load), 
                                        dim = files_to_load)
    names(dim(sub_array_of_files_to_load)) <- known_dims
    sub_array_of_not_found_files <- array(!dataset_has_files[i], 
                                          dim = files_to_load)
    names(dim(sub_array_of_not_found_files)) <- known_dims
    j <- 1
    selector_indices_save <- vector('list', prod(files_to_load))
    while (j <= prod(files_to_load)) {
      selector_indices <- which(sub_array_of_files_to_load == j, arr.ind = TRUE)[1, ]
      names(selector_indices) <- known_dims
      selector_indices_save[[j]] <- selector_indices
      selectors <- sapply(1:length(known_dims), 
                          function (x) {
                            vector_to_pick <- 1
                            if (known_dims[x] %in% names(depending_file_dims)) {
                              vector_to_pick <- selector_indices[which(known_dims == depending_file_dims[[known_dims[x]]])]
                            }
                            dat_selectors[known_dims][[x]][[vector_to_pick]][selector_indices[x]]
                          })
      names(selectors) <- known_dims
      replace_values[known_dims] <- selectors
      if (!dataset_has_files[i]) {
        if (any(is.na(selectors))) {
          replace_values <- replace_values[-which(names(replace_values) %in% names(selectors[which(is.na(selectors))]))]
        }
        file_path <- .ReplaceVariablesInString(dat[[i]][['path']], replace_values, TRUE)
        sub_array_of_files_to_load[j] <- file_path
        #sub_array_of_not_found_files[j] <- TRUE???
      } else {
        if (any(is.na(selectors))) {
          replace_values <- replace_values[-which(names(replace_values) %in% names(selectors[which(is.na(selectors))]))]
          file_path <- .ReplaceVariablesInString(dat[[i]][['path']], replace_values, TRUE)
          sub_array_of_files_to_load[j] <- file_path
          sub_array_of_not_found_files[j] <- TRUE
        } else {
          file_path <- .ReplaceVariablesInString(dat[[i]][['path']], replace_values)

#NOTE: After replacing tags, there is still * if path_glob_permissive is not FALSE.
          if (grepl('\\*', file_path)) {
            found_files <- Sys.glob(file_path)
            file_path <- found_files[1]   # choose only the first file.
#NOTE: Above line chooses only the first found file. Because * is not tags, which means
#      it is not a dimension. So it cannot store more than one item. If use * to define
#      the path, that * should only represent one possibility.
            if (length(found_files) > 1) {
              .warning("Using glob expression * to define the path, but more ",
                       "than one match is found. Choose the first match only.")
            }
          }

          if (!(length(grep("^http", file_path)) > 0)) {
            if (grepl(file_path, '*', fixed = TRUE)) {
              file_path_full <- Sys.glob(file_path)[1]
              if (nchar(file_path_full) > 0) {
                file_path <- file_path_full
              }
            }
          }
          sub_array_of_files_to_load[j] <- file_path
          if (is.null(indices_of_first_files_with_data[[i]])) {
            if (!(length(grep("^http", file_path)) > 0)) {
              if (!file.exists(file_path)) {
                file_path <- NULL
              }
            }
            if (!is.null(file_path)) {
              test_file <- NULL
              ## TODO: suppress error messages
              test_file <- file_opener(file_path)
              if (!is.null(test_file)) {
                selector_indices[which(known_dims == found_pattern_dim)] <- i
                indices_of_first_files_with_data[[i]] <- selector_indices
                selectors_of_first_files_with_data[[i]] <- selectors
                file_closer(test_file)
              }
            }
          }
        }
      }
      j <- j + 1
    }
    # Extend array as needed progressively
    if (is.null(array_of_files_to_load)) {
      array_of_files_to_load <- sub_array_of_files_to_load
      array_of_not_found_files <- sub_array_of_not_found_files
    } else {
      array_of_files_to_load <- .MergeArrays(array_of_files_to_load, sub_array_of_files_to_load,
                                             along = found_pattern_dim)
      ## TODO: file_dims, and variables like that.. are still ok now? I don't think so
      array_of_not_found_files <- .MergeArrays(array_of_not_found_files, sub_array_of_not_found_files,
                                               along = found_pattern_dim)
    }    
    dat[[i]][['selectors']] <- dat_selectors
  }
  if (all(sapply(indices_of_first_files_with_data, is.null))) {
    stop("No data files found for any of the specified datasets.")
  }
  
  ########################### READING INNER DIMS. #############################
  #print("J")
  ## TODO: To be run in parallel (local multi-core)
  # Now time to work out the inner file dimensions.
  # First pick the requested variables.
  dims_to_iterate <- NULL
  for (return_var in names(return_vars)) {
    dims_to_iterate <- unique(c(dims_to_iterate, return_vars[[return_var]]))
  }
  if (found_pattern_dim %in% dims_to_iterate) {
    dims_to_iterate <- dims_to_iterate[-which(dims_to_iterate == found_pattern_dim)]
  }
  common_return_vars <- NULL
  common_first_found_file <- NULL
  common_return_vars_pos <- NULL
  if (length(return_vars) > 0) {
    common_return_vars_pos <- which(sapply(return_vars, function(x) !(found_pattern_dim %in% x)))
  }
  if (length(common_return_vars_pos) > 0) {
    common_return_vars <- return_vars[common_return_vars_pos]
    return_vars <- return_vars[-common_return_vars_pos]
    common_first_found_file <- rep(FALSE, length(which(sapply(common_return_vars, length) == 0)))
    names(common_first_found_file) <- names(common_return_vars[which(sapply(common_return_vars, length) == 0)])
  }
  return_vars <- lapply(return_vars, 
                        function(x) {
                          if (found_pattern_dim %in% x) {
                            x[-which(x == found_pattern_dim)]
                          } else {
                            x
                          }
                        })
  if (length(common_return_vars) > 0) {
    picked_common_vars <- vector('list', length = length(common_return_vars))
    names(picked_common_vars) <- names(common_return_vars)
  } else {
    picked_common_vars <- NULL
  }
  picked_common_vars_ordered <- picked_common_vars
  picked_common_vars_unorder_indices <- picked_common_vars
  picked_vars <- vector('list', length = length(dat))
  names(picked_vars) <- dat_names
  picked_vars_ordered <- picked_vars
  picked_vars_unorder_indices <- picked_vars
  for (i in 1:length(dat)) {
    if (dataset_has_files[i]) {
      # Put all selectors in a list of a single list/vector of selectors. 
      # The dimensions that go across files will later be extended to have 
      # lists of lists/vectors of selectors.
      for (inner_dim in expected_inner_dims[[i]]) {
        if (!is.list(dat[[i]][['selectors']][[inner_dim]]) || 
            (is.list(dat[[i]][['selectors']][[inner_dim]]) && 
             length(dat[[i]][['selectors']][[inner_dim]]) == 2 &&
             is.null(names(dat[[i]][['selectors']][[inner_dim]])))) {
          dat[[i]][['selectors']][[inner_dim]] <- list(dat[[i]][['selectors']][[inner_dim]])
        }
      }
      if (length(return_vars) > 0) {
        picked_vars[[i]] <- vector('list', length = length(return_vars))
        names(picked_vars[[i]]) <- names(return_vars)
        picked_vars_ordered[[i]] <- picked_vars[[i]]
        picked_vars_unorder_indices[[i]] <- picked_vars[[i]]
      }
      indices_of_first_file <- as.list(indices_of_first_files_with_data[[i]])
      array_file_dims <- sapply(dat[[i]][['selectors']][found_file_dims[[i]]], function(x) length(x[[1]]))
      names(array_file_dims) <- found_file_dims[[i]]
      if (length(dims_to_iterate) > 0) {
        indices_of_first_file[dims_to_iterate] <- lapply(array_file_dims[dims_to_iterate], function(x) 1:x)
      }
      array_of_var_files <- do.call('[', c(list(x = array_of_files_to_load), indices_of_first_file, list(drop = FALSE)))
      array_of_var_indices <- array(1:length(array_of_var_files), dim = dim(array_of_var_files))
      array_of_not_found_var_files <- do.call('[', c(list(x = array_of_not_found_files), indices_of_first_file, list(drop = FALSE)))
      previous_indices <- rep(-1, length(indices_of_first_file))
      names(previous_indices) <- names(indices_of_first_file)
      first_found_file <- NULL
      if (length(return_vars) > 0) {
        first_found_file <- rep(FALSE, length(which(sapply(return_vars, length) == 0)))
        names(first_found_file) <- names(return_vars[which(sapply(return_vars, length) == 0)])
      }
      for (j in 1:length(array_of_var_files)) {
        current_indices <- which(array_of_var_indices == j, arr.ind = TRUE)[1, ]
        names(current_indices) <- names(indices_of_first_file)
        if (!is.na(array_of_var_files[j]) && !array_of_not_found_var_files[j]) {
          changed_dims <- which(current_indices != previous_indices)
          vars_to_read <- NULL
          if (length(return_vars) > 0) {
            vars_to_read <- names(return_vars)[sapply(return_vars, function(x) any(names(changed_dims) %in% x))]
          }
          if (!is.null(first_found_file)) {
            if (any(!first_found_file)) {
              vars_to_read <- c(vars_to_read, names(first_found_file[which(!first_found_file)]))
            }
          }
          if ((i == 1) && (length(common_return_vars) > 0)) {
            vars_to_read <- c(vars_to_read, names(common_return_vars)[sapply(common_return_vars, function(x) any(names(changed_dims) %in% x))])
          }
          if (!is.null(common_first_found_file)) {
            if (any(!common_first_found_file)) {
              vars_to_read <- c(vars_to_read, names(common_first_found_file[which(!common_first_found_file)]))
            }
          }
          file_object <- file_opener(array_of_var_files[j])
          if (!is.null(file_object)) {
            for (var_to_read in vars_to_read) {
              if (var_to_read %in% unlist(var_params)) {
                associated_dim_name <- names(var_params)[which(unlist(var_params) == var_to_read)]
              }
              var_name_to_reader <- var_to_read
              names(var_name_to_reader) <- 'var'
              var_dims <- file_dim_reader(NULL, file_object, var_name_to_reader, NULL,
                                          synonims)
              # file_dim_reader returns dimension names as found in the file.
              # Need to translate accoridng to synonims:
              names(var_dims) <- sapply(names(var_dims), 
                                        function(x) {
                                          which_entry <- which(sapply(synonims, function(y) x %in% y))
                                          if (length(which_entry) > 0) {
                                            names(synonims)[which_entry]
                                          } else {
                                            x
                                          }
                                        })
              if (!is.null(var_dims)) {
                var_file_dims <- NULL
                if (var_to_read %in% names(common_return_vars)) {
                  var_to_check <- common_return_vars[[var_to_read]]
                } else {
                  var_to_check <- return_vars[[var_to_read]]
                }
                if (any(names(dim(array_of_files_to_load)) %in% var_to_check)) {
                  var_file_dims <- dim(array_of_files_to_load)[which(names(dim(array_of_files_to_load)) %in% 
                                                                       var_to_check)]
                }
                if (((var_to_read %in% names(common_return_vars)) && 
                     is.null(picked_common_vars[[var_to_read]])) ||
                    ((var_to_read %in% names(return_vars)) && 
                     is.null(picked_vars[[i]][[var_to_read]]))) {
                  if (any(names(var_file_dims) %in% names(var_dims))) {
                    stop("Found a requested var in 'return_var' requested for a ",
                         "file dimension which also appears in the dimensions of ",
                         "the variable inside the file.\n", array_of_var_files[j])
                  }
                  special_types <- list('POSIXct' = as.POSIXct, 'POSIXlt' = as.POSIXlt, 
                                        'Date' = as.Date)
                  first_sample <- file_var_reader(NULL, file_object, NULL, 
                                                  var_to_read, synonims)
                  if (any(class(first_sample) %in% names(special_types))) {
                    array_size <- prod(c(var_file_dims, var_dims))
                    new_array <- rep(special_types[[class(first_sample)[1]]](NA), array_size)
                    dim(new_array) <- c(var_file_dims, var_dims)
                  } else {
                    new_array <- array(dim = c(var_file_dims, var_dims))
                  }
                  attr(new_array, 'variables') <- attr(first_sample, 'variables')
                  if (var_to_read %in% names(common_return_vars)) {
                    picked_common_vars[[var_to_read]] <- new_array
                    pick_ordered <- FALSE 
                    if (var_to_read %in% unlist(var_params)) {
                      if (associated_dim_name %in% names(dim_reorder_params) && !aiat) {
                        picked_common_vars_ordered[[var_to_read]] <- new_array
                        pick_ordered <- TRUE
                      }
                    }
                    if (!pick_ordered) {
                      picked_common_vars_ordered[[var_to_read]] <- NULL
                    }
                  } else {
                    picked_vars[[i]][[var_to_read]] <- new_array
                    pick_ordered <- FALSE
                    if (var_to_read %in% unlist(var_params)) {
                      if (associated_dim_name %in% names(dim_reorder_params) && !aiat) {
                        picked_vars_ordered[[i]][[var_to_read]] <- new_array
                        pick_ordered <- TRUE
                      }
                    }
                    if (!pick_ordered) {
                      picked_vars_ordered[[i]][[var_to_read]] <- NULL
                    }
                  }
                } else {
                  if (var_to_read %in% names(common_return_vars)) {
                    array_var_dims <- dim(picked_common_vars[[var_to_read]])
                  } else {
                    array_var_dims <- dim(picked_vars[[i]][[var_to_read]])
                  }
                  full_array_var_dims <- array_var_dims
                  if (any(names(array_var_dims) %in% names(var_file_dims))) {
                    array_var_dims <- array_var_dims[-which(names(array_var_dims) %in% names(var_file_dims))]
                  }
                  if (names(array_var_dims) != names(var_dims)) {
                    stop("Error while reading the variable '", var_to_read, "' from ",
                         "the file. Dimensions do not match.\nExpected ", 
                         paste(paste0("'", names(array_var_dims), "'"), 
                               collapse = ', '), " but found ",
                         paste(paste0("'", names(var_dims), "'"), 
                               collapse = ', '), ".\n", array_of_var_files[j])
                  }
                  if (any(var_dims > array_var_dims)) {
                    longer_dims <- which(var_dims > array_var_dims)
                    if (length(longer_dims) == 1) {
                      longer_dims_in_full_array <- longer_dims
                      if (any(names(full_array_var_dims) %in% names(var_file_dims))) {
                        candidates <- (1:length(full_array_var_dims))[-which(names(full_array_var_dims) %in% names(var_file_dims))]
                        longer_dims_in_full_array <- candidates[longer_dims]
                      }
                      padding_dims <- full_array_var_dims
                      padding_dims[longer_dims_in_full_array] <- var_dims[longer_dims] - 
                        array_var_dims[longer_dims]
                      special_types <- list('POSIXct' = as.POSIXct, 'POSIXlt' = as.POSIXlt,
                                            'Date' = as.Date)
                      if (var_to_read %in% names(common_return_vars)) {
                        var_class <- class(picked_common_vars[[var_to_read]])
                      } else {
                        var_class <- class(picked_vars[[i]][[var_to_read]])
                      }
                      if (any(var_class %in% names(special_types))) {
                        padding_size <- prod(padding_dims)
                        padding <- rep(special_types[[var_class[1]]](NA), padding_size)
                        dim(padding) <- padding_dims
                      } else {
                        padding <- array(dim = padding_dims)
                      }
                      if (var_to_read %in% names(common_return_vars)) {
                        picked_common_vars[[var_to_read]] <- .abind2(
                          picked_common_vars[[var_to_read]],
                          padding,
                          names(full_array_var_dims)[longer_dims_in_full_array]
                        )
                      } else {
                        picked_vars[[i]][[var_to_read]] <- .abind2(
                          picked_vars[[i]][[var_to_read]],
                          padding,
                          names(full_array_var_dims)[longer_dims_in_full_array]
                        )
                      }
                    } else {
                      stop("Error while reading the variable '", var_to_read, "' from ",
                           "the file. Found size (", paste(var_dims, collapse = ' x '), 
                           ") is greater than expected maximum size (", 
                           array_var_dims, ").")
                    }
                  }
                }
                var_store_indices <- c(as.list(current_indices[names(var_file_dims)]), lapply(var_dims, function(x) 1:x))
                var_values <- file_var_reader(NULL, file_object, NULL, var_to_read, synonims)
                if (var_to_read %in% unlist(var_params)) {
                  if ((associated_dim_name %in% names(dim_reorder_params)) && !aiat) {
                    ## Is this check really needed?
                    if (length(dim(var_values)) > 1) {
                      stop("Requested a '", associated_dim_name, "_reorder' for a dimension ",
                           "whose coordinate variable that has more than 1 dimension. This is ",
                           "not supported.")
                    }
                    ordered_var_values <- dim_reorder_params[[associated_dim_name]](var_values)
                    attr(ordered_var_values$x, 'variables') <- attr(var_values, 'variables')
                    if (!all(c('x', 'ix') %in% names(ordered_var_values))) {
                      stop("All the dimension reorder functions must return a list with the components 'x' and 'ix'.")
                    }
                    # Save the indices to reorder back the ordered variable values.
                    # This will be used to define the first round indices.
                    unorder <- sort(ordered_var_values$ix, index.return = TRUE)$ix
                    if (var_to_read %in% names(common_return_vars)) {
                      picked_common_vars_ordered[[var_to_read]] <- do.call('[<-', 
                                                                           c(list(x = picked_common_vars_ordered[[var_to_read]]), 
                                                                             var_store_indices, 
                                                                             list(value = ordered_var_values$x)))
                      picked_common_vars_unorder_indices[[var_to_read]] <- do.call('[<-', 
                                                                                   c(list(x = picked_common_vars_unorder_indices[[var_to_read]]), 
                                                                                     var_store_indices, 
                                                                                     list(value = unorder)))
                    } else {
                      picked_vars_ordered[[i]][[var_to_read]] <- do.call('[<-', 
                                                                         c(list(x = picked_vars_ordered[[i]][[var_to_read]]), 
                                                                           var_store_indices, 
                                                                           list(value = ordered_var_values$x)))
                      picked_vars_unorder_indices[[i]][[var_to_read]] <- do.call('[<-', 
                                                                                 c(list(x = picked_vars_unorder_indices[[i]][[var_to_read]]), 
                                                                                   var_store_indices, 
                                                                                   list(value = unorder)))
                    }
                  }
                }
                if (var_to_read %in% names(common_return_vars)) {
                  picked_common_vars[[var_to_read]] <- do.call('[<-', 
                                                               c(list(x = picked_common_vars[[var_to_read]]), 
                                                                 var_store_indices, 
                                                                 list(value = var_values)))
                  # Turn time zone back to UTC if this var_to_read is 'time'
                  if (all(class(picked_common_vars[[var_to_read]]) == c('POSIXct', 'POSIXt'))) {
                    attr(picked_common_vars[[var_to_read]], "tzone") <- 'UTC'
                  }
                } else {
                  picked_vars[[i]][[var_to_read]] <- do.call('[<-', 
                                                             c(list(x = picked_vars[[i]][[var_to_read]]), 
                                                               var_store_indices, 
                                                               list(value = var_values)))
                  # Turn time zone back to UTC if this var_to_read is 'time'
                  if (all(class(picked_vars[[i]][[var_to_read]]) == c('POSIXct', 'POSIXt'))) {
                    attr(picked_vars[[i]][[var_to_read]], "tzone") <- 'UTC'
                  }
                }
                if (var_to_read %in% names(first_found_file)) {
                  first_found_file[var_to_read] <- TRUE
                }
                if (var_to_read %in% names(common_first_found_file)) {
                  common_first_found_file[var_to_read] <- TRUE
                }
              } else {
                stop("Could not find variable '", var_to_read, 
                     "' in the file ", array_of_var_files[j])
              }
            }
            file_closer(file_object)
          }
        }
        previous_indices <- current_indices
      }
    }
  }
  # Once we have the variable values, we can work out the indices
  # for the implicitly defined selectors.
  #
  # Trnasforms a vector of indices v expressed in a world of 
  # length N from 1 to N, into a world of length M, from
  # 1 to M. Repeated adjacent indices are collapsed.
  transform_indices <- function(v, n, m) { 
    #unique2 turns e.g. 1 1 2 2 2 3 3 1 1 1 into 1 2 3 1
    unique2 <- function(v) {
      if (length(v) < 2) {
        v
      } else {
        v[c(1, v[2:length(v)] - v[1:(length(v) - 1)]) != 0]
      }
    }
    unique2(round(((v - 1) / (n - 1)) * (m - 1))) + 1 # this rounding may generate 0s. what then?
  }
  beta <- transform_extra_cells
  dims_to_crop <- vector('list')
  transformed_vars <- vector('list', length = length(dat))
  names(transformed_vars) <- dat_names
  transformed_vars_ordered <- transformed_vars
  transformed_vars_unorder_indices <- transformed_vars
  transformed_common_vars <- NULL
  transformed_common_vars_ordered <- NULL
  transformed_common_vars_unorder_indices <- NULL
  
  for (i in 1:length(dat)) {
    if (dataset_has_files[i]) {
      indices <- indices_of_first_files_with_data[[i]]
      if (!is.null(indices)) {
        file_path <- do.call("[", c(list(array_of_files_to_load), as.list(indices_of_first_files_with_data[[i]])))
        # The following 5 lines should go several lines below, but were moved
        # here for better performance.
        # If any of the dimensions comes without defining variable, then we read
        # the data dimensions.
        data_dims <- NULL
        if (length(unlist(var_params[expected_inner_dims[[i]]])) < length(expected_inner_dims[[i]])) {
          file_to_open <- file_path
          data_dims <- file_dim_reader(file_to_open, NULL, selectors_of_first_files_with_data[[i]],
                                       lapply(dat[[i]][['selectors']][expected_inner_dims[[i]]], '[[', 1),
                                       synonims)
          # file_dim_reader returns dimension names as found in the file.
          # Need to translate accoridng to synonims:
          names(data_dims) <- sapply(names(data_dims),
                                     function(x) {
                                       which_entry <- which(sapply(synonims, function(y) x %in% y))
                                       if (length(which_entry) > 0) {
                                         names(synonims)[which_entry]
                                       } else {
                                         x
                                       }
                                     })
        }
        # Transform the variables if needed and keep them apart.
        if (!is.null(transform) && (length(transform_vars) > 0)) {
          if (!all(transform_vars %in% c(names(picked_vars[[i]]), names(picked_common_vars)))) {
            stop("Could not find all the required variables in 'transform_vars' ",
                 "for the dataset '", dat[[i]][['name']], "'.")
          }
          vars_to_transform <- NULL
          picked_vars_to_transform <- which(names(picked_vars[[i]]) %in% transform_vars)
          if (length(picked_vars_to_transform) > 0) {
            picked_vars_to_transform <- names(picked_vars[[i]])[picked_vars_to_transform]
            new_vars_to_transform <- picked_vars[[i]][picked_vars_to_transform]
            which_are_ordered <- which(!sapply(picked_vars_ordered[[i]][picked_vars_to_transform], is.null))
            
            ##NOTE: The following 'if' replaces the original with reordering vector
            if (length(which_are_ordered) > 0) {
              tmp <- which(!is.na(match(names(picked_vars_ordered[[i]]), names(which_are_ordered))))
              new_vars_to_transform[which_are_ordered] <- picked_vars_ordered[[i]][tmp]
              
            }
            vars_to_transform <- c(vars_to_transform, new_vars_to_transform)
          }
          
          ##NOTE: Above is non-common vars, here is common vars (ie, return_vars = NULL).
          picked_common_vars_to_transform <- which(names(picked_common_vars) %in% transform_vars)
          if (length(picked_common_vars_to_transform) > 0) {
            picked_common_vars_to_transform <- names(picked_common_vars)[picked_common_vars_to_transform]
            
            new_vars_to_transform <- picked_common_vars[picked_common_vars_to_transform]
            which_are_ordered <- which(!sapply(picked_common_vars_ordered[picked_common_vars_to_transform], is.null))
            
            if (length(which_are_ordered) > 0) {
              
              tmp <- which(!is.na(match(names(picked_common_vars_ordered), names(which_are_ordered))))
              new_vars_to_transform[which_are_ordered] <- picked_common_vars_ordered[tmp]
            }
            vars_to_transform <- c(vars_to_transform, new_vars_to_transform)
          }
          
          # Transform the variables
          transformed_data <- do.call(transform, c(list(data_array = NULL,
                                                        variables = vars_to_transform,
                                                        file_selectors = selectors_of_first_files_with_data[[i]]),
                                                   transform_params))
          # Discard the common transformed variables if already transformed before
          if (!is.null(transformed_common_vars)) {
            common_ones <- which(names(picked_common_vars) %in% names(transformed_data$variables))
            if (length(common_ones) > 0) {
              transformed_data$variables <- transformed_data$variables[-common_ones]
            }
          }
          transformed_vars[[i]] <- list()
          transformed_vars_ordered[[i]] <- list()
          transformed_vars_unorder_indices[[i]] <- list()
          # Order the transformed variables if needed
          # 'var_to_read' should be 'transformed_var', but is kept to reuse the same code as above.
          for (var_to_read in names(transformed_data$variables)) {
            if (var_to_read %in% unlist(var_params)) {
              associated_dim_name <- names(var_params)[which(unlist(var_params) == var_to_read)]
              if ((associated_dim_name %in% names(dim_reorder_params)) && aiat) {
                ## Is this check really needed?
                if (length(dim(transformed_data$variables[[associated_dim_name]])) > 1) {
                  stop("Requested a '", associated_dim_name, "_reorder' for a dimension ",
                       "whose coordinate variable that has more than 1 dimension (after ",
                       "transform). This is not supported.")
                }
                ordered_var_values <- dim_reorder_params[[associated_dim_name]](transformed_data$variables[[associated_dim_name]])
                attr(ordered_var_values, 'variables') <- attr(transformed_data$variables[[associated_dim_name]], 'variables')
                if (!all(c('x', 'ix') %in% names(ordered_var_values))) {
                  stop("All the dimension reorder functions must return a list with the components 'x' and 'ix'.")
                }
                # Save the indices to reorder back the ordered variable values.
                # This will be used to define the first round indices.
                unorder <- sort(ordered_var_values$ix, index.return = TRUE)$ix
                if (var_to_read %in% names(picked_common_vars)) {
                  transformed_common_vars_ordered[[var_to_read]] <- ordered_var_values$x
                  transformed_common_vars_unorder_indices[[var_to_read]] <- unorder
                } else {
                  transformed_vars_ordered[[i]][[var_to_read]] <- ordered_var_values$x
                  transformed_vars_unorder_indices[[i]][[var_to_read]] <- unorder
                }
              }
            }
          }
          transformed_picked_vars <- which(names(picked_vars[[i]]) %in% names(transformed_data$variables))
          if (length(transformed_picked_vars) > 0) {
            transformed_picked_vars <- names(picked_vars[[i]])[transformed_picked_vars]
            transformed_vars[[i]][transformed_picked_vars] <- transformed_data$variables[transformed_picked_vars]
          }
          if (is.null(transformed_common_vars)) {
            transformed_picked_common_vars <- which(names(picked_common_vars) %in% names(transformed_data$variables))
            if (length(transformed_picked_common_vars) > 0) {
              transformed_picked_common_vars <- names(picked_common_vars)[transformed_picked_common_vars]
              transformed_common_vars <- transformed_data$variables[transformed_picked_common_vars]
            }
          }
        }
        # Once the variables are transformed, we compute the indices to be 
        # taken for each inner dimension.
        # In all cases, indices will have to be computed to know which data
        # values to take from the original data for each dimension (if a 
        # variable is specified for that dimension, it will be used to 
        # convert the provided selectors into indices). These indices are
        # referred to as 'first round of indices'.
        # The taken data will then be transformed if needed, together with
        # the dimension variable if specified, and, in that case, indices 
        # will have to be computed again to know which values to take from the 
        # transformed data. These are the 'second round of indices'. In the 
        # case there is no transformation, the second round of indices will
        # be all the available indices, i.e. from 1 to the number of taken
        # values with the first round of indices.
        for (inner_dim in expected_inner_dims[[i]]) {
          if (debug) {
            print("-> DEFINING INDICES FOR INNER DIMENSION:")
            print(inner_dim)
          }
          file_dim <- NULL
          if (inner_dim %in% unlist(inner_dims_across_files)) {
            file_dim <- names(inner_dims_across_files)[which(sapply(inner_dims_across_files, function(x) inner_dim %in% x))[1]]
            chunk_amount <- length(dat[[i]][['selectors']][[file_dim]][[1]])
            names(chunk_amount) <- file_dim
          } else {
            chunk_amount <- 1
          }
          # In the special case that the selectors for a dimension are 'all', 'first', ...
          # and chunking (dividing in more than 1 chunk) is requested, the selectors are
          # replaced for equivalent indices.
          if ((any(dat[[i]][['selectors']][[inner_dim]][[1]] %in% c('all', 'first', 'last'))) && 
              (chunks[[inner_dim]]['n_chunks'] != 1)) {
            selectors <- dat[[i]][['selectors']][[inner_dim]][[1]]
            if (selectors == 'all') {
              selectors <- indices(1:(data_dims[[inner_dim]] * chunk_amount))
            } else if (selectors == 'first') {
              selectors <- indices(1)
            } else {
              selectors <- indices(data_dims[[inner_dim]] * chunk_amount)
            }
            dat[[i]][['selectors']][[inner_dim]][[1]] <- selectors
          }
          # The selectors for the inner dimension are taken.
          selector_array <- dat[[i]][['selectors']][[inner_dim]][[1]]
          if (debug) {
            if (inner_dim %in% dims_to_check) {
              print(paste0("-> DEBUG MESSAGES FOR THE DATASET", i, " AND INNER DIMENSION '", inner_dim, "':"))
              print("-> STRUCTURE OF SELECTOR ARRAY:")
              print(str(selector_array))
              print("-> PICKED VARS:")
              print(picked_vars)
              print("-> TRANSFORMED VARS:")
              print(transformed_vars)
            }
          }
          if (is.null(dim(selector_array))) {
            dim(selector_array) <- length(selector_array)
          }
          if (is.null(names(dim(selector_array)))) {
            if (length(dim(selector_array)) == 1) {
              names(dim(selector_array)) <- inner_dim
            } else {
              stop("Provided selector arrays must be provided with dimension ",
                   "names. Found an array of selectors without dimension names ",
                   "for the dimension '", inner_dim, "'.")
            } 
          }
          selectors_are_indices <- FALSE
          if (!is.null(attr(selector_array, 'indices'))) {
            if (!is.logical(attr(selector_array, 'indices'))) {
              stop("The atribute 'indices' for the selectors for the dimension '", 
                   inner_dim, "' must be TRUE or FALSE.")
            }
            selectors_are_indices <- attr(selector_array, 'indices')
          }
          taken_chunks <- rep(FALSE, chunk_amount)
          selector_file_dims <- 1
          if (any(found_file_dims[[i]] %in% names(dim(selector_array)))) {
            selector_file_dims <- dim(selector_array)[which(names(dim(selector_array)) %in% found_file_dims[[i]])]
          }
          selector_inner_dims <- dim(selector_array)[which(!(names(dim(selector_array)) %in% found_file_dims[[i]]))]
          var_with_selectors <- NULL
          var_with_selectors_name <- var_params[[inner_dim]]
          var_ordered <- NULL
          var_unorder_indices <- NULL
          with_transform <- FALSE
          # If the selectors come with an associated variable
          if (!is.null(var_with_selectors_name)) {
            if ((var_with_selectors_name %in% transform_vars) && (!is.null(transform))) {
              with_transform <- TRUE
              if (!is.null(file_dim)) {
                stop("Requested a transformation over the dimension '", 
                     inner_dim, "', wich goes across files. This feature ", 
                     "is not supported. Either do the request without the ",
                     "transformation or request it over dimensions that do ",
                     "not go across files.")
              }
            }
            if (debug) {
              if (inner_dim %in% dims_to_check) {
                print("-> NAME OF THE VARIABLE WITH THE SELECTOR VALUES FOR THE CURRENT INNER DIMENSION:")
                print(var_with_selectors_name)
                print("-> NAMES OF THE VARIABLES TO BE TRANSFORMED:")
                print(transform_vars)
                print("-> STRUCTURE OF THE TRANSFORMATION FUNCTION:")
                print(str(transform))
              }
            }
            if (var_with_selectors_name %in% names(picked_vars[[i]])) {
              var_with_selectors <- picked_vars[[i]][[var_with_selectors_name]]
              var_ordered <- picked_vars_ordered[[i]][[var_with_selectors_name]]
              var_unorder_indices <- picked_vars_unorder_indices[[i]][[var_with_selectors_name]]
            } else if (var_with_selectors_name %in% names(picked_common_vars)) {
              var_with_selectors <- picked_common_vars[[var_with_selectors_name]]
              var_ordered <- picked_common_vars_ordered[[var_with_selectors_name]]
              var_unorder_indices <- picked_common_vars_unorder_indices[[var_with_selectors_name]]
            }
            n <- prod(dim(var_with_selectors))
            if (is.null(var_unorder_indices)) {
              var_unorder_indices <- 1:n
            }
            if (with_transform) {
              if (var_with_selectors_name %in% names(transformed_vars[[i]])) {
                m <- prod(dim(transformed_vars[[i]][[var_with_selectors_name]]))
                if (aiat) {
                  var_with_selectors <- transformed_vars[[i]][[var_with_selectors_name]]
                  var_ordered <- transformed_vars_ordered[[i]][[var_with_selectors_name]]
                  var_unorder_indices <- transformed_vars_unorder_indices[[i]][[var_with_selectors_name]]
                }
              } else if (var_with_selectors_name %in% names(transformed_common_vars)) {
                m <- prod(dim(transformed_common_vars[[var_with_selectors_name]]))
                if (aiat) {
                  var_with_selectors <- transformed_common_vars[[var_with_selectors_name]]
                  var_ordered <- transformed_common_vars_ordered[[var_with_selectors_name]]
                  var_unorder_indices <- transformed_common_vars_unorder_indices[[var_with_selectors_name]]
                }
              }
              if (is.null(var_unorder_indices)) {
                var_unorder_indices <- 1:m
              }
            }
            if (debug) {
              if (inner_dim %in% dims_to_check) {
                print("-> SIZE OF ORIGINAL VARIABLE:")
                print(n)
                print("-> SIZE OF TRANSFORMED VARIABLE:")
                if (with_transform) print(m)
                print("-> STRUCTURE OF ORDERED VAR:")
                print(str(var_ordered))
                print("-> UNORDER INDICES:")
                print(var_unorder_indices)
              }
            }
            var_dims <- dim(var_with_selectors)
            var_file_dims <- 1
            if (any(names(var_dims) %in% found_file_dims[[i]])) {
              if (with_transform) {
                stop("Requested transformation for inner dimension '", 
                     inner_dim, "' but provided selectors for such dimension ",
                     "over one or more file dimensions. This is not ",
                     "supported. Either request no transformation for the ",
                     "dimension '", inner_dim, "' or specify the ",
                     "selectors for this dimension without the file dimensions.")
              }
              var_file_dims <- var_dims[which(names(var_dims) %in% found_file_dims[[i]])]
              var_dims <- var_dims[-which(names(var_dims) %in% found_file_dims[[i]])]
            }
            ##            # Keep the selectors if they correspond to a variable that will be transformed.
            ##            if (with_transform) {
            ##              if (var_with_selectors_name %in% names(picked_vars[[i]])) {
            ##                transformed_var_with_selectors <- transformed_vars[[i]][[var_with_selectors_name]]
            ##              } else if (var_with_selectors_name %in% names(picked_common_vars)) {
            ##                transformed_var_with_selectors <- transformed_common_vars[[var_with_selectors_name]]
            ##              }
            ##              transformed_var_dims <- dim(transformed_var_with_selectors)
            ##              transformed_var_file_dims <- 1
            ##              if (any(names(transformed_var_dims) %in% found_file_dims[[i]])) {
            ##                transformed_var_file_dims <- transformed_var_dims[which(names(transformed_var_dims) %in% found_file_dims[[i]])]
            ##                transformed_var_dims <- tranasformed_var_dims[-which(names(transformed_var_dims) %in% found_file_dims[[i]])]
            ##              }
            ##if (inner_dim %in% dims_to_check) {
            ##print("111m")
            ##print(str(transformed_var_dims))
            ##}
            ##
            ##              m <- prod(transformed_var_dims)
            ##            }
            # Work out var file dims and inner dims.
            if (inner_dim %in% unlist(inner_dims_across_files)) {
              #TODO: if (chunk_amount != number of chunks in selector_file_dims), crash
              if (length(var_dims) > 1) {
                stop("Specified a '", inner_dim, "_var' for the dimension '", 
                     inner_dim, "', which goes across files (across '", file_dim, 
                     "'). The specified variable, '", var_with_selectors_name, "', has more ",
                     "than one dimension and can not be used as selector variable. ",
                     "Select another variable or fix it in the files.")
              }
            }
            ## TODO HERE::
            #- indices_of_first_files_with_data may change, because array is now extended
            var_full_dims <- dim(var_with_selectors)
            if (!(inner_dim %in% names(var_full_dims))) {
              stop("Could not find the dimension '", inner_dim, "' in ",
                   "the file. Either change the dimension name in ",
                   "your request, adjust the parameter ",
                   "'dim_names_in_files' or fix the dimension name in ",
                   "the file.\n", file_path)
            }
          } else if (((is.numeric(selector_array) || is.list(selector_array)) && selectors_are_indices) ||
                     (is.character(selector_array) && (length(selector_array) == 1) &&
                      (selector_array %in% c('all', 'first', 'last')) &&
                      !is.null(file_dim_reader))) {
            #### TODO HERE::
            ###- indices_of_first_files_with_data may change, because array is now extended
            # Lines moved above for better performance.
            ##data_dims <- file_dim_reader(file_path, NULL, selectors_of_first_files_with_data[[i]],
            ##                             lapply(dat[[i]][['selectors']][expected_inner_dims[[i]]], '[[', 1))
            if (!(inner_dim %in% names(data_dims))) {
              stop("Could not find the dimension '", inner_dim, "' in ",
                   "the file. Either change the dimension name in ",
                   "your request, adjust the parameter ",
                   "'dim_names_in_files' or fix the dimension name in ",
                   "the file.\n", file_path)
            }
          } else {
            stop(paste0("Can not translate the provided selectors for '", inner_dim, 
                        "' to numeric indices. Provide numeric indices and a ",
                        "'file_dim_reader' function, or a '", inner_dim, 
                        "_var' in order to calculate the indices."))
          }
          # At this point, if no selector variable was provided, the variable 
          # data_dims has been populated. If a selector variable was provided,
          # the variables var_dims, var_file_dims and var_full_dims have been 
          # populated instead.
          fri <- first_round_indices <- NULL
          sri <- second_round_indices <- NULL
          # This variable will keep the indices needed to crop the transformed 
          # variable (the one that has been transformed without being subset 
          # with the first round indices).
          tvi <- tranaformed_variable_indices <- NULL
          ordered_fri <- NULL
          ordered_sri <- NULL
          if ((length(selector_array) == 1) && is.character(selector_array) &&
              (selector_array %in% c('all', 'first', 'last')) &&
              (chunks[[inner_dim]]['n_chunks'] == 1)) {
            if (is.null(var_with_selectors_name)) {
              fri <- vector('list', length = chunk_amount)
              dim(fri) <- c(chunk_amount)
              sri <- vector('list', length = chunk_amount)
              dim(sri) <- c(chunk_amount)
              if (selector_array == 'all') {
                fri[] <- replicate(chunk_amount, list(1:(data_dims[inner_dim])))
                taken_chunks <- rep(TRUE, chunk_amount)
                #sri <- NULL
              } else if (selector_array == 'first') {
                fri[[1]] <- 1
                taken_chunks[1] <- TRUE
                #sri <- NULL
              } else if (selector_array == 'last') {
                fri[[chunk_amount]] <- data_dims[inner_dim]
                taken_chunks[length(taken_chunks)] <- TRUE
                #sri <- NULL
              }
            } else {
              if ((!is.null(file_dim)) && !(file_dim %in% names(var_file_dims))) {
                stop("The variable '", var_with_selectors_name, "' must also be ",
                     "requested for the file dimension '", file_dim, "' in ",
                     "this configuration.")
              }
              fri <- vector('list', length = prod(var_file_dims))
              dim(fri) <- var_file_dims
              ordered_fri <- fri
              sri <- vector('list', length = prod(var_file_dims))
              dim(sri) <- var_file_dims
              ordered_sri <- sri
              if (selector_array == 'all') {
                # TODO: Populate ordered_fri
                ordered_fri[] <- replicate(prod(var_file_dims), list(1:n))
                fri[] <- replicate(prod(var_file_dims), list(var_unorder_indices[1:n]))
                taken_chunks <- rep(TRUE, chunk_amount)
                if (!with_transform) {
                  #fri[] <- replicate(prod(var_file_dims), list(1:n))
                  #taken_chunks <- rep(TRUE, chunk_amount)
                  #sri <- NULL
                } else {
                  ordered_sri[] <- replicate(prod(var_file_dims), list(1:m))
                  sri[] <- replicate(prod(var_file_dims), list(1:m))
                  ## var_file_dims instead??
                  #if (!aiat) {
                  #fri[] <- replicate(prod(var_file_dims), list(1:n))
                  #taken_chunks <- rep(TRUE, chunk_amount)
                  #sri[] <- replicate(prod(transformed_var_file_dims), list(1:m))
                  #} else {
                  #fri[] <- replicate(prod(var_file_dims), list(1:n))
                  #taken_chunks <- rep(TRUE, chunk_amount)
                  #sri[] <- replicate(prod(transformed_var_file_dims), list(1:m))
                  #}
                  tvi <- 1:m
                }
              } else if (selector_array == 'first') {
                taken_chunks[1] <- TRUE
                if (!with_transform) {
                  ordered_fri[[1]] <- 1
                  fri[[1]] <- var_unorder_indices[1]
                  #taken_chunks[1] <- TRUE
                  #sri <- NULL
                } else {
                  if (!aiat) {
                    ordered_fri[[1]] <- 1
                    fri[[1]] <- var_unorder_indices[1]
                    # TODO: TO BE IMPROVED
                    #taken_chunks[1] <- TRUE
                    ordered_sri[[1]] <- 1:ceiling(m / n)
                    sri[[1]] <- 1:ceiling(m / n)
                    tvi <- 1:ceiling(m / n)
                  } else {
                    ordered_fri[[1]] <- 1:ceiling(m / n)
                    fri[[1]] <- var_unorder_indices[1:ceiling(m / n)]
                    #taken_chunks[1] <- TRUE
                    ordered_sri[[1]] <- 1
                    sri[[1]] <- 1
                    tvi <- 1
                  }
                }
              } else if (selector_array == 'last') {
                taken_chunks[length(taken_chunks)] <- TRUE
                if (!with_transform) {
                  ordered_fri[[prod(var_file_dims)]] <- n
                  fri[[prod(var_file_dims)]] <- var_unorder_indices[n]
                  #taken_chunks[length(taken_chunks)] <- TRUE
                  #sri <- NULL
                } else {
                  if (!aiat) {
                    ordered_fri[[prod(var_file_dims)]] <- prod(var_dims)
                    fri[[prod(var_file_dims)]] <- var_unorder_indices[prod(var_dims)]
                    #taken_chunks[length(taken_chunks)] <- TRUE
                    ordered_sri[[prod(var_file_dims)]] <- 1:ceiling(m / n)
                    sri[[prod(var_file_dims)]] <- 1:ceiling(m / n)
                    # TODO: TO BE IMPROVED. THE TVI MAY BE WRONG IF THERE'S BEEN A REORDERING.
                    tvi <- 1:ceiling(m / n)
                  } else {
                    ordered_fri[[prod(var_file_dims)]] <- (n - ceiling(m / n) + 1):n
                    fri[[prod(var_file_dims)]] <- var_unorder_indices[(n - ceiling(m / n) + 1):n]
                    #taken_chunks[length(taken_chunks)] <- TRUE
                    ordered_sri[[prod(var_file_dims)]] <- 1
                    sri[[prod(var_file_dims)]] <- 1
                    tvi <- 1
                  }
                }
              }
            }
            # If the selectors are not 'all', 'first', 'last', ...
          } else {
            if (!is.null(var_with_selectors_name)) {
              unmatching_file_dims <- which(!(names(var_file_dims) %in% names(selector_file_dims)))
              if ((length(unmatching_file_dims) > 0)) {
                raise_error <- FALSE
                if (is.null(file_dim)) {
                  raise_error <- TRUE
                } else {
                  if (!((length(unmatching_file_dims) == 1) && 
                        (names(var_file_dims)[unmatching_file_dims] == file_dim) &&
                        (inner_dim %in% names(selector_inner_dims)))) {
                    raise_error <- TRUE
                  }
                }
                if (raise_error) {
                  stop("Provided selectors for the dimension '", inner_dim, "' must have as many ",
                       "file dimensions as the variable the dimension is defined along, '", 
                       var_with_selectors_name, "', with the exceptions of the file pattern dimension ('",
                       found_pattern_dim, "') and any depended file dimension (if specified as ",
                       "depended dimension in parameter 'inner_dims_across_files' and the ",
                       "depending file dimension is present in the provided selector array).")
                }
              }
              if (any(names(selector_file_dims) %in% names(dim(var_with_selectors)))) {
                if (any(dim(var_with_selectors)[names(selector_file_dims)] != selector_file_dims)) {
                  stop("Size of selector file dimensions must mach size of requested ",
                       "variable dimensions.")
                }
              }
            }
            ## TODO: If var dimensions are not in the same order as selector dimensions, reorder
            if (is.null(names(selector_file_dims))) {
              if (is.null(file_dim)) {
                fri_dims <- 1
              } else {
                fri_dims <- chunk_amount
                names(fri_dims) <- file_dim
              }
            } else {
              fri_dim_names <- names(selector_file_dims)
              if (!is.null(file_dim)) {
                fri_dim_names <- c(fri_dim_names, file_dim)
              }
              fri_dim_names <- found_file_dims[[i]][which(found_file_dims[[i]] %in% fri_dim_names)]
              fri_dims <- rep(NA, length(fri_dim_names))
              names(fri_dims) <- fri_dim_names
              fri_dims[names(selector_file_dims)] <- selector_file_dims
              if (!is.null(file_dim)) {
                fri_dims[file_dim] <- chunk_amount
              }
            }
            fri <- vector('list', length = prod(fri_dims))
            dim(fri) <- fri_dims
            sri <- vector('list', length = prod(fri_dims))
            dim(sri) <- fri_dims
            selector_file_dim_array <- array(1:prod(selector_file_dims), dim = selector_file_dims)
            selector_store_position <- fri_dims
            for (j in 1:prod(dim(selector_file_dim_array))) {
              selector_indices_to_take <- which(selector_file_dim_array == j, arr.ind = TRUE)[1, ]
              names(selector_indices_to_take) <- names(selector_file_dims)
              selector_store_position[names(selector_indices_to_take)] <- selector_indices_to_take
              sub_array_of_selectors <- Subset(selector_array, names(selector_indices_to_take),
                                               as.list(selector_indices_to_take), drop = 'selected')
              if (debug) {
                if (inner_dim %in% dims_to_check) {
                  print("-> ITERATING OVER FILE DIMENSIONS OF THE SELECTORS.")
                  print("-> STRUCTURE OF A SUB ARRAY:")
                  print(str(sub_array_of_selectors))
                  print("-> STRUCTURE OF THE VARIABLE WITH SELECTORS:")
                  print(str(var_with_selectors))
                  print(dim(var_with_selectors))
                }
              }
              if (selectors_are_indices) {
                sub_array_of_values <- NULL
                #} else if (!is.null(var_ordered)) {
                #  sub_array_of_values <- var_ordered
              } else {
                if (length(var_file_dims) > 0) {
                  var_indices_to_take <- selector_indices_to_take[which(names(selector_indices_to_take) %in% names(var_file_dims))]
                  sub_array_of_values <- Subset(var_with_selectors, names(var_indices_to_take),
                                                as.list(var_indices_to_take), drop = 'selected')
                } else {
                  sub_array_of_values <- var_with_selectors
                }
              }
              if (debug) {
                if (inner_dim %in% dims_to_check) {
                  print("-> STRUCTURE OF THE SUB ARRAY FROM THE VARIABLE CORRESPONDING TO THE SUB ARRAY OF SELECTORS")
                  print(str(sub_array_of_values))
                  print(dim(sub_array_of_values))
                  print("-> NAME OF THE FILE DIMENSION THE CURRENT INNER DIMENSION EXTENDS ALONG:")
                  print(file_dim)
                }
              }
              if ((!is.null(file_dim) && (file_dim %in% names(selector_file_dims))) || is.null(file_dim)) {
                if (length(sub_array_of_selectors) > 0) {
                  if (debug) {
                    if (inner_dim %in% dims_to_check) {
                      print("-> THE INNER DIMENSION DOES NOT GO ACROSS ANY FILE DIMENSION OR IT DOES BUT IS IN THE PROVIDED SELECTOR ARRAY.")
                    }
                  }
                  if (selectors_are_indices) {
                    if (!is.null(var_with_selectors_name)) {
                      max_allowed <- ifelse(aiat, m, n)
                    } else {
                      max_allowed <- data_dims[inner_dim]
                    }
                    if (any(na.omit(unlist(sub_array_of_selectors)) > max_allowed) ||
                        any(na.omit(unlist(sub_array_of_selectors)) < 1)) {
                      stop("Provided indices out of range for dimension '", inner_dim, "' ", 
                           "for dataset '", dat[[i]][['name']], "' (accepted range: 1 to ", 
                           max_allowed, ").")
                    }
                  }
                  
                  # The selector_checker will return either a vector of indices or a list
                  # with the first and last desired indices.
                  goes_across_prime_meridian <- FALSE
                  if (!is.null(var_ordered) && !selectors_are_indices) {
                    if (!is.null(dim_reorder_params[[inner_dim]])) {
                      if (is.list(sub_array_of_selectors)) {
                        
                        ## NOTE: The check of 'goes_across_prime_meridian' is moved forward to here.
                        is_circular_dim <- attr(dim_reorder_params[[inner_dim]], "circular")
                        if (!is.null(is_circular_dim)) {
                          if (is_circular_dim) {
                            
                            # NOTE: Use CircularSort() to put the values in the assigned range, and get the order.
                            # For example, [-10, 20] in CircularSort(0, 360) is [350, 20]. The $ix list is [2, 1].
                            # 'goes_across_prime_meridian' means the selector range across the border. For example,
                            # CircularSort(-180, 180) with selector [170, 190] -> goes_across_prime_meridian = TRUE.
                            tmp <- dim_reorder_params[[inner_dim]](unlist(sub_array_of_selectors))$ix
                            goes_across_prime_meridian <-  tmp[1] > tmp[2]
                          }
                        }
                        
                        # HERE change to the same code as below (under 'else'). Not sure why originally 
                        #it uses additional lines, which make reorder not work.
                        sub_array_of_selectors <- as.list(dim_reorder_params[[inner_dim]](unlist(sub_array_of_selectors))$x)
                        #sub_array_reordered <- dim_reorder_params[[inner_dim]](unlist(sub_array_of_selectors))
                        #sub_array_unorder <- sort(sub_array_reordered$ix, index.return = TRUE)$ix
                        #sub_array_of_selectors <- as.list(sub_array_reordered$x[sub_array_unorder])
                        
                        # Add warning if the boundary is out of range
                        if (sub_array_of_selectors[1] < range(var_ordered)[1] | sub_array_of_selectors[1] > range(var_ordered)[2]) {
                          .warning(paste0("The lower boundary of selector of ",
                                          inner_dim,
                                          " is out of range [",
                                          min(var_ordered), ", ", max(var_ordered), "]. ",
                                          "Check if the desired range is all included."))
                        }
                        if (sub_array_of_selectors[2] < range(var_ordered)[1] | sub_array_of_selectors[2] > range(var_ordered)[2]) {
                          .warning(paste0("The upper boundary of selector of ",
                                          inner_dim,
                                          " is out of range [",
                                          min(var_ordered), ", ", max(var_ordered), "]. ",
                                          "Check if the desired range is all included."))
                        }
                        
                        
                      } else {
                        sub_array_of_selectors <- dim_reorder_params[[inner_dim]](sub_array_of_selectors)$x
                      }
                    }
                    
                    # NOTE: The ideal solution for selecting indices in goes_across_prime_meridian case
                    # is modified SelectorCheckor.R. But now SelectorCheckor doesn't know the info of
                    #goes_across_prime_meridian, so I do the adjustion after calling SelectorCheckor().
                    sub_array_of_indices <- selector_checker(sub_array_of_selectors, var_ordered,
                                                             tolerance = if (aiat) {
                                                               NULL
                                                             } else {
                                                               tolerance_params[[inner_dim]]
                                                             })
                    
                    if (goes_across_prime_meridian & sub_array_of_indices[[1]] < sub_array_of_indices[[2]]) {
                      if (!(sub_array_of_selectors[[1]] %in% var_ordered)){
                        sub_array_of_indices[[1]] <- sub_array_of_indices[[1]] - 1
                      }
                      
                      if (!(sub_array_of_selectors[[2]] %in% var_ordered)){
                        sub_array_of_indices[[2]] <- sub_array_of_indices[[2]] + 1
                      }
                    }
                    
                    #NOTE: the possible case?
                    if (goes_across_prime_meridian & sub_array_of_indices[[1]] > sub_array_of_indices[[2]]) {
                      stop("The case is goes_across_prime_meridian but no adjustion for the indices!")
                    }
                    
                    if (any(is.na(sub_array_of_indices))) {
                      
                      stop(paste0("The selectors of ", inner_dim,
                                  " are out of range [", min(var_ordered),
                                  ", ", max(var_ordered), "]."))
                    }
                    
                  } else {
                    
                    # Add warning if the boundary is out of range
                    if (is.list(sub_array_of_selectors)) {
                      if (sub_array_of_selectors[1] <
                          min(sub_array_of_values) | sub_array_of_selectors[1] >
                          max(sub_array_of_values)) {
                        .warning(paste0("The lower boundary of selector of ",
                                        inner_dim, " is out of range [",
                                        min(sub_array_of_values), ", ",
                                        max(sub_array_of_values), "]. ",
                                        "Check if the desired range is all included."))
                      }
                      if (sub_array_of_selectors[2] <
                          min(sub_array_of_values) | sub_array_of_selectors[2] >
                          max(sub_array_of_values)) {
                        .warning(paste0("The upper boundary of selector of ",
                                        inner_dim, " is out of range [",
                                        min(sub_array_of_values), ", ",
                                        max(sub_array_of_values), "]. ",
                                        "Check if the desired range is all included."))
                      }
                    }
                    
                    sub_array_of_indices <- selector_checker(sub_array_of_selectors, sub_array_of_values,
                                                             tolerance = if (aiat) {
                                                               NULL
                                                             } else {
                                                               tolerance_params[[inner_dim]]
                                                             })
                    
                    if (any(is.na(sub_array_of_indices))) {
                      
                      stop(paste0("The selectors of ", inner_dim,
                                  " are out of range [", min(sub_array_of_values),
                                  ", ", max(sub_array_of_values), "]."))
                    }
                    
                  }
                  ## This 'if' runs in both Start() and Compute(). In Start(), it doesn't have any effect (no chunk).
                  ## In Compute(), it creates the indices for each chunk. For example, if 'sub_array_of_indices'
                  ## is c(5:10) and chunked into 2, 'sub_array_of_indices' becomes c(5:7) for chunk = 1, c(8:10)
                  ## for chunk = 2. If 'sub_array_of_indices' is list(55, 62) and chunked into 2, it becomes
                  ## list(55, 58) for chunk = 1 and list(59, 62) for chunk = 2. 
                  ## TODO: The list can be turned into vector here? So afterward no need to judge if it is list 
                  ## or vector.
                  if (!is.list(sub_array_of_indices)) {
                    sub_array_of_indices <- sub_array_of_indices[chunk_indices(length(sub_array_of_indices), 
                                                                               chunks[[inner_dim]]["chunk"], 
                                                                               chunks[[inner_dim]]["n_chunks"], 
                                                                               inner_dim)]
                  } else {
                    tmp <- chunk_indices(length(sub_array_of_indices[[1]]:sub_array_of_indices[[2]]),
                                         chunks[[inner_dim]]["chunk"], chunks[[inner_dim]]["n_chunks"],
                                         inner_dim)
                    vect <- sub_array_of_indices[[1]]:sub_array_of_indices[[2]]
                    sub_array_of_indices[[1]] <- vect[tmp[1]]
                    sub_array_of_indices[[2]] <- vect[tmp[length(tmp)]]
                  }
                  # The sub_array_of_indices now contains numeric indices of the values to be taken by each chunk.
                  
                  # Check if all the files have the selectors assigned (e.g., region = 'Grnland') _20191015
                  if (is.character(sub_array_of_selectors)) {
                    array_of_var_files_check <- vector('list', length(selector_indices))
                    for (k in 1:length(selector_indices)) {
                      asdasd <- selector_indices[[k]]
                      array_of_var_files_check <- do.call('[', c(list(x = array_of_files_to_load), asdasd, list(drop = FALSE)))[j]
                      file_object <- file_opener(array_of_var_files_check)
                      var_values_check <- file_var_reader(NULL, file_object, NULL, var_to_read, synonims)
                      if (any(as.vector(var_values_check)[sub_array_of_indices] != sub_array_of_selectors)) {
                        .warning('Not all the files has correponding selectors. Check the selector attributes')
                      }
                    }
                  }
                  
                  if (debug) {
                    if (inner_dim %in% dims_to_check) {
                      print("-> TRANSFORMATION REQUESTED?")
                      print(with_transform)
                      print("-> BETA:")
                      print(beta)
                    }
                  }
                  if (with_transform) {
                    # If there is a transformation and selector values are provided, these
                    # selectors will be processed in the same way either if aiat = TRUE or
                    # aiat = FALSE.
                    ## TODO: If sub_array_of_selectors was integer and aiat then... do what's commented 50 lines below.
                    ##       otherwise, do what's coded.
                    if (debug) {
                      if (inner_dim %in% dims_to_check) {
                        print("-> SELECTORS REQUESTED BEFORE TRANSFORM.")
                      }
                    }
                    
                    ###NOTE: Here, the transform, is different from the below part of non-transform.
                    #  search 'if (goes_across_prime_meridian' to find the lines below.
                    if (goes_across_prime_meridian) {
                      # NOTE: before changing, the return is already correct.
                      
                      #NOTE: The fix below has the same explanation as no with_transform part below. 
                      #      Search the next next 'if (goes_across_prime_meridian) {'.
                      if (sub_array_of_indices[[1]] == sub_array_of_indices[[2]]) {
                        # global longitude
                        sub_array_of_fri <- 1:n
                        # Warning if transform_extra_cell != 0
                        if (beta != 0) {
                          .warning(paste0("Adding parameter transform_extra_cells =  ",
                                          transform_extra_cells, " to the transformed index excesses ",
                                          "the border. The border index is used for transformation."))
                        }
                        
                      } else {
                        # normal case, i.e., not global
                        first_index <- min(unlist(sub_array_of_indices))
                        last_index <- max(unlist(sub_array_of_indices))
                        gap_width <- last_index - first_index - 1
                        sub_array_of_fri <- c(1:(min(unlist(sub_array_of_indices)) + min(gap_width, beta)),                       
                                              (max(unlist(sub_array_of_indices)) - min(gap_width, beta)):n)
                        
                        if (min(gap_width, beta) != beta) {
                          .warning(paste0("Adding parameter transform_extra_cells =  ",
                                          transform_extra_cells, " to the transformed index excesses ",
                                          "the border. The border index is used for transformation."))
                        }
                      }
                      
                    } else {
                      #NOTE: This if seems redundant.
                      if (is.list(sub_array_of_indices)) {
                        sub_array_of_indices <- sub_array_of_indices[[1]]:sub_array_of_indices[[2]]
                      }
                      first_index <- min(unlist(sub_array_of_indices))
                      last_index <- max(unlist(sub_array_of_indices))
                      
                      start_padding <- min(beta, first_index - 1)
                      end_padding <- min(beta, n - last_index)
                      
                      if (exists("is_circular_dim")) {
                        if (!is_circular_dim) {  #latitude
                          sub_array_of_fri <- (first_index - start_padding):(last_index + end_padding)
                          if (start_padding != beta | end_padding != beta) {
                            .warning(paste0("Adding parameter transform_extra_cells =  ",
                                            transform_extra_cells, " to the transformed index excesses ",
                                            "the border. The border index is used for transformation."))
                          }
                        } else {  #longitude
                          if ((last_index - first_index + 1 + beta * 2) >= n) {
                            sub_array_of_fri <- 1:n
                          } else if (start_padding < beta) {  # left side too close to border, need to go to right side
                            sub_array_of_fri <- c((first_index - start_padding):(last_index + end_padding), (n - (beta - start_padding - 1)):n)
                          } else if (end_padding < beta) { # right side too close to border, need to go to left side
                            sub_array_of_fri <- c(1: (beta - end_padding), (first_index - start_padding):(last_index + end_padding))
                          } else {  #normal
                            sub_array_of_fri <- (first_index - start_padding):(last_index + end_padding)
                          }
                        }
                      } else {   # when <var>_reorder is not used
                        sub_array_of_fri <- (first_index - start_padding):(last_index + end_padding)
                        if (start_padding != beta | end_padding != beta) {
                          .warning(paste0("Adding parameter transform_extra_cells =  ",
                                          transform_extra_cells, " to the transformed index excesses ",
                                          "the border. The border index is used for transformation."))
                        }
                      }
                      
                    }
                    subset_vars_to_transform <- vars_to_transform
                    if (!is.null(var_ordered)) {
                      
                      ##NOTE: if var_ordered is common_vars, it doesn't have attributes and it is a vector.
                      ## Turn it into array and add dimension name.
                      if (!is.array(var_ordered)) {
                        var_ordered <- as.array(var_ordered)
                        names(dim(var_ordered)) <- inner_dim
                      }
                      
                      subset_vars_to_transform[[var_with_selectors_name]] <- Subset(var_ordered, inner_dim, sub_array_of_fri)
                    } else {
                      ##NOTE: It should be redundant because without reordering the var should remain array
                      ## But just stay same with above...
                      if (!is.array(sub_array_of_values)) {
                        sub_array_of_values <- as.array(sub_array_of_values)
                        names(dim(sub_array_of_values)) <- inner_dim
                      }
                      
                      subset_vars_to_transform[[var_with_selectors_name]] <- Subset(sub_array_of_values, inner_dim, sub_array_of_fri)
                    }
                    
                    # Change the order of longitude crop if no reorder + from big to small.
                    # cdo -sellonlatbox, the lon is west, east (while lat can be north 
                    # to south or opposite)
                    
                    # Before changing crop, first we need to find the name of longitude. 
                    # NOTE: The potential bug here (also the bug for CDORemapper): the lon name
                    #       is limited (only the ones listed in .KnownLonNames() are available.
                    known_lon_names <- .KnownLonNames()
                    lon_name <- names(subset_vars_to_transform)[which(names(subset_vars_to_transform) %in% known_lon_names)[1]]
                    
                    # NOTE: The cases not considered: (1) if lon reorder(decreasing = T)
                    #       It doesn't make sense, but if someone uses it, here should
                    #       occur error. (2) crop = TRUE/FALSE
                    if ('crop' %in% names(transform_params) & var_with_selectors_name == lon_name & is.null(dim_reorder_params[[inner_dim]])) {
                      if (is.numeric(class(transform_params$crop))) {
                        if (transform_params$crop[1] > transform_params$crop[2]) {
                          tmp <- transform_params$crop[1]
                          transform_params$crop[1] <- transform_params$crop[2]
                          transform_params$crop[2] <- tmp
                        }
                      }
                    }
                    
                    transformed_subset_var <- do.call(transform, c(list(data_array = NULL,
                                                                        variables = subset_vars_to_transform,
                                                                        file_selectors = selectors_of_first_files_with_data[[i]]),
                                                                   transform_params))$variables[[var_with_selectors_name]]
                    # Sorting the transformed variable and working out the indices again after transform.
                    if (!is.null(dim_reorder_params[[inner_dim]])) {
                      transformed_subset_var_reorder <- dim_reorder_params[[inner_dim]](transformed_subset_var)
                      transformed_subset_var <- transformed_subset_var_reorder$x
                      #NOTE: The fix here solves the mis-ordered lon when across_meridian. 
                      transformed_subset_var_unorder <- transformed_subset_var_reorder$ix
                      #                      transformed_subset_var_unorder <- sort(transformed_subset_var_reorder$ix, index.return = TRUE)$ix
                    } else {
                      transformed_subset_var_unorder <- 1:length(transformed_subset_var)
                    }
                    sub_array_of_sri <- selector_checker(sub_array_of_selectors, transformed_subset_var,
                                                         tolerance = if (aiat) {
                                                           tolerance_params[[inner_dim]]
                                                         } else {
                                                           NULL
                                                         })
                    
                    # Check if selectors fall out of the range of the transform grid
                    # It may happen when original lon is [-180, 180] while want to regrid to
                    # [0, 360], and lon selector = [-20, -10].
                    if (any(is.na(sub_array_of_sri))) {
                      stop(paste0("The selectors of ",
                                  inner_dim, " are out of range of transform grid '",
                                  transform_params$grid, "'. Use parameter '",
                                  inner_dim, "_reorder' or change ", inner_dim,
                                  " selectors."))
                    }
                    
                    if (goes_across_prime_meridian) {
                      
                      if (sub_array_of_sri[[1]] == sub_array_of_sri[[2]]) {
                        # global longitude
                        sub_array_of_sri <- c(1:length(transformed_subset_var))
                      } else {
                        # the common case, i.e., non-global
                        # NOTE: Because sub_array_of_sri order is exchanged due to 
                        # previous development, here [[1]] and [[2]] should exchange
                        sub_array_of_sri <- c(1:sub_array_of_sri[[1]],
                                              sub_array_of_sri[[2]]:length(transformed_subset_var))
                      }
                      
                    } else if (is.list(sub_array_of_sri)) {
                      sub_array_of_sri <- sub_array_of_sri[[1]]:sub_array_of_sri[[2]]
                    }
                    ordered_sri <- sub_array_of_sri
                    sub_array_of_sri <- transformed_subset_var_unorder[sub_array_of_sri]
                    # In this case, the tvi are not defined and the 'transformed_subset_var'
                    # will be taken instead of the var transformed before in the code.
                    if (debug) {
                      if (inner_dim %in% dims_to_check) {
                        print("-> FIRST INDEX:")
                        print(first_index)
                        print("-> LAST INDEX:")
                        print(last_index)
                        print("-> STRUCTURE OF FIRST ROUND INDICES:")
                        print(str(sub_array_of_fri))
                        print("-> STRUCTURE OF SECOND ROUND INDICES:")
                        print(str(sub_array_of_sri))
                        print("-> STRUCTURE OF TRANSFORMED VARIABLE INDICES:")
                        print(str(tvi))
                      }
                    }
                    ###                    # If the selectors are expressed after transformation
                    ###                    } else {
                    ###if (debug) {
                    ###if (inner_dim %in% dims_to_check) {
                    ###print("-> SELECTORS REQUESTED AFTER TRANSFORM.")
                    ###}
                    ###}
                    ###                      if (goes_across_prime_meridian) {
                    ###                        sub_array_of_indices <- c(sub_array_of_indices[[1]]:m,
                    ###                                                    1:sub_array_of_indices[[2]])
                    ###                      }
                    ###                      first_index <- min(unlist(sub_array_of_indices))
                    ###                      last_index <- max(unlist(sub_array_of_indices))
                    ###                      first_index_before_transform <- max(transform_indices(first_index, m, n) - beta, 1)
                    ###                      last_index_before_transform <- min(transform_indices(last_index, m, n) + beta, n)
                    ###                      sub_array_of_fri <- first_index_before_transform:last_index_before_transform
                    ###                      n_of_extra_cells <- round(beta / n * m)
                    ###                      if (is.list(sub_array_of_indices) && (length(sub_array_of_indices) > 1)) {
                    ###                        sub_array_of_sri <- 1:(last_index - first_index + 1) 
                    ###                        if (is.null(tvi)) {
                    ###                          tvi <- sub_array_of_sri + first_index - 1
                    ###                        }
                    ###                      } else {
                    ###                        sub_array_of_sri <- sub_array_of_indices - first_index + 1
                    ###                        if (is.null(tvi)) {
                    ###                          tvi <- sub_array_of_indices
                    ###                        }
                    ###                      }
                    ###                      sub_array_of_sri <- sub_array_of_sri + n_of_extra_cells
                    sri <- do.call('[[<-', c(list(x = sri), as.list(selector_store_position),
                                             list(value = sub_array_of_sri)))
                  } else {
                    if (goes_across_prime_meridian) {
                      #NOTE: The potential problem here is, if it is global longitude,
                      #      and the indices overlap (e.g., lon = [0, 359.723] and 
                      #      CircularSort(-180, 180), then sub_array_of_indices = list(649, 649)). 
                      #      Therefore, sub_array_of_fri will be c(1:649, 649:1296). We'll
                      #      get two 649.
                      #      The fix below may not be the best solution, but it works for 
                      #      the example above.
                      
                      if (sub_array_of_indices[[1]] == sub_array_of_indices[[2]]) {
                        # global longitude
                        sub_array_of_fri <- c(1:n)
                      } else {
                        # the common case, i.e., non-global
                        sub_array_of_fri <- c(1:min(unlist(sub_array_of_indices)),
                                              max(unlist(sub_array_of_indices)):n)
                      }
                      
                    } else if (is.list(sub_array_of_indices)) {
                      sub_array_of_fri <- sub_array_of_indices[[1]]:sub_array_of_indices[[2]]
                    } else {
                      sub_array_of_fri <- sub_array_of_indices
                    }
                  }
                  if (!is.null(var_unorder_indices)) {
                    if (is.null(ordered_fri)) {
                      ordered_fri <- sub_array_of_fri
                    }
                    sub_array_of_fri <- var_unorder_indices[sub_array_of_fri]
                  }
                  fri <- do.call('[[<-', c(list(x = fri), as.list(selector_store_position),
                                           list(value = sub_array_of_fri)))
                  if (!is.null(file_dim)) {
                    taken_chunks[selector_store_position[[file_dim]]] <- TRUE
                  } else {
                    taken_chunks <- TRUE
                  }
                }
              } else {
                if (debug) {
                  if (inner_dim %in% dims_to_check) {
                    print("-> THE INNER DIMENSION GOES ACROSS A FILE DIMENSION.")
                  }
                }
                if (inner_dim %in% names(dim(sub_array_of_selectors))) {
                  if (is.null(var_with_selectors_name)) {
                    if (any(na.omit(unlist(sub_array_of_selectors)) < 1) ||
                        any(na.omit(unlist(sub_array_of_selectors)) > data_dims[inner_dim] * chunk_amount)) {
                      stop("Provided indices out of range for dimension '", inner_dim, "' ", 
                           "for dataset '", dat[[i]][['name']], "' (accepted range: 1 to ", 
                           data_dims[inner_dim] * chunk_amount, ").")
                    }
                  } else {
                    if (inner_dim %in% names(dim(sub_array_of_values))) {
                      # NOTE: Put across-inner-dim at the 1st position.
                      # POSSIBLE PROB!! Only organize inner dim, the rest dims may not in the same order as sub_array_of_selectors below.
                      inner_dim_pos_in_sub_array <- which(names(dim(sub_array_of_values)) == inner_dim)
                      if (inner_dim_pos_in_sub_array != 1) {
                        new_sub_array_order <- (1:length(dim(sub_array_of_values)))[-inner_dim_pos_in_sub_array]
                        new_sub_array_order <- c(inner_dim_pos_in_sub_array, new_sub_array_order)
                        sub_array_of_values <- .aperm2(sub_array_of_values, new_sub_array_order)
                      }
                    }
                  }
                  
                  # NOTE: Put across-inner-dim at the 1st position.
                  # POSSIBLE PROB!! Only organize inner dim, the rest dims may not in the same order as sub_array_of_values above.
                  inner_dim_pos_in_sub_array <- which(names(dim(sub_array_of_selectors)) == inner_dim)
                  if (inner_dim_pos_in_sub_array != 1) {
                    new_sub_array_order <- (1:length(dim(sub_array_of_selectors)))[-inner_dim_pos_in_sub_array]
                    new_sub_array_order <- c(inner_dim_pos_in_sub_array, new_sub_array_order)
                    sub_array_of_selectors <- .aperm2(sub_array_of_selectors, new_sub_array_order)
                  }
                  sub_array_of_indices <- selector_checker(sub_array_of_selectors, sub_array_of_values,
                                                           tolerance = tolerance_params[[inner_dim]])
                  # It is needed to expand the indices here, otherwise for 
                  # values(list(date1, date2)) only 2 values are picked.
                  if (is.list(sub_array_of_indices)) {
                    sub_array_of_indices <- sub_array_of_indices[[1]]:sub_array_of_indices[[2]]
                  }
                  sub_array_of_indices <- sub_array_of_indices[chunk_indices(length(sub_array_of_indices),
                                                                             chunks[[inner_dim]]['chunk'],
                                                                             chunks[[inner_dim]]['n_chunks'],
                                                                             inner_dim)]
                  sub_array_is_list <- FALSE
                  if (is.list(sub_array_of_indices)) {
                    sub_array_is_list <- TRUE
                    sub_array_of_indices <- unlist(sub_array_of_indices)
                  }
                  if (is.null(var_with_selectors_name)) {
                    indices_chunk <- floor((sub_array_of_indices - 1) / data_dims[inner_dim]) + 1
                    transformed_indices <- ((sub_array_of_indices - 1) %% data_dims[inner_dim]) + 1
                  } else {
                    indices_chunk <- floor((sub_array_of_indices - 1) / var_full_dims[inner_dim]) + 1
                    transformed_indices <- ((sub_array_of_indices - 1) %% var_full_dims[inner_dim]) + 1
                  }
                  if (sub_array_is_list) {
                    sub_array_of_indices <- as.list(sub_array_of_indices)
                  }
                  if (debug) {
                    if (inner_dim %in% dims_to_check) {
                      print("-> GOING TO ITERATE ALONG CHUNKS.")
                    }
                  }
                  for (chunk in 1:chunk_amount) {
                    if (!is.null(names(selector_store_position))) {
                      selector_store_position[file_dim] <- chunk
                    } else {
                      selector_store_position <- chunk
                    }
                    chunk_selectors <- transformed_indices[which(indices_chunk == chunk)]
                    sub_array_of_indices <- chunk_selectors
                    if (with_transform) {
                      # If the provided selectors are expressed in the world
                      # before transformation
                      if (!aiat) {
                        first_index <- min(unlist(sub_array_of_indices))
                        last_index <- max(unlist(sub_array_of_indices))
                        sub_array_of_fri <- max(c(first_index - beta, 1)):min(c(last_index + beta, n))
                        sub_array_of_sri <- transform_indices(unlist(sub_array_of_indices) - first_index + 1, n, m)
                        if (is.list(sub_array_of_indices)) {
                          if (length(sub_array_of_sri) > 1) {
                            sub_array_of_sri <- sub_array_of_sri[[1]]:sub_array_of_sri[[2]]
                          }
                        }
                        ##TODO: TRANSFORM SUBSET VARIABLE AS ABOVE, TO COMPUTE SRI
                        # If the selectors are expressed after transformation
                      } else {
                        first_index <- min(unlist(sub_array_of_indices))
                        last_index <- max(unlist(sub_array_of_indices))
                        first_index_before_transform <- max(transform_indices(first_index, m, n) - beta, 1)
                        last_index_before_transform <- min(transform_indices(last_index, m, n) + beta, n)
                        sub_array_of_fri <- first_index_before_transform:last_index_before_transform
                        if (is.list(sub_array_of_indices) && (length(sub_array_of_indices) > 1)) {
                          sub_array_of_sri <- 1:(last_index - first_index + 1) + 
                            round(beta / n * m) 
                        } else {
                          sub_array_of_sri <- sub_array_of_indices - first_index + 1 +
                            round(beta / n * m)
                        }
                        ##TODO: FILL IN TVI
                      }
                      sri <- do.call('[[<-', c(list(x = sri), as.list(selector_store_position),
                                               list(value = sub_array_of_sri)))
                      if (length(sub_array_of_sri) > 0) {
                        taken_chunks[chunk] <- TRUE
                      }
                    } else {
                      sub_array_of_fri <- sub_array_of_indices
                      if (length(sub_array_of_fri) > 0) {
                        taken_chunks[chunk] <- TRUE
                      }
                    }
                    if (!is.null(var_unorder_indices)) {
                      ordered_fri <- sub_array_of_fri
                      sub_array_of_fri <- var_unorder_indices[sub_array_of_fri]
                    }
                    fri <- do.call('[[<-', c(list(x = fri), as.list(selector_store_position),
                                             list(value = sub_array_of_fri)))
                  }
                  if (debug) {
                    if (inner_dim %in% dims_to_check) {
                      print("-> FINISHED ITERATING ALONG CHUNKS")
                    }
                  }
                } else {
                  stop("Provided array of indices for dimension '", inner_dim, "', ",
                       "which goes across the file dimension '", file_dim, "', but ",
                       "the provided array does not have the dimension '", inner_dim, 
                       "', which is mandatory.")
                }
              }
            }
          }
          if (debug) {
            if (inner_dim %in% dims_to_check) {
              print("-> PROCEEDING TO CROP VARIABLES")
            }
          }
          #if ((length(selector_array) == 1) && (selector_array %in% c('all', 'first', 'last'))) {
          #if (!is.null(var_with_selectors_name) || (is.null(var_with_selectors_name) && is.character(selector_array) &&
          #    (length(selector_array) == 1) && (selector_array %in% c('all', 'first', 'last')))) {
          empty_chunks <- which(!taken_chunks)
          if (length(empty_chunks) >= length(taken_chunks)) {
            stop("Selectors do not match any of the possible values for the dimension '", inner_dim, "'.")
          }
          if (length(empty_chunks) > 0) {
            #                # Get the first group of chunks to remove, and remove them. 
            #                # E.g., from c(1, 2, 4, 5, 6, 8, 9) remove only 1 and 2
            #                dist <- abs(rev(empty_chunks) - c(rev(empty_chunks)[1] - 1, head(rev(empty_chunks), length(rev(empty_chunks)) - 1)))
            #                if (all(dist == 1)) {
            #                  start_chunks_to_remove <- NULL
            #                } else {
            #                  first_chunk_to_remove <- tail(which(dist > 1), 1)
            #                  start_chunks_to_remove <- rev(rev(empty_chunks)[first_chunk_to_remove:length(empty_chunks)])
            #                }
            #                # Get the last group of chunks to remove, and remove them. 
            #                # E.g., from c(1, 2, 4, 5, 6, 8, 9) remove only 8 and 9
            #                dist <- abs(empty_chunks - c(empty_chunks[1] - 1, head(empty_chunks, length(empty_chunks) - 1)))
            #                if (all(dist == 1)) {
            #                  first_chunk_to_remove <- 1
            #                } else {
            #                  first_chunk_to_remove <- tail(which(dist > 1), 1)
            #                }
            #                end_chunks_to_remove <- empty_chunks[first_chunk_to_remove:length(empty_chunks)]
            #                chunks_to_keep <- which(!((1:length(taken_chunks)) %in% c(start_chunks_to_remove, end_chunks_to_remove)))
            chunks_to_keep <- which(taken_chunks)
            dims_to_crop[[file_dim]] <- c(dims_to_crop[[file_dim]], list(chunks_to_keep))
            #                found_indices <- Subset(found_indices, file_dim, chunks_to_keep)
            #                # Crop dataset variables file dims.
            #                for (picked_var in names(picked_vars[[i]])) {
            #                  if (file_dim %in% names(dim(picked_vars[[i]][[picked_var]]))) {
            #                    picked_vars[[i]][[picked_var]] <- Subset(picked_vars[[i]][[picked_var]], file_dim, chunks_to_keep)
            #                  }
            #                }
          }
          #}
          dat[[i]][['selectors']][[inner_dim]] <- list(fri = fri, sri = sri)
          # Crop dataset variables inner dims.
          # Crop common variables inner dims.
          types_of_var_to_crop <- 'picked'
          if (with_transform) {
            types_of_var_to_crop <- c(types_of_var_to_crop, 'transformed')
          }
          if (!is.null(dim_reorder_params[[inner_dim]])) {
            types_of_var_to_crop <- c(types_of_var_to_crop, 'reordered')
          }
          for (type_of_var_to_crop in types_of_var_to_crop) {
            if (type_of_var_to_crop == 'transformed') {
              if (is.null(tvi)) {
                if (!is.null(dim_reorder_params[[inner_dim]])) {
                  crop_indices <- unique(unlist(ordered_sri))
                } else {
                  crop_indices <- unique(unlist(sri))
                }
              } else {
                crop_indices <- unique(unlist(tvi))
              }
              vars_to_crop <- transformed_vars[[i]]
              common_vars_to_crop <- transformed_common_vars
            } else if (type_of_var_to_crop == 'reordered') {
              crop_indices <- unique(unlist(ordered_fri))
              vars_to_crop <- picked_vars_ordered[[i]]
              common_vars_to_crop <- picked_common_vars_ordered
            } else {
              crop_indices <- unique(unlist(fri))
              vars_to_crop <- picked_vars[[i]]
              common_vars_to_crop <- picked_common_vars
            }
            for (var_to_crop in names(vars_to_crop)) {
              if (inner_dim %in% names(dim(vars_to_crop[[var_to_crop]]))) {
                if (!is.null(crop_indices)) {
                  if (type_of_var_to_crop == 'transformed') {
                    if (!aiat) {
                      vars_to_crop[[var_to_crop]] <- Subset(transformed_subset_var, inner_dim, crop_indices)
                    } else {
                      vars_to_crop[[var_to_crop]] <- Subset(vars_to_crop[[var_to_crop]], inner_dim, crop_indices)
                    }
                  } else {
                    vars_to_crop[[var_to_crop]] <- Subset(vars_to_crop[[var_to_crop]], inner_dim, crop_indices)
                  }
                }
              }
            }
            if (i == length(dat)) {
              for (common_var_to_crop in names(common_vars_to_crop)) {
                if (inner_dim %in% names(dim(common_vars_to_crop[[common_var_to_crop]]))) {

                  if (type_of_var_to_crop == 'transformed' & !aiat) {
                  common_vars_to_crop[[common_var_to_crop]] <- Subset(transformed_subset_var, inner_dim, crop_indices)
                  } else {  #old code
                  common_vars_to_crop[[common_var_to_crop]] <- Subset(common_vars_to_crop[[common_var_to_crop]], inner_dim, crop_indices)
                  }

                }
              }
            }
            if (type_of_var_to_crop == 'transformed') {
              if (!is.null(vars_to_crop)) {
                transformed_vars[[i]] <- vars_to_crop
              }
              if (i == length(dat)) {
                transformed_common_vars <- common_vars_to_crop
              }
            } else if (type_of_var_to_crop == 'reordered') {
              if (!is.null(vars_to_crop)) {
                picked_vars_ordered[[i]] <- vars_to_crop
              }
              if (i == length(dat)) {
                picked_common_vars_ordered <- common_vars_to_crop
              }
            } else {
              if (!is.null(vars_to_crop)) {
                picked_vars[[i]] <- vars_to_crop
              }
              if (i == length(dat)) {
                picked_common_vars <- common_vars_to_crop
              }
            }
          }
          #}
        }
        # After the selectors have been picked (using the original variables), 
        # the variables are transformed. At that point, the original selectors
        # for the transformed variables are also kept in the variable original_selectors.
        #print("L")
      }
    }
  }
  #  if (!is.null(transformed_common_vars)) {
  #    picked_common_vars[names(transformed_common_vars)] <- transformed_common_vars
  #  }
  # Remove the trailing chunks, if any.
  for (file_dim in names(dims_to_crop)) {
    #    indices_to_keep <- min(sapply(dims_to_crop[[file_dim]], min)):max(sapply(dims_to_crop[[file_dim]], max))
    ## TODO: Merge indices in dims_to_crop with some advanced mechanism?
    indices_to_keep <- unique(unlist(dims_to_crop[[file_dim]]))
    array_of_files_to_load <- Subset(array_of_files_to_load, file_dim, indices_to_keep)
    array_of_not_found_files <- Subset(array_of_not_found_files, file_dim, indices_to_keep)
    for (i in 1:length(dat)) {
      # Crop selectors
      for (selector_dim in names(dat[[i]][['selectors']])) {
        if (selector_dim == file_dim) {
          for (j in 1:length(dat[[i]][['selectors']][[selector_dim]][['fri']])) {
            dat[[i]][['selectors']][[selector_dim]][['fri']][[j]] <- dat[[i]][['selectors']][[selector_dim]][['fri']][[j]][indices_to_keep]
          }
          for (j in 1:length(dat[[i]][['selectors']][[selector_dim]][['sri']])) {
            dat[[i]][['selectors']][[selector_dim]][['sri']][[j]] <- dat[[i]][['selectors']][[selector_dim]][['sri']][[j]][indices_to_keep]
          }
        }
        if (file_dim %in% names(dim(dat[[i]][['selectors']][[selector_dim]][['fri']]))) {
          dat[[i]][['selectors']][[selector_dim]][['fri']] <- Subset(dat[[i]][['selectors']][[selector_dim]][['fri']], file_dim, indices_to_keep)
          dat[[i]][['selectors']][[selector_dim]][['sri']] <- Subset(dat[[i]][['selectors']][[selector_dim]][['sri']], file_dim, indices_to_keep)
        }
      }
      # Crop dataset variables file dims.
      for (picked_var in names(picked_vars[[i]])) {
        if (file_dim %in% names(dim(picked_vars[[i]][[picked_var]]))) {
          picked_vars[[i]][[picked_var]] <- Subset(picked_vars[[i]][[picked_var]], file_dim, indices_to_keep)
        }
      }
      for (transformed_var in names(transformed_vars[[i]])) {
        if (file_dim %in% names(dim(transformed_vars[[i]][[transformed_var]]))) {
          transformed_vars[[i]][[transformed_var]] <- Subset(transformed_vars[[i]][[transformed_var]], file_dim, indices_to_keep)
        }
      }
    }
    # Crop common variables file dims.
    for (picked_common_var in names(picked_common_vars)) {
      if (file_dim %in% names(dim(picked_common_vars[[picked_common_var]]))) {
        picked_common_vars[[picked_common_var]] <- Subset(picked_common_vars[[picked_common_var]], file_dim, indices_to_keep)
      }
    }
    for (transformed_common_var in names(transformed_common_vars)) {
      if (file_dim %in% names(dim(transformed_common_vars[[transformed_common_var]]))) {
        transformed_common_vars[[transformed_common_var]] <- Subset(transformed_common_vars[[transformed_common_var]], file_dim, indices_to_keep)
      }
    }
  }
  # Calculate the size of the final array.
  total_inner_dims <- NULL
  for (i in 1:length(dat)) {
    if (dataset_has_files[i]) {
      inner_dims <- expected_inner_dims[[i]]
      inner_dims <- sapply(inner_dims, 
                           function(x) {
                             if (!all(sapply(dat[[i]][['selectors']][[x]][['sri']], is.null))) {
                               max(sapply(dat[[i]][['selectors']][[x]][['sri']], length))
                             } else {
                               if (length(var_params[[x]]) > 0) {
                                 if (var_params[[x]] %in% names(transformed_vars[[i]])) {
                                   length(transformed_vars[[i]][[var_params[[x]]]])
                                 } else if (var_params[[x]] %in% names(transformed_common_vars)) {
                                   length(transformed_common_vars[[var_params[[x]]]])
                                 } else {
                                   max(sapply(dat[[i]][['selectors']][[x]][['fri']], length))
                                 }
                               } else {
                                 max(sapply(dat[[i]][['selectors']][[x]][['fri']], length))
                               }
                             }
                           })
      names(inner_dims) <- expected_inner_dims[[i]]
      if (is.null(total_inner_dims)) {
        total_inner_dims <- inner_dims
      } else {
        new_dims <- .MergeArrayDims(total_inner_dims, inner_dims)
        total_inner_dims <- new_dims[[3]]
      }
    }
  }
  new_dims <- .MergeArrayDims(dim(array_of_files_to_load), total_inner_dims)
  final_dims <- new_dims[[3]][dim_names]
  # final_dims_fake is the vector of final dimensions after having merged the 
  # 'across' file dimensions with the respective 'across' inner dimensions, and
  # after having broken into multiple dimensions those dimensions for which 
  # multidimensional selectors have been provided.
  # final_dims will be used for collocation of data, whereas final_dims_fake 
  # will be used for shaping the final array to be returned to the user.
  final_dims_fake <- final_dims
  if (merge_across_dims) {
    if (!is.null(inner_dims_across_files)) {
      for (file_dim_across in names(inner_dims_across_files)) {
        inner_dim_pos <- which(names(final_dims_fake) == inner_dims_across_files[[file_dim_across]])
        new_dims <- c()
        if (inner_dim_pos > 1) {
          new_dims <- c(new_dims, final_dims_fake[1:(inner_dim_pos - 1)])
        }
        new_dims <- c(new_dims, setNames(prod(final_dims_fake[c(inner_dim_pos, inner_dim_pos + 1)]), 
                                         inner_dims_across_files[[file_dim_across]]))
        if (inner_dim_pos + 1 < length(final_dims_fake)) {
          new_dims <- c(new_dims, final_dims_fake[(inner_dim_pos + 2):length(final_dims_fake)])
        }
        final_dims_fake <- new_dims
      }
    }
  }
  all_split_dims <- NULL
  if (split_multiselected_dims) {
    for (dim_param in 1:length(dim_params)) {
      if (!is.null(dim(dim_params[[dim_param]]))) {
        if (length(dim(dim_params[[dim_param]])) > 1) {
          split_dims <- dim(dim_params[[dim_param]])
          all_split_dims <- c(all_split_dims, setNames(list(split_dims), 
                                                       names(dim_params)[dim_param]))
          if (is.null(names(split_dims))) {
            names(split_dims) <- paste0(names(dim_params)[dim_param], 
                                        1:length(split_dims))
          }
          old_dim_pos <- which(names(final_dims_fake) == names(dim_params)[dim_param])
          
          # NOTE: Three steps to create new dims.
          # 1st: Put in the dims before split_dim.
          # 2nd: Replace the old_dim with split_dims.
          # 3rd: Put in the dims after split_dim.
          new_dims <- c()
          if (old_dim_pos > 1) {
            new_dims <- c(new_dims, final_dims_fake[1:(old_dim_pos - 1)])
          }
          new_dims <- c(new_dims, split_dims)
          if (old_dim_pos < length(final_dims_fake)) {
            new_dims <- c(new_dims, final_dims_fake[(old_dim_pos + 1):length(final_dims_fake)])
          }
          final_dims_fake <- new_dims
        }
      }
    }
  }
  if (merge_across_dims_narm) {
    # only merge_across_dims -> the 'time' dim length needs to be adjusted
    across_inner_dim <- inner_dims_across_files[[1]]  #TODO: more than one?
    across_file_dim <- names(inner_dims_across_files)  #TODO: more than one?
    # Get the length of each inner_dim ('time') along each file_dim ('file_date')  
    length_inner_across_dim <- lapply(dat[[i]][['selectors']][[across_inner_dim]][['fri']], length)
    
    if (!split_multiselected_dims) {
      final_dims_fake_name <- names(final_dims_fake)
      pos_across_inner_dim <- which(final_dims_fake_name == across_inner_dim)
      new_length_inner_dim <- sum(unlist(length_inner_across_dim))
      if (pos_across_inner_dim != length(final_dims_fake)) {
        final_dims_fake <- c(final_dims_fake[1:(pos_across_inner_dim - 1)],
                             new_length_inner_dim,
                             final_dims_fake[(pos_across_inner_dim + 1):length(final_dims_fake)])
      } else {
        final_dims_fake <- c(final_dims_fake[1:(pos_across_inner_dim - 1)],
                             new_length_inner_dim)
      }
      names(final_dims_fake) <- final_dims_fake_name
    }
  }
  
  if (!silent) {
    .message("Detected dimension sizes:")
    longest_dim_len <- max(sapply(names(final_dims_fake), nchar))
    longest_size_len <- max(sapply(paste0(final_dims_fake, ''), nchar))
    sapply(names(final_dims_fake), 
           function(x) {
             message(paste0("*   ", paste(rep(' ', longest_dim_len - nchar(x)), collapse = ''), 
                            x, ": ", paste(rep(' ', longest_size_len - nchar(paste0(final_dims_fake[x], ''))), collapse = ''), 
                            final_dims_fake[x]))
           })
    bytes <- prod(c(final_dims_fake, 8))
    dim_sizes <- paste(final_dims_fake, collapse = ' x ')
    if (retrieve) {
      .message(paste("Total size of requested data:"))
    } else {
      .message(paste("Total size of involved data:"))
    }
    .message(paste(dim_sizes, " x 8 bytes =", 
                   format(structure(bytes, class = "object_size"), units = "auto")), 
             indent = 2)
  }
  
  # NOTE: If split_multiselected_dims + merge_across_dims, the dim order may need to be changed.
  #       The inner_dim needs to be the first dim among split dims.
  #       Cannot control the rest dims are in the same order or not...
  #       Suppose users put the same order of across inner and file dims.
  if (split_multiselected_dims & merge_across_dims) {
    # TODO: More than one split?
    inner_dim_pos_in_split_dims <- which(names(all_split_dims[[1]]) == inner_dims_across_files)  
    # if inner_dim is not the first, change!
    if (inner_dim_pos_in_split_dims != 1) {
      split_dims <- c(split_dims[inner_dim_pos_in_split_dims],
                      split_dims[1:length(split_dims)][-inner_dim_pos_in_split_dims])
      split_dims_pos <- which(!is.na(match(names(final_dims_fake), names(split_dims))))
      # Save the current final_dims_fake for later reorder back
      final_dims_fake_output <- final_dims_fake
      new_dims <- c()
      if (split_dims_pos[1] != 1) {
        new_dims <- c(new_dims, final_dims_fake[1:(split_dims_pos[1] - 1)])
      }
      new_dims <- c(new_dims, split_dims)
      if (split_dims_pos[length(split_dims_pos)] < length(final_dims_fake)) {
        new_dims <- c(new_dims, final_dims_fake[(split_dims_pos[length(split_dims_pos)] + 1):length(final_dims_fake)])
      }
      final_dims_fake <- new_dims
    }
  }
  
  # The following several lines will only be run if retrieve = TRUE
  if (retrieve) {
    
    ########## CREATING THE SHARED MATRIX AND DISPATCHING WORK PIECES ###########
    # TODO: try performance of storing all in cols instead of rows
    # Create the shared memory array, and a pointer to it, to be sent
    # to the work pieces.
    data_array <- bigmemory::big.matrix(nrow = prod(final_dims), ncol = 1)
    shared_matrix_pointer <- bigmemory::describe(data_array)
    if (is.null(num_procs)) {
      num_procs <- future::availableCores()
    }
    # Creating a shared tmp folder to store metadata from each chunk
    array_of_metadata_flags <- array(FALSE, dim = dim(array_of_files_to_load))
    if (!is.null(metadata_dims)) {
      metadata_indices_to_load <- as.list(rep(1, length(dim(array_of_files_to_load))))
      names(metadata_indices_to_load) <- names(dim(array_of_files_to_load))
      metadata_indices_to_load[metadata_dims] <- as.list(rep(TRUE, length(metadata_dims)))
      array_of_metadata_flags <- do.call('[<-', c(list(array_of_metadata_flags),  metadata_indices_to_load,
                                                  list(value = rep(TRUE, prod(dim(array_of_files_to_load)[metadata_dims])))))
    }
    metadata_file_counter <- 0
    metadata_folder <- tempfile('metadata')
    dir.create(metadata_folder)
    # Build the work pieces, each with:
    # - file path
    # - total size (dims) of store array
    # - start position in store array
    # - file selectors (to provide extra info. useful e.g. to select variable)
    # - indices to take from file
    work_pieces <- list()
    for (i in 1:length(dat)) {
      if (dataset_has_files[i]) {
        selectors <- dat[[i]][['selectors']]
        file_dims <- found_file_dims[[i]]
        inner_dims <- expected_inner_dims[[i]]
        sub_array_dims <- final_dims[file_dims]
        sub_array_dims[found_pattern_dim] <- 1
        sub_array_of_files_to_load <- array(1:prod(sub_array_dims), 
                                            dim = sub_array_dims)
        names(dim(sub_array_of_files_to_load)) <- names(sub_array_dims)
        # Detect which of the dimensions of the dataset go across files.
        file_dim_across_files <- lapply(inner_dims, 
                                        function(x) {
                                          dim_across <- sapply(inner_dims_across_files, function(y) x %in% y)
                                          if (any(dim_across)) {
                                            names(inner_dims_across_files)[which(dim_across)[1]]
                                          } else {
                                            NULL
                                          }
                                        })
        names(file_dim_across_files) <- inner_dims
        j <- 1
        while (j <= prod(sub_array_dims)) {
          # Work out file path.
          file_to_load_sub_indices <- which(sub_array_of_files_to_load == j, arr.ind = TRUE)[1, ]
          names(file_to_load_sub_indices) <- names(sub_array_dims)
          file_to_load_sub_indices[found_pattern_dim] <- i
          big_dims <- rep(1, length(dim(array_of_files_to_load)))
          names(big_dims) <- names(dim(array_of_files_to_load))
          file_to_load_indices <- .MergeArrayDims(file_to_load_sub_indices, big_dims)[[1]]
          file_to_load <- do.call('[[', c(list(array_of_files_to_load), 
                                          as.list(file_to_load_indices)))
          not_found_file <- do.call('[[', c(list(array_of_not_found_files),
                                            as.list(file_to_load_indices)))
          load_file_metadata <- do.call('[', c(list(array_of_metadata_flags), 
                                               as.list(file_to_load_indices)))
          if (load_file_metadata) {
            metadata_file_counter <- metadata_file_counter + 1
          }
          if (!is.na(file_to_load) && !not_found_file) {
            # Work out indices to take
            first_round_indices <- lapply(inner_dims, 
                                          function (x) {
                                            if (is.null(file_dim_across_files[[x]])) {
                                              selectors[[x]][['fri']][[1]]
                                            } else {
                                              which_chunk <- file_to_load_sub_indices[file_dim_across_files[[x]]] 
                                              selectors[[x]][['fri']][[which_chunk]]
                                            }
                                          })
            names(first_round_indices) <- inner_dims
            second_round_indices <- lapply(inner_dims, 
                                           function (x) {
                                             if (is.null(file_dim_across_files[[x]])) {
                                               selectors[[x]][['sri']][[1]]
                                             } else {
                                               which_chunk <- file_to_load_sub_indices[file_dim_across_files[[x]]]
                                               selectors[[x]][['sri']][[which_chunk]]
                                             }
                                           })
            if (debug) {
              print("-> BUILDING A WORK PIECE")
              #print(str(selectors))
            }
            names(second_round_indices) <- inner_dims
            if (!any(sapply(first_round_indices, length) == 0)) {
              work_piece <- list()
              work_piece[['first_round_indices']] <- first_round_indices
              work_piece[['second_round_indices']] <- second_round_indices
              work_piece[['file_indices_in_array_of_files']] <- file_to_load_indices
              work_piece[['file_path']] <- file_to_load
              work_piece[['store_dims']] <- final_dims
              # Work out store position
              store_position <- final_dims
              store_position[names(file_to_load_indices)] <- file_to_load_indices
              store_position[inner_dims] <- rep(1, length(inner_dims))
              work_piece[['store_position']] <- store_position
              # Work out file selectors
              file_selectors <- sapply(file_dims, 
                                       function (x) {
                                         vector_to_pick <- 1
                                         if (x %in% names(depending_file_dims)) {
                                           vector_to_pick <- file_to_load_indices[depending_file_dims[[x]]]
                                         }
                                         selectors[file_dims][[x]][[vector_to_pick]][file_to_load_indices[x]]
                                       })
              names(file_selectors) <- file_dims
              work_piece[['file_selectors']] <- file_selectors
              # Send variables for transformation
              if (!is.null(transform) && (length(transform_vars) > 0)) {
                vars_to_transform <- NULL
                picked_vars_to_transform <- which(names(picked_vars[[i]]) %in% transform_vars)
                if (length(picked_vars_to_transform) > 0) {
                  picked_vars_to_transform <- names(picked_vars[[i]])[picked_vars_to_transform]
                  vars_to_transform <- c(vars_to_transform, picked_vars[[i]][picked_vars_to_transform])
                  if (any(picked_vars_to_transform %in% names(picked_vars_ordered[[i]]))) {
                    picked_vars_ordered_to_transform <- picked_vars_to_transform[which(picked_vars_to_transform %in% names(picked_vars_ordered[[i]]))]
                    vars_to_transform[picked_vars_ordered_to_transform] <- picked_vars_ordered[[i]][picked_vars_ordered_to_transform]
                  }
                }
                picked_common_vars_to_transform <- which(names(picked_common_vars) %in% transform_vars)
                if (length(picked_common_vars_to_transform) > 0) {
                  picked_common_vars_to_transform <- names(picked_common_vars)[picked_common_vars_to_transform]
                  vars_to_transform <- c(vars_to_transform, picked_common_vars[picked_common_vars_to_transform])
                  if (any(picked_common_vars_to_transform %in% names(picked_common_vars_ordered))) {
                    picked_common_vars_ordered_to_transform <- picked_common_vars_to_transform[which(picked_common_vars_to_transform %in% names(picked_common_vars_ordered))]
                    vars_to_transform[picked_common_vars_ordered_to_transform] <- picked_common_vars_ordered[picked_common_vars_ordered_to_transform]
                  }
                }
                work_piece[['vars_to_transform']] <- vars_to_transform
              }
              # Send flag to load metadata
              if (load_file_metadata) {
                work_piece[['save_metadata_in']] <- paste0(metadata_folder, '/', metadata_file_counter)
              }
              work_pieces <- c(work_pieces, list(work_piece))
            }
          }
          j <- j + 1
        }
      }
    }
    #print("N")
    if (debug) {
      print("-> WORK PIECES BUILT")
    }
    
    # Calculate the progress %s that will be displayed and assign them to 
    # the appropriate work pieces.
    if (length(work_pieces) / num_procs >= 2 && !silent) {
      if (length(work_pieces) / num_procs < 10) {
        amount <- 100 / ceiling(length(work_pieces) / num_procs)
        reps <- ceiling(length(work_pieces) / num_procs)
      } else {
        amount <- 10
        reps <- 10
      }
      progress_steps <- rep(amount, reps)
      if (length(work_pieces) < (reps + 1)) {
        selected_pieces <- length(work_pieces)
        progress_steps <- c(sum(head(progress_steps, reps)),
                            tail(progress_steps, reps))
      } else {
        selected_pieces <- round(seq(1, length(work_pieces), 
                                     length.out = reps + 1))[-1]
      }
      progress_steps <- paste0(' + ', round(progress_steps, 2), '%')
      progress_message <- 'Progress: 0%'
    } else {
      progress_message <- ''
      selected_pieces <- NULL
    }
    piece_counter <- 1
    step_counter <- 1
    work_pieces <- lapply(work_pieces, 
                          function (x) {
                            if (piece_counter %in% selected_pieces) {
                              wp <- c(x, list(progress_amount = progress_steps[step_counter]))
                              step_counter <<- step_counter + 1
                            } else {
                              wp <- x
                            }
                            piece_counter <<- piece_counter + 1
                            wp
                          })
    if (!silent) {
      .message("If the size of the requested data is close to or above the free shared RAM memory, R may crash.")
      .message("If the size of the requested data is close to or above the half of the free RAM memory, R may crash.")
      .message(paste0("Will now proceed to read and process ", length(work_pieces), " data files:"))
      if (length(work_pieces) < 30) {
        lapply(work_pieces, function (x) .message(x[['file_path']], indent = 2))
      } else {
        .message("The list of files is long. You can check it after Start() finishes in the output '$Files'.", indent = 2, exdent = 5)
      }
    }
    
    # Build the cluster of processes that will do the work and dispatch work pieces.
    # The function .LoadDataFile is applied to each work piece. This function will
    # open the data file, regrid if needed, subset, apply the mask, 
    # compute and apply the weights if needed,
    # disable extreme values and store in the shared memory matrix.
    #print("O")
    if (!silent) {
      .message("Loading... This may take several minutes...")
      if (progress_message != '') {
        .message(progress_message, appendLF = FALSE)
      }
    }

# NOTE: In .LoadDataFile(), metadata is saved in metadata_folder/1 or /2 etc. Before here,
#       the path name is created in work_pieces but the path hasn't been built yet.
    if (num_procs == 1) {
      found_files <- lapply(work_pieces, .LoadDataFile, 
                            shared_matrix_pointer = shared_matrix_pointer,
                            file_data_reader = file_data_reader, 
                            synonims = synonims,
                            transform = transform, 
                            transform_params = transform_params,
                            silent = silent, debug = debug)
    } else {
      cluster <- parallel::makeCluster(num_procs, outfile = "")
      # Send the heavy work to the workers
      work_errors <- try({
        found_files <- parallel::clusterApplyLB(cluster, work_pieces, .LoadDataFile, 
                                                shared_matrix_pointer = shared_matrix_pointer,
                                                file_data_reader = file_data_reader,
                                                synonims = synonims,
                                                transform = transform, 
                                                transform_params = transform_params,
                                                silent = silent, debug = debug)
      })
      parallel::stopCluster(cluster)
    }
    
    if (!silent) {
      if (progress_message != '') {
        .message("\n", tag = '')
      }
    }
    #print("P")
    
    # NOTE: If merge_across_dims = TRUE, there might be additional NAs due to
    #       unequal inner_dim ('time') length across file_dim ('file_date').
    #       If merge_across_dims_narm = TRUE, add additional lines to remove these NAs.
    # TODO: Now it assumes that only one '_across'. Add a for loop for more-than-one case. 
    if (merge_across_dims_narm) {
      
      # Get the length of these two dimensions in final_dims
      length_inner_across_store_dims <- final_dims[across_inner_dim]
      length_file_across_store_dims <- final_dims[across_file_dim]
      
      # Create a logical array for merge_across_dims
      logi_array <- array(rep(FALSE,
                              length_file_across_store_dims * length_inner_across_store_dims),
                          dim = c(length_inner_across_store_dims, length_file_across_store_dims))
      for (i in 1:length_file_across_store_dims) {  #1:4
        logi_array[1:length_inner_across_dim[[i]], i] <- TRUE
      }
      
      # First, get the data array with final_dims dimension
      data_array_final_dims <- array(bigmemory::as.matrix(data_array), dim = final_dims)
      
      # Change the NA derived from additional spaces to -9999, then remove these -9999
      func_remove_blank <- function(data_array, logi_array) {
        # dim(data_array) = [time, file_date]
        # dim(logi_array) = [time, file_date]
        # Change the blank spaces from NA to -9999
        data_array[which(!logi_array)] <- -9999
        return(data_array)
      }
      data_array_final_dims <- multiApply::Apply(data_array_final_dims,
                                                 target_dims = c(across_inner_dim, across_file_dim),  #c('time', 'file_date')
                                                 output_dims = c(across_inner_dim, across_file_dim),
                                                 fun = func_remove_blank,
                                                 logi_array = logi_array)$output1
      ## reorder back to the correct dim
      tmp <- match(names(final_dims), names(dim(data_array_final_dims)))
      data_array_final_dims <- .aperm2(data_array_final_dims, tmp)
      data_array_tmp <- data_array_final_dims[data_array_final_dims != -9999]  # become a vector
      
 #NOTE: When one file contains values for dicrete dimensions, rearrange the 
 #      chunks (i.e., work_piece) is necessary.
      if (split_multiselected_dims) {

      # generate the correct order list from indices_chunk 
        final_order_list <- list()
        i <- 1
        j <- 1
        a <- indices_chunk[i]
        while (i < length(indices_chunk)) {
          while (indices_chunk[i+1] == indices_chunk[i] & i < length(indices_chunk)) {
            a <- c(a, indices_chunk[i+1])
            i <- i + 1
          }
          final_order_list[[j]] <- a
          a <- indices_chunk[i+1]
          i <- i + 1
          j <- j + 1
        }
        names(final_order_list) <- sapply(final_order_list, '[[', 1)
        final_order_list <- lapply(final_order_list, length)
        
        if (!all(diff(as.numeric(names(final_order_list))) > 0)) {


      # shape the vector into the array without split_dims
        split_dims_pos <- match(split_dims, final_dims_fake)
        new_dims <- c()
        if (split_dims_pos[1] > 1) {
          new_dims <- c(new_dims, final_dims_fake[1:(split_dims_pos[1] - 1)])
        }
        new_dims <- c(new_dims,  prod(split_dims))
        names(new_dims)[split_dims_pos[1]] <- across_inner_dim
        if (split_dims_pos[length(split_dims_pos)] < length(final_dims_fake)) {
          new_dims <- c(new_dims, final_dims_fake[(split_dims_pos[length(split_dims_pos)] + 1):length(final_dims_fake)])
        }
        final_dims_fake_no_split <- new_dims
        data_array_no_split <- array(data_array_tmp, dim = final_dims_fake_no_split)
      # seperate 'time' dim into each work_piece length
        data_array_seperate <- list()
        tmp <- cumsum(unlist(length_inner_across_dim))
        tmp <- c(0, tmp)
        for (i in 1:length(length_inner_across_dim)) {
          data_array_seperate[[i]] <- Subset(data_array_no_split, across_inner_dim,
                                             (tmp[i] + 1):tmp[i + 1])
        }

      # re-build the array: chunk 
        which_chunk <- as.numeric(names(final_order_list))
        how_many_indices <- unlist(final_order_list)
        array_piece <- list()
        ind_in_array_seperate <- as.list(rep(1, length(data_array_seperate)))
        for (i in 1:length(final_order_list)) {
          array_piece[[i]] <- Subset(data_array_seperate[[which_chunk[i]]],
                                     across_inner_dim,
                                     ind_in_array_seperate[[which_chunk[i]]]:(ind_in_array_seperate[[which_chunk[i]]] + how_many_indices[i] - 1))
          ind_in_array_seperate[[which_chunk[i]]] <- ind_in_array_seperate[[which_chunk[i]]] + how_many_indices[i]
        }
        
        # re-build the array: paste
        data_array_tmp <- array_piece[[1]]
        along_pos <- which(names(dim(data_array_tmp)) == across_inner_dim)
        if (length(array_piece) > 1) {
          for (i in 2:length(array_piece)) {
          data_array_tmp <- abind::abind(data_array_tmp, array_piece[[i]],
                                        along = along_pos)
          }
        }
      }        
      }
  
      data_array <- array(data_array_tmp, dim = final_dims_fake)
      
    } else {  # merge_across_dims_narm = F (old version)
      data_array <- array(bigmemory::as.matrix(data_array), dim = final_dims_fake)
    }
    
    # NOTE: If split_multiselected_dims + merge_across_dims, the dimension order may change above.
    #       To get the user-required dim order, we need to reorder the array again.
    if (split_multiselected_dims & merge_across_dims) {
      if (inner_dim_pos_in_split_dims != 1) {
        correct_order <- match(names(final_dims_fake_output), names(final_dims_fake))
        data_array <- .aperm2(data_array, correct_order)
      }
    }
    
    gc()
    
    # Load metadata and remove the metadata folder
    if (!is.null(metadata_dims)) {
      loaded_metadata_files <- list.files(metadata_folder)

      if (!identical(loaded_metadata_files, character(0))) {  # old code
        loaded_metadata <- lapply(paste0(metadata_folder, '/', loaded_metadata_files), readRDS)
      } else {
        loaded_metadata <- NULL
      }

      unlink(metadata_folder, recursive = TRUE)

#NOTE: Here, metadata can be saved in one of two ways: one for $common and the other for $dat
#      for $common, it is a list of metadata length. For $dat, it is a list of dat length,
#      and each sublist has the metadata for each dat.
      dim_of_metadata <- dim(array_of_metadata_flags)[metadata_dims]
      if (!any(names(dim_of_metadata) == pattern_dims) |
          (any(names(dim_of_metadata) == pattern_dims) &
           dim_of_metadata[pattern_dims] == 1)) {  # put under $common; old code
        return_metadata <- vector('list',
                                  length = prod(dim(array_of_metadata_flags)[metadata_dims]))
        return_metadata[as.numeric(loaded_metadata_files)] <- loaded_metadata
        dim(return_metadata) <- dim(array_of_metadata_flags[metadata_dims])

      } else { # put under $dat. metadata_dims has 'dat' and dat length > 1
        return_metadata <- vector('list',
                                  length = dim_of_metadata[pattern_dims])
        names(return_metadata) <- dat_names
        for (kk in 1:length(return_metadata)) {
          return_metadata[[kk]] <- vector('list', length = prod(dim_of_metadata[-1])) # 1 is dat
        }
        loaded_metadata_count <- 1
        for (kk in 1:length(return_metadata)) {
          for (jj in 1:length(return_metadata[[kk]])) {
            return_metadata[[kk]][jj] <- loaded_metadata[[loaded_metadata_count]]
            names(return_metadata[[kk]])[jj] <- names(loaded_metadata[[loaded_metadata_count]])
            loaded_metadata_count <- loaded_metadata_count + 1
          }
        }
      }
      attr(data_array, 'Variables') <- return_metadata
      # TODO: Try to infer data type from loaded_metadata
      # as.integer(data_array) 
    }
    
    failed_pieces <- work_pieces[which(unlist(found_files))]
    for (failed_piece in failed_pieces) {
      array_of_not_found_files <- do.call('[<-', 
                                          c(list(array_of_not_found_files), 
                                            as.list(failed_piece[['file_indices_in_array_of_files']]),
                                            list(value = TRUE)))
    }
    if (any(array_of_not_found_files)) {
      for (i in 1:prod(dim(array_of_files_to_load))) {
        if (is.na(array_of_not_found_files[i])) {
          array_of_files_to_load[i] <- NA
        } else {
          if (array_of_not_found_files[i]) {
            array_of_not_found_files[i] <- array_of_files_to_load[i]
            array_of_files_to_load[i] <- NA
          } else {
            array_of_not_found_files[i] <- NA
          }
        }
      }
    } else {
      array_of_not_found_files <- NULL
    }
    
  } # End if (retrieve)
  
  # Change final_dims_fake back because retrieve = FALSE will use it for attributes later
  if (exists("final_dims_fake_output")) {
    final_dims_fake <- final_dims_fake_output
  }
  # Replace the vars and common vars by the transformed vars and common vars
  for (i in 1:length(dat)) {
    if (length(names(transformed_vars[[i]])) > 0) {
      picked_vars[[i]][names(transformed_vars[[i]])] <- transformed_vars[[i]]
    } else if (length(names(picked_vars_ordered[[i]])) > 0) {
      picked_vars[[i]][names(picked_vars_ordered[[i]])] <- picked_vars_ordered[[i]]
    }
  }
  if (length(names(transformed_common_vars)) > 0) {
    picked_common_vars[names(transformed_common_vars)] <- transformed_common_vars
  } else if (length(names(picked_common_vars_ordered)) > 0) {
    picked_common_vars[names(picked_common_vars_ordered)] <- picked_common_vars_ordered
  }
  if (debug) {
    print("-> THE TRANSFORMED VARS:")
    print(str(transformed_vars))
    print("-> THE PICKED VARS:")
    print(str(picked_vars))
  }
  
  file_selectors <- NULL
  for (i in 1:length(dat)) {
    file_selectors[[dat[[i]][['name']]]] <- dat[[i]][['selectors']][which(names(dat[[i]][['selectors']]) %in% found_file_dims[[i]])]
  }
  if (retrieve) {
    if (!silent) {
      .message("Successfully retrieved data.")
    }

    if (all(sapply(attr(data_array, 'Variables'), is.null))) {
      var_backup <- NULL
      .warning(paste0("Metadata cannot be retrieved. The reason may be the ",
                      "non-existence of the first file. Use parameter 'metadata_dims'",
                      " to assign to file dimensions along which to return metadata, ",
                      "or check the existence of the first file."))
    } else {

#NOTE: The metadata of variables can be saved in one of the two different structures.
#      (1) metadata_dims != 'dat', or (metadata_dims == 'dat' & length(dat) == 1):
#          put under $common
#      (2) (metadata_dims == 'dat' & length(dat) > 1):
#          put under $dat1, $dat2, .... Put it in picked_vars list
#TODO: The current (2) uses the inefficient method. Should define the list structure first
#      then fill the list, rather than expand it in the for loop.
    if (any(metadata_dims == pattern_dims) & length(dat) > 1) { # (2)
      var_backup <- attr(data_array, 'Variables')
      for (kk in 1:length(var_backup)) {
        sublist_names <- lapply(var_backup, names)[[kk]]
        for (jj in 1:length(sublist_names)) {
          picked_vars[[kk]][[sublist_names[jj]]] <- var_backup[[kk]][[jj]]
        }
      }
      var_backup <- NULL

    } else {  #(1)
      var_backup <- attr(data_array, 'Variables')
      var_backup_names <- unlist(lapply(var_backup, names))
      new_list <- vector('list', length = length(var_backup_names))
      count <- 1
      for (kk in 1:length(var_backup)) {
        for (jj in 1:length(var_backup[[kk]])) {
          new_list[[count]] <- var_backup[[kk]][[jj]]
          count <- count + 1
        }
      }
      names(new_list) <- var_backup_names
      var_backup <- new_list
    }
}
    attr(data_array, 'Variables') <- NULL
    attributes(data_array) <- c(attributes(data_array), 
                                list(Variables = c(list(common = c(picked_common_vars, var_backup)), 
                                                   picked_vars),
                                     Files = array_of_files_to_load, 
                                     NotFoundFiles = array_of_not_found_files,
                                     FileSelectors = file_selectors,
                                     PatternDim = found_pattern_dim)
    )
    attr(data_array, 'class') <- c('startR_array', attr(data_array, 'class'))
    data_array
  } else {
    if (!silent) {
      .message("Successfully discovered data dimensions.")
    }
    start_call <- match.call()
    for (i in 2:length(start_call)) {
      if (class(start_call[[i]]) %in% c('name', 'call')) {
        start_call[[i]] <- eval.parent(start_call[[i]])
      }
    }
    start_call[['retrieve']] <- TRUE
    attributes(start_call) <- c(attributes(start_call),
                                list(Dimensions = final_dims_fake,
                                     Variables = c(list(common = picked_common_vars), picked_vars),
                                     ExpectedFiles = array_of_files_to_load,
                                     FileSelectors = file_selectors,
                                     PatternDim = found_pattern_dim,
                                     MergedDims = if (merge_across_dims) {
                                       inner_dims_across_files
                                     } else {
                                       NULL
                                     },
                                     SplitDims = if (split_multiselected_dims) {
                                       all_split_dims
                                     } else {
                                       NULL
                                     })
    )
    attr(start_call, 'class') <- c('startR_cube', attr(start_call, 'class'))
    start_call
  }
}

# This function is the responsible for loading the data of each work
# piece.
.LoadDataFile <- function(work_piece, shared_matrix_pointer, 
                          file_data_reader, synonims,
                          transform, transform_params,
                          silent = FALSE, debug = FALSE) {
  #  suppressPackageStartupMessages({library(bigmemory)})
  ### TODO: Specify dependencies as parameter
  #  suppressPackageStartupMessages({library(ncdf4)})
  
  #print("1")
  store_indices <- as.list(work_piece[['store_position']])
  first_round_indices <- work_piece[['first_round_indices']]
  second_round_indices <- work_piece[['second_round_indices']]
  #print("2")
  file_to_open <- work_piece[['file_path']]
  sub_array <- file_data_reader(file_to_open, NULL, 
                                work_piece[['file_selectors']],
                                first_round_indices, synonims)
  if (debug) {
    if (all(unlist(store_indices[1:6]) == 1)) {
      print("-> LOADING A WORK PIECE")
      print("-> STRUCTURE OF READ UNTRANSFORMED DATA:")
      print(str(sub_array))
      print("-> STRUCTURE OF VARIABLES TO TRANSFORM:")
      print(str(work_piece[['vars_to_transform']]))
      print("-> COMMON ARRAY DIMENSIONS:")
      print(str(work_piece[['store_dims']]))
    }
  }
  if (!is.null(sub_array)) {
    # Apply data transformation once we have the data arrays.
    if (!is.null(transform)) {
      if (debug) {
        if (all(unlist(store_indices[1:6]) == 1)) {
          print("-> PROCEEDING TO TRANSFORM ARRAY")
          print("-> DIMENSIONS OF ARRAY RIGHT BEFORE TRANSFORMING:")
          print(dim(sub_array))
        }
      }
      sub_array <- do.call(transform, c(list(data_array = sub_array,
                                             variables = work_piece[['vars_to_transform']],
                                             file_selectors = work_piece[['file_selectors']]),
                                        transform_params))
      if (debug) {
        if (all(unlist(store_indices[1:6]) == 1)) {
          print("-> STRUCTURE OF ARRAY AND VARIABLES RIGHT AFTER TRANSFORMING:")
          print(str(sub_array))
          print("-> DIMENSIONS OF ARRAY RIGHT AFTER TRANSFORMING:")
          print(dim(sub_array$data_array))
        }
      }
      sub_array <- sub_array$data_array
      # Subset with second round of indices
      dims_to_crop <- which(!sapply(second_round_indices, is.null))
      if (length(dims_to_crop) > 0) {
        dimnames_to_crop <- names(second_round_indices)[dims_to_crop]
        sub_array <- Subset(sub_array, dimnames_to_crop, 
                            second_round_indices[dimnames_to_crop])
      }
      if (debug) {
        if (all(unlist(store_indices[1:6]) == 1)) {
          print("-> STRUCTURE OF ARRAY AND VARIABLES RIGHT AFTER SUBSETTING WITH 2nd ROUND INDICES:")
          print(str(sub_array))
        }
      }
    }
    
    metadata <- attr(sub_array, 'variables')
    
    names_bk <- names(store_indices)
    store_indices <- lapply(names(store_indices), 
                            function (x) {
                              if (!(x %in% names(first_round_indices))) {
                                store_indices[[x]]
                              } else if (is.null(second_round_indices[[x]])) {
                                1:dim(sub_array)[x]
                              } else {
                                if (is.numeric(second_round_indices[[x]])) {
                                  ## TODO: Review carefully this line. Inner indices are all 
                                  ## aligned to the left-most positions. If dataset A has longitudes
                                  ## 1, 2, 3, 4 but dataset B has only longitudes 3 and 4, then
                                  ## they will be stored as follows:
                                  ## 1, 2, 3, 4
                                  ## 3, 4, NA, NA
                                  ##x - min(x) + 1
                                  1:length(second_round_indices[[x]])
                                } else {
                                  1:length(second_round_indices[[x]])
                                }
                              }
                            })
    names(store_indices) <- names_bk
    if (debug) {
      if (all(unlist(store_indices) == 1)) {
        print("-> STRUCTURE OF FIRST ROUND INDICES FOR THIS WORK PIECE:")
        print(str(first_round_indices))
        print("-> STRUCTURE OF SECOND ROUND INDICES FOR THIS WORK PIECE:")
        print(str(second_round_indices))
        print("-> STRUCTURE OF STORE INDICES FOR THIS WORK PIECE:")
        print(str(store_indices))
      }
    }
    
    store_indices <- lapply(store_indices, as.integer)
    store_dims <- work_piece[['store_dims']]
    
    # split the storage work of the loaded subset in parts
    largest_dim_name <- names(dim(sub_array))[which.max(dim(sub_array))]
    max_parts <- length(store_indices[[largest_dim_name]])
    
    # Indexing a data file of N MB with expand.grid takes 30*N MB
    # The peak ram of Start is, minimum, 2 * total data to load from all files
    # due to inefficiencies in other regions of the code
    # The more parts we split the indexing done below in, the lower
    # the memory footprint of the indexing and the fast. 
    # But more than 10 indexing iterations (parts) for each MB processed 
    # makes the iteration slower (tested empirically on BSC workstations).
    subset_size_in_mb <- prod(dim(sub_array)) * 8 / 1024 / 1024
    best_n_parts <- ceiling(subset_size_in_mb * 10)
    # We want to set n_parts to a greater value than the one that would 
    # result in a memory footprint (of the subset indexing code below) equal
    # to 2 * total data to load from all files.
    # s = subset size in MB
    # p = number of parts to break it in
    # T = total size of data to load
    # then, s / p * 30 = 2 * T
    # then, p = s * 15 / T
    min_n_parts <- ceiling(prod(dim(sub_array)) * 15 / prod(store_dims))
    # Make sure we pick n_parts much greater than the minimum calculated
    n_parts <- min_n_parts * 10
    if (n_parts > best_n_parts) {
      n_parts <- best_n_parts
    }
    # Boundary checks
    if (n_parts < 1) {
      n_parts <- 1
    }
    if (n_parts > max_parts) {
      n_parts <- max_parts
    }
    
    if (n_parts > 1) {
      make_parts <- function(length, n) {
        clusters <- cut(1:length, n, labels = FALSE)
        lapply(1:n, function(y) which(clusters == y))
      }
      part_indices <- make_parts(max_parts, n_parts)
      parts <- lapply(part_indices, 
                      function(x) {
                        store_indices[[largest_dim_name]][x]
                      })
    } else {
      part_indices <- list(1:max_parts)
      parts <- store_indices[largest_dim_name]
    }
    
    # do the storage work
    weights <- sapply(1:length(store_dims), 
                      function(i) prod(c(1, store_dims)[1:i]))
    part_indices_in_sub_array <- as.list(rep(TRUE, length(dim(sub_array))))
    names(part_indices_in_sub_array) <- names(dim(sub_array))
    data_array <- bigmemory::attach.big.matrix(shared_matrix_pointer)
    for (i in 1:n_parts) {
      store_indices[[largest_dim_name]] <- parts[[i]]
      # Converting array indices to vector indices
      matrix_indices <- do.call("expand.grid", store_indices)
      # Given a matrix where each row is a set of array indices of an element
      # the vector indices are computed
      matrix_indices <- 1 + colSums(t(matrix_indices - 1) * weights)
      part_indices_in_sub_array[[largest_dim_name]] <- part_indices[[i]]
      data_array[matrix_indices] <- as.vector(do.call('[',
                                                      c(list(x = sub_array), 
                                                        part_indices_in_sub_array)))
    }
    rm(data_array)
    gc()
    
    if (!is.null(work_piece[['save_metadata_in']])) {
      saveRDS(metadata, file = work_piece[['save_metadata_in']])
    }
  }
  if (!is.null(work_piece[['progress_amount']]) && !silent) {
    message(work_piece[['progress_amount']], appendLF = FALSE)
  }
  is.null(sub_array)
}
