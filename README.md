Automatically retrieve multidimensional distributed data sets in R
==================================================================

The first step in data analysis made easy
-----------------------------------------

Data retrieval and alignment is the first step in data analysis in any field and is often highly complex and time-consuming, especially nowadays in the era of Big Data, where large multidimensional data sets from diverse sources need to be combined and processed. Taking subsets of these datasets (Divide) to then be processed efficiently (and Conquer) becomes an indispensable technique.

`startR` (Subset, Transform, Arrange and ReTrieve multidimensional subsets in R) is an R project started at BSC with the aim to develop a tool that allows the user to automatically retrieve, homogenize and align subsets of multidimensional distributed data sets. It is an open source project that is open to external collaboration and funding, and will continuously evolve to support as many data set formats as possible while maximizing its efficiency.


What it does
------------

`startR`, through its main function `Start()`, provides an interface that allows to perceive and access one or a collection of data sets as if they all were part of a large multidimensional array. Indices or bounds can be specified for each of the dimensions in order to crop the whole array into a smaller sub-array. `Start()` will perform the required operations to fetch the corresponding regions of the corresponding files (potentially distributed over various remote servers) and arrange them into a local R multidimensional array. By default, as many cores as available locally are used in this procedure.

Usually, in retrieval processes previous to multidimensional data analysis, it is needed to apply a set of common transformations, pre-processes or reorderings to the data as it comes in. `Start()` accepts user-defined transformation or reordering functions to be applied for such purposes.

`Start()` is not bound to a specific file format. Interface functions to custom file formats can be provided for `Start()` to read them. As of April 2017 `startR` includes interface functions to the following file formats:
- NetCDF

Metadata and auxilliary data is also preserved and arranged by `Start()` in the measure that it is retrieved by the interface functions for a specific file format.


How to use it
-------------

`Start()` is the only function in the package intended for direct use. This function has a rather steep learning curve but it makes the retrieval process straightforward and highly customizable. The header looks as follows:

```R
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
      silent = FALSE)
```

Usually most of the required information will be provided through the `...` parameter and only a few of the parameters in the function header will be used.

The parameters can be grouped as follows:
- The parameters provided via `...`, with information on the structure of the datasets which to take data from, information on which dimensions they have, which indices to take from each of the dimensions, and how to reorder each of the dimensions if needed.
- `synonims` with information to identify dimensions and variablse across the multiple files/datasets (useful if they don't follow a homogeneous naming convention).
- `return_vars` with information on auxilliary data to be retrieved.
- `file_*` parameters, which allow to specify interface functions to the desired file format.
- `transform_*` parameters, which allow to specify a transformation to be applied to the data as it comes in.
- Other general configuration parameters.

Examples / Tutorial
-------------------

Next, an explanation on how to use `Start()`, starting from a simple example and progressively adding complexity.

### Retrieving a single entire file
`Start()` can be used in the simplest situation to take a subset of data from a single file.

Let's imagine we have a file named 'file.nc' which contains an array of data (a single variable) with the following dimensions:
```
c(a = 5, time = 12, b = 100, c = 100)
```

It is then possible to take the entire data file as follows:
```R
data <- Start(dataset = '/path/to/file.nc',
              a = 'all',
              time = 'all', 
              b = 'all',
              c = 'all')
* Exploring files... This will take a variable amount of time depending
*   on the issued request and the performance of the file server...
* Detected dimension sizes:
*   dataset: 1
*         a: 5
*      time: 3
*         b: 100
*         c: 100
* Total size of requested data:
*   1 x 5 x 3  x 100 x 100 x 8 bytes = 1.2 Mb
* If the size of the requested data is close to or above the free shared
*   RAM memory, R may crash.
* If the size of the requested data is close to or above the half of the
*   free RAM memory, R may crash.
* Will now proceed to read and process 1 data files:
*   /path/to/file.nc
* Loading... This may take several minutes...
starting worker pid=30733 on localhost:11276 at 15:30:39.997
starting worker pid=30749 on localhost:11276 at 15:30:40.180
starting worker pid=30765 on localhost:11276 at 15:30:40.382
starting worker pid=30781 on localhost:11276 at 15:30:40.557
* Successfully retrieved data.
```

None of the provided parameters to `Start()` are in the set of known parameters (`return_vars`, `synonims`, ...). Each unknown parameter is interpreted as a specification of a dimension of the data set you want to retrieve data from, where the name of the parameter matches the name of the dimension and the value associated expresses the indices you want to retrieve from the dimension. In this example, we have defined that the data set has 5 dimensions, with the names 'dataset', 'a', 'time', 'b', and 'c', and we want to take all indices of each of these dimensions.

It is mandatory to make `Start()` aware of all the existing dimensions in the file (unless they are of length 1).

Note that the file to read is considered to be an element that belongs to the 'dataset' dimension (could be any other name). `Start()` automatically looks for at least one of the dimension specifications with an expression pointing to a set of files or URLs.

The returned result looks as follows:
```R
str(data)
List of 5
 $ Data         : num [1, 1:5, 1:3, 1:100, 1:100] 1 2 3 4 5 6 7 8 ...
 $ Variables    :List of 2
  ..$ common  : NULL
  ..$ dataset1: NULL
 $ Files        : chr [1(1d)] "/path/to/file.nc"
 $ NotFoundFiles: NULL
 $ FileSelectors:List of 1
  ..$ dataset1:List of 1
  .. ..$ dataset:List of 1
  .. .. ..$ : chr "dataset1"
```

In this case, most of the returned information is empty.

These are the dimensions of the actual data array:
```R
dim(data$Data)
dataset       a    time       b       c
      1       5       3     100     100
```

### Reordering array dimensions
If the dimensions are specified and requested in a different order, the resulting array will be arranged following the same order:
```R
data <- Start(dataset = '/path/to/file.nc',
              a = 'all',
              b = 'all',
              c = 'all',
              time = 'all')
dim(data$Data)
dataset       a       b       c    time
      1       5     100     100       3
```

### Retrieving multiple files
Assuming we have the files
```
  /path/to/
         |-> group_a/
         |    |-> file_1.nc
         |    |-> file_2.nc
         |    |-> file_3.nc
         |-> group_b/
              |-> file_1.nc
              |-> file_2.nc
              |-> file_3.nc
```
We can load them as follows:
```R
data <- Start(dataset = '/path/to/group_$group$/file_$number$.nc',
              group = 'all',
              number = 'all',
              a = 'all',
              b = 'all',
              c = 'all',
              time = 'all')
dim(data$Data)
dataset   group  number       a       b       c    time
      1       2       3       5     100     100       3
```

### Path pattern tags that depend on other tags
Assuming we have the files
```
  /path/to/
         |-> group_a/
         |    |-> file_1.nc
         |    |-> file_2.nc
         |    |-> file_3.nc
         |-> group_b/
              |-> file_4.nc
              |-> file_5.nc
              |-> file_6.nc
```
We can load them as follows:
```R
data <- Start(dataset = '/path/to/group_$group$/file_$number$.nc',
              group = 'all',
              number = 'all',
              number_depends = 'group',
              a = 'all',
              b = 'all',
              c = 'all',
              time = 'all')
dim(data$Data)
dataset   group  number       a       b       c    time
      1       2       3       5     100     100       3
```

### Dimensions inside the files that go across files
Assuming the 'time' dimension goes across all the 'number' files in a group. We would like to select time indices e.g. 3 to 7 without `Start()` crashing because of indices out of bounds. We can do so as follows:
```R
data <- Start(dataset = '/path/to/group_$group$/file_$number$.nc',
              group = 'all',
              number = 'all',
              number_depends = 'group',
              a = 'all',
              b = 'all',
              c = 'all',
              time = indices(list(2, 5)),
              time_across = 'number')
dim(data$Data)
dataset   group  number       a       b       c    time
      1       2       3       5     100     100       5
```
In this case, the dimension 'number' is of length 3 because we have retrieved data from 3 different 'number's: the 'time' index 3 from 'number' 1, the 'time' indices 4 to 6 from 'number' 2 and the 'time' index 7 from 'number 3. The non-taken indices from a 'number' are filled in with NA in the returned array.

### Taking specific indices of a dimension
```R
data <- Start(dataset = '/path/to/file.nc',
              a = indices(c(1, 3)),
              b = 'all',
              c = indices(list(10, 20)),
              time = 'all')
dim(data$Data)
dataset       a       b       c    time
      1       2     100      11       3
```

### Taking specific indices of a dimension in function of associated values
```R
data <- Start(dataset = '/path/to/file.nc',
              a = c('value1', 'value2', 'value5'),
              a_var = 'x',
              b = 'all',
              c = indices(list(10, 20)),
              time = 'all',
              return_vars = list(a_var = NULL))
dim(data$Data)
dataset       a       b       c    time
      1       3     100      11       3
```

### Taking data from NetCDF files with multiple variables
Now let us imagine the data array in the file has an extra dimension, 'var', of length 2, and a variable 'var_names' with the names of the variables at each position along the dimension 'var'. The names of the 2 variables are 'x' and 'y'. We would like being able to tell `Start()` to take only the variable 'y', regardless of its position along the 'var' dimension. This can be achieved by defining the 'var' dimension with more detail, using the '*_var' parameters:

```R
data <- Start(dataset = '/path/to/file.nc',
              var = 'y',
              var_var = 'var_names',
              a = 'all',
              b = 'all',
              c = 'all',
              time = 'all',
              return_vars = list(var_names = NULL))
dim(data$Data)
dataset       var       a       b       c    time
      1         1       5     100     100       3
```

### Taking specific indices of a dimension in function of associated values, with tolerance


### Dimension and variable name synonims


### Reordering inner dims with associated values


### Transformations


### Defining interface functions to a custom file format


### Explanation of outputs


### Fetching metadata


### Other configuration parameters
