# startR v1.0.3 (Release date: 2020-06-19)
- Bugfix for requiring the repetitive values from a single file when using 
'merge_across_dims' and 'split_multiselected_dims'. The value positions were not 
correct before.
- Specify the time zone to be 'UTC' regarding time attributes retrieval. The time zone 
was not specified before and it caused problems when the time crosses daylight saving.
  
# startR v1.0.2 (Release date: 2020-05-11)
- Bugfix for longitude transformation when the required grid point across the borders. The bug apprears at v1.0.0 and v1.0.1.  
- Add one new parameter 'merge_across_dims_narm' in Start(). If it is TRUE,
the additional NAs in the across dimension will be removed. It is useful when 
a continuous time series is required, or parameter 'split_multiselected_dims' is
TRUE and expected dimensions are supposed to have no NAs. 
- Bugfix for the possible mixed dimension problem when 'split_multiselected_dims' and 
'merge_across_dims' are both used. 

# startR v1.0.1 (Release date: 2020-04-21)
- Bugfix for global longitude across the borders.  
- Bugfix for longitude transformation when across the borders.
- Bugfix for transform_extra_cells when across the borders.
- Bugfix for un-reorder longitude transformation crop.
  
# startR v1.0.0 (Release date: 2020-03-23)
- Bugfixes of lat and lon assigned by 'values' in Start(). In v0.1.4 it is incorrect when assigned from big to small values.
- Compatiblity break: Develop longitude and latitude reorder convention. 
The reordering functions (i.e., Sort() and CircularSort()) are well-functioning now.

# startR v0.1.4 (Release date: 2020-02-10)
- Bugfixes of transform in Start(). Change the default value of param 'extra_cells' to 2. (issue37)
- Bugfixes of chunk function in Utils.R (issue23)
- Bugfixes of paramter 'split_multiselected_dims' in ByChunk.R
- Bugfixes for chunking at dimensions which are assigned with list in Start() (issue38)
- Documentation improvement

# startR v0.1.3 (Release date: 2019-08-05)
- Add parameter 'use_attributes' in Step().
- Add paramter 'CDO_module' in Compute(cluster = list()). It is mandatory for using functions which depend on cdo (e.g., s2dverification::CDORemap) in operation.

