% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Collect.R
\name{Collect}
\alias{Collect}
\title{Collect and merge the computation results}
\usage{
Collect(startr_exec, wait = TRUE, remove = TRUE)
}
\arguments{
\item{startr_exec}{An R object returned by Compute() when the parameter 'wait'
of Compute() is FALSE. It can be directly from a Compute() call or read from
the RDS file.}

\item{wait}{A logical value deciding whether the R session waits for the 
Collect() call to finish (TRUE) or not (FALSE). If TRUE, it will be a 
blocking call, in which Collect() will retrieve information from the HPC,
including signals and outputs, each polling_period seconds. The the status
can be monitored on the EC-Flow GUI. Collect() will not return until the 
results of all chunks have been received. If FALSE, Collect() will crash with
an error if the execution has not finished yet, otherwise it will return the
merged array. The default value is TRUE.}

\item{remove}{A logical value deciding whether to remove of all data results 
received from the HPC (and stored under 'ecflow_suite_dir', the parameter in
Compute()) after being collected. To preserve the data and Collect() it as 
many times as desired, set remove to FALSE. The default value is TRUE.}
}
\value{
A list of merged data array.
}
\description{
The final step of the startR workflow after the data operation. It is used when
the parameter 'wait' of Compute() is FALSE, and the functionality includes 
updating the job status shown on the EC-Flow GUI and collecting all the chunks
of results as one data array when the execution is done. See more details on 
\href{https://earth.bsc.es/gitlab/es/startR/}{startR GitLab}.
}
\examples{
 data_path <- system.file('extdata', package = 'startR')
 path_obs <- file.path(data_path, 'obs/monthly_mean/$var$/$var$_$sdate$.nc')
 sdates <- c('200011', '200012')
 data <- Start(dat = list(list(path = path_obs)),
               var = 'tos',
               sdate = sdates,
               time = 'all',
               latitude = 'all',
               longitude = 'all',
               return_vars = list(latitude = 'dat',
                                  longitude = 'dat',
                                  time = 'sdate'),
               retrieve = FALSE)
 fun <- function(x) {
           lat = attributes(x)$Variables$dat1$latitude
           weight = sqrt(cos(lat * pi / 180))
           corrected = Apply(list(x), target_dims = "latitude",
                             fun = function(x) {x * weight})
         }
 step <- Step(fun = fun,
              target_dims = 'latitude',
              output_dims = 'latitude',
              use_libraries = c('multiApply'),
              use_attributes = list(data = "Variables"))
 wf <- AddStep(data, step)
 \dontrun{
 res <- Compute(wf, chunks = list(longitude = 2, sdate = 2),
                threads_load = 1,
                threads_compute = 4,
                cluster = list(queue_host = 'nord3',
                               queue_type = 'lsf',
                               temp_dir = '/on_hpc/tmp_dir/',
                               cores_per_job = 2,
                               job_wallclock = '05:00',
                               max_jobs = 4,
                               extra_queue_params = list('#BSUB -q bsc_es'),
                               bidirectional = FALSE,
                               polling_period = 10
                ),
                ecflow_suite_dir = '/on_local_machine/username/ecflow_dir/',
                wait = FALSE)
 saveRDS(res, file = 'test_collect.Rds')
 collect_info <- readRDS('test_collect.Rds')
 result <- Collect(collect_info, wait = TRUE)
 }

}

