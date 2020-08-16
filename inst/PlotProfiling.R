PlotProfiling <- function(configs, n_test = 1, file_name = NULL,
                          config_names = NULL, items = NULL, 
                          total_timings = TRUE, 
                          ideal_timings = FALSE, 
                          crop = NULL, subtitle = NULL) {
  check_package <- function(x) {
    if (!(x %in% installed.packages())) {
      stop("This function requires ", x, " to be installed.")
    }
  }
  sapply(c('reshape2', 'ggplot2', 'gridExtra'), check_package)
  library(reshape2)
  library(ggplot2)
  library(gridExtra)

  gglegend <- function(x) { 
    tmp <- ggplot_gtable(ggplot_build(x)) 
    leg <- which(sapply(tmp$grobs, function(y) y$name) == "guide-box") 
    tmp$grobs[[leg]]
  }

  # Check configs
  if (is.list(configs)) {
    if (!is.list(configs[[1]])) {
      configs <- list(setNames(list(configs), Sys.time()))
    } else {
      if (!is.list(configs[[1]][[1]])) {
        configs <- list(configs)
      }
    }
  } else {
    stop("Expected one or a list of 'startR_compute_profiling' objects in configs.")
  }

  # Check config names
  if (is.null(config_names)) {
    config_names <- paste0('config_', 1:length(configs))
  }

  # Check items
  if (is.null(items)) {
    items <- c('bychunks_setup', 'transfer', 'all_chunks', 'queue', 'job_setup',
               'load', 'compute', 'transfer_back', 'merge')
  }
  items <- c('nchunks', 'concurrent_chunks', 'cores_per_job', 'threads_load', 
             'threads_compute', items, 'total', 'all_chunks')

  all_timings <- NULL
  config_total_times <- NULL
  config_long_names <- NULL
  config_index <- 1
  for (timings in configs) {
    #config_name <- config_name[length(config_name)]
    #config_name <- strsplit(config_name, '.timings')[[1]]
    #config_name <- config_name[1]
    #config_name <- strsplit(config, 'tests/')[[1]]
    #config_name <- config_name[length(config_name)]

    #timings <- readRDS(config)
    dates <- names(timings)
    if (n_test > length(timings)) {
      selected_sample <- 1
    } else {
      selected_sample <- length(timings) + 1 - n_test
    }
    timings <- timings[[selected_sample]]
#    crop_value <- 400
#    timings[['total']] <- sapply(timings[['total']], function(x) min(crop_value, x))
#    timings[['queue']] <- sapply(timings[['queue']], function(x) min(crop_value, x))
    config_name <- config_names[config_index]
    config_long_name <- paste0('\n', config_name,
                          '\nDate: ', dates[selected_sample],
                          '\nN. chunks: ', timings[['nchunks']],
                          '\nMax. jobs: ', timings[['concurrent_chunks']],
                          '\nAsk cores: ', timings[['cores_per_job']],
                          '\nLoad thr:  ', timings[['threads_load']],
                          '\nComp. thr: ', timings[['threads_compute']],
                          '\n')
    config_long_names <- c(config_long_names, config_long_name)
    config_total_times <- c(config_total_times, timings[['total']])
    timings <- as.data.frame(timings)
    t_all_chunks <- timings[['total']] - timings[['bychunks_setup']] - timings[['merge']] - 
                    timings[['merge']]
    if (!is.na(timings[['transfer_back']])) {
      t_all_chunks <- t_all_chunks - timings[['transfer_back']]
    } else {
      #EEP
    }
    timings$all_chunks <- t_all_chunks
    timings <- timings[items[which(items %in% names(timings))]]
    if (ideal_timings) {
      timings[['T - [q] * N / M']] <- timings[['total']] - 
        mean(timings[['queue']]) * timings[['nchunks']] / timings[['concurrent_chunks']]
      timings[['b_s + ([j_s] + [l] + [c]) * N / M + m']] <- timings[['bychunks_setup']] +
        (mean(timings[['job_setup']]) + mean(timings[['load']]) + 
        mean(timings[['compute']])) * timings[['nchunks']] / 
        timings[['concurrent_chunks']] + timings[['merge']] 
    }
    timings$config <- config_long_name 
    #timings$confign <- config_index
    timings <- melt(timings, id.vars = c('config'))
    if (is.null(all_timings)) {
      all_timings <- timings
    } else {
      all_timings <- rbind(all_timings, timings)
    }
    config_index <- config_index + 1
  }
  if (!is.null(crop)) {
    all_timings$value <- sapply(all_timings$value, function(x) min(crop, x))
  }
  a <- as.factor(all_timings$config)
  all_timings$config <- a
  #new_levels <- levels(a)[order(nchar(levels(a)), levels(a))]
  new_levels <- config_long_names
  all_timings$config <- factor(all_timings$config, levels = new_levels)
  cols <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
  myPal <- cols(length(configs))
  items_total <- c('total')
  if (ideal_timings) {
    items_total <- c(items_total, 'T - [q] * N / M')
  }
  timings_total <- subset(all_timings, variable %in% items_total)
  if (is.null(subtitle)) {
    n_lines_subtitle <- 0
  } else {
    n_lines_subtitle <- length(strsplit(subtitle, "\n")[[1]])
  }
  plot_total <- ggplot(timings_total, aes(x = config, 
                                          y = value, fill = config, label = round(value))) +
                geom_bar(stat = 'summary', fun.y = 'mean') + facet_wrap(~variable, nrow = 1) +
                #geom_text(angle = 90, nudge_y = -10) + 
                labs(y = 'time (s)',
                     title = '    ',
                     subtitle = paste0(rep("\n", n_lines_subtitle), collapse = '')) +
                guides(fill = guide_legend(title = 'configurations')) +
                theme(axis.title.x = element_blank(),
                      axis.text.x = element_blank(),
                      axis.ticks.x = element_blank()) + 
                scale_fill_manual(values = myPal)

  if (ideal_timings) {
    items_ideal <- c('b_s + ([j_s] + [l] + [c]) * N / M + m')
    timings_ideal <- subset(all_timings, variable %in% items_ideal)
    plot_ideal <- ggplot(timings_ideal, aes(x = config, y = value, fill = config)) +
                  geom_bar(stat = 'summary', fun.y = 'mean') + 
                  facet_wrap(~variable, nrow = 1) +
                  labs(y = 'time (s)',
                       title = '    ') +
                  guides(fill = guide_legend(title = 'configurations')) +
                  theme(axis.title.x = element_blank(),
                        axis.text.x = element_blank(),
                        axis.ticks.x = element_blank()) + 
                  scale_fill_manual(values = myPal)
  }
  items_general <- items[which(items %in% c('bychunks_setup', 'transfer', 'all_chunks', 'merge'))]
  timings_general <- subset(all_timings, variable %in% items_general)
  plot_general <- ggplot(timings_general, aes(x = config, y = value, fill = config)) +
                  geom_bar(stat = 'summary', fun.y = 'mean') + facet_wrap(~variable, nrow = 1) +
                  labs(y = 'time (s)',
                       title = 'startR::Compute profiling',
                       subtitle = subtitle) + 
                  guides(fill = guide_legend(title = 'configurations')) +
                  theme(axis.title.x = element_blank(),
                        axis.text.x = element_blank(),
                        axis.ticks.x = element_blank()) + 
                  scale_fill_manual(values = myPal) 

  items_chunk <- items[which(items %in% c('queue', 'job_setup', 'load', 'compute', 'transfer_back'))]
  timings_chunk <- subset(all_timings, variable %in% items_chunk)
  plot_chunk <- ggplot(timings_chunk, aes(x = config, y = value, fill = config)) +
                geom_boxplot() + facet_wrap(~variable, nrow = 1) +
                labs(y = 'time (s)',
                     title = 'summary of performance of all chunks') +
  #                   subtitle = subtitle) + 
                guides(fill = guide_legend(title = 'configurations', ncol = ceiling(length(configs) / 10))) +
                theme(axis.title.x = element_blank(),
                      axis.text.x = element_blank(),
                      axis.ticks.x = element_blank()) + 
                scale_fill_manual(values = myPal)

  legend_cols <- ceiling(length(configs) / 10)
  legend_rows <- ceiling(length(configs) / legend_cols)
  if (legend_rows > 8) {
    height <- 30
  } else if (legend_rows > 6) {
    height <- 25
  } else if (legend_rows > 4) {
    height <- 20
  } else {
    height <- 15
  }
  width <- 25 + 5 * legend_cols
  if (!total_timings) {
    if (!ideal_timings) {
      plot <- list(plot_general + guides(fill = FALSE),
                   plot_chunk + guides(fill = FALSE),
                   gglegend(plot_chunk),
                   #top = 'startR::Compute() profiling',
                   widths = c(3, legend_cols),
                   layout_matrix = rbind(c(1, 3),
                                         c(2, 3)))
    } else {
      extra <- legend_cols - 1
      width <- width + (legend_cols + 1) * 6
      plot <- list(plot_general + guides(fill = FALSE),
                   plot_ideal + guides(fill = FALSE),
                   plot_chunk + guides(fill = FALSE),
                   gglegend(plot_chunk),
                   #top = 'startR::Compute() profiling',
                   widths = c(0.7 + extra / 4, 0.3 + extra / 4,
                              3 + extra / 2, legend_cols),
                   layout_matrix = rbind(c(1, 1, 1, 4),
                                         c(2, 3, 3, 4)))
    }
  } else {
    if (!ideal_timings) {
      plot <- list(plot_total + guides(fill = FALSE),
                   plot_general + guides(fill = FALSE),
                   plot_chunk + guides(fill = FALSE),
                   gglegend(plot_chunk),
                   #top = 'startR::Compute() profiling',
                   widths = c(1, 3, legend_cols),
                   layout_matrix = rbind(c(1, 2, 4),
                                         c(3, 3, 4)))
    } else {
      extra <- legend_cols - 1
      width <- width + (legend_cols + 1) * 6
      plot <- list(plot_total + guides(fill = FALSE),
                   plot_general + guides(fill = FALSE),
                   plot_ideal + guides(fill = FALSE),
                   plot_chunk + guides(fill = FALSE),
                   gglegend(plot_chunk),
                   #top = 'startR::Compute() profiling',
                   widths = c(0.7 + extra / 4, 0.3 + extra / 4,
                              3 + extra / 2, legend_cols),
                   layout_matrix = rbind(c(1, 1, 2, 5),
                                         c(3, 4, 4, 5)))
    }
  }
  if (!is.null(file_name)) {
    plot <- do.call('arrangeGrob', plot)
    ggsave(file_name, plot, units = 'cm', width = width, height = height)
  } else {
    do.call('grid.arrange', plot)
  }
}
