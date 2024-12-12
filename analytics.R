library(purrr)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggpubr)
library(svglite)
library(gt)
library(gtExtras)
library(scales)

base_path = "."

files <- list.files(path = paste(base_path, "metrics", sep="/"), pattern="*.csv", recursive = TRUE, full.names = TRUE)
files <- files[grep("*/new/raw.csv$", files)]

extract_version_folder <- function(x) {
  x = str_replace(x, base_path, "")
  return(str_split(x, fixed("/"))[[1]][3])
}

extract_backend <- function(x) {
  x = extract_version_folder(x)
  backend <-str_split(str_extract(x, "[^/]+"), fixed("-"))[[1]][4]
  return(backend)
}

extract_timestamp <- function(x) {
  x = extract_version_folder(x)
  return(as.POSIXct(substr(x, 0, 15), format = "%Y%m%d-%H%M%S"))
}

extract_commit <- function(x) {
  x = extract_version_folder(x)
  return(substr(x, 17, 56))
}

data = map_df(files, function(x) read.csv(x) %>% mutate(backend = extract_backend(x), timestamp = extract_timestamp(x), commit = extract_commit(x))) 
data$measured_time = data$sample_measured_value / data$iteration_count
names(data)[2] <- "benchmark_name"
data$backend = as.factor(data$backend)
data$commit = as.factor(data$commit)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


plot_distribution_with_mean <- function(input_value, input_benchmark, data) {
  if (input_value == "NA") {
    by_value = data
  } else {
    by_value = data %>% filter(value == input_value)
  }
  benchmarks = by_value %>% select(Benchmark) %>% unique() %>% pull(Benchmark)
  colors = gg_color_hue(length(benchmarks))
  min_max = by_value %>% summarise(min_value = min(measured_time), max_value = max(measured_time))
  diff = (min_max$max - min_max$min) * 0.05
  
  by_value %>% filter(Benchmark == input_benchmark) %>%
    ggplot(aes(x=measured_time)) +
    geom_density(fill = colors[match(input_benchmark, benchmarks)], size = 2, trim = FALSE) + 
    geom_vline(aes(xintercept=median(measured_time)),
                 color="black", linetype="dashed", size=2) +
    theme_minimal() +
    scale_x_continuous(limits = c(min_max$min_value - diff, min_max$max_value + diff), breaks = scales::pretty_breaks(n = 3), labels = comma) + 
    labs(y = NULL) + guides(y = "none") +
    theme(axis.text=element_text(size=50, face = "bold"),
           axis.title=element_blank())
}

plot_facet_data <- function(data, backend_value, time_stamp, benchmark, title, plot_path, width, height, table_path, commit) {
  nrow = data %>% filter(group == benchmark) %>% pull(value) %>% n_distinct()
  bench_data = data %>% filter(group == benchmark) %>% arrange(Benchmark) %>% mutate(measured_time = measured_time/(1000))
  
  facet_labels = bench_data %>% pull(value) %>% unique 
  facet_label_callback <- function(x) {
    if (x == 1) {
      return ("1 Row")
    } else {
      return (paste(x, "Rows"))
    }
  }
  
  if (anyNA(facet_labels, recursive = FALSE)) {
    facet_labels = c()
  } else {
    facet_labels = set_names(facet_labels %>% map_chr(\(x) facet_label_callback(x)), facet_labels)
  }
  
  
  plot = ggplot(bench_data,
                        aes(x = measured_time, y = Benchmark, fill = Benchmark, group = Benchmark)) +
    geom_violin(trim = FALSE, draw_quantiles = c(0.5))  + 
    facet_wrap(~value, scales = "free_x", nrow = nrow, labeller = as_labeller(facet_labels)) +
    ylab(label = "Benchmark") +
    xlab(label = "Time [μs]") +
    scale_x_continuous(expand = c(0,0), limits = c(0, NA), labels = comma, breaks = scales::pretty_breaks(n = 7)) +
    theme(axis.title.y=element_blank(),
          legend.position = "bottom") +
    labs(title = paste(title, " (", backend_value, ") ", sep = ""),
         subtitle = paste(time_stamp, " (", commit, ")", sep = ""))
  
  
  summary_data = bench_data %>% group_by(Benchmark, value) %>% 
    summarise(min_time = min(measured_time), median_time = median(measured_time), max_time = max(measured_time)) %>%
    mutate(raw_data_key=paste(value, Benchmark, sep=",")) %>%
    arrange(value, desc(Benchmark))
  
  bench_count = summary_data %>% ungroup() %>% pull(Benchmark) %>% n_distinct()
  
  median_mins = summary_data %>% group_by(value) %>% mutate(row_id = row_number()) %>% slice_min(median_time)  %>% ungroup() %>% 
    mutate(row_id = row_id + (row_number() - 1)* bench_count) %>% 
    select(value, min_median_time_in_group=median_time, row_id)
  
  min_mins = summary_data %>% group_by(value) %>% mutate(row_id = row_number()) %>% slice_min(min_time)  %>% ungroup() %>% 
    select(value, min_min_time_in_group=min_time)
  
  max_mins = summary_data %>% group_by(value) %>% mutate(row_id = row_number()) %>% slice_min(max_time)  %>% ungroup() %>% 
    select(value, min_max_time_in_group=max_time)
  
  summary_data = summary_data %>% 
    inner_join(median_mins, by =join_by(value)) %>% 
    inner_join(max_mins, by =join_by(value)) %>%
    inner_join(min_mins, by =join_by(value)) %>%
    mutate(fact = median_time / min_median_time_in_group) %>% 
    mutate(fact_min = min_time / min_min_time_in_group) %>% 
    mutate(fact_max = max_time / min_max_time_in_group) %>% 
    select(Benchmark, value,raw_data_key, min_time, median_time, max_time, fact)
  
  if (anyNA(bench_data %>% pull(value) %>% unique, recursive = FALSE)) {
    groupname_col = NULL
    row_group_as_column = FALSE
    hide_column = "value"
  } else {
    groupname_col = "value"
    row_group_as_column = TRUE
    hide_column = "__nothing__"
  }
  
  table = summary_data %>% gt(rowname_col = "Benchmark", groupname_col = groupname_col, row_group_as_column = row_group_as_column) %>% 
    cols_units(min_time="[μs]", max_time="[μs]", median_time="[μs]", .units_pattern = "{1} {2}") %>%
    cols_label(min_time = "min", max_time = "max", median_time = "median", fact = "x Fastest", raw_data_key = "Density Plot") %>%
    cols_hide(columns = matches(hide_column)) %>%
    tab_header(
      title = paste(title, " (", backend_value, ") ", sep=""),
      subtitle = paste(time_stamp, " (", commit, ")", sep = ""),
    )  %>% 
    fmt_number(use_seps = TRUE) %>% 
    gt_highlight_rows(rows = median_mins$row_id) %>%
    text_transform(
      locations = cells_body(columns = "raw_data_key"),
      fn = function(column) {
        map(column, ~str_split_1(., ',')) |>
          map(~plot_distribution_with_mean(.[1], .[2], bench_data)) |>
          ggplot_image(height = px(50), aspect_ratio = 3)
      }
    )
  
  ggsave(plot_path, plot = plot, width = width, height = height)
  gtsave(data = table, filename = paste("table_", benchmark,"_", backend_value, ".html", sep = ""), path = table_path)

}

create_plots <- function(data, backend_value, time_stamp) {
    data = data %>% filter(backend == backend_value & timestamp == time_stamp)
    plot_path = paste(base_path, "/plots/", sep = "");
    names(data)[2] <- "Benchmark"
    commit = data$commit[1]
    ncol = 3
    
    width = 10
    height = 15
    
    plot_facet_data(data, backend_value, time_stamp, "bench_trivial_query", "Trivial query", paste(plot_path, "summary/trivial_query_", backend_value, ".svg", sep = ""), width, height, plot_path, commit)
    plot_facet_data(data, backend_value, time_stamp, "bench_medium_complex_query", "Medium complex query", paste(plot_path, "summary/medium_complex_query_", backend_value, ".svg", sep = ""), width, height, plot_path, commit)
    plot_facet_data(data, backend_value, time_stamp, "bench_insert", "Insert", paste(plot_path, "summary/insert_", backend_value, ".svg", sep = ""), width, height, plot_path, commit)
    plot_facet_data(data, backend_value, time_stamp, "bench_loading_associations_sequentially", "Associations", paste(plot_path, "summary/associations_", backend_value, ".svg", sep = ""), width, height / 2, plot_path, commit)
}

max_date = data %>% group_by(backend) %>% summarise(timestamp = max(timestamp))
backend_value = max_date$backend[1]
time_stamp = max_date$timestamp[1]
create_plots(data, max_date$backend[1], max_date$timestamp[1])
create_plots(data, max_date$backend[2], max_date$timestamp[2])
create_plots(data, max_date$backend[3], max_date$timestamp[3])

create_time_line <- function(data, backend_value) {
  backend_data = data %>% filter(backend == backend_value)
  names(backend_data)[2] = "Benchmark"
  csv_path = paste(base_path, "/aggregated_data/", backend_value, ".csv", sep = "")
  write.csv(backend_data, file = csv_path)
  plot_path = paste(base_path, "/plots/", sep = "");
  date = max(backend_data$timestamp)

  width = 10
  height = 8
  ncol = 3
  alpha = 0.15
  smooth_method = 'loess'
  smooth_span = 0.3


  insert = ggplot(backend_data %>% filter(group == "bench_insert"),
                  aes(x=timestamp, y = mean_time, color = Benchmark)) +
    geom_smooth(method = smooth_method, span = smooth_span) + geom_point(alpha=alpha) + facet_wrap(~ value, scales = "free_y", ncol = ncol) +
    labs(title = paste("Insert over time (", backend_value, ")", sep = ""),
         subtitle = as.character(date),
         y = "Time (ns)") +
    theme(legend.position = "bottom")
  ggsave(paste(plot_path, "/timeline/insert_", backend_value, ".svg", sep = ""), plot = insert, width = width, height = height)

  simple_query = ggplot(backend_data %>% filter(group == "bench_trivial_query"),
                  aes(x=timestamp, y = mean_time, color = Benchmark)) +
    geom_smooth(method = smooth_method, span = smooth_span) + geom_point(alpha=alpha) + facet_wrap(~ value, scales = "free_y", ncol = ncol) +
    labs(title = paste("Trivial query over time (", backend_value, ")", sep = ""),
         subtitle = as.character(date),
         y = "Time (ns)") +
    theme(legend.position = "bottom")

  ggsave(paste(plot_path, "/timeline/trivial_query_", backend_value, ".svg", sep = ""), plot = simple_query, width = width, height = height)

  medium_complex = ggplot(backend_data %>% filter(group == "bench_medium_complex_query"),
                  aes(x=timestamp, y = mean_time, color = Benchmark)) +
    geom_smooth(method = smooth_method, span = smooth_span) + geom_point(alpha=alpha) + facet_wrap(~ value, scales = "free_y", ncol = ncol) +
    labs(title = paste("Medium complex query over time (", backend_value, ")", sep = ""),
         subtitle = as.character(date),
         y = "Time (ns)")+
    theme(legend.position = "bottom")

  ggsave(paste(plot_path, "/timeline/medium_complex_", backend_value, ".svg", sep = ""), plot = medium_complex, width = width, height = height)

  associations = ggplot(backend_data %>% filter(group == "bench_loading_associations_sequentially"),
                  aes(x=timestamp, y = mean_time, color = Benchmark)) +
    geom_smooth(method = smooth_method, span = smooth_span) + geom_point(alpha=alpha) +
    labs(title = paste("Associations over time (", backend_value, ")", sep = ""),
         subtitle = as.character(date),
         y = "Time (ns)") +
    theme(legend.position = "bottom")

  ggsave(paste(plot_path, "/timeline/associations_", backend_value, ".svg", sep = ""), plot = associations, width = width, height = height)
}

aggregated_data = data %>% group_by(group, benchmark_name, value, timestamp, backend, commit) %>%
  summarise(mean_time = mean(measured_time),
            min_time = min(measured_time),
            max_time = max(measured_time)) %>%
  select(group, benchmark_name,value, timestamp, backend, commit, mean_time, max_time, min_time)

create_time_line(aggregated_data, "postgres")
create_time_line(aggregated_data, "sqlite")
create_time_line(aggregated_data, "mysql")
