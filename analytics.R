library(purrr)
library(dplyr)
library(stringr)
library(ggplot2)
library(svglite)

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

aggregated_data = data %>% group_by(group, benchmark_name, value, timestamp, backend, commit) %>%
  summarise(mean_time = mean(measured_time),
            min_time = min(measured_time),
            max_time = max(measured_time)) %>%
  select(group, benchmark_name,value, timestamp, backend, commit, mean_time, max_time, min_time)

create_box_plots <- function(data, backend_value, time_stamp) {
    data = data %>% filter(backend == backend_value & timestamp == time_stamp)
    plot_path = paste(base_path, "/plots/", sep = "");
    names(data)[2] <- "Benchmark"
    commit = data$commit[1]
    ncol = 3
    
    width = 10
    height = 10
    
    insert = ggplot(data %>% filter(group == "bench_insert"),
           aes(x = Benchmark, y = measured_time, fill = Benchmark, group = Benchmark)) +
      geom_boxplot() +
      facet_wrap(~value, scales = "free_y", ncol = ncol) +
      xlab(label = "Benchmark") +
      ylab(label = "Time [ns]") +
      theme(axis.title.x=element_blank(),
	    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            legend.position = "bottom") +
      labs(title = paste("Insert (", backend_value, ") ", sep = ""),
           subtitle = paste(time_stamp, " (", commit, ")", sep = ""))
    
    ggsave(paste(plot_path, "summary/insert_", backend_value, ".svg", sep = ""), plot = insert, width = width, height = height)
    
    query_simple = ggplot(data %>% filter(group == "bench_trivial_query"),
                    aes(x = Benchmark, y = measured_time, fill = Benchmark, group = Benchmark)) +
      geom_boxplot() +
      facet_wrap(~value, scales = "free_y", ncol = ncol) +
      xlab(label = "Benchmark") +
      ylab(label = "Time [ns]") +
      theme(axis.title.x=element_blank(),
	    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            legend.position = "bottom") +
      labs(title = paste("Trivial query (", backend_value, ") ", sep = ""),
           subtitle = paste(time_stamp, " (", commit, ")", sep = ""))
    
    ggsave(paste(plot_path, "summary/trivial_query_", backend_value, ".svg", sep = ""), plot = query_simple, width = width, height = height)
    
    query_medium = ggplot(data %>% filter(group == "bench_medium_complex_query"),
                          aes(x = Benchmark, y = measured_time, fill = Benchmark, group = Benchmark)) +
      geom_boxplot() +
      facet_wrap(~value, scales = "free_y", ncol = ncol) +
      xlab(label = "Benchmark") +
      ylab(label = "Time [ns]") +
      theme(axis.title.x=element_blank(),
	    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            legend.position = "bottom") +
      labs(title = paste("Medium complex query (", backend_value, ") ", sep = ""),
           subtitle = paste(time_stamp, " (", commit, ")", sep = ""))
    
    ggsave(paste(plot_path, "summary/medium_complex_query_", backend_value, ".svg", sep = ""), plot = query_medium, width = width, height = height)
    
    associations = ggplot(data %>% filter(group == "bench_loading_associations_sequentially"),
                          aes(x = Benchmark, y = measured_time, fill = Benchmark, group = Benchmark)) +
      geom_boxplot() +
      xlab(label = "Benchmark") +
      ylab(label = "Time [ns]") +
      theme(axis.title.x=element_blank(),
	    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            legend.position = "bottom") +
      labs(title = paste("Associations (", backend_value, ") ", sep = ""),
           subtitle = paste(time_stamp, " (", commit, ")", sep = ""))
    
    ggsave(paste(plot_path, "summary/associations_", backend_value, ".svg", sep = ""), plot = associations, width = width, height = width)
}

max_date = data %>% group_by(backend) %>% summarise(timestamp = max(timestamp))
create_box_plots(data, max_date$backend[1], max_date$timestamp[1])
create_box_plots(data, max_date$backend[2], max_date$timestamp[2])
create_box_plots(data, max_date$backend[3], max_date$timestamp[3])

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
 
create_time_line(aggregated_data, "postgres")
create_time_line(aggregated_data, "sqlite")
create_time_line(aggregated_data, "mysql")
