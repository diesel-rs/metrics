library(purrr)
library(dplyr)
library(stringr)
library(ggplot2)
library(svglite)

base_path = "."

files <- list.files(path = paste(base_path, "metrics", sep="/"), pattern="*.csv", recursive = TRUE, full.names = TRUE)
files <- files[grep("*/new/raw.csv$", files)]

extract_version_folder <- function(x) {
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

calculate_time_relative_to_diesel <- function (times, group, names) {
  if (group == "bench_insert") {
    idx <- which(names == "diesel")
    return(times/times[idx])
  } else if (group == "bench_loading_associations_sequentially") {
    idx <- which(names == "diesel/bench_loading_associations_sequentially")
    return(times/times[idx])
  } else if (group == "bench_medium_complex_query") {
    idx <- which(names == "diesel")
    return(times/times[idx])
  } else if (group == "bench_trivial_query") {
    idx <- which(names == "diesel")
    return(times/times[idx])
  } else {
    stop(paste("Unknown group:", group))
  }
}

data = map_df(files, function(x) read.csv(paste(base_path, x, sep="/")) %>% mutate(backend = extract_backend(x), timestamp = extract_timestamp(x), commit = extract_commit(x))) 
data$measured_time = data$sample_measured_value / data$iteration_count
names(data)[2] <- "benchmark_name"
data$backend = as.factor(data$backend)
data$commit = as.factor(data$commit)

aggregated_data = data %>% group_by(group, benchmark_name, value, timestamp, backend, commit) %>%
  summarise(mean_time = mean(measured_time),
            min_time = min(measured_time),
            max_time = max(measured_time)) %>%
  select(group, benchmark_name,value, timestamp, backend, commit, mean_time, max_time, min_time) %>% 
  group_by(group, value, timestamp, backend, commit) %>%
  mutate(relative_to_diesel = calculate_time_relative_to_diesel(mean_time, group[1], benchmark_name))

create_plots <- function(data, backend_value) {
  backend_data = data %>% filter(backend == backend_value)
  names(backend_data)[2] = "Benchmark"
  csv_path = paste(base_path, "/aggregated_data/", backend_value, ".csv", sep = "")
  write.csv(backend_data, file = csv_path)
  plot_path = paste(base_path, "/plots/", sep = "");
  
  
  insert = ggplot(backend_data %>% filter(group == "bench_insert"),
                  aes(x=timestamp, y = mean_time, color = Benchmark)) +
    geom_line() + geom_point() + facet_wrap(~ value, scales = "free_y") +
    labs(title = paste("Insert over time (", backend_value, ")", sep = ""),
         y = "Time (ns)") +
    geom_errorbar(aes(ymin = min_time, ymax = max_time), width=.1,
                  position=position_dodge(0.005))
  ggsave(paste(plot_path, "/timeline/insert_", backend_value, ".svg", sep = ""), plot = insert)
  
  simple_query = ggplot(backend_data %>% filter(group == "bench_trivial_query"),
                  aes(x=timestamp, y = mean_time, color = Benchmark)) +
    geom_line() + geom_point() + facet_wrap(~ value, scales = "free_y") +
    labs(title = paste("Trivial query over time (", backend_value, ")", sep = ""),
         y = "Time (ns)") +
    geom_errorbar(aes(ymin = min_time, ymax = max_time), width=.1,
                  position=position_dodge(0.005))
  
  ggsave(paste(plot_path, "/timeline/trivial_query_", backend_value, ".svg", sep = ""), plot = simple_query)
  
  medium_complex = ggplot(backend_data %>% filter(group == "bench_medium_complex_query"),
                  aes(x=timestamp, y = mean_time, color = Benchmark)) +
    geom_line() + geom_point() + facet_wrap(~ value, scales = "free_y") +
    labs(title = paste("Medium complex query over time (", backend_value, ")", sep = ""),
         y = "Time (ns)") +
    geom_errorbar(aes(ymin = min_time, ymax = max_time), width=.1,
                  position=position_dodge(0.005))
  
  ggsave(paste(plot_path, "/timeline/medium_complex_", backend_value, ".svg", sep = ""), plot = medium_complex)
  
  associations = ggplot(backend_data %>% filter(group == "bench_loading_associations_sequentially"),
                  aes(x=timestamp, y = mean_time, color = Benchmark)) +
    geom_line() + geom_point() +
    labs(title = paste("Associations over time (", backend_value, ")", sep = ""),
         y = "Time (ns)") +
    geom_errorbar(aes(ymin = min_time, ymax = max_time), width=.1,
                  position=position_dodge(0.005))
  
  ggsave(paste(plot_path, "/timeline/associations_", backend_value, ".svg", sep = ""), plot = associations)
  
  
  # build bar charts with relative times for the current version
  max_date = max(backend_data$timestamp)
  backend_data = backend_data %>% filter(timestamp >= max_date)
  
  insert = ggplot(backend_data %>% filter(group == "bench_insert") , 
                  aes(x = Benchmark, y = relative_to_diesel, fill = Benchmark))+
    geom_bar(stat = "identity") + facet_wrap(~value) +
    labs(title = paste("Inserts (", backend_value, ")", sep = ""),
         y = "Time relative to diesel") + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  ggsave(paste(plot_path, "summary/insert_", backend_value, ".svg", sep = ""), plot = insert)
  
  simple_query = ggplot(backend_data %>% filter(group == "bench_trivial_query") , 
                        aes(x = Benchmark, y = relative_to_diesel, fill = Benchmark))+
    geom_bar(stat = "identity") + facet_wrap(~value) +
    labs(title = paste("Simple Query (", backend_value, ")", sep = ""),
         y = "Time relative to diesel") + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  ggsave(paste(plot_path, "summary/trivial_query_", backend_value, ".svg", sep = ""), plot = simple_query)
  
  medium_complex_query = ggplot(backend_data %>% filter(group == "bench_medium_complex_query") , 
                        aes(x = Benchmark, y = relative_to_diesel, fill = Benchmark))+
    geom_bar(stat = "identity") + facet_wrap(~value) +
    labs(title = paste("Medium Complex Query (", backend_value, ")", sep = ""),
         y = "Time relative to diesel") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  ggsave(paste(plot_path, "summary/medium_complex_query_", backend_value, ".svg", sep = ""), plot = medium_complex_query)
  
  associations = ggplot(backend_data %>% filter(group == "bench_loading_associations_sequentially"),
         aes(x = Benchmark, y = relative_to_diesel, fill = Benchmark)) +
    geom_bar(stat = "identity") +
    labs(title = paste("Associations (", backend_value, ")", sep = ""),
         y = "Time relative to diesel") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  ggsave(paste(plot_path, "summary/associations_", backend_value, ".svg", sep = ""), plot = associations)
}
 
create_plots(aggregated_data, "postgres")
create_plots(aggregated_data, "sqlite")
create_plots(aggregated_data, "mysql")