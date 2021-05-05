# Performance statistic collection for rusts database connection crates

This repository contains the numbers collected by diesel continuous scheduled benchmark actions to track changes over time. 

## Results

### Comparison

#### Trivial queries

![Postgresql](https://github.com/diesel-rs/metrics/raw/results/plots/summary/trivial_query_postgres.svg)
![Sqlite](https://github.com/diesel-rs/metrics/raw/results/plots/summary/trivial_query_sqlite.svg)
![Mysql](https://github.com/diesel-rs/metrics/raw/results/plots/summary/trivial_query_mysql.svg)

#### Medium complex queries

![Postgresql](https://github.com/diesel-rs/metrics/raw/results/plots/summary/medium_complex_query_postgres.svg)
![Sqlite](https://github.com/diesel-rs/metrics/raw/results/plots/summary/medium_complex_query_sqlite.svg)
![Mysql](https://github.com/diesel-rs/metrics/raw/results/plots/summary/medium_complex_query_mysql.svg)

#### Associations

![Postgresql](https://github.com/diesel-rs/metrics/raw/results/plots/summary/associations_postgres.svg)
![Sqlite](https://github.com/diesel-rs/metrics/raw/results/plots/summary/associations_sqlite.svg)
![Mysql](https://github.com/diesel-rs/metrics/raw/results/plots/summary/associations_mysql.svg)

#### Inserts

![Postgresql](https://github.com/diesel-rs/metrics/raw/results/plots/summary/insert_postgres.svg)
![Sqlite](https://github.com/diesel-rs/metrics/raw/results/plots/summary/insert_sqlite.svg)
![Mysql](https://github.com/diesel-rs/metrics/raw/results/plots/summary/insert_mysql.svg)

### Performance over time

#### Trivial queries

![Postgresql](https://github.com/diesel-rs/metrics/raw/results/plots/timeline/trivial_query_postgres.svg)
![Sqlite](https://github.com/diesel-rs/metrics/raw/results/plots/timeline/trivial_query_sqlite.svg)
![Mysql](https://github.com/diesel-rs/metrics/raw/results/plots/timeline/trivial_query_mysql.svg)

#### Medium complex queries

![Postgresql](https://github.com/diesel-rs/metrics/raw/results/plots/timeline/medium_complex_query_postgres.svg)
![Sqlite](https://github.com/diesel-rs/metrics/raw/results/plots/timeline/medium_complex_query_sqlite.svg)
![Mysql](https://github.com/diesel-rs/metrics/raw/results/plots/timeline/medium_complex_query_mysql.svg)

#### Associations

![Postgresql](https://github.com/diesel-rs/metrics/raw/results/plots/timeline/associations_postgres.svg)
![Sqlite](https://github.com/diesel-rs/metrics/raw/results/plots/timeline/associations_sqlite.svg)
![Mysql](https://github.com/diesel-rs/metrics/raw/results/plots/timeline/associations_mysql.svg)

#### Inserts

![Postgresql](https://github.com/diesel-rs/metrics/raw/results/plots/timeline/insert_postgres.svg)
![Sqlite](https://github.com/diesel-rs/metrics/raw/results/plots/timeline/insert_sqlite.svg)
![Mysql](https://github.com/diesel-rs/metrics/raw/results/plots/timeline/insert_mysql.svg)


## Raw data aggregated data

* [Postgresql](https://github.com/diesel-rs/metrics/blob/results/aggregated_data/postgres.csv)
* [Sqlite](https://github.com/diesel-rs/metrics/blob/results/aggregated_data/sqlite.csv)
* [Mysql](https://github.com/diesel-rs/metrics/blob/results/aggregated_data/mysql.csv)

