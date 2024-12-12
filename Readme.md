# Performance statistic collection for rusts database connection crates

This repository contains the numbers collected by diesel continuous scheduled benchmark actions to track changes over time. 

## Results

### Comparison

#### Trivial queries

![Postgresql](https://github.com/diesel-rs/metrics/raw/results/plots/summary/trivial_query_postgres.svg)

[Result Table](https://htmlpreview.github.io/?https://github.com/diesel-rs/metrics/blob/results/plots/table_bench_trivial_query_postgres.html)

![Sqlite](https://github.com/diesel-rs/metrics/raw/results/plots/summary/trivial_query_sqlite.svg)

[Result Table](https://htmlpreview.github.io/?https://github.com/diesel-rs/metrics/blob/results/plots/table_bench_trivial_query_sqlite.html)

![Mysql](https://github.com/diesel-rs/metrics/raw/results/plots/summary/trivial_query_mysql.svg)

[Result Table](https://htmlpreview.github.io/?https://github.com/diesel-rs/metrics/blob/results/plots/table_bench_trivial_query_mysql.html)

#### Medium complex queries

![Postgresql](https://github.com/diesel-rs/metrics/raw/results/plots/summary/medium_complex_query_postgres.svg)

[Result Table](https://htmlpreview.github.io/?https://github.com/diesel-rs/metrics/blob/results/plots/table_bench_medium_complex_query_postgres.html)

![Sqlite](https://github.com/diesel-rs/metrics/raw/results/plots/summary/medium_complex_query_sqlite.svg)

[Result Table](https://htmlpreview.github.io/?https://github.com/diesel-rs/metrics/blob/results/plots/table_bench_medium_complex_query_sqlite.html)

![Mysql](https://github.com/diesel-rs/metrics/raw/results/plots/summary/medium_complex_query_mysql.svg)

[Result Table](https://htmlpreview.github.io/?https://github.com/diesel-rs/metrics/blob/results/plots/table_bench_medium_complex_query_mysql.html)

#### Associations

![Postgresql](https://github.com/diesel-rs/metrics/raw/results/plots/summary/associations_postgres.svg)

[Result Table](https://htmlpreview.github.io/?https://github.com/diesel-rs/metrics/blob/results/plots/table_bench_loading_associations_sequentially_postgres.html)

![Sqlite](https://github.com/diesel-rs/metrics/raw/results/plots/summary/associations_sqlite.svg)

[Result Table](https://htmlpreview.github.io/?https://github.com/diesel-rs/metrics/blob/results/plots/table_bench_loading_associations_sequentially_sqlite.html)

![Mysql](https://github.com/diesel-rs/metrics/raw/results/plots/summary/associations_mysql.svg)

[Result Table](https://htmlpreview.github.io/?https://github.com/diesel-rs/metrics/blob/results/plots/table_bench_loading_associations_sequentially_mysql.html)

#### Inserts

![Postgresql](https://github.com/diesel-rs/metrics/raw/results/plots/summary/insert_postgres.svg)

[Result Table](https://htmlpreview.github.io/?https://github.com/diesel-rs/metrics/blob/results/plots/table_bench_insert_postgres.html)

![Sqlite](https://github.com/diesel-rs/metrics/raw/results/plots/summary/insert_sqlite.svg)

[Result Table](https://htmlpreview.github.io/?https://github.com/diesel-rs/metrics/blob/results/plots/table_bench_insert_sqlite.html)

![Mysql](https://github.com/diesel-rs/metrics/raw/results/plots/summary/insert_mysql.svg)

[Result Table](https://htmlpreview.github.io/?https://github.com/diesel-rs/metrics/blob/results/plots/table_bench_insert_mysql.html)

### Performance over time

#### Trivial queries

![Postgresql](https://github.com/diesel-rs/metrics/raw/results/plots/timeline/trivial_query_postgres.svg)
![Sqlite](https://github.com/diesel-rs/metrics/raw/results/plots/timeline/trivial_query_sqlite.svg)
![Mysql](https://github.com/diesel-rs/metrics/raw/results/plots/timeline/trivial_query_mysql.svg)

#### Medium complex queries

![Postgresql](https://github.com/diesel-rs/metrics/raw/results/plots/timeline/medium_complex_postgres.svg)
![Sqlite](https://github.com/diesel-rs/metrics/raw/results/plots/timeline/medium_complex_sqlite.svg)
![Mysql](https://github.com/diesel-rs/metrics/raw/results/plots/timeline/medium_complex_mysql.svg)

#### Associations

![Postgresql](https://github.com/diesel-rs/metrics/raw/results/plots/timeline/associations_postgres.svg)
![Sqlite](https://github.com/diesel-rs/metrics/raw/results/plots/timeline/associations_sqlite.svg)
![Mysql](https://github.com/diesel-rs/metrics/raw/results/plots/timeline/associations_mysql.svg)

#### Inserts

![Postgresql](https://github.com/diesel-rs/metrics/raw/results/plots/timeline/insert_postgres.svg)
![Sqlite](https://github.com/diesel-rs/metrics/raw/results/plots/timeline/insert_sqlite.svg)
![Mysql](https://github.com/diesel-rs/metrics/raw/results/plots/timeline/insert_mysql.svg)


## Raw aggregated data

* [Postgresql](https://github.com/diesel-rs/metrics/blob/results/aggregated_data/postgres.csv)
* [Sqlite](https://github.com/diesel-rs/metrics/blob/results/aggregated_data/sqlite.csv)
* [Mysql](https://github.com/diesel-rs/metrics/blob/results/aggregated_data/mysql.csv)

## How to generate the plots from the data

```sh
git clone https://github.com/diesel-rs/metrics/tree/results
R << analytics.R
```

The plots are generated inside the `plots` directory. The `aggregated_data` directory contains a summary of the raw criterion results collected in `metrics`
