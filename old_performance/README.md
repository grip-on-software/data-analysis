# Experiments for measuring performance of GROS queries on MonetDB

This document describes how we test the Grip on Software database setup with 
regards to the performance of the system.

## Scope

At this moment, we do not measure performance of MonetDB compared to other 
database management systems. We consider the choice for MonetDB as the most 
reasonable choice to be made when we started building the GROS pipeline, given 
its strong qualities for column based storage and integration with programming 
languages and diverse development/research environments. We could do some small 
comparisons between MonetDB and other DBMS, such as Postgres, SQLite and 
MariaDB with column storage, but this requires more thinking on how the data 
model, which was made specifically for the feature set of MonetDB, can be 
represented in another DBMS (including, but not limited to, decisions related 
to indexes and data type constraints). This is outside the scope of GROS query 
performance tests currently.

Instead, the focus is to compare a number of queries that we have used 
throughout the time of the data collection and research. The queries to use 
should be representative of the usual work load for the database, when we 
collect features for further analysis, machine learning and information 
visualization. They should collect data from various portions of our data model 
and use varying amounts of complexity in order to provide the necessary feature 
and associated details in the query response.

We only consider data access statements (`SELECT`), not other kinds of SQL 
queries for data definition (`CREATE` for new tables and so on) or data 
manipulation (`INSERT` and `UPDATE` from data acquisition). Queries to build 
the database and to fill it are not considered relevant enough for the typical 
work load.

We also disregard cache tables that were created specifically to hold outcomes 
of long-running queries. Cache tables and updates to them are disabled during 
the tests.

## Preconditions

- The system on which the tests are performed should not be interrupted by 
  other processes where possible. This means we should disable other services 
  and limit access to the server. If the performance tests run on a host with 
  other build systems, this means disabling e.g. Jenkins and disallowing other 
  HTTP access, for example by replacing fine-grained reverse proxy access from 
  a public server with local file hosting of the relevant directories.
- Queries should be performed one-by-one. Enough metrics should be retrieved to 
  properly demonstrate the resource usage during the query execution. This 
  includes new code to measure performance properly.
- We should also compare cold-start and hot-start query timings. A cold start 
  means rebuilding the database with data and running a query on this version, 
  without performing other queries in between, and dropping system memory 
  caches. A hot-start system has seen the query multiple times.
- The queries are started with a test program that reuses portions of the 
  existing code, and additionally ensures a reproducible and consistent setting 
  for all the queries.
- Multiple runs may be performed in order to obtain statistical metrics on how 
  much deviation there is between runs and what the mean value of each metric 
  is, allowing us to determine if the test setup and system behave similarly 
  between runs.

### System details

The tests are performed on a Dell PowerEdge 2950 2u rack-mounted server with 
a Intel Xeon 8-core (4 over 2 sockets) CPU X5450 3GHz, 2x128KiB L1 cache, 
2x12MiB L2 cache, four 4GiB DDR2 FB-DIMM RAM memory of Hynix HYMP351F72AMP4N3Y5 
(667MHz) and a 544GiB DELL PERC 6/i SCSI disk with LUKS-encrypted 512-bit AES 
full disk encryption enabled.

### Services to disable during experiments

- GROS deployment service (`systemctl stop gros-deployer`)
- GROS encrypted file upload server (`systemctl stop gros-uploader`)
- Jenkins (prepared shutdown in the web interface or `systemctl stop jenkins`)
- NGINX (`systemctl stop nginx`)
- ownCloud for dropin/prediction dataset exchange (`docker-compose down` in 
  `/srv/deploy/compose/owncloud`)
- Python package index (`docker-compose down` in `/srv/deploy/compose/pypi`)
- Docker registry (`docker-compose down` in `/srv/deploy/compose/registry`)
- SonarQube server (`docker-compose down` in `/srv/deploy/compose/sonar`)

## Queries (old vs. new)

We compare the performance of queries that we have optimized manually by going 
back to older versions of the query and using those. Note that changes could 
have been made to the query that are not relevant to the performance. We 
consider how to resolve those changes for the old queries and store the 
testable versions of the old queries in this directory, with reference to the 
old Git commit version where they were mostly based on.

1. Metric counts: `sprint_features/09-metric-count.sql` and 
   `09-red-metric-count.sql`: Replaced a JOIN with a direct ID comparison, 
   given that metrics outside of sprints were given a sprint ID of `0` and can 
   thus be easily filtered this way, without introducing problems (Jun 24 2019 
   @ `42ccda3a5391b3e28b670aa54e8e4a3b8093215d`)
2. Team spirit metric: `sprint_features/02-team-spirit-metric.sql`: Replaced 
   a "normal" subquery (many joins and late comparison) with a partitioning 
   subquery (window function) to pick the last measurement of a sprint directly 
   (Mar 22 2018 @ `cb92fcf20e9a3046bea56f7698e2e10a532d4154`) NB: Old query 
   could in some cases get an average value if there are multiple measurements 
   with the same date, the new query avoids that. Addendum: Newest query 
   replaced by metric-generic query which is built with the metric name from 
   a template, no longer within a query file itself.
3. Backlog sprint features: Moved conditions in where clause to join condition 
   in order to filter irrelevant stories earlier. Also, use coalesce operator 
   instead of `OR` (Feb 16 2021 @ `95301e7fcfd3c9cf7378fee569ac43f6f0038464`, 
   Jun 3 2019 @ `d15d186d684f5d40fda769146b63b93778f1f36b`)
4. Several queries: Added a filter to select specific sprints within queries 
   for features (non-patch sprints, etc.) such that expensive joins immediately 
   exclude that data (Oct 19 2021 @ `3ffa909f0e47e34e6b7ac89a79d37b600d8ba4e5`)
5. Done story points: `sprint_features/04-done-story-points.sql`: Replaced 
   multiple nested subqueries with joins. NB: Also includes other condition 
   changes (Dec 7 2018 @ `d47dc248d0097887a8da6518ae25fe228c82fafe`) Addendum: 
   Changes were required to make old query work.

## Metrics

Our `performance.r` script supports collecting measurements for the following 
metrics during the performance test, mostly based on what MonetDB itself 
reports:

- Wall clock time that a query took (from query request to data response)
- System time that the database system took during query processing
- Peak amount of memory in use by the system during the query, including 
  swapped memory if that is in use
- Amount of memory in use by the system before the query, for comparison
- Peak CPU load in use by the system during the query, as a percentage of load 
  per core (thus can be over 100%)
- The number of rows/columns that were included during the processing of query. 
  Because MonetDB is a column store, "tuples" in MonetDB often refer to the 
  columns. The result set can be inspected for both dimensions. The number of 
  rows *considered* during the query is more difficult to obtain, but may 
  potentially be read by debugging/profiling the MAL optimization pipeline 
  (probably not relevant enough to include)
- Metrics that MonetDB provides about the query, such as separate timings of 
  various steps (probably too specific)
- Size of tables that are involved in the query. In particular, the ratio 
  between table size and result set size may indicate somewhat about the 
  complexity of the query.

## Existing performance test setups

The [TPC-H](https://www.tpc.org/tpch/) benchmark is often used, if no specific 
query set is being considered. The benchmark contains a data model, data set 
and query specifications which are considered to be representative for 
a decision support problem, which has relevance in several types of businesses 
and industries. TPC-H and other benchmark suites sometimes come with tools to 
measure the performance of some database systems, but this is commercial and 
still needs adjustments for a particular DBMS, such as MonetDB. The 
specification does include some guidelines on how the measurements should be 
taken and how the system under test should function and be reported. This way, 
the tests can still be performed fairly when complying to the specification.

MonetDB has its own 
[testbench](https://www.monetdb.org/documentation-Sep2022/dev-guide/regression-testing/), 
but it is mostly focused on functional regression testing (queries should still 
work after changing code), not performance regressions (queries should stay 
fast after changing code).

Performance timing with MonetDB is possible in the command-line client using 
the `\t performance` flag. This provides a breakdown of time spent parsing the 
SQL query, optimizing the query in the internal MAL format, performing the 
table searches and a wall-clock time measured by the client, all in 
milliseconds with three digits precision behind comma (although we do not need 
such high precision).

More breakdown of what each step takes can be obtained by adding `TRACE` in 
front of the `SELECT` query, as a [debugging 
feature](https://www.monetdb.org/documentation-Jan2022/admin-guide/debugging-features/trace-sql-stmt/) 
similar to but more powerful than the more common `EXPLAIN` statement. Then, 
each MAL statement is associated with the time in milliseconds spent on it. 
This is probably not relevant for us.

Probably the best method of obtaining details on query performance is the 
[querylog](https://www.monetdb.org/documentation-Jan2022/user-guide/query-timing/). 
This is a feature of MonetDB that tracks the queries that have run before. By 
default, it is disabled, but it can be easily enabled through an SQL procedure 
call. When enabled, statistics are provided in `sys.querylog_catalog` and 
`sys.querylog_calls` tables, with `sys.querylog_history` providing a combined 
view. This includes the time in milliseconds spent in the optimization 
pipeline, start and end times, the number of tuple records (number of *columns* 
in the result set), time in milliseconds to generate the result and to export 
the result, average CPU load percentage and percentage of time waiting for IO 
(with the CPU free).

External performance monitoring tools may be used to track memory and CPU 
usage, such as `ps` and `top` on Linux. Care should be taken that these 
actually properly detect peak memory and load, but at the same time do not 
interfere with the performance test, so a proper interval for tracking resource 
usage is to be selected. Thus, additional code would be necessary to properly 
perform external measurements.
