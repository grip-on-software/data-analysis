# Assorted collection of queries for experiments

This directory contains SQL queries, MonetDB/R functions and Jupyter notebooks 
using R code to perform various experiments on the Grip on Software database 
(GROS DB).

There is little organization to the SQL queries and they are mostly here as 
reference material. Some queries were later adapted for feature extraction 
using compilable query templates that are found in other directories of this 
[data analysis](https://github.com/grip-on-software/data-analysis) repository. 
This set is therefore only here as additional material detailing steps taken 
during the research project, not as a working product.

Roughly speaking, the following directories contain queries from specific 
moments of the research project:

- `initial`: Queries made during initial stages of the research project, mostly 
  for confirming correctness of data and curation of reported metrics, but also 
  some setups for queries and experiments that did not lead to features.
- `questions`: Queries that form the basis of answering questions from 
  interested parties (developers, Scrum coaches and quality engineers) in order 
  to help them understand the data set, before full visualizations were made 
  for such purposes.
- `update-tracker-job-sync`: Queries made from initial data acquisition setups 
  where the "update tracker", a file that indicates the state of the imported 
  data compared to the source system that it refers to, would sometimes be lost 
  and had to be regenerated (otherwise the data for a project would need to be 
  retrieved in full again, which is cumbersome to the pipeline performance). 
  This method became outdated once the database started storing the update 
  tracker and synchronizing it at the start of a pipeline job.
- `notebooks`: Experiments from a later stage of the research, where it was 
  relevant to be able to quickly report on the research output, without 
  creating full visualizations for certain enquiries while demonstrating the 
  potential of the data analysis approach by combining it with Jupyter Notebook 
  setups.
