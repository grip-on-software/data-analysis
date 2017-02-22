FROM r-base
RUN Rscript -e "install.packages(c('MonetDB.R', 'DBI', 'digest', 'foreign', 'jsonlite', 'logging', 'yaml', 'plyr'))"
