FROM r-base
RUN apt-get update \
	&& apt-get install -y --no-install-recommends libnetcdf-dev procps \
	&& rm -rf /var/lib/apt/lists/*
COPY requirements.txt /tmp/
RUN Rscript -e "install.packages(readLines('/tmp/requirements.txt'))"
