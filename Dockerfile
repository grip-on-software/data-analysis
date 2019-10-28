FROM r-base:3.6.1
RUN apt-get update \
	&& apt-get install -y --no-install-recommends libnetcdf-dev procps libssl-dev libxml2-dev \
	&& rm -rf /var/lib/apt/lists/*
COPY requirements.txt /tmp/
RUN Rscript -e "install.packages(readLines('/tmp/requirements.txt'))"

WORKDIR /home/docker
COPY *.r *.yml /home/docker/
COPY analysis_reports/ /home/docker/analysis_reports/
COPY include/ /home/docker/include/
COPY project_features/ /home/docker/project_features/
COPY sprint_events/ /home/docker/sprint_events/
COPY sprint_features/ /home/docker/sprint_features/
VOLUME /home/docker/output

CMD ["sleep", "infinity"]
