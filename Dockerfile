FROM r-base:3.6.3
RUN apt-get update \
	&& apt-get install -y --no-install-recommends libnetcdf-dev procps libssl-dev libxml2-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev \
	&& rm -rf /var/lib/apt/lists/*
COPY requirements.txt install.r /tmp/
RUN ANALYSIS_REQUIREMENTS=/tmp/requirements.txt Rscript /tmp/install.r

WORKDIR /home/docker
COPY *.r *.yml config.yml.example /home/docker/
COPY analysis_reports/ /home/docker/analysis_reports/
COPY include/ /home/docker/include/
COPY project_features/ /home/docker/project_features/
COPY sprint_events/ /home/docker/sprint_events/
COPY sprint_features/ /home/docker/sprint_features/
VOLUME /home/docker/output

CMD ["sleep", "infinity"]
