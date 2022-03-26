# Software development process data analysis

This repository contains R scripts and integrated SQL queries that collect, 
analyze and export data from a Grip on Software database, which is gathered 
from various sources used in software development teams and projects.

## Installation

The data analysis scripts require R version 3.6.x. The scripts have been tested 
on MacOS 10.14+ and Ubuntu LTS 18.04+.

In order to install the dependencies, run the following command within this 
directory: `Rscript install.r`.

This repository also contains a `Dockerfile` which allows building the 
dependencies and installation of the scripts and queries within a Docker image. 
Using `docker build -t gros-data-analysis .`, the Docker image can be built. 
Note that you should already have a `config.yml` file, otherwise you have to 
include one via a volume and point at it when running the scripts using 
a `--config` argument.

## Configuration

The R scripts receive initial configuration using a YAML file. Copy the file 
`config.yml.example` to `config.yml` and replace the variables in the following 
configuration items within their sections to properly configure the scripts:

- `db`: Section for database connection parameters.
  - `host` (`$MONETDB_HOSTNAME`): The FQDN of the host that serves the MonetDB 
    database to connect to.
  - `dbname` (`$MONETDB_DATABASE`): The name of the database to connect to.
  - `user` (`$MONETDB_USER`): Username to use for connecting to the database.
  - `password` (`$MONETDB_PASSWORD`): Password of the user to connect to the 
    database with at least `SELECT`, `INSERT` and `DELETE` privileges.
- `fields` (used in some cases by `backlog-size-interval.r`, `features.r` and 
  `sprint_results.r`): URLs and related indicators for sources and additional 
  features. Sometimes only used as a fallback if a source has no valid URL.
  - `jira_url` (`$JIRA_URL`): URL to a Jira instance or other website 
    explaining what the Jira source is (e.g. an Atlassian documentation site).
  - `vcs_url` (`$SOURCE_URL`): URL to a version control system or another 
    website that explains the most likely type of VCS in use (e.g. GitLab).
  - `jenkins_url` (`$JENKINS_URL`): URL to a Jenkins CI instance or other 
    website explaining what the Jenkins source is.
  - `quality_time_url` (`$QUALITY_TIME_URL`): URL to a Quality Time instance or 
    other website explaining what the Quality Time source is.
  - `quality_url` (`$QUALITY_URL`): URL to a Quality Report instance or other 
    website explaining what the Quality Report source is.
  - `metric_history_file` (`$METRIC_HISTORY_FILE`): Name of a metric history 
    file from a Quality Report instance, for example `compact-history.json`.
  - `metric_options_file` (`$METRIC_OPTIONS_FILE`): Name of a metric options 
    file from a Quality Report configuration repository, for example 
    `project_definition.py`.
  - `prediction_combine` (`$PREDICTION_COMBINE`): Operation to use to combine 
    predictions for overlapping/team sprints, for example `mean`.
  - `prediction_data` (`$PREDICTION_DATA`): URL from which prediction results 
    can be obtained to include as a feature (e.g. in a report).
  - `prediction_url` (`$PREDICTION_URL`): URL to a human-readable website that 
    explains the source of the prediction results, which may itself contain 
    some format patterns such as `${jira_key}`.
  - `sprint_patch` (`$SPRINT_PATCH`): SQL query pattern to use to determine if 
    a sprint has a patch name or not. If no differentiation between normal 
    sprints and patch sprints is necessary, then this can be set to `'false'`.
- `weather` (used by `weather.r`): External weather data collection.
  - `url` (`$WEATHER_URL`): URL to an API endpoint that indicates locations of 
    files for daily temperatures, for example the API URL pointed by the [KNMI 
    dataset](https://dataplatform.knmi.nl/nl/dataset/etmaalgegevensknmistations-1).
  - `api_key` (`$WEATHER_API_KEY`): An API key to use to retrieve the files for 
    daily temperatures, for example as explained at the [KNMI developer 
    platform](https://developer.dataplatform.knmi.nl/get-started#obtain-an-api-key).
  - `lat` (`$WEATHER_LAT`): WGS latitude to use for searching for a nearby 
    active weather station with temperature data.
  - `lon` (`$WEATHER_LON`): WGS longitude to use for searching for a nearby 
    active weather station with temperature data.
  - `origin` (`$WEATHER_DATE_ORIGIN`): First date within the NC file for daily 
    temperatures, for example `1950-01-01` for KNMI data.
- `exclude_domain`: Domain names to exclude for metrics. This section contains 
  a list with each item (`$EXCLUDE_DOMAIN_NAME`) indicating a domain name from 
  a quality control system which is never used when aggregating values or 
  providing details for a feature of a metric.
- `teams`: Section for combined teams. This section contains a list, with each 
  item indicating a team and how it is combined from existing projects, with 
  the following items (not all are required):
  - `name` (`$TEAM_NAME`): The key for the team.
  - `display_name` (`$TEAM_DISPLAY_NAME`): The longer display name of the team.
  - `board` (`$TEAM_BOARD`): The Jira board ID that is in use by the team to 
    display the backlog relevant to them.
  - `recent` (`$TEAM_RECENT`): Whether the team should be considered recent or 
    not. If not provided, the recentness of the team is determined by the most 
    recent sprint start date (as usual for projects).
  - `invisible` (`$TEAM_INVISIBLE`): Whether the team should be hidden from 
    reports (avoids display of the aggregate features). If not provided, then 
    the team is visible.
  - `overlap` (`$TEAM_OVERLAP`): The number of days to consider for combining 
    sprints whose start/close date overlap slightly. This may be used to 
    combine sprints from a single project (e.g. a prior merge or patch sprints) 
    as well as from different projects. If not provided, then no sprint 
    combining takes place.
  - `projects`: A list of projects to include in the team, possibly with some 
    constraints and adjustments. Each project may be a project key, or an 
    object that has the following items:
    - `key` (`$TEAM_PROJECT_KEY`): The project key to include in the team.
    - `board` (`$TEAM_PROJECT_BOARD`): If provided, only include sprints that 
      have their primary board ID set to this board ID (if this is a scalar) or 
      on of these board IDs (for a list of scalars) for the team.
    - `replace` (`$TEAM_PROJECT_REPLACE`): Whether to replace the entire 
      project. If the project is replaced, then any data not used for the team 
      is not exported for the report under the project's former name. This can 
      be used to filter out data from a project team. If not provided, then the 
      project is left as is, or filtered of selected sprints if `board` was 
      provided.
    - `own_board` (`$TEAM_PROJECT_OWN_BOARD`): If `board` was provided and 
      `replace` is not `true`, then the original project that is filtered of 
      selected sprints will be set to use the first ID in `board` as its board, 
      instead of the team's board. This allows each project within the team to 
      still link to a specific board under these circumstances.
    - `start_date` (`$TEAM_PROJECT_START_DATE`): Only include sprints of the 
      project with a start date after this date for the team.
    - `end_date` (`$TEAM_PROJECT_END_DATE`): Only include sprints of the 
      project with a close date before this date for the team.
    - `exclude` (`$TEAM_PROJECT_EXCLUDE`): Do not include sprints of the 
      project with a name that matches a pattern.
    - `include` (`$TEAM_PROJECT_INCLUDE`): Only include sprints of the project 
      with a name that matches a pattern.
- `components`: Section for separated components. This section contains a list, 
  with each item indicating a component and how it is extracted from a project,
  with the following items (not all are required):
  - `name` (`$COMPONENT_NAME`): The key for the component.
  - `display_name` (`$COMPONENT_DISPLAY_NAME`): The longer display name for the 
    component.
  - `project` (`$COMPONENT_PROJECT`): The project key to extract the component 
    from.
  - `$COMPONENT_SOURCE`: An item with constraint indicators for different 
    sources. The variable can be replaced with a source type (defined in 
    `source_types.yml`), and each source type then indicates how features 
    extracted from that source should be separated into the component and the 
    rest of the project (based on the values of a component field). The 
    constraints can be one or both of the following:
    - `include` (`$COMPONENT_INCLUDE`): Only include values for components in 
      this list component names from the source.
    - `exclude` (`$COMPONENT_EXCLUDE`): Do not include values for components in 
      this list component names from the source.
- `arguments` (`$ANALYSIS_ARGUMENTS`): A list of arguments which is always 
  passed to scripts, even if it was not provided, if the argument was set to 
  a different value or if the inverse argument was provided. Arguments not 
  supported by a script are excluded during option parsing.

Optionally, the configuration YAML file may contain additional sections named 
after an organization, which can be selected during script runs using a `--org` 
argument or the `ANALYSIS_ORGANIZATION` environment variable. When such 
a section is selected and found, it replaces all the other sections, and as 
such the organization section must contain all these sections nested within it.

## Usage

This repository contains a number of scripts that can be used to collect, 
analyze and export data. The scripts can all be run using the command format 
`Rscript <filename> [options]`. The following scripts have a `--help` argument 
that displays usage instructions:

- `features.r`: Extract features regarding sprints, stories or projects and 
  export them to ARFF, JSON or CSV file(s), depending on arguments. The 
  features are defined in `sprint_features.yml`, `story_features.yml` and 
  `project_features.yml`, respectively. Used by the `prediction`, 
  `sprint-report` and `leaderboard` repositories.
- `sprint_results.r`: Combine output from a prediction model with other sprint 
  data such that it can be used by an API producer. Used by the `prediction` 
  repository; the API is established by the `visualization-site` repository and 
  consumed by the `prediction-site` repository.
- `timeline.r`: Collect data for timeline visualization. Includes features 
  similar to `features.r`, but also events for sprints; the latter are 
  enumerated in the `sprint_events.yml` file. Used by the `timeline` 
  repository.
- `report.r`: Perform an analysis and generate a report. The available reports 
  are enumerated in the `analysis_reports.yml` file. Used by the 
  `bigboat-status`, `collaboration-graph`, `heatmap`, `process-flow` and 
  `timeline` repositories.
- `weather.r`: Retrieve historical weather information for a nearby active 
  station. Used by the `heatmap` repository.
- `compile_queries.r`: Compile SQL queries with patterns to executable queries, 
  so that they can be used outside of the R scripts.

The following scripts do not have a `--help` argument:

- `install.r`: Install all requirements.
- `lint.r`: Perform a lint on a number of R scripts provided via command line 
  arguments. Used by this repository in the `Jenkinsfile` to check code style.
- `timemachine.r`: Make a train size/score plot based on a time machine 
  prediction run.
- `res.r`: Plot or aggregate results of prediction estimators.
- `qqplot.r`: Make a Quantile-Quantile plot diagram for tabular data aggregated 
  from `res.r`.
- `backlog-size-interval.r`: Generate CSV of backlog sizes at project-based 
  intervals.
- `sentiment.r`: Output sentiment values for issue tracker comments.
- `authors.r`: Count the sprints each developer of each project is active in.
- `status.r`: Read BigBoat status information and create a plot.
- `bigboat-anomaly.r`: Perform anomaly detection on BigBoat status and plot the 
  results.
- `metric-anomaly.r`: Perform anomaly detection on metrics and plot the 
  results.

Aside from the YAML files mentioned with the scripts, there are more files in 
this repository which define fields and conditions as well as attributes for 
sources (such as `source_types.yml` and `trackers.yml`). The YAML files 
mentioned with the scripts refer to SQL template files in directories which 
perform the feature collection. The `sprint_definitions.yml` file contains 
fields and conditions used within patterns in the SQL template files to 
describe how to obtain columns or filter based on them, for different primary 
sources. The `report.r` file additionally uses fields from 
`analysis_definitions.yml` (can be overridden using command line arguments). 
The `bigboat_status` report uses `bigboat_status.yml` to define and 
match/combine fields for platform status.
