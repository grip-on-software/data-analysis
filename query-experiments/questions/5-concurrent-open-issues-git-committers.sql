SELECT project."name", sprint.start_date, AVG(num_open) AS avg_open, committers FROM
(SELECT DISTINCT project, sprint, date, COUNT(*) AS num_open FROM
    (SELECT project_id AS project, issue_id, sprint_id AS sprint, MIN(updated) AS date FROM gros.issue WHERE status = 3 GROUP BY project_id, issue_id, sprint_id
    UNION ALL
    SELECT project_id AS project, issue_id, sprint_id AS sprint, MIN(updated) AS date FROM gros.issue WHERE resolution <> 0 GROUP BY project_id, issue_id, sprint_id) AS updatedata,
    (SELECT project_id, sprint_id, issue_id, MIN(updated) AS start_date FROM gros.issue WHERE status = 3 GROUP BY project_id, sprint_id, issue_id) AS startdata,
    (SELECT project_id, sprint_id, issue_id, MIN(updated) AS end_date FROM gros.issue WHERE resolution <> 0 GROUP BY project_id, sprint_id, issue_id) AS enddata
    WHERE startdata.issue_id = enddata.issue_id
    AND updatedata.project = startdata.project_id
    AND updatedata.sprint = startdata.sprint_id
    AND updatedata.issue_id <> startdata.issue_id
    AND date BETWEEN start_date and end_date
    GROUP BY project, sprint, date
) AS opendata,
(SELECT commits.project_id, commits.sprint_id, COUNT(DISTINCT commits.developer_id) AS committers FROM gros.commits
GROUP BY commits.project_id, commits.sprint_id) AS commitdata, gros.project, gros.sprint
WHERE opendata.project = project.project_id
AND opendata.project = sprint.project_id AND opendata.sprint = sprint.sprint_id
AND opendata.project = commitdata.project_id AND opendata.sprint = commitdata.sprint_id
GROUP BY project."name", sprint.start_date, committers
ORDER BY project."name", sprint.start_date;
