SELECT project_id, sprint_id, AVG(num_progress) AS avg_concurrent_progress FROM
(SELECT DISTINCT project AS project_id, sprint AS sprint_id, date, COUNT(*) AS num_progress FROM
    (SELECT project_id AS project, issue_id, sprint_id AS sprint, MIN(updated) AS date FROM gros.issue WHERE status = 3 GROUP BY project_id, issue_id, sprint_id
    UNION ALL
    SELECT project_id AS project, issue_id, sprint_id AS sprint, MIN(updated) AS date FROM gros.issue WHERE COALESCE(resolution, 0) <> 0 GROUP BY project_id, issue_id, sprint_id) AS updatedata,
    (SELECT project_id, sprint_id, issue_id, MIN(updated) AS start_date FROM gros.issue WHERE status = 3 GROUP BY project_id, sprint_id, issue_id) AS startdata,
    (SELECT project_id, sprint_id, issue_id, MIN(updated) AS end_date FROM gros.issue WHERE COALESCE(resolution, 0) <> 0 GROUP BY project_id, sprint_id, issue_id) AS enddata
    WHERE startdata.issue_id = enddata.issue_id
    AND updatedata.project = startdata.project_id
    AND updatedata.sprint = startdata.sprint_id
    AND updatedata.issue_id <> startdata.issue_id
    AND date BETWEEN start_date and end_date
    GROUP BY project, sprint, date
) AS progressdata
GROUP BY project_id, sprint_id
