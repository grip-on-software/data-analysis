SELECT avg_time, avg_concurrent FROM
(SELECT startdata.project_id, startdata.sprint_id, AVG(EXTRACT(day FROM end_date - start_date)) AS avg_time FROM
(SELECT project_id, issue_id, sprint_id, MIN(updated) AS start_date FROM gros.issue WHERE type = 7 AND status = 3 GROUP BY project_id, issue_id, sprint_id) AS startdata,
(SELECT project_id, issue_id, sprint_id, MIN(updated) AS end_date FROM gros.issue WHERE resolution <> 0 GROUP BY project_id, issue_id, sprint_id) AS enddata
WHERE startdata.issue_id = enddata.issue_id
GROUP BY startdata.project_id, startdata.sprint_id
) AS timedata,
(SELECT project, sprint, AVG(num_open) AS avg_concurrent FROM
(SELECT DISTINCT project, sprint, date, COUNT(*) AS num_open FROM
    (SELECT project_id AS project, issue_id, sprint_id AS sprint, MIN(updated) AS date FROM gros.issue WHERE type = 7 AND status = 3 GROUP BY project_id, issue_id, sprint_id
    UNION ALL
    SELECT project_id AS project, issue_id, sprint_id AS sprint, MIN(updated) AS date FROM gros.issue WHERE resolution <> 0 GROUP BY project_id, issue_id, sprint_id) AS updatedata,
    (SELECT project_id, sprint_id, issue_id, MIN(updated) AS start_date FROM gros.issue WHERE type = 7 AND status = 3 GROUP BY project_id, sprint_id, issue_id) AS startdata,
    (SELECT project_id, sprint_id, issue_id, MIN(updated) AS end_date FROM gros.issue WHERE resolution <> 0 GROUP BY project_id, sprint_id, issue_id) AS enddata
    WHERE startdata.issue_id = enddata.issue_id
    AND updatedata.project = startdata.project_id
    AND updatedata.sprint = startdata.sprint_id
    AND updatedata.issue_id <> startdata.issue_id
    AND date BETWEEN start_date and end_date
    GROUP BY project, sprint, date
) AS opendata
GROUP BY project, sprint) AS concurrentdata
WHERE timedata.project_id = concurrentdata.project AND timedata.sprint_id = concurrentdata.sprint;
