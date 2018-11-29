SELECT project AS project_id, sprint AS sprint_id, key,
MAX(story_points) AS story_points, COUNT(DISTINCT startdata.issue_id) AS num FROM
    (SELECT project_id AS project, issue_id, key, sprint_id AS sprint, MAX(story_points) AS story_points, MIN(updated) AS date FROM gros.issue WHERE type = 7 AND status = 3 GROUP BY project_id, issue_id, key, sprint_id
    UNION ALL
    SELECT project_id AS project, issue_id, key, sprint_id AS sprint, MAX(story_points), MIN(updated) AS date FROM gros.issue WHERE type = 7 AND ${s(issue_done)} GROUP BY project_id, issue_id, key, sprint_id) AS updatedata,
    (SELECT project_id, sprint_id, issue_id, MIN(updated) AS start_date FROM gros.issue WHERE type = 7 AND status = 3 GROUP BY project_id, sprint_id, issue_id) AS startdata,
    (SELECT project_id, sprint_id, issue_id, MIN(updated) AS end_date FROM gros.issue WHERE type = 7 AND ${s(issue_done)} GROUP BY project_id, sprint_id, issue_id) AS enddata
WHERE startdata.issue_id = enddata.issue_id
AND startdata.sprint_id = enddata.sprint_id
AND updatedata.project = startdata.project_id
AND updatedata.sprint = startdata.sprint_id
AND updatedata.issue_id <> startdata.issue_id
AND date BETWEEN start_date and end_date
GROUP BY project, sprint, key
